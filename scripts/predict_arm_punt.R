# Predict the expected-points advantage for a specified third-down arm-punt
# situation. Run from the repository root:
#   Rscript scripts/predict_arm_punt.R --air-yards 35 --yards-to-go 8 --own-yardline 42

parse_inputs <- function(args) {
  if (!length(args)) stop("No situation was supplied. Use --help for usage.")
  if (length(args) == 1L && args[[1L]] %in% c("--help", "-h")) {
    cat(paste(
      "Usage:",
      "  Rscript scripts/predict_arm_punt.R --air-yards N --yards-to-go N --own-yardline N",
      "",
      "Inputs describe a third-down pass: target air depth in yards, yards to go,",
      "and the offense's own-yard-line position (1-99). The target is evaluated",
      "under the primary >=30-air-yard contested-pass model and neutral Q1 baseline.",
      sep = "\n"
    ), "\n")
    return(NULL)
  }
  if (length(args) %% 2L != 0L) stop("Supply each input as a flag followed by a value. Use --help for usage.")
  keys <- args[seq(1L, length(args), by = 2L)]
  vals <- args[seq(2L, length(args), by = 2L)]
  allowed <- c("--air-yards", "--yards-to-go", "--own-yardline")
  if (any(!keys %in% allowed)) stop("Unknown input flag. Use --help for usage.")
  if (anyDuplicated(keys)) stop("Each input flag can be supplied only once.")
  missing <- setdiff(allowed, keys)
  if (length(missing)) stop("Missing required input(s): ", paste(missing, collapse = ", "))
  values <- stats::setNames(as.numeric(vals), keys)
  if (any(!is.finite(values))) stop("All three inputs must be finite numbers.")
  values
}

inputs <- parse_inputs(commandArgs(trailingOnly = TRUE))
if (is.null(inputs)) quit(status = 0L)

repo <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
analysis_script <- file.path(repo, "scripts", "run_public_analysis.R")
if (!file.exists(analysis_script)) stop("Run this command from the repository root: ", repo)

# Load the exact analysis functions and project-local package/cache settings
# without rerunning the batch analysis.
source(analysis_script, local = FALSE)

air_yards <- unname(inputs[["--air-yards"]])
ydstogo <- unname(inputs[["--yards-to-go"]])
own_yardline <- unname(inputs[["--own-yardline"]])
if (air_yards < PRIMARY_CUTOFF) stop("The primary model requires air yards >= ", PRIMARY_CUTOFF, ".")
if (own_yardline < 1 || own_yardline > 99) stop("Own-yardline position must be between 1 and 99.")
if (ydstogo < 1 || ydstogo > 100 - own_yardline) {
  stop("Yards to go must be >= 1 and no greater than the distance to the opponent goal line.")
}

plays <- read_csv(file.path(DATA, "plays_2022_onward.csv.gz"))
passes <- read_csv(file.path(DATA, "third_down_passes_2022_onward.csv"))
punts <- read_csv(file.path(DATA, "punt_plays_2022_onward.csv"))
int_return_train <- prepare_interception_return_training(plays)
completion_yac_train <- prepare_completion_yac_training(plays)
passes$outcome <- classify(passes)
pass_attempt <- flag(passes$pass_attempt) & !flag(passes$sack) & !flag(passes$qb_scramble)
eligible <- pass_attempt & finite(num(passes$air_yards)) &
  (flag(passes$ftn_is_contested_ball) | is_dpi(passes))
candidate_rows <- which(eligible & num(passes$air_yards) >= PRIMARY_CUTOFF)
candidate_states <- passes[candidate_rows, , drop = FALSE]
candidate_states$yards_gained <- 0
candidate_decisions <- neutral_fourth_decisions(candidate_states)
candidate_keys <- paste(round(100 - num(candidate_states$yardline_100), 2),
                        round(num(candidate_states$ydstogo), 2), sep = "|")
candidate_decision_idx <- match(candidate_keys, candidate_decisions$key)
candidate_passes_filter <- !is.na(candidate_decision_idx) &
  candidate_decisions$punt_share[candidate_decision_idx] > .5
eligible_for_model <- rep(FALSE, nrow(passes))
eligible_for_model[candidate_rows] <- candidate_passes_filter
train <- passes[eligible_for_model, , drop = FALSE]
train$air_yards <- num(train$air_yards)
train$ydstogo <- num(train$ydstogo)
train$yardline_100 <- num(train$yardline_100)
train$outcome <- factor(train$outcome)
train <- train[complete.cases(train[, c("air_yards", "ydstogo", "yardline_100")]), , drop = FALSE]
train$own_start <- 100 - train$yardline_100

punt_values <- punt_event_values(punts)
punt_values$game_id <- punts$game_id
throwaway_ep <- punt_ev_at(own_yardline, punt_values)

# Verify that the requested pre-play state meets the same zero-gain punt filter
# used to define the analysis sample.
target_zero <- neutral_fourth_decisions(data.frame(
  yardline_100 = 100 - own_yardline, ydstogo = ydstogo, yards_gained = 0
))
if (!nrow(target_zero) || target_zero$punt_share[[1L]] <= .5) {
  share <- if (nrow(target_zero)) round(target_zero$punt_share[[1L]], 3) else NA_real_
  stop("This situation does not meet the arm-punt definition: nfl4th's punt-best share after zero gain is ", share, ".")
}
short_rows <- train[num(train$air_yards) >= min(CUTOFFS) &
  as.character(train$outcome) == "completion_short", , drop = FALSE]
fd_decisions <- unique(rbind(candidate_decisions, neutral_fourth_decisions(short_rows)))

target <- data.frame(
  air_yards = air_yards,
  ydstogo = ydstogo,
  yardline_100 = 100 - own_yardline,
  own_start = own_yardline,
  throwaway_value = throwaway_ep
)
feature_scale <- make_features(train)
prediction <- score_model(train, target, feature_scale$centers,
                          feature_scale$scales, punt_values, fd_decisions,
                          int_return_train, completion_yac_train)
if (!finite(prediction$expected[[1L]])) {
  stop("The model could not value at least one outcome category with predicted probability above zero.")
}

prob <- prediction$prob[1L, ]
conditional_ep <- prediction$conditional[1L, names(prob)]
contribution <- as.numeric(prob) * as.numeric(conditional_ep)
outcome_table <- data.frame(
  outcome = names(prob),
  predicted_probability = as.numeric(prob),
  conditional_ep = as.numeric(conditional_ep),
  probability_weighted_ep = contribution
)
outcome_table <- outcome_table[order(-outcome_table$predicted_probability), , drop = FALSE]

features <- c(air_yards = air_yards, ydstogo = ydstogo, own_start = own_yardline)
training_ranges <- vapply(c("air_yards", "ydstogo", "own_start"), function(nm) {
  range(num(train[[nm]]), na.rm = TRUE)
}, numeric(2))
outside <- vapply(names(features), function(nm) {
  features[[nm]] < training_ranges[1L, nm] || features[[nm]] > training_ranges[2L, nm]
}, logical(1))

cat("Public-model arm-punt prediction\n")
cat("Situation: third down, target depth", air_yards, "air yards;", ydstogo,
    "to go; offense at its own", own_yardline, "\n")
cat("Training plays:", nrow(train), "eligible contested/DPI plays at >=",
    PRIMARY_CUTOFF, "air yards\n")
cat("Zero-gain fourth-down punt-best share:", round(target_zero$punt_share[[1L]], 3),
    "(situation passes the arm-punt filter)\n")
cat("\nPredicted outcome values:\n")
print(outcome_table, row.names = FALSE, digits = 4)
completion_names <- names(prob)[grepl("^completion", names(prob))]
if (length(completion_names) && any(prob[completion_names] > 0)) {
  yac_summary <- completion_yac_summary(target, completion_yac_train)
  cat("\nPredicted yards-after-catch distribution, conditional on a completion:\n")
  cat("Training completions:", yac_summary$conditional_n, "\n")
  cat("Mean:", round(yac_summary$mean_yac, 2), "yards; quartiles:",
      round(yac_summary$q25_yac, 2), "/", round(yac_summary$median_yac, 2), "/",
      round(yac_summary$q75_yac, 2), "yards\n")
  cat("P(YAC >= 10):", round(yac_summary$share_yac_10_plus, 3), "\n")
}
if ("interception" %in% names(prob) && prob[["interception"]] > 0) {
  return_summary <- interception_return_summary(target, int_return_train)
  cat("\nPredicted interception-return distribution, conditional on an interception:\n")
  cat("Training interception returns:", return_summary$conditional_n, "\n")
  cat("Mean:", round(return_summary$mean_return_yards, 2), "yards; quartiles:",
      round(return_summary$q25_return_yards, 2), "/",
      round(return_summary$median_return_yards, 2), "/",
      round(return_summary$q75_return_yards, 2), "yards\n")
  cat("P(return <= 0):", round(return_summary$share_zero_or_negative, 3),
      "| P(return >= 20):", round(return_summary$share_20_plus, 3), "\n")
}
cat("\nPredicted arm-punt EP:", round(prediction$expected[[1L]], 4), "\n")
cat("Throwaway followed by punt EP:", round(throwaway_ep, 4), "\n")
cat("Arm-punt EP advantage:", round(prediction$expected[[1L]] - throwaway_ep, 4), "\n")
cat("\nAll state values use the public EP model under the neutral Q1 baseline.\n")
if (any(outside)) {
  cat("WARNING: input outside the training range for:",
      paste(names(features)[outside], collapse = ", "),
      " Predictions there rely on extrapolation/nearest available conditional support.\n")
}
