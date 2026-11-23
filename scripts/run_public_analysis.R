# Public-model arm-punt analysis. Run from the repository root with:
#   Rscript scripts/run_public_analysis.R

ROOT <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
DATA <- file.path(ROOT, "data")
OUT <- file.path(ROOT, "outputs")
TABLES <- file.path(OUT, "tables")
dir.create(TABLES, recursive = TRUE, showWarnings = FALSE)

Sys.setenv(R_USER_CACHE_DIR = file.path(ROOT, ".r-cache"))
R_LIB <- file.path(ROOT, ".r-library", paste0(R.version$major, ".", R.version$minor))
.libPaths(c(R_LIB, .libPaths()))
options(nfl4th.force_cache = "true", nfl4th.keep_games = TRUE)

for (pkg in c("data.table", "jsonlite", "nflfastR", "nfl4th", "nnet", "rpart")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Missing required package: ", pkg)
}
suppressPackageStartupMessages({
  library(nflfastR)
  library(nfl4th)
  library(nnet)
})

PRIMARY_CUTOFF <- 30
CUTOFFS <- c(25, 30, 35, 40)
BOOTSTRAPS <- 100L
SEED <- 20261121L
EP_SEASON <- 2025L # latest complete season in the package
FD_SEASON <- 2025L
FD_HOME <- "NYJ"   # interface labels only; neutral lines remove team strength
FD_AWAY <- "HOU"

read_csv <- function(path) {
  con <- if (grepl("\\.gz$", path, ignore.case = TRUE)) gzfile(path, open = "rt") else file(path, open = "rt")
  on.exit(close(con), add = TRUE)
  utils::read.csv(con, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
}
num <- function(x) suppressWarnings(as.numeric(as.character(x)))
flag <- function(x) !is.na(x) & tolower(as.character(x)) %in% c("1", "true", "t", "yes")
finite <- function(x) !is.na(x) & is.finite(x)
mean_or_na <- function(x) if (any(finite(x))) mean(x[finite(x)]) else NA_real_
q_or_na <- function(x, p) if (any(finite(x))) unname(stats::quantile(x[finite(x)], p, na.rm = TRUE)) else NA_real_

ep_state <- function(own_yard, down = 1, ydstogo = 10) {
  n <- length(own_yard)
  if (!n) return(numeric())
  own_yard <- pmin(100, pmax(0, num(own_yard)))
  down <- rep_len(num(down), n)
  ydstogo <- pmax(1, rep_len(num(ydstogo), n))
  d <- data.frame(
    season = rep(EP_SEASON, n), home_team = rep("NYJ", n), away_team = rep("HOU", n),
    posteam = rep("NYJ", n), roof = rep("outdoors", n),
    half_seconds_remaining = rep(3600L, n), qtr = rep(1L, n),
    quarter_seconds_remaining = rep(900L, n), yardline_100 = 100 - own_yard,
    down = down, ydstogo = ydstogo,
    posteam_timeouts_remaining = rep(3L, n), defteam_timeouts_remaining = rep(3L, n)
  )
  ans <- suppressWarnings(nflfastR::calculate_expected_points(d)$ep)
  as.numeric(ans)
}

is_dpi <- function(d) {
  pt <- tolower(ifelse(is.na(d$penalty_type), "", as.character(d$penalty_type)))
  grepl("pass interference", pt) & !is.na(d$penalty_team) &
    as.character(d$penalty_team) == as.character(d$defteam) & flag(d$first_down_penalty)
}

classify <- function(d) {
  n <- nrow(d)
  out <- rep("other_unresolved", n)
  pt <- if ("penalty_type" %in% names(d)) tolower(ifelse(is.na(d$penalty_type), "", as.character(d$penalty_type))) else rep("", n)
  penalty <- nzchar(trimws(pt))
  dpi <- is_dpi(d)
  own_pen <- penalty & !is.na(d$penalty_team) & as.character(d$penalty_team) == as.character(d$posteam)
  intercept <- flag(d$interception)
  complete <- flag(d$complete_pass)
  incomplete <- flag(d$incomplete_pass)
  own <- 100 - num(d$yardline_100)
  gain <- num(d$yards_gained)
  touchdown <- complete & finite(own) & finite(gain) & (own + gain >= 100)
  fumble_turnover <- flag(d$fumble_lost) & !is.na(d$next_drive_team) & as.character(d$next_drive_team) == as.character(d$defteam)
  converts <- flag(d$first_down_pass) | (finite(gain) & finite(num(d$ydstogo)) & gain >= num(d$ydstogo))

  out[own_pen] <- "offensive_penalty"
  out[dpi & !own_pen] <- "accepted_defensive_penalty"
  out[intercept & !own_pen & !dpi] <- "interception"
  out[intercept & !own_pen & !dpi & flag(d$return_touchdown)] <- "interception_return_td"
  out[fumble_turnover & !intercept & !own_pen & !dpi] <- "completion_fumble_lost"
  out[touchdown & !intercept & !own_pen & !dpi & !fumble_turnover] <- "completion_touchdown"
  out[complete & converts & !touchdown & !intercept & !own_pen & !dpi & !fumble_turnover] <- "completion_converts"
  out[complete & !converts & !touchdown & !intercept & !own_pen & !dpi & !fumble_turnover] <- "completion_short"
  out[incomplete & !intercept & !own_pen & !dpi] <- "incomplete"
  out
}

# Insert a project-only neutral matchup into nfl4th's schedule cache. nfl4th's
# public models use game betting lines, so zero spread and equal team totals
# enforce the plan's team-agnostic neutral baseline.
ensure_neutral_fd_fixture <- function() {
  path <- nfl4th:::nfl4th_games_path()
  if (!file.exists(path)) stop("nfl4th schedule cache is missing: ", path,
                               ". Run scripts/cache_public_models.R first.")
  games <- readRDS(path)
  key <- games$season == FD_SEASON & games$type == "reg" &
    games$home_team == FD_HOME & games$away_team == FD_AWAY
  if (any(key)) {
    template <- games[which(key)[1L], , drop = FALSE]
    games <- games[!key, , drop = FALSE]
  } else {
    template <- games[games$season == FD_SEASON & games$type == "reg", , drop = FALSE][1L, , drop = FALSE]
  }
  template$game_id <- "2025_00_NEUTRAL_FD"
  template$season <- FD_SEASON
  template$type <- "reg"
  template$week <- 1L
  template$home_team <- FD_HOME
  template$away_team <- FD_AWAY
  template$spread_line <- 0
  template$total_line <- 44
  template$home_total <- 22
  template$away_total <- 22
  template$model_roof <- "outdoors"
  template$roof <- "outdoors"
  template$outdoors <- 1
  template$dome <- 0
  template$retractable <- 0
  if ("fg_model_roof" %in% names(template)) {
    template$fg_model_roof <- factor("11", levels = levels(games$fg_model_roof))
  }
  games <- rbind(games, template)
  saveRDS(games, path)
  memo <- get("nfl4th_games_memo", envir = asNamespace("nfl4th"))
  memo$data <- NULL
  memo$info <- NULL
  invisible(path)
}

punt_event_values <- function(punts) {
  own_start <- 100 - num(punts$yardline_100)
  posteam <- as.character(punts$posteam)
  defteam <- as.character(punts$defteam)
  next_team <- as.character(punts$next_drive_team)
  next_own <- num(punts$next_drive_own_yardline)
  value <- rep(NA_real_, nrow(punts))
  td <- flag(punts$return_touchdown)
  return_team <- as.character(punts$return_team)
  td_receiver <- td & return_team == defteam
  td_punter <- td & return_team == posteam
  ep25 <- ep_state(rep(25, 2), c(1, 1), c(10, 10))
  value[td_receiver] <- -7 + ep25[1]
  value[td_punter] <- 7 - ep25[2]

  rec <- !td & next_team == defteam & finite(next_own)
  own_retained <- !td & next_team == posteam & finite(next_own)
  if (any(rec)) value[rec] <- -ep_state(next_own[rec], 1, pmin(10, pmax(1, 100 - next_own[rec])))
  if (any(own_retained)) value[own_retained] <- ep_state(next_own[own_retained], 1, pmin(10, pmax(1, 100 - next_own[own_retained])))
  data.frame(own_start = own_start, value = value,
             game_id = as.character(punts$game_id), season = num(punts$season))
}

prepare_interception_return_training <- function(passes) {
  required <- c("air_yards", "yardline_100", "interception_return_yards",
                "next_drive_own_yardline", "next_drive_team", "defteam")
  missing <- setdiff(required, names(passes))
  if (length(missing)) stop("Missing interception-return fields: ", paste(missing, collapse = ", "))
  keep <- flag(passes$interception) & finite(num(passes$air_yards)) &
    finite(num(passes$yardline_100)) & finite(num(passes$interception_return_yards)) &
    finite(num(passes$next_drive_own_yardline)) &
    as.character(passes$next_drive_team) == as.character(passes$defteam) &
    !flag(passes$return_touchdown) & !flag(passes$penalty)
  d <- passes[keep, , drop = FALSE]
  d$air_yards <- num(d$air_yards)
  d$own_start <- 100 - num(d$yardline_100)
  d$int_return_yards <- num(d$interception_return_yards)
  d$int_pick_own <- num(d$next_drive_own_yardline) - d$int_return_yards
  d$target_opp_own <- pmin(100, pmax(0, 100 - (d$own_start + d$air_yards)))
  # Residual location at the pick preserves the observed fact that an
  # interception need not occur exactly at the intended target depth.
  d$int_pick_offset <- d$int_pick_own - d$target_opp_own
  d[finite(d$int_pick_offset) & finite(d$int_return_yards), , drop = FALSE]
}

prepare_completion_yac_training <- function(plays) {
  required <- c("air_yards", "yardline_100", "yards_after_catch", "complete_pass",
                "penalty", "game_id", "season")
  missing <- setdiff(required, names(plays))
  if (length(missing)) stop("Missing completion-YAC fields: ", paste(missing, collapse = ", "))
  keep <- flag(plays$complete_pass) & !flag(plays$penalty) &
    finite(num(plays$air_yards)) & finite(num(plays$yardline_100)) &
    finite(num(plays$yards_after_catch))
  d <- plays[keep, , drop = FALSE]
  d$air_yards <- num(d$air_yards)
  d$own_start <- 100 - num(d$yardline_100)
  d$yards_after_catch <- num(d$yards_after_catch)
  d
}

fit_conditional_tree <- function(d, response, predictors) {
  keep <- finite(num(d[[response]])) & complete.cases(d[, predictors, drop = FALSE])
  train <- d[keep, , drop = FALSE]
  if (!nrow(train)) return(NULL)
  fit <- rpart::rpart(
    stats::reformulate(predictors, response = response), data = train,
    method = "anova",
    control = rpart::rpart.control(
      cp = 0.001, minsplit = 60, minbucket = 25, maxdepth = 3, xval = 0
    )
  )
  list(fit = fit, data = train, response = response, predictors = predictors,
       training_leaf = fit$where)
}

conditional_tree_members <- function(target, tree) {
  nt <- nrow(target)
  if (is.null(tree)) return(rep(list(integer()), nt))
  nd <- nrow(tree$data)
  if (!nt || !nd) return(rep(list(integer()), nt))
  query <- target[, tree$predictors, drop = FALSE]
  for (nm in tree$predictors) query[[nm]] <- num(query[[nm]])
  terminal_nodes <- rpart:::pred.rpart(tree$fit, as.matrix(query))
  # pred.rpart returns row indices into fit$frame, matching fit$where.
  leaf_members <- split(seq_len(nd), tree$training_leaf)
  lapply(as.integer(terminal_nodes), function(leaf_row) {
    members <- leaf_members[[as.character(leaf_row)]]
    if (is.null(members)) integer() else members
  })
}

interception_return_weights <- function(target, int_train) {
  tree <- fit_conditional_tree(int_train, "int_return_yards", c("air_yards", "own_start"))
  members <- conditional_tree_members(target, tree)
  weights <- matrix(0, nrow = nrow(target), ncol = if (is.null(tree)) 0L else nrow(tree$data))
  for (i in seq_along(members)) if (length(members[[i]])) {
    weights[i, members[[i]]] <- 1 / length(members[[i]])
  }
  weights
}

punt_ev_at <- function(spot, punt_values, punt_tree = NULL) {
  if (!length(spot) || !nrow(punt_values)) return(rep(NA_real_, length(spot)))
  if (is.null(punt_tree)) punt_tree <- fit_conditional_tree(punt_values, "value", "own_start")
  if (is.null(punt_tree)) return(rep(NA_real_, length(spot)))
  members <- conditional_tree_members(data.frame(own_start = num(spot)), punt_tree)
  vapply(members, function(i) if (!length(i)) NA_real_ else mean(num(punt_tree$data$value[i])), numeric(1))
}

interception_conditional_ep <- function(target, int_train, int_tree = NULL) {
  nt <- nrow(target)
  nd <- nrow(int_train)
  if (!nt) return(numeric())
  if (!nd) return(rep(NA_real_, nt))
  if (is.null(int_tree)) int_tree <- fit_conditional_tree(int_train, "int_return_yards", c("air_yards", "own_start"))
  members <- conditional_tree_members(target, int_tree)
  n_members <- lengths(members)
  if (any(n_members == 0L)) return(rep(NA_real_, nt))
  target_idx <- rep(seq_len(nt), n_members)
  donor_idx <- unlist(members, use.names = FALSE)
  target_opp_own <- pmin(100, pmax(0, 100 -
    (num(target$own_start[target_idx]) + num(target$air_yards[target_idx]))))
  pick_own <- pmin(100, pmax(0, target_opp_own + num(int_tree$data$int_pick_offset[donor_idx])))
  after_return <- pmin(100, pmax(0, pick_own + num(int_tree$data$int_return_yards[donor_idx])))
  return_td <- after_return >= 100
  value <- rep(NA_real_, length(after_return))
  if (any(return_td)) value[return_td] <- -7 + ep_state(rep(25, sum(return_td)), 1, 10)
  ordinary <- !return_td & finite(after_return)
  if (any(ordinary)) {
    togo <- pmin(10, pmax(1, 100 - after_return[ordinary]))
    value[ordinary] <- -ep_state(after_return[ordinary], 1, togo)
  }
  vapply(seq_len(nt), function(i) mean_or_na(value[target_idx == i]), numeric(1))
}

weighted_quantile <- function(x, w, probs) {
  ok <- finite(x) & finite(w) & w > 0
  if (!any(ok)) return(rep(NA_real_, length(probs)))
  ord <- order(x[ok])
  x <- x[ok][ord]
  w <- w[ok][ord] / sum(w[ok])
  vapply(probs, function(p) x[which(cumsum(w) >= p)[1L]], numeric(1))
}

interception_return_summary <- function(target, int_train) {
  if (!nrow(target) || !nrow(int_train)) return(data.frame())
  tree <- fit_conditional_tree(int_train, "int_return_yards", c("air_yards", "own_start"))
  members <- conditional_tree_members(target[1L, , drop = FALSE], tree)[[1L]]
  weights <- rep(0, nrow(tree$data))
  if (length(members)) weights[members] <- 1 / length(members)
  ret <- num(tree$data$int_return_yards)
  q <- weighted_quantile(ret, weights, c(.25, .5, .75))
  data.frame(
    conditional_n = nrow(tree$data),
    mean_return_yards = sum(weights * ret),
    q25_return_yards = q[1L], median_return_yards = q[2L], q75_return_yards = q[3L],
    share_zero_or_negative = sum(weights[ret <= 0]),
    share_20_plus = sum(weights[ret >= 20])
  )
}

completion_yac_summary <- function(target, completion_train) {
  if (!nrow(target) || !nrow(completion_train)) return(data.frame())
  tree <- fit_conditional_tree(completion_train, "yards_after_catch", c("air_yards", "own_start"))
  members <- conditional_tree_members(target[1L, , drop = FALSE], tree)[[1L]]
  weights <- rep(0, nrow(tree$data))
  if (length(members)) weights[members] <- 1 / length(members)
  yac <- num(tree$data$yards_after_catch)
  q <- weighted_quantile(yac, weights, c(.25, .5, .75))
  data.frame(
    conditional_n = nrow(tree$data), mean_yac = sum(weights * yac),
    q25_yac = q[1L], median_yac = q[2L], q75_yac = q[3L],
    share_yac_10_plus = sum(weights[yac >= 10])
  )
}

completion_conditional_ep <- function(target, completion_tree, outcome, punt_values,
                                      punt_tree, fd_decisions) {
  if (is.null(completion_tree)) return(rep(NA_real_, nrow(target)))
  members <- conditional_tree_members(target, completion_tree)
  yac <- num(completion_tree$data$yards_after_catch)
  state_target <- integer()
  state_gain <- numeric()
  state_own <- numeric()
  state_weight <- numeric()
  for (i in seq_len(nrow(target))) {
    donor_idx <- members[[i]]
    if (!length(donor_idx)) next
    gain <- num(target$air_yards[i]) + yac[donor_idx]
    own_after <- pmin(100, pmax(0, num(target$own_start[i]) + gain))
    if (outcome == "completion_converts") {
      valid <- finite(gain) & gain >= num(target$ydstogo[i]) & own_after < 100
    } else {
      valid <- finite(gain) & gain < num(target$ydstogo[i]) & own_after < 100
    }
    valid_idx <- which(valid)
    if (!length(valid_idx)) next
    valid_gain <- gain[valid_idx]
    unique_gain <- unique(valid_gain)
    counts <- tabulate(match(valid_gain, unique_gain), nbins = length(unique_gain))
    own <- pmin(100, pmax(0, num(target$own_start[i]) + unique_gain))
    state_target <- c(state_target, rep(i, length(unique_gain)))
    state_gain <- c(state_gain, unique_gain)
    state_own <- c(state_own, own)
    state_weight <- c(state_weight, counts / sum(counts))
  }

  result <- rep(NA_real_, nrow(target))
  if (!length(state_target)) return(result)
  if (outcome == "completion_converts") {
    values <- ep_state(state_own, 1, pmin(10, pmax(1, 100 - state_own)))
  } else {
    togo_after <- pmax(1, num(target$ydstogo[state_target]) - state_gain)
    keys <- paste(round(state_own, 2), round(togo_after, 2), sep = "|")
    need <- !keys %in% fd_decisions$key
    if (any(need)) {
      synthetic <- data.frame(yardline_100 = 100 - state_own[need],
        ydstogo = num(target$ydstogo[state_target[need]]), yards_gained = state_gain[need])
      extra <- neutral_fourth_decisions(synthetic)
      fd_decisions <- unique(rbind(fd_decisions, extra))
    }
    idx <- match(keys, fd_decisions$key)
    punt <- !is.na(idx) & fd_decisions$punt_share[idx] > .5
    values <- ep_state(state_own, 4, togo_after)
    if (any(punt)) values[punt] <- punt_ev_at(state_own[punt], punt_values, punt_tree)
  }
  weighted <- values * state_weight
  totals <- rowsum(matrix(weighted, ncol = 1L), state_target, reorder = FALSE)
  result[as.integer(rownames(totals))] <- totals[, 1L]
  result
}

neutral_fourth_decisions <- function(short_rows) {
  if (!nrow(short_rows)) return(data.frame(own_after = numeric(), togo_after = numeric(), punt_share = numeric()))
  own_start <- 100 - num(short_rows$yardline_100)
  own_after <- pmin(100, pmax(0, own_start + num(short_rows$yards_gained)))
  togo_after <- pmax(1, num(short_rows$ydstogo) - num(short_rows$yards_gained))
  key <- paste(round(own_after, 2), round(togo_after, 2), sep = "|")
  unique_states <- unique(data.frame(key = key, own_after = own_after, togo_after = togo_after))
  ensure_neutral_fd_fixture()
  states <- data.frame(
    game_id = rep("2025_00_NEUTRAL_FD", nrow(unique_states)), season = rep(FD_SEASON, nrow(unique_states)),
    week = rep(1L, nrow(unique_states)), type = rep("reg", nrow(unique_states)),
    home_team = rep(FD_HOME, nrow(unique_states)), away_team = rep(FD_AWAY, nrow(unique_states)),
    posteam = rep(FD_HOME, nrow(unique_states)), defteam = rep(FD_AWAY, nrow(unique_states)),
    home_opening_kickoff = rep(1L, nrow(unique_states)), qtr = rep(1L, nrow(unique_states)),
    quarter_seconds_remaining = rep(900L, nrow(unique_states)), half_seconds_remaining = rep(3600L, nrow(unique_states)),
    score_differential = rep(0L, nrow(unique_states)), down = rep(4L, nrow(unique_states)),
    ydstogo = unique_states$togo_after, yardline_100 = 100 - unique_states$own_after,
    posteam_timeouts_remaining = rep(3L, nrow(unique_states)), defteam_timeouts_remaining = rep(3L, nrow(unique_states))
  )
  fit <- nfl4th::add_4th_probs(states, bootstrap = TRUE)
  shares <- vapply(seq_len(nrow(fit)), function(i) {
    go <- as.numeric(fit$go_wp_boot[[i]])
    fg <- as.numeric(fit$fg_wp_boot[[i]])
    punt <- as.numeric(fit$punt_wp_boot[[i]])
    if (length(go) == 201L && length(fg) == 201L && length(punt) == 201L) {
      go <- go[-1L]; fg <- fg[-1L]; punt <- punt[-1L]
    }
    # nfl4th returns NA for punt where its model does not offer a punt value
    # (for example, a short field near the goal line); those draws cannot
    # recommend punt and count as false for the majority rule.
    punt_best <- finite(punt) & (!finite(go) | punt > go) & (!finite(fg) | punt > fg)
    mean(punt_best)
  }, numeric(1))
  data.frame(key = unique_states$key, own_after = unique_states$own_after,
             togo_after = unique_states$togo_after, punt_share = shares,
             public_decision = ifelse(shares > .5, "punt", "continue_on_fourth"))
}

make_features <- function(d, centers = NULL, scales = NULL) {
  x <- data.frame(air = num(d$air_yards), togo = num(d$ydstogo), own = 100 - num(d$yardline_100))
  if (is.null(centers)) centers <- vapply(x, mean, numeric(1), na.rm = TRUE)
  if (is.null(scales)) scales <- vapply(x, stats::sd, numeric(1), na.rm = TRUE)
  scales[!finite(scales) | scales == 0] <- 1
  z <- data.frame(air_z = (x$air - centers["air"]) / scales["air"],
                  togo_z = (x$togo - centers["togo"]) / scales["togo"],
                  own_z = (x$own - centers["own"]) / scales["own"])
  list(z = z, centers = centers, scales = scales)
}

fit_probability_model <- function(train, centers, scales) {
  train <- train[!is.na(train$outcome) & complete.cases(train[, c("air_yards", "ydstogo", "yardline_100")]), , drop = FALSE]
  classes <- unique(as.character(train$outcome))
  if (length(classes) < 2L) return(list(model = NULL, classes = classes, prior = prop.table(table(train$outcome)), centers = centers, scales = scales))
  z <- make_features(train, centers, scales)$z
  model_data <- data.frame(outcome = factor(as.character(train$outcome)), z)
  fit <- tryCatch(nnet::multinom(outcome ~ air_z + togo_z + own_z, data = model_data,
    decay = .5, maxit = 1000, trace = FALSE, MaxNWts = 20000), error = function(e) NULL)
  list(model = fit, classes = levels(model_data$outcome), prior = prop.table(table(model_data$outcome)),
       centers = centers, scales = scales)
}

predict_probabilities <- function(fit, target) {
  n <- nrow(target)
  ans <- matrix(0, nrow = n, ncol = length(fit$classes), dimnames = list(NULL, fit$classes))
  if (!length(fit$classes)) return(ans)
  if (is.null(fit$model)) {
    if (length(fit$classes)) for (k in fit$classes) ans[, k] <- as.numeric(fit$prior[k])
    return(ans)
  }
  z <- make_features(target, fit$centers, fit$scales)$z
  p <- tryCatch(stats::predict(fit$model, newdata = z, type = "probs"), error = function(e) NULL)
  if (is.null(p)) {
    for (k in fit$classes) ans[, k] <- as.numeric(fit$prior[k])
  } else if (is.null(dim(p))) {
    if (n == 1L && length(p) == length(fit$classes)) {
      if (!is.null(names(p))) {
        for (k in intersect(names(p), fit$classes)) ans[1L, k] <- p[k]
      } else {
        ans[1L, ] <- p
      }
    } else if (length(fit$classes) == 2L && length(p) == n) {
      ans[, fit$classes[1L]] <- 1 - p
      ans[, fit$classes[2L]] <- p
    } else if (length(fit$classes) == 1L) ans[, fit$classes] <- 1
  } else {
    for (k in intersect(colnames(p), fit$classes)) ans[, k] <- p[, k]
  }
  rs <- rowSums(ans)
  ans[rs > 0, ] <- ans[rs > 0, , drop = FALSE] / rs[rs > 0]
  ans
}

conditional_state_values <- function(target, donors, outcome, centers, scales, punt_values,
                                     fd_decisions, completion_tree = NULL, punt_tree = NULL) {
  if (outcome %in% c("completion_converts", "completion_short")) {
    return(completion_conditional_ep(target, completion_tree, outcome,
                                    punt_values, punt_tree, fd_decisions))
  }
  nt <- nrow(target)
  nd <- nrow(donors)
  if (!nt || !nd) return(rep(NA_real_, nt))
  ti <- rep(seq_len(nt), each = nd)
  di <- rep(seq_len(nd), times = nt)
  target_own <- num(target$own_start[ti])
  donor_gain <- num(donors$yards_gained[di])
  donor_own <- 100 - num(donors$yardline_100[di])
  values <- rep(NA_real_, nt * nd)

  if (outcome %in% c("incomplete", "offensive_penalty")) {
    values <- num(target$throwaway_value[ti])
  } else if (outcome == "completion_converts") {
    own_after <- pmin(100, pmax(0, target_own + donor_gain))
    is_td <- finite(own_after) & own_after >= 100
    ok <- finite(own_after) & finite(donor_gain) & !is_td
    if (any(ok)) {
      togo <- pmin(10, pmax(1, 100 - own_after[ok]))
      values[ok] <- ep_state(own_after[ok], 1, togo)
    }
    if (any(is_td)) values[is_td] <- 7 - ep_state(rep(25, sum(is_td)), 1, 10)
  } else if (outcome == "completion_touchdown") {
    values <- rep(7 - ep_state(25, 1, 10), nt * nd)
  } else if (outcome == "accepted_defensive_penalty") {
    spot <- pmin(100, pmax(0, target_own + num(donors$penalty_yards[di])))
    ok <- finite(spot)
    if (any(ok)) values[ok] <- ep_state(spot[ok], 1, pmin(10, pmax(1, 100 - spot[ok])))
  } else if (outcome %in% c("completion_fumble_lost", "interception_return_td")) {
    is_td <- flag(donors$return_touchdown[di]) &
      as.character(donors$return_team[di]) == as.character(donors$defteam[di])
    is_td[outcome == "interception_return_td"] <- TRUE
    if (any(is_td)) values[is_td] <- -7 + ep_state(rep(25, sum(is_td)), 1, 10)
    next_own <- num(donors$next_drive_own_yardline[di])
    good <- !is_td & as.character(donors$next_drive_team[di]) == as.character(donors$defteam[di]) & finite(next_own)
    if (any(good)) values[good] <- -ep_state(next_own[good], 1, pmin(10, pmax(1, 100 - next_own[good])))
  } else if (outcome == "completion_short") {
    own_after <- pmin(100, pmax(0, target_own + donor_gain))
    togo_after <- pmax(1, num(target$ydstogo[ti]) - donor_gain)
    synthetic <- data.frame(yardline_100 = 100 - own_after, ydstogo = num(target$ydstogo[ti]), yards_gained = donor_gain)
    keys <- paste(round(own_after, 2), round(togo_after, 2), sep = "|")
    need <- !keys %in% fd_decisions$key
    if (any(need)) {
      extra <- neutral_fourth_decisions(synthetic[need, , drop = FALSE])
      fd_decisions <- unique(rbind(fd_decisions, extra))
    }
    idx <- match(keys, fd_decisions$key)
    punt_idx <- !is.na(idx) & fd_decisions$punt_share[idx] > .5
    ok <- finite(own_after) & finite(togo_after)
    if (any(ok)) values[ok] <- ep_state(own_after[ok], 4, togo_after[ok])
    if (any(punt_idx & ok)) values[punt_idx & ok] <- punt_ev_at(target_own[punt_idx & ok], punt_values, punt_tree)
  }

  donor_features <- make_features(donors, centers, scales)$z
  target_features <- make_features(target, centers, scales)$z
  donor_matrix <- as.matrix(donor_features)
  target_matrix <- as.matrix(target_features)
  result <- rep(NA_real_, nt)
  for (i in seq_len(nt)) {
    row <- ((i - 1L) * nd + 1L):(i * nd)
    valid <- finite(values[row]) & complete.cases(donor_matrix)
    if (!any(valid)) next
    distance <- sqrt(rowSums((donor_matrix[valid, , drop = FALSE] -
      matrix(target_matrix[i, ], sum(valid), ncol(target_matrix), byrow = TRUE))^2))
    k <- min(length(distance), max(15L, ceiling(sqrt(length(distance)) * 2)))
    bandwidth <- max(sort(distance, partial = k)[k], .15)
    weight <- exp(-.5 * (distance / bandwidth)^2)
    if (!any(weight > 0)) weight[which.min(distance)] <- 1
    result[i] <- sum(weight * values[row][valid]) / sum(weight)
  }
  result
}

score_model <- function(train, target, centers, scales, punt_values, fd_decisions,
                        int_train, completion_train) {
  fit <- fit_probability_model(train, centers, scales)
  prob <- predict_probabilities(fit, target)
  int_tree <- fit_conditional_tree(int_train, "int_return_yards", c("air_yards", "own_start"))
  completion_tree <- fit_conditional_tree(completion_train, "yards_after_catch", c("air_yards", "own_start"))
  punt_tree <- fit_conditional_tree(punt_values, "value", "own_start")
  expected <- rep(0, nrow(target))
  missing_support <- rep(FALSE, nrow(target))
  cond <- matrix(NA_real_, nrow(target), ncol(prob), dimnames = list(NULL, colnames(prob)))
  for (k in colnames(prob)) {
    donors <- train[as.character(train$outcome) == k, , drop = FALSE]
    if (k == "interception") {
      cond[, k] <- interception_conditional_ep(target, int_train, int_tree)
    } else {
      cond[, k] <- conditional_state_values(target, donors, k, centers, scales,
        punt_values, fd_decisions, completion_tree, punt_tree)
    }
  }
  for (i in seq_len(nrow(target))) {
    active <- prob[i, ] > 1e-8
    if (any(active & !finite(cond[i, ]))) {
      missing_support[i] <- TRUE
      expected[i] <- NA_real_
    } else {
      expected[i] <- sum(prob[i, active] * cond[i, active])
    }
  }
  list(prob = prob, conditional = cond, expected = expected, missing_support = missing_support, fit = fit)
}

resample_game_rows <- function(d, draw) {
  if (!nrow(d)) return(d)
  # Index once per resample instead of rescanning the full league-wide donor
  # table separately for every sampled game.
  by_game <- split(seq_len(nrow(d)), as.character(d$game_id))
  idx <- unlist(by_game[draw], use.names = FALSE)
  d[idx, , drop = FALSE]
}

bootstrap_games <- function(d, punt_values, fd_decisions, int_train, completion_train,
                            reps = BOOTSTRAPS, seed = SEED) {
  set.seed(seed + nrow(d))
  games <- unique(as.character(d$game_id))
  scope_seasons <- unique(num(d$season))
  punt_scope <- punt_values[num(punt_values$season) %in% scope_seasons, , drop = FALSE]
  int_scope <- int_train[num(int_train$season) %in% scope_seasons, , drop = FALSE]
  completion_scope <- completion_train[num(completion_train$season) %in% scope_seasons, , drop = FALSE]
  out <- numeric()
  if (length(games) < 2L) return(out)
  for (b in seq_len(reps)) {
    draw <- sample(games, length(games), replace = TRUE)
    idx <- unlist(lapply(draw, function(g) which(as.character(d$game_id) == g)), use.names = FALSE)
    oob_games <- setdiff(games, unique(draw))
    oob_idx <- which(as.character(d$game_id) %in% oob_games)
    if (length(idx) < 10L || length(oob_idx) < 10L) next
    boot <- d[idx, , drop = FALSE]
    heldout <- d[oob_idx, , drop = FALSE]
    punt_boot <- resample_game_rows(punt_scope, draw)
    int_boot <- resample_game_rows(int_scope, draw)
    completion_boot <- resample_game_rows(completion_scope, draw)
    heldout$throwaway_value <- punt_ev_at(heldout$own_start, punt_boot)
    feat <- make_features(boot)
    scored <- score_model(boot, heldout, feat$centers, feat$scales,
                          punt_boot, fd_decisions, int_boot, completion_boot)
    delta <- scored$expected - heldout$throwaway_value
    if (any(finite(delta))) out <- c(out, mean(delta[finite(delta)]))
  }
  out
}

season_heldout_validation <- function(d, cutoff, scope_name, punt_values, fd_decisions,
                                      int_train, completion_train) {
  seasons <- sort(unique(num(d$season)))
  int_scope <- int_train[num(int_train$season) %in% seasons, , drop = FALSE]
  completion_scope <- completion_train[num(completion_train$season) %in% seasons, , drop = FALSE]
  classes <- sort(unique(as.character(d$outcome)))
  prob <- matrix(0, nrow(d), length(classes), dimnames = list(NULL, classes))
  conditional <- matrix(NA_real_, nrow(d), length(classes), dimnames = list(NULL, classes))
  expected <- rep(NA_real_, nrow(d))
  throwaway_value <- rep(NA_real_, nrow(d))
  fold <- rep(NA_integer_, nrow(d))
  for (s in seasons) {
    test_idx <- which(num(d$season) == s)
    train_idx <- which(num(d$season) != s)
    if (!length(test_idx) || length(train_idx) < 10L) next
    train <- d[train_idx, , drop = FALSE]
    test <- d[test_idx, , drop = FALSE]
    punt_fold <- punt_values[num(punt_values$season) %in% seasons &
                               num(punt_values$season) != s, , drop = FALSE]
    test$throwaway_value <- punt_ev_at(test$own_start, punt_fold)
    fs <- make_features(train)
    int_fold <- int_scope[num(int_scope$season) != s, , drop = FALSE]
    completion_fold <- completion_scope[num(completion_scope$season) != s, , drop = FALSE]
    scored <- score_model(train, test, fs$centers, fs$scales, punt_fold,
                          fd_decisions, int_fold, completion_fold)
    for (k in intersect(colnames(scored$prob), classes)) prob[test_idx, k] <- scored$prob[, k]
    for (k in intersect(colnames(scored$conditional), classes)) conditional[test_idx, k] <- scored$conditional[, k]
    expected[test_idx] <- scored$expected
    throwaway_value[test_idx] <- test$throwaway_value
    fold[test_idx] <- s
  }
  kept <- which(!is.na(fold))
  if (!length(kept)) return(list(metrics = data.frame(), calibration = data.frame(), predicted = list()))
  truth <- matrix(0, length(kept), length(classes), dimnames = list(NULL, classes))
  actual <- as.character(d$outcome[kept])
  truth[cbind(seq_along(kept), match(actual, classes))] <- 1
  p <- prob[kept, , drop = FALSE]
  p_actual <- pmax(1e-8, p[cbind(seq_along(kept), match(actual, classes))])
  logloss <- -log(p_actual)
  brier <- rowSums((p - truth)^2)
  metrics <- do.call(rbind, lapply(sort(unique(fold[kept])), function(s) {
    i <- which(fold[kept] == s)
    delta <- expected[kept[i]] - throwaway_value[kept[i]]
    data.frame(scope = scope_name, cutoff = cutoff, heldout_season = s, n = length(i),
      log_loss = mean(logloss[i]), multiclass_brier = mean(brier[i]),
      mean_predicted_delta_ep = mean_or_na(delta),
      valued_n = sum(finite(delta)))
  }))
  calibration <- do.call(rbind, lapply(sort(unique(fold[kept])), function(s) {
    i <- which(fold[kept] == s)
    do.call(rbind, lapply(classes, function(k) data.frame(scope = scope_name, cutoff = cutoff,
      heldout_season = s, outcome = k, n = length(i), observed_rate = mean(actual[i] == k),
      mean_predicted_probability = mean(p[i, k]))))
  }))
  list(metrics = metrics, calibration = calibration,
       predicted = list(prob = prob, conditional = conditional, expected = expected,
                        throwaway_value = throwaway_value,
                        missing_support = !finite(expected), fold = fold))
}

write_csv <- function(d, name) data.table::fwrite(d, file.path(TABLES, name), na = "")
rbind_fill <- function(xs) {
  cols <- unique(unlist(lapply(xs, names), use.names = FALSE))
  xs <- lapply(xs, function(x) {
    for (nm in setdiff(cols, names(x))) x[[nm]] <- NA
    x[, cols, drop = FALSE]
  })
  do.call(rbind, xs)
}

main <- function() {
  plays <- read_csv(file.path(DATA, "plays_2022_onward.csv.gz"))
  passes <- read_csv(file.path(DATA, "third_down_passes_2022_onward.csv"))
  punts <- read_csv(file.path(DATA, "punt_plays_2022_onward.csv"))
  raw_passes <- passes
  int_return_train <- prepare_interception_return_training(plays)
  completion_yac_train <- prepare_completion_yac_training(plays)
  manifest <- jsonlite::fromJSON(file.path(DATA, "manifest.json"), simplifyVector = FALSE)

  passes$outcome <- classify(passes)
  pass_attempt <- flag(passes$pass_attempt) & !flag(passes$sack) & !flag(passes$qb_scramble)
  deep <- finite(num(passes$air_yards))
  eligible_base <- pass_attempt & deep & (flag(passes$ftn_is_contested_ball) | is_dpi(passes))
  prefilter_rows <- which(eligible_base & num(passes$air_yards) >= min(CUTOFFS))
  zero_gain_states <- passes[prefilter_rows, , drop = FALSE]
  zero_gain_states$yards_gained <- 0
  zero_gain_decisions <- neutral_fourth_decisions(zero_gain_states)
  zero_keys <- paste(round(100 - num(zero_gain_states$yardline_100), 2),
                     round(num(zero_gain_states$ydstogo), 2), sep = "|")
  zero_decision_idx <- match(zero_keys, zero_gain_decisions$key)
  punt_filter <- !is.na(zero_decision_idx) &
    zero_gain_decisions$punt_share[zero_decision_idx] > .5
  passes <- passes[eligible_base, , drop = FALSE]
  passes$passes_zero_gain_punt_filter <- FALSE
  passes$passes_zero_gain_punt_filter[match(prefilter_rows, which(eligible_base))] <- punt_filter
  eligible_with_punt_filter <- rep(FALSE, nrow(raw_passes))
  eligible_with_punt_filter[which(eligible_base)] <- passes$passes_zero_gain_punt_filter
  passes <- passes[passes$passes_zero_gain_punt_filter, , drop = FALSE]
  passes$air_yards <- num(passes$air_yards)
  passes$ydstogo <- num(passes$ydstogo)
  passes$yardline_100 <- num(passes$yardline_100)
  passes$outcome <- factor(passes$outcome)

  manifest_rows <- function(name) as.numeric(manifest$files[[name]]$rows)
  audit <- data.frame(
    file = c("third_down_passes_2022_onward.csv", "punt_plays_2022_onward.csv"),
    observed_rows = c(nrow(raw_passes), nrow(punts)),
    manifest_rows = c(manifest_rows("third_down_passes_2022_onward.csv"), manifest_rows("punt_plays_2022_onward.csv"))
  )
  audit$rows_match_manifest <- audit$observed_rows == audit$manifest_rows
  audit <- rbind(audit, data.frame(
    file = c("third_down_passes_ftn_contested_true", "third_down_passes_ftn_contested_false",
             "third_down_passes_ftn_contested_missing", "eligible_contested_or_dpi_passes"),
    observed_rows = c(sum(flag(raw_passes$ftn_is_contested_ball)),
      sum(!is.na(raw_passes$ftn_is_contested_ball) & !flag(raw_passes$ftn_is_contested_ball)),
      sum(is.na(raw_passes$ftn_is_contested_ball)), sum(eligible_base)),
      manifest_rows = NA_real_, rows_match_manifest = NA
  ))
  audit <- rbind(audit, data.frame(
    file = "eligible_contested_or_dpi_passing_zero_gain_punt_filter",
    observed_rows = sum(eligible_with_punt_filter), manifest_rows = NA_real_, rows_match_manifest = NA
  ))
  audit <- rbind(audit, do.call(rbind, lapply(CUTOFFS, function(x) data.frame(
    file = paste0("eligible_cutoff_", x, "_air_yards_and_zero_gain_punt_filter"),
    observed_rows = sum(eligible_with_punt_filter & num(raw_passes$air_yards) >= x),
    manifest_rows = NA_real_, rows_match_manifest = NA
  ))))
  write_csv(audit, "data_audit.csv")

  write_csv(data.frame(
    metric = c("third_down_interceptions", "all_play_interception_return_training_rows",
               "interception_return_training_mean_yards", "interception_return_training_median_yards"),
    value = c(sum(flag(raw_passes$interception)), nrow(int_return_train),
              mean_or_na(int_return_train$int_return_yards), q_or_na(int_return_train$int_return_yards, .5))
  ), "interception_return_model_coverage.csv")
  write_csv(data.frame(
    metric = c("all_play_completion_yac_training_rows", "completion_yac_mean_yards",
               "completion_yac_median_yards"),
    value = c(nrow(completion_yac_train), mean_or_na(completion_yac_train$yards_after_catch),
              q_or_na(completion_yac_train$yards_after_catch, .5))
  ), "completion_yac_model_coverage.csv")

  punt_values <- punt_event_values(punts)
  punt_values$game_id <- punts$game_id
  punt_coverage <- data.frame(
    metric = c("punt_attempts", "punt_outcome_value_available", "punt_outcome_value_missing",
               "punt_distribution_tree_training_rows"),
    count = c(nrow(punts), sum(finite(punt_values$value)), sum(!finite(punt_values$value)),
              sum(finite(punt_values$value) & finite(punt_values$own_start)))
  )
  write_csv(punt_coverage, "punt_model_coverage.csv")

  # The public 4th-down model is evaluated on short-completion states under a
  # neutral public schedule fixture; EP at non-punt 4th-down states is the
  # pre-declared league-level continuation value.
  all_short <- passes[num(passes$air_yards) >= min(CUTOFFS) &
    as.character(passes$outcome) == "completion_short", , drop = FALSE]
  short_decisions <- neutral_fourth_decisions(all_short)
  fd_decisions <- unique(rbind(zero_gain_decisions, short_decisions))
  if (nrow(fd_decisions)) write_csv(fd_decisions, "fourth_down_decisions.csv") else {
    write_csv(data.frame(key = character(), own_after = numeric(), togo_after = numeric(), punt_share = numeric(), public_decision = character()), "fourth_down_decisions.csv")
  }

  result_rows <- list()
  scope_rows <- list()
  cand_outputs <- list()
  prob_outputs <- list()
  conditional_outputs <- list()
  dist_outputs <- list()
  cv_metric_outputs <- list()
  cv_calibration_outputs <- list()
  idx_result <- 0L
  idx_cand <- 0L
  idx_prob <- 0L
  idx_conditional <- 0L
  idx_dist <- 0L
  idx_cv <- 0L

  scopes <- list(
    `2022_2026_including_partial` = passes,
    completed_2022_2025 = passes[num(passes$season) <= 2025, , drop = FALSE]
  )
  for (scope_name in names(scopes)) {
    scope_passes <- scopes[[scope_name]]
    scope_punts <- if (scope_name == "completed_2022_2025") punts[num(punts$season) <= 2025, , drop = FALSE] else punts
    scope_punt_values <- punt_values[match(seq_len(nrow(scope_punts)), seq_len(nrow(punts))), , drop = FALSE]
    # Keep values aligned after the season filter.
    if (scope_name == "completed_2022_2025") {
      pidx <- which(num(punts$season) <= 2025)
      scope_punt_values <- punt_values[pidx, , drop = FALSE]
      scope_punt_values$game_id <- scope_punts$game_id
    }
    for (cutoff in CUTOFFS) {
      cand <- scope_passes[num(scope_passes$air_yards) >= cutoff, , drop = FALSE]
      cand <- cand[complete.cases(cand[, c("air_yards", "ydstogo", "yardline_100")]), , drop = FALSE]
      if (nrow(cand) < 10L) next
      cand$own_start <- 100 - num(cand$yardline_100)

      cv <- season_heldout_validation(cand, cutoff, scope_name, scope_punt_values,
        fd_decisions, int_return_train, completion_yac_train)
      scored <- list(prob = cv$predicted$prob, conditional = cv$predicted$conditional,
                     expected = cv$predicted$expected, missing_support = cv$predicted$missing_support)
      throwaway_value <- cv$predicted$throwaway_value
      delta <- scored$expected - throwaway_value
      boot <- bootstrap_games(cand, scope_punt_values, fd_decisions,
                              int_return_train, completion_yac_train,
                              reps = BOOTSTRAPS, seed = SEED + cutoff)
      ci <- if (length(boot)) stats::quantile(boot, c(.025, .975), na.rm = TRUE) else c(NA, NA)

      idx_result <- idx_result + 1L
      result_rows[[idx_result]] <- data.frame(
        scope = scope_name, air_yard_cutoff = cutoff, eligible_n = nrow(cand),
        valued_n = sum(finite(delta)), unvalued_n = sum(!finite(delta)),
        mean_arm_punt_ep = mean_or_na(scored$expected),
        mean_throwaway_punt_ep = mean_or_na(throwaway_value),
        mean_delta_ep = mean_or_na(delta), game_bootstrap_95ci_low = ci[1], game_bootstrap_95ci_high = ci[2],
        positive_delta_share = mean_or_na(delta > 0), game_clusters = length(unique(cand$game_id)),
        bootstrap_replicates = length(boot), model_missing_support_n = sum(scored$missing_support)
      )

      counts <- as.data.frame(table(cand$outcome), stringsAsFactors = FALSE)
      names(counts) <- c("outcome", "count")
      counts$rate <- counts$count / nrow(cand)
      counts$scope <- scope_name
      counts$air_yard_cutoff <- cutoff
      idx_dist <- idx_dist + 1L
      dist_outputs[[idx_dist]] <- counts

      idx_cand <- idx_cand + 1L
      cand_outputs[[idx_cand]] <- data.frame(
        scope = scope_name, cutoff = cutoff, game_id = cand$game_id, season = cand$season,
        nflverse_play_id = cand$nflverse_play_id, outcome = as.character(cand$outcome),
        air_yards = cand$air_yards, ydstogo = cand$ydstogo, own_yardline = cand$own_start,
        arm_punt_ep = scored$expected, throwaway_punt_ep = throwaway_value,
        delta_ep = delta, missing_conditional_support = scored$missing_support
      )

      if (scope_name == "2022_2026_including_partial") {
        pr <- as.data.frame(scored$prob, check.names = FALSE)
        pr$scope <- scope_name
        pr$cutoff <- cutoff
        pr$game_id <- cand$game_id
        pr$nflverse_play_id <- cand$nflverse_play_id
        idx_prob <- idx_prob + 1L
        prob_outputs[[idx_prob]] <- pr
        cr <- as.data.frame(scored$conditional, check.names = FALSE)
        names(cr) <- paste0("conditional_ep_", names(cr))
        cr$scope <- scope_name
        cr$cutoff <- cutoff
        cr$game_id <- cand$game_id
        cr$nflverse_play_id <- cand$nflverse_play_id
        idx_conditional <- idx_conditional + 1L
        conditional_outputs[[idx_conditional]] <- cr
        if (nrow(cv$metrics)) {
          idx_cv <- idx_cv + 1L
          cv_metric_outputs[[idx_cv]] <- cv$metrics
          cv_calibration_outputs[[idx_cv]] <- cv$calibration
        }
      }
    }
  }

  results <- do.call(rbind, result_rows)
  write_csv(results, "public_model_strategy_results.csv")
  write_csv(do.call(rbind, cand_outputs), "public_model_candidate_values.csv")
  write_csv(rbind_fill(prob_outputs), "outcome_probability_predictions.csv")
  write_csv(rbind_fill(conditional_outputs), "conditional_expected_path_values.csv")
  write_csv(do.call(rbind, dist_outputs), "outcome_category_rates.csv")
  write_csv(do.call(rbind, cv_metric_outputs), "season_heldout_validation.csv")
  write_csv(do.call(rbind, cv_calibration_outputs), "season_heldout_calibration.csv")

  # Realized conditional outcome-state summaries: distributional diagnostics,
  # not substitutes for the separate multinomial probability model.
  primary <- passes[num(passes$air_yards) >= PRIMARY_CUTOFF, , drop = FALSE]
  primary$own_start <- 100 - num(primary$yardline_100)
  primary$completion_yards <- ifelse(grepl("completion", as.character(primary$outcome)), num(primary$yards_gained), NA_real_)
  primary$yac <- num(primary$yards_after_catch)
  primary$interception_return_yards <- num(primary$interception_return_yards)
  primary$penalty_yards <- num(primary$penalty_yards)
  primary$next_drive_own_yardline <- num(primary$next_drive_own_yardline)
  categories <- unique(as.character(primary$outcome))
  vars <- c("completion_yards", "yac", "interception_return_yards", "penalty_yards", "next_drive_own_yardline")
  cond_summary <- do.call(rbind, lapply(categories, function(k) do.call(rbind, lapply(vars, function(v) {
    x <- primary[as.character(primary$outcome) == k, v]
    data.frame(outcome = k, variable = v, n = length(x), observed_n = sum(finite(x)), missing_rate = mean(!finite(x)),
      mean = mean_or_na(x), q25 = q_or_na(x, .25), median = q_or_na(x, .5), q75 = q_or_na(x, .75))
  }))))
  write_csv(cond_summary, "conditional_outcome_state_distributions.csv")

  # Public-model warm-up verification under the same neutral fourth-down state.
  warm_ep <- ep_state(50, 4, 5)
  warm_fd <- neutral_fourth_decisions(data.frame(
    game_id = "warm", season = 2025L, week = 1L, type = "reg", home_team = FD_HOME, away_team = FD_AWAY,
    posteam = FD_HOME, defteam = FD_AWAY, home_opening_kickoff = 1L, qtr = 1L,
    quarter_seconds_remaining = 900L, half_seconds_remaining = 3600L, score_differential = 0L,
    down = 4L, ydstogo = 5L, yardline_100 = 50, posteam_timeouts_remaining = 3L,
    defteam_timeouts_remaining = 3L, air_yards = 30, yards_gained = 0
  ))
  write_csv(data.frame(public_ep_neutral_4th_5 = warm_ep,
    nfl4th_bootstrap_draws = 200L,
    punt_best_share = if (nrow(warm_fd)) warm_fd$punt_share[1] else NA_real_,
    candidate_zero_gain_filter_states = nrow(zero_gain_decisions),
    candidate_short_completion_states = nrow(short_decisions)), "public_model_verification.csv")

  report <- c(
    "# Public-model arm-punt analysis", "",
    "## Method", "",
    "The primary estimate uses third-down pass attempts with at least 30 air yards, FTN charted contested status or qualifying defensive pass interference, and a pre-play fourth-down filter. For each candidate situation, nfl4th evaluates a hypothetical zero-yard result (4th down at the same line and distance); the situation is retained only when punt is best in more than 50% of 200 game-bootstrap fits. Cutoffs 25, 35, and 40 are reported as sensitivities. Outcome probabilities are fit with a regularized multinomial logistic model using only air yards, yards to go, and field position. Interception-return and completion-YAC distributions use the same shallow regression-tree architecture trained on all eligible league pass plays, with target depth and own-yard-line position as predictors; down and distance are omitted from these conditional models. The training observations in each terminal leaf form an empirical outcome distribution, retaining paired interception pick-location offsets and return lengths. Punt values use a separate tree conditioned on punt starting own-yard line. Other outcome states use kernel-weighted empirical conditional distributions from comparable candidate plays. Predicted states are evaluated with public EP and combined with outcome probabilities. All data-trained components are held out by season. No player or team identity predictors are used.", "",
    "Every predicted possession state is evaluated with nflfastR's public EP model at Q1 15:00, three timeouts, outdoor, using the latest complete season (2025). Punt outcomes are first mapped to offense-relative public EP values; the conditional punt-value distribution is estimated from the terminal leaf of the punt-starting-position tree. For retained candidates, incompletions and offensive penalties are valued as followed by a punt, consistent with the zero-gain fourth-down filter. A non-converting completion is evaluated at its actual fourth-down state with nfl4th; punt is used when it is best in more than half of 200 game-bootstrap fits. Otherwise, public EP at the fourth-down state is the continuation value.", "",
    "## Results", "",
    "| Scope | Air-yard cutoff | Eligible | Valued | Mean arm-punt EP | Mean throwaway-punt EP | Mean difference | Game-bootstrap 95% interval |", "|---|---:|---:|---:|---:|---:|---:|---:|"
  )
  for (i in seq_len(nrow(results))) {
    r <- results[i, ]
    report <- c(report, sprintf("| %s | %d | %d | %d | %.3f | %.3f | %.3f | [%.3f, %.3f] |",
      r$scope, r$air_yard_cutoff, r$eligible_n, r$valued_n, r$mean_arm_punt_ep,
      r$mean_throwaway_punt_ep, r$mean_delta_ep, r$game_bootstrap_95ci_low, r$game_bootstrap_95ci_high))
  }
  cv_all <- do.call(rbind, cv_metric_outputs)
  cv_means <- aggregate(cbind(log_loss, multiclass_brier) ~ cutoff, data = cv_all, FUN = mean)
  report <- c(report, "", "## Season-held-out validation", "",
    "Outcome probabilities and conditional state distributions were fit on the other seasons and used to predict the held-out season. Average held-out log loss and multiclass Brier score are:", "",
    "| Air-yard cutoff | Mean log loss | Mean multiclass Brier |", "|---:|---:|---:|")
  for (i in seq_len(nrow(cv_means))) report <- c(report, sprintf("| %d | %.3f | %.3f |", cv_means$cutoff[i], cv_means$log_loss[i], cv_means$multiclass_brier[i]))
  report <- c(report, "", "## Limits", "",
    "The observed arm-punt sample is selected and small, so these estimates are predictive standardized associations, not causal effects. Point estimates are held-out predictions: each play's season was excluded from fitting its outcome-probability, interception-return, completion-YAC, and punt-value models. The 100-replicate game bootstrap resamples training games and evaluates out-of-bag games; all data-trained conditional trees are refit on in-bag plays/punts, while public EP and nfl4th remain fixed. There were no observed non-converting completions at the analyzed 25-yard-or-deeper cutoffs, so nfl4th's >50% punt rule did not change those strategy estimates; its neutral 4th-and-5 bootstrap check is in `public_model_verification.csv`.", "",
    "Detailed tables are in `outputs/tables/`: strategy results, candidate-level values, outcome probabilities, conditional expected path values, category rates, observed conditional outcome-state summaries, interception-return, completion-YAC, and punt-model coverage, fourth-down decisions, and public-model verification.")
  writeLines(report, file.path(OUT, "public_model_results.md"), useBytes = TRUE)
  cat("Wrote public-model results and tables to outputs/.\n")
  print(results)
}

if (sys.nframe() == 0L) main()
