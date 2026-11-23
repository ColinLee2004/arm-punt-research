# Preload nfl4th's public schedule and bootstrap caches from a PowerShell
# session that has outbound access to nflverse/GitHub.
# Run from the repository root with:
#   & 'C:\Program Files\R\R-4.6.2\bin\Rscript.exe' scripts/cache_public_models.R

repo <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
lib <- file.path(repo, ".r-library", paste0(R.version$major, ".", R.version$minor))
if (!dir.exists(lib)) stop("Project R library not found: ", lib)
.libPaths(c(lib, .libPaths()))

# Make the nfl4th cache stable and visible to later Codex R processes. The
# default Windows cache is isolated per execution sandbox in this environment.
cache_root <- file.path(repo, ".r-cache")
dir.create(cache_root, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(R_USER_CACHE_DIR = cache_root)
options(nfl4th.force_cache = "true", nfl4th.keep_games = TRUE)

for (pkg in c("nflfastR", "nfl4th", "nflreadr")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Missing package: ", pkg)
}

# Ensure nfl4th can construct/cache its schedule feature table before scoring.
nflreadr::load_schedules()

# A real 2024 regular-season matchup supplies nfl4th's game-level context.
# The state itself is a neutral tied Q1 fourth down; it is only used to warm
# the public point and bootstrap model caches.
state <- data.frame(
  game_id = "2024_09_HOU_NYJ",
  season = 2024L,
  week = 9L,
  type = "reg",
  home_team = "NYJ",
  away_team = "HOU",
  posteam = "NYJ",
  defteam = "HOU",
  home_opening_kickoff = 1L,
  qtr = 1L,
  quarter_seconds_remaining = 900L,
  half_seconds_remaining = 3600L,
  score_differential = 0L,
  down = 4L,
  ydstogo = 5L,
  yardline_100 = 50,
  posteam_timeouts_remaining = 3L,
  defteam_timeouts_remaining = 3L
)

ep_state <- nflfastR::calculate_expected_points(transform(
  state,
  roof = "outdoors"
))
if (!is.finite(ep_state$ep[[1L]])) stop("Public nflfastR EP prediction failed.")

fourth <- nfl4th::add_4th_probs(state, bootstrap = TRUE)
required <- c("go_wp_boot", "fg_wp_boot", "punt_wp_boot")
if (!all(required %in% names(fourth))) {
  stop("nfl4th returned no bootstrap option values: ",
       paste(setdiff(required, names(fourth)), collapse = ", "))
}

go <- fourth$go_wp_boot[[1L]]
fg <- fourth$fg_wp_boot[[1L]]
punt <- fourth$punt_wp_boot[[1L]]
if (length(go) == 201L && length(fg) == 201L && length(punt) == 201L) {
  # nfl4th places its shipped fit first, followed by 200 game-bootstrap fits.
  go <- go[-1L]
  fg <- fg[-1L]
  punt <- punt[-1L]
}
if (!(length(go) == length(fg) && length(fg) == length(punt) && length(punt) > 0L)) {
  stop("nfl4th bootstrap option vectors have inconsistent lengths.")
}
punt_share <- mean(punt > pmax(go, fg), na.rm = TRUE)

cat("nflfastR EP at warm-up state:", ep_state$ep[[1L]], "\n")
cat("nfl4th bootstrap draws:", length(punt), "\n")
cat("Share selecting punt at warm-up state:", punt_share, "\n")
cat("nfl4th cache:", nfl4th:::nfl4th_games_path(), "\n")
cat("nfl4th bootstrap cache:", nfl4th:::nfl4th_bootstrap_path(), "\n")
cat("Caches are stored under .r-cache for subsequent runs.\n")
