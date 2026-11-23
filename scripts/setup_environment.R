# Install the R packages needed for the planned public-model analysis.
# Run from the repository root with: Rscript scripts/setup_environment.R

repos <- c(
  nflverse = "https://nflverse.r-universe.dev",
  CRAN = "https://cloud.r-project.org"
)
options(repos = repos)

if (getRversion() < "4.1.0") {
  stop("R 4.1.0 or newer is required by the nflverse packages.")
}

# Keep packages out of the protected Program Files R installation. Install
# under the repository root because this execution host restricts writes to
# the project workspace.
workspace_lib <- file.path(getwd(), ".r-library", paste0(R.version$major, ".", R.version$minor))
dir.create(workspace_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(workspace_lib, .libPaths()))
Sys.setenv(R_USER_CACHE_DIR = file.path(getwd(), ".r-cache"))

required <- c("nflfastR", "nfl4th", "nnet", "rpart")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing)) {
  install.packages(missing, lib = workspace_lib, type = "binary")
}

still_missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(still_missing)) {
  stop("Package installation failed for: ", paste(still_missing, collapse = ", "))
}

versions <- vapply(required, function(pkg) as.character(utils::packageVersion(pkg)), character(1))
print(data.frame(package = names(versions), version = unname(versions), row.names = NULL))

message("Environment packages are ready. nfl4th bootstrap models may download on first use.")
