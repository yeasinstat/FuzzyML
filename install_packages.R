# ============================================================
# FuzzyML - one-time package installation
# Run once:  source("install_packages.R")
# ============================================================
options(repos = c(CRAN = "https://cloud.r-project.org"), timeout = 1200)

pkgs <- c(
  # web application
  "shiny", "bslib", "DT", "plotly", "shinycssloaders",
  # data input / output
  "readxl", "openxlsx", "lubridate",
  # learners (CatBoost is installed separately below)
  "nnet", "randomForest", "e1071", "xgboost", "lightgbm", "rpart",
  # helper to install CatBoost
  "remotes"
)

for (attempt in 1:2) {
  missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(missing)) install.packages(missing, type = getOption("pkgType"))
}

# ---- CatBoost (not on CRAN: installed from the official GitHub release) ----
if (!requireNamespace("catboost", quietly = TRUE)) {
  ver <- "1.2.8"
  os <- Sys.info()[["sysname"]]
  file <- switch(os,
                 Windows = paste0("catboost-R-windows-x86_64-", ver, ".tgz"),
                 Darwin  = paste0("catboost-R-darwin-universal2-", ver, ".tgz"),
                 paste0("catboost-R-linux-x86_64-", ver, ".tgz"))
  url <- paste0("https://github.com/catboost/catboost/releases/download/v", ver, "/", file)
  message("Installing CatBoost from ", url)
  ok <- tryCatch({
    remotes::install_url(url, INSTALL_opts = c("--no-multiarch", "--no-test-load"))
    TRUE
  }, error = function(e) { message("CatBoost could not be installed: ", conditionMessage(e)); FALSE })
  if (!ok) message("FuzzyML still works; only IFCatBoost will show 'package not installed'.")
}

missing <- c(pkgs, "catboost")[!c(pkgs, "catboost") %in% rownames(installed.packages())]
if (length(missing)) {
  message("\nStill missing: ", paste(missing, collapse = ", "),
          "\nRun source(\"install_packages.R\") again or try: options(repos = c(CRAN = \"https://cran.rstudio.com\"))")
} else {
  message("\nAll packages installed. Start the app with:  shiny::runApp()")
}
