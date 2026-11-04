# FuzzyML — ICAR-IASRI

**Intuitionistic Fuzzy Machine Learning for Time Series Forecasting** — an R Shiny web application with seven models:
IFANN, IFRF, IFSVR, IFXGBoost, IFLightGBM, IFAdaBoost and IFCatBoost.

## Run locally

```r
setwd("path/to/FuzzyML_App")
source("install_packages.R")      # first time only (also installs CatBoost from its GitHub release)
shiny::runApp()
```

R 4.1 or later is required.

## Folder structure

| Path | Purpose |
|---|---|
| `app.R` | User interface (Home, Model, Instructions, Developers) and server logic |
| `R/ifml_engine.R` | Intuitionistic fuzzy engine: IFCM clustering, lag + membership + non-membership features, grid-search tuning, the seven learners |
| `R/data_utils.R` | File reading and automatic time-column recognition |
| `R/plots.R` | Interactive plots |
| `www/` | Stylesheet, ICAR-IASRI logo and developer photographs |
| `sample_data/` | Sample file (Excel and CSV) offered for download and "Load sample data" |

## Method

* Intuitionistic fuzzy c-means (Chaira, 2011) with Yager non-membership `nu = (1 - mu^alpha)^(1/alpha)` on the training set.
* Inputs for target `y_t` with lag `L`: `y_{t-1..t-L}` and their membership and non-membership degrees in every cluster (`p = L(1 + 2C)`).
* Every hyperparameter, including the lag, can be a single value or a `min, max, step` range; range parameters are tuned
  jointly by grid search with time-ordered hold-out or expanding-window cross-validation (RMSE, MAE or MAPE).
* IFANN and IFRF reproduce the IFANN and IFRF R packages exactly (same seed gives identical results).
* IFAdaBoost implements AdaBoost.R2 (Drucker, 1997) with `rpart` regression trees.

## Learner packages

| Model | R package |
|---|---|
| IFANN | nnet |
| IFRF | randomForest |
| IFSVR | e1071 |
| IFXGBoost | xgboost |
| IFLightGBM | lightgbm |
| IFAdaBoost | rpart |
| IFCatBoost | catboost (GitHub release, installed by `install_packages.R`) |

A model whose package is missing is marked "package not installed" in the app; the other models still work.

## Deployment

* **shinyapps.io**: `rsconnect::deployApp()`. CatBoost may not be available there; the other six models work.
* **Shiny Server**: copy the folder to the server's app directory and make it writable for `visitor_count.txt`.
