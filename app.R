# ============================================================
#  FuzzyML
#  Intuitionistic Fuzzy Machine Learning for Time Series Forecasting
#  ICAR-Indian Agricultural Statistics Research Institute, New Delhi
# ------------------------------------------------------------
#  Run locally :  shiny::runApp("path/to/FuzzyML_App")
#  Packages    :  see install_packages.R
#  Engine      :  R/ifml_engine.R  |  Data: R/data_utils.R  |  Plots: R/plots.R
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(plotly)
  library(shinycssloaders)
})
# Files in R/ are sourced automatically by Shiny.

options(shiny.maxRequestSize = 50 * 1024^2)

APP_TITLE   <- "FuzzyML"
APP_TAGLINE <- "Intuitionistic Fuzzy Machine Learning for Time Series Forecasting"
IASRI_URL   <- "https://www.iasri.res.in/"
COUNTER_FILE <- "visitor_count.txt"
SAMPLE_FILE  <- "sample_data/Sample_FuzzyML_data.csv"

increment_counter <- function() {
  n <- suppressWarnings(tryCatch(as.numeric(readLines(COUNTER_FILE, warn = FALSE)[1]), error = function(e) 0))
  if (length(n) == 0 || is.na(n)) n <- 0
  n <- n + 1
  tryCatch(writeLines(as.character(n), COUNTER_FILE), error = function(e) NULL)
  n
}

DT_STYLE <- local({
  st <- tryCatch(eval(formals(DT::datatable)$style), error = function(e) character())
  if ("bootstrap5" %in% st) "bootstrap5" else "bootstrap4"
})

spin <- function(x) shinycssloaders::withSpinner(x, type = 6, color = "#5b3fd1")
stat_box <- function(label, value, icon_name, cls = "") {
  div(class = paste("stat", cls), div(class = "si", icon(icon_name)),
      div(div(class = "sv", value), div(class = "sl", label)))
}
help_txt <- function(...) div(class = "help", ...)
fmt <- function(x, d = 4) ifelse(is.na(x), "-", formatC(x, digits = d, format = "fg", flag = "#"))

dt_out <- function(df, digits = 4, page = 10, filter = "none", dom = "Bfrtip") {
  if (is.null(df) || !nrow(df)) return(DT::datatable(data.frame(Message = "Nothing to show"), rownames = FALSE, style = DT_STYLE, options = list(dom = "t")))
  num <- names(df)[vapply(df, function(v) is.numeric(v) && !is.integer(v), logical(1)) & !names(df) %in% c("Combination", "Lag", "Rank")]
  dt <- DT::datatable(df, rownames = FALSE, filter = filter, extensions = "Buttons", style = DT_STYLE,
                      class = "table-sm table-striped table-hover nowrap",
                      options = list(dom = dom, buttons = c("copy", "csv", "excel"), pageLength = page,
                                     scrollX = TRUE, lengthMenu = c(10, 25, 50, 100)))
  if (length(num)) dt <- DT::formatRound(dt, num, digits)
  dt
}

# ============================================================
#  UI
# ============================================================

banner <- div(class = "top-banner",
              tags$a(href = IASRI_URL, target = "_blank", title = "Visit ICAR-IASRI website",
                     tags$img(src = "img/iasri_logo.png", alt = "ICAR-Indian Agricultural Statistics Research Institute")))

footer <- tags$footer(
  class = "site-footer",
  div(class = "fi",
      div(div(class = "copy", HTML("&copy; 2026 "),
              tags$a(href = IASRI_URL, target = "_blank", "ICAR-Indian Agricultural Statistics Research Institute"),
              ", New Delhi. All rights reserved."),
          div(class = "addr", "Library Avenue, PUSA, New Delhi - 110012, India")),
      div(class = "visitor", icon("users"),
          div(div(class = "vl", "Visitors"), div(class = "vn", textOutput("visitor_count", inline = TRUE))))))

model_icon_cls <- c(IFANN = "", IFRF = "g", IFSVR = "o", IFXGBoost = "", IFLightGBM = "g", IFAdaBoost = "o", IFCatBoost = "")

# ---------------- HOME ----------------
home_tab <- tabPanel(
  "Home", value = "home", icon = icon("house"),
  div(class = "hero",
      div(class = "hero-inner",
          div(span(class = "pill", icon("seedling"), " ICAR-IASRI | Statistical Web Tool"),
              h1(APP_TITLE, tags$br(), span(class = "accent", style = "font-size:1.55rem;", APP_TAGLINE)),
              p(class = "lead",
                "Forecast any time series with seven machine learning models that see the data through ",
                tags$b("intuitionistic fuzzy sets"), ". Each past value enters the model together with its degree of ",
                tags$b("membership"), " and ", tags$b("non-membership"), " in fuzzy clusters, so the model also learns from ",
                "the uncertainty (hesitation) in the data."),
              div(class = "hero-actions",
                  actionButton("go_model", label = tagList(icon("play"), " Start Forecasting"), class = "btn-gold"),
                  actionButton("go_instr", label = tagList(icon("book-open"), " How to use"), class = "btn-ghost"))),
          div(class = "hero-card",
              h5(icon("diagram-project"), " What the tool does"),
              div(class = "mini-step", div(class = "num", "1"), div("Upload a time series (or load the sample) and choose the series to forecast")),
              div(class = "mini-step", div(class = "num", "2"), div("Split the series into a training set and a test set")),
              div(class = "mini-step", div(class = "num", "3"), div("Choose IFANN, IFRF, IFSVR, IFXGBoost, IFLightGBM, IFAdaBoost or IFCatBoost")),
              div(class = "mini-step", div(class = "num", "4"), div("Give each hyperparameter, including the lag, as a single value or a search range")),
              div(class = "mini-step", div(class = "num", "5"), div("Get accuracy, forecasts, tuning results, fuzzy clusters and feature importance"))))),
  div(class = "page-wrap",
      div(class = "eyebrow", "About the method"),
      h2(class = "section-title", "Why intuitionistic fuzzy machine learning?"),
      p(class = "section-sub",
        "Classical fuzzy sets give each value a single membership degree. Intuitionistic fuzzy sets (Atanassov, 1986) add a separate ",
        "non-membership degree, and the gap between the two measures hesitation. Feeding these degrees to a machine learning model, ",
        "together with the lagged values, gives it a richer description of where each observation lies in the range of the series."),
      div(class = "feature-grid four",
          lapply(names(IF_MODELS), function(k) {
            mm <- IF_MODELS[[k]]
            div(class = paste("feature", model_icon_cls[[k]]), div(class = "ico", icon(mm$icon)),
                h4(mm$name), div(class = "muted", style = "margin:-4px 0 8px;font-weight:600;", mm$full), p(mm$blurb))
          }),
          div(class = "feature o", div(class = "ico", icon("magnifying-glass-chart")),
              h4("Grid-search tuning"), div(class = "muted", style = "margin:-4px 0 8px;font-weight:600;", "Lag and hyperparameters"),
              p("Every hyperparameter, including the lag order, can be fixed or searched over a min-max-step range with time-ordered hold-out or cross-validation."))),
      br(),
      div(class = "panel",
          withMathJax(),
          h3(icon("square-root-variable"), " Model in brief"),
          p(tags$b("1. Intuitionistic fuzzy c-means (IFCM)."), " The training values are grouped into \\(C\\) clusters with centres \\(v_k\\). ",
            "The membership degree of a value \\(x\\) in cluster \\(k\\) is the fuzzy c-means degree"),
          div(class = "equation", "$$\\mu_k(x) = \\left[ \\sum_{j=1}^{C} \\left( \\frac{|x - v_k|}{|x - v_j|} \\right)^{2/(m-1)} \\right]^{-1}$$"),
          p("and the non-membership and hesitation degrees come from the Yager generator (\\(0 < \\alpha \\le 1\\); \\(\\alpha = 1\\) gives ordinary fuzzy sets):"),
          div(class = "equation", "$$\\nu_k(x) = \\left(1 - \\mu_k(x)^{\\alpha}\\right)^{1/\\alpha}, \\qquad \\pi_k(x) = 1 - \\mu_k(x) - \\nu_k(x)$$"),
          p("Cluster centres are updated with the hesitation-augmented memberships \\(\\mu^{*} = \\mu + \\pi\\) (Chaira, 2011) until convergence."),
          p(tags$b("2. Inputs."), " For a target \\(y_t\\) and lag order \\(L\\), the input vector holds the lagged values and their degrees in every cluster:"),
          div(class = "equation", "$$\\mathbf{x}_t = \\big[\\, y_{t-1},\\dots,y_{t-L},\\; \\mu_k(y_{t-l}),\\; \\nu_k(y_{t-l}) \\,\\big]_{l=1..L,\\;k=1..C}, \\qquad p = L\\,(1 + 2C)$$"),
          p(tags$b("3. Learner."), " A machine learning model \\(\\hat{y}_t = f(\\mathbf{x}_t)\\) is trained on the training set; \\(f\\) is a neural network, random forest, ",
            "support vector regression, XGBoost, LightGBM, AdaBoost.R2 or CatBoost. Clustering and scaling use only the training data, and only past values enter the inputs, ",
            "so the test forecasts never use information from the values being forecast."),
          h3(style = "margin-top:22px;", icon("list-check"), " Outputs you get"),
          tags$ul(
            tags$li("Training and test accuracy (RMSE, MAE, MAPE) and the selected hyperparameters"),
            tags$li("Interactive plot of actual, fitted and predicted values and a table of test-set predictions"),
            tags$li("Grid-search history with the validation score of every combination"),
            tags$li("Intuitionistic fuzzy cluster centres with membership, non-membership and hesitation curves"),
            tags$li("Feature importance of the lagged values and fuzzy degrees"),
            tags$li("A comparison table of all models run in the session and an Excel workbook of every result"))),
      div(class = "panel",
          h3(icon("book"), " Key references"),
          tags$ol(class = "ref-list",
                  tags$li("Atanassov, K.T. (1986). Intuitionistic fuzzy sets. Fuzzy Sets and Systems, 20(1), 87-96."),
                  tags$li("Chaira, T. (2011). A novel intuitionistic fuzzy C means clustering algorithm and its application to medical images. Applied Soft Computing, 11(2), 1711-1717."),
                  tags$li("Yolcu, O.C. and Yolcu, U. (2023). A novel intuitionistic fuzzy time series prediction model with cascaded structure for financial time series. Expert Systems with Applications, 215, 119336."),
                  tags$li("Venables, W.N. and Ripley, B.D. (2002). Modern Applied Statistics with S (4th ed.). Springer, New York."),
                  tags$li("Breiman, L. (2001). Random forests. Machine Learning, 45(1), 5-32."),
                  tags$li("Drucker, H., Burges, C.J.C., Kaufman, L., Smola, A. and Vapnik, V. (1997). Support vector regression machines. Advances in Neural Information Processing Systems 9, 155-161."),
                  tags$li("Chen, T. and Guestrin, C. (2016). XGBoost: A scalable tree boosting system. Proceedings of the 22nd ACM SIGKDD International Conference on Knowledge Discovery and Data Mining, 785-794."),
                  tags$li("Ke, G., Meng, Q., Finley, T., Wang, T., Chen, W., Ma, W., Ye, Q. and Liu, T.-Y. (2017). LightGBM: A highly efficient gradient boosting decision tree. Advances in Neural Information Processing Systems 30."),
                  tags$li("Drucker, H. (1997). Improving regressors using boosting techniques. Proceedings of the 14th International Conference on Machine Learning, 107-115."),
                  tags$li("Prokhorenkova, L., Gusev, G., Vorobev, A., Dorogush, A.V. and Gulin, A. (2018). CatBoost: unbiased boosting with categorical features. Advances in Neural Information Processing Systems 31.")))))

# ---------------- MODEL: step panels ----------------
upload_panel <- tagList(
  div(class = "note ok", style = "display:flex;justify-content:space-between;align-items:center;flex-wrap:wrap;gap:10px;",
      span(icon("lightbulb"), " New here? Load the built-in sample (monthly values of five commodity series, 2015-2024) to try the tool."),
      actionButton("load_sample", tagList(icon("database"), " Load sample data"), class = "btn-primary-x")),
  fluidRow(
    column(5, card(class = "card-y",
                   card_header(icon("circle-info"), "What should the file contain?", span(class = "sub", "One sheet or CSV, one row per time point")),
                   card_body(
                     tags$ul(style = "padding-left:18px;margin-bottom:8px;",
                             tags$li(tags$b("First row:"), " column names."),
                             tags$li(tags$b("Time column (optional):"), " dates, months, quarters, years, week numbers, date-times or any labels. It is read automatically and shown back exactly as written."),
                             tags$li(tags$b("One or more numeric series columns:"), " you choose which one to forecast. At least 30 observations are recommended."),
                             tags$li(tags$b("Formats:"), " .xlsx, .xls or .csv. Missing values can be filled by interpolation.")),
                     tags$table(class = "mini-table",
                                tags$tr(tags$th("Date"), tags$th("Y1"), tags$th("Y2"), tags$th("...")),
                                tags$tr(tags$td("2015-01-01"), tags$td("768.20"), tags$td("1266.65"), tags$td("...")),
                                tags$tr(tags$td("2015-02-01"), tags$td("928.81"), tags$td("1178.63"), tags$td("...")),
                                tags$tr(tags$td("..."), tags$td("..."), tags$td("..."), tags$td("..."))),
                     div(class = "sample-row", icon("file-arrow-down"), "Sample file:",
                         downloadButton("dl_sample_xlsx", "Excel", class = "btn-soft"),
                         downloadButton("dl_sample_csv", "CSV", class = "btn-soft"))))),
    column(7, card(class = "card-x",
                   card_header(icon("upload"), "Upload your file", span(class = "sub", "Excel or CSV")),
                   card_body(
                     div(class = "upload-zone",
                         fileInput("file", NULL, accept = c(".csv", ".xlsx", ".xls", ".txt", ".tsv"),
                                   buttonLabel = tagList(icon("upload"), " Browse"), placeholder = "CSV, XLSX or XLS file", width = "100%")),
                     uiOutput("file_ui"),
                     fluidRow(
                       column(6, selectInput("time_fmt", "Time format", time_format_choices, "auto")),
                       column(6, selectInput("missing", "Missing values", c("Fill by linear interpolation" = "interpolate", "Leave (series with gaps cannot be modelled)" = "none"), "interpolate"))))))),
  uiOutput("data_status"),
  conditionalPanel("output.has_data",
                   card(card_header(icon("chart-line"), "Selected series"),
                        card_body(fluidRow(column(8, spin(plotlyOutput("pl_series", height = "360px"))),
                                           column(4, DTOutput("tbl_stats"))))),
                   card(card_header(icon("table"), "Data preview"), card_body(DTOutput("tbl_preview"))),
                   div(style = "text-align:right;", actionButton("to_split", tagList("Next: Train-test split ", icon("arrow-right")), class = "btn-primary-x"))))

split_panel <- tagList(
  conditionalPanel("!output.has_data", div(class = "note warn", icon("hourglass-half"), " Upload data first.")),
  conditionalPanel("output.has_data",
                   card(card_header(icon("scissors"), "Train-test split", span(class = "sub", "The first part of the series trains the model; the last part tests it")),
                        card_body(
                          fluidRow(column(5, sliderInput("split", "Training share", min = 0.5, max = 0.95, value = 0.8, step = 0.01, width = "100%"),
                                          help_txt("The test set is never used for fitting or tuning. Fuzzy clusters and scaling are estimated from the training set only.")),
                                   column(7, uiOutput("split_info"))),
                          spin(plotlyOutput("pl_split", height = "380px")))),
                   div(style = "text-align:right;", actionButton("to_model", tagList("Next: Select model ", icon("arrow-right")), class = "btn-primary-x"))))

model_choice_panel <- tagList(
  card(card_header(icon("cubes"), "Select the model", span(class = "sub", "All models use the same intuitionistic fuzzy inputs; only the learner differs")),
       card_body(uiOutput("model_cards"))),
  div(style = "text-align:right;", actionButton("to_hp", tagList("Next: Hyperparameters ", icon("arrow-right")), class = "btn-primary-x")))

hp_panel <- tagList(
  uiOutput("hp_title"),
  card(card_header(icon("sliders"), "Hyperparameters", span(class = "sub", "Choose 'Single value' to fix a parameter or 'Range' to tune it by grid search (min, max, step). Both are pre-filled with defaults.")),
       card_body(uiOutput("hp_ui"), uiOutput("grid_info"))),
  fluidRow(
    column(6, card(card_header(icon("circle-nodes"), "Intuitionistic fuzzy settings"),
                   card_body(fluidRow(
                     column(6, numericInput("if_class", "Number of clusters (C)", 3, min = 2, max = 10, step = 1),
                            numericInput("if_alpha", "Yager exponent (alpha)", 0.85, min = 0.01, max = 1, step = 0.05),
                            help_txt("In (0, 1]; 1 gives ordinary fuzzy sets.")),
                     column(6, numericInput("if_m", "Fuzzifier (m)", 2, min = 1.1, max = 5, step = 0.1),
                            numericInput("if_maxiter", "Maximum IFCM iterations", 100, min = 10, max = 1000, step = 10),
                            numericInput("if_tol", "Convergence tolerance", 1e-5, min = 1e-8, max = 1e-2, step = 1e-5)))))),
    column(6, card(card_header(icon("check-double"), "Validation and forecasting"),
                   card_body(fluidRow(
                     column(6, selectInput("val_method", "Validation (for tuning)", c("Hold-out" = "holdout", "Time-series cross-validation" = "tscv"), "holdout"),
                            numericInput("val_split", "Validation share of training set", 0.2, min = 0.05, max = 0.5, step = 0.05),
                            conditionalPanel("input.val_method == 'tscv'", numericInput("val_folds", "Number of folds", 3, min = 2, max = 10, step = 1))),
                     column(6, selectInput("metric", "Tuning metric", c("RMSE", "MAE", "MAPE"), "RMSE"),
                            selectInput("fc_type", "Forecast type", c("One-step ahead" = "one-step", "Recursive (multi-step)" = "recursive"), "one-step"),
                            numericInput("seed", "Random seed", 123, min = 1, step = 1))))))),
  div(class = "run-bar",
      div(h4(style = "margin:0;", icon("gears"), " Ready to run"), uiOutput("run_summary")),
      div(actionButton("reset_hp", tagList(icon("rotate-left"), " Reset defaults"), class = "btn-soft", style = "margin-right:10px;"),
          actionButton("run", tagList(icon("play"), " Run Model"), class = "btn-run"))))

results_panel <- tagList(
  uiOutput("results_header"),
  conditionalPanel(
    "output.has_results",
    navset_card_tab(
      id = "res_tabs",
      nav_panel("Accuracy", icon = icon("bullseye"),
                uiOutput("interpretation"),
                fluidRow(column(7, h5("Forecast accuracy"), DTOutput("tbl_acc")),
                         column(5, h5("Selected hyperparameters"), DTOutput("tbl_params"))),
                br(), h5("Settings used"), DTOutput("tbl_settings")),
      nav_panel("Forecast plot", icon = icon("chart-line"),
                spin(plotlyOutput("pl_forecast", height = "460px")),
                fluidRow(column(6, h5("Test set: actual vs predicted"), spin(plotlyOutput("pl_scatter", height = "360px"))),
                         column(6, h5("Test set errors"), DTOutput("tbl_err_summary")))),
      nav_panel("Predictions", icon = icon("table"),
                radioButtons("pred_set", NULL, c("Test set" = "test", "Training set (fitted)" = "train"), "test", inline = TRUE),
                DTOutput("tbl_pred")),
      nav_panel("Tuning", icon = icon("magnifying-glass-chart"),
                uiOutput("tuning_info"),
                conditionalPanel("output.has_tuning", spin(plotlyOutput("pl_tuning", height = "360px")), DTOutput("tbl_tuning"))),
      nav_panel("Fuzzy clusters", icon = icon("circle-nodes"),
                div(class = "note", icon("circle-info"), " Solid lines: membership (mu); dashed lines: non-membership (nu); click a legend item to show the hesitation (pi) curve. Clusters are estimated from the training set."),
                fluidRow(column(8, spin(plotlyOutput("pl_membership", height = "440px"))),
                         column(4, h5("Cluster centres"), DTOutput("tbl_centres"), uiOutput("ifcm_info")))),
      nav_panel("Feature importance", icon = icon("ranking-star"),
                uiOutput("imp_info"),
                fluidRow(column(8, spin(plotlyOutput("pl_importance", height = "520px"))),
                         column(4, h5("By feature group"), DTOutput("tbl_imp_group")))),
      nav_panel("Model comparison", icon = icon("scale-balanced"),
                div(class = "note", icon("circle-info"), " Every run in this session is listed here. Run other models (or other settings) on the same series and split to compare them."),
                DTOutput("tbl_compare"),
                spin(plotlyOutput("pl_compare", height = "420px")),
                actionButton("clear_runs", tagList(icon("trash"), " Clear comparison"), class = "btn-soft")),
      nav_panel("Download", icon = icon("download"),
                fluidRow(
                  column(6, card(card_header(icon("file-excel"), "Results workbook", span(class = "sub", "Current model")),
                                 card_body(p("Sheets: Accuracy, Parameters, Test_Predictions, Training_Fitted, Tuning_History, IFCM_Centres, Feature_Importance, Settings, Data_Used."),
                                           downloadButton("dl_excel", "Download Excel workbook", class = "btn-dl")))),
                  column(6, card(card_header(icon("scale-balanced"), "Model comparison", span(class = "sub", "All runs of this session")),
                                 card_body(p("One row per run with the selected hyperparameters and training / test accuracy."),
                                           downloadButton("dl_compare", "Download comparison (Excel)", class = "btn-dl")))))))))

model_tab <- tabPanel(
  "Model", value = "model", icon = icon("flask"),
  div(class = "page-wrap",
      div(class = "eyebrow", "Forecasting workspace"),
      h2(class = "section-title", "Build an intuitionistic fuzzy forecasting model"),
      p(class = "section-sub", "Follow the steps from left to right. Every choice has a sensible default."),
      div(class = "method-tabs",
          tabsetPanel(id = "steps", type = "pills",
                      tabPanel("1. Upload data", value = "upload", icon = icon("upload"), upload_panel),
                      tabPanel("2. Train-test split", value = "split", icon = icon("scissors"), split_panel),
                      tabPanel("3. Select model", value = "choose", icon = icon("cubes"), model_choice_panel),
                      tabPanel("4. Hyperparameters", value = "hp", icon = icon("sliders"), hp_panel),
                      tabPanel("5. Results", value = "results", icon = icon("square-poll-vertical"), results_panel)))))

# ---------------- INSTRUCTIONS ----------------
ins_step <- function(n, title, ...) div(class = "ins-step", div(class = "n", n), div(h4(title), ...))
type_text <- c(int = "positive integer", num0 = "number >= 0", pos = "number > 0", prop = "proportion in (0, 1]")

hp_reference <- tags$table(class = "param-table",
  tags$tr(tags$th("Model"), tags$th("Hyperparameter"), tags$th("Default single value"), tags$th("Default range (min, max, step)"), tags$th("Meaning")),
  tags$tr(tags$td("All models"), tags$td(LAG_PARAM$label), tags$td(LAG_PARAM$single), tags$td(paste(LAG_PARAM$range, collapse = ", ")), tags$td(LAG_PARAM$help)),
  lapply(names(IF_MODELS), function(k) lapply(IF_MODELS[[k]]$params, function(p)
    tags$tr(tags$td(k), tags$td(p$label), tags$td(p$single), tags$td(paste(p$range, collapse = ", ")),
            tags$td(paste0(p$help, " (", type_text[[p$type]], ")"))))))

instructions_tab <- tabPanel(
  "Instructions", value = "instructions", icon = icon("book-open"),
  div(class = "page-wrap",
      div(class = "eyebrow", "User guide"),
      h2(class = "section-title", "How to use FuzzyML"),
      p(class = "section-sub", "Upload a series, split it, pick a model, set the hyperparameters and run. Results can be compared across models and downloaded."),
      ins_step("1", "Prepare your file",
               tags$ul(
                 tags$li("One sheet (Excel) or a CSV file with column names in the first row."),
                 tags$li("An optional time column in any format: dates, months (Jan-2015, 2015-01), quarters (2015Q1), years, financial years (2015-16), week numbers, date-times or labels."),
                 tags$li("One or more numeric columns. Each column is a separate series; you choose which one to forecast."),
                 tags$li("At least 30 observations are recommended; longer series allow larger lags.")),
               tags$table(class = "mini-table",
                          tags$tr(tags$th("Date"), tags$th("Y1"), tags$th("Y2"), tags$th("...")),
                          tags$tr(tags$td("2015-01-01"), tags$td("768.20"), tags$td("1266.65"), tags$td("...")),
                          tags$tr(tags$td("2015-02-01"), tags$td("928.81"), tags$td("1178.63"), tags$td("..."))),
               p(class = "muted", icon("download"), " A sample file (Excel and CSV) is available on Model > 1. Upload data, together with a 'Load sample data' button.")),
      ins_step("2", "Upload and choose the series",
               tags$ul(
                 tags$li("Upload the file; for Excel choose the sheet. Confirm the time column (or choose 'No time column' to use row order)."),
                 tags$li("Select the series to forecast. Its plot, summary statistics and a data preview are shown."),
                 tags$li("Missing values inside the series are filled by linear interpolation (default)."))),
      ins_step("3", "Split into training and test sets",
               tags$ul(
                 tags$li("Use the slider to set the training share (default 0.8 = first 80% for training, last 20% for testing)."),
                 tags$li("The plot shades the test period. The test set is only used to measure accuracy, never for fitting or tuning."))),
      ins_step("4", "Select the model",
               tags$ul(lapply(names(IF_MODELS), function(k) tags$li(tags$b(k), ": ", IF_MODELS[[k]]$full, ". ", IF_MODELS[[k]]$blurb))),
               p(class = "muted", "A model marked 'package not installed' needs its R package; see install_packages.R.")),
      ins_step("5", "Set the hyperparameters",
               tags$ul(
                 tags$li(tags$b("Single value:"), " the parameter is fixed at the value given."),
                 tags$li(tags$b("Range:"), " the parameter is tuned by grid search over seq(min, max, by = step); e.g. lag 1, 12, 1 tries lags 1 to 12. Use min = max for one value."),
                 tags$li("Parameters set to Range are tuned jointly: every combination is evaluated. The number of combinations is shown live; keep it moderate (a few hundred) for quick runs."),
                 tags$li(tags$b("Lag order:"), " the number of past values used as inputs; tuned over 1 to 12 by default."),
                 tags$li(tags$b("Intuitionistic fuzzy settings:"), " number of clusters C (default 3), fuzzifier m (2), Yager exponent alpha (0.85)."),
                 tags$li(tags$b("Validation:"), " candidates are compared on the last part of the training set (hold-out) or by expanding-window time-series cross-validation, using RMSE, MAE or MAPE."),
                 tags$li(tags$b("Forecast type:"), " one-step ahead uses the observed past values; recursive feeds predictions back for multi-step forecasts."),
                 tags$li(tags$b("Random seed:"), " fixes the random starts so results can be reproduced.")),
               h5(style = "margin-top:14px;", "Hyperparameter reference"),
               div(style = "overflow-x:auto;", hp_reference)),
      ins_step("6", "Run and read the results",
               tags$ul(
                 tags$li("Click ", tags$b("Run Model"), ". A progress bar shows each combination being evaluated and the best score so far."),
                 tags$li(tags$b("Accuracy:"), " RMSE, MAE and MAPE (%) on the training and test sets, with the selected hyperparameters. Lower is better; judge the model mainly by the test set."),
                 tags$li(tags$b("Forecast plot / Predictions:"), " actual, fitted and predicted values; the test-set table lists errors and absolute percentage errors."),
                 tags$li(tags$b("Tuning:"), " the validation score of every combination (best per lag when the lag is tuned)."),
                 tags$li(tags$b("Fuzzy clusters:"), " cluster centres with membership, non-membership and hesitation curves."),
                 tags$li(tags$b("Feature importance:"), " which lags and fuzzy degrees matter most (model-based for tree models, permutation-based for IFANN and IFSVR)."),
                 tags$li(tags$b("Model comparison:"), " every run of the session side by side; run several models on the same split to choose the best."))),
      ins_step("7", "Download",
               tags$ul(
                 tags$li("Results > Download: an Excel workbook of the current model and an Excel table comparing all runs."),
                 tags$li("Every table has Copy / CSV / Excel buttons and every plot a camera icon to save a PNG."))),
      div(class = "panel",
          h3(icon("circle-question"), " Troubleshooting"),
          tags$ul(
            tags$li(tags$b("'Lags larger than ... were dropped'"), " - the validation window is too short for the largest lags; use a longer series, a larger training share or a smaller validation share."),
            tags$li(tags$b("Run takes long"), " - reduce the number of Range parameters or widen their steps; IFAdaBoost and large forests are the slowest."),
            tags$li(tags$b("MAPE error"), " - MAPE cannot be used when the series contains zeros; use RMSE or MAE."),
            tags$li(tags$b("Need at least 'C' distinct values"), " - the training set has too few different values for the number of clusters; reduce C."),
            tags$li(tags$b("Package not installed"), " - run install_packages.R; CatBoost is installed from its GitHub release (the script does this)."))))
)

# ---------------- DEVELOPERS ----------------
dev_card <- function(img, name, role, email = NULL, scholar = NULL,
                     org = "ICAR-Indian Agricultural Statistics Research Institute, New Delhi") {
  div(class = "dev", div(class = "band"), tags$img(src = img, alt = name),
      div(class = "body", h4(name), div(class = "role", role),
          div(class = "org", org),
          div(class = "links",
              if (!is.null(email)) tags$a(class = "mail", href = paste0("mailto:", email), icon("envelope"), email),
              if (!is.null(scholar)) tags$a(class = "gs", href = scholar, target = "_blank", icon("graduation-cap"), "Google Scholar profile"))))
}

developers_tab <- tabPanel(
  "Developers", value = "developers", icon = icon("users"),
  div(class = "page-wrap",
      div(class = "eyebrow", "Developer team"),
      h2(class = "section-title", "Meet the team"),
      p(class = "section-sub", "This web application was developed at ICAR-Indian Agricultural Statistics Research Institute, New Delhi."),
      div(class = "dev-grid",
          dev_card("img/md_yeasin.jpg", "Dr. Md Yeasin", "Scientist", "yeasin.iasri@gmail.com",
                   "https://scholar.google.com/citations?user=xejMKD0AAAAJ&hl=en"),
          dev_card("img/ranjit_kumar_paul.jpg", "Dr. Ranjit Kumar Paul", "ICAR National Fellow", "ranjitstat@gmail.com",
                   "https://scholar.google.com/citations?user=wBWuZJgAAAAJ&hl=en"),
          dev_card("img/pushkar_bora.jpg", "Pushkar Bora", "Junior Research Fellow", "borapushkar1999@gmail.com",
                   "https://scholar.google.com/citations?user=xVIXGlwAAAAJ&hl=en"),
          dev_card("img/anita_sarkar.jpg", "Anita Sarkar", "Student"),
          dev_card("img/amrit_kumar_paul.jpg", "Dr. Amrit Kumar Paul", "Principal Scientist", "amritkumarpaul@iasri.res.in",
                   "https://scholar.google.com/citations?user=a7vv3nQAAAAJ&hl=en"))))

ui <- tagList(
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&family=Merriweather:wght@700;900&display=swap"),
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$meta(name = "description", content = paste(APP_TITLE, "-", APP_TAGLINE, "| ICAR-IASRI")),
    tags$link(rel = "icon", type = "image/png", href = "img/iasri_logo.png")),
  banner,
  navbarPage(
    title = span(span(class = "brand-badge", "IF-ML"), "FuzzyML"),
    windowTitle = paste(APP_TITLE, "| ICAR-IASRI"), id = "nav", collapsible = TRUE,
    theme = bs_theme(version = 5, primary = "#5b3fd1", secondary = "#6b6487", success = "#0e9488", warning = "#ff7a59",
                     base_font = "Inter, Segoe UI, sans-serif", heading_font = "Merriweather, Georgia, serif", "border-radius" = "0.6rem"),
    home_tab, model_tab, instructions_tab, developers_tab),
  footer)

# ============================================================
#  SERVER
# ============================================================

server <- function(input, output, session) {

  visits <- increment_counter()
  output$visitor_count <- renderText(formatC(visits, width = 6, flag = "0", format = "d"))

  observeEvent(input$go_model, updateNavbarPage(session, "nav", "model"))
  observeEvent(input$go_instr, updateNavbarPage(session, "nav", "instructions"))
  observeEvent(input$to_split, updateTabsetPanel(session, "steps", "split"))
  observeEvent(input$to_model, updateTabsetPanel(session, "steps", "choose"))
  observeEvent(input$to_hp, updateTabsetPanel(session, "steps", "hp"))

  rv <- reactiveValues(path = NULL, name = NULL, res = NULL, runs = list(), hp_reset = 0)

  # ---------------- data ----------------
  observeEvent(input$file, { rv$path <- input$file$datapath; rv$name <- input$file$name })
  observeEvent(input$load_sample, {
    rv$path <- SAMPLE_FILE; rv$name <- basename(SAMPLE_FILE)
    showNotification("Sample data loaded: monthly values of five commodity series (2015-2024).", type = "message", duration = 5)
  })
  output$dl_sample_csv <- downloadHandler(filename = function() "Sample_FuzzyML_data.csv",
                                          content = function(file) file.copy("sample_data/Sample_FuzzyML_data.csv", file))
  output$dl_sample_xlsx <- downloadHandler(filename = function() "Sample_FuzzyML_data.xlsx",
                                           content = function(file) file.copy("sample_data/Sample_FuzzyML_data.xlsx", file))

  sheets <- reactive({ req(rv$path); file_sheets(rv$path, rv$name) })
  raw <- reactive({
    req(rv$path)
    sh <- sheets()
    sel <- if (!is.null(sh)) { s <- input$sheet; if (is.null(s) || !s %in% sh) sh[1] else s } else NULL
    tryCatch(read_any(rv$path, rv$name, sel), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
  })
  output$file_ui <- renderUI({
    d <- raw(); req(d)
    sh <- sheets()
    tc <- guess_date_col(d)
    first_is_time <- !is.numeric(d[[tc]]) || grepl("date|time|month|year|period|week|day|quarter", tc, ignore.case = TRUE)
    tagList(
      div(class = "muted", style = "margin-bottom:8px;", icon("file-lines"), " ", rv$name, ": ", tags$b(nrow(d)), " rows, ", tags$b(ncol(d)), " columns"),
      fluidRow(
        if (!is.null(sh)) column(4, selectInput("sheet", "Worksheet", sh, isolate(input$sheet) %||% sh[1])),
        column(if (is.null(sh)) 6 else 4, selectInput("time_col", "Time column", c(NO_TIME, names(d)), if (first_is_time) tc else NO_TIME)),
        column(if (is.null(sh)) 6 else 4, uiOutput("target_ui"))))
  })
  prepared <- reactive({
    d <- raw(); req(d, input$time_col)
    tryCatch(prepare_series(d, input$time_col, input$time_fmt, input$missing), error = function(e) list(error = conditionMessage(e)))
  })
  output$target_ui <- renderUI({
    p <- prepared(); req(is.null(p$error))
    selectInput("target", "Series to forecast", p$series, isolate(if (!is.null(input$target) && input$target %in% p$series) input$target else p$series[1]))
  })
  data_ok <- reactive({ p <- prepared(); is.null(p$error) && !is.null(input$target) && input$target %in% p$series })
  output$has_data <- reactive(isTRUE(tryCatch(data_ok(), error = function(e) FALSE)))
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)

  series <- reactive({
    req(data_ok())
    p <- prepared(); v <- p$data[[input$target]]
    validate(need(!anyNA(v), "The selected series still has missing values. Choose 'Fill by linear interpolation' under Missing values."))
    v
  })

  output$data_status <- renderUI({
    p <- tryCatch(prepared(), error = function(e) NULL)
    if (is.null(p)) return(div(class = "note", icon("circle-info"), " Upload a file or load the sample data to begin."))
    if (!is.null(p$error)) return(div(class = "note err", icon("circle-xmark"), " ", p$error))
    d <- p$data
    tagList(
      div(class = "stat-grid",
          stat_box("Observations", nrow(d), "list-ol"),
          stat_box("Period", paste(d$.label[1], "to", d$.label[nrow(d)]), "calendar", "o"),
          stat_box("Time unit", tools::toTitleCase(p$unit), "clock", "g"),
          stat_box("Numeric series", length(p$series), "chart-line")),
      div(class = "note", icon("clock"), HTML(paste0(" Time column read as <b>", htmltools::htmlEscape(p$time_desc), "</b>.",
                                                     if (length(p$messages)) paste0("<br>", paste(htmltools::htmlEscape(p$messages), collapse = "<br>")) else ""))))
  })
  output$pl_series <- renderPlotly({ req(data_ok()); p <- prepared(); plot_series(p$data, input$target, p$time_name) })
  output$tbl_stats <- renderDT({ req(data_ok()); dt_out(series_stats(prepared()$data[[input$target]]), dom = "t", page = 20) })
  output$tbl_preview <- renderDT({
    req(data_ok()); p <- prepared(); d <- p$data
    out <- data.frame(d$.label, d[, p$series, drop = FALSE], check.names = FALSE); names(out)[1] <- p$time_name
    dt_out(out, digits = 3)
  })

  # ---------------- split ----------------
  ntrain <- reactive({ req(data_ok()); as.integer(round(nrow(prepared()$data) * input$split)) })
  output$split_info <- renderUI({
    req(data_ok()); d <- prepared()$data; nt <- ntrain(); n <- nrow(d)
    div(class = "stat-grid", style = "margin-bottom:0;",
        stat_box("Training set", paste0(nt, " obs (", d$.label[1], " to ", d$.label[nt], ")"), "graduation-cap"),
        stat_box("Test set", paste0(n - nt, " obs (", d$.label[nt + 1], " to ", d$.label[n], ")"), "flag-checkered", "o"))
  })
  output$pl_split <- renderPlotly({ req(data_ok()); p <- prepared(); plot_series(p$data, input$target, p$time_name, ntrain()) })

  # ---------------- model choice ----------------
  output$model_cards <- renderUI({
    choices <- names(IF_MODELS)
    radioButtons("model", NULL, selected = isolate(input$model) %||% "IFANN", choiceValues = choices,
                 choiceNames = lapply(choices, function(k) {
                   mm <- IF_MODELS[[k]]; ok <- model_available(k)
                   div(class = "model-card",
                       div(class = "mc-top", span(class = paste("mc-ico", model_icon_cls[[k]]), icon(mm$icon)),
                           div(div(class = "mc-name", mm$name), div(class = "mc-full", mm$full))),
                       div(class = "mc-blurb", mm$blurb),
                       div(class = "mc-foot", span(class = "mc-pkg", icon("box"), " R package: ", mm$pkg),
                           if (!ok) span(class = "mc-missing", icon("triangle-exclamation"), " package not installed")))
                 }))
  })

  # ---------------- hyperparameters ----------------
  hp_id <- function(model, id, part) paste0("hp_", model, "_", id, "_", part)
  st_id <- function(model, id) paste0("st_", model, "_", id)
  cur_model <- reactive(input$model %||% "IFANN")

  output$hp_title <- renderUI({
    mm <- IF_MODELS[[cur_model()]]
    div(class = "note ok", icon(mm$icon), HTML(paste0(" Selected model: <b>", mm$name, "</b> - ", mm$full, ". ",
                                                     "Change it in step 3.")))
  })

  output$hp_ui <- renderUI({
    rv$hp_reset
    k <- cur_model(); mm <- IF_MODELS[[k]]
    ps <- c(list(LAG_PARAM), mm$params)
    stepv <- function(p) if (p$type == "int") 1 else 0.01
    rows <- lapply(ps, function(p) {
      div(class = "hp-row",
          div(class = "hp-name", tags$b(p$label), div(class = "help", style = "margin:2px 0 0;", p$help)),
          div(class = "hp-mode", radioButtons(hp_id(k, p$id, "mode"), NULL, c("Single value" = "single", "Range" = "range"),
                                              if (p$id == "Lag") "range" else "single", inline = TRUE)),
          div(class = "hp-single", numericInput(hp_id(k, p$id, "val"), "Single value", p$single, step = stepv(p))),
          div(class = "hp-range",
              numericInput(hp_id(k, p$id, "min"), "Min", p$range[1], step = stepv(p)),
              numericInput(hp_id(k, p$id, "max"), "Max", p$range[2], step = stepv(p)),
              numericInput(hp_id(k, p$id, "step"), "Step", p$range[3], step = stepv(p))))
    })
    sets <- if (length(mm$settings)) {
      div(class = "hp-settings", h6(icon("gear"), " Other ", mm$name, " settings (fixed)"),
          fluidRow(lapply(mm$settings, function(s) column(4,
            if (s$type == "choice") selectInput(st_id(k, s$id), s$label, s$choices, s$value)
            else numericInput(st_id(k, s$id), s$label, s$value, min = 1, step = 1)))))
    }
    tagList(div(class = "hp-table", rows), sets)
  })

  observeEvent(input$reset_hp, {
    rv$hp_reset <- rv$hp_reset + 1
    updateNumericInput(session, "if_class", value = 3); updateNumericInput(session, "if_m", value = 2)
    updateNumericInput(session, "if_alpha", value = 0.85); updateNumericInput(session, "if_maxiter", value = 100)
    updateNumericInput(session, "if_tol", value = 1e-5); updateSelectInput(session, "val_method", selected = "holdout")
    updateNumericInput(session, "val_split", value = 0.2); updateNumericInput(session, "val_folds", value = 3)
    updateSelectInput(session, "metric", selected = "RMSE"); updateSelectInput(session, "fc_type", selected = "one-step")
    updateNumericInput(session, "seed", value = 123)
    showNotification("Hyperparameters and settings reset to defaults.", type = "message")
  })

  collect_spec <- reactive({
    k <- cur_model(); mm <- IF_MODELS[[k]]
    ps <- c(list(LAG_PARAM), mm$params)
    lapply(ps, function(p) {
      mode <- input[[hp_id(k, p$id, "mode")]] %||% (if (p$id == "Lag") "range" else "single")
      c(p[c("id", "label", "type")],
        list(mode = mode, value = input[[hp_id(k, p$id, "val")]] %||% p$single,
             range = c(input[[hp_id(k, p$id, "min")]] %||% p$range[1], input[[hp_id(k, p$id, "max")]] %||% p$range[2],
                       input[[hp_id(k, p$id, "step")]] %||% p$range[3])))
    })
  })
  collect_settings <- reactive({
    k <- cur_model(); st <- IF_MODELS[[k]]$settings
    stats::setNames(lapply(st, function(s) input[[st_id(k, s$id)]] %||% s$value), vapply(st, `[[`, "", "id"))
  })

  grid_details <- reactive({
    sp <- collect_spec()
    rng <- Filter(function(p) identical(p$mode, "range"), sp)
    vals <- lapply(rng, function(p) tryCatch(.range_values(p$range, p$label, p$type), error = function(e) conditionMessage(e)))
    list(rng = rng, vals = vals)
  })
  output$grid_info <- renderUI({
    g <- grid_details()
    if (!length(g$rng)) return(div(class = "note", icon("circle-info"), " All hyperparameters are single values: no tuning, the model is fitted once."))
    errs <- Filter(is.character, g$vals)
    if (length(errs)) return(div(class = "note err", icon("circle-xmark"), " ", HTML(paste(htmltools::htmlEscape(unlist(errs)), collapse = "<br>"))))
    lens <- lengths(g$vals); total <- prod(lens)
    folds <- if (identical(input$val_method, "tscv")) (input$val_folds %||% 3) else 1
    items <- mapply(function(p, v) tags$li(tags$b(p$label), ": ", paste(utils::head(format(v, trim = TRUE, drop0trailing = TRUE), 12), collapse = ", "),
                                            if (length(v) > 12) " ..." , " (", length(v), " values)"), g$rng, g$vals, SIMPLIFY = FALSE)
    div(class = paste("note", if (total > 500) "warn" else "ok"),
        icon("magnifying-glass-chart"), tags$b(" Grid search: "),
        paste0(paste(lens, collapse = " x "), " = ", format(total, big.mark = ","), " combination", if (total == 1) "" else "s",
               if (folds > 1) paste0(" x ", folds, " folds") else "", "."),
        tags$ul(style = "margin:6px 0 0;", items),
        if (total > 500) div(tags$b("This is a large grid and may take a long time; consider wider steps.")))
  })
  output$run_summary <- renderUI({
    if (!isTRUE(tryCatch(data_ok(), error = function(e) FALSE))) return(span(class = "muted", "Upload data first."))
    g <- grid_details(); total <- if (length(g$rng) && !any(vapply(g$vals, is.character, TRUE))) prod(lengths(g$vals)) else 1
    span(class = "muted", tags$b(cur_model()), " on ", tags$b(input$target), " | training ", tags$b(ntrain()), ", test ",
         tags$b(nrow(prepared()$data) - ntrain()), " obs | ", tags$b(format(total, big.mark = ",")), " combination(s)")
  })

  # ---------------- run ----------------
  observeEvent(input$run, {
    if (!isTRUE(tryCatch(data_ok(), error = function(e) FALSE))) { showNotification("Please upload data first.", type = "error"); return() }
    y <- tryCatch(series(), error = function(e) NULL)
    if (is.null(y)) { showNotification("The selected series has missing values; choose interpolation.", type = "error"); return() }
    k <- cur_model()
    if (!model_available(k)) { showNotification(paste0("R package '", IF_MODELS[[k]]$pkg, "' is not installed; it is needed for ", k, "."), type = "error", duration = 10); return() }
    spec <- collect_spec(); settings <- collect_settings()
    set.seed(as.integer(input$seed %||% 123))
    t0 <- Sys.time()
    res <- withProgress(message = paste("Running", k), value = 0, {
      last <- Sys.time() - 10
      cb <- function(i, total, best, el) {
        if (i == total || as.numeric(difftime(Sys.time(), last, units = "secs")) > 0.25) {
          setProgress(0.9 * i / total, detail = sprintf("Combination %d of %d | best %s = %s | %.0f s", i, total, input$metric,
                                                         if (is.finite(best)) format(signif(best, 5)) else "-", el))
          last <<- Sys.time()
        }
      }
      setProgress(0.02, detail = "Preparing fuzzy clusters")
      out <- tryCatch(withCallingHandlers(
        run_ifml(y, k, spec, settings, Class = input$if_class, Split = input$split,
                 Validation = input$val_method, Val_Split = input$val_split, Folds = input$val_folds %||% 3,
                 Metric = input$metric, Forecast_Type = input$fc_type, m = input$if_m, Alpha = input$if_alpha,
                 Max_Iter = input$if_maxiter, Tol = input$if_tol, progress = cb),
        warning = function(w) invokeRestart("muffleWarning")),
        error = function(e) list(error = conditionMessage(e)))
      setProgress(1, detail = "Done")
      out
    })
    if (!is.null(res$error)) { showNotification(res$error, type = "error", duration = 12); return() }
    res$Elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    res$Target <- input$target
    res$Seed <- input$seed
    rv$res <- res
    nm <- paste0(k, " #", sum(startsWith(names(rv$runs) %||% character(), k)) + 1)
    rv$runs[[nm]] <- res
    updateTabsetPanel(session, "steps", "results")
    showNotification(sprintf("%s finished in %.1f s. Test RMSE = %s", k, res$Elapsed, format(signif(res$Accuracy["Test set", "RMSE"], 5))),
                     type = "message", duration = 8)
  })

  output$has_results <- reactive(!is.null(rv$res))
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)
  output$has_tuning <- reactive(!is.null(rv$res$Tuning))
  outputOptions(output, "has_tuning", suspendWhenHidden = FALSE)

  # ---------------- results ----------------
  output$results_header <- renderUI({
    r <- rv$res
    if (is.null(r)) return(div(class = "note warn", icon("hourglass-half"), " No results yet. Set the hyperparameters in step 4 and click ", tags$b("Run Model"), "."))
    te <- r$Accuracy["Test set", ]
    tagList(
      div(class = "stat-grid",
          stat_box("Model", r$Model, IF_MODELS[[r$Model]]$icon),
          stat_box("Selected lag", r$Best_Params$Lag, "clock-rotate-left", "o"),
          stat_box("Test RMSE", format(signif(te[["RMSE"]], 5)), "bullseye", "g"),
          stat_box("Test MAPE", if (is.na(te[["MAPE"]])) "-" else paste0(format(round(te[["MAPE"]], 2), nsmall = 2), "%"), "percent", "g"),
          stat_box("Combinations", if (is.null(r$Tuning)) "1 (no tuning)" else format(r$Tuning$Combinations, big.mark = ","), "magnifying-glass-chart"),
          stat_box("Run time", sprintf("%.1f s", r$Elapsed), "stopwatch")),
      if (length(r$Warnings)) div(class = "note warn", icon("triangle-exclamation"), " ", paste(r$Warnings, collapse = " ")))
  })

  output$interpretation <- renderUI({
    r <- rv$res; req(r)
    te <- r$Accuracy["Test set", ]; tr <- r$Accuracy["Training set", ]
    tuned <- r$Tuning$Tuned %||% character()
    div(class = "note", icon("comment-dots"),
        HTML(paste0(" <b>", r$Model, "</b> (", r$Full_Name, ") on <b>", htmltools::htmlEscape(r$Target), "</b> uses lag <b>", r$Best_Params$Lag, "</b> and ",
                    r$Settings$Class, " intuitionistic fuzzy clusters (", r$Settings$n_inputs, " inputs). ",
                    if (length(tuned)) paste0("Tuned by grid search: ", paste(tuned, collapse = ", "), " (", r$Tuning$Combinations, " combinations, ",
                                              if (r$Tuning$Validation == "holdout") "hold-out" else paste0(r$Tuning$Folds, "-fold time-series CV"), "). ") else "All hyperparameters were fixed. ",
                    "On the ", r$Settings$n_test, " test observations the ", r$Settings$Forecast_Type, " forecasts have RMSE <b>", format(signif(te[["RMSE"]], 5)),
                    "</b>, MAE <b>", format(signif(te[["MAE"]], 5)), "</b>",
                    if (!is.na(te[["MAPE"]])) paste0(" and MAPE <b>", round(te[["MAPE"]], 2), "%</b>") else "", " (training RMSE ", format(signif(tr[["RMSE"]], 5)), ").")))
  })

  output$tbl_acc <- renderDT({
    r <- rv$res; req(r)
    a <- data.frame(Set = rownames(r$Accuracy), r$Accuracy, check.names = FALSE); names(a)[4] <- "MAPE (%)"
    dt_out(a, dom = "Bt")
  })
  output$tbl_params <- renderDT({
    r <- rv$res; req(r)
    bp <- r$Best_Params; tuned <- r$Tuning$Tuned %||% character()
    lab <- function(id) { if (id == "Lag") return(LAG_PARAM$label); if (id == "Mtry_Used") return("Mtry used (number of inputs)")
      p <- Filter(function(q) q$id == id, IF_MODELS[[r$Model]]$params); if (length(p)) p[[1]]$label else id }
    dt_out(data.frame(Hyperparameter = vapply(names(bp), lab, ""), Value = vapply(bp, function(v) format(v, drop0trailing = TRUE), ""),
                      Chosen = ifelse(names(bp) %in% tuned, "Tuned (grid search)", ifelse(names(bp) == "Mtry_Used", "Derived", "Single value")),
                      check.names = FALSE), dom = "Bt", page = 20)
  })
  settings_df <- function(r) {
    s <- r$Settings
    data.frame(Setting = c("Model", "Series", "Training / test observations", "Number of inputs", "Clusters (C)", "Fuzzifier (m)", "Yager exponent (alpha)",
                           "Forecast type", "Validation", "Validation share", "Tuning metric", "Random seed",
                           names(s)[!names(s) %in% c("Class", "Split", "Forecast_Type", "m", "Alpha", "Validation", "Val_Split", "Folds", "Metric", "n_train", "n_test", "n_inputs")]),
               Value = c(r$Full_Name, r$Target, paste(s$n_train, "/", s$n_test), s$n_inputs, s$Class, s$m, s$Alpha, s$Forecast_Type,
                         if (s$Validation == "holdout") "Hold-out" else paste0("Time-series CV (", s$Folds, " folds)"), s$Val_Split, s$Metric, r$Seed,
                         vapply(s[!names(s) %in% c("Class", "Split", "Forecast_Type", "m", "Alpha", "Validation", "Val_Split", "Folds", "Metric", "n_train", "n_test", "n_inputs")],
                                function(v) paste(v, collapse = ", "), "")), check.names = FALSE) |>
      within({ Setting <- vapply(Setting, function(id) { st <- Filter(function(q) q$id == id, IF_MODELS[[r$Model]]$settings)
                                                         if (length(st)) st[[1]]$label else id }, "") })
  }
  output$tbl_settings <- renderDT({ r <- rv$res; req(r); dt_out(settings_df(r), dom = "t", page = 30) })

  res_data <- reactive({ req(rv$res); prepared() })
  output$pl_forecast <- renderPlotly({ r <- rv$res; req(r); p <- res_data(); req(r$Target %in% names(p$data)); plot_forecast(p$data, r$Target, r, p$time_name) })
  output$pl_scatter <- renderPlotly({ r <- rv$res; req(r); plot_scatter(r) })
  output$tbl_err_summary <- renderDT({
    r <- rv$res; req(r)
    e <- r$Test_Actual - r$Test_Prediction
    dt_out(data.frame(Statistic = c("Mean error (bias)", "Mean absolute error", "Root mean squared error", "Largest absolute error", "Mean absolute % error", "Correlation (actual, predicted)"),
                      Value = c(mean(e), mean(abs(e)), sqrt(mean(e^2)), max(abs(e)),
                                if (any(r$Test_Actual == 0)) NA else 100 * mean(abs(e / r$Test_Actual)),
                                if (length(e) > 2) stats::cor(r$Test_Actual, r$Test_Prediction) else NA)), dom = "t")
  })
  pred_df <- function(r, which = "test", p = NULL) {
    idx <- if (which == "test") r$Test_Index else r$Train_Index
    act <- if (which == "test") r$Test_Actual else r$Train_Actual
    prd <- if (which == "test") r$Test_Prediction else r$Train_Fitted
    lab <- if (!is.null(p)) p$data$.label[idx] else idx
    out <- data.frame(Time = lab, Actual = act, Predicted = prd, Error = act - prd,
                      `Absolute % error` = if (any(act == 0)) NA else 100 * abs((act - prd) / act), check.names = FALSE)
    if (!is.null(p)) names(out)[1] <- p$time_name
    out
  }
  output$tbl_pred <- renderDT({ r <- rv$res; req(r); dt_out(pred_df(r, input$pred_set, res_data()), page = 15) })

  output$tuning_info <- renderUI({
    r <- rv$res; req(r)
    t <- r$Tuning
    if (is.null(t)) return(div(class = "note", icon("circle-info"), " No tuning was done: every hyperparameter was a single value. Set a parameter to 'Range' in step 4 to tune it."))
    div(class = "note ok", icon("magnifying-glass-chart"),
        HTML(paste0(" Grid search over <b>", paste(t$Tuned, collapse = ", "), "</b>: ", format(t$Combinations, big.mark = ","), " combinations, ",
                    if (t$Validation == "holdout") "hold-out validation" else paste0(t$Folds, "-fold time-series cross-validation"),
                    " on the last ", round(100 * r$Settings$Val_Split), "% of the training set. Best validation ", t$Metric, " = <b>", format(signif(t$Best_Score, 5)),
                    "</b> (", sprintf("%.1f", t$Elapsed_Seconds), " s). The final model was refitted on the whole training set with the best combination.")))
  })
  output$pl_tuning <- renderPlotly({ r <- rv$res; req(r, r$Tuning); plot_tuning(r) })
  output$tbl_tuning <- renderDT({
    r <- rv$res; req(r, r$Tuning)
    h <- r$Tuning$History; h <- h[order(h$Score), , drop = FALSE]
    names(h)[names(h) == "Score"] <- paste("Validation", r$Tuning$Metric)
    h$Rank <- seq_len(nrow(h)); h <- h[, c("Rank", setdiff(names(h), "Rank"))]
    dt_out(h, page = 10)
  })

  output$pl_membership <- renderPlotly({ r <- rv$res; req(r); plot_membership(r, r$Target) })
  output$tbl_centres <- renderDT({
    r <- rv$res; req(r); cl <- r$IFCM
    dom <- table(factor(max.col(cl$membership), levels = seq_along(cl$centers)))
    dt_out(data.frame(Cluster = paste("C", seq_along(cl$centers)), Centre = cl$centers,
                      `Training obs (dominant)` = as.integer(dom),
                      `Mean hesitation` = colMeans(cl$hesitation), check.names = FALSE), dom = "t")
  })
  output$ifcm_info <- renderUI({
    r <- rv$res; req(r); cl <- r$IFCM
    div(class = "muted", style = "margin-top:8px;", sprintf("IFCM %s after %d iterations (m = %s, alpha = %s).",
                                                             if (cl$converged) "converged" else "did not converge", cl$iterations, cl$m, cl$alpha))
  })

  output$imp_info <- renderUI({
    r <- rv$res; req(r)
    div(class = "note", icon("circle-info"), HTML(paste0(" Importance measure: <b>", r$Importance_Type, "</b>. ",
                                                         "Lag<i>l</i> = value at lag <i>l</i>; Mu_L<i>l</i>_C<i>k</i> / Nu_L<i>l</i>_C<i>k</i> = membership / non-membership of that lag in cluster <i>k</i>. ",
                                                         "Bars: blue = lagged values, green = membership, orange = non-membership.")))
  })
  output$pl_importance <- renderPlotly({ r <- rv$res; req(r, length(r$Importance)); plot_importance(r) })
  output$tbl_imp_group <- renderDT({ r <- rv$res; req(r, length(r$Importance)); dt_out(importance_groups(r$Importance), dom = "t", digits = 3) })

  # ---------------- comparison ----------------
  compare_df <- reactive({
    runs <- rv$runs; req(length(runs))
    do.call(rbind, lapply(names(runs), function(nm) {
      r <- runs[[nm]]; a <- r$Accuracy
      bp <- r$Best_Params; bp <- bp[names(bp) != "Mtry_Used"]
      data.frame(Run = nm, Model = r$Model, Series = r$Target,
                 `Test RMSE` = a["Test set", "RMSE"], `Test MAE` = a["Test set", "MAE"], `Test MAPE (%)` = a["Test set", "MAPE"],
                 `Train RMSE` = a["Training set", "RMSE"], `Train / test` = paste(r$Settings$n_train, "/", r$Settings$n_test),
                 Hyperparameters = paste(paste(names(bp), vapply(bp, function(v) format(v, drop0trailing = TRUE), ""), sep = " = "), collapse = "; "),
                 `Time (s)` = r$Elapsed, check.names = FALSE)
    }))
  })
  output$tbl_compare <- renderDT({
    d <- compare_df()
    dt <- dt_out(d, page = 10)
    best <- d$Run[which.min(d$`Test RMSE`)]
    DT::formatStyle(dt, "Run", target = "row", backgroundColor = DT::styleEqual(best, "#d3f1ec"))
  })
  output$pl_compare <- renderPlotly({
    runs <- rv$runs; req(length(runs), rv$res)
    p <- prepared(); r0 <- rv$res
    same <- Filter(function(r) identical(r$Target, r0$Target) && identical(r$Test_Index, r0$Test_Index) && r$Target %in% names(p$data), runs)
    req(length(same))
    plot_compare(p$data, r0$Target, same, p$time_name)
  })
  observeEvent(input$clear_runs, { rv$runs <- if (!is.null(rv$res)) list(`Current` = rv$res) else list() })

  # ---------------- downloads ----------------
  write_book <- function(sheets, file) {
    wb <- openxlsx::createWorkbook(creator = "FuzzyML, ICAR-IASRI")
    hs <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#23184A", fontColour = "#FFFFFF", halign = "center")
    for (nm in names(sheets)) {
      df <- sheets[[nm]]; if (is.null(df) || !nrow(df)) df <- data.frame(Message = "Not available")
      openxlsx::addWorksheet(wb, nm); openxlsx::writeData(wb, nm, df, headerStyle = hs)
      openxlsx::freezePane(wb, nm, firstRow = TRUE); openxlsx::setColWidths(wb, nm, seq_len(ncol(df)), "auto")
    }
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  }
  output$dl_excel <- downloadHandler(
    filename = function() paste0("FuzzyML_", rv$res$Model, "_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx"),
    content = function(file) {
      r <- rv$res; req(r); p <- prepared()
      bp <- r$Best_Params
      d <- p$data; used <- data.frame(d$.label, d[[r$Target]], check.names = FALSE); names(used) <- c(p$time_name, r$Target)
      cl <- r$IFCM
      write_book(list(
        Accuracy = data.frame(Set = rownames(r$Accuracy), r$Accuracy, check.names = FALSE),
        Parameters = data.frame(Hyperparameter = names(bp), Value = vapply(bp, function(v) format(v, drop0trailing = TRUE), ""),
                                Chosen = ifelse(names(bp) %in% (r$Tuning$Tuned %||% character()), "Tuned", "Single value")),
        Test_Predictions = pred_df(r, "test", p), Training_Fitted = pred_df(r, "train", p),
        Tuning_History = if (!is.null(r$Tuning)) r$Tuning$History else NULL,
        IFCM_Centres = data.frame(Cluster = seq_along(cl$centers), Centre = cl$centers, Mean_Hesitation = colMeans(cl$hesitation)),
        Feature_Importance = data.frame(Feature = names(r$Importance), Importance = as.numeric(r$Importance), Measure = r$Importance_Type),
        Settings = settings_df(r), Data_Used = used), file)
    })
  output$dl_compare <- downloadHandler(
    filename = function() paste0("FuzzyML_model_comparison_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx"),
    content = function(file) write_book(list(Comparison = compare_df()), file))
}

shinyApp(ui, server)
