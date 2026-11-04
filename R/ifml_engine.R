# ============================================================
# FuzzyML : Intuitionistic Fuzzy Machine Learning engine
# ICAR-Indian Agricultural Statistics Research Institute
# ------------------------------------------------------------
# The intuitionistic fuzzy part (IFCM clustering, Yager non-membership,
# lagged feature construction, time-ordered validation and grid search)
# follows the IFANN and IFRF R packages exactly; the learner is pluggable:
#   IFANN, IFRF, IFSVR, IFXGBoost, IFLightGBM, IFAdaBoost, IFCatBoost
# ============================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

# ------------------------------------------------------------
# 1. INTUITIONISTIC FUZZY C-MEANS (identical to the packages)
# ------------------------------------------------------------

.yager_nonmembership <- function(mu, alpha) (1 - mu^alpha)^(1 / alpha)

.fcm_membership <- function(x, centers, m) {
  D <- abs(outer(x, centers, "-"))
  zero <- D < 1e-12
  U <- D^(-2 / (m - 1))
  U <- U / rowSums(U)
  hit <- rowSums(zero) > 0
  if (any(hit)) {
    Z <- zero[hit, , drop = FALSE] * 1
    U[hit, ] <- Z / rowSums(Z)
  }
  U
}

.ifcm <- function(x, Class, m = 2, alpha = 0.85, max_iter = 100, tol = 1e-5) {
  x <- as.numeric(x)
  n <- length(x)
  if (Class < 2) stop("Class must be greater than or equal to 2")
  if (length(unique(x)) < Class) stop("The training data need at least 'Class' distinct values")
  U <- matrix(stats::runif(n * Class), nrow = n, ncol = Class)
  U <- U / rowSums(U)
  converged <- FALSE
  iter <- 0L
  centers <- numeric(Class)
  for (iter in seq_len(max_iter)) {
    nu <- .yager_nonmembership(U, alpha)
    W <- (1 - nu)^m
    centers <- colSums(W * x) / colSums(W)
    U_new <- .fcm_membership(x, centers, m)
    delta <- max(abs(U_new - U))
    U <- U_new
    if (delta < tol) { converged <- TRUE; break }
  }
  o <- order(centers)
  centers <- centers[o]
  U <- U[, o, drop = FALSE]
  nu <- .yager_nonmembership(U, alpha)
  list(centers = centers, membership = U, nonmembership = nu, hesitation = 1 - U - nu,
       iterations = iter, converged = converged, m = m, alpha = alpha)
}

.if_degrees <- function(x, centers, m, alpha) {
  mu <- .fcm_membership(as.numeric(x), centers, m)
  list(mu = mu, nu = .yager_nonmembership(mu, alpha))
}

# ------------------------------------------------------------
# 2. FEATURES (lags + membership + non-membership degrees)
# ------------------------------------------------------------

.minmax <- function(x) {
  lo <- min(x); hi <- max(x)
  if (hi - lo < .Machine$double.eps) hi <- lo + 1
  list(lo = lo, hi = hi)
}
.scale01 <- function(x, s) (x - s$lo) / (s$hi - s$lo)
.unscale01 <- function(z, s) z * (s$hi - s$lo) + s$lo

.prepare <- function(y_fit, Class, m, alpha, max_iter, tol) {
  cl <- .ifcm(y_fit, Class = Class, m = m, alpha = alpha, max_iter = max_iter, tol = tol)
  list(centers = cl$centers, m = m, alpha = alpha, scaling = .minmax(y_fit), ifcm = cl)
}

.feature_matrix <- function(y, idx, L, prep, scale_lags = FALSE) {
  n <- length(idx)
  lag_idx <- outer(idx, seq_len(L), "-")
  lagmat <- matrix(y[lag_idx], nrow = n, ncol = L)
  d <- .if_degrees(as.vector(lagmat), prep$centers, prep$m, prep$alpha)
  K <- length(prep$centers)
  mu_blocks <- vector("list", L); nu_blocks <- vector("list", L)
  for (k in seq_len(L)) {
    rows <- ((k - 1L) * n + 1L):(k * n)
    mu_blocks[[k]] <- d$mu[rows, , drop = FALSE]
    nu_blocks[[k]] <- d$nu[rows, , drop = FALSE]
  }
  lagpart <- if (scale_lags) .scale01(lagmat, prep$scaling) else lagmat
  X <- cbind(lagpart, do.call(cbind, mu_blocks), do.call(cbind, nu_blocks))
  colnames(X) <- c(paste0("Lag", seq_len(L)),
                   paste0("Mu_L", rep(seq_len(L), each = K), "_C", rep(seq_len(K), L)),
                   paste0("Nu_L", rep(seq_len(L), each = K), "_C", rep(seq_len(K), L)))
  X
}

.metrics <- function(actual, pred) {
  e <- actual - pred
  c(RMSE = sqrt(mean(e^2)), MAE = mean(abs(e)),
    MAPE = if (any(actual == 0)) NA_real_ else mean(abs(e / actual)) * 100)
}

.seed_int <- function() sample.int(.Machine$integer.max - 1L, 1L)

# ------------------------------------------------------------
# 3. AdaBoost.R2 (Drucker, 1997) with regression-tree base learners
# ------------------------------------------------------------

.adaboost_r2_fit <- function(X, y, n_estimators, learning_rate, max_depth, loss = "linear") {
  n <- nrow(X)
  df <- data.frame(X, .y = y, check.names = FALSE)
  names(df) <- c(colnames(X), ".y")
  w <- rep(1 / n, n)
  trees <- list(); est_w <- numeric(0)
  ctrl <- rpart::rpart.control(maxdepth = max_depth, cp = 0, minsplit = 2, minbucket = 1, xval = 0)
  for (b in seq_len(n_estimators)) {
    idx <- sample.int(n, n, replace = TRUE, prob = w)
    tree <- rpart::rpart(.y ~ ., data = df[idx, , drop = FALSE], method = "anova", control = ctrl)
    pred <- stats::predict(tree, df)
    err <- abs(pred - y)
    D <- max(err)
    if (D <= 0) { trees[[length(trees) + 1]] <- tree; est_w <- c(est_w, 1); break }
    Lb <- err / D
    if (loss == "square") Lb <- Lb^2 else if (loss == "exponential") Lb <- 1 - exp(-Lb)
    ebar <- sum(w * Lb)
    if (ebar <= 0) { trees[[length(trees) + 1]] <- tree; est_w <- c(est_w, 1); break }
    if (ebar >= 0.5) {
      if (length(trees) == 0) { trees[[1]] <- tree; est_w <- 1 }
      break
    }
    beta <- ebar / (1 - ebar)
    trees[[length(trees) + 1]] <- tree
    est_w <- c(est_w, learning_rate * log(1 / beta))
    w <- w * beta^((1 - Lb) * learning_rate)
    w <- w / sum(w)
  }
  structure(list(trees = trees, weights = est_w, features = colnames(X)), class = "ifml_adaboost")
}

.adaboost_r2_predict <- function(fit, X) {
  df <- as.data.frame(X); names(df) <- fit$features
  P <- vapply(fit$trees, function(t) as.numeric(stats::predict(t, df)), numeric(nrow(df)))
  if (!is.matrix(P)) P <- matrix(P, nrow = nrow(df))
  w <- fit$weights
  apply(P, 1, function(p) {                       # weighted median
    o <- order(p); cw <- cumsum(w[o])
    p[o][which(cw >= 0.5 * sum(w))[1]]
  })
}

# ------------------------------------------------------------
# 4. MODEL REGISTRY
#    type: int (positive integer), num0 (>= 0), pos (> 0), prop (0, 1]
# ------------------------------------------------------------

LAG_PARAM <- list(id = "Lag", label = "Lag order (L)", type = "int", single = 1, range = c(1, 12, 1),
                  help = "Number of past values used as inputs.")

IF_MODELS <- list(
  IFANN = list(
    name = "IFANN", full = "Intuitionistic Fuzzy Artificial Neural Network", pkg = "nnet",
    icon = "brain", scale = TRUE,
    blurb = "Feed-forward neural network with one hidden layer; several networks from different random starts are averaged.",
    params = list(
      list(id = "Size", label = "Hidden neurons (Size)", type = "int", single = 4, range = c(2, 8, 2), help = "Number of neurons in the hidden layer."),
      list(id = "Decay", label = "Weight decay", type = "num0", single = 0.01, range = c(0.01, 0.09, 0.04), help = "L2 penalty on the network weights.")),
    settings = list(
      list(id = "Maxit", label = "Maximum iterations", type = "int", value = 200),
      list(id = "Repeats", label = "Networks averaged (Repeats)", type = "int", value = 5)),
    fit = function(X, y, p, s) {
      size <- as.integer(p$Size)
      nets <- lapply(seq_len(as.integer(s$Repeats)), function(r)
        nnet::nnet(x = X, y = y, size = size, decay = p$Decay, maxit = as.integer(s$Maxit),
                   linout = TRUE, trace = FALSE, MaxNWts = (ncol(X) + 1L) * size + size + 11L))
      nets
    },
    predict = function(fit, X) {
      P <- vapply(fit, function(net) as.numeric(stats::predict(net, X)), numeric(nrow(X)))
      if (is.matrix(P)) rowMeans(P) else mean(P)
    },
    importance = NULL),

  IFRF = list(
    name = "IFRF", full = "Intuitionistic Fuzzy Random Forest", pkg = "randomForest",
    icon = "tree", scale = FALSE,
    blurb = "Bagged regression trees with random feature selection at every split (Breiman, 2001).",
    params = list(
      list(id = "Ntree", label = "Number of trees (Ntree)", type = "int", single = 500, range = c(100, 500, 200), help = "Trees in the forest."),
      list(id = "Mtry", label = "Mtry (proportion of inputs)", type = "prop", single = 0.33, range = c(0.2, 1, 0.4), help = "Share of inputs tried at each split, in (0, 1]."),
      list(id = "Nodesize", label = "Minimum node size", type = "int", single = 5, range = c(1, 9, 4), help = "Minimum size of terminal nodes.")),
    settings = list(),
    fit = function(X, y, p, s) {
      randomForest::randomForest(x = X, y = y, ntree = as.integer(p$Ntree),
                                 mtry = as.integer(max(1, min(ncol(X), floor(p$Mtry * ncol(X))))),
                                 nodesize = as.integer(p$Nodesize), importance = FALSE)
    },
    predict = function(fit, X) as.numeric(stats::predict(fit, X)),
    importance = function(fit) fit$importance[, 1]),

  IFSVR = list(
    name = "IFSVR", full = "Intuitionistic Fuzzy Support Vector Regression", pkg = "e1071",
    icon = "vector-square", scale = TRUE,
    blurb = "Epsilon-insensitive support vector regression with a kernel (Drucker et al., 1997).",
    params = list(
      list(id = "Cost", label = "Cost (C)", type = "pos", single = 1, range = c(1, 10, 3), help = "Penalty for errors outside the epsilon tube."),
      list(id = "Gamma", label = "Gamma", type = "pos", single = 0.05, range = c(0.01, 0.1, 0.03), help = "Kernel width parameter (radial, polynomial, sigmoid)."),
      list(id = "Epsilon", label = "Epsilon", type = "num0", single = 0.1, range = c(0.05, 0.25, 0.1), help = "Width of the insensitive tube (on the 0-1 scaled target).")),
    settings = list(
      list(id = "Kernel", label = "Kernel", type = "choice", value = "radial",
           choices = c("Radial (RBF)" = "radial", "Linear" = "linear", "Polynomial" = "polynomial", "Sigmoid" = "sigmoid"))),
    fit = function(X, y, p, s) {
      e1071::svm(x = X, y = y, type = "eps-regression", kernel = s$Kernel, cost = p$Cost,
                 gamma = p$Gamma, epsilon = p$Epsilon, scale = FALSE)
    },
    predict = function(fit, X) as.numeric(stats::predict(fit, X)),
    importance = NULL),

  IFXGBoost = list(
    name = "IFXGBoost", full = "Intuitionistic Fuzzy Extreme Gradient Boosting", pkg = "xgboost",
    icon = "bolt", scale = FALSE,
    blurb = "Regularised gradient-boosted regression trees (Chen and Guestrin, 2016).",
    params = list(
      list(id = "Nrounds", label = "Boosting rounds", type = "int", single = 200, range = c(100, 300, 100), help = "Number of trees."),
      list(id = "Eta", label = "Learning rate (eta)", type = "prop", single = 0.1, range = c(0.05, 0.25, 0.1), help = "Shrinkage of each tree."),
      list(id = "Max_Depth", label = "Maximum tree depth", type = "int", single = 4, range = c(2, 6, 2), help = "Depth of each tree."),
      list(id = "Subsample", label = "Row subsample", type = "prop", single = 0.8, range = c(0.6, 1, 0.2), help = "Share of rows used for each tree.")),
    settings = list(),
    fit = function(X, y, p, s) {
      xgboost::xgb.train(
        params = list(objective = "reg:squarederror", eta = p$Eta, max_depth = as.integer(p$Max_Depth),
                      subsample = p$Subsample, nthread = 1, seed = .seed_int()),
        data = xgboost::xgb.DMatrix(X, label = y), nrounds = as.integer(p$Nrounds), verbose = 0)
    },
    predict = function(fit, X) as.numeric(stats::predict(fit, xgboost::xgb.DMatrix(X))),
    importance = function(fit) {
      imp <- xgboost::xgb.importance(model = fit)
      stats::setNames(imp$Gain, imp$Feature)
    }),

  IFLightGBM = list(
    name = "IFLightGBM", full = "Intuitionistic Fuzzy Light Gradient Boosting Machine", pkg = "lightgbm",
    icon = "feather", scale = FALSE,
    blurb = "Histogram-based, leaf-wise gradient boosting (Ke et al., 2017).",
    params = list(
      list(id = "Nrounds", label = "Boosting rounds", type = "int", single = 200, range = c(100, 300, 100), help = "Number of trees."),
      list(id = "Learning_Rate", label = "Learning rate", type = "prop", single = 0.1, range = c(0.05, 0.25, 0.1), help = "Shrinkage of each tree."),
      list(id = "Num_Leaves", label = "Number of leaves", type = "int", single = 15, range = c(7, 31, 8), help = "Maximum leaves per tree."),
      list(id = "Min_Data_In_Leaf", label = "Minimum data in leaf", type = "int", single = 5, range = c(3, 9, 3), help = "Minimum observations in a leaf.")),
    settings = list(),
    fit = function(X, y, p, s) {
      ds <- lightgbm::lgb.Dataset(X, label = y, params = list(feature_pre_filter = FALSE, verbose = -1))
      lightgbm::lgb.train(
        params = list(objective = "regression", learning_rate = p$Learning_Rate,
                      num_leaves = as.integer(p$Num_Leaves), min_data_in_leaf = as.integer(p$Min_Data_In_Leaf),
                      num_threads = 1, seed = .seed_int(), verbose = -1),
        data = ds, nrounds = as.integer(p$Nrounds), verbose = -1)
    },
    predict = function(fit, X) as.numeric(stats::predict(fit, X)),
    importance = function(fit) {
      imp <- lightgbm::lgb.importance(fit)
      if (is.null(imp) || !nrow(imp)) return(NULL)
      stats::setNames(imp$Gain, imp$Feature)
    }),

  IFAdaBoost = list(
    name = "IFAdaBoost", full = "Intuitionistic Fuzzy Adaptive Boosting (AdaBoost.R2)", pkg = "rpart",
    icon = "layer-group", scale = FALSE,
    blurb = "AdaBoost.R2 (Drucker, 1997): regression trees fitted to re-weighted samples, combined by weighted median.",
    params = list(
      list(id = "N_Estimators", label = "Number of estimators", type = "int", single = 100, range = c(50, 150, 50), help = "Maximum boosting iterations."),
      list(id = "Learning_Rate", label = "Learning rate", type = "pos", single = 1, range = c(0.5, 1.5, 0.5), help = "Contribution of each tree."),
      list(id = "Max_Depth", label = "Maximum tree depth", type = "int", single = 3, range = c(2, 4, 1), help = "Depth of each regression tree.")),
    settings = list(
      list(id = "Loss", label = "Loss function", type = "choice", value = "linear",
           choices = c("Linear" = "linear", "Square" = "square", "Exponential" = "exponential"))),
    fit = function(X, y, p, s) .adaboost_r2_fit(X, y, as.integer(p$N_Estimators), p$Learning_Rate, as.integer(p$Max_Depth), s$Loss),
    predict = function(fit, X) .adaboost_r2_predict(fit, X),
    importance = function(fit) {
      imps <- lapply(fit$trees, function(t) t$variable.importance)
      feats <- fit$features
      M <- vapply(imps, function(v) { out <- stats::setNames(rep(0, length(feats)), feats); if (length(v)) out[names(v)] <- v; out }, numeric(length(feats)))
      if (!is.matrix(M)) M <- matrix(M, ncol = 1, dimnames = list(feats, NULL))
      as.numeric(M %*% (fit$weights / sum(fit$weights))) |> stats::setNames(feats)
    }),

  IFCatBoost = list(
    name = "IFCatBoost", full = "Intuitionistic Fuzzy Categorical Boosting", pkg = "catboost",
    icon = "cat", scale = FALSE,
    blurb = "Gradient boosting with ordered boosting and symmetric (oblivious) trees (Prokhorenkova et al., 2018).",
    params = list(
      list(id = "Iterations", label = "Iterations", type = "int", single = 300, range = c(100, 500, 200), help = "Number of trees."),
      list(id = "Learning_Rate", label = "Learning rate", type = "prop", single = 0.1, range = c(0.05, 0.25, 0.1), help = "Shrinkage of each tree."),
      list(id = "Depth", label = "Tree depth", type = "int", single = 4, range = c(2, 6, 2), help = "Depth of the symmetric trees."),
      list(id = "L2_Leaf_Reg", label = "L2 leaf regularisation", type = "pos", single = 3, range = c(1, 5, 2), help = "L2 penalty on leaf values.")),
    settings = list(),
    fit = function(X, y, p, s) {
      pool <- catboost::catboost.load_pool(as.data.frame(X), label = y)
      catboost::catboost.train(pool, params = list(
        loss_function = "RMSE", iterations = as.integer(p$Iterations), learning_rate = p$Learning_Rate,
        depth = as.integer(p$Depth), l2_leaf_reg = p$L2_Leaf_Reg, random_seed = .seed_int() %% 1e6,
        logging_level = "Silent", thread_count = 1, allow_writing_files = FALSE))
    },
    predict = function(fit, X) as.numeric(catboost::catboost.predict(fit, catboost::catboost.load_pool(as.data.frame(X)))),
    importance = function(fit) {
      imp <- catboost::catboost.get_feature_importance(fit)
      stats::setNames(as.numeric(imp), rownames(imp))
    })
)

model_available <- function(key) requireNamespace(IF_MODELS[[key]]$pkg, quietly = TRUE)

# ------------------------------------------------------------
# 5. FIT / PREDICT WRAPPERS
# ------------------------------------------------------------

.fit_model <- function(learner, y, idx, L, prep, params, settings) {
  X <- .feature_matrix(y, idx, L, prep, scale_lags = learner$scale)
  target <- if (learner$scale) .scale01(y[idx], prep$scaling) else y[idx]
  list(model = learner$fit(X, target, params, settings), learner = learner, features = colnames(X))
}

.predict_values <- function(fit, X, prep) {
  p <- fit$learner$predict(fit$model, X)
  if (fit$learner$scale) .unscale01(p, prep$scaling) else p
}

.predict_series <- function(fit, y, idx, L, prep, type = "one-step") {
  sc <- fit$learner$scale
  if (type == "one-step") return(.predict_values(fit, .feature_matrix(y, idx, L, prep, sc), prep))
  yy <- y; out <- numeric(length(idx))
  for (i in seq_along(idx)) {
    t <- idx[i]
    p <- .predict_values(fit, .feature_matrix(yy, t, L, prep, sc), prep)
    yy[t] <- p; out[i] <- p
  }
  out
}

# permutation importance (increase in training RMSE) for models without a native measure
.perm_importance <- function(fit, y, idx, L, prep) {
  X <- .feature_matrix(y, idx, L, prep, fit$learner$scale)
  base <- sqrt(mean((y[idx] - .predict_values(fit, X, prep))^2))
  vapply(seq_len(ncol(X)), function(j) {
    Xp <- X; Xp[, j] <- Xp[sample.int(nrow(X)), j]
    sqrt(mean((y[idx] - .predict_values(fit, Xp, prep))^2)) - base
  }, numeric(1)) |> stats::setNames(colnames(X))
}

# ------------------------------------------------------------
# 6. TUNING HELPERS (as in the packages)
# ------------------------------------------------------------

.range_values <- function(r, name, type = "pos") {
  if (!is.numeric(r) || length(r) != 3L || anyNA(r)) stop(name, ": give min, max and step")
  lo <- r[[1]]; hi <- r[[2]]; st <- r[[3]]
  if (hi < lo) stop(name, ": max must be greater than or equal to min")
  if (lo == hi) v <- lo else {
    if (!(st > 0)) stop(name, ": step must be positive")
    v <- seq(lo, hi, by = st)
  }
  v <- unique(round(v, 10))
  .check_values(v, name, type)
}

.check_values <- function(v, name, type) {
  if (type == "int") {
    if (any(v != round(v)) || any(v < 1)) stop(name, ": values must be positive integers")
    return(as.integer(v))
  }
  if (type == "num0" && any(v < 0)) stop(name, ": values must be non-negative")
  if (type == "pos" && any(!(v > 0))) stop(name, ": values must be positive")
  if (type == "prop" && (any(!(v > 0)) || any(v > 1))) stop(name, ": values must be in (0, 1]")
  v
}

.make_folds <- function(ntrain, Validation, Val_Split, Folds) {
  ntrain <- as.integer(ntrain)
  n_val <- as.integer(max(1, round(ntrain * Val_Split)))
  val_start <- ntrain - n_val + 1L
  if (Validation == "holdout") return(list(list(fit_end = val_start - 1L, idx = val_start:ntrain)))
  Folds <- min(Folds, n_val)
  blocks <- split(val_start:ntrain, cut(seq_len(n_val), Folds, labels = FALSE))
  lapply(unname(blocks), function(b) list(fit_end = as.integer(min(b) - 1L), idx = b))
}

grid_size <- function(spec) {
  prod(vapply(spec, function(p) {
    if (identical(p$mode, "range")) length(tryCatch(.range_values(p$range, p$id, p$type), error = function(e) NA)) else 1L
  }, numeric(1)))
}

# ------------------------------------------------------------
# 7. MAIN FUNCTION
#   spec: list of parameters (Lag first), each list(id, type, mode = "single"/"range", value, range)
# ------------------------------------------------------------

run_ifml <- function(y, model, spec, settings = list(), Class = 3, Split = 0.8,
                     Validation = "holdout", Val_Split = 0.2, Folds = 3, Metric = "RMSE",
                     Forecast_Type = "one-step", m = 2, Alpha = 0.85, Max_Iter = 100, Tol = 1e-5,
                     progress = NULL) {
  learner <- IF_MODELS[[model]]
  if (is.null(learner)) stop("Unknown model: ", model)
  if (!model_available(model)) stop("R package '", learner$pkg, "' is not installed; it is needed for ", model, ".")
  y <- as.numeric(y)
  if (anyNA(y)) stop("The series must not contain missing values")
  n <- length(y)
  if (Class < 2 || Class != round(Class)) stop("Number of clusters must be an integer >= 2")
  Class <- as.integer(Class)
  if (Split <= 0 || Split >= 1) stop("Training share must be between 0 and 1")
  if (m <= 1) stop("Fuzzifier m must be greater than 1")
  if (Alpha <= 0 || Alpha > 1) stop("Alpha must be in (0, 1]")
  if (Val_Split <= 0 || Val_Split >= 1) stop("Validation share must be between 0 and 1")
  ntrain <- as.integer(round(n * Split))
  if (ntrain < 1 || ntrain >= n) stop("Series too short: training and test sets must both be non-empty")
  if (Metric == "MAPE" && any(y[seq_len(ntrain)] == 0)) stop("MAPE cannot be used when the training data contain zeros")
  warn <- character()

  # single values and grids
  fixed <- list(); gv <- list()
  for (p in spec) {
    if (identical(p$mode, "range")) gv[[p$id]] <- .range_values(p$range, p$label %||% p$id, p$type)
    else {
      if (is.null(p$value) || is.na(p$value)) stop(p$label %||% p$id, ": please give a value")
      fixed[[p$id]] <- .check_values(p$value, p$label %||% p$id, p$type)
    }
  }
  min_rows <- 5L
  tuning <- NULL
  best <- fixed
  if (length(gv)) {
    total <- prod(lengths(gv))
    if (total > 5000) stop("The search grid has ", format(total, big.mark = ","), " combinations; please narrow the ranges (maximum 5,000).")
    folds <- .make_folds(ntrain, Validation, Val_Split, Folds)
    first_end <- min(vapply(folds, `[[`, integer(1), "fit_end"))
    max_lag <- first_end - min_rows
    if (!is.null(gv$Lag)) {
      keep <- gv$Lag[gv$Lag <= max_lag]
      if (!length(keep)) stop("The training set is too short for tuning even with lag ", min(gv$Lag), "; use more data, a larger training share or a smaller validation share")
      if (length(keep) < length(gv$Lag)) warn <- c(warn, paste0("Lags larger than ", max_lag, " are too long for the validation window and were dropped from the lag range."))
      gv$Lag <- keep
    } else if (fixed$Lag > max_lag) {
      stop("Lag = ", fixed$Lag, " is too large for tuning on this training set (maximum ", max(max_lag, 0), ")")
    }
    preps <- lapply(folds, function(fo) .prepare(y[seq_len(fo$fit_end)], Class, m, Alpha, Max_Iter, Tol))
    objective <- function(vals) {
      p <- utils::modifyList(fixed, vals)
      L <- as.integer(p$Lag)
      res <- tryCatch({
        act <- numeric(0); prd <- numeric(0)
        for (f in seq_along(folds)) {
          fo <- folds[[f]]
          fit <- .fit_model(learner, y, (L + 1L):fo$fit_end, L, preps[[f]], p, settings)
          prd <- c(prd, .predict_series(fit, y, fo$idx, L, preps[[f]], Forecast_Type))
          act <- c(act, y[fo$idx])
        }
        .metrics(act, prd)[[Metric]]
      }, error = function(e) Inf)
      if (is.na(res)) Inf else res
    }
    grid <- expand.grid(gv, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    total <- nrow(grid)
    scores <- numeric(total); bestsc <- Inf
    t0 <- Sys.time()
    for (i in seq_len(total)) {
      scores[i] <- objective(as.list(grid[i, , drop = FALSE]))
      if (scores[i] < bestsc) bestsc <- scores[i]
      if (is.function(progress)) progress(i, total, bestsc, as.numeric(difftime(Sys.time(), t0, units = "secs")))
    }
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    history <- cbind(Combination = seq_len(total), grid, Score = scores)
    if (!any(is.finite(history$Score))) stop("Every combination failed during tuning; check the search ranges")
    b <- which.min(history$Score)
    for (nm in names(gv)) best[[nm]] <- history[[nm]][b]
    tuning <- list(Validation = Validation, Metric = Metric, Combinations = total,
                   Best_Score = history$Score[b], Elapsed_Seconds = elapsed, History = history,
                   Tuned = names(gv), Folds = length(folds))
  }
  L <- as.integer(best$Lag)
  if (ntrain - L < min_rows) stop("Lag = ", L, " leaves fewer than ", min_rows, " training observations; use a smaller lag or more data")

  # final model on the whole training set
  prep <- .prepare(y[seq_len(ntrain)], Class, m, Alpha, Max_Iter, Tol)
  train_idx <- (L + 1L):ntrain
  test_idx <- (ntrain + 1L):n
  fit <- .fit_model(learner, y, train_idx, L, prep, best, settings)
  extra <- list()
  if (model == "IFRF") extra$Mtry_Used <- as.integer(fit$model$mtry)
  train_fitted <- .predict_series(fit, y, train_idx, L, prep, "one-step")
  test_pred <- .predict_series(fit, y, test_idx, L, prep, Forecast_Type)
  acc <- rbind("Training set" = .metrics(y[train_idx], train_fitted))
  if (model == "IFRF") acc <- rbind(acc, "Training OOB" = .metrics(y[train_idx], as.numeric(fit$model$predicted)))
  acc <- rbind(acc, "Test set" = .metrics(y[test_idx], test_pred))

  imp_type <- "Model-based"
  imp <- if (is.function(learner$importance)) tryCatch(learner$importance(fit$model), error = function(e) NULL) else NULL
  if (is.null(imp)) { imp <- .perm_importance(fit, y, train_idx, L, prep); imp_type <- "Permutation (increase in training RMSE)" }
  imp <- sort(imp[is.finite(imp)], decreasing = TRUE)

  list(Model = model, Full_Name = learner$full,
       Accuracy = acc,
       Train_Fitted = train_fitted, Test_Prediction = test_pred,
       Train_Actual = y[train_idx], Test_Actual = y[test_idx],
       Train_Index = train_idx, Test_Index = test_idx,
       Best_Params = c(best[c("Lag", setdiff(names(best), "Lag"))], extra),
       Importance = imp, Importance_Type = imp_type,
       Tuning = tuning, IFCM = prep$ifcm, Prep = prep, Fit = fit,
       Settings = c(list(Class = Class, Split = Split, Forecast_Type = Forecast_Type, m = m, Alpha = Alpha,
                         Validation = Validation, Val_Split = Val_Split, Folds = Folds, Metric = Metric,
                         n_train = ntrain, n_test = n - ntrain, n_inputs = length(fit$features)), settings),
       Warnings = warn)
}

# spec helpers ------------------------------------------------
default_spec <- function(model, lag_mode = "range") {
  ps <- c(list(LAG_PARAM), IF_MODELS[[model]]$params)
  lapply(ps, function(p) c(p, list(mode = if (p$id == "Lag") lag_mode else "single", value = p$single)))
}

default_settings <- function(model) {
  st <- IF_MODELS[[model]]$settings
  stats::setNames(lapply(st, `[[`, "value"), vapply(st, `[[`, "", "id"))
}

# feature-group summary of importance (lags / membership / non-membership)
importance_groups <- function(imp) {
  grp <- ifelse(grepl("^Lag", names(imp)), "Lagged values",
                ifelse(grepl("^Mu_", names(imp)), "Membership degrees (mu)", "Non-membership degrees (nu)"))
  agg <- tapply(pmax(imp, 0), grp, sum)
  data.frame(Group = names(agg), Importance = as.numeric(agg), Share_percent = 100 * as.numeric(agg) / sum(agg))
}
