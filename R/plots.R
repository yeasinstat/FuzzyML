# ============================================================
# FuzzyML : interactive plots (plotly)
# ============================================================

pal_series <- c("#5b3fd1", "#ff7a59", "#0e9488", "#e84393", "#f59e0b", "#3b82f6",
                "#8b5cf6", "#ef4444", "#10b981", "#a855f7", "#334155", "#06b6d4")
series_cols <- function(n) if (n <= length(pal_series)) pal_series[seq_len(n)] else grDevices::colorRampPalette(pal_series)(n)
hex_rgba <- function(hex, a) { v <- grDevices::col2rgb(hex); sprintf("rgba(%d,%d,%d,%.2f)", v[1], v[2], v[3], a) }

plotly_theme <- function(p, xlab = "", ylab = "", legend = TRUE) {
  p |>
    plotly::layout(
      font = list(family = "Inter, Segoe UI, Arial", size = 13, color = "#211a38"),
      xaxis = list(title = xlab, gridcolor = "#ece8f6", zeroline = FALSE),
      yaxis = list(title = ylab, gridcolor = "#ece8f6", zeroline = FALSE),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      hovermode = "closest", showlegend = legend,
      legend = list(orientation = "h", y = -0.2),
      margin = list(t = 30, r = 20, b = 60, l = 70)) |>
    plotly::config(displaylogo = FALSE, toImageButtonOptions = list(format = "png", scale = 3))
}

split_shape <- function(x0, x1) {
  list(type = "rect", xref = "x", yref = "paper", x0 = x0, x1 = x1, y0 = 0, y1 = 1,
       fillcolor = "rgba(255,122,89,0.10)", line = list(width = 0), layer = "below")
}

# series with optional train/test shading
plot_series <- function(dat, var, time_name, ntrain = NULL) {
  p <- plotly::plot_ly(x = dat$.x, y = dat[[var]], type = "scatter", mode = "lines", name = var,
                       text = dat$.label, line = list(color = "#5b3fd1", width = 2),
                       hovertemplate = paste0("%{text}<br>", var, ": %{y:.4g}<extra></extra>"))
  p <- plotly_theme(p, time_name, var, legend = FALSE)
  if (!is.null(ntrain) && ntrain < nrow(dat)) {
    p <- plotly::layout(p, shapes = list(split_shape(dat$.x[ntrain + 1], dat$.x[nrow(dat)])),
                        annotations = list(
                          list(x = dat$.x[max(1, round(ntrain / 2))], y = 1.04, yref = "paper", text = "<b>Training</b>", showarrow = FALSE, font = list(color = "#5b3fd1")),
                          list(x = dat$.x[round((ntrain + 1 + nrow(dat)) / 2)], y = 1.04, yref = "paper", text = "<b>Test</b>", showarrow = FALSE, font = list(color = "#e04e2b"))))
  }
  p
}

# actual vs fitted (training) and predicted (test)
plot_forecast <- function(dat, var, res, time_name) {
  x <- dat$.x; lab <- dat$.label
  ti <- res$Train_Index; te <- res$Test_Index
  plotly::plot_ly() |>
    plotly::add_lines(x = x, y = dat[[var]], name = "Actual", text = lab, line = list(color = "#23184a", width = 2),
                      hovertemplate = "%{text}<br>Actual: %{y:.4g}<extra></extra>") |>
    plotly::add_lines(x = x[ti], y = res$Train_Fitted, name = "Fitted (training)", text = lab[ti],
                      line = list(color = "#8b5cf6", width = 2, dash = "dot"),
                      hovertemplate = "%{text}<br>Fitted: %{y:.4g}<extra></extra>") |>
    plotly::add_trace(x = x[te], y = res$Test_Prediction, name = "Predicted (test)", text = lab[te],
                      type = "scatter", mode = "lines+markers", line = list(color = "#ff7a59", width = 3),
                      marker = list(size = 7, color = "#ff7a59", line = list(color = "#fff", width = 1)),
                      hovertemplate = "%{text}<br>Predicted: %{y:.4g}<extra></extra>") |>
    plotly_theme(time_name, var) |>
    plotly::layout(shapes = list(split_shape(x[te[1]], x[te[length(te)]])))
}

plot_scatter <- function(res) {
  a <- res$Test_Actual; p <- res$Test_Prediction
  lim <- range(c(a, p))
  plotly::plot_ly() |>
    plotly::add_lines(x = lim, y = lim, name = "Perfect fit", line = list(color = "grey", dash = "dash")) |>
    plotly::add_markers(x = a, y = p, name = "Test set", marker = list(color = "#ff7a59", size = 9, line = list(color = "#fff", width = 1)),
                        hovertemplate = "Actual: %{x:.4g}<br>Predicted: %{y:.4g}<extra></extra>") |>
    plotly_theme("Actual", "Predicted")
}

plot_tuning <- function(res) {
  h <- res$Tuning$History
  metric <- res$Tuning$Metric
  h <- h[is.finite(h$Score), , drop = FALSE]
  if ("Lag" %in% names(h) && length(unique(h$Lag)) > 1) {
    agg <- stats::aggregate(Score ~ Lag, data = h, FUN = min)
    b <- agg$Lag[which.min(agg$Score)]
    plotly::plot_ly(agg, x = ~Lag, y = ~Score, type = "scatter", mode = "lines+markers", name = "Best score per lag",
                    line = list(color = "#5b3fd1", width = 3), marker = list(size = 9, color = ifelse(agg$Lag == b, "#ff7a59", "#5b3fd1")),
                    hovertemplate = paste0("Lag %{x}<br>", metric, ": %{y:.4g}<extra></extra>")) |>
      plotly_theme("Lag order", paste("Validation", metric), legend = FALSE) |>
      plotly::layout(xaxis = list(dtick = 1))
  } else {
    pars <- setdiff(names(h), c("Combination", "Score"))
    txt <- apply(h[, pars, drop = FALSE], 1, function(r) paste(paste(pars, r, sep = " = "), collapse = "<br>"))
    best <- which.min(h$Score)
    plotly::plot_ly(x = h$Combination, y = h$Score, type = "bar", text = txt, textposition = "none",
                    marker = list(color = ifelse(seq_len(nrow(h)) == best, "#ff7a59", "#5b3fd1")),
                    hovertemplate = paste0("%{text}<br>", metric, ": %{y:.4g}<extra></extra>")) |>
      plotly_theme("Combination", paste("Validation", metric), legend = FALSE)
  }
}

# intuitionistic fuzzy membership, non-membership and hesitation curves
plot_membership <- function(res, var) {
  cl <- res$IFCM
  rng <- range(res$Prep$ifcm$centers, res$Train_Actual, res$Test_Actual)
  xs <- seq(rng[1], rng[2], length.out = 300)
  d <- .if_degrees(xs, cl$centers, cl$m, cl$alpha)
  K <- length(cl$centers); cols <- series_cols(K)
  p <- plotly::plot_ly()
  for (k in seq_len(K)) {
    nm <- sprintf("C%d (%.4g)", k, cl$centers[k])
    p <- p |>
      plotly::add_lines(x = xs, y = d$mu[, k], name = paste(nm, "- membership"), legendgroup = k,
                        line = list(color = cols[k], width = 3),
                        hovertemplate = paste0("x = %{x:.4g}<br>mu = %{y:.3f}<extra>C", k, "</extra>")) |>
      plotly::add_lines(x = xs, y = d$nu[, k], name = paste(nm, "- non-membership"), legendgroup = k,
                        line = list(color = cols[k], width = 2, dash = "dash"),
                        hovertemplate = paste0("x = %{x:.4g}<br>nu = %{y:.3f}<extra>C", k, "</extra>")) |>
      plotly::add_lines(x = xs, y = 1 - d$mu[, k] - d$nu[, k], name = paste(nm, "- hesitation"), legendgroup = k,
                        line = list(color = cols[k], width = 1.5, dash = "dot"), visible = "legendonly",
                        hovertemplate = paste0("x = %{x:.4g}<br>pi = %{y:.3f}<extra>C", k, "</extra>"))
  }
  plotly_theme(p, paste("Value of", var), "Degree") |>
    plotly::layout(yaxis = list(range = c(-0.02, 1.02)), legend = list(orientation = "v", x = 1.02, y = 1, font = list(size = 11)),
                   margin = list(t = 20, r = 10, b = 60, l = 60))
}

plot_importance <- function(res, top = 20) {
  imp <- res$Importance
  if (!length(imp)) return(NULL)
  imp <- utils::head(imp, top)
  grp <- ifelse(grepl("^Lag", names(imp)), "#5b3fd1", ifelse(grepl("^Mu_", names(imp)), "#0e9488", "#ff7a59"))
  plotly::plot_ly(x = rev(as.numeric(imp)), y = factor(rev(names(imp)), levels = rev(names(imp))), type = "bar", orientation = "h",
                  marker = list(color = rev(grp)), hovertemplate = "%{y}: %{x:.4g}<extra></extra>") |>
    plotly_theme(res$Importance_Type, "", legend = FALSE) |>
    plotly::layout(margin = list(l = 110))
}

plot_compare <- function(dat, var, runs, time_name) {
  first <- runs[[1]]
  te <- first$Test_Index
  p <- plotly::plot_ly() |>
    plotly::add_lines(x = dat$.x[te], y = dat[[var]][te], name = "Actual", text = dat$.label[te],
                      line = list(color = "#23184a", width = 3), hovertemplate = "%{text}<br>Actual: %{y:.4g}<extra></extra>")
  cols <- series_cols(length(runs))
  for (i in seq_along(runs)) {
    r <- runs[[i]]
    p <- plotly::add_trace(p, x = dat$.x[r$Test_Index], y = r$Test_Prediction, name = names(runs)[i], text = dat$.label[r$Test_Index],
                           type = "scatter", mode = "lines+markers", line = list(color = cols[i], width = 2), marker = list(size = 5),
                           hovertemplate = paste0("%{text}<br>", names(runs)[i], ": %{y:.4g}<extra></extra>"))
  }
  plotly_theme(p, time_name, var)
}
