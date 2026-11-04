# ============================================================
# FuzzyML : data utilities (file reading and automatic time-column parsing)
# ============================================================

# ------------------------------------------------------------
# 1. FILE READING
# ------------------------------------------------------------

file_sheets <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("xlsx", "xls")) readxl::excel_sheets(path) else NULL
}

read_any <- function(path, name, sheet = NULL) {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("xlsx", "xls")) {
    df <- readxl::read_excel(path, sheet = if (is.null(sheet) || sheet == "") 1 else sheet)
  } else if (ext %in% c("csv", "txt", "tsv")) {
    first <- readLines(path, n = 1, warn = FALSE)
    sep <- if (ext == "tsv" || grepl("\t", first)) "\t" else if (grepl(";", first) && !grepl(",", first)) ";" else ","
    df <- utils::read.csv(path, sep = sep, check.names = FALSE,
                          stringsAsFactors = FALSE, strip.white = TRUE)
  } else {
    stop("Unsupported file type '.", ext, "'. Please upload a .csv, .xlsx or .xls file.")
  }
  df <- as.data.frame(df, check.names = FALSE)
  # drop fully empty rows / columns
  blank <- vapply(df, function(v) is.na(v) | (is.character(v) & trimws(as.character(v)) == ""),
                  logical(nrow(df)))
  blank <- matrix(blank, nrow = nrow(df))
  df <- df[rowSums(!blank) > 0, colSums(!blank) > 0, drop = FALSE]
  names(df) <- trimws(names(df))
  df
}

guess_date_col <- function(df) {
  nm <- names(df)
  hit <- grep("date|time|month|year|period|week|day|quarter|qtr|hour|season|^fy|^t$|index|obs", nm, ignore.case = TRUE)
  if (length(hit)) nm[hit[1]] else nm[1]
}

# ------------------------------------------------------------
# 2. TIME COLUMN PARSING (fully automatic, any format)
# ------------------------------------------------------------
# The time column is kept exactly as the user wrote it (".label") for display,
# and converted to an internal ordering value (".x") for sorting, merging and plotting:
#   kind = "date"     -> .x is a Date            (days, weeks, months, quarters, years ...)
#   kind = "datetime" -> .x is a POSIXct (UTC)   (hours, minutes ...)
#   kind = "index"    -> .x is a number          (t1, t2 ... / Period 5 / 1, 2, 3 / any labels)

time_format_choices <- c(
  "Auto-detect (recommended)" = "auto",
  "Plain time index / labels (no calendar)" = "index",
  "YYYY-MM-DD (2015-01-31)" = "%Y-%m-%d",
  "DD-MM-YYYY (31-01-2015)" = "%d-%m-%Y",
  "DD/MM/YYYY (31/01/2015)" = "%d/%m/%Y",
  "MM/DD/YYYY (01/31/2015)" = "%m/%d/%Y",
  "YYYY/MM/DD (2015/01/31)" = "%Y/%m/%d",
  "MM/YYYY (01/2015)" = "%m/%Y",
  "MM-YYYY (01-2015)" = "%m-%Y",
  "YYYY-MM (2015-01)" = "%Y-%m",
  "Mon-YYYY (Jan-2015)" = "%b-%Y",
  "Mon-YY (Jan-15)" = "%b-%y",
  "Mon YYYY (Jan 2015)" = "%b %Y",
  "Month YYYY (January 2015)" = "%B %Y",
  "YYYY (2015)" = "%Y",
  "YYYY-MM-DD HH:MM (2015-01-31 14:00)" = "%Y-%m-%d %H:%M",
  "DD-MM-YYYY HH:MM (31-01-2015 14:00)" = "%d-%m-%Y %H:%M",
  "DD/MM/YYYY HH:MM (31/01/2015 14:00)" = "%d/%m/%Y %H:%M",
  "MM/DD/YYYY HH:MM (01/31/2015 14:00)" = "%m/%d/%Y %H:%M"
)
date_format_choices <- time_format_choices   # backward compatibility

# time values exactly as written by the user
as_label <- function(v) {
  if (inherits(v, "POSIXt")) {
    midnight <- all(is.na(v) | format(v, "%H:%M:%S") == "00:00:00")
    return(format(v, if (midnight) "%Y-%m-%d" else "%Y-%m-%d %H:%M"))
  }
  if (inherits(v, "Date")) return(format(v, "%Y-%m-%d"))
  if (is.numeric(v)) return(ifelse(is.na(v), NA, format(v, trim = TRUE, scientific = FALSE, drop0trailing = TRUE)))
  out <- trimws(as.character(v))
  out[out %in% c("", "NA", "N/A", "na")] <- NA
  out
}

# fixed-format parser (used when the user picks a format manually)
parse_dates <- function(x, fmt) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  x <- trimws(as.character(x))
  if (!grepl("%d", fmt, fixed = TRUE)) {
    if (fmt == "%Y") return(as.Date(paste0(x, "-01-01")))
    return(as.Date(paste0("01-", x), format = paste0("%d-", fmt)))
  }
  as.Date(x, format = fmt)
}

tm <- function(x, kind, desc) list(x = x, kind = kind, desc = desc)

# ordered index for labels that are not calendar times (t1, Period 5, Kharif-2015, ...)
index_time <- function(lab, desc = "Time labels (ordered as in the file)") {
  num <- suppressWarnings(as.numeric(sub("^.*?(-?\\d+(?:\\.\\d+)?)\\D*$", "\\1", lab, perl = TRUE)))
  ok <- !is.na(lab)
  if (all(!is.na(num[ok])) && !anyDuplicated(num[ok]) && all(diff(num[ok]) > 0))
    return(tm(num, "index", paste0(desc, " - numbered")))
  x <- rep(NA_real_, length(lab)); x[ok] <- seq_len(sum(ok))
  tm(x, "index", desc)
}

parse_time <- function(v, fmt = "auto") {
  lab <- as_label(v)
  ok <- !is.na(lab)
  if (fmt == "index") return(index_time(lab, "Time index (as given)"))
  if (inherits(v, "POSIXt")) {
    v <- as.POSIXct(format(v, "%Y-%m-%d %H:%M:%S"), tz = "UTC")
    if (all(is.na(v) | format(v, "%H:%M:%S") == "00:00:00")) return(tm(as.Date(v), "date", "Calendar dates"))
    return(tm(v, "datetime", "Date-time stamps"))
  }
  if (inherits(v, "Date")) return(tm(v, "date", "Calendar dates"))
  if (fmt != "auto") {
    if (grepl("%H", fmt, fixed = TRUE))
      return(tm(as.POSIXct(lab, format = fmt, tz = "UTC"), "datetime", "Date-time stamps (format chosen by user)"))
    return(tm(parse_dates(lab, fmt), "date", "Calendar dates (format chosen by user)"))
  }
  s <- lab
  if (!any(ok)) return(tm(rep(NA_real_, length(s)), "index", "Empty"))
  all_match <- function(re) all(grepl(re, s[ok], perl = TRUE))
  grab <- function(re, i) suppressWarnings(as.integer(sub(re, paste0("\\", i), s, perl = TRUE)))

  # ---- purely numeric values
  num <- suppressWarnings(as.numeric(s))
  if (all(!is.na(num[ok]))) {
    nn <- num[ok]; int <- all(nn == round(nn))
    if (int && all(nn >= 1000 & nn <= 3000)) return(tm(as.Date(paste0(num, "-01-01")), "date", "Years"))
    if (int && all(nn >= 100001 & nn <= 300012) && all(nn %% 100 >= 1 & nn %% 100 <= 12))
      return(tm(as.Date(sprintf("%d-%02d-01", num %/% 100, num %% 100)), "date", "Year-month numbers (YYYYMM)"))
    if (int && all(nn >= 10000101 & nn <= 30001231)) {
      d <- as.Date(as.character(num), format = "%Y%m%d")
      if (all(!is.na(d[ok]))) return(tm(d, "date", "Date numbers (YYYYMMDD)"))
    }
    if (all(nn > 15000 & nn < 80000) && length(unique(nn)) > 1) {        # Excel serial numbers
      if (int) return(tm(as.Date(num, origin = "1899-12-30"), "date", "Excel date serial numbers"))
      return(tm(as.POSIXct(round((num - 25569) * 86400), origin = "1970-01-01", tz = "UTC"), "datetime", "Excel date-time serial numbers"))
    }
    return(tm(num, "index", "Numeric time index"))
  }

  # ---- quarters: 2015Q1, 2015-Q1, 2015 Qtr 1, Q1 2015, Q1-2015, Quarter 1 2015
  r1 <- "^(\\d{4})\\s*[-/:._ ]?\\s*[Qq](?:tr|uarter)?\\s*[-._ ]?([1-4])$"
  r2 <- "^[Qq](?:tr|uarter)?\\s*[-._ ]?([1-4])\\s*[-/:._, ]?\\s*(\\d{4})$"
  if (all_match(r1)) return(tm(as.Date(sprintf("%d-%02d-01", grab(r1, 1), (grab(r1, 2) - 1) * 3 + 1)), "date", "Quarters"))
  if (all_match(r2)) return(tm(as.Date(sprintf("%d-%02d-01", grab(r2, 2), (grab(r2, 1) - 1) * 3 + 1)), "date", "Quarters"))
  # ---- half-years: 2015H1, H2-2015
  r3 <- "^(\\d{4})\\s*[-/:._ ]?\\s*[Hh]\\s*[-._ ]?([12])$"
  r4 <- "^[Hh]\\s*[-._ ]?([12])\\s*[-/:._, ]?\\s*(\\d{4})$"
  if (all_match(r3)) return(tm(as.Date(sprintf("%d-%02d-01", grab(r3, 1), (grab(r3, 2) - 1) * 6 + 1)), "date", "Half-years"))
  if (all_match(r4)) return(tm(as.Date(sprintf("%d-%02d-01", grab(r4, 2), (grab(r4, 1) - 1) * 6 + 1)), "date", "Half-years"))
  # ---- weeks: 2015W05, 2015-W5, 2015 Week 5, W5 2015
  r5 <- "^(\\d{4})\\s*[-/:._ ]?\\s*[Ww](?:k|eek)?\\s*[-._ ]?(\\d{1,2})$"
  r6 <- "^[Ww](?:k|eek)?\\s*[-._ ]?(\\d{1,2})\\s*[-/:._, ]?\\s*(\\d{4})$"
  wk <- function(y, w) as.Date(paste0(y, "-01-01")) + (w - 1) * 7
  if (all_match(r5)) return(tm(wk(grab(r5, 1), grab(r5, 2)), "date", "Weeks"))
  if (all_match(r6)) return(tm(wk(grab(r6, 2), grab(r6, 1)), "date", "Weeks"))
  # ---- financial / agricultural years: 2015-16, 2015/16, 2015-2016, FY2015-16
  r7 <- "^(?:[A-Za-z]+\\s*)?(\\d{4})\\s*[-/]\\s*(\\d{2}|\\d{4})$"
  if (all_match(r7)) {
    y1 <- grab(r7, 1); y2 <- grab(r7, 2)
    if (all(((y2 %% 100) == ((y1 + 1) %% 100))[ok])) return(tm(as.Date(paste0(y1, "-01-01")), "date", "Financial / agricultural years"))
  }
  # ---- date-times with a clock time
  if (all_match("\\d{1,2}:\\d{2}")) {
    if (all_match("^\\d{1,2}:\\d{2}(:\\d{2})?\\s*([AaPp][Mm])?$")) {     # time of day only
      d <- suppressWarnings(lubridate::parse_date_time(paste("2000-01-01", s), c("Ymd HMS", "Ymd HM", "Ymd IMSp", "Ymd IMp"), tz = "UTC", quiet = TRUE))
      if (mean(!is.na(d[ok])) >= 0.9) return(tm(d, "datetime", "Clock times"))
    }
    d <- suppressWarnings(lubridate::parse_date_time(
      s, c("Ymd HMS", "Ymd HM", "dmY HMS", "dmY HM", "mdY HMS", "mdY HM", "Ymd IMSp", "Ymd IMp",
           "dmY IMSp", "dmY IMp", "mdY IMSp", "mdY IMp", "dbY HMS", "dbY HM", "bdY HMS", "bdY HM"),
      tz = "UTC", quiet = TRUE))
    if (mean(!is.na(d[ok])) >= 0.9) {
      if (all(is.na(d) | format(d, "%H:%M:%S") == "00:00:00")) return(tm(as.Date(d), "date", "Calendar dates"))
      return(tm(d, "datetime", "Date-time stamps"))
    }
  }
  # ---- general calendar formats (any separator, month names, 2- or 4-digit years)
  # count date parts (number groups + month name) so that "Jan-2015" is never read as day 20, year 15
  mon_re <- "(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)[a-z]*"
  parts <- lengths(regmatches(s, gregexpr("\\d+", s))) + grepl(mon_re, tolower(s))
  try_orders <- function(ord, desc) {
    d <- suppressWarnings(lubridate::parse_date_time(s, ord, tz = "UTC", quiet = TRUE))
    if (mean(!is.na(d[ok])) >= 0.9 && length(unique(d[ok & !is.na(d)])) > 1) tm(as.Date(d), "date", desc) else NULL
  }
  res <- NULL
  if (all(parts[ok] >= 3))
    res <- try_orders(c("Ymd", "dmY", "mdY", "dbY", "bdY", "Ybd", "dmy", "mdy", "ymd", "dby", "bdy"), "Calendar dates")
  if (is.null(res) && all(parts[ok] == 2))
    res <- try_orders(c("bY", "BY", "Yb", "mY", "Ym", "by", "yb", "my", "ym"), "Months")
  if (!is.null(res)) return(res)

  # ---- anything else: ordered time labels
  index_time(s)
}

to_numeric <- function(v) {
  if (is.numeric(v)) return(as.numeric(v))
  v <- gsub("[,[:space:]]", "", as.character(v))
  v[v %in% c("", "NA", "na", "-", ".", "NaN")] <- NA
  suppressWarnings(as.numeric(v))
}

# ------------------------------------------------------------
# 3. TIME UNIT, SEASONAL PERIOD AND SEASON INDEX
# ------------------------------------------------------------

detect_time_structure <- function(x, kind) {
  if (kind == "index") return(list(freq = 1, unit = "periods"))
  u <- sort(unique(x))
  if (length(u) < 3) return(list(freq = 1, unit = "periods"))
  if (kind == "datetime") {
    md <- stats::median(diff(as.numeric(u)))                 # seconds
    if (md <= 90) return(list(freq = 60, unit = "minutes"))
    if (md <= 5400) return(list(freq = 24, unit = "hours"))
    u <- unique(as.Date(u))
  }
  md <- stats::median(as.numeric(diff(u)))                    # days
  if (md <= 1.5) return(list(freq = 7, unit = "days"))
  if (md <= 8) return(list(freq = 52, unit = "weeks"))
  if (md <= 17) return(list(freq = 24, unit = "fortnights"))
  if (md <= 35) return(list(freq = 12, unit = "months"))
  if (md <= 100) return(list(freq = 4, unit = "quarters"))
  if (md <= 200) return(list(freq = 2, unit = "half-years"))
  list(freq = 1, unit = "years")
}

# legacy helper (kept for compatibility)
frequency_unit <- function(freq) {
  switch(as.character(freq), "365" = "days", "52" = "weeks", "24" = "fortnights", "12" = "months",
         "4" = "quarters", "2" = "half-years", "1" = "years", "periods")
}

season_index <- function(dat, freq, unit = "periods") {
  n <- nrow(dat)
  if (is.null(freq) || freq <= 1) return(rep(1L, n))
  x <- dat$.x
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    m <- as.integer(format(x, "%m"))
    if (unit == "months" && freq == 12) return(m)
    if (unit == "quarters" && freq == 4) return((m - 1L) %/% 3L + 1L)
    if (unit == "half-years" && freq == 2) return(ifelse(m <= 6, 1L, 2L))
    if (unit == "weeks" && freq == 52) return(pmin(52L, as.integer(format(x, "%V"))))
    if (unit == "days" && freq == 7) return(as.integer(format(x, "%u")))
    if (unit == "days" && freq == 365) return(pmin(365L, as.integer(format(x, "%j"))))
    if (unit == "hours" && freq == 24) return(as.integer(format(x, "%H")) + 1L)
    if (unit == "minutes" && freq == 60) return(as.integer(format(x, "%M")) + 1L)
  }
  as.integer(((seq_len(n) - 1) %% freq) + 1)                  # position in the cycle
}

# ------------------------------------------------------------
# 4. PREPARE THE SERIES FILE
# ------------------------------------------------------------
NO_TIME <- "(No time column - use row order)"

prepare_series <- function(df, time_col, time_fmt = "auto", missing = "interpolate") {
  msgs <- character()
  if (is.null(time_col) || time_col == NO_TIME || !time_col %in% names(df)) {
    tp <- list(x = seq_len(nrow(df)), kind = "index", desc = "Row order (no time column)")
    lab <- as.character(seq_len(nrow(df)))
    vals <- df
    time_name <- "Observation"
  } else {
    tp <- parse_time(df[[time_col]], time_fmt)
    lab <- as_label(df[[time_col]])
    vals <- df[, setdiff(names(df), time_col), drop = FALSE]
    time_name <- time_col
  }
  vals[] <- lapply(vals, to_numeric)
  keep <- vapply(vals, function(v) sum(!is.na(v)) >= 10, logical(1))
  if (any(!keep)) msgs <- c(msgs, paste0("Columns ignored (not numeric or fewer than 10 values): ", paste(names(vals)[!keep], collapse = ", ")))
  vals <- vals[, keep, drop = FALSE]
  if (!ncol(vals)) stop("No numeric series column found. The file needs at least one numeric column with 10 or more values.")
  out <- data.frame(.x = tp$x, .label = lab, vals, check.names = FALSE, stringsAsFactors = FALSE)
  bad <- is.na(out$.x)
  if (all(bad)) stop("The time column could not be read. Choose another 'Time format' or '", NO_TIME, "'.")
  if (any(bad)) msgs <- c(msgs, sprintf("%d row(s) with an empty or unreadable time value were dropped.", sum(bad)))
  out <- out[!bad, , drop = FALSE]
  out <- out[order(as.numeric(out$.x)), , drop = FALSE]
  if (anyDuplicated(as.numeric(out$.x))) msgs <- c(msgs, "Some time values are repeated; rows are kept in time order.")
  for (v in names(vals)) {
    z <- out[[v]]
    if (anyNA(z)) {
      if (missing == "interpolate" && sum(!is.na(z)) >= 2) {
        out[[v]] <- stats::approx(seq_along(z), z, xout = seq_along(z), rule = 2)$y
        msgs <- c(msgs, sprintf("%s: %d missing value(s) filled by linear interpolation.", v, sum(is.na(z))))
      }
    }
  }
  rownames(out) <- NULL
  ts_info <- detect_time_structure(out$.x, tp$kind)
  list(data = out, series = names(vals), kind = tp$kind, time_desc = tp$desc, time_name = time_name,
       unit = ts_info$unit, messages = msgs)
}

series_stats <- function(v) {
  v <- v[is.finite(v)]
  n <- length(v); m <- mean(v); s <- stats::sd(v)
  sk <- mean((v - m)^3) / mean((v - m)^2)^1.5
  ku <- mean((v - m)^4) / mean((v - m)^2)^2
  vals <- c(m, stats::median(v), s, min(v), max(v), 100 * s / abs(m), sk, ku)
  data.frame(Statistic = c("Observations", "Mean", "Median", "Standard deviation", "Minimum", "Maximum",
                           "Coefficient of variation (%)", "Skewness", "Kurtosis"),
             Value = c(format(n), formatC(vals, digits = 4, format = "f", big.mark = ",")))
}
