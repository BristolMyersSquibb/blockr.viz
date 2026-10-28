#' The domain a column's marks are drawn against, as the axis prints it: the
#' numbers `summarize_cells()` scales with. `NULL` for a column with no scale.
#' @noRd
summarize_axis_domain <- function(p, prep = NULL) {
  ok <- function(a, b) {
    length(a) && length(b) && is.finite(a) && is.finite(b) && b > a
  }
  kind <- p$kind %||% ""
  if (kind %in% c("box", "pointrange", "pair")) {
    if (!ok(p$dmin, p$dmax)) return(NULL)
    return(list(d0 = p$dmin, d1 = p$dmax))
  }
  if (kind %in% c("bar", "barsplit")) {
    # Zero baseline: width is value / column max. A 100% split's domain is
    # the percentage itself.
    d1 <- if (identical(p$mode, "percent")) 100 else p$dmax %||% prep$bar_max
    if (!ok(0, d1)) return(NULL)
    list(d0 = 0, d1 = d1,
         # Faceted percentage bars (`denom`) and 100% splits read as percentages.
         pct = identical(p$mode, "percent") || !is.null(p$denom))
  } else if (identical(kind, "bardiv")) {
    # Zero-centred: -max .. +max around the comparator.
    mx <- p$dmax %||% prep$bar_max
    if (!ok(0, mx)) return(NULL)
    list(d0 = -mx, d1 = mx)
  } else if (kind %in% c("interval", "sparkline")) {
    # The x domain shared by the column.
    d <- p$dom %||% prep$dom
    if (length(d) < 2L || !ok(d[[1L]], d[[2L]])) return(NULL)
    list(d0 = d[[1L]], d1 = d[[2L]],
         date = isTRUE(p$dom_date %||% prep$dom_date))
  } else {
    NULL
  }
}

#' Nice tick values: the 1 / 2 / 2.5 / 5 / 10 ladder, taking the step closest
#' to an even split. Ticks outside the domain are cut, not clamped.
#' @noRd
summarize_axis_ticks <- function(d0, d1, k = 4L) {
  raw <- (d1 - d0) / max(1L, k - 1L)
  if (!is.finite(raw) || raw <= 0) return(numeric())
  mag <- 10^floor(log10(raw))
  cand <- c(1, 2, 2.5, 5, 10) * mag
  step <- cand[[which.min(abs(cand - raw))]]
  t <- seq(ceiling(d0 / step) * step, d1, by = step)
  t[t >= d0 & t <= d1]
}

#' Decimals for a column's value labels: one finer than its axis ticks. Whole
#' numbers stay whole. `NA` without an axis (labels keep lane_fmt()). `x` is
#' every value the column prints, all colour levels, so it has one precision.
#' @noRd
summarize_val_digits <- function(x, d0 = NULL, d1 = NULL) {
  x <- x[is.finite(x)]
  if (length(x) && all(abs(x - round(x)) < 1e-9)) return(0L)
  if (!length(d0) || !length(d1) || !is.finite(d0) || !is.finite(d1)) {
    return(NA_integer_)
  }
  at <- summarize_axis_ticks(d0, d1)
  if (length(at) < 2L) return(NA_integer_)
  step <- at[[2L]] - at[[1L]]
  dig <- 0L
  while (dig < 6L && abs(round(step, dig) - step) > 1e-9 * max(1, step)) {
    dig <- dig + 1L
  }
  dig + 1L
}

#' A value label at `dig` decimals (summarize_val_digits()); `NA` digits fall
#' back to lane_fmt(). An NA value prints as "".
#' @noRd
summarize_val_str <- function(x, dig) {
  if (is.na(dig)) return(lane_fmt(x))
  out <- formatC(x, format = "f", digits = dig, big.mark = "")
  out[is.na(x)] <- ""
  out
}

#' The value slot of a colour-split column: one width for every level's
#' label, so the lanes of all levels and rows end at the same x.
#' @noRd
summarize_multi_dw <- function(lv) {
  max(c(1L, unlist(lapply(lv, function(g) nchar(g$disp %||% "")))))
}

#' The ink for a number printed ON a fill: white on a dark fill, the text
#' colour on a light one (the palette's yellow would swallow white).
#' @noRd
summarize_ink <- function(fills) {
  vapply(fills, function(f) {
    rgb <- tryCatch(grDevices::col2rgb(f)[, 1L] / 255,
                    error = function(e) c(0, 0, 0))
    lin <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
    lum <- sum(c(0.2126, 0.7152, 0.0722) * lin)
    if (lum > 0.4) "#1f2937" else "#ffffff"
  }, character(1L), USE.NAMES = FALSE)
}

# The calendar ladder: step, length in days and tick format. Steps are real
# calendar units because "every 30 days" drifts through the months.
SUMMARIZE_DATE_STEPS <- list(
  list(by = "1 day", days = 1, fmt = "%d %b", unit = "day"),
  list(by = "2 days", days = 2, fmt = "%d %b", unit = "day"),
  list(by = "1 week", days = 7, fmt = "%d %b", unit = "week"),
  list(by = "2 weeks", days = 14, fmt = "%d %b", unit = "week"),
  list(by = "1 month", days = 30.44, fmt = "%b %Y", unit = "month"),
  list(by = "3 months", days = 91.3, fmt = "%b %Y", unit = "month"),
  list(by = "6 months", days = 182.6, fmt = "%b %Y", unit = "month"),
  list(by = "1 year", days = 365.25, fmt = "%Y", unit = "year"),
  list(by = "2 years", days = 730.5, fmt = "%Y", unit = "year"),
  list(by = "5 years", days = 1826.2, fmt = "%Y", unit = "year"),
  list(by = "10 years", days = 3652.5, fmt = "%Y", unit = "year")
)

#' Nice tick dates across a day-numbered domain: three ticks, since a date
#' label is about three times as wide as a number.
#' @noRd
summarize_axis_date_ticks <- function(d0, d1, k = 3L) {
  raw <- (d1 - d0) / max(1L, k - 1L)
  if (!is.finite(raw) || raw <= 0) {
    return(list(at = numeric(), labels = character()))
  }
  st <- SUMMARIZE_DATE_STEPS[[which.min(abs(
    vapply(SUMMARIZE_DATE_STEPS, `[[`, numeric(1L), "days") - raw
  ))]]
  from <- summarize_axis_date_start(as.Date(d0, origin = "1970-01-01"), st)
  at <- seq(from, as.Date(ceiling(d1), origin = "1970-01-01"), by = st$by)
  at <- at[as.numeric(at) >= d0 & as.numeric(at) <= d1]
  list(at = as.numeric(at), labels = format(at, st$fmt))
}

#' The first tick: the earliest boundary of the step's unit at or after the
#' domain start (month firsts, January 1sts).
#' @noRd
summarize_axis_date_start <- function(d, st) {
  if (identical(st$unit, "day")) return(d)
  if (identical(st$unit, "week")) {
    # Monday, the ISO week boundary.
    return(d + (8L - as.integer(format(d, "%u"))) %% 7L)
  }
  yr <- as.integer(format(d, "%Y"))
  if (identical(st$unit, "month")) {
    first <- as.Date(sprintf("%04d-%02d-01", yr, as.integer(format(d, "%m"))))
    return(if (first >= d) {
      first
    } else {
      seq(first, by = "1 month", length.out = 2L)[[2L]]
    })
  }
  jan <- as.Date(sprintf("%04d-01-01", yr))
  if (jan >= d) jan else as.Date(sprintf("%04d-01-01", yr + 1L))
}
