# Checkers for the axis arguments: limits and ticks (SPEC_CHECKERS).

# Axis limits: a from and a to, either missing (NA) for auto. Numbers, or
# dates as ISO text. Both missing is NULL; from above to is an error.
check_limits <- function(x, arg) {
  if (inherits(x, c("Date", "POSIXt"))) {
    x <- ifelse(is.na(x), NA_character_, format(x))
  }
  x <- json_atomic(x)
  if (is.null(x) || all(is.na(x))) {
    return(NULL)
  }
  if (!is.atomic(x) || length(x) != 2L ||
        !(is.numeric(x) || is.character(x) || is.logical(x))) {
    value_fail("must be two values, from and to (NA for auto), not ",
               value_text(x))
  }
  x <- if (is.numeric(x)) as.numeric(x) else as.character(x)
  if (is.numeric(x) && any(is.infinite(x))) {
    value_fail("must be finite or NA, not ", value_text(x))
  }
  if (is.character(x)) {
    d <- suppressWarnings(as.numeric(x))
    if (all(is.na(x) | !is.na(d))) x <- d
  }
  if (!anyNA(x) && x[[1L]] >= x[[2L]]) {
    value_fail("must have from below to, not ", x[[1L]], " to ", x[[2L]])
  }
  unname(x)
}

# Axis ticks: one value is a step (a number above 0, or a count and a date
# unit, "7 days"), two or more are the ticks themselves, numbers or dates as
# ISO text. A step comes back as a number or as "<n> <unit>s"; a list sorted.
check_ticks <- function(x, arg) {
  if (inherits(x, c("Date", "POSIXt"))) {
    x <- format(x)
  }
  x <- json_atomic(x)
  if (is.null(x) || !length(x) || all(is.na(x))) {
    return(NULL)
  }
  if (!is.atomic(x) || anyNA(x) || !(is.numeric(x) || is.character(x))) {
    value_fail("must be a step (10, \"7 days\") or two values or more, not ",
               value_text(x))
  }
  if (is.character(x)) {
    x <- trimws(sub("^(every|at)\\s+", "", trimws(x)))
    d <- suppressWarnings(as.numeric(x))
    if (!anyNA(d)) x <- d
  }
  if (is.integer(x)) x <- as.numeric(x)
  if (length(x) == 1L) {
    return(ticks_step(x))
  }
  if (is.numeric(x) && any(!is.finite(x))) {
    value_fail("must be finite, not ", value_text(x))
  }
  if (is.character(x) && anyNA(as.Date(x, optional = TRUE))) {
    value_fail("must be numbers or dates as \"2024-01-01\", not ",
               paste0("\"", x, "\"", collapse = ", "))
  }
  x <- sort(unique(x))
  if (length(x) < 2L) {
    value_fail("as a list must have two values or more")
  }
  unname(x)
}

TICK_UNITS <- c("day", "week", "month", "quarter", "year")

ticks_step <- function(x) {
  if (is.numeric(x)) {
    if (!is.finite(x) || x <= 0) {
      value_fail("as a step must be above 0, not ", format(x))
    }
    return(unname(x))
  }
  x <- tolower(x)
  m <- regmatches(x, regexec(
    paste0("^([0-9]*\\.?[0-9]+)?\\s*(", paste(TICK_UNITS, collapse = "|"),
           ")s?$"), x
  ))[[1L]]
  if (!length(m)) {
    value_fail("must be a step such as 10, \"7 days\" or \"month\", not \"",
               x, "\"")
  }
  n <- if (nzchar(m[[2L]])) as.numeric(m[[2L]]) else 1
  if (n <= 0 || n != round(n)) {
    value_fail("must count whole ", m[[3L]], "s, not \"", x, "\"")
  }
  paste0(n, " ", m[[3L]], if (n != 1) "s")
}
