# Checking argument values against the spec.
#
# viz_check_value() takes a value as R code writes it (a character vector) or
# as JSON arrives (a list of scalars, NULL for a JSON null) and returns it in
# the one form the block holds, or fails with a `viz_arg_error`. viz_check()
# does that for a constructor call, viz_heal() for a restored payload.

#' Check one argument value
#'
#' @param arg A `viz_arg`.
#' @param x The value.
#' @param name The argument's name, for the error message.
#' @return The value in the block's form. `NULL` gives the default unless the
#'   argument allows `NULL`.
#' @noRd
viz_check_value <- function(arg, x, name) {
  if (is.list(x) && !length(x) && !arg$type$name %in% c("map", "records")) {
    x <- NULL
  }
  if (is.null(x)) {
    if (!isFALSE(arg$null)) {
      return(NULL)
    }
    x <- spec_default(arg)
    if (is.null(x)) {
      spec_fail(name, "cannot be NULL")
    }
  }
  check <- SPEC_CHECKERS[[arg$type$name]]
  tryCatch(
    check(x, arg),
    viz_value_error = function(e) spec_fail(name, conditionMessage(e))
  )
}

spec_fail <- function(name, what) {
  stop(structure(
    class = c("viz_arg_error", "error", "condition"),
    list(message = paste0("`", name, "` ", what, "."), call = NULL,
         arg = name)
  ))
}

# A checker reports a bad value with value_fail(); viz_check_value() adds the
# argument's name.
value_fail <- function(...) {
  stop(structure(
    class = c("viz_value_error", "error", "condition"),
    list(message = paste0(...), call = NULL)
  ))
}

# A JSON array arrives as a list of scalars, a JSON null inside it as NULL.
# Both become an atomic vector, NULL as NA.
json_atomic <- function(x) {
  if (!is.list(x)) {
    return(x)
  }
  ok <- vapply(x, function(e) is.null(e) || (is.atomic(e) && length(e) == 1L),
               logical(1L))
  if (!all(ok)) {
    value_fail("must be a vector of single values")
  }
  x[vapply(x, is.null, logical(1L))] <- list(NA)
  unlist(x, use.names = FALSE)
}

one_string <- function(x) {
  x <- json_atomic(x)
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    value_fail("must be a single string, not ", value_text(x))
  }
  x
}

some_strings <- function(x) {
  x <- json_atomic(x)
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (!is.character(x) || anyNA(x)) {
    value_fail("must be a character vector without NA, not ", value_text(x))
  }
  unname(x)
}

value_text <- function(x) {
  if (is.null(x)) {
    return("NULL")
  }
  if (is.atomic(x) && length(x) == 1L) {
    return(if (is.character(x)) paste0("\"", x, "\"") else format(x))
  }
  paste0("a ", class(x)[[1L]], " of length ", length(x))
}

one_of <- function(x, values) {
  if (!x %in% values) {
    value_fail("must be one of ", paste0("\"", values, "\"", collapse = ", "),
               ", not \"", x, "\"")
  }
  x
}

check_column <- function(x, arg) {
  x <- one_string(x)
  if (!nzchar(x)) {
    if (isFALSE(arg$null)) value_fail("must name a column")
    return(NULL)
  }
  x
}

check_columns <- function(x, arg) {
  x <- some_strings(x)
  x <- x[nzchar(x)]
  if (!length(x) && !isFALSE(arg$null)) NULL else x
}

check_enums <- function(x, arg) {
  x <- some_strings(x)
  for (v in x) {
    one_of(v, arg$values)
  }
  if (!length(x) && !isFALSE(arg$null)) NULL else x
}

check_number <- function(x, arg) {
  x <- json_atomic(x)
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
    value_fail("must be a single finite number, not ", value_text(x))
  }
  in_bounds(as.numeric(x), arg$bounds)
}

check_integer <- function(x, arg) {
  x <- check_number(x, arg)
  if (x != round(x)) {
    value_fail("must be a whole number, not ", x)
  }
  as.integer(x)
}

# `x` inside the range `b` from bounds(), or a value error.
in_bounds <- function(x, b) {
  if (is.null(b)) {
    return(x)
  }
  low <- if (isTRUE(b$min_open)) x <= b$min else x < b$min
  if (low || x > b$max) {
    value_fail("must be ", bounds_text(b), ", not ", x)
  }
  x
}

bounds_text <- function(b) {
  lo <- if (is.finite(b$min)) {
    paste(if (isTRUE(b$min_open)) "above" else "at least", b$min)
  }
  hi <- if (is.finite(b$max)) paste("at most", b$max)
  paste(c(lo, hi), collapse = " and ")
}

check_bool <- function(x, arg) {
  x <- json_atomic(x)
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    value_fail("must be TRUE or FALSE, not ", value_text(x))
  }
  x
}

# Values whose names are labels arrive from JSON as `[{value, label}]`;
# they become a vector named by the labels.
unlabel <- function(x) {
  if (!is.list(x) || !length(x) || !all(vapply(x, is.list, logical(1L)))) {
    return(x)
  }
  labels <- vapply(x, function(e) e$label %||% "", character(1L))
  stats::setNames(json_atomic(lapply(x, `[[`, "value")), labels)
}

# Numbers whose names are labels.
check_numbers <- function(x, arg) {
  x <- unlabel(x)
  nms <- names(x)
  x <- json_atomic(x)
  if (!is.numeric(x) || !all(is.finite(x))) {
    value_fail("must be finite numbers, not ", value_text(x))
  }
  x <- as.numeric(x)
  if (!is.null(nms) && any(nzchar(nms))) names(x) <- nms
  if (!length(x) && !isFALSE(arg$null)) NULL else x
}

# Values of a column: numbers when every value is a number, else text, dates
# as ISO text. Names are labels, as on `numbers`.
check_values <- function(x, arg) {
  x <- unlabel(x)
  nms <- names(x)
  if (inherits(x, c("Date", "POSIXt"))) {
    x <- format(x)
  }
  x <- json_atomic(x)
  if (!is.atomic(x) || anyNA(x)) {
    value_fail("must be numbers or strings without NA, not ", value_text(x))
  }
  out <- if (is.numeric(x)) as.numeric(x) else as.character(x)
  if (!is.null(nms) && any(nzchar(nms))) names(out) <- nms
  if (!length(out) && !isFALSE(arg$null)) NULL else out
}

check_format <- function(x, arg) {
  x <- json_atomic(x)
  if (length(x) == 1L && is.character(x) && x %in% VIZ_FORMATS) {
    return(x)
  }
  d <- suppressWarnings(as.numeric(x))
  if (length(d) != 1L || is.na(d) || d != round(d) || d < 0 || d > 6) {
    value_fail("must be one of \"number\", \"compact\", \"percent\" or a ",
               "whole number of decimals from 0 to 6, not ", value_text(x))
  }
  as.integer(d)
}

# One record: a named list whose fields are checked by their types. A field
# may be left out, which leaves it NULL.
check_record <- function(x, arg) {
  fields <- arg$type$fields
  if (!is.list(x) || (length(x) && is.null(names(x)))) {
    value_fail("must be a named list with fields ",
               paste(names(fields), collapse = ", "))
  }
  extra <- setdiff(names(x), names(fields))
  if (length(extra)) {
    value_fail("has no field `", extra[[1L]], "`")
  }
  out <- list()
  for (f in names(fields)) {
    field_arg <- list(type = fields[[f]], null = "unset", bounds = NULL,
                      also = NULL, values = NULL)
    v <- if (is.null(x[[f]])) NULL else SPEC_CHECKERS[[fields[[f]]$name]](
      x[[f]], field_arg
    )
    out[f] <- list(v)
  }
  out
}

check_records <- function(x, arg) {
  if (!is.list(x) || (!is.null(names(x)) && length(x))) {
    value_fail("must be a list of records")
  }
  out <- lapply(x, check_record, arg = arg)
  if (!length(out) && !isFALSE(arg$null)) NULL else out
}

check_map <- function(x, arg) {
  if (!is.list(x) || (length(x) && (is.null(names(x)) ||
                                      any(!nzchar(names(x)))))) {
    value_fail("must be a named list")
  }
  if (length(x)) x else list()
}

SPEC_CHECKERS <- list(
  column = check_column,
  columns = check_columns,
  enum = function(x, arg) one_of(one_string(x), arg$values),
  enums = check_enums,
  string = function(x, arg) one_string(x),
  template = function(x, arg) one_string(x),
  number = check_number,
  integer = check_integer,
  bool = check_bool,
  numbers = check_numbers,
  strings = function(x, arg) check_columns(x, arg),
  values = check_values,
  format = check_format,
  record = check_record,
  records = check_records,
  map = check_map
)

#' Check a constructor call against the spec
#'
#' @param spec A `viz_spec`.
#' @param args The arguments the caller gave, a named list. Names not in the
#'   spec are an error that suggests the nearest one.
#' @return A named list with every spec argument: the checked value, or the
#'   default.
#' @noRd
viz_check <- function(spec, args) {
  bad <- setdiff(names(args), names(spec$args))
  if (length(bad)) {
    near <- spec_nearest(bad[[1L]], names(spec$args))
    stop("unknown argument `", bad[[1L]], "`.",
         if (!is.null(near)) paste0(" Did you mean `", near, "`?"),
         call. = FALSE)
  }
  out <- lapply(names(spec$args), function(nm) {
    arg <- spec$args[[nm]]
    x <- if (nm %in% names(args)) args[[nm]] else spec_default(arg)
    viz_check_value(arg, x, nm)
  })
  stats::setNames(out, names(spec$args))
}

spec_nearest <- function(x, choices) {
  d <- utils::adist(x, choices)[1L, ]
  if (min(d) <= 2) choices[[which.min(d)]] else NULL
}

#' Heal a restored payload against the spec
#'
#' A board restores what it saved, so a value the constructor would refuse
#' (a bad enum, an empty list from an old clipboard paste) becomes the
#' default, and names the spec does not know are dropped. One warning lists
#' what changed.
#'
#' @param spec A `viz_spec`.
#' @param payload The saved values, a named list.
#' @param what How the warning names the block ("a chart").
#' @return A named list with every spec argument.
#' @noRd
viz_heal <- function(spec, payload, what = paste("a", spec$block)) {
  dropped <- setdiff(names(payload), names(spec$args))
  healed <- character()
  out <- list()
  for (nm in names(spec$args)) {
    arg <- spec$args[[nm]]
    v <- tryCatch(
      list(viz_check_value(arg, payload[[nm]], nm)),
      viz_arg_error = function(e) NULL
    )
    if (is.null(v)) {
      healed <- c(healed, nm)
      v <- list(viz_check_value(arg, spec_default(arg), nm))
    }
    out[nm] <- v
  }
  if (length(dropped) || length(healed)) {
    warning(heal_text(what, healed, dropped), call. = FALSE)
  }
  out
}

heal_text <- function(what, healed, dropped) {
  paste0(
    "Restoring ", what, ": ",
    paste(c(
      if (length(healed)) paste0("reset ", paste0("`", healed, "`",
                                                  collapse = ", "),
                                 " to the default"),
      if (length(dropped)) paste0("dropped ", paste0("`", dropped, "`",
                                                     collapse = ", "))
    ), collapse = "; "),
    "."
  )
}
