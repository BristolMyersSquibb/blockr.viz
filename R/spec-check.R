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
  # An empty list from JSON means unset, except where an empty list is a value.
  if (is.list(x) && !length(x) &&
        !arg$type$name %in% c("map", "records", "records_by", "named")) {
    x <- NULL
  }
  if (is.null(x)) {
    if (isTRUE(arg$required)) {
      spec_fail(name, "is required")
    }
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

# Check `x` and return it in a one-element list, or NULL when it fails. For
# callers that reject a bad value and carry on.
try_check_value <- function(arg, x, name) {
  tryCatch(
    list(viz_check_value(arg, x, name)),
    viz_arg_error = function(e) NULL
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
  if (!all(vapply(x, is_scalar_or_null, logical(1L)))) {
    value_fail("must be a vector of single values")
  }
  x[vapply(x, is.null, logical(1L))] <- list(NA)
  unlist(x, use.names = FALSE)
}

is_scalar_or_null <- function(x) {
  is.null(x) || (is.atomic(x) && length(x) == 1L)
}

# The list-valued types give NULL for an empty value unless `null = FALSE`.
empty_or_null <- function(x, arg) {
  if (!length(x) && !isFALSE(arg$null)) NULL else x
}

# `x` named by `nms` when any name is not empty.
with_labels <- function(x, nms) {
  if (!is.null(nms) && any(nzchar(nms))) names(x) <- nms
  x
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

check_string <- function(x, arg) one_string(x)

check_enum <- function(x, arg) one_of(one_string(x), arg$values)

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
  empty_or_null(x[nzchar(x)], arg)
}

check_enums <- function(x, arg) {
  x <- some_strings(x)
  for (v in x) {
    one_of(v, arg$values)
  }
  empty_or_null(x, arg)
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
  empty_or_null(with_labels(as.numeric(x), nms), arg)
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
  empty_or_null(with_labels(out, nms), arg)
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

# One record: a named list whose fields are checked as arguments. A field
# left out takes its default, NULL for most.
check_record <- function(x, arg) {
  check_fields(x, arg$type$fields)
}

check_fields <- function(x, fields) {
  if (!is.list(x) || (length(x) && is.null(names(x)))) {
    value_fail("must be a named list with fields ",
               paste(names(fields), collapse = ", "))
  }
  extra <- setdiff(names(x), names(fields))
  if (length(extra)) {
    value_fail("has no field `", extra[[1L]], "`")
  }
  check_entries(x, names(fields), function(f) fields[[f]], "has field ")
}

# Each entry of `x` named in `nms`, checked as the argument `arg_of(name)`.
# A failure names the entry: "has field `a` ...".
check_entries <- function(x, nms, arg_of, what) {
  out <- list()
  for (nm in nms) {
    v <- tryCatch(
      viz_check_value(arg_of(nm), x[[nm]], nm),
      viz_arg_error = function(e) value_fail(what, inner_message(e))
    )
    out[nm] <- list(v)
  }
  out
}

# The message of a nested argument error, without its closing period.
inner_message <- function(e) sub("\\.$", "", conditionMessage(e))

check_records <- function(x, arg) {
  check_record_list(x, arg, function(r) check_record(r, arg))
}

# Records whose fields depend on their `key` field: each is checked against
# the variant its key names, with the key kept as its first field.
check_records_by <- function(x, arg) {
  key <- arg$type$key
  variants <- arg$type$variants
  check_record_list(x, arg, function(r) {
    k <- if (is.list(r)) json_atomic(r[[key]])
    if (!is.character(k) || length(k) != 1L || !k %in% names(variants)) {
      value_fail("needs `", key, "` in each record, one of ",
                 paste0("\"", names(variants), "\"", collapse = ", "))
    }
    fields <- variants[[k]]$fields
    rest <- check_fields(r[setdiff(names(r), key)],
                         fields[setdiff(names(fields), key)])
    c(stats::setNames(list(k), key), rest)
  })
}

check_record_list <- function(x, arg, check_one) {
  if (!is.list(x) || (!is.null(names(x)) && length(x))) {
    value_fail("must be a list of records")
  }
  empty_or_null(lapply(x, check_one), arg)
}

# A list keyed by free names, each value checked against the value type.
check_named <- function(x, arg) {
  nms <- names(x)
  bad_names <- length(x) && (is.null(nms) || any(!nzchar(nms)) ||
                               anyDuplicated(nms))
  if ((!is.list(x) && !is.atomic(x)) || bad_names) {
    value_fail("must be a list with a unique name for each entry")
  }
  max <- arg$type$max
  if (!is.null(max) && length(x) > max) {
    value_fail("takes at most ", max, " entries, not ", length(x))
  }
  out <- check_entries(x, nms, function(nm) arg$type$value, "has entry ")
  empty_or_null(out, arg)
}

check_map <- function(x, arg) {
  if (!is.list(x) || (length(x) && (is.null(names(x)) ||
                                      any(!nzchar(names(x)))))) {
    value_fail("must be a named list")
  }
  if (length(x)) map_canon(x) else list()
}

# A map's values in the one form R and a JSON round trip agree on: a JSON
# array of single values is an atomic vector again, numbers are doubles
# (JSON does not say integer), a vector of only NA is logical (JSON does
# not say its type), an empty vector is NULL, and a factor is text. Named
# lists keep their names and are read the same way inside.
map_canon <- function(x) {
  if (is.list(x)) {
    if (is.null(names(x)) && length(x) &&
          all(vapply(x, is_scalar_or_null, logical(1L)))) {
      return(map_canon(json_atomic(x)))
    }
    return(lapply(x, map_canon))
  }
  if (!is.atomic(x) || !length(x)) {
    return(if (length(x)) x else NULL)
  }
  if (is.factor(x)) x <- as.character(x)
  if (all(is.na(x))) {
    return(rep(NA, length(x)))
  }
  if (is.integer(x)) as.numeric(x) else unname(as.vector(x))
}

SPEC_CHECKERS <- list(
  column = check_column,
  columns = check_columns,
  enum = check_enum,
  enums = check_enums,
  string = check_string,
  template = check_string,
  number = check_number,
  integer = check_integer,
  bool = check_bool,
  numbers = check_numbers,
  strings = check_columns,
  values = check_values,
  format = check_format,
  record = check_record,
  records = check_records,
  records_by = check_records_by,
  named = check_named,
  map = check_map
)

# Block arguments a constructor passes on through `...` to
# blockr.core::new_block(): core's own, and blockr.dock's `visible` (which
# panes a block shows), which boards set in code. Any other name in `...` is
# an old argument name, or an error from viz_check(). A restore also keeps
# other saved names a constructor does not take, as attributes
# (legacy_build()).
VIZ_CORE_BLOCK_ARGS <- c("block_name", "block_metadata", "ctor", "ctor_pkg",
                         "visible")

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
    v <- try_check_value(arg, payload[[nm]], nm)
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
  quoted <- function(x) paste0("`", x, "`", collapse = ", ")
  paste0(
    "Restoring ", what, ": ",
    paste(c(
      if (length(healed)) paste0("reset ", quoted(healed), " to the default"),
      if (length(dropped)) paste0("dropped ", quoted(dropped))
    ), collapse = "; "),
    "."
  )
}

# Heal corrupted block state written by the DAG clipboard.
#
# Older blockr.dag versions serialize live block state without
# `null = "null"`, so every NULL state field is written as `{}` and comes back
# from a paste as an empty named list(). blockr.core hands that to the
# constructor verbatim, and a saved board keeps the list() and re-emits `{}` on
# the next copy. The heal therefore lives in the constructor, which paste,
# restore and AI-set config all go through.
#
# Symptoms: "[object Object]" in a gear mapping row that should be hidden;
# "`needed` must be logical, numeric, or character, not a list"; "argument is
# of length zero" from nzchar() on a list(). All normalizers are idempotent.

# Any slot: an empty list() (the corrupted NULL) becomes NULL, everything else
# passes through with its type intact. For non-character slots (numeric
# ranges, points, sizes).
null_state <- function(x) {
  if (is.list(x) && !length(x)) NULL else x
}

# A scalar column-role slot: NULL or a length-1 character vector.
chr_state <- function(x) {
  if (is.null(x)) return(NULL)
  x <- as.character(unlist(x, use.names = FALSE))
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) NULL else x[[1L]]
}

# A multi-column role slot: NULL or a character vector.
chr_vec_state <- function(x) {
  if (is.null(x)) return(NULL)
  x <- as.character(unlist(x, use.names = FALSE))
  x <- x[!is.na(x)]
  if (!length(x)) NULL else x
}

# A boolean toggle slot: always a length-1 logical. The JS gear transports a
# segmented control as "on" / "off" and older chart boards carry "off" for
# `identity_line`, so accept a logical, "on" / "off", "true" / "false" (a JSON
# round-trip) and 1 / 0.
bool_state <- function(x, default = FALSE) {
  if (is.null(x)) return(default)

  x <- unlist(x, use.names = FALSE)
  x <- x[!is.na(x)]
  if (!length(x)) return(default)

  x <- x[[1L]]
  if (is.logical(x)) return(isTRUE(x))

  if (is.character(x)) {
    v <- tolower(trimws(x))
    if (identical(v, "on")) return(TRUE)
    if (identical(v, "off")) return(FALSE)
    return(isTRUE(as.logical(v)))
  }

  isTRUE(as.logical(x))
}
