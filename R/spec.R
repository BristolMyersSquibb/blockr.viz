# The block spec: every argument of a block, written once.
#
# A spec is a plain list built with viz_spec() and viz_arg(). The
# interpreters in spec-check.R, spec-state.R, wire.R and kit-input.R read it
# to check constructor arguments, heal restored state, hold reactive state,
# read the browser's settings message and write the payload; dev/gen-spec.R
# writes the JS spec and its TypeScript types from it.

# The value types an argument can have.
VIZ_TYPES <- c(
  "column", "columns", "enum", "enums", "string", "template", "number",
  "integer", "bool", "numbers", "strings", "values", "format", "record",
  "records", "map"
)

# Types whose value has fields, given by a type object rather than a string.
VIZ_COMPOUND_TYPES <- c("values", "record", "records")

# The gear controls an argument can be edited with.
VIZ_CONTROLS <- c(
  "column", "columns", "values", "select", "tiles", "segmented", "toggle",
  "number", "text", "template", "slider", "none"
)

# The keywords a `format` takes besides a number of decimals (0 to 6).
VIZ_FORMATS <- c("number", "compact", "percent")

#' A block spec
#'
#' @param block The block's name, `"chart"` or `"table"`.
#' @param ... Named `viz_arg()` entries, or unnamed lists of them (shared
#'   entries spliced into several specs).
#' @return A `viz_spec`: `block` and `args`, a named list of `viz_arg`.
#' @noRd
viz_spec <- function(block, ...) {
  stopifnot(is.character(block), length(block) == 1L, nzchar(block))
  args <- spec_flatten(list(...))
  nms <- names(args)
  if (is.null(nms) || any(!nzchar(nms))) {
    stop("Every argument of a spec must be named.", call. = FALSE)
  }
  if (anyDuplicated(nms)) {
    stop("Spec ", block, " names ", nms[duplicated(nms)][[1L]], " twice.",
         call. = FALSE)
  }
  for (nm in nms) {
    spec_arg_refs_ok(args[[nm]], nm, nms)
    # A literal default must pass its own type; an option default is read
    # and checked when the block is built.
    if (!inherits(args[[nm]]$default, "viz_opt")) {
      viz_check_value(args[[nm]], args[[nm]]$default, nm)
    }
    if (!is.null(args[[nm]]$example)) {
      viz_check_value(args[[nm]], args[[nm]]$example, nm)
    }
  }
  structure(list(block = block, args = args), class = "viz_spec")
}

# Named entries pass through; an unnamed list of entries is spliced in.
spec_flatten <- function(x) {
  nms <- names(x) %||% rep("", length(x))
  out <- list()
  for (i in seq_along(x)) {
    if (nzchar(nms[[i]])) {
      out <- c(out, stats::setNames(list(x[[i]]), nms[[i]]))
    } else if (is.list(x[[i]]) && !inherits(x[[i]], "viz_arg")) {
      out <- c(out, spec_flatten(x[[i]]))
    } else {
      stop("An unnamed spec entry must be a list of named arguments.",
           call. = FALSE)
    }
  }
  out
}

# An argument may name other arguments: `values(of = "x")` and
# `gear(when = "facet")`. Both must exist in the same spec.
spec_arg_refs_ok <- function(arg, name, all) {
  if (!inherits(arg, "viz_arg")) {
    stop("Spec entry `", name, "` is not a viz_arg().", call. = FALSE)
  }
  refs <- c(spec_type_refs(arg$type), arg$gear$when)
  bad <- setdiff(refs, all)
  if (length(bad)) {
    stop("Spec entry `", name, "` refers to unknown argument `", bad[[1L]],
         "`.", call. = FALSE)
  }
  invisible(TRUE)
}

# The arguments a type names. Inside a record, `values(of = <field>)` names a
# sibling field, which is not an argument.
spec_type_refs <- function(type) {
  if (identical(type$name, "values")) {
    return(type$of)
  }
  refs <- unlist(lapply(type$fields, spec_type_refs), use.names = FALSE)
  setdiff(refs, names(type$fields))
}

#' One argument of a block spec
#'
#' @param type One of [VIZ_TYPES] as a string, or a type object from
#'   `type_values()`, `type_record()` or `type_records()`.
#' @param default The default: a literal, or `opt()` for a value read from an
#'   option when the block is built.
#' @param values For `enum` and `enums`, the allowed values.
#' @param bounds For `number` and `integer`, a `bounds()` range.
#' @param null `FALSE` when `NULL` is not a valid value, else a short text
#'   saying what `NULL` means.
#' @param also For `column` and `columns`, keywords allowed besides a column
#'   name (`drill = "auto"`).
#' @param applies The types or layouts the argument means something for:
#'   `NULL` for all, a character vector, or `except()`.
#' @param gear A `gear()` entry, or `NULL` when the gear does not edit it.
#' @param slot Whether `{@name}` resolves in a title template.
#' @param ctrl Whether the argument is externally controllable.
#' @param ai Whether the block registry lists it for the assistant.
#' @param describe The registry text, and the source of its `@param` docs.
#' @param example A worked value for the registry, checked against the type.
#' @return A `viz_arg`.
#' @noRd
viz_arg <- function(type, default = NULL, values = NULL, bounds = NULL,
                    null = FALSE, also = NULL, applies = NULL, gear = NULL,
                    slot = FALSE, ctrl = TRUE, ai = TRUE, describe = NULL,
                    example = NULL) {
  type <- as_viz_type(type)
  arg <- structure(
    list(type = type, default = default, values = values, bounds = bounds,
         null = null, also = also, applies = applies, gear = gear,
         slot = slot, ctrl = ctrl, ai = ai, describe = describe,
         example = example),
    class = "viz_arg"
  )
  viz_arg_valid(arg)
  arg
}

# The field rules of viz_arg(), apart from the type itself.
viz_arg_valid <- function(arg) {
  t <- arg$type$name
  needs_values <- t %in% c("enum", "enums")
  if (needs_values != !is.null(arg$values)) {
    stop("`values` is for enum and enums arguments, and they need it.",
         call. = FALSE)
  }
  if (!is.null(arg$values) && (!is.character(arg$values) ||
                                 !length(arg$values))) {
    stop("`values` must be a non-empty character vector.", call. = FALSE)
  }
  if (!is.null(arg$bounds) && !t %in% c("number", "integer")) {
    stop("`bounds` is for number and integer arguments.", call. = FALSE)
  }
  if (!is.null(arg$also) && !t %in% c("column", "columns")) {
    stop("`also` is for column arguments.", call. = FALSE)
  }
  if (!isFALSE(arg$null) && !(is.character(arg$null) &&
                                length(arg$null) == 1L)) {
    stop("`null` is FALSE or a text saying what NULL means.", call. = FALSE)
  }
  if (!is.null(arg$gear) && !inherits(arg$gear, "viz_gear")) {
    stop("`gear` must come from gear().", call. = FALSE)
  }
  if (!is.null(arg$applies) && !is.character(arg$applies)) {
    stop("`applies` is NULL, a character vector or except().", call. = FALSE)
  }
  stopifnot(is_flag(arg$slot), is_flag(arg$ctrl), is_flag(arg$ai))
  if (arg$slot && t %in% c("record", "records", "map")) {
    stop("A ", t, " argument cannot be a title slot.", call. = FALSE)
  }
  invisible(TRUE)
}

is_flag <- function(x) is.logical(x) && length(x) == 1L && !is.na(x)

# A type given as a string becomes a type object, so every interpreter reads
# `type$name` and, for compound types, its fields.
as_viz_type <- function(x) {
  if (inherits(x, "viz_type")) {
    return(x)
  }
  if (!is.character(x) || length(x) != 1L || !x %in% VIZ_TYPES) {
    stop("Unknown argument type ", deparse(x), ". Use one of ",
         paste(VIZ_TYPES, collapse = ", "), ".", call. = FALSE)
  }
  if (x %in% VIZ_COMPOUND_TYPES) {
    stop("Type ", x, " needs its fields: use type_", x, "().", call. = FALSE)
  }
  structure(list(name = x), class = "viz_type")
}

#' Compound types
#'
#' `type_values(of)`: values of the column another argument names (reference
#' lines on x, highlighted values). `type_record(...)`: one record with the
#' given fields, or `NULL`. `type_records(...)`: a list of such records.
#' Fields are given as types, by name: `type_records(name = "string",
#' from = "string", to = "string")`.
#' @noRd
type_values <- function(of) {
  stopifnot(is.character(of), length(of) == 1L, nzchar(of))
  structure(list(name = "values", of = of), class = "viz_type")
}

#' @rdname type_values
#' @noRd
type_record <- function(...) {
  structure(list(name = "record", fields = type_fields(list(...))),
            class = "viz_type")
}

#' @rdname type_values
#' @noRd
type_records <- function(...) {
  structure(list(name = "records", fields = type_fields(list(...))),
            class = "viz_type")
}

type_fields <- function(fields) {
  nms <- names(fields)
  if (!length(fields) || is.null(nms) || any(!nzchar(nms))) {
    stop("A record type needs named fields.", call. = FALSE)
  }
  lapply(fields, as_viz_type)
}

#' Where an argument sits in the gear
#'
#' @param group The tray section ("Mapping", "Look", ...).
#' @param control One of [VIZ_CONTROLS].
#' @param label The row's label.
#' @param labels For `select`, `segmented` and `tiles`, the option labels, in
#'   the order of the argument's `values`.
#' @param parse For `text`, the rule that reads the typed text ("lines",
#'   "ranges").
#' @param when Another argument that must be set for the row to show.
#' @noRd
gear <- function(group, control, label, labels = NULL, parse = NULL,
                 when = NULL) {
  stopifnot(is.character(group), length(group) == 1L,
            is.character(label), length(label) == 1L)
  if (!is.character(control) || length(control) != 1L ||
        !control %in% VIZ_CONTROLS) {
    stop("Unknown gear control ", deparse(control), ". Use one of ",
         paste(VIZ_CONTROLS, collapse = ", "), ".", call. = FALSE)
  }
  structure(
    list(group = group, control = control, label = label, labels = labels,
         parse = parse, when = when),
    class = "viz_gear"
  )
}

#' A default read from an option when the block is built
#' @param name The option name.
#' @param fallback The value when the option is unset.
#' @noRd
opt <- function(name, fallback = NULL) {
  stopifnot(is.character(name), length(name) == 1L)
  structure(list(name = name, fallback = fallback), class = "viz_opt")
}

#' Every type or layout except these
#' @noRd
except <- function(...) {
  structure(c(...), class = c("viz_except", "character"))
}

#' A numeric range, inclusive unless `min_open`
#' @noRd
bounds <- function(min = -Inf, max = Inf, min_open = FALSE) {
  stopifnot(is.numeric(min), is.numeric(max), min <= max)
  structure(list(min = min, max = max, min_open = min_open),
            class = "viz_bounds")
}

# The default of an argument, with an option default resolved.
spec_default <- function(arg) {
  d <- arg$default
  if (inherits(d, "viz_opt")) getOption(d$name, d$fallback) else d
}

# Whether an argument means something for a type or layout.
spec_applies <- function(arg, type) {
  a <- arg$applies
  if (is.null(a)) {
    return(TRUE)
  }
  if (inherits(a, "viz_except")) !type %in% a else type %in% a
}
