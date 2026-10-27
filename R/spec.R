# The block spec: every argument of a block, written once.
#
# A spec is a plain list built with viz_spec() and viz_arg(). The builders
# only assemble lists; viz_spec_check() (spec-validate.R) checks a spec, and
# viz_block_spec() builds and checks a block's spec the first time it is
# asked for. A block's spec file therefore defines a function,
# `chart_spec <- function() viz_spec("chart", ...)`, and calls no viz_*()
# builder at top level: package load does no spec work and does not depend
# on the order R sources the files in.
#
# The interpreters in spec-check.R, spec-state.R, spec-registry.R, wire.R
# and kit-input.R read a spec; dev/gen-spec.R writes the JS spec and its
# TypeScript types from it.

# The value types an argument can have.
VIZ_TYPES <- c(
  "column", "columns", "enum", "enums", "string", "template", "number",
  "integer", "bool", "numbers", "strings", "values", "format", "record",
  "records", "records_by", "named", "map"
)

# Types built with a type_*() function because they have parts.
VIZ_COMPOUND_TYPES <- c("values", "record", "records", "records_by", "named")

# The gear controls an argument can be edited with.
VIZ_CONTROLS <- c(
  "column", "columns", "values", "select", "tiles", "segmented", "toggle",
  "number", "text", "template", "slider", "none"
)

# The keywords a `format` takes besides a number of decimals (0 to 6).
VIZ_FORMATS <- c("number", "compact", "percent")

# The block specs by block name. Each block's spec file defines the
# function named here.
VIZ_BLOCK_SPECS <- list(
  chart = function() chart_spec(),
  table = function() table_spec()
)

#' A block spec
#'
#' @param .block The block's name, `"chart"` or `"table"`, or the name of a
#'   nested spec (a summary type). Dotted so that no argument name matches it
#'   partially.
#' @param ... Named `viz_arg()` entries, or unnamed lists of them (shared
#'   entries spliced into several specs).
#' @return A `viz_spec`: `block` and `args`, a named list of `viz_arg`.
#' @noRd
viz_spec <- function(.block, ...) {
  structure(list(block = .block, args = spec_flatten(list(...))),
            class = "viz_spec")
}

# Named entries pass through; an unnamed list is spliced in. Anything else
# keeps an empty name, which viz_spec_check() reports.
spec_flatten <- function(x) {
  nms <- names(x) %||% rep("", length(x))
  out <- list()
  for (i in seq_along(x)) {
    splice <- !nzchar(nms[[i]]) && is.list(x[[i]]) &&
      !inherits(x[[i]], "viz_arg")
    out <- c(out, if (splice) {
      spec_flatten(x[[i]])
    } else {
      stats::setNames(list(x[[i]]), nms[[i]])
    })
  }
  out
}

#' One argument of a block spec
#'
#' @param type One of [VIZ_TYPES] as a string, or a type from
#'   `type_values()`, `type_record()`, `type_records()` or
#'   `type_records_by()`.
#' @param default The default: a literal, or `opt()` for a value read from an
#'   option when the block is built.
#' @param values For `enum` and `enums`, the allowed values.
#' @param bounds For `number` and `integer`, a `bounds()` range.
#' @param null `FALSE` when `NULL` is not a valid value, else a short text
#'   saying what `NULL` means.
#' @param required `TRUE` when the value has no default and must be given
#'   (a dist summary's column).
#' @param also For `column` and `columns`, keywords allowed besides a column
#'   name (`drill = "auto"`).
#' @param applies The types or layouts the argument means something for:
#'   `NULL` for all, a character vector, or `except()`.
#' @param gear A `gear()` entry, or `NULL` when the gear does not edit it.
#' @param slot Whether `{@name}` resolves in a title template.
#' @param ctrl Whether the argument is externally controllable.
#' @param ai Whether the block registry lists it for the assistant.
#' @param describe The registry text, and the source of its `@param` docs.
#' @param example A worked value for the registry.
#' @return A `viz_arg`.
#' @noRd
viz_arg <- function(type, default = NULL, values = NULL, bounds = NULL,
                    null = FALSE, required = FALSE, also = NULL,
                    applies = NULL, gear = NULL, slot = FALSE, ctrl = TRUE,
                    ai = TRUE, describe = NULL, example = NULL) {
  structure(
    list(type = spec_type(type), default = default, values = values,
         bounds = bounds, null = null, required = required, also = also,
         applies = applies, gear = gear, slot = slot, ctrl = ctrl, ai = ai,
         describe = describe, example = example),
    class = "viz_arg"
  )
}

# A type given as a string becomes a type object, so every interpreter reads
# `type$name`.
spec_type <- function(x) {
  if (is.character(x)) structure(list(name = x), class = "viz_type") else x
}

#' Compound types
#'
#' - `type_values(of)`: values of the column another argument names
#'   (reference lines on x). With `of = NULL`, of a column named elsewhere,
#'   such as the entry name of a `type_named()` list.
#' - `type_record(...)`: one record, or `NULL`. The fields are given by name
#'   as types (`column = "column"`), as `viz_arg()` entries when a field
#'   needs values, a default or `required`, or all at once as a spec
#'   (`spec = table_shading_spec()`).
#' - `type_records(...)`: a list of such records.
#' - `type_records_by(key, specs)`: a list of records whose fields depend on
#'   the value of `key`; `specs` is a named list of specs, one per value
#'   (the table's summaries, by `type`).
#' - `type_named(value, max)`: a list whose names are free text, a column or
#'   a label, and whose values have the type `value` (a type, or a
#'   `viz_arg()`): `highlight = list(country = "Switzerland")`,
#'   `x_ranges = list(Recession = c(from, to))`. `max` caps the entries.
#' @noRd
type_values <- function(of = NULL) {
  structure(list(name = "values", of = of), class = "viz_type")
}

#' @rdname type_values
#' @noRd
type_record <- function(..., spec = NULL) {
  structure(list(name = "record", fields = type_fields(list(...), spec)),
            class = "viz_type")
}

#' @rdname type_values
#' @noRd
type_records <- function(..., spec = NULL) {
  structure(list(name = "records", fields = type_fields(list(...), spec)),
            class = "viz_type")
}

#' @rdname type_values
#' @noRd
type_records_by <- function(key, specs) {
  structure(
    list(name = "records_by", key = key,
         variants = lapply(specs, function(s) type_record(spec = s))),
    class = "viz_type"
  )
}

#' @rdname type_values
#' @noRd
type_named <- function(value, max = NULL) {
  if (!inherits(value, "viz_arg")) {
    value <- viz_arg(value, required = TRUE)
  }
  structure(list(name = "named", value = value, max = max),
            class = "viz_type")
}

# Record fields as a named list of viz_arg. A field given as a bare type is
# optional: left out, it is NULL.
type_fields <- function(fields, spec) {
  if (!is.null(spec)) {
    return(spec$args)
  }
  lapply(fields, function(f) {
    if (inherits(f, "viz_arg")) f else viz_arg(f, null = "unset")
  })
}

#' Where an argument sits in the gear
#'
#' @param group The tray section ("Mapping", "Look", ...).
#' @param control One of [VIZ_CONTROLS].
#' @param label The row's label.
#' @param labels Option labels: for an enum, in the order of its `values`;
#'   for a bool, the labels of `TRUE` and `FALSE`.
#' @param parse For `text`, the rule that reads the typed text ("lines",
#'   "ranges").
#' @param when Another argument that must be set for the row to show.
#' @noRd
gear <- function(group, control, label, labels = NULL, parse = NULL,
                 when = NULL) {
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
  structure(list(min = min, max = max, min_open = min_open),
            class = "viz_bounds")
}

#' A block's spec, built and checked once
#'
#' @param block A name in `VIZ_BLOCK_SPECS`.
#' @return The block's `viz_spec`.
#' @noRd
viz_block_spec <- function(block) {
  if (is.null(spec_cache[[block]])) {
    build <- VIZ_BLOCK_SPECS[[block]]
    if (is.null(build)) {
      stop("No spec for block `", block, "`.", call. = FALSE)
    }
    spec_cache[[block]] <- viz_spec_check(build())
  }
  spec_cache[[block]]
}

spec_cache <- new.env(parent = emptyenv())

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
