# Checking a spec itself: the rules viz_spec() and viz_arg() do not check
# while they assemble it. viz_block_spec() runs this once per block.

#' Check a spec
#'
#' Every argument is named once; types, controls and fields are known; enum
#' values, bounds, `also`, `null` and `required` are where they belong; gear
#' labels match the values; a slot holds a single value; names that
#' arguments refer to exist; every literal default and example passes its
#' own type.
#'
#' @param spec A `viz_spec`.
#' @return `spec`, invisibly. Fails with every problem listed.
#' @noRd
viz_spec_check <- function(spec) {
  nms <- names(spec$args)
  problems <- character()
  if (is.null(nms) || any(!nzchar(nms))) {
    problems <- "every argument must be named"
    nms <- nms %||% character()
  }
  dup <- unique(nms[duplicated(nms) & nzchar(nms)])
  problems <- c(problems, sprintf("`%s` is named twice", dup))
  for (nm in setdiff(nms[nzchar(nms)], dup)) {
    problems <- c(problems, spec_arg_problems(spec$args[[nm]], nm, nms))
  }
  if (length(problems)) {
    stop("Spec `", spec$block, "`: ", paste(problems, collapse = "; "), ".",
         call. = FALSE)
  }
  invisible(spec)
}

# Problems with one argument, as short texts naming it.
spec_arg_problems <- function(arg, name, all) {
  if (!inherits(arg, "viz_arg")) {
    return(sprintf("`%s` is not a viz_arg()", name))
  }
  p <- c(type_problems(arg$type), field_rule_problems(arg),
         gear_problems(arg$gear, arg))
  bad_refs <- setdiff(c(spec_type_refs(arg$type), arg$gear$when), all)
  p <- c(p, sprintf("refers to unknown argument `%s`", bad_refs))
  if (!length(p)) {
    p <- value_problems(arg, name)
  }
  if (length(p)) sprintf("`%s` %s", name, p) else character()
}

type_problems <- function(type) {
  t <- type$name
  if (!is.character(t) || length(t) != 1L || !t %in% VIZ_TYPES) {
    return(paste("has unknown type", deparse(t)))
  }
  if (t %in% VIZ_COMPOUND_TYPES && !inherits(type, "viz_type")) {
    return(paste0("needs type_", t, "()"))
  }
  switch(
    t,
    values = if (!is.null(type$of) && (!is.character(type$of) ||
                                          length(type$of) != 1L)) {
      "has a values type whose `of` is not one name"
    },
    named = c(
      if (!inherits(type$value, "viz_arg")) "has a named type without a value",
      if (!is.null(type$max) && !is_whole_count(type$max)) {
        "has a named type whose `max` is not a count"
      },
      if (inherits(type$value, "viz_arg")) {
        p <- spec_arg_problems(type$value, "value", character())
        if (length(p)) paste("has", p)
      }
    ),
    record = , records = fields_problems(type$fields),
    records_by = c(
      if (!is.character(type$key) || length(type$key) != 1L) {
        "has a records_by type without `key`"
      },
      if (!length(type$variants) || is.null(names(type$variants))) {
        "has a records_by type without named variants"
      },
      unlist(lapply(type$variants, function(v) fields_problems(v$fields)))
    ),
    NULL
  )
}

fields_problems <- function(fields) {
  nms <- names(fields)
  if (!length(fields) || is.null(nms) || any(!nzchar(nms))) {
    return("has a record type without named fields")
  }
  unlist(lapply(nms, function(f) {
    p <- spec_arg_problems(fields[[f]], f, nms)
    if (length(p)) paste("has field", p)
  }))
}

field_rule_problems <- function(arg) {
  t <- arg$type$name
  c(
    if ((t %in% c("enum", "enums")) != !is.null(arg$values)) {
      "needs `values` exactly when it is an enum or enums"
    },
    if (!is.null(arg$values) && (!is.character(arg$values) ||
                                   !length(arg$values))) {
      "has `values` that are not a non-empty character vector"
    },
    if (!is.null(arg$bounds) && (!t %in% c("number", "integer") ||
                                   !inherits(arg$bounds, "viz_bounds"))) {
      "has `bounds` but is not a number or integer"
    },
    if (!is.null(arg$also) && !t %in% c("column", "columns")) {
      "has `also` but is not a column type"
    },
    if (!isFALSE(arg$null) && !(is.character(arg$null) &&
                                  length(arg$null) == 1L)) {
      "has `null` that is neither FALSE nor a text"
    },
    if (isTRUE(arg$required) && (!is.null(arg$default) ||
                                   !isFALSE(arg$null))) {
      "is required, so it takes no default and no `null`"
    },
    if (!is.null(arg$applies) && !is.character(arg$applies)) {
      "has `applies` that is not a character vector or except()"
    },
    if (!all(vapply(arg[c("slot", "ctrl", "ai", "required")], is_flag,
                    logical(1L)))) {
      "has `slot`, `ctrl`, `ai` or `required` that is not TRUE or FALSE"
    },
    if (isTRUE(arg$slot) && t %in% c("record", "records", "records_by",
                                     "named", "map")) {
      "cannot be a title slot"
    }
  )
}

is_flag <- function(x) is.logical(x) && length(x) == 1L && !is.na(x)

gear_problems <- function(g, arg) {
  if (is.null(g)) {
    return(NULL)
  }
  if (!inherits(g, "viz_gear")) {
    return("has a gear entry that is not from gear()")
  }
  n <- switch(arg$type$name, enum = , enums = length(arg$values), bool = 2L,
              NA_integer_)
  c(
    if (!is.character(g$control) || !g$control %in% VIZ_CONTROLS) {
      paste("has unknown gear control", deparse(g$control))
    },
    if (!is.character(g$group) || !is.character(g$label)) {
      "has a gear entry without group and label"
    },
    if (!is.null(g$labels) && !is.na(n) && length(g$labels) != n) {
      sprintf("has %d gear labels for %d values", length(g$labels), n)
    }
  )
}

# The arguments a type names. Inside a record, `values(of = <field>)` names a
# sibling field, which is not an argument.
spec_type_refs <- function(type) {
  if (!identical(type$name, "values")) {
    return(character())
  }
  type$of %||% character()
}

# A literal default and an example must pass their own type.
value_problems <- function(arg, name) {
  vals <- list(default = if (!inherits(arg$default, "viz_opt") &&
                               !isTRUE(arg$required)) list(arg$default),
               example = if (!is.null(arg$example)) list(arg$example))
  p <- character()
  for (what in names(vals)) {
    if (is.null(vals[[what]])) {
      next
    }
    msg <- tryCatch({
      viz_check_value(arg, vals[[what]][[1L]], name)
      NULL
    }, viz_arg_error = function(e) conditionMessage(e))
    if (!is.null(msg)) {
      p <- c(p, paste0("has a ", what, " that fails: ",
                       sub("^`[^`]*` ", "", sub("\\.$", "", msg))))
    }
  }
  p
}
