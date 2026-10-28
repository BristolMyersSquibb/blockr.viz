# The constructors' help pages, from the specs.
#
# `#' @eval viz_spec_params("chart")` in R/doc-chart-block.R writes one
# @param per spec argument when roxygen builds the docs, so the help page
# and the argument checks read the same spec.

#' Roxygen `@param` lines for a block, from its spec
#'
#' @param block A name in `VIZ_BLOCK_SPECS`.
#' @param dots The text for `...`.
#' @return A character vector of roxygen lines.
#' @noRd
viz_spec_params <- function(block, dots = NULL) {
  spec <- viz_block_spec(block)
  lines <- vapply(names(spec$args), function(nm) {
    paste0("@param ", nm, " ", spec_param_text(spec$args[[nm]]))
  }, character(1L))
  c(if (!is.null(dots)) paste0("@param ... ", dots), unname(lines))
}

# One argument's text: what it is (the spec's `describe`), the values it
# takes, and its default.
spec_param_text <- function(arg) {
  parts <- c(
    arg$describe %||% "",
    if (!is.null(arg$values)) {
      paste0("One of ", paste0("`\"", arg$values, "\"`", collapse = ", "),
             ".")
    },
    spec_param_default(arg)
  )
  paste(parts[nzchar(parts)], collapse = " ")
}

spec_param_default <- function(arg) {
  d <- arg$default
  if (isTRUE(arg$required)) {
    return("Required.")
  }
  if (inherits(d, "viz_opt")) {
    return(paste0("Default: the option `", d$name, "`, else ",
                  spec_value_text(d$fallback), "."))
  }
  if (is.null(d) || (is.list(d) && !length(d))) {
    if (!isFALSE(arg$null)) {
      return(paste0("Default `NULL`: ", arg$null, "."))
    }
    return(paste0("Default: ", spec_value_text(d), "."))
  }
  paste0("Default: ", spec_value_text(d), ".")
}

spec_value_text <- function(x) {
  if (is.null(x)) {
    return("`NULL`")
  }
  if (is.list(x) && !length(x)) {
    return("`list()`")
  }
  paste0("`", paste(deparse(x, width.cutoff = 500L), collapse = ""), "`")
}

#' A roxygen `@section` listing the keys of nested specs
#'
#' For an argument whose entries are records: the table's `summaries` (one
#' spec per summary type) and `shadings`.
#'
#' @param title The section title.
#' @param intro Text before the list.
#' @param specs A named list of specs, one per value of `key`, or one spec.
#' @param key The field that picks the spec (`"type"`), or `NULL`.
#' @return A character vector of roxygen lines.
#' @noRd
viz_spec_section <- function(title, intro, specs, key = NULL) {
  if (inherits(specs, "viz_spec")) {
    specs <- list(specs)
  }
  out <- c(paste0("@section ", title, ":"), intro, "")
  for (nm in names(specs) %||% "") {
    args <- specs[[if (nzchar(nm)) nm else 1L]]$args
    args <- args[setdiff(names(args), key)]
    if (!is.null(key)) {
      out <- c(out, paste0("**`", key, " = \"", nm, "\"`**"), "")
    }
    out <- c(out, vapply(names(args), function(a) {
      paste0("* `", a, "`: ", spec_param_text(args[[a]]))
    }, character(1L)), "")
  }
  out
}
