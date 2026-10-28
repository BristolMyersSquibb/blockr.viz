# The groups table's row order: `sort_by`, `sort_dir` and the sort keys.

# "label" is an alias of "alpha".
#' @noRd
groups_sort_by <- function(sort_by) {
  sb <- chr_state(sort_by) %||% "value"
  if (identical(sb, "label")) "alpha" else sb
}

# `sort_by` is either a plan-independent keyword or a facet level name.
#' @noRd
groups_sort_key <- function(sort_by, plan) {
  sb <- groups_sort_by(sort_by)
  if (identical(sb, "alpha")) return(".label")
  if (identical(sb, "data")) return(".ord")
  if (identical(sb, "value")) return(".v")
  hit <- vapply(plan, function(p) identical(p$label, sb), logical(1L))
  if (any(hit)) {
    key <- plan[[which(hit)[[1L]]]]$key
    if (!is.null(key)) return(key)
  }
  ".v"
}

# A raw data column as the ordering key: the group's MINIMUM of it (chart `mins`
# in orderGroups). AVISITN orders visits a character AVISIT cannot. Groups with
# no value keep NA and sort last in both directions.
#' @noRd
groups_min_ord <- function(data, keycol, sortcol, labels) {
  v <- suppressWarnings(as.numeric(data[[sortcol]]))
  m <- tapply(v, as.character(data[[keycol]]), function(z) {
    z <- z[is.finite(z)]
    if (length(z)) min(z) else NA_real_
  })
  as.numeric(m[as.character(labels)])
}

# Resolve `sort_by` to a column on the assembled frames; a raw data column is
# folded into `.ord` so the assembler sees a single key.
#' @noRd
groups_resolve_sort <- function(sort_by, plan, data, leaf, par_rows, group,
                                   parent) {
  key <- groups_sort_key(sort_by, plan)
  sb <- groups_sort_by(sort_by)
  # A numeric row NAME sorts numerically; `.label` is text, where "10" sorts
  # before "2".
  if (identical(key, ".label")) {
    num <- function(df, col) {
      if (is.null(df) || is.null(col) || !is.numeric(data[[col]])) return(df)
      df$.labn <- suppressWarnings(as.numeric(df$.label))
      df
    }
    if (is.numeric(data[[group]])) {
      leaf <- num(leaf, group)
      par_rows <- if (is.null(parent)) NULL else num(par_rows, parent)
      if (!is.null(leaf$.labn)) key <- ".labn"
    }
  }
  if (identical(key, ".v") && !sb %in% c("value", "alpha", "data") &&
        sb %in% names(data)) {
    leaf$.ord <- groups_min_ord(data, group, sb, leaf$.label)
    if (!is.null(par_rows)) {
      par_label <- if (!is.null(par_rows$.parent_key)) {
        par_rows$.parent_key
      } else {
        par_rows$.label
      }
      par_rows$.ord <- groups_min_ord(data, parent, sb, par_label)
    }
    key <- ".ord"
  }
  list(key = key, leaf = leaf, par_rows = par_rows)
}
