# The groups table's rows in order: ranked, capped and nested.

#' Shared ordering, fold and row assembly: rank order, cap, nest.
#' @noRd
groups_assemble_rows <- function(leaf, par_rows, parent, sort_key, sort_dir,
                                    top_n) {
  ord <- function(df) {
    if (is.null(df) || !nrow(df)) return(df)
    v <- if (identical(sort_key, ".label")) df$.label else df[[sort_key]]
    if (is.null(v)) v <- df$.v
    if (is.character(v)) {
      df[order(v, decreasing = identical(sort_dir, "desc")), , drop = FALSE]
    } else {
      df[order(v, decreasing = identical(sort_dir, "desc"), na.last = TRUE), ,
         drop = FALSE]
    }
  }

  if (is.null(parent)) {
    top <- groups_top_n(ord(leaf), top_n)
    rows <- top$rows
    rows$.level <- 0L
    rows$.is_parent <- FALSE
  } else {
    par_rows <- if (is.null(par_rows$.lvl)) {
      ord(par_rows)
    } else {
      par_rows[order(par_rows$.lvl, na.last = TRUE), , drop = FALSE]
    }
    top <- groups_top_n(par_rows, top_n)
    par_rows <- top$rows
    par_rows$.level <- 0L
    par_rows$.is_parent <- TRUE
    leaf$.level <- 1L
    leaf$.is_parent <- FALSE
    # Renderer-internal columns are dot-prefixed; the key columns are not, and
    # rbind() must not demand them from both frames.
    keep <- grep("^\\.", names(leaf), value = TRUE)
    # Each parent, then its rows in sort order. order() keeps ties in input
    # order, so sorting all inner rows once and grouping them by parent is
    # the per-parent sort; one rbind() instead of one per parent, which took
    # the time of a table with many groups.
    at <- match(leaf$.parent, par_rows$.parent_key)
    kids <- ord(leaf[!is.na(at), , drop = FALSE])
    at <- match(kids$.parent, par_rows$.parent_key)
    np <- nrow(par_rows)
    rows <- rbind(par_rows[, intersect(keep, names(par_rows)), drop = FALSE],
                  kids[, intersect(keep, names(kids)), drop = FALSE])
    o <- order(c(seq_len(np), at), c(integer(np), seq_along(at)))
    rows <- rows[o, , drop = FALSE]
  }
  list(rows = rows, folded = top$folded, fold_max = top$fold_max)
}

# The top N rows of the outer level. The rest fold into one line that says how
# many (`folded`) and their largest value (`fold_max`).
groups_top_n <- function(rows, top_n) {
  if (is.null(top_n) || !is.finite(top_n) || top_n <= 0 ||
        nrow(rows) <= top_n) {
    return(list(rows = rows, folded = 0L, fold_max = NA_real_))
  }
  rest <- rows[seq.int(top_n + 1L, nrow(rows)), , drop = FALSE]
  list(rows = rows[seq_len(top_n), , drop = FALSE], folded = nrow(rest),
       fold_max = suppressWarnings(max(rest$.v, na.rm = TRUE)))
}
