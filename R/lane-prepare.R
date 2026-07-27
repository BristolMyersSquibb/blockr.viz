# Summarize table: shared row assembly ----------------------------------------
#
# Helpers shared by the bar path (rank-table.R) and the summaries path
# (lane-summaries.R). The single-mark preparers that briefly lived here are
# gone: the column list is the one config model, and only the ranked bar
# keeps a dedicated path.
#
# The rule for what belongs on this surface: one row is one category, and the
# mark is a horizontal glyph on a shared linear domain confined to a cell
# (_blockr.design/open/summarize-table/).

#' Shared ordering + fold + row assembly: the tail of rank_prepare(), split out
#' so every mark reuses one definition of "rank order, cap, nest".
#' @noRd
rank_assemble_rows <- function(leaf, par_rows, parent, sort_key, sort_dir,
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

  folded <- 0L
  fold_max <- NA_real_
  if (is.null(parent)) {
    rows <- ord(leaf)
    if (!is.null(top_n) && is.finite(top_n) && top_n > 0 && nrow(rows) > top_n) {
      rest <- rows[seq.int(top_n + 1L, nrow(rows)), , drop = FALSE]
      folded <- nrow(rest)
      fold_max <- suppressWarnings(max(rest$.v, na.rm = TRUE))
      rows <- rows[seq_len(top_n), , drop = FALSE]
    }
    rows$.level <- 0L
    rows$.is_parent <- FALSE
  } else {
    par_rows <- ord(par_rows)
    if (!is.null(top_n) && is.finite(top_n) && top_n > 0 &&
          nrow(par_rows) > top_n) {
      rest <- par_rows[seq.int(top_n + 1L, nrow(par_rows)), , drop = FALSE]
      folded <- nrow(rest)
      fold_max <- suppressWarnings(max(rest$.v, na.rm = TRUE))
      par_rows <- par_rows[seq_len(top_n), , drop = FALSE]
    }
    par_rows$.level <- 0L
    par_rows$.is_parent <- TRUE
    leaf$.level <- 1L
    leaf$.is_parent <- FALSE
    # Every renderer-internal column is dot-prefixed; the original data
    # columns (the group / parent keys) are not, and rbind() must not demand
    # them from both frames.
    keep <- grep("^\\.", names(leaf), value = TRUE)
    pieces <- list()
    for (i in seq_len(nrow(par_rows))) {
      p <- par_rows[i, , drop = FALSE]
      kids <- ord(leaf[!is.na(leaf$.parent) & leaf$.parent == p$.label, ,
                       drop = FALSE])
      pieces[[length(pieces) + 1L]] <-
        p[, intersect(keep, names(p)), drop = FALSE]
      if (nrow(kids)) {
        pieces[[length(pieces) + 1L]] <- kids[, intersect(keep, names(kids)),
                                              drop = FALSE]
      }
    }
    rows <- do.call(rbind, pieces)
  }
  list(rows = rows, folded = folded, fold_max = fold_max)
}

#' Match an arbitrary stat column from `src` into `target` by `keys`
#' (rank_match generalized beyond `.v`; absent keys stay NA -- an absent
#' facet cell draws nothing).
#' @noRd
rank_match_col <- function(target, src, keys, col) {
  tk <- do.call(paste, c(lapply(keys, function(k) as.character(target[[k]])),
                         list(sep = "\r")))
  sk <- do.call(paste, c(lapply(keys, function(k) as.character(src[[k]])),
                         list(sep = "\r")))
  as.numeric(src[[col]][match(tk, sk)])
}
