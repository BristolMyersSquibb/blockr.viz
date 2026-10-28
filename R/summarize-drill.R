# Summarize row click. A row claims its grouping path (`AEBODSYS = X,
# AEDECOD = Y` for a child), since a child label can repeat under several
# parents. Stored as `filter_column` (path columns) and `filter_values` (one
# value per column); a saved single column with several values means any of
# them.

#' The stored click as filters: a named list, column -> value(s), ANDed.
#' `list()` when there is no click or the slots do not pair up.
#' @noRd
summarize_drill_filters <- function(col, vals) {
  col <- as.character(unlist(col %||% character()))
  col <- col[!is.na(col) & nzchar(col)]
  vals <- as.character(unlist(vals %||% character()))
  if (!length(col) || !length(vals)) {
    return(list())
  }
  if (length(col) == 1L) {
    return(stats::setNames(list(vals), col))
  }
  if (length(vals) != length(col)) {
    return(list())
  }
  stats::setNames(as.list(vals), col)
}

#' The footer's words for a click, "AEBODSYS = X, AEDECOD = Y"; NULL if none.
#' @noRd
summarize_drill_phrase <- function(filters) {
  if (!length(filters)) {
    return(NULL)
  }
  paste(
    vapply(names(filters), function(nm) {
      paste0(nm, " = ", paste(filters[[nm]], collapse = ", "))
    }, character(1L)),
    collapse = ", "
  )
}

#' Rows lit by the stored click: those whose own path ends on the last filtered
#' column and holds every filtered value.
#' @noRd
summarize_drill_on <- function(rows, group, parent, filters) {
  n <- nrow(rows)
  if (!length(filters) || is.null(group)) {
    return(rep(FALSE, n))
  }
  label <- as.character(rows$.label)
  is_par <- if (is.null(rows$.is_parent)) rep(FALSE, n) else rows$.is_parent
  par_val <- if (is.null(rows$.parent)) {
    rep(NA_character_, n)
  } else {
    as.character(rows$.parent)
  }
  own_col <- if (is.null(parent)) {
    rep(group, n)
  } else {
    ifelse(is_par, parent, group)
  }
  last <- names(filters)[[length(filters)]]
  on <- own_col == last
  for (nm in names(filters)) {
    vals <- filters[[nm]]
    hit <- if (!is.null(parent) && identical(nm, parent)) {
      ifelse(is_par, label, par_val) %in% vals
    } else if (identical(nm, group)) {
      !is_par & label %in% vals
    } else {
      rep(FALSE, n)
    }
    on <- on & hit
  }
  on
}

#' The click as a `dplyr::filter()` call on `data`. `members` maps a grouped
#' column to the raw values its clicked group pools. A click on "(Missing)"
#' claims missing values.
#' @noRd
summarize_drill_expr <- function(filters, members = NULL) {
  if (!length(filters)) {
    return(quote(identity(data)))
  }
  conds <- lapply(names(filters), function(nm) {
    vals <- as.character(members[[nm]] %||% filters[[nm]])
    cond <- bquote(.data[[.(nm)]] %in% .(vals))
    if (MISSING_LEVEL %in% vals) {
      cond <- bquote((.(cond) | is.na(.data[[.(nm)]])))
    }
    cond
  })
  as.call(c(list(quote(dplyr::filter), quote(data)), conds))
}

#' The block's expression: the click filter around the prepare script `sx`.
#' Columns in `src_cols` (upstream names) are filtered before the script, the
#' rest (made by the script) after it. NULL `src_cols` filters before.
#' @noRd
summarize_drill_wrap <- function(sx, filters, members = NULL,
                                 src_cols = NULL) {
  if (is.null(sx)) {
    return(summarize_drill_expr(filters, members))
  }
  post <- dd_drill_post(as.list(names(filters)), src_cols)
  out <- dd_splice_slot(sx, summarize_drill_expr(filters[!post], members))
  if (!any(post)) {
    return(out)
  }
  ex <- summarize_drill_expr(filters[post], members)
  ex[[2L]] <- out
  ex
}
