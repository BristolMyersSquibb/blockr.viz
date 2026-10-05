# The summarize table's row click.
#
# A row stands for its grouping PATH, not for its label alone. On a nested
# table (`by = c("AEBODSYS", "AEDECOD")`) a parent row is one body system and
# a child row is one term inside one body system, so the click claims
# `AEBODSYS = X` for the parent and `AEBODSYS = X, AEDECOD = Y` for the child.
# The single drill column the block used to claim on every row sent a body
# system name as an AEDECOD value, which matched nothing, and a child label that
# repeats under several parents (SEX > ARM) claimed it under all of them.
#
# The click is stored as `filter_column` (the path's columns) and
# `filter_values` (one value per column). A board saved before carries one
# column and its values, which keeps its old meaning: that column, any of
# those values.

#' The stored click as filters: a named list, column -> value(s), the columns
#' ANDed. `list()` when there is no click, or the two slots do not pair up.
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

#' The footer's words for a click: "AEBODSYS = X, AEDECOD = Y". NULL when
#' there is none.
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

#' Which rows the stored click lights: those whose own path ends on the last
#' filtered column and holds every filtered value. A parent click lights the
#' parent only; a saved single-column click on the inner column lights the
#' matching child under every parent, as it filtered.
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

#' The click as a `dplyr::filter()` call on `data`, one condition per column.
#' `members` maps a column carrying a group definition to the raw values its
#' clicked group pools (dd_group_filters_members()).
#' @noRd
summarize_drill_expr <- function(filters, members = NULL) {
  if (!length(filters)) {
    return(quote(identity(data)))
  }
  conds <- lapply(names(filters), function(nm) {
    vals <- members[[nm]] %||% filters[[nm]]
    bquote(.data[[.(nm)]] %in% .(as.character(vals)))
  })
  as.call(c(list(quote(dplyr::filter), quote(data)), conds))
}
