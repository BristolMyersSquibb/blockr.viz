# What a click on a table claims, how the receipt says it, and the filter it
# emits, for every layout. A groups row claims its whole grouping path
# (`AEBODSYS = X, AEDECOD = Y` for a child), since a child label can repeat
# under several parents.

#' The stored click as filters: a named list, column -> value(s), ANDed.
#' `list()` when there is no click or the slots do not pair up.
#' @noRd
table_drill_filters <- function(col, vals) {
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
table_drill_phrase <- function(filters) {
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

#' The click as a `dplyr::filter()` call on `data`. `members` maps a grouped
#' column to the raw values its clicked group pools. A click on "(Missing)"
#' claims missing values.
#' @noRd
table_drill_expr <- function(filters, members = NULL) {
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
table_drill_wrap <- function(sx, filters, members = NULL,
                                 src_cols = NULL) {
  if (is.null(sx)) {
    return(table_drill_expr(filters, members))
  }
  post <- dd_drill_post(as.list(names(filters)), src_cols)
  out <- dd_splice_slot(sx, table_drill_expr(filters[!post], members))
  if (!any(post)) {
    return(out)
  }
  ex <- table_drill_expr(filters[post], members)
  ex[[2L]] <- out
  ex
}

# `.data[[c1]] == v1 & .data[[c2]] == v2 & ...` for a grouped row click.
table_group_filter_call <- function(cols, vals) {
  conds <- Map(function(cl, vl) bquote(.data[[.(cl)]] == .(vl)),
               as.character(cols), as.character(vals))
  Reduce(function(a, b) bquote(.(a) & .(b)), conds)
}

# A nested records claim as a filter on the input: `.group<k>_level` stands
# for the k-th `by` column; a real column (the drill column) is kept. `f` is
# the click's filters, one value per column.
table_nest_drill_expr <- function(by, f) {
  gc <- as.character(names(f))
  gv <- as.character(unlist(f))
  by <- as.character(unlist(by))
  if (!length(gc) || length(gc) != length(gv)) {
    return(table_drill_expr(list()))
  }
  k <- suppressWarnings(
    as.integer(sub("^\\.group([0-9]+)_level$", "\\1", gc))
  )
  grp <- !is.na(k) & k <= length(by)
  real <- is.na(k) & !startsWith(gc, ".")
  cols <- gc
  cols[grp] <- by[k[grp]]
  keep <- grp | real
  table_drill_expr(stats::setNames(as.list(gv[keep]), cols[keep]))
}

# A claim in the data's words, outermost first: `.variable` /
# `.variable_level` read "AEDECOD = ABDOMINAL PAIN", `.group<k>_level` reads
# "ARM = Placebo" given `group_names` (table_group_names()). `f` is the
# click's filters, one value per column.
table_claim_label <- function(f, group_names = character()) {
  gc <- as.character(names(f))
  gv <- as.character(unlist(f))
  if (!length(gc) || length(gc) != length(gv)) {
    return(character())
  }
  glv <- grepl("^\\.group[0-9]+_level$", gc)
  vi <- match(".variable", gc)
  li <- match(".variable_level", gc)
  if (!is.na(vi) && !is.na(li)) {
    c(gv[glv], paste0(gv[vi], " = ", gv[li]))
  } else if (any(glv)) {
    k <- as.integer(sub("^\\.group([0-9]+)_level$", "\\1", gc[glv]))
    nm <- group_names[k]
    real <- !glv & !startsWith(gc, ".")
    c(ifelse(!is.na(nm) & nzchar(nm), paste0(nm, " = ", gv[glv]), gv[glv]),
      if (any(real)) paste0(gc[real], " = ", gv[real]))
  } else {
    paste0(gc, " = ", gv)
  }
}

# The column each `.group<k>` stands for, read off the `.group<k>` column.
table_group_names <- function(data) {
  if (!is.data.frame(data) || !nrow(data)) return(character())
  out <- character()
  k <- 1L
  while (paste0(".group", k) %in% names(data)) {
    v <- as.character(data[[paste0(".group", k)]])
    v <- v[!is.na(v) & nzchar(v)]
    out[[k]] <- if (length(v)) v[[1L]] else ""
    k <- k + 1L
  }
  out
}

# "ARM = Low dose" for each column claim (column to values).
table_col_keys_label <- function(columns) {
  vapply(names(columns), function(cl) {
    paste0(cl, " = ", paste(columns[[cl]], collapse = ", "))
  }, character(1L), USE.NAMES = FALSE)
}

# `dplyr::filter(<ex>, ARM %in% "Low dose")` for a column claim.
table_col_keys_filter <- function(ex, columns) {
  if (!length(columns)) return(ex)
  out <- table_drill_expr(columns)
  out[[2L]] <- ex
  out
}
