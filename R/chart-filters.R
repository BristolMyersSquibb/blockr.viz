# The chart block's click filter.
#
# A click in chart engine v2 sends the clicked mark's keys as a named list,
# `{AETERM: ["Rash"], AESEV: ["MODERATE"]}`; a value may be JSON null, a
# missing key. v1 sends one `column` and its `values`. Both are held as one
# shape, the block's `filters` state: a named list of character vectors, NA
# for a missing value, NULL when nothing is filtered. The columns AND
# together, in the block's own output and in a claim sent to a drill target
# (dd_ctrl_claims()).

# A filter from the constructor, saved state or a message, as a named list of
# character vectors (NA = missing), or NULL. A JSON null arrives from Shiny
# as NULL inside a list or as NA; both become NA. Unnamed or empty columns
# are dropped.
chart_filters_state <- function(x) {
  if (!is.list(x) || !length(x) || is.null(names(x))) {
    return(NULL)
  }
  out <- list()
  for (i in seq_along(x)) {
    col <- names(x)[[i]]
    if (is.na(col) || !nzchar(col)) {
      next
    }
    vals <- chart_filter_values(x[[i]])
    if (length(vals)) {
      out[[col]] <- vals
    }
  }
  if (length(out)) out else NULL
}

chart_filter_values <- function(v) {
  if (is.null(v)) {
    return(character())
  }
  if (is.list(v)) {
    v <- vapply(v, function(e) {
      if (length(e) != 1L || is.na(e)) NA_character_ else as.character(e)
    }, character(1L))
  }
  unique(unname(as.character(v)))
}

# v1's filter_column + filter_values (a saved board, or v1's message) as a
# filter. v1 never sends a missing value.
chart_legacy_filters <- function(column, values) {
  col <- chr_state(column)
  vals <- as.character(unlist(values, use.names = FALSE))
  vals <- vals[!is.na(vals)]
  if (is.null(col) || !length(vals)) {
    return(NULL)
  }
  stats::setNames(list(vals), col)
}

# The filter a categorical message carries: v2's `filters`, else v1's
# `column` / `values`. NULL clears.
chart_msg_filters <- function(msg) {
  if (!is.null(msg$filters)) {
    return(chart_filters_state(msg$filters))
  }
  chart_legacy_filters(msg$column, msg$values)
}

# The filter as the chart's config carries it to the browser: each column a
# JSON array, NA as null.
chart_filters_json <- function(filters) {
  if (!length(filters)) {
    return(NULL)
  }
  lapply(filters, as.list)
}

# v1 reads one column and its values. A filter v1 cannot show (several
# columns, a missing value, a range) gives it none.
chart_filters_v1 <- function(filters, type) {
  if (!identical(type, "categorical") || length(filters) != 1L ||
        anyNA(filters[[1L]])) {
    return(list(column = NULL, values = NULL))
  }
  list(column = names(filters), values = filters[[1L]])
}

# One condition per filtered column, for dplyr::filter(), which ANDs them.
# `members` maps a column to the members of the clicked groups of an overlap
# definition (dd_group_filters_members()). A missing value matches NA and the
# empty string, as the aggregation folds both; `%in%` rather than `==` so
# that a date column does not try to read "" as a date.
chart_filter_conds <- function(filters, members = NULL) {
  lapply(names(filters), function(col) {
    vals <- filters[[col]]
    missing <- anyNA(vals)
    vals <- vals[!is.na(vals)]
    if (length(vals)) {
      vals <- members[[col]] %||% vals
    }
    if (missing) {
      bquote(is.na(.data[[.(col)]]) | .data[[.(col)]] %in% .(c("", vals)))
    } else if (length(vals) == 1L) {
      bquote(.data[[.(col)]] == .(vals))
    } else {
      bquote(.data[[.(col)]] %in% .(vals))
    }
  })
}

# The emitted filter over `.(data)`: every condition ANDed, or the input
# passed through.
chart_filter_call <- function(conds) {
  if (!length(conds)) {
    return(blockr.core::bbquote(dplyr::filter(.(data), TRUE)))
  }
  blockr.core::bbquote(
    dplyr::filter(.(data), ..(conds)),
    list(conds = conds),
    splice = TRUE
  )
}

# A range filter's condition: x (and y, for a 2D brush) between its ends.
chart_range_cond <- function(rng) {
  if (is.null(rng) || is.null(rng$x_col) || is.null(rng$x_range)) {
    return(NULL)
  }
  xc <- rng$x_col
  xr <- rng$x_range
  yr <- rng$y_range
  if (!is.null(yr) && !is.null(rng$y_col)) {
    bquote(
      dplyr::between(.data[[.(xc)]], .(xlo), .(xhi)) &
        dplyr::between(.data[[.(yc)]], .(ylo), .(yhi)),
      list(xc = xc, yc = rng$y_col, xlo = xr[1], xhi = xr[2],
           ylo = yr[1], yhi = yr[2])
    )
  } else {
    bquote(
      dplyr::between(.data[[.(xc)]], .(xlo), .(xhi)),
      list(xc = xc, xlo = xr[1], xhi = xr[2])
    )
  }
}

# How the assistant reads a filter: "AETERM = Rash; AESEV = (missing)".
fmt_chart_filters <- function(filters) {
  if (!length(filters)) {
    return(NULL)
  }
  paste(
    vapply(names(filters), function(col) {
      v <- filters[[col]]
      v[is.na(v)] <- "(missing)"
      paste0(col, " = ", fmt_chart_val(v))
    }, character(1L)),
    collapse = "; "
  )
}
