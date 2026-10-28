# The table block's emitted filter, from its selection. Pure functions of
# the selection and the drill mode; the layouts' servers call them.

# The records layout's filter, a bquoted call. It coerces to the annotated data
# frame first, so a plain frame and a table-producing object give one output
# contract, and it is self-qualified so the emitted code stands alone.
# `click` is table_click()'s; `source`: whether `drill = "source"` is the
# source mode.
table_records_expr <- function(click, source = FALSE) {
  f <- click$filters
  ex <- table_records_source(table_records_filter(click$structured, f),
                             names(f), source)
  # The arm of the clicked cell or header, on the source rows.
  if (length(click$columns) && isTRUE(source)) {
    ex <- table_col_keys_filter(ex, click$columns)
  }
  ex
}

# The filter itself: AND each clicked key of a structured table, else the
# clicked column's values, else every row.
table_records_filter <- function(structured, f) {
  if (isTRUE(structured) && length(f)) {
    # A pure subset of the annotated df: the identity columns already carry
    # the selection.
    cond <- table_group_filter_call(names(f), unlist(f))
    return(blockr.core::bbquote(
      dplyr::filter(blockr.viz::as_annotated_df(.(data)), .(cond)),
      list(cond = cond)
    ))
  }
  if (!length(f)) {
    return(blockr.core::bbquote(
      dplyr::filter(blockr.viz::as_annotated_df(.(data)), TRUE)
    ))
  }
  col <- names(f)[[1L]]
  vals <- f[[1L]]
  if (length(vals) == 1) {
    return(blockr.core::bbquote(
      dplyr::filter(
        blockr.viz::as_annotated_df(.(data)),
        .data[[.(col)]] == .(val)
      ),
      list(col = col, val = vals[[1]])
    ))
  }
  blockr.core::bbquote(
    dplyr::filter(
      blockr.viz::as_annotated_df(.(data)),
      .data[[.(col)]] %in% .(vals)
    ),
    list(col = col, vals = vals)
  )
}

# `drill = "source"` wraps whatever the drill built. With no click the output
# is every source row (`unclaimed = "all"`); a click claiming nothing
# resolvable gives zero rows. `columns` follows ctrl_claims(): dotted names
# are ARD identity columns (identity mode, no `columns`), undotted ones the
# source columns an aggregated table claims.
table_records_source <- function(ex, claim_cols, source) {
  if (!isTRUE(source)) {
    return(ex)
  }
  cols <- if (length(claim_cols)) {
    claim_cols[!startsWith(claim_cols, ".")]
  }
  call <- list(quote(blockr.viz::drill_source), ex)
  if (length(cols)) call <- c(call, list(columns = cols))
  if (!length(claim_cols)) call <- c(call, list(unclaimed = "all"))
  as.call(call)
}

# The kit's selection as a click on a frame: `filters` (column to values)
# and `columns` (the clicked cell's or header's column identity), as
# character. A structured or nested table claims its rows' identity columns,
# one value each; a flat table claims one column.
table_click <- function(sel, structured = FALSE) {
  chr <- function(x) {
    x <- lapply(x %||% list(), function(v) as.character(unlist(v)))
    x[lengths(x) > 0L]
  }
  f <- chr(if (is.list(sel)) sel$filters)
  f <- if (isTRUE(structured)) lapply(f, `[`, 1L) else utils::head(f, 1L)
  list(filters = f, columns = chr(if (is.list(sel)) sel$columns),
       structured = isTRUE(structured))
}

# The kit's selection as the groups layout's filters, column to values, as
# table_drill_expr() reads them; empty for no click.
table_selection_groups <- function(sel) {
  f <- if (is.list(sel)) sel$filters
  if (!length(f)) {
    return(list())
  }
  lapply(f, as.character)
}
