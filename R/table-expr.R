# The table block's emitted filter, from its selection. Pure functions of
# the selection and the drill mode; the layouts' servers call them.

# The records layout's filter, a bquoted call. It coerces to the annotated data
# frame first, so a plain frame and a table-producing object give one output
# contract, and it is self-qualified so the emitted code stands alone.
# `sel` is the click: `column` and `values` (flat table) or `group_cols` and
# `group_vals` (structured), plus `col_keys` (the clicked cell's or header's
# column identity). `source`: whether `drill = "source"` is the source mode.
table_records_expr <- function(sel, source = FALSE) {
  gc <- sel$group_cols
  gv <- sel$group_vals
  col <- sel$column
  vals <- sel$values
  grouped <- length(gc) && length(gc) == length(gv)
  ex <- table_records_filter(grouped, gc, gv, col, vals)
  claim_cols <- if (grouped) {
    as.character(gc)
  } else if (!is.null(col)) {
    as.character(col)
  }
  ex <- table_records_source(ex, claim_cols, source)
  ck <- sel$col_keys
  # The arm of the clicked cell or header, on the source rows.
  if (length(ck) && isTRUE(source)) ex <- table_col_keys_filter(ex, ck)
  ex
}

# The filter itself: AND each clicked key of a grouped or structured table,
# else the clicked column's values, else every row.
table_records_filter <- function(grouped, gc, gv, col, vals) {
  if (grouped) {
    # A pure subset of the annotated df: the identity columns already carry
    # the selection.
    cond <- table_group_filter_call(gc, gv)
    return(blockr.core::bbquote(
      dplyr::filter(blockr.viz::as_annotated_df(.(data)), .(cond)),
      list(cond = cond)
    ))
  }
  if (is.null(col) || is.null(vals) || length(vals) == 0) {
    return(blockr.core::bbquote(
      dplyr::filter(blockr.viz::as_annotated_df(.(data)), TRUE)
    ))
  }
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

# The kit's selection as the records layout's click, as table_records_expr()
# reads it. The kit holds `filters` (column to values) and `columns` (the
# clicked cell's or header's column identity). A flat table claims one column;
# a structured or nested table its rows' identity columns, one value each.
table_selection_records <- function(sel, structured = FALSE) {
  f <- if (is.list(sel)) sel$filters
  out <- list()
  if (length(f) && isTRUE(structured)) {
    out$group_cols <- names(f)
    out$group_vals <- lapply(f, function(v) as.character(v)[[1L]])
  } else if (length(f)) {
    out$column <- names(f)[[1L]]
    out$values <- as.character(f[[1L]])
  }
  ck <- if (is.list(sel)) sel$columns
  if (length(ck)) {
    out$col_keys <- unname(Map(function(cl, vl) {
      list(column = cl, values = as.character(vl))
    }, names(ck), ck))
  }
  out
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
