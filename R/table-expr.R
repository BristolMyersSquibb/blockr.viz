# The table block's emitted filter, from its selection. Pure functions of
# the selection and the drill mode; the layouts' servers call them.

#' The records layout's filter
#'
#' The expr coerces before filtering, so the block's data output (and the
#' drill filter) is always the annotated data frame: one downstream contract
#' whether the input was a plain data frame (passthrough) or a
#' table-producing object such as a composer table. Self-qualified so the
#' emitted code stands alone.
#'
#' @param sel The click: `column` and `values` (a flat table), or
#'   `group_cols` and `group_vals` (a structured table), and `col_keys` (the
#'   clicked cell's or header's column identity).
#' @param source Whether `drill = "source"` is the source mode here (a
#'   structured or aggregated table without a column called "source").
#' @return The bquoted filter call.
#' @noRd
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
  if (length(ck) && isTRUE(source)) ex <- dd_col_keys_filter(ex, ck)
  ex
}

# The filter itself: AND each clicked key of a grouped or structured table,
# else the clicked column's values, else every row.
table_records_filter <- function(grouped, gc, gv, col, vals) {
  if (grouped) {
    # The output is a pure SUBSET of the annotated df -- the identity columns
    # (`.variable`, `.variable_level`, `.group<k>*`) already carry the
    # selection, so no synthetic column is added; downstream consumers read
    # the identity columns themselves.
    cond <- dd_group_filter_call(gc, gv)
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

# `drill = "source"` wraps whatever the drill built, so every branch goes
# through one resolution step. Before any click the output is every source
# row, as below a chart or a groups table (`unclaimed = "all"`); a click that
# claims nothing resolvable still gives zero rows. The wrap is a plain call
# around the bbquote result, so the emitted code still stands alone.
#
# `columns` tells drill_source() which mode to resolve in, using
# dd_ctrl_claims()'s rule: dotted names are ARD identity columns (structured
# table -> identity mode, no `columns`), undotted ones are real source
# columns (aggregated table -> those are the claim).
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
