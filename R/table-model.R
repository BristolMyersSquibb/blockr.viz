# The table model: what each layout of the table block shows, computed from
# the block's state under the spec's names (TABLE_SPEC) and the data. Pure
# functions, no Shiny; the layout servers read the state, call these, and
# send the result.
#
#   table_model_records()    the records layout's cell model
#   table_model_tint()       the records layout's row tint
#   table_model_groups()     the groups layout's prepared rows and cells
#   table_model_variables()  the variables layout's Table 1
#
# The compute behind them: R/table-model-records.R (cells),
# R/table-model-format.R (number formats), R/table-model-shading.R (cell
# shading), R/table-model-groups*.R (the groups preparation) and
# R/table-model-cells*.R (the groups cell model).

#' The records layout's cell model
#'
#' @param state The table's state, under the spec's names.
#' @param data The input rows.
#' @param tint The row tint, from table_model_tint(): `list(col, hex)`.
#' @param active The current selection, to mark the selected rows.
#' @param gear_cols The columns the gear offers, as JSON; `NULL` reads them
#'   off `data`.
#' @return dt_flat_build()'s cell model (`kind = "flat"`, or `"message"`
#'   for a state with nothing to show), or `list(kind = "structured", data)`
#'   for a frame that carries its own structure (a Table 1, the rows nested
#'   under `by`), which the structured renderer draws.
#' @noRd
table_model_records <- function(state, data, tint = NULL, active = NULL,
                                gear_cols = NULL) {
  if (length(state$by) && is.data.frame(data)) {
    data <- table_nest(data, state$by, state$rowname,
                       missing = state$missing %||% "level",
                       columns = state$columns)
  }
  if (dt_is_structured(data)) {
    return(list(kind = "structured", data = data))
  }
  toggles <- list(
    sortable = isTRUE(state$sortable), collapsible = isTRUE(state$collapsible),
    search = isTRUE(state$search), download = isTRUE(state$download),
    rotate_titles = identical(state$look, "compact"), unit = state$unit
  )
  data <- dt_unit_labels(data, state$unit, state$rowname)
  dt_flat_build(
    data, state$rowname, state$columns, dd_parse_shadings(state$shadings),
    state$drill, state$format %||% "number", row_hex = tint$hex,
    color = tint$col, toggles = toggles, active = active,
    gear_cols = gear_cols
  )
}

#' The records layout's row tint
#'
#' `color = "auto"` tints by the stub column where the board's scale map
#' has it; a column tints by that column's levels; `NULL` is no tint.
#' @param state The table's state, under the spec's names.
#' @param data The input rows.
#' @param scale_map The board's scale map, or `NULL`.
#' @return `list(col, hex)`: the column the tint reads and one colour per
#'   row, `NA` where a row has none; both `NULL` without a tint.
#' @noRd
table_model_tint <- function(state, data, scale_map = NULL) {
  color <- state$color
  if (is.null(color) || !is.data.frame(data)) {
    return(list(col = NULL, hex = NULL))
  }
  if (identical(color, "auto")) {
    col <- state$rowname %||% names(data)[1L]
    return(list(col = col, hex = dd_row_hex(scale_map, col, data)))
  }
  list(col = color, hex = dd_ident_hex(scale_map, color, data))
}

#' The groups layout's rows and cells
#'
#' @param state The table's state, under the spec's names.
#' @param data The input rows.
#' @param scale_map The board's scale map, or `NULL`.
#' @param subject The board's subject column for distinct counts, or `NULL`.
#' @param active The current selection, to mark the selected rows.
#' @return `list(prep, cells)`: summarize_prepare()'s rows and plan, and
#'   summarize_cells()'s cell model; `cells` is `NULL` when `prep` carries
#'   an error (`prep$err`).
#' @noRd
table_model_groups <- function(state, data, scale_map = NULL, subject = NULL,
                               active = NULL) {
  prep <- summarize_prepare(
    data, summaries = state$summaries, by = state$by,
    facet_layout = state$facet_layout %||% "by_summary",
    sort_by = state$sort_by %||% "value",
    sort_dir = state$sort_dir %||%
      (if (identical(state$sort_by %||% "value", "value")) "desc" else "asc"),
    top_n = state$top_n, scale_map = scale_map, subject = subject,
    pct_of = state$percent_of %||% "facet",
    missing = state$missing %||% "level",
    format = state$format, unit = state$unit
  )
  if (!is.null(prep$err)) {
    return(list(prep = prep, cells = NULL))
  }
  drill <- if (length(state$by)) state$drill
  cfg <- list(
    value_labels = state$value_labels, sortable = state$sortable,
    axis = state$column_axis, compact = identical(state$look, "compact"),
    look = state$look, search = state$search, download = state$download,
    bar_width = state$bar_width, drill = state$drill
  )
  list(prep = prep,
       cells = summarize_cells(prep, drill = drill, active = active,
                               cfg = cfg))
}

#' The variables layout's Table 1
#'
#' The variables are the summaries' variables less the `by` columns.
#' @param state The table's state, under the spec's names.
#' @param data The input rows.
#' @return summary_table()'s annotated frame, or `data` unchanged while no
#'   summary names a variable.
#' @noRd
table_model_variables <- function(state, data) {
  vars <- setdiff(table_summaries_vars(state$summaries), state$by)
  args <- table_variables_args(list(
    vars = vars, sections = state$sections, by = state$by,
    stats = state$stats, total = state$total,
    total_label = state$total_label, nest = state$nest, id = state$id,
    missing = state$missing
  ))
  if (is.null(args)) {
    return(data)
  }
  do.call(summary_table, c(list(data), args))
}
