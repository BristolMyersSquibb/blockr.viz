# The table model: what each layout shows, computed from the block's state
# (under TABLE_SPEC's names) and the data. Pure functions, no Shiny.
#
#   table_model_records()    the records layout's cell model
#   table_model_tint()       the records layout's row tint
#   table_model_groups()     the groups layout's prepared rows and cells
#   table_model_variables()  the variables layout's Table 1

# The records layout's cell model: dt_flat_build()'s (`kind` "flat" or
# "message"), or list(kind = "structured", data) for a frame that carries its
# own structure. `tint` is table_model_tint()'s list(col, hex).
table_model_records <- function(state, data, tint = NULL) {
  if (length(state$by) && is.data.frame(data)) {
    data <- table_nest(data, state$by, state$rowname,
                       missing = state$missing %||% "level",
                       columns = state$columns)
  }
  if (table_is_structured(data)) {
    return(list(kind = "structured", data = data))
  }
  data <- dt_unit_labels(data, state$unit, state$rowname)
  dt_flat_build(
    data, state$rowname, state$columns, dd_parse_shadings(state$shadings),
    state$drill, state$format %||% "number", row_hex = tint$hex,
    rotate = identical(state$look, "compact")
  )
}

# The records layout's row tint, list(col, hex): `color = "auto"` tints by the
# stub column where the scale map has it, a column name by that column's
# levels, NULL not at all. `hex` is NA where a row has none.
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

# The groups layout's list(prep, cells): groups_prepare()'s rows and plan,
# and groups_cells()'s cell model, which is NULL when `prep$err` is set.
table_model_groups <- function(state, data, scale_map = NULL, subject = NULL) {
  state <- table_state_defaults(state)
  prep <- groups_prepare(
    data, summaries = state$summaries, by = state$by,
    facet_layout = state$facet_layout, sort_by = state$sort_by,
    sort_dir = table_sort_dir(state), top_n = state$top_n,
    scale_map = scale_map, subject = subject, pct_of = state$percent_of,
    missing = state$missing, format = state$format, unit = state$unit
  )
  if (!is.null(prep$err)) {
    return(list(prep = prep, cells = NULL))
  }
  cfg <- list(value_labels = state$value_labels,
              compact = identical(state$look, "compact"))
  list(prep = prep,
       cells = groups_cells(prep, drill = if (length(state$by)) state$drill,
                            cfg = cfg))
}

# A partial state completed with the spec's defaults.
table_state_defaults <- function(state) {
  d <- lapply(viz_block_spec("table")$args, spec_default)
  for (nm in names(state)) d[nm] <- list(state[[nm]])
  d
}

# The groups layout's sort direction; unset, descending for `sort_by = "value"`
# and ascending otherwise.
table_sort_dir <- function(state) {
  state$sort_dir %||%
    (if (identical(state$sort_by %||% "value", "value")) "desc" else "asc")
}

# The variables layout's Table 1: summary_table()'s frame for the summaries'
# variables less the `by` columns, or `data` while no summary names a variable.
table_model_variables <- function(state, data) {
  args <- table_variables_args(table_variables_cfg(state, data))
  if (is.null(args)) {
    return(data)
  }
  do.call(summary_table, c(list(data), args))
}

# The variables layout's settings as summary_table() reads them. A variable
# listed as a row count that the data holds as a number is summarised as a
# distribution (table_fix_var_types()).
table_variables_cfg <- function(state, data = NULL) {
  summaries <- table_fix_var_types(state$summaries, data)
  list(
    vars = setdiff(table_summaries_vars(summaries), state$by),
    sections = state$sections, by = state$by, stats = state$stats,
    total = state$total, total_label = state$total_label, nest = state$nest,
    id = state$id, missing = state$missing
  )
}

# Whether a wide frame carries the dotted structure columns (`.group<k>*`,
# `.variable*`, `.label`, `.indent`, `.strong`) that summary_table() writes.
table_is_structured <- function(data) {
  any(grepl("^\\.(group\\d+(_level|_label)?|variable(_level|_label)?|label|indent|strong)$",
            names(data)))
}
