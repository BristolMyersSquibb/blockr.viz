# The table block's arguments: one entry per public argument of
# new_table_block(), grouped as in the gear and assembled in formal order by
# table_spec() at the end. `applies` names layouts (records, groups,
# variables). The summary entries and shading rules have their own specs in
# R/spec-table-summaries.R. viz_arg() is in R/spec.R, the shared entries in
# R/spec-base.R, old names in R/legacy.R.
# Written from the spec table in _scratch/viz-cran-review/spec-table.md.

TABLE_LAYOUTS <- c("records", "groups", "variables")

table_spec_data <- function() {
  list(

    layout = viz_arg(
      type = "enum", default = "records", values = TABLE_LAYOUTS, null = FALSE,
      gear = gear("Table", "tiles", "Layout",
                  labels = c("List", "Per group", "Per variable")),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = "groups",
      describe = paste(
        "What a row is: \"records\" (the input rows), \"groups\" (one row per",
        "level of `by`, one column per summary) or \"variables\" (a Table 1:",
        "one row per statistic or level, one column per level of `by`)."
      )
    ),

    by = viz_arg(
      type = "columns", default = character(), null = FALSE,
      gear = gear("Table", "columns", "One row per"),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = "TRT01A",
      describe = paste(
        "Grouping columns, outer first, at most two. Records: rows under one",
        "collapsible header per level. Groups: one row per level. Variables:",
        "one column per level."
      )
    ),

    rowname = viz_arg(
      type = "column", default = NULL, null = "first column",
      applies = "records",
      gear = gear("Table", "column", "Row names"),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = "Region",
      describe = "The stub column: first column, row label."
    ),

    columns = viz_arg(
      type = "columns", default = NULL, null = "all columns",
      applies = "records",
      gear = gear("Table", "columns", "Columns"),
      slot = FALSE, ctrl = TRUE, ai = TRUE,
      example = c("Revenue", "Profit"),
      describe = "Columns shown beside the row names, in order."
    ),

    summaries = viz_arg(
      type = type_records_by("type", table_summary_specs()), default = list(),
      null = FALSE,
      applies = c("groups", "variables"),
      gear = gear("Columns", "none", "Columns"),
      slot = FALSE, ctrl = TRUE, ai = TRUE,
      example = list(
        list(type = "simple", name = "Subjects", func = "count_distinct",
             col = "USUBJID", show = "bar"),
        list(type = "dist", name = "Duration", col = "DUR",
             summary = "median_q1_q3", show = "box"),
        list(type = "field", name = "Arm", col = "TRT01A")
      ),
      describe = paste(
        "Groups: the table's columns. Variables: its variables. An ordered",
        "list of typed summaries, each `list(type = , ...)`: simple, dist,",
        "field, sparkline, spans, pair, expr, custom. Keys per type: see",
        "table_summary_specs()."
      )
    ),

    id = viz_arg(
      type = "column", default = NULL, null = "count rows",
      applies = "variables",
      gear = gear("Variables", "select", "Individual id"),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = "USUBJID",
      describe = paste(
        "Column whose distinct values are counted instead of rows, so a count",
        "reads as \"subjects with at least one event\"."
      )
    )
  )
}

table_spec_numbers <- function() {
  list(

    percent_of = viz_arg(
      type = "enum", default = "facet", values = c("facet", "total"),
      null = FALSE,
      applies = "groups",
      gear = gear("Numbers", "segmented", "Percent of",
                  labels = c("Facet", "Table")),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = "facet",
      describe = paste(
        "The base of a count's percentage: the rows of the column's own facet",
        "level, or the table."
      )
    ),

    missing = viz_missing_arg(group = "Rows shown", example = "drop"),

    format = viz_format_arg(c("records", "groups")),
    unit = viz_unit_arg(c("records", "groups"))
  )
}

table_spec_color <- function() {
  list(

    color = viz_arg(
      type = "column", default = "auto", also = "auto", null = "no tint",
      applies = "records",
      gear = gear("Color", "column", "Color"),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = "SEX",
      describe = paste(
        "Column whose colors tint the rows, from the board's scale map.",
        "\"auto\" uses the stub column when the scale map has it."
      )
    ),

    shadings = viz_arg(
      type = type_records(spec = table_shading_spec()), default = list(),
      null = FALSE,
      applies = "records",
      gear = gear("Color", "none", "Shade cells"),
      slot = FALSE, ctrl = TRUE, ai = TRUE,
      example = list(list(mode = "diverging", columns = character())),
      describe = paste(
        "Cell color rules, each `list(mode, columns, domain)` with `mode` one",
        "of \"diverging\", \"sequential\", \"bar\". A column belongs to the",
        "first rule that names it."
      )
    )
  )
}

table_spec_look <- function() {
  list(

    look = viz_arg(
      type = "enum", default = "table", values = c("table", "compact", "cards"),
      null = FALSE,
      gear = gear("Look", "tiles", "Look",
                  labels = c("Table", "Compact", "Cards")),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = "cards",
      describe = paste(
        "\"compact\" rotates the column titles and draws `tile` summaries as",
        "square cells (a heatmap). \"cards\" draws one card per summary",
        "(groups) or per column (records)."
      )
    ),

    facet_layout = viz_arg(
      type = "enum", default = "by_summary", values = c("by_summary", "by_level"),
      null = FALSE,
      applies = "groups",
      gear = gear("Look", "segmented", "Facet layout",
                  labels = c("By summary", "By level")),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = "by_summary",
      describe = paste(
        "Column order of faceted summaries. \"by_level\" needs exactly one",
        "facet column in the table."
      )
    ),

    collapsible = viz_arg(
      type = "bool", default = TRUE, null = FALSE,
      applies = c("records", "variables"),
      gear = gear("Look", "segmented", "Collapse groups",
                  labels = c("Collapsible", "Not collapsible")),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      describe = "Allow collapsing the group rows of a nested table."
    )
  )
}

table_spec_sorting <- function() {
  list(

    sort_by = viz_arg(
      type = "string", default = "value", null = FALSE,
      applies = "groups",
      gear = gear("Order", "select", "Sort"),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = "value",
      describe = paste(
        "Row order: \"value\", \"data\", \"alpha\", a summary's name, a facet",
        "level, or a numeric column (its per-row minimum)."
      )
    ),

    sort_dir = viz_arg(
      type = "enum", default = NULL, values = c("asc", "desc"),
      null = "desc for sort_by = \"value\", else asc",
      applies = "groups",
      gear = gear("Order", "segmented", "Order",
                  labels = c("Ascending", "Descending")),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = "desc",
      describe = "Sort direction."
    ),

    top_n = viz_arg(
      type = "integer", default = NULL, bounds = bounds(1), null = "all rows",
      applies = "groups",
      gear = gear("Rows shown", "number", "Top"),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = 10L,
      describe = "Rows past the cut fold into one \"Other\" row."
    )
  )
}

table_spec_bars <- function() {
  list(

    value_labels = viz_arg(
      type = "bool", default = TRUE, null = FALSE,
      applies = "groups",
      gear = gear("Bars", "segmented", "Values",
                  labels = c("Show values", "Off")),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = TRUE,
      describe = "TRUE writes each mark's number beside it."
    ),

    column_axis = viz_arg(
      type = "bool", default = TRUE, null = FALSE,
      applies = "groups",
      gear = gear("Bars", "segmented", "Column axis",
                  labels = c("Column axis", "No column axis")),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = TRUE,
      describe = "TRUE draws the domain strip under each glyph column."
    ),

    bar_width = viz_arg(
      type = "enum", default = "fit", values = c("fit", "narrow", "medium", "wide"),
      null = FALSE,
      applies = "groups",
      gear = gear("Bars", "segmented", "Bar width",
                  labels = c("Fit", "Narrow", "Medium", "Wide")),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = "fit",
      describe = "Mark length: fit the column, or 90, 150 or 240 px."
    )
  )
}

table_spec_interact <- function() {
  list(

    drill = viz_arg(
      type = "column", default = NULL, also = c("auto", "source"),
      null = "off",
      gear = gear("Drill-down", "toggle", "Filter on"),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = "Region",
      describe = paste(
        "Turns the row click on. Records: the column a click filters on,",
        "\"auto\", or \"source\" (the input's source rows). Groups and",
        "variables: \"auto\"."
      )
    ),

    search = viz_arg(
      type = "bool", default = TRUE, null = FALSE,
      gear = gear("Reader tools", "segmented", "Search",
                  labels = c("Search bar", "No search bar")),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      describe = "Show the search input."
    ),

    sortable = viz_arg(
      type = "bool", default = TRUE, null = FALSE,
      gear = gear("Reader tools", "segmented", "Header sorting",
                  labels = c("Header sorting", "No header sorting")),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      describe = "Allow sorting by the column headers."
    ),

    download = viz_download_arg()
  )
}

table_spec_variables <- function() {
  list(

    stats = viz_arg(
      type = "enums", default = "mean_sd",
      values = c("n", "n_pct", "mean", "sd", "mean_sd", "median",
                 "median_q1_q3", "q1_q3", "min_max"),
      null = FALSE,
      applies = "variables",
      gear = gear("Variables", "columns", "Statistics"),
      slot = FALSE, ctrl = TRUE, ai = TRUE,
      example = "mean_sd",
      describe = "Rows shown for numeric variables, in the order given."
    ),

    sections = viz_arg(
      type = "columns", default = character(), null = FALSE,
      applies = "variables",
      gear = gear("Variables", "columns", "Section by"),
      slot = FALSE, ctrl = TRUE, ai = TRUE,
      example = "AEBODSYS",
      describe = "Outer header rows above the variables."
    ),

    total = viz_arg(
      type = "bool", default = FALSE, null = FALSE,
      applies = "variables",
      gear = gear("Variables", "toggle", "Overall column"),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = TRUE,
      describe = "TRUE adds a column over all levels of `by`."
    ),

    total_label = viz_arg(
      type = "string", default = "Total", null = FALSE,
      applies = "variables",
      gear = gear("Variables", "text", "Label"),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = "Total",
      describe = "Header of the overall column."
    ),

    nest = viz_arg(
      type = "bool", default = FALSE, null = FALSE,
      applies = "variables",
      gear = gear("Variables", "toggle", "Nest hierarchy"),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = FALSE,
      describe = "TRUE nests adjacent categorical hierarchies."
    )
  )
}

# The table spec, in the formal order of new_table_block(). `...` comes
# second, after `layout`. Called by viz_block_spec("table").
table_spec <- function() {
  shared <- viz_shared_args()
  order <- c(
    "layout", "by", "rowname", "columns", "summaries", "id", "percent_of",
    "missing", "color", "shadings", "format", "unit", "look", "facet_layout",
    "sort_by", "sort_dir", "top_n", "value_labels", "column_axis",
    "bar_width", "drill", "search", "sortable", "collapsible", "download",
    "stats", "sections", "total", "total_label", "nest", names(shared)
  )
  args <- c(table_spec_data(), table_spec_numbers(), table_spec_color(),
            table_spec_look(), table_spec_sorting(), table_spec_bars(),
            table_spec_interact(), table_spec_variables(), shared)
  viz_spec("table", args[order])
}
