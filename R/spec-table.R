# The table block's arguments, grouped as in the gear. table_spec() assembles
# them in the formal order of new_table_block(). `applies` names layouts
# (records, groups, variables). The summary entries and shading rules are in
# R/spec-table-summaries.R.

# viz_arg() with `slot = TRUE` as the default: most table arguments are slots.
table_arg <- function(..., slot = TRUE) viz_arg(..., slot = slot)

TABLE_LAYOUTS <- c("records", "groups", "variables")

table_spec_data <- function() {
  list(
    layout = table_arg(
      type = "enum", default = "records", values = TABLE_LAYOUTS,
      gear = gear("Table", "tiles", "Layout",
                  labels = c("List", "Per group", "Per variable")),
      example = "groups",
      describe = paste(
        "What a row is: \"records\" (the input rows), \"groups\" (one row per",
        "level of `by`, one column per summary) or \"variables\" (a Table 1:",
        "one row per statistic or level, one column per level of `by`)."
      )
    ),

    by = table_arg(
      type = "columns", default = character(),
      gear = gear("Table", "columns", "One row per"),
      example = "TRT01A",
      describe = paste(
        "Grouping columns, outer first, at most two. Records: rows under one",
        "collapsible header per level. Groups: one row per level. Variables:",
        "one column per level."
      )
    ),

    rowname = table_arg(
      type = "column", default = NULL, null = "first column",
      applies = "records",
      gear = gear("Table", "column", "Row names"),
      example = "Region",
      describe = "The stub column: first column, row label."
    ),

    columns = table_arg(
      type = "columns", default = NULL, null = "all columns",
      applies = "records",
      gear = gear("Table", "columns", "Columns"),
      slot = FALSE,
      example = c("Revenue", "Profit"),
      describe = "Columns shown beside the row names, in order."
    ),

    summaries = table_arg(
      type = type_records_by("type", table_summary_specs()), default = list(),
      applies = c("groups", "variables"),
      gear = gear("Columns", "none", "Columns"),
      slot = FALSE,
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
        "field, sparkline, spans, pair, expr, custom. Keys per type: see the",
        "Summaries section of ?new_table_block."
      )
    ),

    id = table_arg(
      type = "column", default = NULL, null = "count rows",
      applies = "variables",
      gear = gear("Variables", "select", "Individual id"),
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
    percent_of = table_arg(
      type = "enum", default = "facet", values = c("facet", "total"),
      applies = "groups",
      gear = gear("Numbers", "segmented", "Percent of",
                  labels = c("Facet", "Table")),
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
    color = table_arg(
      type = "column", default = "auto", also = "auto", null = "no tint",
      applies = "records",
      gear = gear("Color", "column", "Color"),
      example = "SEX",
      describe = paste(
        "Column whose colors tint the rows, from the board's scale map.",
        "\"auto\" uses the stub column when the scale map has it."
      )
    ),

    shadings = table_arg(
      type = type_records(spec = table_shading_spec()), default = list(),
      applies = "records",
      gear = gear("Color", "none", "Shade cells"),
      slot = FALSE,
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
    look = table_arg(
      type = "enum", default = "table", values = c("table", "compact", "cards"),
      gear = gear("Look", "tiles", "Look",
                  labels = c("Table", "Compact", "Cards")),
      example = "cards",
      describe = paste(
        "\"compact\" rotates the column titles and draws `tile` summaries as",
        "square cells (a heatmap). \"cards\" draws one card per summary",
        "(groups) or per column (records)."
      )
    ),

    facet_layout = table_arg(
      type = "enum", default = "by_summary", values = c("by_summary", "by_level"),
      applies = "groups",
      gear = gear("Look", "segmented", "Facet layout",
                  labels = c("By summary", "By level")),
      example = "by_summary",
      describe = paste(
        "Column order of faceted summaries. \"by_level\" needs exactly one",
        "facet column in the table."
      )
    ),

    collapsible = table_arg(
      type = "bool", default = TRUE,
      applies = c("records", "variables"),
      gear = gear("Look", "segmented", "Collapse groups",
                  labels = c("Collapsible", "Not collapsible")),
      describe = "Allow collapsing the group rows of a nested table."
    )
  )
}

table_spec_sorting <- function() {
  list(
    sort_by = table_arg(
      type = "string", default = "value",
      applies = "groups",
      gear = gear("Order", "select", "Sort"),
      example = "value",
      describe = paste(
        "Row order: \"value\", \"data\", \"alpha\", a summary's name, a facet",
        "level, or a numeric column (its per-row minimum)."
      )
    ),

    sort_dir = table_arg(
      type = "enum", default = NULL, values = c("asc", "desc"),
      null = "desc for sort_by = \"value\", else asc",
      applies = "groups",
      gear = gear("Order", "segmented", "Order",
                  labels = c("Ascending", "Descending")),
      example = "desc",
      describe = "Sort direction."
    ),

    top_n = table_arg(
      type = "integer", default = NULL, bounds = bounds(1), null = "all rows",
      applies = "groups",
      gear = gear("Rows shown", "number", "Top"),
      example = 10L,
      describe = "Rows past the cut fold into one \"Other\" row."
    )
  )
}

table_spec_bars <- function() {
  list(
    value_labels = table_arg(
      type = "bool", default = TRUE,
      applies = "groups",
      gear = gear("Bars", "segmented", "Values",
                  labels = c("Show values", "Off")),
      example = TRUE,
      describe = "TRUE writes each mark's number beside it."
    ),

    column_axis = table_arg(
      type = "bool", default = TRUE,
      applies = "groups",
      gear = gear("Bars", "segmented", "Column axis",
                  labels = c("Column axis", "No column axis")),
      example = TRUE,
      describe = "TRUE draws the domain strip under each glyph column."
    ),

    bar_width = table_arg(
      type = "enum", default = "fit", values = c("fit", "narrow", "medium", "wide"),
      applies = "groups",
      gear = gear("Bars", "segmented", "Bar width",
                  labels = c("Fit", "Narrow", "Medium", "Wide")),
      example = "fit",
      describe = "Mark length: fit the column, or 90, 150 or 240 px."
    )
  )
}

table_spec_interact <- function() {
  list(
    drill = table_arg(
      type = "column", default = NULL, also = c("auto", "source"),
      null = "off",
      gear = gear("Drill-down", "toggle", "Filter on"),
      example = "Region",
      describe = paste(
        "Turns the row click on. Records: the column a click filters on,",
        "\"auto\", or \"source\" (the input's source rows). Groups and",
        "variables: \"auto\"."
      )
    ),

    search = table_arg(
      type = "bool", default = TRUE,
      gear = gear("Reader tools", "segmented", "Search",
                  labels = c("Search bar", "No search bar")),
      describe = "Show the search input."
    ),

    sortable = table_arg(
      type = "bool", default = TRUE,
      gear = gear("Reader tools", "segmented", "Header sorting",
                  labels = c("Header sorting", "No header sorting")),
      describe = "Allow sorting by the column headers."
    ),

    download = viz_download_arg()
  )
}

table_spec_variables <- function() {
  list(
    stats = table_arg(
      type = "enums", default = "mean_sd",
      values = c("n", "n_pct", "mean", "sd", "mean_sd", "median",
                 "median_q1_q3", "q1_q3", "min_max"),
      applies = "variables",
      gear = gear("Variables", "columns", "Statistics",
                  labels = c("N", "n (%)", "Mean", "SD", "Mean (SD)",
                             "Median", "Median (Q1, Q3)", "Q1, Q3",
                             "Min, Max")),
      slot = FALSE,
      example = "mean_sd",
      describe = "Rows shown for numeric variables, in the order given."
    ),

    sections = table_arg(
      type = "columns", default = character(),
      applies = "variables",
      gear = gear("Variables", "columns", "Section by"),
      slot = FALSE,
      example = "AEBODSYS",
      describe = "Outer header rows above the variables."
    ),

    total = table_arg(
      type = "bool", default = FALSE,
      applies = "variables",
      gear = gear("Variables", "toggle", "Overall column"),
      example = TRUE,
      describe = "TRUE adds a column over all levels of `by`."
    ),

    total_label = table_arg(
      type = "string", default = "Total",
      applies = "variables",
      gear = gear("Variables", "text", "Label"),
      example = "Total",
      describe = "Header of the overall column."
    ),

    nest = table_arg(
      type = "bool", default = FALSE,
      applies = "variables",
      gear = gear("Variables", "toggle", "Nest hierarchy"),
      example = FALSE,
      describe = "TRUE nests adjacent categorical hierarchies."
    )
  )
}

# The table spec, in the formal order of new_table_block(). `...` is not an
# argument. Called by viz_block_spec("table").
table_spec <- function() {
  shared <- viz_shared_args()
  args <- c(table_spec_data(), table_spec_numbers(), table_spec_color(),
            table_spec_look(), table_spec_sorting(), table_spec_bars(),
            table_spec_interact(), table_spec_variables(), shared)
  order <- setdiff(names(formals(new_table_block)), "...")
  viz_spec("table", args[order])
}
