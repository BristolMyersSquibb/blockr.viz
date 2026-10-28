# The chart block's arguments: one entry per public argument of
# new_chart_block(), grouped as in the gear and assembled in formal order by
# chart_spec() at the end. viz_arg() is in R/spec.R, the shared entries in
# R/spec-base.R, old names in R/legacy.R.

# viz_arg() with `slot = TRUE` as the default: most chart arguments are slots.
chart_arg <- function(..., slot = TRUE) viz_arg(..., slot = slot)

SPEC_CHART_TYPES <- c("bar", "histogram", "waterfall", "pie", "treemap",
                      "boxplot", "pointrange", "scatter", "line", "area",
                      "band", "gantt")
SPEC_CHART_AGGREGATED <- c("bar", "waterfall", "pie", "treemap",
                           "boxplot", "pointrange")
SPEC_CHART_INDIVIDUAL <- c("scatter", "line", "area", "band")
SPEC_CHART_NUMBER_TYPES <- c(SPEC_CHART_AGGREGATED, "scatter", "line",
                             "area", "histogram")

chart_spec_mapping <- function() {
  list(
    type = chart_arg(
      type = "enum", default = "bar", values = SPEC_CHART_TYPES,
      gear = gear("Chart type", "tiles", "Chart type"),
      example = "scatter",
      describe = paste(
        "Chart type. Aggregated: bar, waterfall, pie, treemap, boxplot,",
        "pointrange. One mark per row: scatter, line, area, band.",
        "Also histogram and gantt (intervals on lanes)."
      )
    ),

    x = chart_arg(
      type = "column", null = "unset",
      gear = gear("Mapping", "column", "X"),
      example = "Sepal.Length",
      describe = paste(
        "Column on the horizontal axis. For an aggregated type one of x and y",
        "is the category and the other the column `func` aggregates. Gantt:",
        "the interval start. Histogram: the column that is binned."
      )
    ),

    y = chart_arg(
      type = "column", null = "unset",
      applies = except("histogram"),
      gear = gear("Mapping", "column", "Y"),
      example = "Sepal.Width",
      describe = paste(
        "Column on the vertical axis. With `func = \"count\"` set only the",
        "category column. Gantt: the lane. Pie and treemap: the value."
      )
    ),

    func = chart_arg(
      type = "enum", default = "count",
      values = c("count", "count_distinct", "pct_distinct", "mean", "median",
                 "sum", "min", "max", "identity"),
      applies = c("bar", "waterfall", "pie", "treemap"),
      gear = gear("Mapping", "select", "Aggregate",
                  labels = c("Count", "Count distinct", "% of panel", "Mean",
                             "Median", "Sum", "Min", "Max", "None (as is)")),
      example = "count",
      describe = paste(
        "Aggregation of the value column per category. \"identity\" draws",
        "the value as is; \"pct_distinct\" is the share of the panel's",
        "distinct values (see `percent_of`)."
      )
    ),

    color = chart_arg(
      type = "column", null = "no color",
      applies = except("waterfall", "pie", "treemap"),
      gear = gear("Add mapping", "column", "Color"),
      example = "Species",
      describe = "Column mapped to color, at most 15 levels."
    ),

    facet = chart_arg(
      type = "column", null = "one panel",
      gear = gear("Add mapping", "column", "Facet"),
      describe = "Column to facet by, one small panel per level."
    ),

    series = chart_arg(
      type = "column", null = "no series",
      applies = c(SPEC_CHART_INDIVIDUAL, "gantt"),
      gear = gear("Add mapping", "column", "Series"),
      describe = paste(
        "Column whose values split rows into separate series: one line or",
        "point group per value, the bar label on a gantt."
      )
    ),

    xend = chart_arg(
      type = "column", null = "dots at x",
      applies = "gantt",
      gear = gear("Mapping", "column", "X end"),
      describe = "Interval end. A row without an end is drawn as a dot at `x`."
    ),

    label = chart_arg(
      type = "column", null = "no labels",
      applies = c("bar", "pie", "treemap", "scatter", "line", "area", "gantt"),
      gear = gear("Add mapping", "column", "Label"),
      describe = paste(
        "Column written on the marks. A bar, slice or tile shows it only when",
        "its rows agree on one value."
      )
    ),

    linetype = chart_arg(
      type = "column", null = "all solid",
      applies = c("line", "area"),
      gear = gear("Add mapping", "column", "Dash by"),
      describe = paste(
        "Column whose values set a line's dash: the first value is solid,",
        "the others dashed."
      )
    ),

    highlight = chart_arg(
      type = type_named(type_values(), max = 1L),
      null = "no highlight",
      applies = c("bar", "scatter", "line", "area"),
      gear = gear("Add mapping", "column", "Highlight"),
      slot = FALSE,
      describe = paste(
        "One column and some of its values, as `list(country = \"Switzerland\")`.",
        "Matching marks keep their color; every other mark is gray."
      ),
      example = list(country = "Switzerland")
    ),

    tooltip = chart_arg(
      type = "columns", null = "no extra columns",
      applies = c("bar", SPEC_CHART_INDIVIDUAL, "gantt"),
      gear = gear("Add mapping", "columns", "Tooltip fields"),
      slot = FALSE,
      describe = "Extra columns shown in each mark's tooltip."
    ),

    ymin = chart_arg(
      type = "columns", null = "no interval",
      applies = c("scatter", "line", "area"),
      gear = gear("Marks", "column", "Interval low"),
      slot = FALSE,
      describe = paste(
        "Columns holding the lower end of an interval per row, the first the",
        "outermost. A line draws shaded bands, a scatter error bars."
      )
    ),

    ymax = chart_arg(
      type = "columns", null = "no interval",
      applies = c("scatter", "line", "area"),
      gear = gear("Marks", "column", "Interval high"),
      slot = FALSE,
      describe = "Columns holding the upper end of an interval, as `ymin`."
    )
  )
}

chart_spec_look <- function() {
  list(
    orientation = chart_arg(
      type = "enum", values = c("horizontal", "vertical"),
      null = "inferred",
      applies = c("bar", "boxplot", "pointrange"),
      gear = gear("Look", "segmented", "Orientation",
                  labels = c("Horizontal", "Vertical")),
      describe = paste(
        "Which way a bar, boxplot or pointrange lies. Leave NULL: it is",
        "inferred. Needed only when both columns are non-numeric."
      )
    ),

    position = chart_arg(
      type = "enum", default = "stack", values = c("stack", "dodge", "fill"),
      applies = c("bar", "area", "histogram"),
      gear = gear("Look", "segmented", "Stacking",
                  labels = c("Stacked", "Side by side", "100%"), when = "color"),
      example = "stack",
      describe = "Layout of a color split: stacked, side by side, or to 100%."
    ),

    legend = chart_arg(
      type = "enum", default = "bottom",
      values = c("bottom", "top", "marks", "none"),
      gear = gear("Look", "segmented", "Legend",
                  labels = c("Below", "Above", "On the marks", "None"),
                  when = "color"),
      example = "marks",
      describe = paste(
        "Where the color legend goes. \"marks\" writes the names on the marks",
        "where the type has room, and keeps the legend elsewhere."
      )
    ),

    y_transform = chart_arg(
      type = "enum", default = "identity", values = c("identity", "log10"),
      applies = c("scatter", "line", "band"),
      gear = gear("Look", "segmented", "Y scale", labels = c("Linear", "Log")),
      example = "log10",
      describe = paste(
        "\"log10\" draws a base-10 y axis. Values at or below zero are not",
        "drawn; a note under the chart counts them."
      )
    ),

    x_transform = chart_arg(
      type = "enum", default = "identity", values = c("identity", "log10"),
      applies = "scatter",
      gear = gear("Look", "segmented", "X scale", labels = c("Linear", "Log")),
      example = "log10",
      describe = "As `y_transform`, for the x axis of a scatter."
    ),

    x_ranges = chart_arg(
      type = type_named(type_values()), null = "no ranges",
      applies = c("bar", "scatter", "line", "area"),
      gear = gear("Look", "text", "Shaded", parse = "ranges"),
      slot = FALSE,
      describe = paste(
        "Named stretches of x shaded behind the marks, as",
        "`list(Recession = c(\"2008-01-01\", \"2009-06-30\"))`. Text form:",
        "\"Recession: 2008-01-01 to 2009-06-30; ...\"."
      ),
      example = list(Recession = c("2008-01-01", "2009-06-30"))
    ),

    waterfall_totals = chart_arg(
      type = type_values(of = "x"), null = "no totals",
      applies = "waterfall",
      gear = gear("Look", "values", "Totals"),
      slot = FALSE,
      describe = "Steps of a waterfall drawn as totals, from zero."
    )
  )
}

chart_spec_sorting <- function() {
  list(
    sort_by = chart_arg(
      type = "string", null = "by type",
      applies = c(SPEC_CHART_AGGREGATED, "gantt"),
      gear = gear("Order", "select", "Sort"),
      example = "value",
      describe = paste(
        "Category order: \"value\", \"data\", \"alpha\", or a column (its",
        "per-category minimum). Gantt: \"onset\", \"alpha\" or a column.",
        "NULL: \"data\" for date, numeric and factor categories, else \"value\"."
      )
    ),

    sort_dir = chart_arg(
      type = "enum", values = c("asc", "desc"),
      null = "desc for sort_by = \"value\", else asc",
      applies = c(SPEC_CHART_AGGREGATED, "gantt"),
      gear = gear("Order", "segmented", "Order",
                  labels = c("Ascending", "Descending")),
      example = "desc",
      describe = "Sort direction."
    )
  )
}

chart_spec_numbers <- function() {
  list(
    value_labels = chart_arg(
      type = "bool", default = FALSE,
      applies = c("bar", "waterfall", "histogram"),
      gear = gear("Bars", "segmented", "Values",
                  labels = c("Show values", "Off")),
      example = TRUE,
      describe = paste(
        "TRUE writes each bar's value at its end: a stack its total, a",
        "filled bar each share."
      )
    ),

    format = viz_format_arg(SPEC_CHART_NUMBER_TYPES, example = "compact"),
    unit = viz_unit_arg(SPEC_CHART_NUMBER_TYPES),

    percent_of = chart_arg(
      type = "enum", default = "facet",
      values = c("facet", "category", "color", "total"),
      applies = "bar",
      gear = gear("Numbers", "select", "Percent of",
                  labels = c("Facet", "Category", "Color", "Whole chart")),
      example = "facet",
      describe = paste(
        "The role whose levels a `pct_distinct` denominator is taken",
        "within, or \"total\" for the whole chart."
      )
    ),

    missing = viz_missing_arg(applies = "bar"),

    show_n = chart_arg(
      type = "enum", default = "none",
      values = c("none", "axis", "facet", "both"),
      gear = gear("Counts", "select", "Group counts",
                  labels = c("Off", "On category axis", "On facet labels",
                             "On axis + facets")),
      example = "none",
      describe = "Observation counts in labels, e.g. \"Female (12)\"."
    ),

    id = chart_arg(
      type = "column", null = "count rows",
      gear = gear("Counts", "column", "Individual id"),
      example = "customer_id",
      describe = paste(
        "Column that identifies an individual, counted distinct by the band",
        "and by `show_n`."
      )
    )
  )
}

chart_spec_lines <- function() {
  list(
    y_lines = chart_arg(
      type = "numbers", null = "no lines",
      applies = c("bar", "waterfall", "boxplot", "pointrange", "scatter", "line",
                  "area", "histogram"),
      gear = gear("Reference lines", "text", "Horizontal", parse = "lines"),
      describe = paste(
        "Positions of dashed guide lines across the value axis. Names are",
        "labels. Text form: \"Target: 4; 6\"."
      )
    ),

    x_lines = chart_arg(
      type = type_values(of = "x"), null = "no lines",
      applies = c("bar", "scatter", "line", "area"),
      gear = gear("Reference lines", "text", "Vertical", parse = "lines"),
      describe = paste(
        "Positions of dashed guide lines across x: numbers, dates or",
        "category values. Names are labels."
      )
    ),

    identity_line = chart_arg(
      type = "bool", default = FALSE,
      applies = "scatter",
      gear = gear("Reference lines", "segmented", "Identity",
                  labels = c("Identity line (y = x)", "Off")),
      example = FALSE,
      describe = "TRUE draws a dashed y = x line."
    ),

    ref_min = chart_arg(
      type = "column", null = "no lower limit",
      applies = "band",
      gear = gear("Reference lines", "column", "Reference low"),
      describe = paste(
        "Column holding a lower reference limit per row, drawn as a dashed",
        "line at its median and labelled with its spread."
      )
    ),

    ref_max = chart_arg(
      type = "column", null = "no upper limit",
      applies = "band",
      gear = gear("Reference lines", "column", "Reference high"),
      describe = "Column holding an upper reference limit per row, as `ref_min`."
    )
  )
}

chart_spec_facets <- function() {
  list(
    facet_scales = chart_arg(
      type = "enum", default = "fixed", values = c("fixed", "free", "free_y"),
      gear = gear("Facets", "select", "Panel scales",
                  labels = c("Shared across panels", "Free x and y per panel",
                             "Free y per panel"),
                  when = "facet"),
      example = "fixed",
      describe = paste(
        "\"fixed\" shares one scale across panels, \"free\" lets each panel",
        "size itself, \"free_y\" frees y only (scatter, line and band)."
      )
    ),

    facet_cols = chart_arg(
      type = "integer", default = opt("blockr.viz.facet_cols"),
      bounds = bounds(1), null = "fit the width",
      gear = gear("Facets", "select", "Panel columns", when = "facet"),
      describe = "Panels per row. NULL fits as many as the card is wide."
    )
  )
}

chart_spec_interact <- function() {
  list(
    drill = chart_arg(
      type = "column", also = "auto",
      null = "a click does nothing",
      gear = gear("Drill-down", "toggle", "Filter on"),
      example = "Species",
      describe = paste(
        "Column a click filters downstream on, or \"auto\" for the type's",
        "natural target (the clicked group, bin or lane)."
      )
    ),

    download = viz_download_arg(ctrl = FALSE)
  )
}

# The chart spec, in the formal order of new_chart_block(). `...` comes
# second, after `type`. Called by viz_block_spec("chart").
chart_spec <- function() {
  shared <- viz_shared_args()
  order <- c(
    "type", "x", "xend", "y", "func", "color", "facet", "series", "label",
    "orientation", "position", "sort_by", "sort_dir", "value_labels",
    "format", "unit", "y_transform", "x_transform", "tooltip", "drill",
    "line_width", "point_size", "connect", "y_lines", "x_lines", "ymin",
    "ymax", "linetype", "x_ranges", "legend", "highlight", "smoother",
    "identity_line", "box_points", "summary", "whiskers", "connect_centers",
    "band_window", "band_size", "band_min_n", "id", "ref_min", "ref_max",
    "waterfall_totals", "bins", "show_n", "missing", "percent_of",
    "facet_scales", "facet_cols", "download", names(shared)
  )
  args <- c(chart_spec_mapping(), chart_spec_look(), chart_spec_sorting(),
            chart_spec_numbers(), chart_spec_lines(), chart_spec_marks(),
            chart_spec_facets(), chart_spec_interact(), shared)
  viz_spec("chart", args[order])
}
