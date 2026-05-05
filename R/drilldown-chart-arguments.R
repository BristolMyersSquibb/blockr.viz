#' Build arguments metadata for the drill-down chart block
#' @noRd
drilldown_chart_arguments <- function() {
  structure(
    c(
      chart_type = paste0(
        "Chart type. One of \"bar\", \"pie\", \"treemap\", \"boxplot\" ",
        "(aggregated — use group_by + metric + agg_fn) or \"scatter\", ",
        "\"line\" (individual — use x_col + y_col). Default \"bar\"."
      ),
      group_by = paste0(
        "Column to group rows by (aggregated chart types only). ",
        "Click a group in the chart to filter downstream rows to that value."
      ),
      color_by = paste0(
        "Column for color/stack encoding. Works with both chart families. ",
        "null for no color mapping."
      ),
      facet_by = paste0(
        "Column to facet by — one small panel per level. Optional, both families."
      ),
      metric = paste0(
        "Column to aggregate (aggregated charts only). Use \".count\" for ",
        "row counts; otherwise a numeric column name."
      ),
      agg_fn = paste0(
        "Aggregation function for `metric` (aggregated charts only). ",
        "One of \"count\", \"count_distinct\", \"mean\", \"median\", \"sum\". ",
        "Default \"count\"."
      ),
      x_col = paste0(
        "X-axis column (individual chart types: scatter, line). Required for ",
        "scatter and line."
      ),
      y_col = paste0(
        "Y-axis column (individual chart types: scatter, line). Required for ",
        "scatter and line."
      ),
      filter_type = paste0(
        "Filter mode for click/brush interaction. \"categorical\" (aggregated ",
        "charts, click to filter) or \"range\" (individual charts, brush to ",
        "filter). Default \"categorical\"."
      ),
      filter_column = paste0(
        "Currently filtered column (aggregated charts). Usually left null at ",
        "creation — set at runtime when the user clicks a group."
      ),
      filter_values = paste0(
        "Currently filtered values (aggregated charts). Array of values kept ",
        "after the user's click. Usually null at creation."
      ),
      filter_range = paste0(
        "Currently brushed range (individual charts). Object with x_col, ",
        "y_col, x_range, y_range. Usually null at creation."
      ),
      sort_by = paste0(
        "Category-axis ordering for aggregated charts (bar/pie/treemap/ ",
        "boxplot). One of \"alpha\" (default — group name, alphabetical), ",
        "\"value\" (the computed metric across stacks), or a column name ",
        "(sorts groups by ascending min of that column). For timeline ",
        "charts: \"onset\" (default, by min of x_col per term), \"alpha\", ",
        "or a column name. Ignored for individual (scatter/line) charts."
      ),
      sort_dir = paste0(
        "Direction for `sort_by`. One of \"asc\" (default) or \"desc\". ",
        "Ignored for individual (scatter/line) charts."
      )
    ),
    examples = list(
      chart_type = "scatter",
      group_by = "Species",
      color_by = "Species",
      facet_by = NULL,
      metric = ".count",
      agg_fn = "count",
      x_col = "Sepal.Length",
      y_col = "Sepal.Width",
      filter_type = "categorical",
      filter_column = "Species",
      filter_values = list("setosa"),
      filter_range = NULL,
      sort_by = "value",
      sort_dir = "desc"
    ),
    prompt = paste(
      "This block renders an interactive chart and also acts as a filter.",
      "Two chart families share the same block:",
      "\n\n**Aggregated** (chart_type bar / pie / treemap / boxplot):",
      "set group_by to the categorical column, agg_fn to the aggregation,",
      "and metric to the column being aggregated (\".count\" for row counts).",
      "Clicking a group filters downstream rows to that value.",
      "\n\n**Individual** (chart_type scatter / line): set x_col and y_col",
      "to the numeric columns. Brush-dragging a region filters to rows in",
      "that x/y range.",
      "\n\nMap common user requests:",
      "\n- \"scatter plot of X vs Y\" -> chart_type=\"scatter\",",
      "x_col=\"X\", y_col=\"Y\"",
      "\n- \"colored by Z\" -> color_by=\"Z\"",
      "\n- \"faceted by Z\" -> facet_by=\"Z\"",
      "\n- \"bar chart of counts by X\" -> chart_type=\"bar\",",
      "group_by=\"X\", metric=\".count\", agg_fn=\"count\"",
      "\n- \"mean Y by X\" -> chart_type=\"bar\", group_by=\"X\",",
      "metric=\"Y\", agg_fn=\"mean\"",
      "\n- \"pie of X\" -> chart_type=\"pie\", group_by=\"X\"",
      "\n- \"boxplot of Y by X\" -> chart_type=\"boxplot\",",
      "group_by=\"X\", metric=\"Y\"",
      "\n\nPrefer this block over ggplot_block for tabular data when the",
      "user will want click/hover/drill-down interaction.",
      "\n\nLeave filter_column, filter_values, and filter_range at their",
      "defaults (null) at creation — they're set at runtime by user clicks",
      "or brushes and cascade to downstream blocks.",
      "\n\nIMPORTANT — when picking color_by, check the data first:",
      "if all rows would share the same value (e.g. coloring by `net`",
      "or `magType` on filtered top-N earthquakes where everything",
      "uses USGS global standard), the chart degenerates to a single",
      "color and the encoding wastes a channel. Drop color_by in that",
      "case, or pick a column with real variation across the rows",
      "the chart will see (preview the upstream block first if",
      "unsure).",
      "\n\n**IMPORTANT — bar/pie/treemap/boxplot ordering is controlled by",
      "this block's `sort_by` + `sort_dir` params, NOT by an upstream",
      "arrange_block.** Defaults are sort_by=\"alpha\" + sort_dir=\"asc\",",
      "i.e. alphabetical by group name. For a 'top N by value' chart,",
      "set sort_by=\"value\", sort_dir=\"desc\" on this block — and DO NOT",
      "add an arrange_block upstream just to order the bars (it has no",
      "effect on the rendered chart). Other allowed sort_by values: a",
      "column name (sorts groups by ascending min of that column), or",
      "\"onset\" for timeline charts.",
      "\n\nFor 'top N by value' workflows, also prefer slice_block with",
      "type=\"max\", order_by=COL, n=N over the arrange + slice(\"head\")",
      "pattern — saves a block."
    )
  )
}
