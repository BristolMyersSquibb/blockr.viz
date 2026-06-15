#' Build arguments metadata for the chart block
#' @noRd
chart_arguments <- function() {
  structure(
    c(
      chart_type = paste0(
        "Chart type. One of \"bar\", \"waterfall\", \"pie\", \"treemap\", ",
        "\"boxplot\", \"radar\" (aggregated \u2014 use group + metric + agg_fn), ",
        "\"scatter\", \"line\" (individual \u2014 use x + y), or \"gantt\" ",
        "(timeline \u2014 use x + xend + y). Default \"bar\". Radar: group levels ",
        "are the spokes, one shape per color level. Waterfall: a bar with a ",
        "cumulative baseline \u2014 each step's metric value is a DELTA and bars ",
        "float from the running total (group = step axis, honored in data ",
        "order). Use for P&L / bridge charts; reshape wide measures with ",
        "pivot_longer to (step, value) upstream first."
      ),
      group = paste0(
        "Column for the categorical axis (aggregated charts). Names a ",
        "data column, never a literal."
      ),
      color = paste0(
        "Column mapped to colour. All families. Names a data column, ",
        "never a literal colour. null for no colour mapping."
      ),
      facet = paste0(
        "Column to facet by \u2014 one small panel per level. Optional."
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
      x = paste0(
        "X-axis column (individual: scatter/line; timeline: interval ",
        "start). Names a data column."
      ),
      y = paste0(
        "Y-axis column (individual: scatter/line; timeline: the lane, ",
        "e.g. USUBJID). Names a data column."
      ),
      series = paste0(
        "Column whose distinct values split rows into separate series ",
        "(individual: one line/scatter group per value; timeline: per-bar ",
        "label). Splits only \u2014 not a colour, not a drill target. ",
        "Independent of color. High cardinality is fine."
      ),
      xend = paste0(
        "Interval end column (timeline only). Rows with no end render as ",
        "a dot at x."
      ),
      label = paste0(
        "Column written as on-mark text. Optional; default null = no ",
        "on-mark text. For pie/treemap, null falls back to `group` ",
        "(a label-less pie is unusable). Label only \u2014 does not affect ",
        "colour, series, or drill."
      ),
      drill = paste0(
        "Drill-down: what a SELECTION (click or brush) filters downstream on. ",
        "Tri-state: null/\"\" = OFF (the chart is a static display \u2014 no filter, ",
        "no hover effect; the default); \"auto\" = ON with the family's natural ",
        "target (aggregated -> the clicked group; radar -> the clicked ",
        "shape's color value; scatter -> the selected ",
        "point's x&y; line -> the clicked series; timeline -> the clicked ",
        "lane); a COLUMN NAME = ON, overriding the natural target to filter on ",
        "that column's value(s) for the selected marks. Click and brush follow ",
        "the same rule (a click is a one-point selection). With \"auto\" on a ",
        "scatter, a brush filters the geometric x/y box."
      ),
      smoother = paste0(
        "Trend overlay for scatter charts. One of \"none\" (default), ",
        "\"lm\" (linear fit) or \"loess\" (local regression)."
      ),
      lo = paste0(
        "Lower error-band column (individual line only). Set together ",
        "with hi to draw a band; numeric only."
      ),
      hi = paste0(
        "Upper error-band column (individual line only). Set together ",
        "with lo to draw a band; numeric only."
      ),
      line_width_mult = paste0(
        "Line width multiplier for line charts (individual only). 1.0\u00d7 ",
        "is the default look; range 0.5\u00d7\u20133.0\u00d7."
      ),
      dot_size_mult = paste0(
        "Marker size multiplier for scatter points and line markers ",
        "(individual only). 1.0\u00d7 is the default; range 0.5\u00d7\u20133.0\u00d7."
      ),
      filter_type = paste0(
        "Runtime filter-transport state. Normally left at default ",
        "\"categorical\"; set by interaction, not at creation."
      ),
      filter_column = paste0(
        "Runtime filter-transport state. The column the last click ",
        "filtered on. Usually null at creation."
      ),
      filter_values = paste0(
        "Runtime filter-transport state. Values kept after the last ",
        "click. Usually null at creation."
      ),
      filter_range = paste0(
        "Runtime filter-transport state for brush/drag on scatter/line ",
        "(x_col, y_col, x_range, y_range). Usually null at creation."
      ),
      sort_by = paste0(
        "Category-axis ordering for aggregated charts. \"value\" ",
        "(default), \"alpha\", or a column name. For timeline: ",
        "\"onset\" (default), \"alpha\", or a column name. Ignored for ",
        "individual (scatter/line) charts."
      ),
      sort_dir = paste0(
        "Direction for `sort_by`. One of \"asc\" or \"desc\". Ignored ",
        "for individual (scatter/line) charts."
      )
    ),
    # Mirror the FULL configurable arg set (every name in the descriptions
    # above) so the model copies the real shape instead of inventing names
    # for the styling / filter-transport slots it can't see here. Omitting
    # them made gpt-class models guess plausible-but-wrong names
    # (trend/line_size/marker_size/filter_mode/brush) -> a rejected add_block
    # and a wasted retry on every chart. Runtime-transport slots show their
    # creation-time defaults.
    examples = list(
      chart_type = "scatter",
      group = "Species",
      color = "Species",
      facet = NULL,
      metric = ".count",
      agg_fn = "count",
      x = "Sepal.Length",
      y = "Sepal.Width",
      series = NULL,
      xend = NULL,
      label = NULL,
      drill = "Species",
      smoother = "none",
      lo = NULL,
      hi = NULL,
      line_width_mult = 1,
      dot_size_mult = 1,
      filter_type = "categorical",
      filter_column = NULL,
      filter_values = NULL,
      filter_range = NULL,
      sort_by = "value",
      sort_dir = "desc"
    ),
    prompt = paste(
      "This block renders an interactive chart that can also act as a",
      "filter. Every aesthetic argument names a DATA COLUMN, never a",
      "literal value (`color=\"ARM\"` maps the ARM column, it is not the",
      "colour red). Roles are orthogonal: `color` only colours, `series`",
      "only splits into series, `label` only writes on-mark text,",
      "position is `x`/`y`/`xend` or `group`.",
      "\n\nInteractivity is explicit and opt-in via `drill`. With `drill`",
      "unset a click does nothing. Set `drill` to the column a click",
      "should filter downstream on: clicking a mark emits a categorical",
      "filter on that column's value(s) for the clicked mark. `color`",
      "and `series` never drive drill.",
      "\n\nThree chart families share the block (an internal detail that",
      "never changes what an argument means):",
      "\n- Aggregated (bar/pie/treemap/boxplot/radar): set `group`,",
      "`agg_fn`, and `metric` (\".count\" for row counts). To make clicking",
      "a bar filter to that group, set `drill=\"<the group column>\"`.",
      "A radar puts the `group` levels on the spokes and draws one shape",
      "per `color` level (each vertex = agg_fn(metric) for that cell);",
      "clicking a shape drills on its `color` value. To compare several",
      "numeric columns as spokes, pivot longer upstream and map the name",
      "column to `group`.",
      "\n- Individual (scatter/line): set `x` and `y`. `series` splits",
      "into one line/group per value (e.g. `series=\"USUBJID\"`).",
      "Brush-drag filters the brushed points: on `drill` if set (categorical",
      "on the drill column's values), else the x/y range.",
      "ALSO set `metric` (e.g. metric=\".count\") even though the",
      "individual family doesn't render it -- the block's state",
      "requires the metric slot to initialize, and omitting it",
      "errors with \"State values 'metric' are not yet",
      "initialized\".",
      "\n- Timeline (gantt): set `x` (start), `xend` (end), `y` (the",
      "lane, e.g. USUBJID). To drill to the clicked entity set `drill`",
      "(e.g. `drill=\"USUBJID\"` to drill the patient lane, or",
      "`drill=\"AEDECOD\"` to drill the term).",
      "\n\nMap common requests:",
      "\n- \"scatter of X vs Y\" -> chart_type=\"scatter\", x=\"X\", y=\"Y\"",
      "\n- \"coloured by Z\" -> color=\"Z\"",
      "\n- \"faceted by Z\" -> facet=\"Z\"",
      "\n- \"bar of counts by X, click filters X\" -> chart_type=\"bar\",",
      "group=\"X\", metric=\".count\", agg_fn=\"count\", drill=\"X\"",
      "\n- \"mean Y by X\" -> chart_type=\"bar\", group=\"X\",",
      "metric=\"Y\", agg_fn=\"mean\"",
      "\n- \"radar / spider of mean Y across X, one shape per Z\" ->",
      "chart_type=\"radar\", group=\"X\", metric=\"Y\", agg_fn=\"mean\",",
      "color=\"Z\". Works best with 3+ group levels and few color levels.",
      "\n- \"waterfall / bridge of value V across steps S\" (e.g. a P&L:",
      "Revenue -> Costs -> Profit) -> chart_type=\"waterfall\", group=\"S\",",
      "metric=\"V\", agg_fn=\"sum\". Each step's value is a DELTA; bars float",
      "from the running cumulative and the step axis is shown in DATA ORDER",
      "(make S an ordered factor, or arrange upstream). Wide measures-as-",
      "columns data must be pivot_longer'd to (step, value) first.",
      "\n- \"mean Y trend over time T by group G\" (e.g. mean",
      "ADAS-Cog over visits by treatment arm) -> requires an",
      "UPSTREAM summarize_block first (group_by=[T,G], compute",
      "mean_Y = mean(Y)). Then on THIS block: chart_type=\"line\",",
      "x=T, y=\"mean_Y\", series=G. There is no aggregated-line",
      "family; raw rows would be drawn in row order and produce",
      "tangles or empty plots.",
      "\n- \"distribution / spread / boxplot of Y by X\" ->",
      "chart_type=\"boxplot\", group=\"X\", metric=\"Y\". A boxplot",
      "shows the SPREAD of `metric` within each `group` \u2014 do NOT set",
      "an aggregating `agg_fn` (mean/median/sum/count); that collapses",
      "each group to one value and the plot renders EMPTY. Use `color`",
      "or `facet` to split the boxes (e.g. by treatment arm).",
      "\n- \"label bars with W\" -> label=\"W\"",
      "\n\nLeave filter_type/filter_column/filter_values/filter_range at",
      "defaults \u2014 they are runtime transport for the emitted filter, not",
      "creation-time config.",
      "\n\nWhen picking `color`, check the data: if all visible rows",
      "share one value the colour channel is wasted \u2014 drop `color` or",
      "pick a column with real variation.",
      "\n\nBar/pie/treemap/boxplot ordering is `sort_by` + `sort_dir` on",
      "THIS block, not an upstream arrange_block. \"value\"+\"desc\" for",
      "top-N by value. For 'top N by value' workflows prefer slice_block",
      "(type=\"max\", order_by=COL, n=N).",
      "\n\nEmpty-plot gotchas (data flows in, no marks drawn):",
      "\n- chart_type=\"line\" with raw (non-summarized) rows: lines",
      "connect rows IN ROW ORDER, often producing tangles or invisible",
      "plots when you actually wanted a mean trajectory. Summarize",
      "upstream first (see the 'mean Y trend over time' pattern above).",
      "\n- color/series pointing at a column missing from this block's",
      "upstream data. ADaM tables vary on treatment columns (adsl has",
      "TRT01A/TRT01P; many other tables have TRTA/TRTP only). Run",
      "describe_block or query_data on the upstream to confirm the",
      "column exists here before referencing it.",
      "\n- Mixing chart families: setting BOTH `group`/`metric` AND",
      "`x`/`y` confuses the renderer. Pick one family per chart_type.",
      "\n- agg_fn=\"mean\"/\"sum\" with a metric column that's all-NA or",
      "non-numeric in scope -> NA bars (invisible). Pre-filter to rows",
      "where the metric is populated, or pick a different metric."
    )
  )
}
