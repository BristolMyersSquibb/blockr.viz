#' Build arguments metadata for the chart block
#'
#' Mirrors the FULL configurable arg set (every name in the descriptions
#' below) so the model copies the real shape instead of inventing names for
#' the styling / filter-transport slots it can't see here. Omitting them made
#' gpt-class models guess plausible-but-wrong names
#' (trend/line_size/marker_size/filter_mode/brush) -> a rejected add_block and
#' a wasted retry on every chart. Runtime-transport slots show their
#' creation-time defaults.
#' @noRd
chart_arguments <- function() {
  new_block_args(
    chart_type = new_block_arg(
      paste0(
        "Chart type. One of \"bar\", \"waterfall\", \"pie\", \"treemap\", ",
        "\"boxplot\", \"radar\" (aggregated \u2014 use group + value + func), ",
        "\"scatter\", \"line\" (individual \u2014 use x + y), or \"gantt\" ",
        "(timeline \u2014 use x + xend + y). Default \"bar\". Radar: group levels ",
        "are the spokes, one shape per color level. Waterfall: a bar with a ",
        "cumulative baseline \u2014 each step's value value is a DELTA and bars ",
        "float from the running total (group = step axis, honored in data ",
        "order). Use for P&L / bridge charts; reshape wide measures with ",
        "pivot_longer to (step, value) upstream first."
      ),
      example = "scatter",
      type = arg_enum(
        c("bar", "waterfall", "pie", "treemap", "boxplot", "radar",
          "scatter", "line", "gantt")
      )
    ),
    group = new_block_arg(
      paste0(
        "Column for the categorical axis (aggregated charts). Names a ",
        "data column, never a literal."
      ),
      example = "Species",
      type = arg_string()
    ),
    color = new_block_arg(
      paste0(
        "Column mapped to colour. All families. Names a data column, ",
        "never a literal colour. null for no colour mapping."
      ),
      example = "Species",
      type = arg_string()
    ),
    facet = new_block_arg(
      paste0(
        "Column to facet by \u2014 one small panel per level. Optional."
      ),
      example = NULL,
      type = arg_string()
    ),
    value = new_block_arg(
      paste0(
        "Column to aggregate (aggregated charts only). Must match `func`: ",
        "\".count\" with func \"count\" (row counts; the value is ignored ",
        "otherwise), any column with \"count_distinct\" (e.g. a subject id ",
        "such as USUBJID to count patients instead of records), a numeric ",
        "column with the numeric aggregations."
      ),
      example = ".count",
      type = arg_string()
    ),
    func = new_block_arg(
      paste0(
        "Aggregation function for `value` (aggregated charts only). ",
        "One of \"count\", \"count_distinct\", \"mean\", \"median\", ",
        "\"sum\", \"min\", \"max\". Default \"count\" (row count; ignores ",
        "`value`). \"count_distinct\" counts distinct `value` values per ",
        "group -- note that with a `color` split an entity appearing ",
        "under several color levels is counted once per level; deduplicate ",
        "upstream if segments must sum to the per-group distinct count."
      ),
      example = "count",
      type = arg_enum(AGG_FNS)
    ),
    x = new_block_arg(
      paste0(
        "X-axis column (individual: scatter/line; timeline: interval ",
        "start). Names a data column."
      ),
      example = "Sepal.Length",
      type = arg_string()
    ),
    y = new_block_arg(
      paste0(
        "Y-axis column (individual: scatter/line; timeline: the lane, ",
        "e.g. USUBJID). Names a data column."
      ),
      example = "Sepal.Width",
      type = arg_string()
    ),
    series = new_block_arg(
      paste0(
        "Column whose distinct values split rows into separate series ",
        "(individual: one line/scatter group per value; timeline: per-bar ",
        "label). Splits only \u2014 not a colour, not a drill target. ",
        "Independent of color. High cardinality is fine."
      ),
      example = NULL,
      type = arg_string()
    ),
    xend = new_block_arg(
      paste0(
        "Interval end column (timeline only). Rows with no end render as ",
        "a dot at x."
      ),
      example = NULL,
      type = arg_string()
    ),
    label = new_block_arg(
      paste0(
        "Column written as on-mark text. Optional; default null = no ",
        "on-mark text. For pie/treemap, null falls back to `group` ",
        "(a label-less pie is unusable). Label only \u2014 does not affect ",
        "colour, series, or drill."
      ),
      example = NULL,
      type = arg_string()
    ),
    tt_fields = new_block_arg(
      paste0(
        "Extra column names appended to each mark's hover tooltip, beyond ",
        "the mapped roles (timeline/gantt). Display-only \u2014 never affects ",
        "the plot; a listed column dropped upstream is silently omitted. ",
        "Empty = no extra tooltip fields."
      ),
      example = NULL,
      type = arg_array(arg_string())
    ),
    drill = new_block_arg(
      paste0(
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
      example = "Species",
      type = arg_string()
    ),
    ctrl_target = new_block_arg(
      paste0(
        "BETA. Block id of a value filter block on the SAME board: a ",
        "categorical drill click's claim (e.g. SEX = F) is also pushed into ",
        "that block over the board's control channel, so the drill filters ",
        "a pipeline the chart has no data link to. Requires `drill` to be ",
        "on and the board to carry the control bridge extension. Range / ",
        "point / brush selections are never pushed (a claim is one value). ",
        "Empty (default) = off; the drill then behaves exactly as ",
        "documented above."
      ),
      example = "cohort_filter",
      type = arg_string()
    ),
    ctrl_table = new_block_arg(
      paste0(
        "BETA. Only with `ctrl_target`: the table in the target's dm the ",
        "pushed conditions apply to (e.g. \"adsl\"). Leave empty when the ",
        "target filters a plain data frame."
      ),
      example = "adsl",
      type = arg_string()
    ),
    smoother = new_block_arg(
      paste0(
        "Trend overlay for scatter charts. One of \"none\" (default), ",
        "\"lm\" (linear fit) or \"loess\" (local regression)."
      ),
      example = "none",
      type = arg_enum(c("none", "lm", "loess"))
    ),
    identity_line = new_block_arg(
      paste0(
        "Identity-line overlay for scatter charts: \"off\" (default) or ",
        "\"on\" draws a dashed 45-degree y = x guide line. Use for shift ",
        "or agreement plots (e.g. baseline vs post-baseline)."
      ),
      example = "off",
      type = arg_enum(c("off", "on"))
    ),
    box_points = new_block_arg(
      paste0(
        "Observation overlay for boxplots (chart_type=\"boxplot\"): ",
        "\"none\" (default, box only), \"outliers\" (plot only the points ",
        "beyond the 1.5x IQR whiskers) or \"all\" (a jittered strip of ",
        "every observation drawn over the box). Use \"outliers\" to flag ",
        "extreme values, \"all\" to show the full distribution / sample ",
        "size. No-op for non-boxplot charts."
      ),
      example = "none",
      type = arg_enum(c("none", "outliers", "all"))
    ),
    lo = new_block_arg(
      paste0(
        "Lower error-band column (individual line only). Set together ",
        "with hi to draw a band; numeric only."
      ),
      example = NULL,
      type = arg_string()
    ),
    hi = new_block_arg(
      paste0(
        "Upper error-band column (individual line only). Set together ",
        "with lo to draw a band; numeric only."
      ),
      example = NULL,
      type = arg_string()
    ),
    step = new_block_arg(
      paste0(
        "Step-line mode for line charts. null (default) draws straight ",
        "segments; \"start\", \"middle\" or \"end\" draw a stepped line ",
        "(where along the interval the vertical jump happens). Use for ",
        "values that hold between observations (dose levels, states). ",
        "Line charts only."
      ),
      example = NULL,
      type = arg_enum(c("start", "middle", "end"))
    ),
    ref_x = new_block_arg(
      paste0(
        "Numeric reference-line value(s): each entry draws a dashed ",
        "VERTICAL guide line at that x position. LITERAL numbers (e.g. a ",
        "threshold like 5), never column names. Empty = no vertical ",
        "reference line. Scatter/line charts."
      ),
      example = NULL,
      type = arg_array(arg_number())
    ),
    ref_y = new_block_arg(
      paste0(
        "Numeric reference-line value(s): each entry draws a dashed ",
        "HORIZONTAL guide line at that y position (e.g. a normal-range ",
        "limit). LITERAL numbers, never column names. Empty = no ",
        "horizontal reference line. Scatter/line charts."
      ),
      example = NULL,
      type = arg_array(arg_number())
    ),
    line_width_mult = new_block_arg(
      paste0(
        "Line width multiplier for line charts (individual only). 1.0\u00d7 ",
        "is the default look; range 0.5\u00d7\u20133.0\u00d7."
      ),
      example = 1,
      type = arg_number()
    ),
    dot_size_mult = new_block_arg(
      paste0(
        "Marker size multiplier for scatter points and line markers ",
        "(individual only). 1.0\u00d7 is the default; range 0.5\u00d7\u20133.0\u00d7."
      ),
      example = 1,
      type = arg_number()
    ),
    filter_type = new_block_arg(
      paste0(
        "Runtime filter-transport state. Normally left at default ",
        "\"categorical\"; set by interaction, not at creation."
      ),
      example = "categorical",
      type = arg_string()
    ),
    # Runtime filter-transport slots: NULL examples (dropped) and their type
    # varies (column name / value array / range object) -> left untyped.
    filter_column = new_block_arg(
      paste0(
        "Runtime filter-transport state. The column the last click ",
        "filtered on. Usually null at creation."
      ),
      example = NULL
    ),
    filter_values = new_block_arg(
      paste0(
        "Runtime filter-transport state. Values kept after the last ",
        "click. Usually null at creation."
      ),
      example = NULL
    ),
    filter_range = new_block_arg(
      paste0(
        "Runtime filter-transport state for brush/drag on scatter/line ",
        "(x_col, y_col, x_range, y_range). Usually null at creation."
      ),
      example = NULL
    ),
    filter_point = new_block_arg(
      paste0(
        "Runtime filter-transport state for a single-point click on a ",
        "scatter with no drill column (x_col, y_col, x_val, y_val). ",
        "Usually null at creation."
      ),
      example = NULL
    ),
    sort_by = new_block_arg(
      paste0(
        "Category-axis ordering for aggregated charts. \"value\" ",
        "(default), \"alpha\", or a column name. For timeline: ",
        "\"onset\" (default), \"alpha\", or a column name. Ignored for ",
        "individual (scatter/line) charts."
      ),
      example = "value",
      type = arg_string()
    ),
    sort_dir = new_block_arg(
      paste0(
        "Direction for `sort_by`. One of \"asc\" or \"desc\". Ignored ",
        "for individual (scatter/line) charts."
      ),
      example = "desc",
      type = arg_enum(c("asc", "desc"))
    ),
    orientation = new_block_arg(
      paste0(
        "Bar orientation: \"horizontal\" (default; category on the y-axis, ",
        "best for long labels) or \"vertical\". Presentation only \u2014 the ",
        "group/value mapping is unchanged. Bar charts only."
      ),
      example = "horizontal",
      type = arg_enum(c("horizontal", "vertical"))
    ),
    bar_mode = new_block_arg(
      paste0(
        "Layout for a color-split bar: \"stacked\" (default \u2014 color ",
        "segments stack into one bar per group), \"grouped\" (segments sit ",
        "side-by-side / dodged, for comparing absolute values), or ",
        "\"percent\" (stacked but each group normalized to 100%, for ",
        "comparing composition). No effect without a `color` split; ignored ",
        "when baseline=\"cumulative\" (waterfall). Bar charts only."
      ),
      example = "stacked",
      type = arg_enum(c("stacked", "grouped", "percent"))
    ),
    baseline = new_block_arg(
      paste0(
        "Bar baseline mode: \"zero\" (default \u2014 every bar starts at 0) or ",
        "\"cumulative\" (a waterfall/bridge \u2014 each bar floats from the ",
        "running cumulative of the bars before it; each value is a DELTA and ",
        "the step axis honors data order). chart_type=\"waterfall\" implies ",
        "\"cumulative\". Bar family only."
      ),
      example = "zero",
      type = arg_enum(c("zero", "cumulative"))
    ),
    waterfall_totals = new_block_arg(
      paste0(
        "Group (step) values rendered as total/subtotal bars in a ",
        "cumulative-baseline bar: their baseline resets to 0 and they show ",
        "the absolute running cumulative (e.g. [\"Profit\"] in a Revenue -> ",
        "Costs -> Profit bridge). Empty = every bar is a relative delta."
      ),
      example = NULL,
      type = arg_array(arg_string())
    )
  )
}

#' Construction guidance for the chart block
#' @noRd
chart_guidance <- function() {
  paste(
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
      "`func`, and `value` (\".count\" for row counts). To make clicking",
      "a bar filter to that group, set `drill=\"<the group column>\"`.",
      "A radar puts the `group` levels on the spokes and draws one shape",
      "per `color` level (each vertex = func(value) for that cell);",
      "clicking a shape drills on its `color` value. To compare several",
      "numeric columns as spokes, pivot longer upstream and map the name",
      "column to `group`.",
      "\n- Individual (scatter/line): set `x` and `y`. `series` splits",
      "into one line/group per value (e.g. `series=\"USUBJID\"`).",
      "Brush-drag filters the brushed points: on `drill` if set (categorical",
      "on the drill column's values), else the x/y range.",
      "ALSO set `value` (e.g. value=\".count\") even though the",
      "individual family doesn't render it -- the block's state",
      "requires the value slot to initialize, and omitting it",
      "errors with \"State values 'value' are not yet",
      "initialized\".",
      "\n- Timeline (gantt): set `x` (start), `xend` (end), `y` (the",
      "lane, e.g. USUBJID). To drill to the clicked entity set `drill`",
      "(e.g. `drill=\"USUBJID\"` to drill the patient lane, or",
      "`drill=\"AEDECOD\"` to drill the term).",
      "\n\nMap common requests:",
      "\n- \"scatter of X vs Y\" -> chart_type=\"scatter\", x=\"X\", y=\"Y\"",
      "\n- \"shift plot of X vs Y\" (baseline vs post-baseline, agreement)",
      "-> chart_type=\"scatter\", x=\"X\", y=\"Y\", identity_line=\"on\"",
      "\n- \"coloured by Z\" -> color=\"Z\"",
      "\n- \"faceted by Z\" -> facet=\"Z\"",
      "\n- \"bar of counts by X, click filters X\" -> chart_type=\"bar\",",
      "group=\"X\", value=\".count\", func=\"count\", drill=\"X\"",
      "\n- \"mean Y by X\" -> chart_type=\"bar\", group=\"X\",",
      "value=\"Y\", func=\"mean\"",
      "\n- \"radar / spider of mean Y across X, one shape per Z\" ->",
      "chart_type=\"radar\", group=\"X\", value=\"Y\", func=\"mean\",",
      "color=\"Z\". Works best with 3+ group levels and few color levels.",
      "\n- \"waterfall / bridge of value V across steps S\" (e.g. a P&L:",
      "Revenue -> Costs -> Profit) -> chart_type=\"waterfall\", group=\"S\",",
      "value=\"V\", func=\"sum\". Each step's value is a DELTA; bars float",
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
      "chart_type=\"boxplot\", group=\"X\", value=\"Y\". A boxplot",
      "shows the SPREAD of `value` within each `group` \u2014 do NOT set",
      "an aggregating `func` (mean/median/sum/count); that collapses",
      "each group to one value and the plot renders EMPTY. Use `color`",
      "or `facet` to split the boxes (e.g. by treatment arm). To show",
      "the underlying observations set `box_points`: \"outliers\" flags",
      "the extreme points past the whiskers, \"all\" draws a jittered",
      "strip of every point over the box.",
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
      "\n- Mixing chart families: setting BOTH `group`/`value` AND",
      "`x`/`y` confuses the renderer. Pick one family per chart_type.",
      "\n- func=\"mean\"/\"sum\" with a value column that's all-NA or",
      "non-numeric in scope -> NA bars (invisible). Pre-filter to rows",
      "where the value is populated, or pick a different value."
  )
}
