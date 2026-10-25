#' Build arguments metadata for the chart block
#'
#' Mirrors the configurable arg set (every name in the descriptions below) so
#' the model copies the real shape instead of inventing names for the
#' styling slots it can't see here. Omitting them made gpt-class models guess
#' plausible-but-wrong names (trend/line_size/marker_size/filter_mode/brush)
#' -> a rejected add_block and a wasted retry on every chart. `script` and
#' `script_values` are left out on purpose (see new_chart_block()), and so is
#' `selection`, which the chart writes itself.
#' @noRd
chart_arguments <- function() {
  new_arg_specs(
    type = new_arg_spec(
      paste0(
        "Chart type. Aggregated (the chart aggregates the rows itself; ",
        "set `x`, `y` and `func`): \"bar\", \"waterfall\", \"pie\", ",
        "\"treemap\", \"boxplot\", \"pointrange\", and \"histogram\" (set only ",
        "`x`, a numeric column, and `bins`). Individual (one mark per ",
        "row; set `x` and `y`): \"scatter\", \"line\", \"band\". Timeline: ",
        "\"gantt\" (set `x`, `xend` and `y`). Default \"bar\". Waterfall: ",
        "each step's value is a DELTA and bars float from the running total ",
        "(the step axis is in data order); reshape wide measures with ",
        "pivot_longer to (step, value) upstream first. Boxplot / ",
        "pointrange: they summarize the raw value column within each ",
        "category in the browser (see `summary`); no upstream summarize ",
        "block, no aggregating func."
      ),
      example = "scatter",
      type = arg_enum(CHART_TYPES)
    ),
    x = new_arg_spec(
      paste0(
        "Column on the horizontal axis. For an aggregated chart either the ",
        "category or the aggregated value, whichever should run along x: ",
        "a vertical bar of counts by class is x=\"class\", a horizontal ",
        "bar of summed sales by region is x=\"sales\", y=\"region\". For ",
        "pie and treemap, x is the slice. For gantt, the interval start. ",
        "Names a data column, never a literal."
      ),
      example = "Sepal.Length",
      type = arg_string()
    ),
    y = new_arg_spec(
      paste0(
        "Column on the vertical axis. For an aggregated chart either the ",
        "category or the aggregated value (see `x`); leave it empty for a ",
        "row count (func=\"count\"). For pie and treemap, the value. For ",
        "gantt, the lane (e.g. a subject id). Names a data column."
      ),
      example = "Sepal.Width",
      type = arg_string()
    ),
    func = new_arg_spec(
      paste0(
        "Aggregation (aggregated charts only), applied to the value column ",
        "per category. One of \"count\", \"count_distinct\", \"mean\", ",
        "\"median\", \"sum\", \"min\", \"max\", \"identity\", ",
        "\"pct_distinct\". Default \"count\" (row count; set only the ",
        "category column). \"count_distinct\" counts distinct values of ",
        "the value column per category, e.g. a subject id to count ",
        "subjects instead of records; with a `color` split an entity under ",
        "several colour levels is counted once per level. \"identity\" ",
        "does NOT aggregate: it plots the value as is (one bar per row when ",
        "the category is unique, for precomputed heights). \"pct_distinct\" ",
        "is the share of the PANEL's distinct values: a facet's own ",
        "population is the denominator. Pair it with missing=\"drop\" when ",
        "the data carries rows that exist only to be counted (a subject ",
        "with no event); they then draw no bar and still count."
      ),
      example = "count",
      # identity and pct_distinct are chart-only, so they widen the chart's
      # enum here rather than the shared AGG_FNS (the table/tile vocabulary,
      # drift-tested against JS).
      type = arg_enum(c(AGG_FNS, "identity", "pct_distinct"))
    ),
    orientation = new_arg_spec(
      paste0(
        "Usually omitted: which way a bar, boxplot or pointrange lies ",
        "follows from `x` and `y` (the category axis holds the non-numeric ",
        "column). Set \"horizontal\" (categories on y) or \"vertical\" ",
        "(categories on x) only when both columns are non-numeric, e.g. a ",
        "distinct count of a character id by a category."
      ),
      example = NULL,
      type = arg_enum(c("horizontal", "vertical"))
    ),
    color = new_arg_spec(
      paste0(
        "Column mapped to colour. All types. Names a data column, never a ",
        "literal colour. null for no colour mapping."
      ),
      example = "Species",
      type = arg_string()
    ),
    facet = new_arg_spec(
      "Column to facet by: one small panel per level. Optional.",
      example = NULL,
      type = arg_string()
    ),
    series = new_arg_spec(
      paste0(
        "Column whose distinct values split rows into separate series ",
        "(scatter/line: one line or point group per value; gantt: per-bar ",
        "label). Splits only: not a colour, not a drill target. ",
        "Independent of color. High cardinality is fine."
      ),
      example = NULL,
      type = arg_string()
    ),
    xend = new_arg_spec(
      paste0(
        "Interval end column (gantt only). Rows with no end render as a ",
        "dot at x."
      ),
      example = NULL,
      type = arg_string()
    ),
    label = new_arg_spec(
      paste0(
        "Column written as on-mark text. Optional; default null = no ",
        "on-mark text. For pie/treemap, null falls back to the slice ",
        "column (a label-less pie is unusable). Label only: does not ",
        "affect colour, series, or drill."
      ),
      example = NULL,
      type = arg_string()
    ),
    format = new_arg_spec(
      paste0(
        "How values are printed on the value axis, in tooltips and value ",
        "labels (aggregated types): \"number\" (default), \"compact\" ",
        "(1.2K, 3.4M) or \"percent\" (a fraction x100 with %). Same as the ",
        "tile block's `format`."
      ),
      example = "compact",
      type = arg_enum(c("number", "compact", "percent"))
    ),
    unit = new_arg_spec(
      paste0(
        "Free-text unit printed after values (e.g. \"CHF\", \"kg\"); the ",
        "axis title carries it once. Never guessed from the data."
      ),
      example = "CHF",
      type = arg_string()
    ),
    tooltip = new_arg_spec(
      paste0(
        "Extra column names appended to each mark's hover tooltip, beyond ",
        "the mapped roles (gantt, scatter, line, bar). On a bar the value ",
        "shown is the category's representative (its first row): exact for ",
        "func=\"identity\" (one row per bar), a representative for an ",
        "aggregation. Display only; a listed column dropped upstream is ",
        "silently omitted. Empty = no extra tooltip fields."
      ),
      example = NULL,
      type = arg_array(arg_string())
    ),
    drill = new_arg_spec(
      paste0(
        "What a SELECTION (click or brush) filters downstream on. ",
        "null/\"\" = OFF (a static display: no filter, no hover effect; the ",
        "default); \"auto\" = ON with the type's natural target (aggregated ",
        "-> the clicked category; scatter -> the selected point's x and y; ",
        "line -> the clicked series; gantt -> the clicked lane); a COLUMN ",
        "NAME = ON, filtering on that column's value(s) for the selected ",
        "marks. Click and brush follow the same rule (a click is a ",
        "one-point selection). With \"auto\" on a scatter, a brush filters ",
        "the x/y box."
      ),
      example = "Species",
      type = arg_string()
    ),
    drill_target = new_arg_spec(
      paste0(
        "BETA. Block id of a value filter block on the SAME board: a ",
        "categorical drill click's claim (e.g. SEX = F) is also pushed into ",
        "that block over the board's control channel, so the drill filters ",
        "a pipeline the chart has no data link to. Requires `drill` to be ",
        "on and the board to carry the control bridge extension. Range / ",
        "point / brush selections are never pushed (a claim is one value). ",
        "Empty (default) = off. \"auto\" needs no id: it is the board's ",
        "drill destination, found by class (the one drill filter block, ",
        "else the one value filter block). Prefer it over a typed id."
      ),
      example = "cohort_filter",
      type = arg_string()
    ),
    drill_table = new_arg_spec(
      paste0(
        "BETA. Only with `drill_target`: the table in the target's dm the ",
        "pushed conditions apply to (e.g. \"adsl\"). Leave empty when the ",
        "target filters a plain data frame."
      ),
      example = "adsl",
      type = arg_string()
    ),
    smoother = new_arg_spec(
      paste0(
        "Trend overlay for scatter charts. One of \"none\" (default), ",
        "\"lm\" (linear fit) or \"loess\" (local regression)."
      ),
      example = "none",
      type = arg_enum(c("none", "lm", "loess"))
    ),
    identity_line = new_arg_spec(
      paste0(
        "Identity line for scatter charts: true draws a dashed 45-degree ",
        "y = x guide line, false (default) omits it. Use for shift or ",
        "agreement plots (e.g. baseline vs post-baseline)."
      ),
      example = FALSE,
      type = arg_boolean()
    ),
    box_points = new_arg_spec(
      paste0(
        "Observation overlay for boxplots: \"none\" (default, box only) or ",
        "\"outliers\" (only the points beyond the whiskers)."
      ),
      example = "none",
      type = arg_enum(c("none", "outliers"))
    ),
    summary = new_arg_spec(
      paste0(
        "Distribution statistic for boxplot and pointrange: the point ",
        "range's interval, or the boxplot's BODY. One of \"median_q1_q3\", ",
        "\"mean_sd\", \"mean_2sd\", \"mean_se\", \"p5_p95\", \"p10_p90\", ",
        "\"min_max\". Computed in the browser from the raw value column per ",
        "category (x `color`): no upstream summarize block, no ymin/ymax ",
        "mapping. Default \"median_q1_q3\" (boxplot), \"mean_se\" ",
        "(pointrange)."
      ),
      example = "mean_se",
      type = arg_enum(c("median_q1_q3", "mean_sd", "mean_2sd", "mean_se",
                        "p5_p95", "p10_p90", "min_max"))
    ),
    whiskers = new_arg_spec(
      paste0(
        "Boxplot whisker rule, the box's OUTER interval: \"tukey\" ",
        "(default, 1.5x IQR fences clipped to the data) or any `summary` ",
        "value, e.g. \"min_max\" for range whiskers or \"p5_p95\" for the ",
        "percentile convention."
      ),
      example = "tukey",
      type = arg_enum(c("tukey", "median_q1_q3", "mean_sd", "mean_2sd",
                        "mean_se", "p5_p95", "p10_p90", "min_max"))
    ),
    connect_centers = new_arg_spec(
      paste0(
        "Pointrange only: true draws a line through the interval centers ",
        "in category order (e.g. mean value by visit per group). Default ",
        "false."
      ),
      example = TRUE,
      type = arg_boolean()
    ),
    band_window = new_arg_spec(
      paste0(
        "Band (type=\"band\") windowing: \"adaptive\" (default) grows the ",
        "window at each grid point until it holds `band_size` distinct ",
        "individuals, keeping the band's reliability constant as the data ",
        "thins; \"fixed\" treats `band_size` as a half-width in x units. ",
        "Prefer adaptive when x is unevenly spaced: a narrow fixed window ",
        "lands in the gaps and tears the band into pieces."
      ),
      example = "adaptive",
      type = arg_enum(c("adaptive", "fixed"))
    ),
    band_size = new_arg_spec(
      paste0(
        "Band window size: distinct individuals per window when ",
        "band_window=\"adaptive\" (default 45), or the half-width in x ",
        "units when \"fixed\"."
      ),
      example = 45,
      type = arg_number()
    ),
    band_min_n = new_arg_spec(
      paste0(
        "Band cut-off: grid points holding fewer than this many distinct ",
        "individuals draw nothing, so the band stops rather than running ",
        "through a stretch with almost no data. Default 12."
      ),
      example = 12,
      type = arg_number()
    ),
    id = new_arg_spec(
      paste0(
        "Column that identifies an individual (a subject, a customer, a ",
        "store). The band counts it DISTINCT per window and carries it on ",
        "an outlier's drill; the N labels (`show_n`) count it DISTINCT. ",
        "Empty (default) counts rows, which over-counts whenever one ",
        "individual contributes several rows."
      ),
      example = "USUBJID",
      type = arg_string()
    ),
    ymin = new_arg_spec(
      paste0(
        "Column holding the lower end of an interval per row, drawn with ",
        "`ymax` as error bars on a scatter or a line (a forest plot is a ",
        "scatter with ymin/ymax). Numeric."
      ),
      example = NULL,
      type = arg_string()
    ),
    ymax = new_arg_spec(
      "Column holding the upper end of the interval. See `ymin`.",
      example = NULL,
      type = arg_string()
    ),
    ref_lo = new_arg_spec(
      paste0(
        "Column holding a LOWER reference limit, drawn as a dashed line. ",
        "Unlike `y_lines`, which takes values, this names a column: a ",
        "reference range can vary by row, so the block reduces it to its ",
        "median and labels the line with the spread it reduced from. Empty ",
        "(default) draws nothing."
      ),
      example = NULL,
      type = arg_string()
    ),
    ref_hi = new_arg_spec(
      "Column holding an UPPER reference limit. See `ref_lo`.",
      example = NULL,
      type = arg_string()
    ),
    show_n = new_arg_spec(
      paste0(
        "Append observation counts to labels, e.g. \"Female (12)\": \"off\" ",
        "(default), \"axis\" (on the category axis of bar and boxplot ",
        "charts), \"facet\" (on the facet labels) or \"both\". Counts ",
        "distinct `id` values when `id` is set, rows otherwise. With ",
        "func=\"identity\" (precomputed bars) the `id` column is shown as ",
        "is, e.g. a precomputed \"N\" column."
      ),
      example = "off",
      type = arg_enum(c("off", "axis", "facet", "both"))
    ),
    missing = new_arg_spec(
      paste0(
        "Rows whose category is missing: \"level\" (default) gives them ",
        "their own category; \"drop\" removes them from the categories but ",
        "NOT from the panel, so they draw nothing and still count toward ",
        "pct_distinct's denominator."
      ),
      example = "level",
      type = arg_enum(c("level", "drop"))
    ),
    pct_of = new_arg_spec(
      paste0(
        "Which mapped role a func=\"pct_distinct\" denominator is taken ",
        "WITHIN: \"facet\" (default), \"category\" or \"color\". It cannot ",
        "be inferred: the chart would have to know whether a column is a ",
        "POPULATION split (arm, sex, country, where a per-level denominator ",
        "is meaningful) or an EVENT attribute (a grade, where the ",
        "denominator would be circular). Worked example: 4 of 10 US ",
        "subjects and 3 of 5 Japanese subjects had an event. Categories by ",
        "country, faceted by event: \"facet\" divides both by all 15 and ",
        "gives 27% and 20%, which is wrong and reverses the ranking; ",
        "\"category\" gives 40% and 60%."
      ),
      example = "facet",
      type = arg_string()
    ),
    facet_scales = new_arg_spec(
      paste0(
        "How the facet panels scale, like ggplot2's facet_wrap(scales=). ",
        "\"fixed\" (default) shares one numeric domain and one category set ",
        "across the panels, so equal bar lengths mean equal values. ",
        "\"free\" lets each panel size itself; use it when the panels do ",
        "not share a unit. \"free_y\" frees the value axis but keeps a ",
        "shared x (scatter/line/band only). No-op without a facet."
      ),
      example = "fixed",
      type = arg_enum(c("fixed", "free_y", "free"))
    ),
    facet_cols = new_arg_spec(
      paste0(
        "How many facet panels sit in a row, like ggplot2's ",
        "facet_wrap(ncol=). null (default) is auto: as many panels per row ",
        "as the card is wide. A number pins the row and the deck render ",
        "follows it. Capped at the panel count. No-op without a facet."
      ),
      example = NULL,
      type = arg_enum(c("1", "2", "3", "4", "6"))
    ),
    connect = new_arg_spec(
      paste0(
        "How a line chart connects its points. \"straight\" (default) draws ",
        "plain segments; \"monotone\" draws smoothed lines with no ",
        "overshoot (the usual look for lab values over time); ",
        "\"step-start\", \"step-middle\", \"step-end\" hold the value ",
        "between observations (dose levels, on/off states)."
      ),
      example = NULL,
      type = arg_enum(
        c("monotone", "straight", "step-start", "step-middle", "step-end")
      )
    ),
    y_lines = new_arg_spec(
      paste0(
        "Helper lines: each number draws one dashed guide line across the ",
        "VALUE axis at that value (a target, a threshold). That is y on ",
        "scatter/line charts and the value axis of bar, waterfall, boxplot ",
        "and pointrange charts either way round. On a percent axis the ",
        "numbers are percentages (50 = half). Plain numbers, never column ",
        "names. Empty = no lines."
      ),
      example = NULL,
      type = arg_array(arg_number())
    ),
    x_lines = new_arg_spec(
      paste0(
        "Helper lines: each number draws one dashed VERTICAL guide line at ",
        "that x position. Plain numbers, never column names. Empty = no ",
        "lines. Scatter/line charts only."
      ),
      example = NULL,
      type = arg_array(arg_number())
    ),
    line_width = new_arg_spec(
      paste0(
        "Line width relative to the default, for line charts. 1 is the ",
        "default look; range 0.5 to 3."
      ),
      example = 1,
      type = arg_number()
    ),
    point_size = new_arg_spec(
      paste0(
        "Marker size relative to the default, for scatter points and line ",
        "markers. 1 is the default; range 0.5 to 3."
      ),
      example = 1,
      type = arg_number()
    ),
    sort_by = new_arg_spec(
      paste0(
        "Category order for aggregated charts: \"value\" (default for ",
        "bar/pie/treemap), \"data\" (default for boxplot/pointrange: the ",
        "data's own order, factor levels when present, else first ",
        "appearance, so visits and dose groups keep their order), ",
        "\"alpha\", or a column name (per-category minimum of that column, ",
        "e.g. a visit number). For gantt: \"onset\" (default), \"alpha\", ",
        "or a column name. Ignored for scatter and line."
      ),
      example = "value",
      type = arg_string()
    ),
    sort_dir = new_arg_spec(
      "Direction for `sort_by`: \"asc\" or \"desc\".",
      example = "desc",
      type = arg_enum(c("asc", "desc"))
    ),
    position = new_arg_spec(
      paste0(
        "Layout of a colour-split bar: \"stack\" (default: segments stack ",
        "into one bar per category), \"dodge\" (segments side by side, for ",
        "comparing absolute values) or \"fill\" (stacked, each category ",
        "normalized to 100%, for comparing composition). No effect without ",
        "a `color` split. Bar charts only."
      ),
      example = "stack",
      type = arg_enum(names(CHART_POSITIONS))
    ),
    value_labels = new_arg_spec(
      paste0(
        "Write each bar's value at its end: true or false (default). A ",
        "stacked bar labels the stack total, a dodged bar every bar, a ",
        "filled bar each segment's share. Overlapping labels are left out; ",
        "the tooltip keeps every value. Bar and waterfall only."
      ),
      example = TRUE,
      type = arg_boolean()
    ),
    bins = new_arg_spec(
      paste0(
        "Histogram only: about how many bins `x` is cut into. The breaks are ",
        "rounded to readable values, so the count can differ a little. ",
        "Default 30."
      ),
      example = 30,
      type = arg_number()
    ),
    waterfall_totals = new_arg_spec(
      paste0(
        "Step values drawn as total/subtotal bars in a waterfall: their ",
        "baseline resets to 0 and they show the absolute running total ",
        "(e.g. [\"Profit\"] in a Revenue -> Costs -> Profit bridge). Empty = ",
        "every bar is a relative delta."
      ),
      example = NULL,
      type = arg_array(arg_string())
    ),
    title = new_arg_spec(
      paste0(
        "Chart title shown above the chart. Unset = auto (the input data ",
        "frame's label attribute when present); \"\" = explicitly no title. ",
        "Supports {...} tokens resolved against the CURRENT data on every ",
        "render, so the title follows upstream filters and pickers: {col} ",
        "= the distinct values of that column collapsed with \", \"; ",
        "{label(col)} = the column's variable label; {n} = row count; ",
        "{n_distinct(col)} = distinct count. Tokens are data lookups, never ",
        "code."
      ),
      example = "Sales by {region}",
      type = arg_string()
    ),
    subtitle = new_arg_spec(
      paste0(
        "Subtitle under the title, same {...} tokens as `title`, and the ",
        "block's control surface. An {@arg} token prints one of this ",
        "block's own settings and makes that word a control: the reader ",
        "clicks it and picks, without opening the gear or unlocking the ",
        "board. {label(@y)} prints the column's label and is still the ",
        "picker for `y`. {@func in mean, median} limits the word's menu to ",
        "the listed choices; it works inside label() too. A clause in [ ] ",
        "disappears when its setting is empty, and the setting is offered ",
        "at the end of the sentence instead. Name the settings a reader ",
        "should steer (colour, facet, which value), not the ones that ",
        "define what the chart IS. Unset = auto (the input data's subtitle ",
        "attribute when present); \"\" = explicitly none."
      ),
      example = "{@func} of {label(@y)} by {@x}[, coloured by {@color}]",
      type = arg_string()
    ),
    caption = new_arg_spec(
      paste0(
        "Caption under the chart (source / footnote line), same {...} ",
        "tokens as `title`. Unset = auto (the input data's caption ",
        "attribute when present); \"\" = explicitly none."
      ),
      example = "N = {n_distinct(id)} customers",
      type = arg_string()
    )
  )
}

#' Construction guidance for the chart block
#' @noRd
chart_guidance <- function() {
  paste(
    "This block renders an interactive chart that can also act as a",
    "filter. Every aesthetic argument names a DATA COLUMN, never a literal",
    "value (`color=\"ARM\"` maps the ARM column, it is not the colour red).",
    "Roles are orthogonal: `color` only colours, `series` only splits into",
    "series, `label` only writes on-mark text.",
    "\n\n`x` and `y` are the screen axes for every type, as in ggplot2.",
    "For an aggregated type (bar, waterfall, pie, treemap, boxplot,",
    "pointrange) one of them is the category and the other the column",
    "`func` aggregates; which way the chart lies follows from that. A row",
    "count needs only the category column: x=\"class\" draws vertical bars,",
    "y=\"class\" horizontal ones. `orientation` is only for two",
    "non-numeric columns (a distinct count of a character id).",
    "\n\nInteractivity is explicit and opt-in via `drill`. With `drill`",
    "unset a click does nothing. Set `drill` to the column a click should",
    "filter downstream on. `color` and `series` never drive drill.",
    "\n\nMap common requests:",
    "\n- \"scatter of X vs Y\" -> type=\"scatter\", x=\"X\", y=\"Y\"",
    "\n- \"shift plot of X vs Y\" (baseline vs post-baseline, agreement)",
    "-> type=\"scatter\", x=\"X\", y=\"Y\", identity_line=true",
    "\n- \"estimates with confidence intervals\" (a forest plot) ->",
    "type=\"scatter\", x=\"estimate\", y=\"term\", or the other way round",
    "with ymin=\"lower\", ymax=\"upper\" for vertical error bars",
    "\n- \"line at x = 3\" -> x_lines=[3]; \"line at y = 2\" or \"target",
    "of 2\" -> y_lines=[2], also on bar/boxplot charts either way round.",
    "\n- \"coloured by Z\" -> color=\"Z\"",
    "\n- \"faceted by Z\" -> facet=\"Z\". The panels share one scale by",
    "default, which is what makes them comparable. Set facet_scales=\"free\"",
    "ONLY when the panels do not share a unit, or when the user asks for",
    "per-panel axes.",
    "\n- \"bar of counts by X, click filters X\" -> type=\"bar\", y=\"X\",",
    "func=\"count\", drill=\"X\" (horizontal; x=\"X\" for vertical)",
    "\n- \"mean Y by X\" -> type=\"bar\", x=\"X\", y=\"Y\", func=\"mean\"",
    "\n- \"number of subjects by X\" -> type=\"bar\", y=\"X\",",
    "x=\"USUBJID\", func=\"count_distinct\", orientation=\"horizontal\"",
    "(both columns are non-numeric, so the orientation is stated)",
    "\n- \"waterfall / bridge of value V across steps S\" (a P&L: Revenue",
    "-> Costs -> Profit) -> type=\"waterfall\", x=\"S\", y=\"V\",",
    "func=\"sum\". Each step's value is a DELTA; the step axis is in DATA",
    "ORDER (make S an ordered factor, or arrange upstream). Wide",
    "measures-as-columns data must be pivot_longer'd to (step, value)",
    "first.",
    "\n- \"mean Y with an interval over visits T by group G\" ->",
    "type=\"pointrange\", x=\"T\", y=\"Y\", color=\"G\", summary=\"mean_se\",",
    "connect_centers=true. NO upstream summarize block: the chart",
    "summarizes the raw rows itself.",
    "\n- \"mean Y trend over time T by group G\" as a plain LINE (no",
    "interval) -> an UPSTREAM summarize block first (group by T and G,",
    "mean_Y = mean(Y)), then type=\"line\", x=\"T\", y=\"mean_Y\",",
    "series=\"G\". Raw rows would be connected in row order. Prefer",
    "pointrange when an interval is wanted.",
    "\n- \"distribution / spread / boxplot of Y by X\" -> type=\"boxplot\",",
    "x=\"X\", y=\"Y\". A boxplot shows the SPREAD of Y within each X; do",
    "NOT set an aggregating func (mean/median/sum/count), which collapses",
    "each category to one value and the plot renders EMPTY. Use `color` or",
    "`facet` to split the boxes. box_points=\"outliers\" flags the points",
    "past the whiskers; `summary` / `whiskers` swap the body / whisker",
    "statistic.",
    "\n- \"distribution of X\" as bars, \"histogram of X\" ->",
    "type=\"histogram\", x=\"X\", bins=30. A click on a bin filters",
    "X to that bin's range.",
    "\n- \"label bars with W\" -> label=\"W\"",
    "\n\nWhen picking `color`, check the data: if all visible rows share one",
    "value the colour channel is wasted; drop `color` or pick a column with",
    "real variation.",
    "\n\nBar/pie/treemap/boxplot/pointrange ordering is `sort_by` +",
    "`sort_dir` on THIS block, not an upstream arrange block. \"value\" +",
    "\"desc\" for top-N by value. For 'top N by value' workflows prefer a",
    "slice block (type=\"max\", order_by=COL, n=N).",
    "\n\nEmpty-plot gotchas (data flows in, no marks drawn):",
    "\n- type=\"line\" with raw (non-summarized) rows: lines connect rows",
    "IN ROW ORDER, often producing tangles when a mean trajectory was",
    "wanted. Summarize upstream first (see above).",
    "\n- color/series naming a column missing from this block's upstream",
    "data. Check the upstream columns before referencing one.",
    "\n- func=\"mean\"/\"sum\" with a value column that is all-NA or",
    "non-numeric in scope -> NA bars (invisible). Pre-filter to rows where",
    "the value is populated, or pick a different column."
  )
}
