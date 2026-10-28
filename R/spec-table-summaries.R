# The keys of a table summary entry (`summaries`) and of a shading rule
# (`shadings`). The table spec types `summaries` as records by `type`, one
# spec per summary type (table_summary_specs()), and `shadings` as records
# of table_shading_spec(). Functions, so nothing runs while R sources the
# files.

# The color split, taken by simple (bar and dot), dist (not as text), spans
# and pair. In each of those specs rather than in the common keys.
summary_color_arg <- function() {
  viz_arg(
    type = "column", null = "no color split",
    gear = gear("Columns", "column", "Color"),
    describe = "Color split, at most 15 levels."
  )
}

# Keys every summary type takes.
summary_name_arg <- function() {
  viz_arg(
    type = "string", null = "auto (\"Rows\", \"Mean AGE\")",
    gear = gear("Columns", "text", "Name"),
    describe = "Column header."
  )
}

# Every type but field.
summary_facet_arg <- function() {
  viz_arg(
    type = "column", null = "no facet",
    gear = gear("Columns", "column", "Facet"),
    describe = paste(
      "Repeats the column once per level, on one scale. At most 15 levels,",
      "300 for tiles."
    )
  )
}

table_summary_specs <- function() {
  list(

    simple = viz_spec(
      "summary_simple",
      name = summary_name_arg(),
      facet = summary_facet_arg(),
      show = viz_arg(
        type = "enum", default = "bar", values = c("bar", "number", "dot", "tile"),
        gear = gear("Columns", "tiles", "Show",
                    labels = c("Bar", "Number", "Dot", "Tile")),
        describe = "The mark. \"tile\" is a square cell, offered with look = \"compact\"."
      ),
      func = viz_arg(
        type = "enum", default = "count",
        values = c("count", "count_distinct", "sum", "mean", "median", "min",
                   "max", "identity"),
        gear = gear("Columns", "select", "Aggregate",
                    labels = c("Count", "Count distinct", "Sum", "Mean",
                               "Median", "Min", "Max", "None (as is)")),
        describe = "Aggregation per row group. An unknown value is an error."
      ),
      col = viz_arg(
        type = "column", null = "only with func = \"count\"",
        gear = gear("Columns", "column", "Of column"),
        describe = "The column aggregated. Required unless func is \"count\"."
      ),
      position = viz_arg(
        type = "enum", values = c("stack", "dodge", "fill"),
        null = "by the measure",
        gear = gear("Columns", "segmented", "Stacking",
                    labels = c("Stacked", "Side by side", "100%"),
                    when = "color"),
        describe = "Layout of a color split on a bar."
      ),
      fill = viz_arg(
        type = "column", null = "value ramp",
        gear = gear("Columns", "column", "Paint by"),
        describe = paste(
          "Tile only. The cell is painted by the worst level of this column",
          "(factor order, numeric order, else sorted)."
        )
      ),
      color = summary_color_arg()
    ),

    dist = viz_spec(
      "summary_dist",
      name = summary_name_arg(),
      facet = summary_facet_arg(),
      show = viz_arg(
        type = "enum", default = "pointrange",
        values = c("pointrange", "box", "text"),
        gear = gear("Columns", "tiles", "Show",
                    labels = c("Dot range", "Box", "Text")),
        describe = "The mark, or the statistic as text."
      ),
      col = viz_arg(
        type = "column", required = TRUE,
        gear = gear("Columns", "column", "Value"),
        describe = "Numeric column summarized."
      ),
      summary = viz_arg(
        type = "enum", default = "median_q1_q3", values = c(SPEC_STATS, "none"),
        gear = gear("Columns", "select", "Inner range",
                    labels = c(SPEC_STAT_LABELS, "None")),
        describe = paste(
          "Center and inner range, as the chart's `summary`. With show =",
          "\"text\" it is the printed statistic."
        )
      ),
      whiskers = viz_arg(
        type = "enum", values = c("tukey", SPEC_STATS, "none"),
        null = "Tukey on a box, none on a dot range",
        gear = gear("Columns", "select", "Outer range",
                    labels = c("Tukey (1.5\u00d7IQR)", SPEC_STAT_LABELS, "None")),
        describe = "Outer range, as the chart's `whiskers`."
      ),
      color = summary_color_arg()
    ),

    field = viz_spec(
      "summary_field",
      name = summary_name_arg(),
      col = viz_arg(
        type = "column", required = TRUE,
        gear = gear("Columns", "column", "Column"),
        describe = paste(
          "Distinct values joined: up to 3, then \"+n more\"; over 8,",
          "\"n values\"."
        )
      )
    ),

    sparkline = viz_spec(
      "summary_sparkline",
      name = summary_name_arg(),
      facet = summary_facet_arg(),
      x = viz_arg(
        type = "column", required = TRUE,
        gear = gear("Columns", "column", "Order (x)"),
        describe = "Numeric column the line runs along."
      ),
      col = viz_arg(
        type = "column", required = TRUE,
        gear = gear("Columns", "column", "Value (y)"),
        describe = "Numeric column drawn."
      ),
      ymin = viz_arg(
        type = "column", null = "no band",
        gear = gear("Columns", "column", "Band low"),
        describe = "Lower end of a band behind the line. Both or neither."
      ),
      ymax = viz_arg(
        type = "column", null = "no band",
        gear = gear("Columns", "column", "Band high"),
        describe = "Upper end of the band."
      ),
      summary = viz_arg(
        type = "enum", default = "none",
        values = c("none", "mean", "mean_sd", "median_q1_q3"),
        gear = gear("Columns", "select", "Reference",
                    labels = c("None", "Mean", "Mean \u00b1 SD",
                               "Median \u00b7 Q1\u2013Q3")),
        describe = "Reference band behind the line."
      )
    ),

    spans = viz_spec(
      "summary_spans",
      name = summary_name_arg(),
      facet = summary_facet_arg(),
      x = viz_arg(
        type = "column", required = TRUE,
        gear = gear("Columns", "column", "Start"),
        describe = "Interval start, numeric or date, as the chart's gantt."
      ),
      xend = viz_arg(
        type = "column", required = TRUE,
        gear = gear("Columns", "column", "End"),
        describe = "Interval end."
      ),
      label = viz_arg(
        type = "column", null = "no label",
        gear = gear("Columns", "column", "Label"),
        describe = "Headlines the tooltip and keys the same-event highlight."
      ),
      tooltip = viz_arg(
        type = "columns", null = "no extra columns",
        gear = gear("Columns", "columns", "Tooltip fields"),
        describe = "Extra columns in the tooltip."
      ),
      size = viz_arg(
        type = "enum", default = "md", values = c("md", "lg"),
        gear = gear("Columns", "segmented", "Width",
                    labels = c("Regular", "Wide")),
        describe = "Segment thickness."
      ),
      color = summary_color_arg()
    ),

    pair = viz_spec(
      "summary_pair",
      name = summary_name_arg(),
      facet = summary_facet_arg(),
      x = viz_arg(
        type = "column", required = TRUE,
        gear = gear("Columns", "column", "From"),
        describe = "Start value, drawn as a hollow diamond."
      ),
      xend = viz_arg(
        type = "column", required = TRUE,
        gear = gear("Columns", "column", "To"),
        describe = "End value, drawn as a dot."
      ),
      x_func = viz_arg(
        type = "enum", default = "identity",
        values = c("identity", "mean", "median", "min", "max"),
        gear = gear("Columns", "select", "From function",
                    labels = c("None (as is)", "Mean", "Median", "Min", "Max")),
        describe = "How `x` is read per group."
      ),
      xend_func = viz_arg(
        type = "enum", default = "max",
        values = c("identity", "mean", "median", "min", "max"),
        gear = gear("Columns", "select", "To function",
                    labels = c("None (as is)", "Mean", "Median", "Min", "Max")),
        describe = "How `xend` is read per group."
      ),
      ref_min = viz_arg(
        type = "string", null = "no range",
        gear = gear("Columns", "text", "Reference low"),
        describe = paste(
          "A number or a column: the low end of a shaded reference range. An",
          "`xend` outside the range is drawn open."
        )
      ),
      ref_max = viz_arg(
        type = "string", null = "no range",
        gear = gear("Columns", "text", "Reference high"),
        describe = "A number or a column: the high end of the range."
      ),
      x_lines = viz_arg(
        type = "numbers", null = "no line",
        gear = gear("Columns", "text", "Reference line", parse = "lines"),
        describe = "Dashed guide lines, as the chart's `x_lines`."
      ),
      linetype = viz_arg(
        type = "column", null = "all solid",
        gear = gear("Columns", "column", "Dash by"),
        describe = "Non-first levels of this column are drawn dashed."
      ),
      color = summary_color_arg()
    ),

    expr = viz_spec(
      "summary_expr",
      name = summary_name_arg(),
      facet = summary_facet_arg(),
      expr = viz_arg(
        type = "string", required = TRUE,
        gear = gear("Columns", "text", "Expression"),
        describe = paste(
          "R expression evaluated in dplyr::summarise() per row group. A",
          "numeric result is a number cell, anything else text."
        )
      )
    ),

    custom = viz_spec(
      "summary_custom",
      name = summary_name_arg(),
      facet = summary_facet_arg(),
      show = viz_arg(
        type = "enum",
        values = c("bar", "number", "dumbbell", "box", "pointrange", "text"),
        null = "read off the result",
        gear = gear("Columns", "tiles", "Show",
                    labels = c("Bar", "Number", "Dumbbell", "Box", "Dot range",
                               "Text")),
        describe = "The mark; NULL reads it off the result's columns."
      ),
      fn = viz_arg(
        type = "string", required = TRUE,
        gear = gear("Columns", "text", "Summary of d"),
        describe = paste(
          "A function of `d`, the rows grouped by table row. Returns a data",
          "frame or named vector with columns among value, from, to, lo, q1,",
          "mid, q3, hi, text, plus at most one color column."
        )
      ),
      id = viz_arg(
        type = "column", null = "count rows",
        gear = gear("Columns", "column", "Percent of"),
        describe = "A count prints \"n (pct%)\" with N the distinct values of this column."
      )
    )
  )
}

table_shading_spec <- function() {
  viz_spec(
    "shading",

    mode = viz_arg(
      type = "enum", default = "diverging",
      values = c("diverging", "sequential", "bar"),
      gear = gear("Color", "tiles", "Mode",
                  labels = c("Diverging", "Sequential", "Data bar")),
      describe = paste(
        "\"diverging\" centers on 0, \"sequential\" ramps two colors, \"bar\"",
        "draws a data bar in the cell. An unknown mode is an error."
      )
    ),

    columns = viz_arg(
      type = "columns",
      null = "every numeric column no earlier rule claims",
      gear = gear("Color", "columns", "on"),
      describe = "Numeric columns the rule paints."
    ),

    domain = viz_arg(
      type = "numbers",
      null = "the data range; diverging is symmetric around 0",
      describe = "`c(low, high)` the colors span, e.g. `c(-1, 1)` for correlations."
    )
  )
}
