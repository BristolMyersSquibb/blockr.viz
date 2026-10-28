# The chart spec's Marks group: smoother, line and point style, the
# distribution statistics, the band and the histogram bins. Split out of
# spec-chart.R for length; chart_spec() assembles it.

chart_spec_marks <- function() {
  list(
    smoother = chart_arg(
      type = "enum", default = "none", values = c("none", "lm", "loess"),
      applies = "scatter",
      gear = gear("Marks", "segmented", "Smoother",
                  labels = c("None", "Linear", "Loess"), icons = TRUE),
      example = "none",
      describe = "Trend line, fit per color or series group and per panel."
    ),

    connect = chart_arg(
      type = "enum", default = "straight",
      values = c("straight", "monotone", "step_start", "step_middle",
                 "step_end"),
      applies = c("line", "area"),
      gear = gear("Marks", "segmented", "Line",
                  labels = c("Straight", "Monotone", "Step at start",
                             "Step at middle", "Step at end"),
                  icons = TRUE),
      describe = "How a line connects its points."
    ),

    line_width = chart_arg(
      type = "number", default = 1, bounds = bounds(0.5, 3),
      applies = SPEC_CHART_INDIVIDUAL,
      gear = gear("Marks", "slider", "Line width"),
      example = 1,
      describe = "Line width relative to the default (1)."
    ),

    point_size = chart_arg(
      type = "number", default = 1, bounds = bounds(0.5, 3),
      applies = SPEC_CHART_INDIVIDUAL,
      gear = gear("Marks", "slider", "Point size"),
      example = 1,
      describe = "Marker size relative to the default (1)."
    ),

    summary = chart_arg(
      type = "enum", values = SPEC_STATS,
      null = "median_q1_q3 on a boxplot, mean_se on a pointrange",
      applies = c("boxplot", "pointrange", "band"),
      gear = gear("Marks", "select", "Inner range", labels = SPEC_STAT_LABELS),
      example = "mean_se",
      describe = paste(
        "Statistic of a boxplot's body, a pointrange's interval or a band's",
        "inner range."
      )
    ),

    whiskers = chart_arg(
      type = "enum", values = c("tukey", SPEC_STATS),
      null = "tukey on a boxplot, p10_p90 on a band",
      applies = c("boxplot", "band"),
      gear = gear("Marks", "select", "Outer range",
                  labels = c("Tukey (1.5\u00d7IQR)", SPEC_STAT_LABELS)),
      example = "tukey",
      describe = "Outer interval: Tukey fences or any `summary` statistic."
    ),

    box_points = chart_arg(
      type = "enum", default = "none", values = c("none", "outliers"),
      applies = c("boxplot", "band"),
      gear = gear("Marks", "segmented", "Points",
                  labels = c("None", "Outliers"), icons = TRUE),
      example = "none",
      describe = "\"outliers\" draws the points beyond the whiskers."
    ),

    connect_centers = chart_arg(
      type = "bool", default = FALSE,
      applies = "pointrange",
      gear = gear("Marks", "segmented", "Centers",
                  labels = c("Connect", "Off")),
      example = TRUE,
      describe = "TRUE joins a pointrange's centers in category order."
    ),

    band_window = chart_arg(
      type = "enum", default = "adaptive", values = c("adaptive", "fixed"),
      applies = "band",
      gear = gear("Marks", "select", "Window",
                  labels = c("Adaptive (by subjects)", "Fixed (x units)")),
      example = "adaptive",
      describe = paste(
        "\"adaptive\" grows the window at each point until it holds",
        "`band_size` individuals; \"fixed\" uses `band_size` as a half-width",
        "in x units."
      )
    ),

    band_size = chart_arg(
      type = "number", default = 45, bounds = bounds(0, min_open = TRUE),
      applies = "band",
      gear = gear("Marks", "text", "Subjects per window"),
      example = 45,
      describe = "Individuals per window, or the half-width; see `band_window`."
    ),

    band_min_n = chart_arg(
      type = "integer", default = 12L, bounds = bounds(1),
      applies = "band",
      gear = gear("Marks", "text", "Cut below n"),
      example = 12L,
      describe = "Points with fewer individuals than this are not drawn."
    ),

    bins = chart_arg(
      type = "integer", default = 30L, bounds = bounds(1, 200),
      applies = "histogram",
      gear = gear("Mapping", "number", "Bins"),
      example = 30L,
      describe = "About how many bins x is cut into, at readable breaks."
    )
  )
}
