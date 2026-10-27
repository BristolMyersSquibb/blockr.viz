# The chart spec's Marks group: smoother, line and point style, the
# distribution statistics, the band and the histogram bins. Split out of
# spec-chart.R for length; chart_spec() assembles it.

chart_spec_marks <- function() {
  list(

    smoother = viz_arg(
      type = "enum", default = "none", values = c("none", "lm", "loess"),
      null = FALSE,
      applies = "scatter",
      gear = gear("Marks", "select", "Smoother",
                  labels = c("None", "Linear", "Loess")),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = "none",
      describe = "Trend line, fit per color or series group and per panel."
    ),

    connect = viz_arg(
      type = "enum", default = "straight",
      values = c("straight", "monotone", "step_start", "step_middle",
                 "step_end"),
      null = FALSE,
      applies = c("line", "area"),
      gear = gear("Marks", "select", "Line",
                  labels = c("Straight", "Monotone", "Step at start",
                             "Step at middle", "Step at end")),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      describe = "How a line connects its points."
    ),

    line_width = viz_arg(
      type = "number", default = 1, bounds = bounds(0.5, 3), null = FALSE,
      applies = SPEC_CHART_INDIVIDUAL,
      gear = gear("Marks", "slider", "Line width"),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = 1,
      describe = "Line width relative to the default (1)."
    ),

    point_size = viz_arg(
      type = "number", default = 1, bounds = bounds(0.5, 3), null = FALSE,
      applies = SPEC_CHART_INDIVIDUAL,
      gear = gear("Marks", "slider", "Point size"),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = 1,
      describe = "Marker size relative to the default (1)."
    ),

    summary = viz_arg(
      type = "enum", default = NULL, values = SPEC_STATS,
      null = "median_q1_q3 on a boxplot, mean_se on a pointrange",
      applies = c("boxplot", "pointrange", "band"),
      gear = gear("Marks", "select", "Inner range", labels = SPEC_STAT_LABELS),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = "mean_se",
      describe = paste(
        "Statistic of a boxplot's body, a pointrange's interval or a band's",
        "inner range."
      )
    ),

    whiskers = viz_arg(
      type = "enum", default = NULL, values = c("tukey", SPEC_STATS),
      null = "tukey on a boxplot, p10_p90 on a band",
      applies = c("boxplot", "band"),
      gear = gear("Marks", "select", "Outer range",
                  labels = c("Tukey (1.5\u00d7IQR)", SPEC_STAT_LABELS)),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = "tukey",
      describe = "Outer interval: Tukey fences or any `summary` statistic."
    ),

    box_points = viz_arg(
      type = "enum", default = "none", values = c("none", "outliers"),
      null = FALSE,
      applies = c("boxplot", "band"),
      gear = gear("Marks", "segmented", "Points",
                  labels = c("None", "Outliers")),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = "none",
      describe = "\"outliers\" draws the points beyond the whiskers."
    ),

    connect_centers = viz_arg(
      type = "bool", default = FALSE, null = FALSE,
      applies = "pointrange",
      gear = gear("Marks", "segmented", "Centers",
                  labels = c("Connect centers", "Off")),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = TRUE,
      describe = "TRUE joins a pointrange's centers in category order."
    ),

    band_window = viz_arg(
      type = "enum", default = "adaptive", values = c("adaptive", "fixed"),
      null = FALSE,
      applies = "band",
      gear = gear("Marks", "select", "Window",
                  labels = c("Adaptive (by subjects)", "Fixed (x units)")),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = "adaptive",
      describe = paste(
        "\"adaptive\" grows the window at each point until it holds",
        "`band_size` individuals; \"fixed\" uses `band_size` as a half-width",
        "in x units."
      )
    ),

    band_size = viz_arg(
      type = "number", default = 45, bounds = bounds(0, min_open = TRUE), null = FALSE,
      applies = "band",
      gear = gear("Marks", "text", "Subjects per window"),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = 45,
      describe = "Individuals per window, or the half-width; see `band_window`."
    ),

    band_min_n = viz_arg(
      type = "integer", default = 12L, bounds = bounds(1), null = FALSE,
      applies = "band",
      gear = gear("Marks", "text", "Cut below n"),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = 12L,
      describe = "Points with fewer individuals than this are not drawn."
    ),

    bins = viz_arg(
      type = "integer", default = 30L, bounds = bounds(1, 200), null = FALSE,
      applies = "histogram",
      gear = gear("Mapping", "number", "Bins"),
      slot = TRUE, ctrl = TRUE, ai = TRUE,
      example = 30L,
      describe = "About how many bins x is cut into, at readable breaks."
    )
  )
}
