# Chart capabilities tour — one renderer, every chart type.
#
# `new_chart_block()` is the universal interactive chart (ECharts). It
# self-aggregates raw data (group + metric + agg_fn) for the aggregated types
# and plots rows directly (x + y) for the individual types. Each dock VIEW (the
# layout switcher, top-right) isolates one capability; the left "Workflow"
# canvas shows how each is wired. Open a chart's gear to see every knob
# (type, roles, sort, orientation, smoother, drill).
#
# Views
#   1. Bar + drill   counts by arm; clicking a bar emits a filter that flows
#                    downstream to a table (the signature drill-to-filter)
#   2. Part-to-whole pie + treemap of category shares
#   3. Waterfall     a cumulative bridge — a bar with baseline = "cumulative"
#                    (chart_type = "waterfall" is sugar for it)
#   4. Distribution  a boxplot of a numeric measure by group
#   5. Scatter+trend x vs y coloured by a category, with a linear trend overlay
#   6. Lines         individual line series (one line per `series` value)
#   7. Radar         group levels on the spokes, one shape per `color` level
#   8. Small mults   the same bar, facetted into one panel per level
#
# Aggregated (bar/waterfall/pie/treemap/boxplot/radar) use group + metric +
# agg_fn; individual (scatter/line) use x + y (+ series); the timeline type
# (gantt) uses x + xend + y and is not shown here. `color` (a categorical hue)
# and `drill` (click-to-filter) are orthogonal roles available throughout.
#
# Run from the workspace root (inside or outside the dev container):
#   Rscript blockr.viz/dev/example-chart.R
# open the local URL serve() prints (or uncomment the options line to pin 3838).

options(blockr.html_table_preview = TRUE)
options(blockr.dock_is_locked = FALSE)
# options(shiny.port = 3838L, shiny.host = "0.0.0.0")  # uncomment to pin

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.viz")

stopifnot(requireNamespace("safetyData", quietly = TRUE))
adsl <- safetyData::adam_adsl

# A waterfall wants a long (step, value) bridge where each value is a delta;
# the running cumulative is what makes it a bridge rather than plain bars.
bridge <- data.frame(
  step  = c("Opening", "New", "Upsell", "Churn", "Contraction"),
  value = c(1200, 480, 260, -210, -90)
)

# Individual line series want one row per (series, x) point — Orange has three
# trees measured over time (age -> circumference).
orange <- datasets::Orange

board <- new_dock_board(
  blocks = c(
    data = new_static_block(adsl, block_name = "ADaM ADSL"),

    # 1. BAR + DRILL — the chart aggregates subjects per arm (coloured by sex);
    #    with drill on, clicking a bar emits a categorical filter that the
    #    linked table applies, so it shows only the drilled subjects.
    bar = new_chart_block(
      chart_type = "bar", group = "ARM", color = "SEX",
      metric = ".count", agg_fn = "count", drill = "ARM",
      block_name = "Subjects by arm x sex (bar + drill)"),
    bar_tbl = new_table_block(
      rowname = "USUBJID", values = c("SEX", "ARM", "AGE", "BMIBL"),
      block_name = "Drilled subjects (downstream of the bar)"),

    # 2. PART-TO-WHOLE — pie and treemap of category shares.
    pie = new_chart_block(
      chart_type = "pie", group = "ARM",
      metric = ".count", agg_fn = "count",
      block_name = "Arm share (pie)"),
    tree = new_chart_block(
      chart_type = "treemap", group = "RACE",
      metric = ".count", agg_fn = "count",
      block_name = "Race share (treemap)"),

    # 3. WATERFALL — a bar with a cumulative baseline; each bar floats from the
    #    running total to draw the bridge.
    bridge_data = new_static_block(bridge, block_name = "Revenue bridge"),
    wf = new_chart_block(
      chart_type = "waterfall", group = "step", metric = "value",
      agg_fn = "sum", block_name = "Cumulative bridge (waterfall)"),

    # 4. DISTRIBUTION — a boxplot of age by arm (the spread, not just the mean).
    box = new_chart_block(
      chart_type = "boxplot", group = "ARM", metric = "AGE", agg_fn = "mean",
      block_name = "Age distribution by arm (boxplot)"),

    # 5. SCATTER + TREND — individual points (age vs weight), coloured by sex,
    #    with a linear smoother overlay.
    scatter = new_chart_block(
      chart_type = "scatter", x = "AGE", y = "WEIGHTBL", color = "SEX",
      smoother = "lm", block_name = "Age vs weight (scatter + trend)"),

    # 6. LINES — one line per series value (one tree each).
    orange_data = new_static_block(orange, block_name = "Orange trees"),
    lines = new_chart_block(
      chart_type = "line", x = "age", y = "circumference", series = "Tree",
      block_name = "Growth over time (line per tree)"),

    # 7. RADAR — RACE levels are the spokes, one shape per ARM, each vertex the
    #    mean age for that race x arm.
    radar = new_chart_block(
      chart_type = "radar", group = "RACE", color = "ARM",
      metric = "AGE", agg_fn = "mean",
      block_name = "Mean age: race (spokes) x arm (shapes)"),

    # 8. SMALL MULTIPLES — the same aggregated bar, facetted one panel per sex.
    facet_bar = new_chart_block(
      chart_type = "bar", group = "AGEGR1", facet = "SEX",
      metric = ".count", agg_fn = "count",
      block_name = "Age group counts, facetted by sex")
  ),
  links = links(
    from = c("data", "bar", "data", "data", "bridge_data", "data",
             "data", "orange_data", "data", "data"),
    to   = c("bar", "bar_tbl", "pie", "tree", "wf", "box",
             "scatter", "lines", "radar", "facet_bar")
  ),
  layouts = list(
    bar_drill    = dock_layout("bar", "bar_tbl", name = "1. Bar + drill"),
    part_whole   = dock_layout("pie", "tree", name = "2. Part-to-whole"),
    waterfall    = dock_layout("wf", name = "3. Waterfall"),
    distribution = dock_layout("box", name = "4. Distribution"),
    scatter      = dock_layout("scatter", name = "5. Scatter + trend"),
    lines        = dock_layout("lines", name = "6. Lines"),
    radar        = dock_layout("radar", name = "7. Radar"),
    small_mults  = dock_layout("facet_bar", name = "8. Small multiples")
  ),
  options = dock_board_options(),
  active = "bar_drill",
  extensions = list(blockr.dag::new_dag_extension())
)

serve(board)
