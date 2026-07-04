# Phase-1 verification: chart multi-metric (bar). `metrics` is the table
# block's shape; each aggregation x column pair renders one bar series per
# group (side-by-side), the legend names the metrics. Non-bar types use the
# first entry. Port pinned via BLOCKR_PORT (container 3838 is the VS Code
# forward -- pick a verified-free port).
options(shiny.port = as.integer(Sys.getenv("BLOCKR_PORT", "3838")),
        shiny.host = "0.0.0.0")

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.viz")

adsl <- safetyData::adam_adsl

board <- new_dock_board(
  blocks = c(
    data = new_static_block(adsl, block_name = "ADaM ADSL"),

    # 1. Multi-metric bar: mean AGE + mean BMIBL side-by-side per ARM.
    mm_bar = new_chart_block(
      chart_type = "bar", group = "ARM",
      metrics = list(list(agg_fn = "mean", cols = c("AGE", "BMIBL"))),
      block_name = "Mean AGE + BMIBL by arm (multi-metric)"),

    # 2. Mixed functions: count + mean AGE (mixed units -> axis "Value").
    mm_mixed = new_chart_block(
      chart_type = "bar", group = "ARM",
      metrics = list(list(agg_fn = "count", cols = list()),
                     list(agg_fn = "mean", cols = "AGE")),
      drill = "auto",
      block_name = "Count + mean AGE by arm (drill on)"),
    mm_members = new_table_block(
      rowname = "USUBJID", values = c("SEX", "ARM", "AGE"),
      block_name = "Drilled subjects"),

    # 3. Regression: single-metric bar exactly as before.
    single = new_chart_block(
      chart_type = "bar", group = "ARM", color = "SEX",
      metric = "AGE", agg_fn = "mean",
      block_name = "Single-metric bar + color (regression)"),

    # 4. Fallback: pie fed the SAME metrics uses the first entry (mean AGE).
    pie_fb = new_chart_block(
      chart_type = "pie", group = "ARM",
      metrics = list(list(agg_fn = "mean", cols = c("AGE", "BMIBL"))),
      block_name = "Pie with metrics (first-entry fallback)")
  ),
  links = links(
    from = c("data", "data", "mm_mixed", "data", "data"),
    to   = c("mm_bar", "mm_mixed", "mm_members", "single", "pie_fb")
  ),
  layouts = list(
    multi  = dock_layout("mm_bar", "mm_mixed", "mm_members",
                         name = "1. Multi-metric"),
    single = dock_layout("single", "pie_fb", name = "2. Regression + fallback")
  ),
  options = dock_board_options(),
  active = "multi"
)

serve(board)
