# Putting it together — how to use blockr.viz (chart, table, tile).
#
# The three per-renderer tours (example-chart / example-table / example-tile)
# show every option of one block. THIS board is the opposite: the idiomatic
# workflow a blockr.viz user follows — shape the data, pick the right renderer,
# wire them together. blockr.dock arranges; blockr.viz draws. One clinical
# dataset (safetyData ADSL subjects + ADAE adverse events) runs throughout.
#
# Each dock VIEW teaches one habit:
#   1. Tiles    AGGREGATE UPSTREAM, then tile. A tile renders a tidy frame, so
#               summarize() first and feed the result as KPI cards.
#   2. Charts   LET THE CHART AGGREGATE. Hand it raw rows + group/metric/agg_fn
#               and it does the counting/averaging itself (the one renderer that
#               self-aggregates, because a bar IS the sum of a group).
#   3. Tables   RENDER TIDY DATA. A raw frame is a listing; a summary_table()
#               shapes a "Table 1". The SAME shaped frame renders interactively
#               (table) or for print (gt) — one shaper, two renderers.
#   4. Drill    COMBINE THEM. Turn on `drill` and a click on the chart filters
#               the linked table downstream — the signature dashboard pattern.
#
# Run from the workspace root (inside or outside the dev container):
#   Rscript blockr.viz/dev/example-dashboard.R
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
adsl <- safetyData::adam_adsl   # one row per subject
adae <- safetyData::adam_adae   # one row per adverse event

board <- new_dock_board(
  blocks = c(
    subjects = new_static_block(adsl, block_name = "ADSL (subjects)"),
    events   = new_static_block(adae, block_name = "ADAE (adverse events)"),

    # 1. TILES — aggregate upstream with a dplyr summarize, then render the
    #    one-row result as KPI cards. The tile never aggregates; it draws.
    kpi = new_summarize_block(
      state = list(
        summaries = list(
          list(type = "simple", name = "subjects", func = "n",    col = "USUBJID"),
          list(type = "simple", name = "mean_age", func = "mean", col = "AGE"),
          list(type = "simple", name = "mean_bmi", func = "mean", col = "BMIBL")
        )
      ),
      block_name = "Summarize to one KPI row"),
    kpi_tiles = new_tile_block(
      value = c("subjects", "mean_age", "mean_bmi"),
      format = "compact", layout = "cards",
      block_name = "Headline numbers (tiles)"),

    # 2. CHARTS — raw rows in, the chart aggregates. Subjects per arm from ADSL;
    #    AE counts per system-organ-class from raw ADAE (sorted, biggest first).
    arm_chart = new_chart_block(
      chart_type = "bar", group = "TRT01A",
      metric = ".count", agg_fn = "count",
      block_name = "Subjects by treatment (chart counts for you)"),
    soc_chart = new_chart_block(
      chart_type = "bar", group = "AEBODSYS",
      metric = ".count", agg_fn = "count",
      sort_by = "value", sort_dir = "desc",
      block_name = "AE count by system organ class"),

    # 3. TABLES — shape once, render twice. summary_table() emits a tidy "Table
    #    1"; the interactive table and the static gt both render it.
    summ = new_summary_table_block(
      state = list(
        vars = list("AGE", "SEX", "RACE"),
        by = list("TRT01A"),
        add_overall = TRUE
      ),
      block_name = "summary_table (Table 1 shaper)"),
    summ_tbl = new_table_block(block_name = "Table 1 — interactive (table)"),
    gt_pub = new_gt_table_block(
      title = "Table 1. Demographics",
      subtitle = "Safety population",
      block_name = "Table 1 — publication (gt)"),

    # 4. DRILL — the chart filters the table. Clicking a SOC bar emits a filter
    #    that flows down the link; the table shows only those adverse events.
    drill_chart = new_chart_block(
      chart_type = "bar", group = "AEBODSYS", color = "TRT01A",
      metric = ".count", agg_fn = "count", drill = "AEBODSYS",
      sort_by = "value", sort_dir = "desc",
      block_name = "AEs by SOC — click a bar to filter"),
    drill_tbl = new_table_block(
      rowname = "USUBJID", values = c("AEDECOD", "AEBODSYS", "TRT01A"),
      block_name = "Adverse events (drilled)")
  ),
  links = links(
    from = c("subjects", "kpi", "subjects", "events", "subjects", "summ",
             "summ", "events", "drill_chart"),
    to   = c("kpi", "kpi_tiles", "arm_chart", "soc_chart", "summ", "summ_tbl",
             "gt_pub", "drill_chart", "drill_tbl")
  ),
  layouts = list(
    tiles  = dock_layout("kpi_tiles", name = "1. Tiles"),
    charts = dock_layout("arm_chart", "soc_chart", name = "2. Charts"),
    tables = dock_layout("summ_tbl", "gt_pub", name = "3. Tables (+ gt)"),
    drill  = dock_layout("drill_chart", "drill_tbl", name = "4. Drill")
  ),
  options = dock_board_options(),
  active = "tiles",
  extensions = list(blockr.dag::new_dag_extension())
)

serve(board)
