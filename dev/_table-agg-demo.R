# Phase-2 verification: the table block aggregates in place (group/metric/agg_fn),
# and a row click drills to the group's RAW members downstream.
# Port defaults to 3838, override with BLOCKR_PORT=3939 (e.g. running on the
# host Mac where 3838 is held by the VS Code container forward).
options(shiny.port = as.integer(Sys.getenv("BLOCKR_PORT", "3838")),
        shiny.host = "0.0.0.0")
options(blockr.html_table_preview = TRUE)
options(blockr.dock_is_locked = FALSE)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.viz")

adsl <- safetyData::adam_adsl

board <- new_dock_board(
  blocks = c(
    data = new_static_block(adsl, block_name = "ADaM ADSL"),

    # 1. Single-group COUNT: one row per ARM with n; click a row -> raw subjects.
    by_arm = new_table_block(
      group = "ARM",
      metrics = list(list(agg_fn = "count", cols = list())),
      block_name = "Subjects by arm (grouped count)"),
    by_arm_members = new_table_block(
      rowname = "USUBJID", values = c("SEX", "ARM", "AGE"),
      block_name = "Drilled subjects (raw members)"),

    # 2. Multi-group, MULTI-METRIC: one row per ARM x SEX with mean(AGE),
    #    mean(BMIBL) AND sum(WEIGHTBL) at once; multi-key drill.
    by_arm_sex = new_table_block(
      group = c("ARM", "SEX"),
      metrics = list(
        list(agg_fn = "mean", cols = list("AGE", "BMIBL")),
        list(agg_fn = "sum",  cols = list("WEIGHTBL"))
      ),
      block_name = "Age/BMI mean + weight sum by arm x sex"),
    by_arm_sex_members = new_table_block(
      rowname = "USUBJID", values = c("SEX", "ARM", "AGE"),
      block_name = "Drilled arm x sex cell (raw members)"),

    # 3. Plain RAW table (no group) — must render exactly as before.
    raw = new_table_block(
      rowname = "USUBJID", values = c("SEX", "ARM", "AGE"),
      block_name = "Plain raw table (ungrouped)")
  ),
  links = links(
    from = c("data", "by_arm", "data", "by_arm_sex", "data"),
    to   = c("by_arm", "by_arm_members", "by_arm_sex", "by_arm_sex_members", "raw")
  ),
  layouts = list(
    count = dock_layout("by_arm", "by_arm_members", name = "1. Grouped count + drill"),
    mean  = dock_layout("by_arm_sex", "by_arm_sex_members", name = "2. Multi-group mean + drill"),
    raw   = dock_layout("raw", name = "3. Plain raw table")
  ),
  options = dock_board_options(),
  active = "mean"
)

serve(board)
