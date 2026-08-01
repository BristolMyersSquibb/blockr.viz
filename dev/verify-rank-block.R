# Verification board for new_rank_block(): the five bar shapes side by side on
# real ADaM data (safetyData ADAE / ADSL), plus the drill wired to a table so a
# row click is visibly filtering downstream.
#
#   BLOCKR_PORT=3838 R -q -f dev/verify-rank-block.R
#
# What to check:
#   1. Rank        -- ranked bars, search narrows, click a numeric header sorts,
#                     click a row filters the table below it.
#   2. Hierarchy   -- SOC rows collapsed; the caret expands; searching a
#                     preferred term auto-expands its class.
#   3. Colour split -- severity segments, one hue stepped; legend present.
#   4. Facet       -- one bar column per arm, one shared scale, n (%) per arm.
#   5. Difference  -- zero-centred bars vs placebo, both polarities.
#   6. Chrome      -- chart | rank | table on the same data, for diffing the
#                     gear structure and the header spacing against the two
#                     reference blocks. (Was dev/compare-rank-chrome.R; folded
#                     in so there is ONE app, because only port 3838 is
#                     forwarded out of the devcontainer.)
options(shiny.port = as.integer(Sys.getenv("BLOCKR_PORT", "3838")),
        shiny.host = "0.0.0.0")

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.viz")

adae <- safetyData::adam_adae
adae$AESEV <- factor(adae$AESEV, levels = c("MILD", "MODERATE", "SEVERE"))
adae$TRTA <- factor(adae$TRTA,
                    levels = c("Placebo", "Xanomeline Low Dose",
                               "Xanomeline High Dose"))
attr(adae, "label") <- "Adverse events (ADAE)"

board <- new_dock_board(
  blocks = c(
    data = new_static_block(adae, block_name = "ADaM ADAE"),

    # 1. The CDEx case: most frequent AEs, subjects not events, drill on.
    rank = new_rank_block(
      group = "AEDECOD", func = "count_distinct", id_var = "USUBJID",
      drill = "AEDECOD",
      title = "Most frequent adverse events",
      subtitle = "Subjects with at least one event · N = {n_distinct(USUBJID)}",
      caption = "Source: ADAE",
      block_name = "Rank (drill on)"),
    drilled = new_table_block(block_name = "Drilled events"),

    # 2. Hierarchy: system organ class over preferred term.
    nested = new_rank_block(
      group = "AEDECOD", parent = "AEBODSYS",
      func = "count_distinct", id_var = "USUBJID",
      title = "Adverse events by system organ class",
      caption = "A class is not the sum of its terms — each level counts distinct subjects",
      block_name = "Hierarchy"),

    # 3. Colour split by severity (ordered, so one hue stepped).
    split = new_rank_block(
      group = "AEDECOD", color = "AESEV", func = "count_distinct",
      id_var = "USUBJID", bar_mode = "stacked",
      title = "Adverse events by maximum severity",
      block_name = "Colour split"),

    # 4. Facet: one bar column per treatment arm, shared scale.
    faceted = new_rank_block(
      group = "AEDECOD", facet = "TRTA", func = "count_distinct",
      id_var = "USUBJID", sort_by = "Xanomeline High Dose",
      title = "Adverse events by treatment arm",
      block_name = "Facet by arm"),

    # 6. Chrome references: the two blocks the rank block borrows from.
    chart_ref = new_chart_block(
      chart_type = "bar", group = "AEDECOD", orientation = "horizontal",
      drill = "AEDECOD", title = "Most frequent adverse events",
      block_name = "Chart (reference gear)"),
    tbl_ref = new_table_block(
      title = "Most frequent adverse events",
      block_name = "Table (reference chrome)"),
  ),
  links = links(
    from = c("data", "rank", "data", "data", "data", "data", "data"),
    to   = c("rank", "drilled", "nested", "split", "faceted",
             "chart_ref", "tbl_ref")
  ),
  options = dock_board_options(),
  views = list(
    Rank = dock_view(c("rank", "drilled", "nested", "split")),
    Facet = dock_view("faceted"),
    Chrome = dock_view(c("chart_ref", "rank", "tbl_ref"))
  ),
  grids = list(
    # Nested sub-trees are group() nodes, not nested dock_grid() calls.
    Rank = dock_grid(group("rank", "drilled"), group("nested", "split"),
                     orientation = "horizontal"),
    Facet = dock_grid("faceted"),
    Chrome = dock_grid("chart_ref", "rank", "tbl_ref")
  ),
  active = "Rank"
)

cat("\nServing rank-block verification on http://127.0.0.1:",
    getOption("shiny.port"), "/\n\n", sep = "")
serve(board)
