# Writes tests/testthat/fixtures/table-golden/old-boards.json: a board saved
# by blockr.viz 0.2.223 (main before the table rewrite, commit c79f5fb) with
# the old table, summarize table and summary table blocks. The table
# recordings restore it (helper-table-golden.R, "restore" scenarios).
#
# Needs 0.2.223 in a library of its own, ahead of a current blockr.core:
#
#   git -C blockr.viz worktree add --detach $TMPDIR/vizold c79f5fb
#   R CMD INSTALL -l $TMPDIR/oldlib $TMPDIR/vizold
#   Rscript -e '.libPaths(c("$TMPDIR/oldlib", .libPaths()))' \
#     dev/table-golden-old-boards.R
#
# Run from the package root. The tile board is fixtures/tile-board-main.json,
# saved by the same version.

stopifnot(utils::packageVersion("blockr.viz") == "0.2.223")
library(blockr.core)
library(blockr.viz)

blocks <- list(
  # --- the old table block (records) --------------------------------------
  table_plain = new_table_block(),
  table_full = new_table_block(
    rowname = "cat", value = c("num", "num2", "grp"),
    cell_color = drilldown_table_color("sequential", columns = "num"),
    row_color = "grp", digits = 1, rotate_titles = TRUE,
    excel_download = TRUE, html_download = FALSE, pptx_download = FALSE,
    drill = "cat", filter_column = "cat", filter_values = list("a"),
    sortable = FALSE, collapsible = FALSE, search = FALSE,
    title = "Old table", subtitle = "", caption = "{n} rows",
    max_height = "600px", block_name = "Old records table"
  ),
  table_shadings = new_table_block(
    shadings = list(
      list(mode = "bar", cols = list("num")),
      list(mode = "diverging", cols = list("num2"), domain = list(1, 9))
    ),
    color = ""
  ),
  # In-block aggregation: becomes the groups layout.
  table_group_agg = new_table_block(
    group = "cat",
    summaries = list(list(func = "mean", cols = list("num", "num2")),
                     list(func = "count", cols = list())),
    filter_group_cols = "cat", filter_group_vals = "b"
  ),
  table_agg_total = new_table_block(
    summaries = list(list(func = "sum", cols = list("num")))
  ),
  table_ctrl = new_table_block(drill = "cat", ctrl_target = "vf1",
                               ctrl_table = "adsl"),

  # --- the old summarize table block (groups) ------------------------------
  # The ranked bar: no summaries, the measure in group / value / func.
  summarize_ranked = new_summarize_table_block(
    group = "cat", value = "num", func = "mean", color = "grp",
    bar_mode = "grouped", cols = c("n"), sort_dir = "asc", top_n = 2L,
    title = "Ranked", block_name = "Ranked bar"
  ),
  summarize_ranked_count = new_summarize_table_block(
    group = "cat", parent = "grp", func = "count_distinct", id_var = "id",
    facet = "fac"
  ),
  summarize_ranked_fields = new_summarize_table_block(
    group = "id", value = "num", func = "identity", fields = c("cat", "day")
  ),
  # The summaries form, with the old summary keys and table-level colour
  # and facet.
  summarize_summaries = new_summarize_table_block(
    group = "cat", parent = "grp", color = "fac", bar_mode = "percent",
    summaries = list(
      list(type = "simple", func = "count", show = "bar"),
      list(type = "dist", col = "num", stat = "mean_sd", show = "box"),
      list(type = "pair", from = "num", to = "num2", lo = 2, hi = 8),
      list(type = "field", col = "id")
    ),
    drill = "cat", filter_column = c("grp", "cat"),
    filter_values = c("g1", "a"), axis = FALSE, value_labels = FALSE,
    bar_width = "narrow", facet_layout = "by_level", sort_by = "label",
    download = TRUE, search = FALSE
  ),
  summarize_script = new_summarize_table_block(
    by = "kind",
    summaries = list(list(type = "simple", func = "sum", col = "num",
                          show = "number")),
    script = c("k <- 5", "transform(data, kind = ifelse(num > k, 'big', 'small'))"),
    values = list(k = 6), ctrl_target = "vf2", ctrl_table = "t"
  ),

  # --- the old summary table block (variables) -----------------------------
  summary_flat = new_summary_table_block(
    vars = c("num", "cat"), by = "grp", stats = c("mean_sd", "median_q1_q3"),
    add_overall = TRUE, overall_label = "All", nest_hierarchies = FALSE,
    id_var = "id", block_name = "Table 1"
  ),
  summary_sections = new_summary_table_block(
    vars = "cat", sections = "fac", by = "grp", nest_hierarchies = TRUE
  )
)

board <- new_board(blocks = blocks)
ser <- blockr_ser(board)

# The interim summary table format kept its settings in one `state` blob
# (blockr_deser.summary_table_block). 0.2.223 no longer writes it, so this
# entry is the flat one rewrapped by hand.
interim <- ser$payload$blocks$payload$summary_flat
st <- c("vars", "sections", "by", "stats", "add_overall", "overall_label",
        "indent_details", "nest_hierarchies", "id_var")
interim$payload <- c(list(state = interim$payload[st]),
                     interim$payload[setdiff(names(interim$payload), st)])
ser$payload$blocks$payload$summary_interim <- interim

out <- file.path("tests", "testthat", "fixtures", "table-golden",
                 "old-boards.json")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
writeLines(typedjson::json_write_str(ser), out)
cat("wrote", out, "with", length(ser$payload$blocks$payload), "blocks\n")
