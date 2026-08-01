# Preview: the summarize table's download control in a live block.
#
#   Rscript blockr.viz/dev/pptx-summarize/preview-downloads.R [port]
#
# What the unit tests cannot see: that the control lands in the GEAR ROW (the
# rank script hoists the toolbar there, so the download rides along with the
# search box), that it wears the table block's own download styling, and that
# the <details> menu opens with four writable formats.

options(blockr.tabular_display = blockr.ui::html_table_display)
options(blockr.dock_is_locked = FALSE)
.self <- grep("^--file=", commandArgs(FALSE), value = TRUE)[1]
.ws <- normalizePath(if (is.na(.self)) "." else
  file.path(dirname(sub("^--file=", "", .self)), "..", "..", ".."))
.port <- as.integer(c(commandArgs(TRUE), Sys.getenv("BLOCKR_PORT"), "3838")[1])
options(shiny.port = .port, shiny.host = "0.0.0.0")

pkgload::load_all(file.path(.ws, "blockr.core"))
pkgload::load_all(file.path(.ws, "blockr.ui"))
pkgload::load_all(file.path(.ws, "blockr.dock"))
pkgload::load_all(file.path(.ws, "blockr.viz"))

stopifnot(requireNamespace("pharmaverseadam", quietly = TRUE))

adae <- as.data.frame(pharmaverseadam::adae)
adae <- adae[!is.na(adae$AEBODSYS) & !is.na(adae$AEDECOD), ]
adae$DUR <- adae$AENDY - adae$ASTDY + 1

board <- new_dock_board(
  blocks = c(
    ae = new_static_block(adae, block_name = "ADaM ADAE"),
    tbl = new_summarize_table_block(
      by = "AEBODSYS",
      summaries = list(
        list(type = "simple", func = "count_distinct", col = "USUBJID",
             show = "number", name = "Subjects"),
        list(type = "simple", func = "count_distinct", col = "USUBJID",
             show = "bar", facet = "TRT01A", name = "Subjects with an event"),
        list(type = "dist", col = "DUR", style = "box",
             inner = "median_q1_q3", outer = "p10_p90",
             name = "Duration (days)")
      ),
      sort_by = "value", sort_dir = "desc", top_n = 12,
      download = TRUE,
      title = "Adverse events by system organ class",
      subtitle = "downloads on: xlsx, html, pptx, png",
      block_name = "Downloads on")
  ),
  links = links(from = "ae", to = "tbl"),
  grids = list(Table = dock_grid("tbl")),
  active = "Table"
)

cat("\n  http://127.0.0.1:", .port, "/\n\n", sep = "")
serve(board)
