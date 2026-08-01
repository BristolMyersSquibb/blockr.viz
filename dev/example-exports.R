# Every renderer, every export route, on one board.
#
#   Rscript blockr.viz/dev/example-exports.R [port]
#
# Three render blocks over one ADaM dataset -- a display table, a summarize
# table and a chart -- each with downloads on, plus the slide builder. The
# point is to take the SAME exhibit out twice, from the block's own download
# button and from the deck, and see that they agree.
#
#   block download        deck / slide builder
#   ------------------    --------------------------------------------
#   table       xlsx html pptx      native PowerPoint cells, paged
#   summarize   xlsx html pptx png  painted picture, paged
#   chart       xlsx html pptx png  the ggplot a report compiles
#
# No template is set here, on purpose: both routes fall back to the same
# bundled widescreen deck (blockr.outline's `widescreen-default.pptx`), so the
# block download and the deck slide come out the same size without any board
# configuring anything. Point both at a house deck with
# `options(blockr.outline.template = "...")`.

options(blockr.tabular_display = blockr.ui::html_table_display)
options(blockr.dock_is_locked = FALSE)

.self <- grep("^--file=", commandArgs(FALSE), value = TRUE)[1]
.ws <- normalizePath(if (is.na(.self)) "." else
  file.path(dirname(sub("^--file=", "", .self)), "..", ".."))
.port <- as.integer(c(commandArgs(TRUE), Sys.getenv("BLOCKR_PORT"), "3838")[1])
options(shiny.port = .port, shiny.host = "0.0.0.0")

pkgload::load_all(file.path(.ws, "blockr.core"))
pkgload::load_all(file.path(.ws, "blockr.ui"))
pkgload::load_all(file.path(.ws, "blockr.dock"))
pkgload::load_all(file.path(.ws, "blockr.dag"))
pkgload::load_all(file.path(.ws, "blockr.viz"))
pkgload::load_all(file.path(.ws, "blockr.outline"))

stopifnot(requireNamespace("pharmaverseadam", quietly = TRUE))

adae <- as.data.frame(pharmaverseadam::adae)
adae <- adae[!is.na(adae$AEBODSYS) & !is.na(adae$AEDECOD), ]
adae$DUR <- adae$AENDY - adae$ASTDY + 1

board <- new_dock_board(
  blocks = c(
    ae = new_static_block(adae, block_name = "ADaM ADAE"),

    # 1. The display table: real PowerPoint cells on a slide, so its download
    #    has no image format -- editable text beats a picture of text.
    tbl = new_table_block(
      group = "AEBODSYS",
      summaries = list(
        list(func = "count_distinct", cols = "USUBJID"),
        list(func = "median", cols = "DUR")
      ),
      download = TRUE,
      title = "Adverse events by system organ class",
      subtitle = "display table: xlsx, html, pptx",
      block_name = "1. Table"),

    # 2. The summarize table: glyphs, so a slide gets a painted picture (a
    #    DrawingML cell holds text runs only) and the xlsx gets the numbers
    #    each mark was drawn from.
    smry = new_summarize_table_block(
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
      subtitle = "summarize table: xlsx, html, pptx, png",
      block_name = "2. Summarize table"),

    # 3. The chart: downloads are ON by default, and write the ggplot a
    #    report compiles -- not a capture of the live canvas.
    cht = new_chart_block(
      chart_type = "bar", group = "AEBODSYS", func = "count_distinct",
      value = "USUBJID", color = "TRT01A", bar_mode = "grouped",
      title = "Adverse events by system organ class",
      subtitle = "chart: xlsx, html, pptx, png",
      block_name = "3. Chart")
  ),
  links = links(from = c("ae", "ae", "ae"), to = c("tbl", "smry", "cht")),
  grids = list(
    Exhibits = dock_grid("tbl", "smry", "cht"),
    # The other route to the same exhibits: pick blocks, drag them into
    # order, download the deck.
    Slides = dock_grid("slides"),
    Data = dock_grid("dag_extension")
  ),
  active = "Exhibits",
  extensions = list(
    dag_extension = blockr.dag::new_dag_extension(),
    # Pre-filled with the three exhibits, in the order they appear above, so
    # the deck is one Download away. Drag to reorder, search to add or remove.
    # These slides carry the same exhibits the download buttons write.
    slides = blockr.outline::new_slides_extension(
      slides = c("tbl", "smry", "cht"),
      title = "Adverse events")
  )
)

cat("\n  http://127.0.0.1:", .port, "/\n\n", sep = "")
serve(board)
