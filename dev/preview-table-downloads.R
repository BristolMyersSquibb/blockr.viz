# Table downloads — the toolbar control, in both of its shapes.
#
# The table block can hand its rendered (annotated) frame back as three
# artifacts, each written by the format's own writer over the SAME frame and
# the same resolved title / subtitle / caption:
#
#   Excel (.xlsx)      write_annotated_xlsx()  — indents, sections, spanners
#   Web page (.html)   write_exhibit_html()    — one self-contained file that
#                                                keeps sorting and collapse
#   PowerPoint (.pptx) write_exhibit_pptx()    — one slide, a NATIVE editable
#                                                table on the house template
#
# The two views are the two shapes of the control: one format enabled renders
# a direct download button (the pre-existing behaviour), several render a
# <details> menu. Toggle any of them live in the gear -> Presentation.
#
# Run from the workspace root (inside or outside the dev container):
#   Rscript blockr.viz/dev/preview-table-downloads.R
# then open http://127.0.0.1:3838/

options(blockr.tabular_display = blockr.ui::html_table_display)
options(blockr.dock_is_locked = FALSE)
options(shiny.port = 3838L, shiny.host = "0.0.0.0")

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.theme")
pkgload::load_all("blockr.viz")

stopifnot(requireNamespace("safetyData", quietly = TRUE))
adsl <- safetyData::adam_adsl

board <- new_dock_board(
  blocks = c(
    data = new_static_block(adsl, block_name = "ADaM ADSL"),

    summ = new_summary_table_block(
      vars = list("AGE", "SEX", "RACE"),
      by = list("ARM"),
      add_overall = TRUE,
      block_name = "Demographics by arm"),

    # One format on: the toolbar shows the plain download button, exactly as
    # it did when Excel was the only artifact.
    one = new_table_block(
      excel_download = TRUE,
      title = "Demographic Characteristics",
      subtitle = "Safety population",
      caption = "Percentages are of the column N.",
      block_name = "One format (button)"),

    # All three on: the same 30px control, now opening a menu. Nothing in the
    # toolbar moves when a format is added -- the button just gains a menu.
    all = new_table_block(
      excel_download = TRUE, html_download = TRUE, pptx_download = TRUE,
      title = "Demographic Characteristics",
      subtitle = "Safety population",
      caption = "Percentages are of the column N.",
      block_name = "Three formats (menu)")
  ),
  links = links(
    from = c("data", "summ", "summ"),
    to   = c("summ", "one", "all")
  ),
  views = list(
    menu   = dock_view("all", name = "1. Three formats (menu)"),
    button = dock_view("one", name = "2. One format (button)")
  ),
  options = dock_board_options(),
  active = "menu",
  extensions = list(blockr.dag::new_dag_extension())
)

serve(board)
