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
# Downloads are ONE toggle (gear -> Presentation -> Download): on, the table
# offers every format this machine can write -- which file to take is the
# reader's choice. The control's shape follows from what is installed: several
# writable formats render a <details> menu, one renders a direct download
# button (uninstall openxlsx and officer to see that shape). The two views are
# downloads on and off; flip either live in the gear.
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

    # Downloads on: one control on the toolbar, offering every format this
    # machine can write.
    on = new_table_block(
      download = TRUE,
      title = "Demographic Characteristics",
      subtitle = "Safety population",
      caption = "Percentages are of the column N.",
      block_name = "Downloads on"),

    # Off (the default): no control at all -- nothing greyed out, nothing to
    # explain. Switch it on in the gear and the control appears in place.
    off = new_table_block(
      title = "Demographic Characteristics",
      subtitle = "Safety population",
      caption = "Percentages are of the column N.",
      block_name = "Downloads off")
  ),
  links = links(
    from = c("data", "summ", "summ"),
    to   = c("summ", "on", "off")
  ),
  views = list(
    on  = dock_view("on",  name = "1. Downloads on"),
    off = dock_view("off", name = "2. Downloads off")
  ),
  options = dock_board_options(),
  active = "on",
  extensions = list(blockr.dag::new_dag_extension())
)

serve(board)
