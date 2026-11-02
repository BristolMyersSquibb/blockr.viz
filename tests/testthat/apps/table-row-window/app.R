# interactive_table() as a composer block draws it: no height limit, in a
# box that scrolls (the dock panel), refilled by update_interactive_table().
# Fifty sections of twenty rows are more items than the row window draws at
# once (dom.js VIRT_MIN); "Small" swaps in two sections, "Big" the fifty
# again. For the row window test in test-table-sticky-header.R.
#
# Loads blockr.viz from BLOCKR_VIZ_SRC (the source tree under test) when set,
# and blockr.ui from BLOCKR_UI_SRC.

ui_src <- Sys.getenv("BLOCKR_UI_SRC")
if (nzchar(ui_src)) pkgload::load_all(ui_src, quiet = TRUE)
src <- Sys.getenv("BLOCKR_VIZ_SRC")
if (nzchar(src)) pkgload::load_all(src, quiet = TRUE) else library(blockr.viz)
library(shiny)

sections <- function(k) {
  n <- k * 20L
  d <- data.frame(
    .label = sprintf("Row %04d", seq_len(n)),
    .group1_level = rep(sprintf("Group %02d", seq_len(k)), each = 20L),
    Value = as.character(seq_len(n)),
    stringsAsFactors = FALSE
  )
  attr(d$.label, "label") <- "Characteristic"
  d
}

ui <- fluidPage(
  actionButton("small", "Small"),
  actionButton("big", "Big"),
  div(
    class = "panel-box", style = "height: 500px; overflow: auto;",
    interactive_table(sections(50L), drill = FALSE, max_height = NULL,
                      elem_id = "tb")
  )
)

server <- function(input, output, session) {
  observeEvent(input$small, update_interactive_table(session, "tb", sections(2L)))
  observeEvent(input$big, update_interactive_table(session, "tb", sections(50L)))
}

shinyApp(ui, server)
