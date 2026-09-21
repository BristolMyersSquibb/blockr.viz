# Panel columns -- how many facet panels sit in a row.
#
# `facet_cols` is unset by default, which is auto: the canvas fits as many
# panels per row as the card is wide, and ggplot2 picks its own square-ish
# grid for the deck. The two used to disagree; one setting now feeds both.
#
# Views
#   1. Auto vs 2  the same six-panel bar twice, once on auto and once pinned
#                 to two per row. Resize the card and only the first moves.
#   2. Sentence   the same chart with "[, in {@facet_cols} columns]" in its
#                 subtitle: on auto the clause is gone and the sentence ends
#                 with a "+ Panel columns" chip, which opens the same menu.
#   3. Deck       pinned to 3; the download writes the ggplot, which carries
#                 the same ncol.
#
# Run from the workspace root:
#   Rscript blockr.viz/dev/facet-cols-demo.R [port]
# Port: argument, else BLOCKR_PORT, else 3838.

port <- suppressWarnings(as.integer(commandArgs(trailingOnly = TRUE)[1]))
if (is.na(port)) {
  port <- as.integer(Sys.getenv("BLOCKR_PORT", "3838"))
}
options(shiny.port = port, shiny.host = "0.0.0.0")
message("http://127.0.0.1:", port)

options(blockr.tabular_display = blockr.ui::html_table_display)
options(blockr.dock_is_locked = FALSE)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.viz")

set.seed(42)
subj <- sprintf("01-%03d", 1:120)
ae <- data.frame(
  USUBJID = subj,
  TRT = rep(c("Placebo", "Active arm"), each = 60),
  AGEGR1 = sample(c("<65", "65-80", ">80"), 120, replace = TRUE),
  # Six panels, so an auto grid, a two-up and a three-up all read differently.
  AEBODSYS = sample(c("Cardiac", "Gastrointestinal", "Infections",
                      "Nervous system", "Skin", "Vascular"), 120,
                    replace = TRUE),
  AVAL = round(runif(120, 1, 9)),
  stringsAsFactors = FALSE
)
attr(ae$AEBODSYS, "label") <- "Body System"

ae <- mark_column_kinds(
  ae,
  group = c("TRT", "AGEGR1", "AEBODSYS"),
  value = "AVAL",
  id = "USUBJID"
)

chart <- function(facet_cols = NULL, subtitle = NULL, name) {
  new_chart_block(
    chart_type = "bar", group = "AGEGR1", color = "TRT",
    facet = "AEBODSYS", orientation = "vertical",
    facet_cols = facet_cols,
    title = "Subjects by age group",
    subtitle = subtitle,
    block_name = name
  )
}

board <- new_dock_board(
  blocks = c(
    raw = new_static_block(ae, block_name = "AE"),
    auto = chart(name = "Auto (fits the width)"),
    two = chart(facet_cols = "2", name = "Two per row"),
    sent = chart(
      subtitle = "By age group, split by {@facet}[, in {@facet_cols} columns]",
      name = "Sentence"
    ),
    deck = chart(facet_cols = "3", name = "Three per row (download it)")
  ),
  links = links(
    from = rep("raw", 4L),
    to = c("auto", "two", "sent", "deck")
  ),
  grids = list(
    compare = dock_grid("auto", "two", orientation = "vertical"),
    sentence = dock_grid("sent"),
    deck = dock_grid("deck")
  ),
  options = dock_board_options(),
  active = "compare",
  extensions = list(blockr.dag::new_dag_extension())
)

serve(board)
