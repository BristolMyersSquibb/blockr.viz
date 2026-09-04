# PROTOTYPE: the download is the picture on the screen.
#
#   BLOCKR_CANVAS_CAPTURE=1 Rscript blockr.viz/dev/capture-demo.R [port]
#
# With the flag on, opening a chart's download menu makes the canvas compose
# itself (chart.js `_downloadImage`) and post the bitmap to R; the png, html
# and pptx downloads then carry that instead of a server-side re-render. Turn
# the flag off and the same buttons go back through static_chart(), so the
# two can be compared side by side in one session.
#
# The board is the shape that started this: long treatment-arm labels on a
# boxplot, where a wide panel keeps them flat and an 11.9in slide cannot.

.self <- grep("^--file=", commandArgs(FALSE), value = TRUE)[1]
.ws <- normalizePath(if (is.na(.self)) "." else
  file.path(dirname(sub("^--file=", "", .self)), "..", ".."))

args <- commandArgs(TRUE)
port <- if (length(args)) as.integer(args[[1]]) else blockr_port()

options(shiny.port = port, shiny.host = "0.0.0.0")

for (p in c("blockr.core", "blockr.ui", "blockr.dock", "blockr.dag",
            "blockr.theme", "blockr.viz")) {
  pkgload::load_all(file.path(.ws, p), helpers = FALSE,
                    attach_testthat = FALSE, export_all = FALSE)
}

set.seed(4)

arms <- c(
  "GROUPEP1B BMS-986507 2.0mg+Pumitamig 1500 or 1200mg",
  "GROUPEP1A BMS-986507+Pumitamig",
  "GROUPBP1 BMS-986507 2.0mg+Pumitamig 1500mg",
  "GROUPBP2 BMS-986507 4.0mg+Pumitamig 1500mg",
  "GROUPCP1 Placebo+Pumitamig 1500mg",
  "GROUPDP1 BMS-986507 2.0mg"
)

adsl <- data.frame(
  TRT = factor(rep(arms, each = 25), levels = arms),
  SEX = rep(c("F", "M"), 75),
  AGE = as.numeric(replicate(6, rnorm(25, 65, 8)))
)
attr(adsl, "label") <- "Age by Demographic"

board <- new_dock_board(
  blocks = c(
    data = new_static_block(adsl, block_name = "ADSL (open data stand-in)"),
    anova = new_chart_block(
      chart_type = "boxplot", group = "TRT", value = "AGE",
      download = TRUE, block_name = "Oneway Anova Analysis"
    ),
    split = new_chart_block(
      chart_type = "boxplot", group = "TRT", value = "AGE", color = "SEX",
      facet = "SEX", download = TRUE, block_name = "Same, split and facetted"
    )
  ),
  links = links(
    from = c("data", "data"), to = c("anova", "split")
  ),
  grids = list(Charts = dock_grid("data", "anova", "split"))
)

# The deck side of the prototype, standing in for blockr.outline: shortly
# after a session connects, ask the capture service for a picture of EVERY
# chart block at the slide box -- including the one whose panel was never
# fronted -- and write the deck when they are all in. This is the two-step
# an export needs (ask, then write), just triggered by a timer instead of a
# button.
deck_out <- file.path("/workspace/_scratch/capture-proto", "deck.pptx")

capture_proof <- function(session) {

  # 11.9in x 5.5in of slide at 96dpi, per chart.
  box <- c(width = 1142, height = 528)
  tokens <- shiny::reactiveVal(NULL)

  shiny::observe({
    shiny::invalidateLater(15000)
    if (!is.null(tokens())) return()
    ids <- blockr.viz:::chart_capture_ids(session)
    if (!length(ids)) {
      message("capture proof: no charts registered yet")
      return()
    }
    message("capture proof: asking ", length(ids), " chart(s): ",
            paste(ids, collapse = ", "))
    ask <- function(i) {
      blockr.viz:::chart_capture_request(i, box[["width"]], box[["height"]],
                                         session)
    }
    tokens(stats::setNames(vapply(ids, ask, character(1L)), ids))
  })

  shiny::observe({
    tk <- tokens()
    if (is.null(tk)) return()
    caps <- lapply(tk, function(t) blockr.viz:::chart_capture_collect(t, session))
    if (any(vapply(caps, is.null, logical(1L)))) return()
    doc <- officer::read_pptx()
    for (nm in names(caps)) {
      doc <- blockr.viz::pptx_add_exhibit(doc, caps[[nm]], title = nm)
    }
    print(doc, target = deck_out)
    message("capture proof: wrote ", deck_out, " (", length(caps), " slides)")
    tokens(NULL)
  })
}

message("\n  capture ", if (blockr.viz:::canvas_capture_on()) "ON" else "OFF",
        "   http://127.0.0.1:", port, "/\n")

app <- serve(board)

print(shiny::shinyApp(
  ui = app$httpHandler,
  server = function(input, output, session) {
    app$serverFuncSource()(input, output, session)
    capture_proof(session)
  },
  options = app$appOptions
))
