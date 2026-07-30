# Preview the deck-side chart renderers against the live canvas chart.
#
#   Rscript blockr.viz/dev/parity/preview.R [port]      # default 3838
#
# One scrolling page, one row per state in dev/parity/states.R, three panes
# of equal width so the marks can be compared directly:
#
#   canvas          the interactive chart block itself (ECharts, client-side)
#   static_chart()  the ggplot renderer, i.e. what a deck embeds under
#                   report_style "static" -- the parity target
#   chart_expr()    the compiled dplyr + ggplot2 pipeline, i.e. what
#                   report_call() emits by DEFAULT (report_style "code")
#
# Both printed forms come from report_call() rather than being called
# directly, so what renders here is what blockr.outline would put in the
# document. The emitted code is under each row in a collapsed panel.

script <- local({
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f)) normalizePath(f[[1L]]) else "dev/parity/preview.R"
})

viz <- dirname(dirname(dirname(script)))
root <- dirname(viz)

port <- local({
  a <- commandArgs(trailingOnly = TRUE)
  if (length(a)) as.integer(a[[1L]]) else
    as.integer(Sys.getenv("BLOCKR_PORT", "3838"))
})

options(shiny.port = port, shiny.host = "0.0.0.0")

for (d in c("blockr.core", "blockr.ui")) {
  pkgload::load_all(
    file.path(root, d),
    helpers = FALSE, attach_testthat = FALSE, export_all = FALSE
  )
}
pkgload::load_all(viz, helpers = FALSE, attach_testthat = FALSE,
                  export_all = FALSE)

library(shiny)
library(bslib)

source(file.path(viz, "dev", "parity", "states.R"))

ds <- parity_datasets()
ids <- names(parity_states)

# blockr.core resolves the ctor from sys.call(), so the constructor must be
# CALLED by name -- do.call(fn, ...) breaks resolve_ctor. Build the call.
blocks <- lapply(ids, function(id) {
  s <- parity_states[[id]]
  eval(as.call(c(quote(new_chart_block), s$args, list(block_name = id))))
})
names(blocks) <- ids

state_data <- function(id) ds[[parity_states[[id]]$data]]

# The printed form for one style, as report_call() would hand it to
# blockr.outline: a call over `data`, with the data snapshot available so
# data-dependent choices (colours, labels, titles) bake into literals.
emitted <- function(id, style) {
  old <- options(blockr.viz.report_style = style)
  on.exit(options(old), add = TRUE)
  report_call(blocks[[id]], var = "data", data = state_data(id))
}

# px caps (barMaxWidth, boxWidth) and the facet band resolve against the
# render device, so the pane width has to reach the renderer.
render_emitted <- function(ex, d, width_px) {
  old <- options(blockr.viz.gg_device_width = (width_px %||% 640) / 96)
  on.exit(options(old), add = TRUE)
  eval(ex, list(data = d), globalenv())
}

# A pane that cannot draw says so in place -- an uncovered chart type falls
# back to a table, and a broken pipeline should not take the page down.
note_plot <- function(txt) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = txt, size = 3.5,
                      colour = "#6b7280", hjust = 0.5) +
    ggplot2::theme_void()
}

CARD_H <- 400

pane <- function(title, body) {
  card(
    card_header(title, class = "py-1 px-2 small text-muted"),
    card_body(body, padding = 0),
    height = CARD_H, full_screen = TRUE
  )
}

state_row <- function(id) {
  s <- parity_states[[id]]
  args <- paste(names(s$args), unlist(lapply(s$args, format)),
                sep = " = ", collapse = ", ")
  # The row carries an id so a screenshot driver can target one state.
  tags$div(
    id = paste0("row_", id),
    tags$h5(id, tags$span(class = "text-muted small ms-2", args),
            class = "mt-4 mb-2"),
    layout_columns(
      col_widths = c(4, 4, 4),
      class = "parity-grid",
      # expr_ui() is the block's OWN ui -- the chart container. block_ui()
      # would show the result display (the filtered data frame) instead.
      tags$div(
        class = "pane-canvas",
        pane("canvas (ECharts)", expr_ui(id, blocks[[id]]))
      ),
      tags$div(
        class = "pane-static",
        pane("static_chart()", plotOutput(paste0("static_", id),
                                          height = paste0(CARD_H - 34, "px")))
      ),
      tags$div(
        class = "pane-code",
        pane("chart_expr() code", plotOutput(paste0("code_", id),
                                             height = paste0(CARD_H - 34, "px")))
      )
    ),
    accordion(
      open = FALSE,
      accordion_panel(
        "emitted code",
        layout_columns(
          col_widths = c(6, 6),
          tagList(tags$strong("report_style = \"static\""),
                  tags$pre(textOutput(paste0("src_static_", id),
                                      container = tags$code))),
          tagList(tags$strong("report_style = \"code\" (default)"),
                  tags$pre(textOutput(paste0("src_code_", id),
                                      container = tags$code)))
        )
      )
    )
  )
}

ui <- page_fluid(
  theme = bs_theme(version = 5),
  title = "static chart parity",
  tags$style(HTML("
    pre { text-align: left; overflow-x: auto; background: #f8f9fa;
          padding: 8px; border-radius: 4px; }
    pre code { font-size: 11px; white-space: pre; }
    .card-header { background: #f8f9fa; }
    /* The canvas sizes itself (28px per horizontal row, responsive facet
       columns), so a tall chart must scroll rather than be clipped. */
    .pane-canvas .card-body { overflow: auto; }
    /* Pane switch: hiding a column hands its width to the other two. */
    body.hide-code .pane-code, body.hide-static .pane-static {
      display: none !important;
    }
    /* bslib lays 12 tracks and spans each item 4; widen the survivors to 6
       rather than retracking the grid (which would wrap every span). */
    body.two-up .parity-grid > * { grid-column: span 6 !important; }
  ")),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('panes', function(msg) {
      // A length-1 character from R may arrive boxed as an array.
      const v = Array.isArray(msg) ? msg[0] : msg;
      const b = document.body.classList;
      b.toggle('hide-code', v === 'static');
      b.toggle('hide-static', v === 'code');
      b.toggle('two-up', v !== 'both');
      // Let ECharts re-measure; Shiny re-renders the plots on width change.
      setTimeout(() => window.dispatchEvent(new Event('resize')), 50);
    });
  ")),
  tags$h4("Deck renderers vs the canvas chart", class = "mt-3"),
  tags$p(
    class = "text-muted small",
    "The live block on the left, and next to it what a deck actually gets:",
    "static_chart() (report_style \"static\") and the compiled dplyr +",
    "ggplot2 pipeline (report_style \"code\", the default). Both come",
    "through report_call(), so these are the real printed forms; the",
    "emitted code is under each row. Compare at equal width -- the canvas",
    "sizes itself from the row count and picks its facet columns from the",
    "space it has, so a narrow pane changes its layout, not just its scale."
  ),
  radioButtons(
    "panes", NULL, inline = TRUE, selected = "static",
    choices = c("canvas + static_chart()" = "static",
                "canvas + chart_expr()" = "code",
                "all three" = "both")
  ),
  lapply(ids, state_row)
)

server <- function(input, output, session) {

  observeEvent(input$panes, {
    session$sendCustomMessage("panes", input$panes)
  })

  for (id in ids) local({
    i <- id
    d <- state_data(i)

    block_server(i, blocks[[i]], list(data = reactiveVal(d)))

    draw <- function(style, out_id) {
      renderPlot({
        ex <- emitted(i, style)
        if (is.null(ex)) {
          return(note_plot("report_call() returned NULL (bare print)"))
        }
        w <- session$clientData[[paste0("output_", out_id, "_width")]]
        p <- tryCatch(render_emitted(ex, d, w), error = function(e) e)
        if (inherits(p, "error")) {
          return(note_plot(paste0("error: ", conditionMessage(p))))
        }
        if (!inherits(p, "ggplot")) {
          return(note_plot(paste0("no ggplot -- fell back to ",
                                  paste(class(p), collapse = "/"))))
        }
        p
      }, res = 96)
    }

    output[[paste0("static_", i)]] <- draw("static", paste0("static_", i))
    output[[paste0("code_", i)]] <- draw("code", paste0("code_", i))

    output[[paste0("src_static_", i)]] <- renderText({
      ex <- emitted(i, "static")
      if (is.null(ex)) "NULL" else paste(deparse(ex, width.cutoff = 60L),
                                         collapse = "\n")
    })
    output[[paste0("src_code_", i)]] <- renderText({
      ex <- emitted(i, "code")
      txt <- chart_code(ex)
      if (is.null(txt)) "NULL" else txt
    })
  })

  invisible()
}

message("Open http://127.0.0.1:", port, "/")

shinyApp(ui, server)
