# Plain visual harness for the tile renderer — every layout x style on one
# page (no dock), so a screenshot shows them all. Exercises the real CSS + JS
# (count-up, fill grow-in, gear popover). Serve on 3838:
#   cd /workspace && Rscript blockr.bi/dev/tile-visual.R > /tmp/tilev.log 2>&1 &
# .libPaths("/workspace/blockr.dev/.devcontainer/.library")
suppressMessages({
  library(shiny)
  pkgload::load_all("blockr.bi", quiet = TRUE)
})

d <- tile_demo_data()
H <- function(...) blockr.bi:::tile_html(...)

ui <- fluidPage(
  tags$h3("cards · delta / fill / pill"),
  fluidRow(
    column(4, uiOutput("delta")),
    column(4, uiOutput("fill")),
    column(4, uiOutput("pill"))
  ),
  tags$h3("table · flat (delta) and grouped matrix"),
  fluidRow(
    column(6, uiOutput("tflat")),
    column(6, uiOutput("tmtx"))
  ),
  tags$h3("grouped cards"),
  uiOutput("gcards")
)

server <- function(input, output, session) {
  output$delta <- renderUI(H(d$scorecard, value = "value", measure = "metric",
    secondary = "delta", style = "delta", good_when = "up", format = "number",
    elem_id = "v-delta"))
  output$fill <- renderUI(H(d$scorecard, value = "value", measure = "metric",
    secondary = "progress", style = "fill", elem_id = "v-fill"))
  output$pill <- renderUI(H(d$scorecard, value = "value", measure = "metric",
    secondary = "status", style = "pill", elem_id = "v-pill"))
  output$tflat <- renderUI(H(d$scorecard, value = "value", measure = "metric",
    secondary = "delta", style = "delta", layout = "table", elem_id = "v-tflat"))
  output$tmtx <- renderUI(H(d$regions, value = c("revenue", "conversion", "orders"),
    by = "region", layout = "table", elem_id = "v-tmtx"))
  # Free-text unit: "USD" (compact) and "orders" — no inferred currency symbol.
  output$gcards <- renderUI(H(d$regions, value = "orders", by = "region",
    unit = "orders", format = "number", elem_id = "v-gcards"))
}

port <- as.integer(Sys.getenv("TILE_PORT", "3838"))
options(shiny.port = port, shiny.host = "0.0.0.0", shiny.launch.browser = FALSE)
shinyApp(ui, server)
