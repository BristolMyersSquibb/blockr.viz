# App fixture for the Playwright ECharts canvas-drill test (see
# chart-drill.spec.mjs). A real bar chart whose click-to-filter drill feeds a
# downstream table, so a genuine canvas click produces an observable row-count
# change. Served on 3838 (the devcontainer's forwarded port).
#
#   Rscript dev/e2e/chart-drill-app.R
#
# This is the one interaction shinytest2 can't drive: ECharts renders to a
# <canvas>, and zrender's hit-testing needs a native mouse event at the bar's
# pixel coords (a synthesized DOM event doesn't trigger it). Playwright's
# page.mouse.click does.

library(blockr.core)
library(blockr.viz)

drill_data <- data.frame(
  region  = c("North", "North", "South", "South", "East", "West"),
  product = c("A", "B", "A", "B", "A", "A"),
  revenue = c(100, 50, 80, 40, 60, 30),
  profit  = c(10, 5, 8, 4, 6, 3),
  stringsAsFactors = FALSE
)

# Defaults to 3838 (the forwarded port) when run standalone; override with
# VIZ_E2E_PORT when 3838 is busy (e.g. a concurrent app) or for CI.
options(
  shiny.port = as.integer(Sys.getenv("VIZ_E2E_PORT", "3838")),
  shiny.host = "0.0.0.0"
)

serve(
  new_board(
    blocks = c(
      data = new_static_block(data = drill_data),
      chart = new_chart_block(
        chart_type = "bar",
        group      = "region",
        metric     = "revenue",
        agg_fn     = "sum",
        drill      = "region"
      ),
      # Downstream of the chart: shows the rows the chart's drill filters to.
      out = new_table_block(rowname = "product", values = c("revenue", "profit"))
    ),
    links = c(
      new_link("data", "chart", "data"),
      new_link("chart", "out", "data")
    )
  ),
  id = "board"
)
