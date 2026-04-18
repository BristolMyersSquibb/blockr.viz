# tile-demo/app.R — Extensive demo of new_tile_block().
#
# Shows each of v1's three showcases (number, spark, progress) plus
# how multi-measure, multi-stat, facet, target, and status layer on
# top of the same block. Reference aesthetic: Tremor / Vercel /
# shadcn — see design spec blockr.design/open/kpi-block-v2/.

options(shiny.port = 3839, shiny.host = "0.0.0.0",
        shiny.launch.browser = FALSE)

library(shiny)
library(blockr.core)
library(blockr.bi)
library(dplyr)

dd <- tile_demo_data()

# --- per-scene inputs -------------------------------------------------

# Scene 1: zero-config number tiles (1x3 summarised frame).
scene1_data <- dd$transactions |>
  summarise(revenue = sum(revenue), orders = sum(orders),
            conversion = mean(conversion))

# Scene 2: multi-stat (one measure, four stats).
scene2_data <- dd$transactions |>
  summarise(revenue = mean(revenue))
# Note: since tile_shape aggregates, we can pass raw transactions too.
scene2_data <- dd$transactions

# Scene 3: scorecard (region x segment).
scene3_data <- dd$transactions

# Scene 4: progress (value + max).
scene4_data <- dd$kpis_with_goals

# Scene 5: spark (time_series).
scene5_data <- dd$time_series

# Scene 6: status + target.
scene6_data <- dd$kpis_with_goals


# --- blocks -----------------------------------------------------------

blk1 <- new_tile_block(showcase = "number")

blk2 <- new_tile_block(
  showcase = "number",
  state = list(
    aesthetics = list(value = "revenue"),
    stats = list(value = c("mean", "sum", "min", "max"))
  )
)

blk3 <- new_tile_block(
  showcase = "number",
  state = list(
    aesthetics = list(
      value = c("revenue", "orders", "conversion"),
      rows = "region",
      cols = "segment"
    ),
    stats = list(value = "sum")
  )
)

blk4 <- new_tile_block(
  showcase = "progress",
  state = list(
    aesthetics = list(value = "value", max = "target", label = "metric",
                      status = "status")
  )
)

blk5 <- new_tile_block(
  showcase = "spark",
  state = list(
    aesthetics = list(
      value = "price", spark_value = "price", spark_x = "date",
      cols = "ticker"
    ),
    stats = list(value = "last")
  )
)

blk6 <- new_tile_block(
  showcase = "number",
  state = list(
    aesthetics = list(value = "value", target = "target",
                      status = "status", label = "metric")
  )
)


# --- UI ---------------------------------------------------------------

scene <- function(id, title, description) {
  div(class = "demo-scene",
    style = "background: var(--tb-surface-1, #fff); padding: 20px 24px;
             border-radius: 14px; margin-bottom: 24px;
             border: 1px solid #e5e7eb;",
    h3(title, style = "font-size: 0.9rem; font-weight: 600;
                       letter-spacing: 0.04em; text-transform: uppercase;
                       color: #6b7280; margin: 0 0 6px 0;"),
    tags$p(description, style = "font-size: 0.875rem; color: #6b7280;
                                  margin: 0 0 18px 0;"),
    div(
      style = "display: grid; grid-template-columns: minmax(280px, 340px) 1fr;
               gap: 20px; align-items: start;",
      div(style = "border-right: 1px solid #e5e7eb; padding-right: 20px;",
          tags$details(
            tags$summary("Block settings",
              style = "font-size: 0.75rem; color: #9ca3af; cursor: pointer; margin-bottom: 8px;"),
            expr_ui(id, get(paste0("blk", substr(id, 6, 6))))
          )
      ),
      div(block_ui(id, get(paste0("blk", substr(id, 6, 6)))))
    )
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background: #f8fafc; padding: 24px;
             font-family: -apple-system, 'Segoe UI', system-ui, sans-serif; }
      .demo-scene h3 { color: #6b7280; }
      .container-fluid { max-width: 1200px; }
      .demo-header { margin-bottom: 32px; }
      .demo-header h1 { font-size: 1.75rem; font-weight: 600;
                        letter-spacing: -0.01em; margin: 0 0 6px 0; }
      .demo-header p { color: #6b7280; margin: 0; }
      .demo-refs { margin: 12px 0 24px 0; font-size: 0.8125rem;
                   color: #9ca3af; }
      .demo-refs a { color: #2563eb; text-decoration: none; margin-right: 12px; }
      .demo-refs a:hover { text-decoration: underline; }
    "))
  ),
  div(class = "demo-header",
    h1("Tile Block \u2014 v1 demo"),
    tags$p("Six scenes exercising the ggplot-style aesthetic mapping across
            the three showcases.")
  ),
  div(class = "demo-refs",
    "Reference aesthetic: ",
    tags$a(href = "https://tremor.so/blocks/kpi-cards", target = "_blank", "Tremor"),
    tags$a(href = "https://vercel.com/templates/next.js/analytics-dashboard", target = "_blank", "Vercel"),
    tags$a(href = "https://ui.shadcn.com/blocks", target = "_blank", "shadcn")
  ),
  scene("scene1", "1. Zero-config number tiles",
        "Pre-aggregated 1x3 frame \u2192 three cards, auto-formatted
         (currency / number / percent from column names)."),
  scene("scene2", "2. Multi-stat",
        "Raw transactions \u2192 one measure (revenue) with four stats
         checked \u2192 four cards (mean / sum / min / max)."),
  scene("scene3", "3. Scorecard",
        "Raw transactions \u2192 rows=region, cols=segment, three
         measures \u2192 scorecard grid."),
  scene("scene4", "4. Progress rings",
        "KPI frame with value + target \u2192 progress ring showing
         value / target ratio per metric."),
  scene("scene5", "5. Sparklines",
        "Daily time series per ticker \u2192 last-value card with
         inline sparkline; trend color driven by sign."),
  scene("scene6", "6. Status + target",
        "Status-colored pills, target values shown in footer.")
)

server <- function(input, output, session) {
  pair <- function(id, blk, d) {
    block_server(id, blk, data = list(data = reactive(d)))
  }
  pair("scene1", blk1, scene1_data)
  pair("scene2", blk2, scene2_data)
  pair("scene3", blk3, scene3_data)
  pair("scene4", blk4, scene4_data)
  pair("scene5", blk5, scene5_data)
  pair("scene6", blk6, scene6_data)
}

shinyApp(ui, server)
