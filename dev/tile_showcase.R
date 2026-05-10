# Tile-block feature catalog.
#
# Static, side-by-side showcase of every tile scene we support: all
# three showcases × all layouts × all color treatments. No dock, no
# DAG — every tile renders directly to the page so you can scan the
# whole feature surface at once.
#
#   Rscript /workspace/blockr.bi/dev/tile_showcase.R
#
# then open http://localhost:3838 .

pkgload::load_all("blockr.core", quiet = TRUE)
pkgload::load_all("blockr.bi",   quiet = TRUE)

library(shiny)
library(htmltools)

dd <- tile_demo_data()

# --- helpers ---------------------------------------------------------

shape <- getFromNamespace("tile_shape",   "blockr.bi")
draw  <- getFromNamespace("render_tiles", "blockr.bi")
deps  <- getFromNamespace("tile_block_deps", "blockr.bi")

scene <- function(n, title, desc, shaped, w = "100%") {
  tags$section(
    class = "scene",
    tags$div(
      class = "scene-head",
      tags$span(class = "scene-num", sprintf("%02d", n)),
      tags$h2(class = "scene-title", title),
      tags$p(class = "scene-desc", desc)
    ),
    tags$div(class = "scene-body", style = sprintf("max-width: %s;", w),
             draw(shaped))
  )
}

# --- scenes ----------------------------------------------------------

scenes <- list(
  scene(
    1, "Zero-config number tiles",
    "Three measures, mean by default. Format inferred from column name.",
    shape(dd$transactions, "number",
          aesthetics = list(value = c("revenue", "orders", "conversion")))
  ),
  scene(
    2, "Multi-stat",
    "One measure, four stats — one card per stat.",
    shape(dd$transactions, "number",
          aesthetics = list(value = "revenue"),
          stats = list(value = c("mean", "sum", "min", "max")))
  ),
  scene(
    3, "Scorecard — region × segment",
    "Two facet aesthetics; one card per cell, one column per measure.",
    shape(dd$transactions, "number",
          aesthetics = list(value = c("revenue", "orders"),
                            rows = "region", cols = "segment"),
          stats = list(value = "sum"))
  ),
  scene(
    4, "Sparklines",
    "Time series, one trend per ticker. Last value as headline.",
    shape(dd$time_series, "spark",
          aesthetics = list(value = "price",
                            spark_value = "price",
                            spark_x = "date",
                            cols = "ticker"),
          stats = list(value = "last"))
  ),
  scene(
    5, "Progress rings",
    "Value vs. max as a ring. KPI frame with per-metric target.",
    shape(dd$kpis_with_goals, "progress",
          aesthetics = list(value = "value", max = "target",
                            label = "metric", status = "status"))
  ),
  scene(
    6, "List-in-card",
    "Label mapped, no facets → one card with stacked rows.",
    shape(dd$kpis_with_goals, "number",
          aesthetics = list(value = "value", label = "metric",
                            target = "target", status = "status"))
  ),
  scene(
    7, "List-in-card · color by status (tint)",
    "Each row picks up a soft tint from its .status — green/amber/red.",
    shape(dd$kpis_with_goals, "number",
          aesthetics = list(value = "value", label = "metric",
                            target = "target", status = "status"),
          color = list(by = "status", intensity = "tint"))
  ),
  scene(
    8, "List-in-card · color by status (solid)",
    "Same data, solid bg with white text. Bold dashboard look.",
    shape(dd$kpis_with_goals, "number",
          aesthetics = list(value = "value", label = "metric",
                            target = "target", status = "status"),
          color = list(by = "status", intensity = "solid"))
  ),
  scene(
    9, "List-in-card · color by status (border)",
    "Quietest treatment: 3px left bar in the row's status color.",
    shape(dd$kpis_with_goals, "number",
          aesthetics = list(value = "value", label = "metric",
                            target = "target", status = "status"),
          color = list(by = "status", intensity = "border"))
  ),
  scene(
    10, "Number tiles · color by measure (tint)",
    "Three measures, each picks its own categorical hue.",
    shape(dd$transactions, "number",
          aesthetics = list(value = c("revenue", "orders", "conversion")),
          color = list(by = "measure", intensity = "tint"))
  ),
  scene(
    11, "Scorecard · color by region (solid)",
    "Region drives the color across all measures within that region.",
    shape(dd$transactions, "number",
          aesthetics = list(value = c("revenue", "orders"),
                            rows = "region"),
          stats = list(value = "sum"),
          color = list(by = "region", intensity = "solid"))
  ),
  scene(
    12, "Sparklines · color by ticker (border)",
    "Per-ticker accent without overpowering the trend line.",
    shape(dd$time_series, "spark",
          aesthetics = list(value = "price",
                            spark_value = "price",
                            spark_x = "date",
                            cols = "ticker"),
          stats = list(value = "last"),
          color = list(by = "ticker", intensity = "border"))
  )
)

# --- page ------------------------------------------------------------

ui <- tagList(
  deps(),
  tags$head(tags$style(HTML("
    body { background: #f8fafc; margin: 0; }
    .showcase {
      max-width: 1100px; margin: 0 auto; padding: 40px 24px 80px 24px;
      font-family: -apple-system, BlinkMacSystemFont, 'Inter',
                   'Segoe UI', sans-serif;
      color: #111827;
    }
    .showcase-header h1 {
      font-size: 1.75rem; font-weight: 600; margin: 0 0 6px 0;
    }
    .showcase-header p {
      color: #6b7280; margin: 0 0 32px 0; font-size: 0.95rem;
    }
    .scene {
      background: #fff; border: 1px solid #e5e7eb; border-radius: 12px;
      padding: 22px 24px 26px 24px; margin-bottom: 22px;
    }
    .scene-head { margin-bottom: 18px; }
    .scene-num {
      display: inline-block; font-size: 0.6875rem; font-weight: 600;
      letter-spacing: 0.08em; color: #9ca3af; vertical-align: top;
      margin-right: 8px; padding-top: 4px;
    }
    .scene-title {
      display: inline-block; font-size: 1.0625rem; font-weight: 600;
      margin: 0; line-height: 1.3;
    }
    .scene-desc {
      color: #6b7280; font-size: 0.875rem; margin: 4px 0 0 28px;
    }
    .scene-body { margin-top: 4px; }
  "))),
  tags$div(
    class = "showcase",
    tags$div(
      class = "showcase-header",
      tags$h1("blockr.bi · tile block"),
      tags$p(paste("Feature catalog. Three showcases (Number / Spark /",
                   "Progress) · layout variants · color",
                   "treatments (tint / solid / border). All scenes",
                   "render side-by-side; nothing here is interactive."))
    ),
    scenes
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
