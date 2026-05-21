# KPI Block Demo
#
# Demonstrates the restored new_kpi_block() with several configurations:
# single and multi-measure display, prefix/suffix glyphs, custom titles,
# subtitles, and colors.

library(blockr)
library(blockr.dag)
pkgload::load_all("blockr.bi")

# ============================================================================
# Example 1: Minimal — a single KPI
# ============================================================================

run_app(
  blocks = c(
    data = new_static_block(bi_demo_data()),

    kpi = new_kpi_block(
      measures = "Revenue",
      prefix = "$"
    )
  ),
  links = c(
    new_link("data", "kpi", "data")
  ),
  extensions = list(new_dag_extension())
)


# ============================================================================
# Example 2: Three KPIs side by side with currency and custom labels
# ============================================================================

if (FALSE) {
  run_app(
    blocks = c(
      data = new_static_block(bi_demo_data()),

      kpis = new_kpi_block(
        measures = c("Revenue", "Profit", "Transactions"),
        prefix = c(Revenue = "$", Profit = "$", Transactions = ""),
        titles = c(
          Revenue      = "Total Revenue",
          Profit       = "Net Profit",
          Transactions = "Transactions"
        ),
        subtitles = c(
          Revenue      = "Year to date",
          Profit       = "After taxes",
          Transactions = "All channels"
        ),
        colors = c(
          Revenue      = "#0072B2",
          Profit       = "#009E73",
          Transactions = "#E69F00"
        )
      )
    ),
    links = c(
      new_link("data", "kpis", "data")
    ),
    extensions = list(new_dag_extension())
  )
}


# ============================================================================
# Example 3: Different aggregations (mean instead of sum, count)
# ============================================================================

if (FALSE) {
  run_app(
    blocks = c(
      data = new_static_block(bi_demo_data()),

      avg_kpi = new_kpi_block(
        measures = c("Revenue", "Profit"),
        agg_fun = "mean",
        prefix = "$",
        digits = 2,
        titles = c(Revenue = "Avg Revenue", Profit = "Avg Profit")
      )
    ),
    links = c(
      new_link("data", "avg_kpi", "data")
    ),
    extensions = list(new_dag_extension())
  )
}
