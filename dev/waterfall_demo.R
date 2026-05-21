# Waterfall Chart Demo
#
# Demonstrates the waterfall/bridge chart block for visualizing
# sequential value progression. Each bar shows the delta (change)
# from the previous step.
#
# Use cases: P&L statements, budget variance, revenue bridges

library(blockr)
library(blockr.dag)
library(blockr.bi)
pkgload::load_all("blockr.bi")

# ============================================================================
# Example 1: P&L Waterfall
# ============================================================================
#
# Shows how revenue cascades down to net income.
# Each bar after the first shows the drop between steps.

pnl_data <- data.frame(
  Revenue = 1000000,
  Gross_Profit = 600000,
  Operating_Income = 350000,
  Net_Income = 250000
)

run_app(
  blocks = c(
    data = new_static_block(pnl_data),

    waterfall = new_waterfall_block(
      measures = c("Revenue", "Gross_Profit", "Operating_Income", "Net_Income")
    )
  ),
  links = c(
    new_link("data", "waterfall", "data")
  ),
  extensions = list(new_dag_extension())
)


# ============================================================================
# Example 2: Quarterly Revenue Progression
# ============================================================================
#
# Shows how total revenue grows (or shrinks) quarter by quarter.
# Green bars = growth, red bars = decline.

if (FALSE) {
  quarterly_data <- data.frame(
    Q1 = 500000,
    Q2 = 620000,
    Q3 = 580000,
    Q4 = 710000
  )

  run_app(
    blocks = c(
      data = new_static_block(quarterly_data),

      waterfall = new_waterfall_block(
        measures = c("Q1", "Q2", "Q3", "Q4")
      )
    ),
    links = c(
      new_link("data", "waterfall", "data")
    ),
    extensions = list(new_dag_extension())
  )
}


