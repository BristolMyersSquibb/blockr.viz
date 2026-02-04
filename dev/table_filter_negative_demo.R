# Table Filter Demo: Negative Values
#
# Demonstrates diverging bars for datasets with negative values.
# Uses a profit/loss scenario where some combinations result in negative profit.

library(blockr)
library(blockr.dag)
library(blockr.bi)
library(dplyr)

# Create demo data with negative values (profit/loss scenario)
create_profit_loss_data <- function() {
  set.seed(42)

  regions <- c("North", "South", "East", "West")
  products <- c("Widget A", "Widget B", "Gadget X", "Gadget Y")
  channels <- c("Online", "Retail", "Wholesale")
  quarters <- c("Q1", "Q2", "Q3", "Q4")

  # Generate combinations
  data <- expand.grid(
    Region = regions,
    Product = products,
    Channel = channels,
    Quarter = quarters,
    stringsAsFactors = FALSE
  )

  n <- nrow(data)

  # Revenue is always positive
  data$Revenue <- round(runif(n, 10000, 100000))

  # Make certain dimensions CONSISTENTLY negative so aggregations show it
  # South region is struggling
  # Widget B and Gadget Y are unprofitable products
  # Wholesale channel loses money
  base_margin <- case_when(
    data$Region == "South" ~ -0.25,
    data$Product == "Widget B" ~ -0.20,
    data$Channel == "Wholesale" ~ -0.15,
    TRUE ~ 0.25
  )

  # Small randomness (not enough to flip signs)
  margin <- base_margin + rnorm(n, 0, 0.05)
  data$Profit <- round(data$Revenue * margin)

  # Units sold
  data$Units <- round(runif(n, 50, 500))

  # YoY Growth: Make South and Widget B consistently negative
  data$YoY_Growth <- case_when(
    data$Region == "South" ~ round(rnorm(n, -30, 10)),
    data$Product == "Widget B" ~ round(rnorm(n, -20, 10)),
    data$Quarter == "Q1" ~ round(rnorm(n, -10, 15)),
    TRUE ~ round(rnorm(n, 25, 15))
  )

  data
}

profit_data <- create_profit_loss_data()

# Show summary
cat("Data summary:\n")
cat("Profit range:", min(profit_data$Profit), "to", max(profit_data$Profit), "\n")
cat("YoY Growth range:", min(profit_data$YoY_Growth), "to", max(profit_data$YoY_Growth), "\n")
cat("Rows with negative profit:", sum(profit_data$Profit < 0), "of", nrow(profit_data), "\n")

# Run the app
run_app(
blocks = c(
    # Data with negative values
    data = new_static_block(profit_data),

    # Table filter - try selecting "Profit" or "YoY_Growth" as measure
    # to see diverging bars for negative values
    filter = new_table_filter_block(
      dimensions = c("Region", "Product", "Channel", "Quarter"),
      measure = "Profit"
    ),

    # Summary pivot table
    summary = new_pivot_table_block(
      rows = c("Product", "Channel"),
      measures = c("Revenue", "Profit", "Units"),
      agg_fun = "sum"
    )
  ),
  links = c(
    new_link("data", "filter", "data"),
    new_link("filter", "summary", "data")
  ),
  extensions = list(new_dag_extension())
)
