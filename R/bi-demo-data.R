#' @importFrom stats runif
NULL

#' BI Demo Dataset
#'
#' Creates a demo dataset for testing BI blocks.
#' Contains European sales data with clear dimension/measure structure.
#'
#' @section Dimensions (categorical columns):
#' - `Region`: Western Europe, Southern Europe, Northern Europe, Central Europe
#' - `Country`: Germany, France, Italy, Spain, UK, Netherlands, Belgium, Switzerland, Austria, Poland
#' - `Category`: Electronics, Clothing, Food & Beverage, Home & Garden
#' - `Channel`: Online, Retail, Wholesale
#' - `Year`: 2022, 2023, 2024
#' - `Quarter`: 1, 2, 3, 4
#'
#' @section Measures (numeric columns):
#' - `Revenue`: Sales revenue (varies by channel and category)
#' - `Quantity`: Number of units sold
#' - `Profit`: Profit margin (8-25% of revenue)
#' - `Transactions`: Number of transactions
#'
#' @section Hierarchies:
#' - Geography: Region > Country
#' - Time: Year > Quarter
#'
#' @return A tibble with ~1440 rows and 10 columns
#'
#' @export
#'
#' @examples
#' # Get the demo data
#' demo <- bi_demo_data()
#' head(demo)
#'
#' # Check dimensions
#' table(demo$Region)
#' table(demo$Category)
#'
#' # Quick summary
#' demo |> dplyr::group_by(Region) |> dplyr::summarise(Revenue = sum(Revenue))
bi_demo_data <- function() {
  # Real European countries
  countries <- c(
    "Germany", "France", "Italy", "Spain", "United Kingdom",
    "Netherlands", "Belgium", "Switzerland", "Austria", "Poland"
  )

  # Product categories
  categories <- c("Electronics", "Clothing", "Food & Beverage", "Home & Garden")

  # Sales channels
  channels <- c("Online", "Retail", "Wholesale")

  # Time periods
  years <- 2022:2024

  # Generate all combinations
  set.seed(42)

  data <- expand.grid(
    Country = countries,
    Category = categories,
    Channel = channels,
    Year = years,
    Quarter = 1:4,
    stringsAsFactors = FALSE
  )

  # Add Region based on Country (geography hierarchy)
  data$Region <- dplyr::case_when(
    data$Country %in% c("Germany", "France", "Netherlands", "Belgium", "Switzerland", "Austria") ~ "Western Europe",
    data$Country %in% c("Italy", "Spain") ~ "Southern Europe",
    data$Country %in% c("United Kingdom") ~ "Northern Europe",
    data$Country %in% c("Poland") ~ "Central Europe"
  )

  # Add measures with realistic variation
  n <- nrow(data)

  # Revenue: base 5k-100k, wholesale 2x, electronics 1.5x
  data$Revenue <- runif(n, 5000, 100000) *
    ifelse(data$Channel == "Wholesale", 2, 1) *
    ifelse(data$Category == "Electronics", 1.5, 1)

  # Quantity: 50-2000 units
  data$Quantity <- sample(50:2000, n, replace = TRUE)

  # Profit: 8-25% of revenue
  data$Profit <- data$Revenue * runif(n, 0.08, 0.25)

  # Transactions: 10-500 per period
  data$Transactions <- sample(10:500, n, replace = TRUE)

  # Reorder columns and return as tibble
  dplyr::tibble(
    Region = data$Region,
    Country = data$Country,
    Category = data$Category,
    Channel = data$Channel,
    Year = data$Year,
    Quarter = data$Quarter,
    Revenue = data$Revenue,
    Quantity = data$Quantity,
    Profit = data$Profit,
    Transactions = data$Transactions
  )
}
