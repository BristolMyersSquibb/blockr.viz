#' Demo data for `new_tile_block()`
#'
#' Returns three data frames used by the tile-block demo app. Small,
#' deterministic, covers the shapes the tile block needs to handle:
#' transactions (facets + multiple measures), time_series (sparklines),
#' kpis_with_goals (target + status).
#'
#' @return A list with three tibbles: `transactions`, `time_series`,
#'   `kpis_with_goals`.
#' @export
tile_demo_data <- function() {
  set.seed(20260417L)

  # --- transactions: ~300 rows, per-region-per-segment-per-product ------
  regions  <- c("EMEA", "AMER", "APAC")
  segments <- c("SMB", "Mid", "Enterprise")
  products <- c("A", "B", "C", "D")
  days <- seq.Date(Sys.Date() - 365, Sys.Date(), by = "day")

  n <- 300
  transactions <- tibble::tibble(
    date       = sample(days, n, replace = TRUE),
    region     = sample(regions, n, replace = TRUE, prob = c(0.35, 0.4, 0.25)),
    segment    = sample(segments, n, replace = TRUE, prob = c(0.4, 0.35, 0.25)),
    product    = sample(products, n, replace = TRUE),
    revenue    = round(stats::rgamma(n, shape = 2, scale = 8000), 0),
    orders     = sample(5:120, n, replace = TRUE),
    conversion = round(stats::runif(n, 0.02, 0.18), 3),
    status     = sample(c("ok", "warn", "bad"), n, replace = TRUE,
                        prob = c(0.7, 0.2, 0.1))
  )

  # --- time_series: daily prices for 4 tickers over 365 days -----------
  tickers <- c("ACME", "WIDGET", "GLOBEX", "INITECH")
  ts_rows <- lapply(tickers, function(tk) {
    start <- stats::runif(1, 50, 300)
    trend <- stats::runif(1, -0.0005, 0.001)
    noise <- stats::rnorm(length(days), 0, 0.015)
    returns <- trend + noise
    price <- start * cumprod(1 + returns)
    tibble::tibble(
      date   = days,
      ticker = tk,
      price  = round(price, 2)
    )
  })
  time_series <- dplyr::bind_rows(ts_rows)

  # --- kpis_with_goals: one row per named KPI --------------------------
  kpis_with_goals <- tibble::tibble(
    metric = c("Revenue", "New Signups", "NPS", "MAU",
               "Uptime %", "Cost / Sale"),
    value  = c(1240000, 842,  68,  18400, 99.6, 42.50),
    target = c(1500000, 1000, 75,  20000, 99.9, 40.00),
    status = c("warn",  "warn", "ok", "warn", "ok", "warn")
  )

  list(
    transactions     = transactions,
    time_series      = time_series,
    kpis_with_goals  = kpis_with_goals
  )
}
