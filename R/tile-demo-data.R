#' Demo data for `new_tile_block()`
#'
#' Small, deterministic frames covering the shapes the tile renderer handles.
#' Because the tile does no arithmetic, the `scorecard` frame ships the
#' secondaries already computed (a signed `delta`, a `progress` fraction, a
#' `status`) so each display style can be demoed directly.
#'
#' @return A list of tibbles:
#'   - `scorecard` — one row per metric with precomputed `delta` / `progress`
#'     / `status` columns (long tile frame; for the card styles).
#'   - `regions` — wide, one row per region with several measure columns (for
#'     the grouped matrix / `by`-clustered cards and drill).
#'   - `transactions` — raw rows, to demo the upstream `summarize` -> tile flow.
#' @export
tile_demo_data <- function() {
  set.seed(20260417L)

  scorecard <- tibble::tibble(
    metric   = c("Revenue", "Active Users", "Avg Order", "Net Cash Flow",
                 "Monthly Churn", "Net Revenue Retention"),
    value    = c(1240000, 38400, 64.20, -128000, 0.021, 1.18),
    delta    = c(0.127, -0.031, 0.070, -0.224, -0.006, 0.040),  # signed fraction
    progress = c(0.62, 0.84, 0.55, 0.30, 0.70, 0.95),           # fraction to target
    status   = c("ok", "warn", "ok", "bad", "ok", "ok")
  )

  regions <- tibble::tibble(
    region     = c("EMEA", "AMER", "APAC", "LATAM"),
    revenue    = c(1.24, 0.98, 1.51, 0.76),   # $M
    conversion = c(0.038, 0.041, 0.029, 0.033),
    orders     = c(842, 1203, 689, 451)
  )

  regions_seq <- c("EMEA", "AMER", "APAC")
  segments    <- c("SMB", "Mid", "Enterprise")
  products    <- c("A", "B", "C", "D")
  days <- seq.Date(Sys.Date() - 365, Sys.Date(), by = "day")
  n <- 300
  transactions <- tibble::tibble(
    date       = sample(days, n, replace = TRUE),
    region     = sample(regions_seq, n, replace = TRUE, prob = c(0.35, 0.4, 0.25)),
    segment    = sample(segments, n, replace = TRUE, prob = c(0.4, 0.35, 0.25)),
    product    = sample(products, n, replace = TRUE),
    revenue    = round(stats::rgamma(n, shape = 2, scale = 8000), 0),
    orders     = sample(5:120, n, replace = TRUE),
    conversion = round(stats::runif(n, 0.02, 0.18), 3)
  )

  list(
    scorecard    = scorecard,
    regions      = regions,
    transactions = transactions
  )
}
