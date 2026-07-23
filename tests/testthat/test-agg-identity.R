# The chart-only "identity" aggregation ("None (as is)"): it plots the value
# column directly, so a bar whose height is precomputed upstream needs no
# fabricated Sum/Mean. It lives ONLY in the JS engine (the chart aggregates
# client-side); it is deliberately absent from the shared AGG_FNS, so it has no
# R twin and cannot join the golden cross-test. This JS-only test guards its
# behaviour instead, driven through the same shipped engine + node runner.

skip_if_no_node <- function() {
  skip_on_cran()
  skip_if(Sys.which("node") == "", "node not available")
}

js_aggregate <- function(rows, config) {
  runner <- testthat::test_path("fixtures", "agg-golden-runner.js")
  engine <- system.file("js", "drilldown-agg.js", package = "blockr.viz")
  payload <- jsonlite::toJSON(
    list(rows = rows, config = config),
    dataframe = "rows", na = "null", auto_unbox = TRUE
  )
  out <- system2(
    Sys.which("node"), shQuote(c(runner, engine)),
    input = as.character(payload), stdout = TRUE, stderr = TRUE
  )
  jsonlite::fromJSON(paste(out, collapse = ""), simplifyVector = FALSE)
}

val_by_group <- function(js) {
  stats::setNames(lapply(js, function(r) r$value),
                  vapply(js, function(r) r$group, ""))
}

test_that("identity plots the value as-is when each category is unique", {
  skip_if_no_node()
  df <- data.frame(
    region = c("North", "South", "East", "West"),
    rev    = c(100, 250, NA, 75),
    stringsAsFactors = FALSE
  )
  js <- js_aggregate(df, list(func = "identity", value = "rev", group = "region"))
  v <- val_by_group(js)
  expect_equal(v[["North"]], 100)
  expect_equal(v[["South"]], 250)
  expect_equal(v[["West"]], 75)
  # A cell with no usable numeric value is a null gap (never a fabricated 0),
  # matching mean/min/max.
  expect_null(v[["East"]])
})

test_that("identity collapses a duplicated category to its first row", {
  skip_if_no_node()
  df <- data.frame(
    g = c("A", "A", "B"),
    v = c(10, 99, 5),
    stringsAsFactors = FALSE
  )
  js <- js_aggregate(df, list(func = "identity", value = "v", group = "g"))
  v <- val_by_group(js)
  expect_equal(v[["A"]], 10)   # first row wins
  expect_equal(v[["B"]], 5)
  # n still reports the rows behind the cell (here 2 for A), so the collapse
  # is discoverable even though only the first value is drawn.
  n_a <- Filter(function(r) r$group == "A", js)[[1]]$n
  expect_identical(n_a, 2L)
})

test_that("identity leaves the shared aggregations untouched", {
  skip_if_no_node()
  df <- data.frame(g = c("A", "A", "B"), v = c(10, 99, 5),
                   stringsAsFactors = FALSE)
  js <- js_aggregate(df, list(func = "sum", value = "v", group = "g"))
  v <- val_by_group(js)
  expect_equal(v[["A"]], 109)
  expect_equal(v[["B"]], 5)
})
