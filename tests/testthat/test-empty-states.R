# Differentiated empty / error states across the table and tile renderers
# (chart parity: chart.js already distinguishes "no data", "mapped column not
# in data" and "pick a role" -- these pin the same three states server-side).

render <- function(x) as.character(htmltools::renderTags(x)$html)

# --- flat table --------------------------------------------------------

test_that("table: 0 rows says no rows, not a config problem", {
  h <- render(drilldown_table(iris[0, ]))
  expect_match(h, "No rows to display")
  expect_no_match(h, "re-pick it in the gear")
})

test_that("table: a rowname renamed upstream names the column + hint", {
  h <- render(drilldown_table(iris, label_col = "gone"))
  expect_match(h, "Mapped column not in data: Rowname = \"gone\"")
  expect_match(h, "re-pick it in the gear")
  expect_no_match(h, "No rows to display")
})

test_that("table: explicitly picked value columns all gone -> config error", {
  h <- render(drilldown_table(iris, value_cols = "gone"))
  expect_match(h, "Mapped column not in data: Value = \"gone\"")
  expect_match(h, "re-pick it in the gear")
})

test_that("table: partially missing value pick still renders survivors", {
  h <- render(drilldown_table(iris, value_cols = c("Sepal.Width", "gone")))
  expect_match(h, "Sepal.Width", fixed = TRUE)
  expect_no_match(h, "Mapped column not in data")
})

test_that("table: no value column left to show prompts a gear pick", {
  # A one-column frame defaults value_cols to nothing -- prompt, not "No data".
  h <- render(drilldown_table(data.frame(x = 1:3)))
  expect_match(h, "Pick a Value column in the gear")
})

test_that("table: missing rowname wins over 0 rows (actionable first)", {
  h <- render(drilldown_table(iris[0, ], label_col = "gone"))
  expect_match(h, "Mapped column not in data: Rowname = \"gone\"")
})

# --- structured table --------------------------------------------------

test_that("structured table: 0 rows says no rows", {
  d <- data.frame(.label = character(), A = numeric(),
                  stringsAsFactors = FALSE)
  h <- render(drilldown_table(d))
  expect_match(h, "No rows to display")
})

test_that("structured table: no data columns says so", {
  d <- data.frame(.label = c("Age", "Sex"), stringsAsFactors = FALSE)
  h <- render(drilldown_table(d))
  expect_match(h, "No value columns to display")
})

# --- tile --------------------------------------------------------------

test_that("tile: unconfigured value prompts a gear pick", {
  h <- render(blockr.viz:::tile_html(iris, value = character()))
  expect_match(h, "Pick a Value column in the gear")
})

test_that("tile: value column renamed upstream names it + hint", {
  h <- render(blockr.viz:::tile_html(iris, value = "gone"))
  expect_match(h, "Mapped column not in data: Value = \"gone\"")
  expect_match(h, "re-pick it in the gear")
})

test_that("tile: 0 rows with a valid mapping says no rows", {
  h <- render(blockr.viz:::tile_html(iris[0, ], value = "Sepal.Length"))
  expect_match(h, "No rows to display")
  expect_no_match(h, "re-pick it in the gear")
})

test_that("tile: valid mapping + rows renders cards, no empty state", {
  h <- render(blockr.viz:::tile_html(head(iris), value = "Sepal.Length"))
  expect_no_match(h, "is-empty")
  expect_match(h, "tk-card")
})
