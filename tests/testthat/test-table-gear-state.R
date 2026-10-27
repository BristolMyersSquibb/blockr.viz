# Gear-popover state transport: the <table> carries the CURRENT pickable
# columns (data-dt-cols, raw input schema) and config on every render, so the
# gear can re-read them at popover-open time (table.js readGearState) instead
# of keeping the snapshot parsed at chrome-init -- the chrome outlives table
# re-renders, and a config that changed server-side (restore race, AI /
# external_ctrl) must not be overwritten by a stale gear.

render <- function(tag) as.character(htmltools::renderTags(tag)$html)

test_that("dt_gear_cols_json emits the raw schema; '[]' for structured", {
  df <- data.frame(region = c("N", "S"), revenue = c(1, 2),
                   stringsAsFactors = FALSE)
  cols <- jsonlite::fromJSON(dt_gear_cols_json(df), simplifyVector = FALSE)
  expect_equal(
    cols,
    list(
      list(name = "region", type = "categorical"),
      list(name = "revenue", type = "numeric")
    )
  )

  structured <- data.frame(
    .label = c("Mean", "SD"), Total = c("1.0", "0.5"), check.names = FALSE
  )
  expect_equal(dt_gear_cols_json(structured), "[]")

  expect_equal(dt_gear_cols_json(NULL), "[]")
})

test_that("data-dt-cols is stamped on flat, message and structured tables", {
  df <- data.frame(region = c("N", "S"), revenue = c(1, 2),
                   stringsAsFactors = FALSE)

  flat <- render(dt_table_tag(df))
  expect_true(grepl("data-dt-cols=", flat))
  expect_true(grepl("&quot;region&quot;", flat))

  # A vanished configured column renders the message table, but the gear must
  # still read the CURRENT input columns to let the user fix the config.
  msg <- render(dt_table_tag(df, value_cols = "gone"))
  expect_true(grepl("Mapped column not in data", msg))
  expect_true(grepl("data-dt-cols=", msg))
  expect_true(grepl("&quot;revenue&quot;", msg))

  structured <- data.frame(
    .label = c("Mean", "SD"), Total = c("1.0", "0.5"), check.names = FALSE
  )
  st <- render(dt_table_tag(structured))
  expect_true(grepl("data-dt-cols=\"\\[\\]\"", st))
})
