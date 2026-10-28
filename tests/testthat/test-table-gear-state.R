# Gear-popover state transport: the <table> carries the CURRENT pickable
# columns (data-dt-cols, raw input schema) and config on every render, so the
# gear can re-read them at popover-open time (table.js readGearState) instead
# of keeping the snapshot parsed at chrome-init -- the chrome outlives table
# re-renders, and a config that changed server-side (restore race, AI /
# external_ctrl) must not be overwritten by a stale gear.

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

