# Gear-popover state transport: the <table> carries the CURRENT pickable
# columns (data-dt-cols, raw input schema) and config on every render, so the
# gear can re-read them at popover-open time (table.js readGearState) instead
# of keeping the snapshot parsed at chrome-init -- the chrome outlives table
# re-renders, and a config that changed server-side (restore race, AI /
# external_ctrl) must not be overwritten by a stale gear.

library(shiny)

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

test_that("aggregated display keeps the RAW input schema on data-dt-cols", {
  df  <- data.frame(region = c("N", "S", "N"), revenue = c(1, 2, 3),
                    stringsAsFactors = FALSE)
  blk <- new_table_block(group = "region",
                         summaries = list(list(func = "count",
                                               cols = list())))
  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    # The body ships as the data-push payload (not a renderUI output); the
    # gear attributes ride on the payload's <table> head.
    p <- jsonlite::fromJSON(r_body_payload(), simplifyVector = FALSE)
    expect_identical(p$kind, "flat")
    # The displayed frame is the aggregate: group + the "Count" metric column
    # (and 2 group rows, not 3 raw rows).
    expect_true(grepl("Count", p$head))
    expect_identical(p$n, 2L)
    # ... but the gear's pickable columns stay the raw input schema: revenue
    # (not displayed) is offered, the synthetic Count column is not.
    m <- regmatches(p$head, regexpr("data-dt-cols=\"[^\"]*\"", p$head))
    expect_length(m, 1L)
    expect_true(grepl("region", m))
    expect_true(grepl("revenue", m))
    expect_false(grepl("Count", m))
  })
})

test_that("excel pill without openxlsx renders a disabled button + hint", {
  df  <- data.frame(grp = c("A", "B"), val = c(1, 2), stringsAsFactors = FALSE)
  blk <- new_table_block(excel_download = TRUE)

  local_mocked_bindings(dt_has_openxlsx = function() FALSE)
  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    html <- as.character(output$dt_download$html)
    expect_true(grepl("blockr-dl-xlsx--off", html))
    expect_true(grepl("requires the openxlsx package", html))
    expect_true(grepl("aria-disabled=\"true\"", html))
    # No download binding: there is no handler to reach.
    expect_false(grepl("shiny-download-link", html))
  })
})

test_that("excel pill with openxlsx renders the live download link", {
  skip_if_not_installed("openxlsx")
  df  <- data.frame(grp = c("A", "B"), val = c(1, 2), stringsAsFactors = FALSE)
  blk <- new_table_block(excel_download = TRUE)

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    html <- as.character(output$dt_download$html)
    expect_true(grepl("shiny-download-link", html))
    expect_false(grepl("blockr-dl-xlsx--off", html))
  })
})
