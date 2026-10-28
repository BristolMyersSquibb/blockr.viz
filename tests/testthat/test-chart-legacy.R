# Arguments of blockr.viz < 0.3.0 (R/legacy.R, R/legacy-chart.R).

chart_state <- function(blk, data = data.frame(a = 1)) {
  out <- NULL
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      out <<- lapply(session$returned$state, function(r) r())
    },
    args = list(x = blk, data = list(data = function() data))
  )
  out
}

test_that("old names in code are translated, with a warning", {
  expect_warning(
    blk <- new_chart_block(chart_type = "bar", group = "ARM",
                           value = ".count", func = "count"),
    "from an earlier blockr.viz"
  )
  st <- chart_state(blk)
  # A bar saved without an orientation was drawn horizontal, as it is now.
  expect_equal(st$type, "bar")
  expect_equal(st$category, "ARM")
  expect_null(st$value)
  expect_null(st$orientation)

  st <- chart_state(suppressWarnings(new_chart_block(
    chart_type = "boxplot", group = "ARM", value = "AVAL"
  )))
  expect_equal(st[c("type", "category", "value")],
               list(type = "boxplot", category = "ARM", value = "AVAL"))

  st <- chart_state(suppressWarnings(new_chart_block(
    chart_type = "bar", group = "ARM", value = "AVAL", func = "mean",
    orientation = "vertical", bar_mode = "grouped", tt_fields = "SEX",
    count_on = "axis", count_col = "USUBJID", na_group = "drop",
    ctrl_target = "flt", ctrl_table = "adsl", pct_of = "group",
    value_lines = 2, line_width_mult = 2, dot_size_mult = 0.5
  )))
  expect_equal(st$category, "ARM")
  expect_equal(st$value, "AVAL")
  expect_equal(st$orientation, "vertical")
  expect_equal(st$position, "dodge")
  expect_equal(st$tooltip, "SEX")
  expect_equal(st$show_n, "axis")
  expect_equal(st$id, "USUBJID")
  expect_equal(st$missing, "drop")
  expect_equal(st$drill_target, "flt")
  expect_equal(st$drill_table, "adsl")
  expect_equal(st$percent_of, "category")
  expect_equal(st$value_lines, 2)
  expect_equal(st$line_width, 2)
  expect_equal(st$point_size, 0.5)
})

test_that("old interval, band, script and filter fields are translated", {
  st <- chart_state(suppressWarnings(new_chart_block(
    chart_type = "scatter", x = "est", y = "term", lo = "lower",
    hi = "upper", band_id = "USUBJID", values = list(k = 2),
    filters = list(term = "a")
  )))
  expect_equal(st$ymin, "lower")
  expect_equal(st$ymax, "upper")
  expect_equal(st$id, "USUBJID")
  expect_equal(st$script_values, list(k = 2))
  expect_equal(st$selection$type, "categorical")
  expect_equal(st$selection$filters, list(term = "a"))

  st <- chart_state(suppressWarnings(new_chart_block(
    chart_type = "bar", group = "ARM", filter_column = "ARM",
    filter_values = list("A")
  )))
  expect_equal(st$selection$filters, list(ARM = "A"))
})

test_that("an old id survives beside identity_line", {
  st <- chart_state(suppressWarnings(new_chart_block(
    chart_type = "scatter", x = "est", y = "term", band_id = "USUBJID",
    identity_line = TRUE
  )))
  expect_equal(st$id, "USUBJID")
})

test_that("a cumulative bar is a waterfall, a radar a bar", {
  st <- chart_state(suppressWarnings(new_chart_block(
    chart_type = "bar", group = "step", value = "v", func = "sum",
    baseline = "cumulative"
  )))
  expect_equal(st$type, "waterfall")
  expect_equal(st[c("category", "value")], list(category = "step", value = "v"))

  w <- capture_warnings(
    new_chart_block(chart_type = "radar", group = "g", color = "c")
  )
  expect_true(any(grepl("radar", w)))
  expect_error(new_chart_block(type = "radar"), "must be one of")
})

test_that("a distinct count keeps its roles and its orientation", {
  st <- chart_state(suppressWarnings(new_chart_block(
    chart_type = "bar", group = "AEDECOD", value = "USUBJID",
    func = "count_distinct"
  )))
  expect_equal(st$category, "AEDECOD")
  expect_equal(st$value, "USUBJID")
  expect_null(st$orientation)
})

test_that("old names never come back on a re-save", {
  blk <- suppressWarnings(new_chart_block(
    chart_type = "bar", group = "ARM", bar_mode = "percent", metric = ".count"
  ))
  payload <- blockr.core::blockr_ser(blk)[["payload"]]
  old <- LEGACY_NAMES$old[LEGACY_NAMES$block == "chart" &
                             LEGACY_NAMES$old != LEGACY_NAMES$new]
  expect_length(intersect(names(payload), old), 0L)
  expect_false(any(old %in% names(attributes(blk))))
  expect_equal(payload$position, "fill")
})

test_that("a board saved with 0.2.x restores silently", {
  ser <- blockr.core::blockr_ser(new_chart_block())
  ser$constructor$version <- "0.2.221"
  ser$payload <- list(
    group = "region", color = NULL, facet = NULL, value = "total_sales",
    func = "sum", chart_type = "bar", x = NULL, y = NULL,
    orientation = NULL, bar_mode = "stacked", filter_type = "categorical",
    filter_column = NULL, filter_values = NULL, filters = NULL,
    count_on = "off", count_col = NULL, na_group = "level",
    func_toggle = NULL, lo = NULL, hi = NULL, baseline = "zero",
    ctrl_target = "", ctrl_table = "", script = NULL, values = list()
  )
  expect_no_warning(blk <- blockr.core::blockr_deser(ser))
  st <- chart_state(blk, data.frame(region = c("N", "S"),
                                    total_sales = c(1, 2)))
  expect_equal(st$category, "region")
  expect_equal(st$value, "total_sales")
  expect_null(st$orientation)
  expect_equal(st$func, "sum")
})

test_that("a board saved with direct_labels restores as legend = marks", {
  ser <- blockr.core::blockr_ser(new_chart_block(type = "line"))
  ser$payload$legend <- NULL
  ser$payload$direct_labels <- TRUE
  expect_no_warning(blk <- blockr.core::blockr_deser(ser))
  expect_equal(blockr.core::blockr_ser(blk)$payload$legend, "marks")
  expect_null(blockr.core::blockr_ser(blk)$payload$direct_labels)
})

test_that("an aggregated type in code takes category and value, not x and y", {
  expect_error(new_chart_block(type = "bar", y = "class"),
               "takes `category` and `value`")
  expect_error(new_chart_block(type = "boxplot", x = "g", y = "v"),
               "takes `category` and `value`")
  # A scatter's x and y left in the state are fine beside a category, and a
  # restore never asks.
  expect_s3_class(new_chart_block(type = "bar", category = "class", x = "a"),
                  "chart_block")
  ser <- blockr.core::blockr_ser(new_chart_block(type = "scatter", x = "a",
                                                 y = "b"))
  ser$payload$type <- "bar"
  expect_s3_class(blockr.core::blockr_deser(ser), "chart_block")
})

test_that("value = \".count\" means a row count", {
  st <- chart_state(new_chart_block(category = "g", value = ".count"))
  expect_null(st$value)
})

test_that("title slots take the public names, and a saved old one is renamed", {
  blk <- suppressWarnings(blockr.core::blockr_deser(
    structure(list(), class = "chart_block"),
    list(payload = list(chart_type = "bar", group = "region", value = ".count",
                        subtitle = "{@chart_type}: {label(@group)}"),
         constructor = list(version = "0.2.9"))
  ))
  st <- blockr.core:::initial_block_state(blk)
  expect_identical(st$subtitle, "{@type}: {label(@category)}")
  expect_identical(st$category, "region")
})
