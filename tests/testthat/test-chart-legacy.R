# Arguments of blockr.viz < 0.3.0 and the public <-> internal translation
# (R/chart-args.R).

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
  # A bar saved without an orientation was drawn horizontal.
  expect_equal(st$type, "bar")
  expect_null(st$x)
  expect_equal(st$y, "ARM")
  expect_null(st$orientation)

  st <- chart_state(suppressWarnings(new_chart_block(
    chart_type = "boxplot", group = "ARM", value = "AVAL"
  )))
  expect_equal(st[c("type", "x", "y")],
               list(type = "boxplot", x = "ARM", y = "AVAL"))

  st <- chart_state(suppressWarnings(new_chart_block(
    chart_type = "bar", group = "ARM", value = "AVAL", func = "mean",
    orientation = "vertical", bar_mode = "grouped", tt_fields = "SEX",
    count_on = "axis", count_col = "USUBJID", na_group = "drop",
    ctrl_target = "flt", ctrl_table = "adsl", pct_of = "group",
    value_lines = 2, line_width_mult = 2, dot_size_mult = 0.5
  )))
  expect_equal(st$x, "ARM")
  expect_equal(st$y, "AVAL")
  expect_null(st$orientation)
  expect_equal(st$position, "dodge")
  expect_equal(st$tooltip, "SEX")
  expect_equal(st$show_n, "axis")
  expect_equal(st$id, "USUBJID")
  expect_equal(st$missing, "drop")
  expect_equal(st$drill_target, "flt")
  expect_equal(st$drill_table, "adsl")
  expect_equal(st$percent_of, "category")
  expect_equal(st$y_lines, 2)
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
  expect_equal(st[c("x", "y")], list(x = "step", y = "v"))

  w <- capture_warnings(
    new_chart_block(chart_type = "radar", group = "g", color = "c")
  )
  expect_true(any(grepl("radar", w)))
  expect_error(new_chart_block(type = "radar"), "must be one of")
})

test_that("a distinct count keeps the orientation it was drawn with", {
  # Two non-numeric columns: the data cannot say which one is the category.
  st <- chart_state(suppressWarnings(new_chart_block(
    chart_type = "bar", group = "AEDECOD", value = "USUBJID",
    func = "count_distinct"
  )))
  expect_equal(st$y, "AEDECOD")
  expect_equal(st$x, "USUBJID")
  expect_equal(st$orientation, "horizontal")
})

test_that("old names never come back on a re-save", {
  blk <- suppressWarnings(new_chart_block(
    chart_type = "bar", group = "ARM", bar_mode = "percent", metric = ".count"
  ))
  payload <- blockr.core::blockr_ser(blk)[["payload"]]
  expect_length(intersect(names(payload), CHART_LEGACY_ARGS), 0L)
  expect_false(any(CHART_LEGACY_ARGS %in% names(attributes(blk))))
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
  expect_equal(st$x, "total_sales")
  expect_equal(st$y, "region")
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

test_that("the orientation is inferred from x and y", {
  kinds <- c(sales = "numeric", AVAL = "numeric", class = "character",
             region = "character", ARM = "character")
  inf <- function(type, x, y, func = "count") {
    chart_infer_lie(list(type = type, x = x, y = y, func = func), kinds)
  }
  # one column: the category for a count, the value otherwise
  expect_equal(inf("bar", "class", NULL), "vertical")
  expect_equal(inf("bar", NULL, "class"), "horizontal")
  expect_equal(inf("bar", NULL, "sales", "sum"), "vertical")
  expect_equal(inf("boxplot", NULL, "AVAL"), "vertical")
  # two: the numeric one is the value
  expect_equal(inf("bar", "sales", "region", "sum"), "horizontal")
  expect_equal(inf("bar", "region", "sales", "sum"), "vertical")
  expect_equal(inf("boxplot", "AVAL", "ARM"), "horizontal")
  # none: vertical, for every type
  expect_equal(inf("bar", NULL, NULL), "vertical")
  expect_equal(inf("pointrange", NULL, NULL), "vertical")
})


test_that("title slots take the public names, and a saved old one is renamed", {
  blk <- suppressWarnings(blockr.core::blockr_deser(
    structure(list(), class = "chart_block"),
    list(payload = list(chart_type = "bar", group = "region", value = ".count",
                        subtitle = "{@chart_type}: {label(@group)}"),
         constructor = list(version = "0.2.9"))
  ))
  st <- blockr.core:::initial_block_state(blk)
  expect_identical(st$subtitle, "{@type}: {label(@y)}")
  expect_identical(st$y, "region")
})
