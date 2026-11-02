# A colour, facet or series column that is NA in every row is unset, as an
# absent one is: upstream switches a split off by filling the column with NA.
# The browser side is in tests/js/chart/model-*.test.js.

es_df <- function() {
  data.frame(
    day = rep(1:10, 2),
    val = c(1:10, 11:20),
    arm = rep(c("A", "B"), each = 10),
    sub = NA_character_,
    stringsAsFactors = FALSE
  )
}

test_that("the column meta marks a column that is NA in every row", {
  meta <- viz_wire_columns(es_df())
  flag <- vapply(meta, function(m) isTRUE(m$all_na), logical(1L))
  names(flag) <- vapply(meta, `[[`, "", "name")
  expect_identical(names(flag)[flag], "sub")
  # No rows: nothing is known to be empty.
  meta0 <- viz_wire_columns(es_df()[0L, ])
  expect_false(any(vapply(meta0, function(m) isTRUE(m$all_na), logical(1L))))
})

test_that("chart_split_in() drops an absent or all-NA column", {
  d <- es_df()
  expect_identical(chart_split_in(d, "arm"), "arm")
  expect_null(chart_split_in(d, "sub"))
  expect_null(chart_split_in(d, "Subgroup"))
  expect_null(chart_split_in(d, NULL))
  d$sub[[3L]] <- "x"
  expect_identical(chart_split_in(d, "sub"), "sub")
})

test_that("the band and the smoother split by no all-NA column", {
  d <- es_df()
  a <- chart_band_args(list(type = "band", x = "day", y = "val",
                            color = "sub", series = "sub", facet = "sub"), d)
  expect_null(a$color_by)
  expect_null(a$series_by)
  expect_null(a$facet_by)
  s <- chart_smoother_args(list(type = "scatter", x = "day", y = "val",
                                smoother = "lm", color = "sub",
                                facet = "sub"), d)
  expect_null(s$color_by)
  expect_null(s$facet_by)
})

test_that("the Summary and the Excel frame have no all-NA key", {
  d <- es_df()
  for (missing in c("level", "drop")) {
    s <- chart_summary(d, list(type = "bar", category = "arm", color = "sub",
                               facet = "sub", func = "count",
                               missing = missing))
    expect_identical(nrow(s), 2L)
    expect_identical(sum(s[["Count of rows"]]), 20L)
    x <- chart_xlsx_frame(d, "bar", group = "arm", color = "sub",
                          facet = "sub", na_group = missing)
    expect_identical(names(x)[[1L]], "arm")
    expect_identical(nrow(x), 2L)
  }
})

test_that("a split by an all-NA column leaves the chart text", {
  d <- es_df()
  v <- list(type = "bar", category = "arm", facet = "sub",
            subtitle = "By arm[, split by {label(@facet)}]")
  tt <- chart_titles(viz_block_spec("chart"), v, d, list(), list())
  expect_identical(tt$subtitle$text, "By arm")
  d$sub[[1L]] <- "x"
  tt <- chart_titles(viz_block_spec("chart"), v, d, list(), list())
  expect_identical(tt$subtitle$text, "By arm, split by sub")
})
