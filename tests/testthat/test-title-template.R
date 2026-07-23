# Dynamic block titles (R/title-template.R): the {...} token resolver and
# the three-tier NULL / "" / template contract shared by the viz blocks.

test_that("resolve_title_template substitutes column-value tokens", {
  d <- data.frame(ARM = c("Placebo", "Drug A", "Placebo"))
  expect_equal(
    resolve_title_template("AEs by {ARM}", d),
    "AEs by Placebo, Drug A"
  )
})

test_that("factor columns collapse in level order, not appearance order", {
  d <- data.frame(
    ARM = factor(c("Drug A", "Placebo"), levels = c("Placebo", "Drug A"))
  )
  expect_equal(resolve_title_template("{ARM}", d), "Placebo, Drug A")
})

test_that("label(col) resolves the label attribute with name fallback", {
  d <- data.frame(value = 1:3, other = 4:6)
  attr(d$value, "label") <- "Systolic Blood Pressure"
  expect_equal(
    resolve_title_template("Mean {label(value)} by visit", d),
    "Mean Systolic Blood Pressure by visit"
  )
  expect_equal(resolve_title_template("{label(other)}", d), "other")
})

test_that("n and n_distinct tokens count rows and values", {
  d <- data.frame(id = c("a", "a", "b", NA))
  expect_equal(resolve_title_template("{n} rows", d), "4 rows")
  expect_equal(resolve_title_template("{n_distinct(id)} ids", d), "2 ids")
})

test_that("missing columns resolve to empty, never error", {
  d <- data.frame(x = 1)
  expect_equal(resolve_title_template("a {gone} b", d), "a  b")
  expect_equal(resolve_title_template("{label(gone)}", d), "")
  expect_equal(resolve_title_template("{n_distinct(gone)}", d), "")
})

test_that("long value lists elide past TITLE_MAX_VALUES", {
  d <- data.frame(id = sprintf("S%02d", 1:20))
  out <- resolve_title_template("{id}", d)
  expect_true(endsWith(out, "…"))
  expect_equal(length(strsplit(out, ", ")[[1]]), TITLE_MAX_VALUES + 1L)
})

test_that("plain text and empty templates pass through", {
  d <- data.frame(x = 1)
  expect_equal(resolve_title_template("No tokens here", d), "No tokens here")
  expect_equal(resolve_title_template("", d), "")
})

test_that("resolve_block_title implements the three tiers", {
  d <- data.frame(ARM = "Placebo")

  # NULL = auto: falls back to the supplied label, verbatim (not a template).
  expect_equal(resolve_block_title(NULL, d, auto = "Table 14.3.1"), "Table 14.3.1")
  expect_null(resolve_block_title(NULL, d, auto = NULL))

  # "" (and whitespace) = explicitly none, even when an auto label exists.
  expect_null(resolve_block_title("", d, auto = "Table 14.3.1"))
  expect_null(resolve_block_title("   ", d, auto = "Table 14.3.1"))

  # Text = template, resolved; the auto label is ignored.
  expect_equal(
    resolve_block_title("AEs: {ARM}", d, auto = "Table 14.3.1"),
    "AEs: Placebo"
  )
})

test_that("title_state keeps '' (off) and heals list() to NULL", {
  expect_null(title_state(NULL))
  expect_null(title_state(list()))     # pre-#144 DAG paste corruption
  expect_equal(title_state(""), "")    # explicitly none must survive
  expect_equal(title_state("x {y}"), "x {y}")
})

test_that("input_display_attrs reads label, subtitle and caption from raw input", {
  d <- data.frame(x = 1)
  expect_equal(
    input_display_attrs(d),
    list(label = NULL, subtitle = NULL, caption = NULL)
  )
  attr(d, "label") <- "Demographics"
  attr(d, "subtitle") <- "Safety population"
  attr(d, "caption") <- "Source: ADSL"
  expect_equal(
    input_display_attrs(d),
    list(label = "Demographics", subtitle = "Safety population",
         caption = "Source: ADSL")
  )
  expect_equal(
    input_display_attrs(NULL),
    list(label = NULL, subtitle = NULL, caption = NULL)
  )
  # Empty strings do not count as present.
  attr(d, "subtitle") <- ""
  expect_null(input_display_attrs(d)$subtitle)
})

test_that("chart block round-trips the three title tiers through state", {
  df <- data.frame(ARM = c("Placebo", "Drug A"), AVAL = c(1, 2))
  blk <- new_chart_block(
    chart_type = "bar", group = "ARM",
    title = "AEs by {ARM}", subtitle = "", caption = NULL
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_equal(session$returned$state$title(), "AEs by {ARM}")
      expect_equal(session$returned$state$subtitle(), "")   # explicitly none
      expect_null(session$returned$state$caption())          # auto
    },
    args = list(x = blk, data = list(data = function() df))
  )
})
