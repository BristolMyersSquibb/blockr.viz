# Tests for drilldown_table() renderer + new_table_block.

df <- data.frame(
  parameter = c("ALT", "AST", "BILI"),
  ALT  = c(1.00, 0.80, 0.10),
  AST  = c(0.80, 1.00, 0.20),
  BILI = c(0.10, 0.20, 1.00),
  stringsAsFactors = FALSE
)

test_that("drilldown_table returns a tagList with a table", {
  h <- as.character(htmltools::renderTags(drilldown_table(df))$html)
  expect_true(grepl("blockr-table", h))
  expect_true(grepl("blockr-sortable", h))
  expect_true(grepl("drilldown-table-container", h))
})

test_that("color = NULL produces no cell backgrounds", {
  h <- as.character(htmltools::renderTags(drilldown_table(df))$html)
  expect_false(grepl("background:#", h))
})

test_that("diverging color produces cell backgrounds and a dependency", {
  tl <- drilldown_table(
    df, color = drilldown_table_color("diverging", domain = c(-1, 1))
  )
  h <- as.character(htmltools::renderTags(tl)$html)
  expect_true(grepl("background:#", h))
  expect_true(grepl("drilldown-table", h))
})

test_that("NA cells render as a dash and uncolored", {
  d <- df
  d$ALT[2] <- NA
  h <- as.character(htmltools::renderTags(
    drilldown_table(d, color = drilldown_table_color("sequential"))
  )$html)
  expect_true(grepl("&mdash;|—", h))
})

test_that("drill without elem_id emits no action attributes", {
  h <- as.character(htmltools::renderTags(
    drilldown_table(df, drill = "parameter")
  )$html)
  expect_false(grepl("data-dt-elem-id", h))
})

test_that("drill with elem_id wires the action attributes", {
  h <- as.character(htmltools::renderTags(
    drilldown_table(df, drill = "parameter",
                    elem_id = "blk-drilldown_table_block")
  )$html)
  expect_true(grepl("data-dt-elem-id", h))
  expect_true(grepl("data-dt-onclick-col", h))
})

test_that("column labels render as muted text in headers", {
  d <- data.frame(USUBJID = c("S1", "S2"), AVAL = c(1, 2),
                   stringsAsFactors = FALSE)
  attr(d$AVAL, "label") <- "Analysis Value"
  h <- as.character(htmltools::renderTags(drilldown_table(d))$html)
  expect_true(grepl("blockr-col-label", h))
  expect_true(grepl("Analysis Value", h))
  # a label equal to the column name is not repeated
  expect_false(grepl("blockr-col-label[^>]*>USUBJID<", h))
})

test_that("empty data renders a No data table", {
  h <- as.character(htmltools::renderTags(
    drilldown_table(df[0, ])
  )$html)
  expect_true(grepl("No data", h))
})

test_that("degenerate color domain falls back to plain render", {
  flat <- data.frame(g = "x", a = 5, b = 5, stringsAsFactors = FALSE)
  h <- as.character(htmltools::renderTags(
    drilldown_table(flat, color = drilldown_table_color("sequential"))
  )$html)
  expect_false(grepl("background:#", h))
})

test_that("digits controls numeric formatting", {
  d <- data.frame(g = "r", v = 0.12345, stringsAsFactors = FALSE)
  h <- as.character(htmltools::renderTags(
    drilldown_table(d, digits = 1L)
  )$html)
  expect_true(grepl(">0.1<", h))
  expect_false(grepl("0.12345", h))
})

test_that("block state round-trips constructor args", {
  blk <- new_table_block(
    cell_color = drilldown_table_color("diverging", domain = c(-1, 1)),
    drill = "USUBJID"
  )
  expect_s3_class(blk, "table_block")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_equal(session$returned$state$drill(), "USUBJID")
      expect_equal(session$returned$state$digits(), 2L)
      expect_equal(session$returned$state$filter_type(), "categorical")
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("cogwheel config actions update color, drill, digits", {
  blk <- new_table_block(
    cell_color = drilldown_table_color("diverging", domain = c(-1, 1))
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_equal(session$returned$state$cell_color()$type, "diverging")

      es <- session$makeScope("expr")
      es$setInputs(drilldown_table_block_action = list(
        action = "config", param = "color_mode", value = "off"
      ))
      session$flushReact()
      expect_null(session$returned$state$cell_color())

      es$setInputs(drilldown_table_block_action = list(
        action = "config", param = "color_mode", value = "diverging"
      ))
      session$flushReact()
      # constructor domain is preserved when toggling back to same type
      expect_equal(session$returned$state$cell_color()$domain, c(-1, 1))

      es$setInputs(drilldown_table_block_action = list(
        action = "config", param = "drill", value = "parameter"
      ))
      es$setInputs(drilldown_table_block_action = list(
        action = "config", param = "digits", value = "0"
      ))
      session$flushReact()
      expect_equal(session$returned$state$drill(), "parameter")
      expect_equal(session$returned$state$digits(), 0L)
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("no click = pass-through, click filters the data", {
  blk <- new_table_block(drill = "parameter")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_equal(nrow(session$returned$result()), 3L)

      expr_scope <- session$makeScope("expr")
      expr_scope$setInputs(drilldown_table_block_action = list(
        action = "filter", column = "parameter",
        values = list("AST"), filter_type = "categorical"
      ))
      session$flushReact()
      res <- session$returned$result()
      expect_equal(nrow(res), 1L)
      expect_equal(res$parameter, "AST")
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("fmt_to_wide tolerates empty/NA group levels (no pivot crash)", {
  # Regression: a blank/NA `.group` (e.g. an untreated subject's empty TRT01A
  # arm) used to abort pivot_wider ("spec$.name can't contain the empty
  # string"), and because the table block calls fmt_to_wide() inside a plain
  # observe(), that escaped as a fatal session disconnect.
  mk <- function(g) {
    data.frame(.label = "Mean", .group = g,
               .fmt = "{n}", n = seq_along(g), stringsAsFactors = FALSE)
  }
  for (g in list(c("Drug X", ""), c("Drug X", NA_character_))) {
    w <- expect_no_error(fmt_to_wide(mk(g)))
    expect_true("(Missing)" %in% names(w))
  }
  # Well-formed groups are untouched.
  w <- fmt_to_wide(mk(c("Drug X", "Drug Y")))
  expect_true(all(c("Drug X", "Drug Y") %in% names(w)))
  expect_false("(Missing)" %in% names(w))
})
