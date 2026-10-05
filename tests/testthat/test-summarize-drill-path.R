# The summarize table's row click claims the row's grouping path
# (R/summarize-drill.R): a parent row its outer column, a child row both.

dp_toy <- function() {
  data.frame(
    USUBJID = c("1", "1", "2", "3", "4", "5"),
    SEX = c("F", "F", "F", "M", "M", "M"),
    ARM = c("Placebo", "Active", "Placebo", "Placebo", "Active", "Active"),
    stringsAsFactors = FALSE
  )
}

dp_block <- function(...) {
  new_summarize_table_block(
    by = c("SEX", "ARM"),
    summaries = list(list(type = "simple", name = "n",
                          func = "count_distinct", col = "USUBJID",
                          show = "bar")),
    drill = "ARM",
    ...
  )
}

dp_click <- function(scope, column, values) {
  scope$setInputs(summarize_table_block_action = list(
    action = "filter", type = "categorical",
    column = as.list(column), values = as.list(values), nonce = 1
  ))
}

test_that("the stored click reads as filters, old single-column form too", {
  expect_identical(summarize_drill_filters(NULL, NULL), list())
  expect_identical(summarize_drill_filters("ARM", c("A", "B")),
                   list(ARM = c("A", "B")))
  expect_identical(summarize_drill_filters(c("SEX", "ARM"), c("F", "A")),
                   list(SEX = "F", ARM = "A"))
  # Unpaired: no click rather than a wrong one.
  expect_identical(summarize_drill_filters(c("SEX", "ARM"), "F"), list())
  expect_identical(
    summarize_drill_phrase(list(SEX = "F", ARM = "Placebo")),
    "SEX = F, ARM = Placebo"
  )
})

test_that("a parent row filters its outer column only", {
  blk <- dp_block()
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      scope <- session$makeScope("expr")
      session$flushReact()
      dp_click(scope, "SEX", "F")
      session$flushReact()
      out <- eval(session$returned$expr(), list(data = dp_toy()))
      expect_setequal(out$USUBJID, c("1", "2"))
      expect_identical(session$returned$state$filter_column(), "SEX")
    },
    args = list(x = blk, data = list(data = function() dp_toy()))
  )
})

test_that("a child row filters under its own parent, not every parent", {
  blk <- dp_block()
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      scope <- session$makeScope("expr")
      session$flushReact()
      # "Placebo" sits under F and under M: the click names the F one.
      dp_click(scope, c("SEX", "ARM"), c("F", "Placebo"))
      session$flushReact()
      out <- eval(session$returned$expr(), list(data = dp_toy()))
      expect_setequal(out$USUBJID, c("1", "2"))
      expect_true(all(out$SEX == "F" & out$ARM == "Placebo"))
      st <- session$returned$state
      expect_identical(st$filter_column(), c("SEX", "ARM"))
      expect_identical(st$filter_values(), c("F", "Placebo"))
    },
    args = list(x = blk, data = list(data = function() dp_toy()))
  )
})

test_that("a board saved with one drill column restores its filter", {
  blk <- dp_block(filter_column = "ARM", filter_values = "Placebo")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      out <- eval(session$returned$expr(), list(data = dp_toy()))
      expect_setequal(out$USUBJID, c("1", "2", "3"))
    },
    args = list(x = blk, data = list(data = function() dp_toy()))
  )
})

test_that("the payload carries the path and lights the clicked row only", {
  d <- dp_toy()
  build <- function(col, vals) {
    summarize_build_payload(
      d, drill = "ARM", active = list(col = col, vals = vals),
      by = c("SEX", "ARM"),
      summaries = list(list(type = "simple", name = "n",
                            func = "count_distinct", col = "USUBJID",
                            show = "bar"))
    )
  }
  key <- function(p) {
    lev <- p$level %||% rep(0L, p$n)
    par <- p$parent %||% rep(NA_character_, p$n)
    paste0(ifelse(lev > 0L, paste0(par, "/"), ""), p$label)
  }

  p <- build(c("SEX", "ARM"), c("F", "Placebo"))
  expect_identical(as.character(p$path), c("SEX", "ARM"))
  expect_identical(key(p)[p$on], "F/Placebo")
  expect_identical(p$chrome$foot$filter, "SEX = F, ARM = Placebo")

  p <- build("SEX", "M")
  expect_identical(key(p)[p$on], "M")

  # The old single-column click lights the term under every parent, as it
  # filtered.
  p <- build("ARM", "Placebo")
  expect_setequal(key(p)[p$on], c("F/Placebo", "M/Placebo"))

  # No drill, no path.
  p <- summarize_build_payload(
    d, by = c("SEX", "ARM"),
    summaries = list(list(type = "simple", name = "n",
                          func = "count_distinct", col = "USUBJID",
                          show = "bar"))
  )
  expect_null(p$path)
})

test_that("a path claim reaches the target as one condition per column", {
  cl <- dd_ctrl_claims(dp_toy(), "", list(SEX = "F", ARM = "Placebo"))
  expect_length(cl, 2L)
  expect_identical(vapply(cl, `[[`, "", "name"), c("SEX", "ARM"))
})
