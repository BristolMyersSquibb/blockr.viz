# Browser-free interaction test for the table block's server logic. This
# drives the SAME contract the client JS uses: the gear/drill popover and the
# drilldown filter send an `input$drilldown_table_block_action` message, and the
# server turns it into reactive state + a dplyr filter expression. This always
# runs (no headless browser needed) and complements the shinytest2 test in
# test-table-block-browser.R.

library(shiny)

# Evaluate a block's bbquote()'d expression. blockr.core's board substitutes
# the upstream data into the `.(data)` placeholder; here we do the same by
# binding `data` and unwrapping the `.()` placeholder.
eval_block_expr <- function(ex, data) {
  e <- new.env(parent = globalenv())
  e$data <- data
  e$. <- function(x) x
  eval(ex, envir = e)
}

test_that("table block passes data through unfiltered by default", {
  df <- data.frame(
    grp = c("A", "A", "B", "C"),
    val = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )
  blk <- new_table_block(values = "val")

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    out <- eval_block_expr(session$returned$expr(), df)
    expect_equal(nrow(out), 4L)
    # no filter set yet
    expect_null(session$returned$state$filter_column())
  })
})

test_that("a single-value filter message filters the table and updates state", {
  df <- data.frame(
    grp = c("A", "A", "B", "C"),
    val = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )
  blk <- new_table_block(values = "val")

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    # the exact message the drilldown JS emits when the user picks a filter
    session$setInputs(
      drilldown_table_block_action = list(
        action = "filter",
        column = "grp",
        values = list("A")
      )
    )

    expect_equal(session$returned$state$filter_column(), "grp")
    expect_equal(session$returned$state$filter_values(), list("A"))

    out <- eval_block_expr(session$returned$expr(), df)
    expect_equal(nrow(out), 2L)
    expect_equal(unique(out$grp), "A")
  })
})

test_that("a multi-value filter message uses %in% and keeps all matches", {
  df <- data.frame(
    grp = c("A", "A", "B", "C"),
    val = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )
  blk <- new_table_block(values = "val")

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$setInputs(
      drilldown_table_block_action = list(
        action = "filter",
        column = "grp",
        values = list("A", "B")
      )
    )

    out <- eval_block_expr(session$returned$expr(), df)
    expect_equal(nrow(out), 3L)
    expect_setequal(unique(out$grp), c("A", "B"))
  })
})

test_that("a config message updates the drill state", {
  df <- data.frame(
    grp = c("A", "B"),
    val = c(1, 2),
    stringsAsFactors = FALSE
  )
  blk <- new_table_block(values = "val")

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$setInputs(
      drilldown_table_block_action = list(
        action = "config",
        param  = "drill",
        value  = "grp"
      )
    )
    expect_equal(session$returned$state$drill(), "grp")

    # "(none)" clears it back to NULL
    session$setInputs(
      drilldown_table_block_action = list(
        action = "config",
        param  = "drill",
        value  = "(none)"
      )
    )
    expect_null(session$returned$state$drill())
  })
})
