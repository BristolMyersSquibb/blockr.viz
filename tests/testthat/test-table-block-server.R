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

# ---- structured (annotated df) drill ---------------------------------------
# The structured click emits the grouped `filters` payload over the identity
# columns plus a `spread` instruction; the expr must filter the annotated df
# AND name the selection as a real column (the dm-bridge contract).

structured_fixture <- function() {
  adae <- data.frame(
    AETOXGR = c(1, 1, 2, 3, 1, 2),
    AEDECOD = c("ABDOMINAL PAIN", "AGITATION", "ABDOMINAL PAIN",
                "ANXIETY", "AGITATION", "ANXIETY"),
    ARM     = c("Placebo", "Placebo", "Drug", "Drug", "Placebo", "Drug"),
    stringsAsFactors = FALSE
  )
  summary_table(adae, vars = c("AETOXGR", "AEDECOD"), by = "ARM")
}

test_that("a structured level-row click filters the annotated df and spreads the column", {
  wide <- structured_fixture()
  blk <- new_table_block(drill = "auto")

  testServer(blk$expr_server, args = list(data = reactive(wide)), {
    # The payload table.js emits from the clicked row's data-dd-keys/-spread.
    session$setInputs(
      drilldown_table_block_action = list(
        action = "filter",
        filter_type = "categorical",
        filters = list(
          list(column = ".variable",       value = "AEDECOD"),
          list(column = ".variable_level", value = "ABDOMINAL PAIN")
        ),
        spread = list(column = "AEDECOD", from = ".variable_level")
      )
    )

    expect_equal(session$returned$state$filter_spread_col(), "AEDECOD")
    expect_equal(session$returned$state$filter_spread_from(), ".variable_level")

    out <- eval_block_expr(session$returned$expr(), wide)
    expect_equal(nrow(out), 1L)
    expect_equal(out$.label, "ABDOMINAL PAIN")
    # The dm-bridge contract: the selection appears under its real name.
    expect_true("AEDECOD" %in% names(out))
    expect_equal(out$AEDECOD, "ABDOMINAL PAIN")
  })
})

test_that("a stat-row click filters without spreading (no source-value claim)", {
  wide <- structured_fixture()
  blk <- new_table_block(drill = "auto")

  testServer(blk$expr_server, args = list(data = reactive(wide)), {
    session$setInputs(
      drilldown_table_block_action = list(
        action = "filter",
        filter_type = "categorical",
        filters = list(
          list(column = ".variable", value = "AETOXGR"),
          list(column = ".label",    value = "Mean (SD)")
        ),
        spread = NULL
      )
    )

    expect_null(session$returned$state$filter_spread_col())
    out <- eval_block_expr(session$returned$expr(), wide)
    expect_equal(nrow(out), 1L)
    expect_equal(out$.label, "Mean (SD)")
    expect_false("AETOXGR" %in% names(out))
  })
})

test_that("clearing a structured drill resets the spread with the keys", {
  wide <- structured_fixture()
  blk <- new_table_block(drill = "auto")

  testServer(blk$expr_server, args = list(data = reactive(wide)), {
    session$setInputs(
      drilldown_table_block_action = list(
        action = "filter",
        filter_type = "categorical",
        filters = list(
          list(column = ".variable",       value = "AEDECOD"),
          list(column = ".variable_level", value = "ANXIETY")
        ),
        spread = list(column = "AEDECOD", from = ".variable_level")
      )
    )
    session$setInputs(
      drilldown_table_block_action = list(
        action = "filter",
        column = NULL, values = NULL, filter_type = "categorical"
      )
    )
    expect_null(session$returned$state$filter_spread_col())
    expect_null(session$returned$state$filter_group_cols())
    out <- eval_block_expr(session$returned$expr(), wide)
    expect_equal(nrow(out), nrow(wide))
    expect_false("AEDECOD" %in% names(out))
  })
})

test_that("structured drill state restores through the constructor", {
  wide <- structured_fixture()
  blk <- new_table_block(
    drill = "auto",
    filter_group_cols  = c(".variable", ".variable_level"),
    filter_group_vals  = c("AEDECOD", "ABDOMINAL PAIN"),
    filter_spread_col  = "AEDECOD",
    filter_spread_from = ".variable_level"
  )

  testServer(blk$expr_server, args = list(data = reactive(wide)), {
    out <- eval_block_expr(session$returned$expr(), wide)
    expect_equal(nrow(out), 1L)
    expect_equal(out$AEDECOD, "ABDOMINAL PAIN")
  })
})
