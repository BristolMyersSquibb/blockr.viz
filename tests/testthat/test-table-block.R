# Tests for new_table_block.

df <- data.frame(
  parameter = c("ALT", "AST", "BILI"),
  ALT  = c(1.00, 0.80, 0.10),
  AST  = c(0.80, 1.00, 0.20),
  BILI = c(0.10, 0.20, 1.00),
  stringsAsFactors = FALSE
)

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
      expect_equal(session$returned$state$format(), "number")
      # The old filter formals are gone; no click means no selection.
      expect_null(session$returned$state$selection())
      expect_false("filter_type" %in% names(session$returned$state))
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

# What the gear sends: settings as `table_set`, a click as `table_select`.
tb_set <- function(values, rev = 0L) {
  viz_read_set(viz_block_spec("table"), list(rev = rev, values = values))
}

tb_click <- function(column, values, nonce = 1) {
  viz_read_selection(list(
    selection = list(type = "categorical",
                     filters = stats::setNames(list(as.list(values)), column),
                     columns = list()),
    nonce = nonce
  ))
}

test_that("cogwheel settings update color, shadings, drill, format", {
  blk <- new_table_block(
    shadings = list(list(mode = "diverging", columns = "ALT",
                         domain = c(-1, 1))),
    color = "parameter"
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expr <- session$makeScope("expr")
      sh <- session$returned$state$shadings()
      expect_length(sh, 1L)
      expect_equal(sh[[1]]$mode, "diverging")
      expect_equal(unlist(sh[[1]]$domain), c(-1, 1))
      expect_equal(session$returned$state$color(), "parameter")

      # The gear sends the whole shadings list; an empty one is off.
      expr$setInputs(table_set = tb_set(list(shadings = list())))
      session$flushReact()
      expect_length(session$returned$state$shadings(), 0L)

      expr$setInputs(table_set = tb_set(list(
        shadings = list(list(mode = "bar", columns = list("result")))
      )))
      session$flushReact()
      sh2 <- session$returned$state$shadings()
      expect_equal(sh2[[1]]$mode, "bar")
      expect_equal(sh2[[1]]$columns, "result")

      # The Color-by picker's "(none)" is NULL: the color is off.
      expr$setInputs(table_set = tb_set(list(color = NULL)))
      session$flushReact()
      expect_null(session$returned$state$color())

      expr$setInputs(table_set = tb_set(list(drill = "parameter", format = 0L)))
      session$flushReact()
      expect_equal(session$returned$state$drill(), "parameter")
      expect_equal(session$returned$state$format(), 0L)
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

      expr <- session$makeScope("expr")
      expr$setInputs(table_select = tb_click("parameter", "AST"))
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

test_that("empty-list state from a pre-#144 DAG paste heals to the defaults", {
  # Where this bug was first diagnosed: a pasted `row_color` holding list()
  # reached nzchar() and threw "argument is of length zero" on first reconnect.
  blk <- suppressWarnings(new_table_block(
    rowname = list(), value = list(), group = list(), color = list(),
    row_color = list(), cell_color = list(), drill = list(),
    filter_column = list(), filter_values = list(), filter_range = list(),
    filter_group_cols = list(), filter_group_vals = list(),
    filter_spread_col = list(), filter_spread_from = list()
  ))
  payload <- blockr.core::blockr_ser(blk)[["payload"]]

  # `summaries` / `shadings` / `script_values` default to list(): that IS
  # their empty value, not corruption. Every other slot must have healed back
  # to NULL or its default.
  empty_list <- vapply(payload, function(v) is.list(v) && !length(v), logical(1))
  expect_equal(sort(names(payload)[empty_list]),
               c("script_values", "shadings", "summaries"))

  # `color` is "auto" by default and NULL is off, in code as in a saved board.
  expect_identical(
    blockr.core::blockr_ser(new_table_block(color = NULL))[["payload"]]$color,
    NULL
  )
  expect_identical(
    blockr.core::blockr_ser(new_table_block())[["payload"]]$color, "auto"
  )
})

test_that("a pasted empty-list color from an old board restores as auto", {
  # Before the rewrite NULL was the smart default and "" was off, so an
  # emptied slot meant the default.
  expect_identical(legacy_restore("table", list(color = list()))$color, "auto")
})
