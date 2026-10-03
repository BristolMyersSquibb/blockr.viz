# Bar value labels (`value_labels`, #42): the argument, its state round-trip
# and the browser payload.

vl_data <- function() {
  data.frame(
    g = c("a", "a", "b", "b", "c"),
    k = c("x", "y", "x", "y", "x"),
    v = c(3, 2, 5, -4, 1234.5678),
    stringsAsFactors = FALSE
  )
}

vl_state <- function(blk) {
  out <- NULL
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      out <<- session$returned$state$value_labels()
    },
    args = list(x = blk, data = list(data = vl_data))
  )
  out
}

test_that("value_labels is off by default and stored as a logical", {
  expect_false(eval(formals(new_chart_block)$value_labels))
  expect_false(vl_state(new_chart_block(group = "g")))
  expect_true(vl_state(new_chart_block(group = "g", value_labels = TRUE)))
  # The gear's transport strings restore too.
  expect_true(vl_state(new_chart_block(group = "g", value_labels = "on")))
  expect_false(vl_state(new_chart_block(group = "g", value_labels = "off")))
})

test_that("value_labels is registered with the other chart args", {
  expect_true("value_labels" %in% names(chart_arguments()))
  blk <- new_chart_block(group = "g")
  expect_true("value_labels" %in% blockr.core::external_ctrl_vars(blk))
})

test_that("value_labels round-trips through serialization", {
  blk <- new_chart_block(group = "g", value = "v", func = "sum",
                         value_labels = TRUE)
  ser <- blockr.core::blockr_ser(blk)
  expect_true(ser[["payload"]]$value_labels)
  back <- blockr.core::blockr_deser(ser)
  expect_true(vl_state(back))

  off <- blockr.core::blockr_ser(new_chart_block(group = "g"))
  expect_false(off[["payload"]]$value_labels)
})

test_that("the gear switch reaches state and the browser payload", {
  blk <- new_chart_block(group = "g", value = "v", func = "sum")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_false(session$returned$state$value_labels())
      expr_scope <- session$makeScope("expr")
      expr_scope$setInputs(drilldown_block_action = list(
        action = "config", value_labels = "on"
      ))
      session$flushReact()
      expect_true(session$returned$state$value_labels())
      expr_scope$setInputs(drilldown_block_action = list(
        action = "config", value_labels = "off"
      ))
      session$flushReact()
      expect_false(session$returned$state$value_labels())
    },
    args = list(x = blk, data = list(data = vl_data))
  )
})
