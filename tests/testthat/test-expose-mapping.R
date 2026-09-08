# `expose`: which mapping roles a chart block carries on its own face instead
# of behind the gear.

test_that("expose_state() normalizes every shape the client can send", {
  expect_equal(expose_state(NULL), character())
  expect_equal(expose_state(""), character())
  # An empty band is sent as "" precisely because an empty JS array arrives
  # here as NULL and the message handler's !is.null() guard would skip it.
  expect_equal(expose_state(list("color", "facet")), c("color", "facet"))
  expect_equal(expose_state(c("color", "facet")), c("color", "facet"))
  expect_equal(expose_state(list("color", "color")), "color")
  expect_equal(expose_state(list(" color ", "")), "color")
  # Order is the band's render order, so it is preserved rather than sorted.
  expect_equal(expose_state(list("facet", "color")), c("facet", "color"))
})

test_that("a chart block defaults to nothing on its face", {
  blk <- new_chart_block(group = "Species", value = ".count", func = "count")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_equal(session$returned$state$expose(), character())
    },
    args = list(x = blk, data = list(data = function() datasets::iris))
  )
})

test_that("exposed roles round-trip through block state", {
  blk <- new_chart_block(
    group = "Species", value = "Sepal.Length", func = "mean",
    color = "Species", expose = c("color", "value")
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_equal(session$returned$state$expose(), c("color", "value"))
    },
    args = list(x = blk, data = list(data = function() datasets::iris))
  )
})

test_that("the client can pin and unpin roles at runtime", {
  blk <- new_chart_block(group = "Species", value = ".count", func = "count",
                         expose = "color")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      expr_scope <- session$makeScope("expr")
      expr_scope$setInputs(
        drilldown_block_action = list(action = "config",
                                      expose = list("color", "facet"))
      )
      session$flushReact()
      expect_equal(session$returned$state$expose(), c("color", "facet"))

      # Taking the last one off the face has to reach the slot, or a control
      # the builder removed stays on screen for every reader.
      expr_scope$setInputs(
        drilldown_block_action = list(action = "config", expose = "")
      )
      session$flushReact()
      expect_equal(session$returned$state$expose(), character())
    },
    args = list(x = blk, data = list(data = function() datasets::iris))
  )
})

test_that("expose is externally controllable and may be empty", {
  blk <- new_chart_block(group = "Species", value = ".count", func = "count")
  expect_true("expose" %in% blockr.core::external_ctrl_vars(blk))
  # A block whose state field is NULL/empty and NOT in allow_empty_state never
  # becomes ready (reference_blockr_allow_empty_state_wedge), and empty is the
  # default here.
  expect_true("expose" %in% attr(blk, "allow_empty_state"))
})
