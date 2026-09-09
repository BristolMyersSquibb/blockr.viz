# `expose` put mapping rows in an always-visible band above the chart. There is
# one channel now and it is the block's own sentence (`subtitle`, whose {@arg}
# words are the controls). A saved board still carries the old argument, so it
# has to arrive without wedging the block.

test_that("a board saved with `expose` still restores, minus the band", {
  expect_warning(
    blk <- new_chart_block(
      group = "Species", value = ".count", func = "count",
      expose = c("color", "facet")
    ),
    "retired"
  )
  # It is gone from the block's state, so it cannot serialize under the old
  # name again.
  expect_false("expose" %in% names(formals(new_chart_block)))
})

test_that("the retired argument is off the AI surface", {
  args <- chart_arguments()
  expect_false("expose" %in% names(args))
  # And the sentence that replaced it is described where the assistant reads.
  expect_match(
    blockr.core:::arg_spec_description(args$subtitle), "\\{@arg\\}",
    fixed = FALSE
  )
})
