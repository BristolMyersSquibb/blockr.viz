# `expose` put mapping rows in an always-visible band above the chart. There is
# one channel now and it is the block's own sentence (`subtitle`, whose {@arg}
# words are the controls). A saved board still carries the old argument, so it
# has to arrive without wedging the block.

test_that("a board saved with `expose` still restores, minus the band", {
  expect_warning(
    blk <- blockr.core::blockr_deser(
      structure(list(), class = "chart_block"),
      list(payload = list(type = "bar", category = "Species", func = "count",
                          expose = c("color", "facet")),
           constructor = list(version = "0.2.9"))
    ),
    "expose"
  )
  expect_s3_class(blk, "chart_block")
  expect_false("expose" %in% names(formals(new_chart_block)))
})

test_that("`expose` in code warns and is ignored (behaviour-changes.md 11)", {
  expect_warning(
    blk <- new_chart_block(type = "bar", category = "a", expose = "color"),
    "`expose` is retired. Name the setting in `subtitle` instead"
  )
  expect_null(attr(blk, "expose"))
  expect_false("expose" %in% names(blockr.core::blockr_ser(blk)$payload))
})

test_that("the retired argument is off the AI surface", {
  args <- viz_registry_args(viz_block_spec("chart"))
  expect_false("expose" %in% names(args))
  # And the sentence that replaced it is described where the assistant reads.
  expect_match(
    blockr.core::arg_spec_description(args$title), "\\{@arg\\}",
    fixed = FALSE
  )
})
