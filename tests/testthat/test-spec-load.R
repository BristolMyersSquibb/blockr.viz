# The specs and the block list are built once, when the package loads, where
# covr cannot count them. Built again here, they must come out the same.

test_that("every block's spec builds again to the spec built at load", {
  old <- as.list(spec_cache)
  withr::defer({
    rm(list = ls(spec_cache), envir = spec_cache)
    list2env(old, spec_cache)
  })
  rm(list = ls(spec_cache), envir = spec_cache)
  for (b in names(VIZ_BLOCK_SPECS)) {
    fresh <- viz_block_spec(b)
    expect_equal(fresh, old[[b]], ignore_function_env = TRUE,
                 label = paste(b, "spec"))
  }
  expect_setequal(names(VIZ_BLOCK_SPECS), c("chart", "table"))
})

test_that("register_viz_blocks() lists the chart and the table again", {
  register_viz_blocks()
  blocks <- blockr.core::available_blocks()
  for (id in c("chart_block", "table_block")) {
    b <- blocks[[id]]
    expect_identical(attr(b, "package"), "blockr.viz")
    expect_true(nzchar(attr(b, "guidance")))
    expect_true(blockr.core::is_arg_specs(attr(b, "arguments")))
  }
  expect_match(attr(blocks[["chart_block"]], "guidance"), "type",
               fixed = TRUE)
})
