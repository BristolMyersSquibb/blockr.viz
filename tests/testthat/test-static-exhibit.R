test_that("static_exhibit renders a table-shaped value as a static table", {
  skip_if_not_installed("flextable")

  # The point of the dispatcher: the STRUCTURE lives in the annotated data
  # frame, so a bare annotated frame (no table block in front of it) prints
  # exactly as the table block's output would.
  tbl <- summary_table(iris, vars = "Sepal.Length", by = "Species")
  attr(tbl, "label") <- "Iris summary"

  ex <- static_exhibit(tbl)
  expect_s3_class(ex, "flextable")
  expect_equal(attr(ex, "pptx_left"), 0.4)

  # identical to the explicit renderer -- one print routine, two entry points
  expect_equal(
    ex$body$dataset,
    static_table(tbl)$body$dataset
  )

  # a plain data frame too (the officer / preview paths always did this)
  expect_s3_class(static_exhibit(head(iris, 3)), "flextable")

  # ... and arguments reach static_table()
  expect_equal(
    static_exhibit(tbl, title = "Overridden")$header$dataset[[1L]][[1L]],
    "Overridden"
  )
})

test_that("static_exhibit returns anything else untouched", {

  # Objects that already print as themselves. A wrap that is a no-op here is
  # what makes it safe to wrap every reported block in a document.
  expect_identical(static_exhibit(1:3), 1:3)
  expect_identical(static_exhibit("a"), "a")
  expect_identical(static_exhibit(NULL), NULL)

  p <- structure(list(), class = c("gg", "ggplot"))
  expect_identical(static_exhibit(p), p)

  gt <- structure(list(), class = "gt_tbl")
  expect_identical(static_exhibit(gt), gt)

  ft <- structure(list(), class = "flextable")
  expect_identical(static_exhibit(ft), ft)

  # an object whose as_annotated_df() method refuses this value degrades to
  # the bare print rather than erroring the document
  refuses <- structure(list(), class = "viz_test_refuses")
  registerS3method("as_annotated_df", "viz_test_refuses",
                   function(x, ...) stop("nope"),
                   envir = asNamespace("blockr.viz"))
  expect_identical(static_exhibit(refuses), refuses)
})

test_that("static_exhibit leaves oversized frames to the bare print", {
  skip_if_not_installed("flextable")

  big <- data.frame(a = 1:50)
  withr::local_options(blockr.viz.static_exhibit_max_rows = 10)
  expect_identical(static_exhibit(big), big)

  withr::local_options(blockr.viz.static_exhibit_max_rows = 100)
  expect_s3_class(static_exhibit(big), "flextable")
})
