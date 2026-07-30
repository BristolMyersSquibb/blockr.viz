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

test_that("an HTML target gets the HTML table, everything else flextable", {
  skip_if_not_installed("flextable")

  df <- data.frame(a = 1:3, b = 4:6)

  # Outside knitr (the officer deck path evaluates the board's code
  # in-process) the target is not HTML.
  expect_s3_class(static_exhibit(df), "flextable")

  local_mocked_bindings(exhibit_html_output = function() TRUE)
  expect_s3_class(static_exhibit(df), "shiny.tag.list")
})

test_that("a tall table opens collapsed only when it has sections to collapse", {
  # Height alone is not the trigger: a flat table has nothing to click to get
  # its rows back, so it stays expanded however long it is.
  flat <- data.frame(.label = paste0("r", 1:40), v = 1:40)
  expect_true(html_exhibit_expanded(flat))

  # Nesting through `.indent` -- the dialect nest_hierarchies = TRUE emits.
  nested <- data.frame(
    .label = c("SOC", paste0("PT", 1:39)),
    .indent = c(0L, rep(1L, 39L)),
    v = 1:40
  )
  expect_false(html_exhibit_expanded(nested))

  # Short enough to fit: expanded, structure or not.
  withr::local_options(blockr.viz.html_exhibit_expanded_max_rows = 100)
  expect_true(html_exhibit_expanded(nested))
})

test_that("html_table_collapsible reads both collapsible structures", {
  expect_true(html_table_collapsible(data.frame(a = 1), "sec"))

  expect_false(html_table_collapsible(data.frame(a = 1:3)))
  expect_false(
    html_table_collapsible(data.frame(a = 1:3, .indent = c(0L, 0L, 0L)))
  )
  expect_true(
    html_table_collapsible(data.frame(a = 1:3, .indent = c(0L, 1L, 1L)))
  )

  # A row that is only ever LESS indented than its predecessor closes a group,
  # it does not open one.
  expect_false(
    html_table_collapsible(data.frame(a = 1:3, .indent = c(2L, 1L, 0L)))
  )
})
