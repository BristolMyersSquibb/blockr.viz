# mark_column_kinds() / column_kinds(): what a column is FOR, as an attribute
# the mapping pickers narrow themselves by.

test_that("marking sets kinds and reading returns only marked columns", {
  d <- mark_column_kinds(
    datasets::iris,
    group = "Species",
    value = c("Sepal.Length", "Sepal.Width")
  )

  expect_equal(
    column_kinds(d),
    c(Sepal.Length = "value", Sepal.Width = "value", Species = "group")
  )
  # Unmarked columns stay unmarked rather than picking up a default: an
  # unmarked column is the "no opinion" state, not a fifth kind.
  expect_false("Petal.Length" %in% names(column_kinds(d)))
  expect_null(attr(d$Petal.Length, "blockr_kind"))
})

test_that("nothing marked reads as an empty vector, never NULL", {
  expect_length(column_kinds(datasets::iris), 0L)
  expect_type(column_kinds(datasets::iris), "character")
})

test_that("marks accumulate across calls and .clear starts over", {
  d <- mark_column_kinds(datasets::iris, group = "Species")
  d <- mark_column_kinds(d, value = "Sepal.Length")
  expect_named(column_kinds(d), c("Sepal.Length", "Species"))

  d <- mark_column_kinds(d, value = "Petal.Width", .clear = TRUE)
  expect_equal(column_kinds(d), c(Petal.Width = "value"))
})

test_that("an unknown kind is an error, not a silently ignored argument", {
  expect_error(
    mark_column_kinds(datasets::iris, colour = "Species"),
    "Unknown column kind"
  )
  expect_error(
    mark_column_kinds(datasets::iris, "Species"),
    "must be named for its kind"
  )
})

test_that("a column that is not there warns and is skipped", {
  expect_warning(
    d <- mark_column_kinds(datasets::iris, group = c("Species", "TRT")),
    "no such column"
  )
  expect_equal(column_kinds(d), c(Species = "group"))
})

test_that("a column marked twice warns and takes the last kind", {
  expect_warning(
    d <- mark_column_kinds(datasets::iris, group = "Species", id = "Species"),
    "marked twice"
  )
  expect_equal(column_kinds(d), c(Species = "id"))
})

test_that("kinds survive the verbs a clinical chain actually uses", {
  d <- mark_column_kinds(datasets::iris, group = "Species")

  expect_equal(column_kinds(dplyr::filter(d, Species == "setosa")),
               c(Species = "group"))
  expect_equal(column_kinds(dplyr::select(d, Species, Sepal.Length)),
               c(Species = "group"))
  expect_equal(column_kinds(dplyr::mutate(d, z = 1)), c(Species = "group"))
  expect_equal(column_kinds(dplyr::arrange(d, Sepal.Length)),
               c(Species = "group"))
  # Base row subsetting and joins are why the mark lives on the FRAME: a
  # per-column attribute is dropped by both.
  expect_equal(column_kinds(d[d$Species == "setosa", ]), c(Species = "group"))
  expect_equal(
    column_kinds(dplyr::left_join(
      d, data.frame(Species = unique(d$Species), k = 1), by = "Species"
    )),
    c(Species = "group")
  )
})

test_that("a mark for a dropped column is ignored, not an error", {
  d <- mark_column_kinds(datasets::iris, group = "Species", value = "Petal.Width")
  expect_equal(column_kinds(dplyr::select(d, -Petal.Width)),
               c(Species = "group"))
})

test_that("the browser payload carries the kind of every marked column", {
  d <- mark_column_kinds(
    data.frame(
      TRT = c("A", "B", "A"), AVAL = c(1, 2, 3),
      NOTE = c("x", "y", "z"), stringsAsFactors = FALSE
    ),
    group = "TRT", value = "AVAL"
  )

  meta <- dd_col_meta(d)
  names(meta) <- vapply(meta, `[[`, character(1L), "name")

  expect_equal(meta$TRT$kind, "group")
  expect_equal(meta$AVAL$kind, "value")
  # An unmarked column carries no `kind` field at all, which is what the
  # client reads as "this frame has opinions about some columns, not this
  # one". A frame with NO kinds anywhere is what turns the filtering off.
  expect_null(meta$NOTE$kind)
  expect_true(all(vapply(dd_col_meta(datasets::iris),
                         function(c) is.null(c$kind), logical(1L))))
})

test_that("column metadata still carries labels, types and factor order", {
  d <- data.frame(g = factor(c("b", "a"), levels = c("b", "a")), n = c(1, 2))
  attr(d$n, "label") <- "Analysis Value"

  meta <- dd_col_meta(d)
  names(meta) <- vapply(meta, `[[`, character(1L), "name")

  expect_equal(meta$g$type, "categorical")
  expect_equal(meta$g$levels, list("b", "a"))
  expect_equal(meta$n$type, "numeric")
  expect_equal(meta$n$label, "Analysis Value")
})
