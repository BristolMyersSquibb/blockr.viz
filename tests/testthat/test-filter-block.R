# Tests for Filter Block

test_that("bi_filter_block constructor creates correct class", {
  blk <- new_bi_filter_block()
  expect_s3_class(blk, c("bi_filter_block", "transform_block", "block"))
})

test_that("empty state produces pass-through expression", {
  expr <- make_filter_block_expr(character(), list(), list(), iris)
  expect_identical(expr, bquote(dplyr::filter(data, TRUE)))
})

test_that("multi-select empty values skip the column (pass-through)", {
  expr <- make_filter_block_expr(
    columns = "Species",
    modes = list(Species = "multi"),
    values = list(Species = character()),
    df = iris
  )
  expect_identical(expr, bquote(dplyr::filter(data, TRUE)))
})

test_that("single-value filter uses %in%", {
  expr <- make_filter_block_expr(
    columns = "Species",
    modes = list(Species = "single"),
    values = list(Species = "setosa"),
    df = iris
  )
  expected <- as.call(list(
    quote(dplyr::filter),
    quote(data),
    bquote(Species %in% "setosa")
  ))
  expect_identical(expr, expected)
})

test_that("multi-value filter uses %in% with vector", {
  expr <- make_filter_block_expr(
    columns = "Species",
    modes = list(Species = "multi"),
    values = list(Species = c("setosa", "versicolor")),
    df = iris
  )
  vec <- c("setosa", "versicolor")
  expected <- as.call(list(
    quote(dplyr::filter),
    quote(data),
    bquote(Species %in% .(vec))
  ))
  expect_identical(expr, expected)
})

test_that("numeric columns coerce string values to numeric", {
  df <- data.frame(x = c(1, 2, 3), y = c("a", "b", "c"))
  expr <- make_filter_block_expr(
    columns = "x",
    modes = list(x = "single"),
    values = list(x = "2"),
    df = df
  )
  # Expression should carry numeric 2, not string "2"
  expected <- as.call(list(
    quote(dplyr::filter),
    quote(data),
    bquote(x %in% 2)
  ))
  expect_identical(expr, expected)
})

test_that("multiple columns combine with &", {
  expr <- make_filter_block_expr(
    columns = c("Species", "Sepal.Length"),
    modes = list(Species = "single", Sepal.Length = "multi"),
    values = list(Species = "setosa", Sepal.Length = c("5.0", "5.1")),
    df = iris
  )
  nums <- c(5, 5.1)
  inner <- bquote(Species %in% "setosa" & Sepal.Length %in% .(nums))
  expected <- as.call(list(quote(dplyr::filter), quote(data), inner))
  expect_equal(expr, expected)
})

test_that("enforce_single_rule fills empty single-select with first value", {
  s <- list(
    columns = "Species",
    modes = list(Species = "single"),
    values = list()
  )
  out <- enforce_single_rule(s, iris)
  expect_equal(out$values$Species, "setosa")
})

test_that("enforce_single_rule leaves non-empty selections alone", {
  s <- list(
    columns = "Species",
    modes = list(Species = "single"),
    values = list(Species = "virginica")
  )
  out <- enforce_single_rule(s, iris)
  expect_equal(out$values$Species, "virginica")
})

test_that("enforce_single_rule does not touch multi-select columns", {
  s <- list(
    columns = "Species",
    modes = list(Species = "multi"),
    values = list(Species = character())
  )
  out <- enforce_single_rule(s, iris)
  expect_equal(out$values$Species, character())
})

test_that("build_column_meta surfaces column labels when present", {
  df <- data.frame(x = 1:3, y = c("a", "b", "c"))
  attr(df$x, "label") <- "X axis"
  meta <- build_column_meta(df)
  expect_equal(meta[[1]], list(value = "x", label = "X axis"))
  expect_equal(meta[[2]], list(value = "y", label = ""))
})

test_that("build_value_options returns plain values for unlabelled columns", {
  df <- data.frame(g = c("b", "a", "b", "c"), stringsAsFactors = FALSE)
  vals <- build_value_options(df)
  # Sorted alphabetically for character columns.
  expect_equal(vals$g, list("a", "b", "c"))
})

test_that("build_value_options honors haven-style value labels", {
  df <- data.frame(sex = c(1L, 2L, 1L))
  attr(df$sex, "labels") <- c(Male = 1L, Female = 2L)
  vals <- build_value_options(df)
  expect_equal(vals$sex[[1]], list(value = "1", label = "Male"))
  expect_equal(vals$sex[[2]], list(value = "2", label = "Female"))
})

test_that("single-numeric-column filter coerces string to numeric", {
  df <- data.frame(x = c(1, 2, 3))
  expr <- make_filter_block_expr(
    columns = "x",
    modes = list(x = "multi"),
    values = list(x = c("1", "3")),
    df = df
  )
  nums <- c(1, 3)
  expected <- as.call(list(
    quote(dplyr::filter),
    quote(data),
    bquote(x %in% .(nums))
  ))
  expect_identical(expr, expected)
})
