test_that("ft_table renders an annotated frame as a themed flextable", {
  skip_if_not_installed("flextable")

  tbl <- summary_table(iris, vars = c("Sepal.Length", "Sepal.Width"),
                       by = "Species")
  attr(tbl, "label") <- "Iris summary"
  attr(tbl, "subtitle") <- "Sepal measures"
  attr(tbl, "caption") <- "Source: iris"

  ft <- ft_table(tbl)
  expect_s3_class(ft, "flextable")

  # pptx placement attributes (the officer deck renderer reads these)
  expect_equal(attr(ft, "pptx_left"), 0.4)
  expect_equal(attr(ft, "pptx_top"), 1.1)

  # 2 variable blocks -> 2 interleaved section-header rows + 2 stat rows
  expect_equal(flextable::nrow_part(ft, "body"), 4L)
  stub <- ft$body$dataset[[1L]]
  expect_equal(stub, c("Sepal.Length", "Mean (SD)", "Sepal.Width", "Mean (SD)"))

  # title + subtitle header lines above the leaf-label row, caption footer
  expect_equal(flextable::nrow_part(ft, "header"), 3L)
  expect_equal(flextable::nrow_part(ft, "footer"), 1L)
})

test_that("title/subtitle/caption: NULL = auto from attrs, '' = off", {
  skip_if_not_installed("flextable")

  tbl <- summary_table(iris, vars = "Sepal.Length", by = "Species")
  attr(tbl, "label") <- "Auto title"

  # auto: label attr becomes a title line
  expect_equal(flextable::nrow_part(ft_table(tbl), "header"), 2L)
  # "" suppresses it
  expect_equal(flextable::nrow_part(ft_table(tbl, title = ""), "header"), 1L)
  # no caption -> no footer part rows
  expect_equal(flextable::nrow_part(ft_table(tbl), "footer"), 0L)
})

test_that("two-level by produces a merged spanner header row", {
  skip_if_not_installed("flextable")

  tbl <- summary_table(mtcars, vars = "mpg", by = c("cyl", "am"))
  ft <- ft_table(tbl, title = "")
  # spanner row + leaf-label row
  expect_equal(flextable::nrow_part(ft, "header"), 2L)
  expect_true(any(grepl("||", names(ft$body$dataset), fixed = TRUE)))
})

test_that("row styling flags map onto the flextable", {
  skip_if_not_installed("flextable")

  df <- data.frame(
    .label = c("All", "Level A", "Level B"),
    .indent = c(0L, 1L, 1L),
    .strong = c(TRUE, FALSE, FALSE),
    .emph = c(FALSE, FALSE, TRUE),
    Value = c("10", NA, "3"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  ft <- ft_table(df)

  expect_equal(flextable::nrow_part(ft, "body"), 3L)
  # NA cell -> na_rep em dash
  expect_equal(ft$body$dataset$Value[2L], "—")
  # .strong row bold across the row, .emph row italic
  expect_true(all(ft$body$styles$text$bold$data[1L, ]))
  expect_true(all(ft$body$styles$text$italic$data[3L, ]))
  expect_false(any(ft$body$styles$text$bold$data[2L, ]))
})

test_that("fit_width distributes columns to fill the slide width", {
  skip_if_not_installed("flextable")

  tbl <- summary_table(iris, vars = "Sepal.Length", by = "Species")
  ft <- ft_table(tbl, title = "", fit_width = 12,
                 first_col_width = 6, other_cols_width = 3)
  w <- ft$body$colwidths
  expect_equal(sum(w), 12, tolerance = 1e-6)
  # stub capped at half the budget; three data cols share the rest
  expect_equal(w[[1L]], 6)
  expect_equal(unname(w[-1L]), rep(2, 3), tolerance = 1e-6)

  # option default is read when the argument is omitted
  withr::local_options(blockr.viz.ft_fit_width = 9)
  ft2 <- ft_table(tbl, title = "", first_col_width = 6)
  expect_equal(sum(ft2$body$colwidths), 9, tolerance = 1e-6)
})

test_that("plain data frames render without annotations", {
  skip_if_not_installed("flextable")

  ft <- ft_table(head(mtcars[, 1:3]), title = "Plain")
  expect_s3_class(ft, "flextable")
  expect_equal(flextable::nrow_part(ft, "body"), 6L)
})

test_that("unnamed header_bg cycles the pool by column group", {
  # Flat columns: each its own group -> pool in order, cycling.
  b <- resolve_header_bands(c("#A59F9F", "#33D6F1"), c("", "", ""),
                            c("a", "b", "c"))
  expect_equal(b$bg, c("#A59F9F", "#33D6F1", "#A59F9F"))
  # contrast text is a readable hex chosen by luminance
  expect_match(b$text[[1L]], "^#")
  expect_true(is.na(b$stub_bg))

  # Spanner groups: leaves under one top share one pool color (arm = band).
  b2 <- resolve_header_bands(c("#A59F9F", "#33D6F1", "#FDA97C"),
                             c("4", "4", "6", "6"), c("c1", "c2", "c3", "c4"))
  expect_equal(b2$bg, c("#A59F9F", "#A59F9F", "#33D6F1", "#33D6F1"))
})

test_that("named header_bg pins arms; unnamed fill the rest; .stub pins stub", {
  # Placebo pinned grey whatever its position; the other arm draws the pool.
  b <- resolve_header_bands(
    c(.stub = "#EEEEEE", Placebo = "grey", "#33D6F1"),
    c("", ""), c("Placebo", "Drug 6 mg")
  )
  expect_equal(b$bg[[1L]], "grey")        # Placebo pinned
  expect_equal(b$bg[[2L]], "#33D6F1")     # Drug from the pool
  expect_equal(b$stub_bg, "#EEEEEE")

  # An arm with no pin and no pool -> NA (unfilled), not an error.
  b2 <- resolve_header_bands(c(Placebo = "grey"), c("", ""),
                             c("Placebo", "Drug"))
  expect_equal(b2$bg[[1L]], "grey")
  expect_true(is.na(b2$bg[[2L]]))
})

test_that("header_bg off / empty resolves to no bands", {
  expect_null(resolve_header_bands("none", c("", ""), c("a", "b")))
  expect_null(resolve_header_bands(NULL, c("", ""), c("a", "b")))
  expect_null(resolve_header_bands(FALSE, c("", ""), c("a", "b")))
  expect_null(resolve_header_bands("#A59F9F", character(), character()))
})
