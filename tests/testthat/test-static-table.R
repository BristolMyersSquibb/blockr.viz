test_that("static_table renders an annotated frame as a themed flextable", {
  skip_if_not_installed("flextable")

  tbl <- summary_table(iris, vars = c("Sepal.Length", "Sepal.Width"),
                       by = "Species")
  attr(tbl, "label") <- "Iris summary"
  attr(tbl, "subtitle") <- "Sepal measures"
  attr(tbl, "caption") <- "Source: iris"

  ft <- static_table(tbl)
  expect_s3_class(ft, "flextable")

  # pptx placement attributes (the officer deck renderer reads these)
  expect_equal(attr(ft, "pptx_left"), 0.4)
  expect_equal(attr(ft, "pptx_top"), 1.1)

  # One statistic, so each variable is a single row and the stub names the
  # variable: 2 rows, not 2 headers each with one child under it.
  expect_equal(flextable::nrow_part(ft, "body"), 2L)
  stub <- ft$body$dataset[[1L]]
  expect_equal(stub, c("Sepal.Length", "Sepal.Width"))

  # title + subtitle header lines above the leaf-label row, caption footer
  expect_equal(flextable::nrow_part(ft, "header"), 3L)
  expect_equal(flextable::nrow_part(ft, "footer"), 1L)
})

test_that("title/subtitle/caption: NULL = auto from attrs, '' = off", {
  skip_if_not_installed("flextable")

  tbl <- summary_table(iris, vars = "Sepal.Length", by = "Species")
  attr(tbl, "label") <- "Auto title"
  # a single-statistic summary also stamps its own subtitle: with the stat
  # gone from the stubs, this line is the only thing naming what the numbers
  # are, so it is part of the auto tier too
  expect_equal(attr(tbl, "subtitle"), "Mean (SD)")

  # auto: label + subtitle attrs become header lines above the leaf-label row
  expect_equal(flextable::nrow_part(static_table(tbl), "header"), 3L)
  # "" suppresses each of them independently
  expect_equal(flextable::nrow_part(static_table(tbl, title = ""), "header"), 2L)
  expect_equal(
    flextable::nrow_part(static_table(tbl, title = "", subtitle = ""), "header"),
    1L
  )
  # no caption -> no footer part rows
  expect_equal(flextable::nrow_part(static_table(tbl), "footer"), 0L)
})

test_that("two-level by produces a merged spanner header row", {
  skip_if_not_installed("flextable")

  tbl <- summary_table(mtcars, vars = "mpg", by = c("cyl", "am"))
  ft <- static_table(tbl, title = "", subtitle = "")
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
  ft <- static_table(df)

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
  ft <- static_table(tbl, title = "", fit_width = 12, col_widths = "even",
                 first_col_width = 6, other_cols_width = 3)
  w <- ft$body$colwidths
  expect_equal(sum(w), 12, tolerance = 1e-6)
  # stub capped at half the budget; three data cols share the rest
  expect_equal(w[[1L]], 6)
  expect_equal(unname(w[-1L]), rep(2, 3), tolerance = 1e-6)

  # option default is read when the argument is omitted
  withr::local_options(blockr.viz.ft_fit_width = 9,
                       blockr.viz.ft_col_widths = "even")
  ft2 <- static_table(tbl, title = "", first_col_width = 6)
  expect_equal(sum(ft2$body$colwidths), 9, tolerance = 1e-6)
})

test_that("measured widths size every column to its own text", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("systemfonts")

  # Short stubs against wide count columns: the even split hands the stub
  # half the slide anyway and starves the counts, the measured one does not.
  tbl <- data.frame(
    .label = c("Nausea", "Vomiting"),
    `Placebo (N=143)` = c("143 (100.0%)", "12 (8.4%)"),
    `Drug A (N=141)` = c("140 (99.3%)", "9 (6.4%)"),
    check.names = FALSE
  )

  even <- static_table(tbl, title = "", fit_width = 5,
                       col_widths = "even")$body$colwidths
  meas <- static_table(tbl, title = "", fit_width = 5)$body$colwidths

  expect_equal(sum(meas), 5, tolerance = 1e-6)
  expect_length(meas, 3L)
  expect_lt(meas[[1L]], even[[1L]] / 2)      # the stub gives width back
  expect_gt(meas[[2L]], even[[2L]])          # the counts get it

  # No data column is narrower than its own widest cell, which is the whole
  # point: a wrapped count costs a row of height and a line of legibility.
  pad <- ft_side_padding() / 72
  need <- vapply(tbl[-1L], function(x) max(ft_text_widths(x, "Inter", 13)),
                 numeric(1L)) + pad
  expect_true(all(meas[-1L] >= need))

  # A long stub is not capped at half the slide: it asks for what it needs.
  long <- tbl
  long$.label <- c("Subjects with at least one treatment emergent adverse event",
                   "Nausea")
  w <- static_table(long, title = "", fit_width = 10)$body$colwidths
  expect_gt(w[[1L]], 5)
  expect_true(all(w[-1L] >= need))
})

test_that("the stub wraps before the data columns are pinched", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("systemfonts")

  # A system organ class long enough to eat a third of the slide, against
  # enough arms to want the room back.
  arms <- function(n, stats) {
    tbl <- data.frame(
      .label = c("General disorders and administration site conditions",
                 "Nausea"),
      check.names = FALSE
    )
    for (a in LETTERS[seq_len(n)]) {
      for (s in stats) {
        tbl[[sprintf("Arm %s (N=143)||%s", a, s)]] <-
          structure(c("143 (100.0%)", "12 (8.4%)"), label = s)
      }
    }
    tbl
  }

  tbl <- arms(2L, c("n (%)", "Events", "Grade 3 or higher"))
  meas <- static_table(tbl, title = "", fit_width = 12)$body$colwidths
  hog <- withr::with_options(
    list(blockr.viz.ft_stub_share = 1),
    static_table(tbl, title = "", fit_width = 12)$body$colwidths
  )

  # The stub takes its share and wraps; what it gives up goes to the headers
  # the reader compares across, which were breaking to hold it.
  expect_lt(meas[[1L]], hog[[1L]])
  expect_lt(meas[[1L]], 0.35 * 12)
  expect_true(all(meas[-1L] >= hog[-1L]))
  expect_gt(max(meas[-1L]), max(hog[-1L]))
  expect_equal(sum(meas), 12, tolerance = 1e-6)

  # With nothing else asking for the room the stub still ends up on one line:
  # the cap is a queue, not a ceiling.
  w <- static_table(arms(2L, "n (%)"), title = "",
                    fit_width = 12)$body$colwidths
  expect_gt(w[[1L]], 0.35 * 12)
})

test_that("a table narrower than the slide keeps its natural width", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("systemfonts")

  tbl <- data.frame(.label = c("A", "B"), n = c("1", "2"))
  w <- static_table(tbl, title = "", subtitle = "", fit_width = 12)
  expect_lt(sum(w$body$colwidths), 12)

  # ... but grows to hold a title, which is merged across the whole width
  wide <- static_table(
    tbl, subtitle = "",
    title = "Subjects by treatment arm and analysis population",
    fit_width = 12
  )
  expect_gt(sum(wide$body$colwidths), sum(w$body$colwidths))
  expect_lte(sum(wide$body$colwidths), 12)
})

test_that("measured widths survive tables that cannot be measured", {
  skip_if_not_installed("flextable")

  # No data columns, no rows, and more columns than the slide can hold: none
  # of these may error, and all of them must still fill the budget.
  expect_s3_class(static_table(data.frame(.label = c("a", "b")),
                               fit_width = 10), "flextable")

  empty <- static_table(data.frame(.label = character(), n = character()),
                        title = "", subtitle = "", fit_width = 10)
  expect_true(all(empty$body$colwidths > 0))

  many <- static_table(as.data.frame(matrix(1:60, 2L, 30L)), title = "",
                       fit_width = 12)
  expect_equal(sum(many$body$colwidths), 12, tolerance = 1e-6)
  expect_true(all(many$body$colwidths > 0))
})

test_that("plain data frames render without annotations", {
  skip_if_not_installed("flextable")

  ft <- static_table(head(mtcars[, 1:3]), title = "Plain")
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

test_that("column .strong / .emph drive the header emphasis ramp", {
  b <- ft_emphasis_bands(c(FALSE, TRUE, FALSE), c(FALSE, FALSE, TRUE))
  # strong -> accent, emph -> dark gray, normal -> light gray
  expect_equal(b$bg, c("#EEEEEE", "#2563EB", "#9AA3B0"))
  expect_equal(b$stub_bg, "#EEEEEE")

  # overridable via option, no house palette baked in
  withr::local_options(
    blockr.viz.ft_emphasis_colors = c(normal = "#FFF", emph = "#888",
                                      strong = "#0A0")
  )
  b2 <- ft_emphasis_bands(c(TRUE, FALSE), c(FALSE, TRUE))
  expect_equal(b2$bg, c("#0A0", "#888"))
})

test_that("a column .strong attribute switches static_table into emphasis mode", {
  skip_if_not_installed("flextable")

  df <- data.frame(
    .label = c("A", "B"), .strong = c(TRUE, FALSE),
    Placebo = c("1", "2"), Drug = c("3", "4"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(df[["Drug"]], "strong") <- TRUE

  # header_bg would otherwise band by identity; the column attr wins.
  ft <- static_table(df, header_bg = c("#A59F9F", "#33D6F1"), title = "")
  fills <- ft$header$styles$cells$background.color$data
  # the Drug column header (col 3) carries the strong accent, not the palette
  expect_true(any(toupper(as.vector(fills)) == "#2563EB"))
  expect_false(any(toupper(as.vector(fills)) == "#33D6F1"))
})

test_that("header_bg off / empty resolves to no bands", {
  expect_null(resolve_header_bands("none", c("", ""), c("a", "b")))
  expect_null(resolve_header_bands(NULL, c("", ""), c("a", "b")))
  expect_null(resolve_header_bands(FALSE, c("", ""), c("a", "b")))
  expect_null(resolve_header_bands("#A59F9F", character(), character()))
})
