# The column axis of a structured table: an optional `column_keys` attribute
# a producer stamps, which the renderer turns into clickable headers and a
# per-column claim. Absent, every click still means "this row".

ck_frame <- function() {
  d <- data.frame(
    .label = c("F", "M"),
    .variable = c("SEX", "SEX"),
    .variable_level = c("F", "M"),
    `Placebo||n` = c("10", "12"),
    `Drug X||n` = c("11", "9"),
    Total = c("21", "21"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  attr(d, "column_keys") <- list(
    "Placebo" = list(list(column = "ARM", values = "Placebo")),
    "Placebo||n" = list(list(column = "ARM", values = "Placebo")),
    "Drug X" = list(list(column = "ARM", values = "Drug X")),
    "Drug X||n" = list(list(column = "ARM", values = "Drug X"))
  )
  d
}

test_that("annotated_column_keys drops malformed entries, keeps good ones", {
  d <- ck_frame()
  expect_length(annotated_column_keys(d), 4L)

  attr(d, "column_keys") <- c(
    attr(d, "column_keys"),
    list(bad = list(list(column = "", values = "x")),
         worse = list(list(values = "x")))
  )
  expect_length(annotated_column_keys(d), 4L)

  attr(d, "column_keys") <- "nonsense"
  expect_null(annotated_column_keys(d))

  expect_null(annotated_column_keys(data.frame(a = 1)))
})

test_that("a named column gets header keys, an unnamed one stays inert", {
  html <- paste(
    as.character(drilldown_table(ck_frame(), drill = "auto", elem_id = "e",
                                 gear = FALSE)),
    collapse = "\n"
  )
  expect_match(html, "dd-col-drill")
  expect_match(html, 'data-dd-colkeys=', fixed = TRUE)
  expect_match(html, "data-dd-colkeys-map")
  # Two arms are named; Total is not, so exactly two leaf headers plus two
  # spanners carry keys and the Total header carries none.
  expect_equal(lengths(regmatches(html, gregexpr("dd-col-drill", html))), 4L)
})

test_that("no attribute renders exactly the markup it rendered before", {
  d <- ck_frame()
  attr(d, "column_keys") <- NULL
  html <- paste(
    as.character(drilldown_table(d, drill = "auto", elem_id = "e",
                                 gear = FALSE)),
    collapse = "\n"
  )
  expect_false(grepl("dd-col-drill", html))
  expect_false(grepl("dd-colkeys", html))
})

test_that("the drill has to be on for a header to look clickable", {
  html <- paste(
    as.character(drilldown_table(ck_frame(), drill = NULL, elem_id = "e",
                                 gear = FALSE)),
    collapse = "\n"
  )
  expect_false(grepl("dd-col-drill", html))
})

test_that("the cell map is indexed the way cellIndex counts", {
  keys <- annotated_column_keys(ck_frame())
  # Stub at 0, then the three data columns; Total (the last) has no entry.
  map <- jsonlite::fromJSON(
    dd_col_keys_map_json(c("Placebo||n", "Drug X||n", "Total"), keys,
                         stub_offset = 1L),
    simplifyVector = FALSE
  )
  expect_length(map, 4L)
  expect_null(map[[1]])
  expect_equal(map[[2]][[1]]$column, "ARM")
  expect_equal(map[[3]][[1]]$column, "ARM")
  expect_null(map[[4]])
})

test_that("values stay a JSON array, single or pooled", {
  one <- jsonlite::fromJSON(
    dd_col_keys_json(list(list(column = "ARM", values = "Placebo"))),
    simplifyVector = FALSE
  )
  expect_equal(one[[1]]$values, list("Placebo"))

  pooled <- jsonlite::fromJSON(
    dd_col_keys_json(list(list(column = "ARM", values = c("A", "B")))),
    simplifyVector = FALSE
  )
  expect_equal(pooled[[1]]$values, list("A", "B"))
})

test_that("dd_col_claims reshapes the column half without touching the data", {
  keys <- list(list(column = "ARM", values = "Placebo"),
               list(column = "SEX", values = "F"))

  expect_equal(
    dd_col_claims(keys),
    list(list(name = "ARM", mode = "multi", values = "Placebo"),
         list(name = "SEX", mode = "multi", values = "F"))
  )
  # A dm-backed target needs the table named.
  expect_equal(dd_col_claims(keys[1], "adsl")[[1]]$table, "adsl")
  # Pools arrive as several values and stay multi.
  expect_equal(
    dd_col_claims(list(list(column = "ARM", values = c("A", "B"))))[[1]]$values,
    c("A", "B")
  )
  expect_equal(dd_col_claims(NULL), list())
  expect_equal(dd_col_claims(list(list(column = "", values = "x"))), list())
})
