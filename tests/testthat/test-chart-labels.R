# A column's label attribute is how the chart names it. R sends it to the
# browser in the column metadata (dd_col_meta()), and the titles resolve
# `{label(col)}` / `{label(@arg)}` to it. The browser side is pinned in
# tests/js/labels.test.js.

labelled_df <- function() {
  d <- data.frame(
    AETERM = c("Rash", "Rash", "Fever", "Nausea"),
    AESEV = c("MILD", "SEVERE", "MILD", "SEVERE"),
    ADY = c(1, 3, 7, 9),
    AVAL = c(4, 6, 2, 9),
    PLAIN = c("p", "q", "p", "q"),
    stringsAsFactors = FALSE
  )
  attr(d$AETERM, "label") <- "Reported Term"
  attr(d$AESEV, "label") <- "Severity"
  attr(d$ADY, "label") <- "Study Day"
  attr(d$AVAL, "label") <- "Analysis Value"
  d
}

meta_of <- function(cols, name) {
  Filter(function(c) identical(c$name, name), cols)[[1L]]
}

# The last drilldown-data message a chart block sends for `blk` on `df`,
# handed to `check`.
chart_msg <- function(blk, df, check) {
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      sent <- list()
      root <- session$rootScope()
      root$sendCustomMessage <- function(type, message) {
        if (identical(type, "drilldown-data")) sent[[length(sent) + 1L]] <<- message
      }
      session$flushReact()
      check(sent[[length(sent)]])
    },
    args = list(x = blk, data = list(data = function() df))
  )
}

test_that("the column metadata carries each label, and none for an unlabelled column", {
  cols <- dd_col_meta(labelled_df())
  expect_equal(meta_of(cols, "AESEV")$label, "Severity")
  expect_equal(meta_of(cols, "AVAL")$label, "Analysis Value")
  expect_null(meta_of(cols, "PLAIN")$label)

  d <- labelled_df()
  attr(d$PLAIN, "label") <- ""
  expect_null(meta_of(dd_col_meta(d), "PLAIN")$label)
})

test_that("the chart's payload carries the labels", {
  blk <- new_chart_block(type = "bar", x = "AETERM", color = "AESEV")
  chart_msg(blk, labelled_df(), function(msg) {
    expect_equal(meta_of(msg$columns, "AETERM")$label, "Reported Term")
    expect_equal(meta_of(msg$columns, "AESEV")$label, "Severity")
    expect_null(meta_of(msg$columns, "PLAIN")$label)
  })
})

test_that("a histogram's bin column carries x's label, else x's name", {
  blk <- new_chart_block(type = "histogram", x = "ADY", bins = 2)
  chart_msg(blk, labelled_df(), function(msg) {
    expect_equal(meta_of(msg$columns, "ADY_bin")$label, "Study Day")
  })
  d <- labelled_df()
  attr(d$ADY, "label") <- NULL
  chart_msg(blk, d, function(msg) {
    expect_equal(meta_of(msg$columns, "ADY_bin")$label, "ADY")
  })
})

test_that("label() tokens resolve to the label, the name only without one", {
  d <- labelled_df()
  expect_equal(resolve_title_template("{label(AESEV)}", d), "Severity")
  expect_equal(resolve_title_template("{label(PLAIN)}", d), "PLAIN")
  expect_equal(
    resolve_title_template("{label(@y)} by {label(@color)}", d,
                           list(y = "AVAL", color = "AESEV")),
    "Analysis Value by Severity"
  )
  # `{@arg}` prints the argument's value as written: the column name.
  expect_equal(resolve_title_template("{@color}", d, list(color = "AESEV")),
               "AESEV")
})

test_that("a word printed from a label is a slot that says so", {
  p <- block_title_parts("by {label(@color)}", labelled_df(),
                         args = list(color = "AESEV"))
  word <- Filter(function(x) !is.null(x$arg), p)[[1L]]
  expect_equal(word$text, "Severity")
  expect_equal(word$arg, "color")
  expect_equal(word$by, "label")
})

test_that("the chart's resolved title and subtitle use the labels", {
  blk <- new_chart_block(
    type = "bar", x = "AETERM", color = "AESEV",
    title = "{label(@x)}",
    subtitle = "coloured by {label(@color)}"
  )
  chart_msg(blk, labelled_df(), function(msg) {
    cfg <- msg$config
    expect_equal(cfg$title_resolved, "Reported Term")
    expect_equal(cfg$subtitle_resolved, "coloured by Severity")
    slot <- Filter(function(x) !is.null(x$arg), cfg$subtitle_parts)[[1L]]
    expect_equal(slot$text, "Severity")
    expect_equal(slot$by, "label")
  })
})

test_that("an empty band names its columns by label", {
  d <- labelled_df()
  d$ADY <- as.character(d$ADY)
  attr(d$ADY, "label") <- "Study Day"
  expect_match(band_empty_reason(d, "ADY", "AVAL"), "Timeline \"Study Day\"")

  d <- labelled_df()
  d$AVAL <- NA_real_
  attr(d$AVAL, "label") <- "Analysis Value"
  expect_match(band_empty_reason(d, "ADY", "AVAL"),
               "missing \"Study Day\" or \"Analysis Value\"")

  d <- labelled_df()
  d$ADY <- 1
  attr(d$ADY, "label") <- "Study Day"
  expect_match(band_empty_reason(d, "ADY", "AVAL"), "^\"Study Day\" has only one")

  # A column the data does not have has no label: it is named.
  expect_match(band_empty_reason(labelled_df(), "NOPE", "AVAL"), "\"NOPE\"")
  # Unlabelled, the name.
  d <- labelled_df()
  d$PLAIN <- NA_real_
  expect_match(band_empty_reason(d, "ADY", "PLAIN"), "\"PLAIN\"")
})

test_that("a band reference sends its column, which the browser names by label", {
  ref <- band_reference(labelled_df(), "AVAL")
  expect_equal(ref$col, "AVAL")
})

test_that("the subtitle example writes columns as labels", {
  ex <- chart_arguments()$subtitle$example
  expect_match(ex, "{label(@x)}", fixed = TRUE)
  expect_false(grepl("{@x}", ex, fixed = TRUE))
  expect_false(grepl("{@color}", ex, fixed = TRUE))
})
