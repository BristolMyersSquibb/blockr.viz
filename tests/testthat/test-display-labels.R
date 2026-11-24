display_frame <- function() {
  d <- data.frame(
    code = c("A-01", "A-01", "B-02", "C-03"),
    name = c("a", "a", "b", NA),
    v = 1:4
  )
  attr(d$code, "blockr_display") <- "name"
  d
}

test_that("a labelled category sends value -> label", {
  out <- chart_display_labels(list(type = "bar", category = "code"),
                              display_frame())
  expect_identical(out, list(code = list(`A-01` = "a", `B-02` = "b")))
})

test_that("no attribute, a missing display column or no key role: no map", {
  d <- display_frame()
  expect_null(chart_display_labels(list(type = "bar", category = "v"), d))
  d$name <- NULL
  expect_null(chart_display_labels(list(type = "bar", category = "code"), d))
  plain <- display_frame()
  attr(plain$code, "blockr_display") <- NULL
  expect_null(chart_display_labels(list(type = "bar", category = "code"), plain))
})

test_that("timeline lanes and colours are labelled too", {
  d <- display_frame()
  expect_named(chart_display_labels(list(type = "gantt", y = "code"), d), "code")
  expect_named(chart_display_labels(list(type = "bar", category = "v",
                                         color = "code"), d), "code")
})

test_that("a script that drops the attribute falls back to the input", {
  raw <- display_frame()
  prepared <- as.data.frame(lapply(raw, function(x) {
    attributes(x) <- NULL
    x
  }))[raw$code != "B-02", , drop = FALSE]
  expect_null(attr(prepared$code, "blockr_display"))
  out <- chart_display_labels(list(type = "bar", category = "code"), prepared,
                              raw)
  expect_identical(out, list(code = list(`A-01` = "a")))
})

test_that("the map travels in the chart payload", {
  d <- display_frame()
  blk <- new_chart_block(type = "bar", category = "code", value = "v",
                         func = "max")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      p <- output[["expr-chart"]]
      if (is.character(p)) p <- jsonlite::fromJSON(p, simplifyVector = FALSE)
      expect_identical(p$derived$display$code$`B-02`, "b")
    },
    args = list(x = blk, data = list(data = function() d))
  )
})
