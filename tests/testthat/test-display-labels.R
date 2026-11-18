display_frame <- function() {
  d <- data.frame(
    id = c("S-01", "S-01", "S-02", "S-03"),
    short = c("1", "1", "2", NA),
    v = 1:4
  )
  attr(d$id, "blockr_display") <- "short"
  d
}

test_that("a labelled category sends value -> label", {
  out <- chart_display_labels(list(type = "bar", category = "id"),
                              display_frame())
  expect_identical(out, list(id = list(`S-01` = "1", `S-02` = "2")))
})

test_that("no attribute, a missing display column or no key role: no map", {
  d <- display_frame()
  expect_null(chart_display_labels(list(type = "bar", category = "v"), d))
  d$short <- NULL
  expect_null(chart_display_labels(list(type = "bar", category = "id"), d))
  plain <- display_frame()
  attr(plain$id, "blockr_display") <- NULL
  expect_null(chart_display_labels(list(type = "bar", category = "id"), plain))
})

test_that("timeline lanes and colours are labelled too", {
  d <- display_frame()
  expect_named(chart_display_labels(list(type = "gantt", y = "id"), d), "id")
  expect_named(chart_display_labels(list(type = "bar", category = "v",
                                         color = "id"), d), "id")
})

test_that("a script that drops the attribute falls back to the input", {
  raw <- display_frame()
  prepared <- as.data.frame(lapply(raw, function(x) {
    attributes(x) <- NULL
    x
  }))[raw$id != "S-02", , drop = FALSE]
  expect_null(attr(prepared$id, "blockr_display"))
  out <- chart_display_labels(list(type = "bar", category = "id"), prepared,
                              raw)
  expect_identical(out, list(id = list(`S-01` = "1")))
})

test_that("the map travels in the chart payload", {
  d <- display_frame()
  blk <- new_chart_block(type = "bar", category = "id", value = "v",
                         func = "max")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      p <- output[["expr-chart"]]
      if (is.character(p)) p <- jsonlite::fromJSON(p, simplifyVector = FALSE)
      expect_identical(p$derived$display$id$`S-02`, "2")
    },
    args = list(x = blk, data = list(data = function() d))
  )
})
