# Rules of the table server that the recordings do not pin.

tsrv_run <- function(blk, data, code) {
  dat <- shiny::reactive(data)
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      expr <- session$makeScope("expr")
      session$flushReact()
      code(expr)
    },
    args = list(x = blk, data = list(data = dat))
  )
}

# The payload as the browser reads it.
tsrv_payload <- function(expr) {
  out <- expr$output$table
  if (!is.character(out)) out <- jsonlite::toJSON(out, auto_unbox = TRUE)
  jsonlite::fromJSON(out, simplifyVector = FALSE)
}

test_that("the payload lists the input's columns for the gear", {
  d <- data.frame(ARM = c("A", "B"), AGE = c(30, 40), SEX = c("F", "M"))
  attr(d$AGE, "label") <- "Age"
  for (layout in c("records", "groups", "variables")) {
    tsrv_run(new_table_block(layout = layout), d, function(expr) {
      cols <- tsrv_payload(expr)$columns
      expect_identical(vapply(cols, `[[`, "", "name"),
                       c("ARM", "AGE", "SEX"), info = layout)
      expect_identical(cols[[2L]]$label, "Age", info = layout)
      expect_identical(cols[[2L]]$class, "number", info = layout)
    })
  }
})

test_that("a script's top_n takes the word from the table's unset top_n", {
  d <- data.frame(ARM = rep(c("A", "B"), 3), AETERM = letters[1:6])
  script <- paste(
    'top_n <- 25 #| number(min = 1, label = "Top adverse events")',
    'ae <- factor("AETERM", levels = c("AETERM", "ARM"))',
    "data", sep = "\n"
  )
  blk <- new_table_block(
    layout = "groups", by = "ARM",
    summaries = list(list(type = "simple", func = "count", show = "number",
                          name = "Events")),
    subtitle = "Top {@top_n} Adverse Event {@ae}", script = script
  )
  tsrv_run(blk, d, function(expr) {
    p <- tsrv_payload(expr)
    ti <- p$derived$titles
    expect_identical(ti$subtitle$text, "Top 25 Adverse Event AETERM")
    expect_identical(ti$args$top_n, "25")
    words <- Filter(function(x) !is.null(x$arg), ti$subtitle$parts)
    expect_identical(vapply(words, `[[`, "", "arg"), c("top_n", "ae"))
    expect_identical(vapply(words, `[[`, "", "text"), c("25", "AETERM"))
  })
})

# A heatmap saved by 0.2.200 (fixture written by that version's
# new_heatmap_block()), its script declaring `top_n` and its subtitle naming
# it. 0.2.200 printed "Top 25 Adverse Event AEDECOD, by TRT01A": the heatmap
# had no `top_n` setting. Restored, it is a table, which has one.
test_that("a saved 0.2 heatmap's {@top_n} prints the script's value", {
  saved <- blockr.core:::read_json(
    test_path("fixtures", "heatmap-script-0.2.200.json")
  )
  ae <- data.frame(
    TRT01A = c("A", "A", "A", "B"), USUBJID = c("1", "1", "2", "3"),
    AEDECOD = c("x", "y", "x", "x"), AETERM = "t", AESEV = "MILD"
  )
  sub <- function(saved) {
    blk <- suppressWarnings(blockr.core::blockr_deser(saved))
    expect_s3_class(blk, "table_block")
    out <- NULL
    tsrv_run(blk, ae, function(expr) {
      out <<- tsrv_payload(expr)$derived$titles$subtitle
    })
    out
  }
  s <- sub(saved)
  expect_identical(s$text, "Top 25 Adverse Event AEDECOD, by TRT01A")
  words <- Filter(function(x) !is.null(x$arg), s$parts)
  expect_identical(words[[1L]][c("text", "arg")],
                   list(text = "25", arg = "top_n"))
  # The heatmap's own top_n seeded the script's value.
  saved$payload$top_n <- 10L
  expect_identical(sub(saved)$text, "Top 10 Adverse Event AEDECOD, by TRT01A")
})
