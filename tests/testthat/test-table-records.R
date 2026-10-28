# table_records_to_model() (R/table-records.R) against the first render of the
# table recordings: the browser gets the same text and values the reference
# sent, and the paint as values instead of CSS.

# The colours in a recorded style chunk, "" where a cell had none.
tr_style_hex <- function(style, what) {
  out <- rep(NA_character_, length(style))
  m <- regmatches(style, regexec(paste0(what, ":(#[0-9A-Fa-f]{6})"), style))
  hit <- lengths(m) == 2L
  out[hit] <- vapply(m[hit], `[[`, "", 2L)
  out
}

tr_bar_pct <- function(style) {
  m <- regmatches(style, regexec(" 0 ([0-9.]+)%", style))
  vapply(m, function(x) if (length(x) == 2L) as.numeric(x[[2L]]) else NA_real_,
         numeric(1L))
}

test_that("table_records_to_model() draws what the reference sent", {
  cases <- tm_cases("records")
  n <- 0L
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    want <- tm_payload(cs$messages, "blockr-viz-table-data")
    if (!identical(want$kind, "flat")) next
    m <- table_model_records(cs$state, cs$data,
                             tint = table_model_tint(cs$state, cs$data))
    v <- table_records_to_model(m)
    got <- tm_json(v)
    expect_identical(got$n, want$n, label = nm)
    expect_length(got$cols, length(want$cols))
    for (j in seq_along(want$cols)) {
      w <- want$cols[[j]]
      g <- got$cols[[j]]
      expect_identical(g$disp, w$disp, label = paste(nm, j, "disp"))
      expect_identical(g$raw, w$raw, label = paste(nm, j, "raw"))
      style <- vapply(w$style %||% list(), function(x) x %||% "", "")
      if (!length(style) || j == 1L) next
      if (any(grepl("linear-gradient", style))) {
        pct <- tr_bar_pct(style)
        bar <- vapply(g$bar, function(x) x %||% NA_real_, numeric(1L))
        expect_equal(bar * 100, pct, tolerance = 1e-3,
                     label = paste(nm, j, "bar"))
      } else {
        bg <- vapply(g$bg, function(x) x %||% NA_character_, "")
        expect_identical(toupper(bg), toupper(tr_style_hex(style, "background")),
                         label = paste(nm, j, "bg"))
      }
    }
    if (!is.null(v$rows$tint)) {
      stub <- vapply(want$cols[[1L]]$style, function(x) x %||% "", "")
      expect_identical(!is.na(v$rows$tint), nzchar(stub), label = paste(nm, "tint"))
    }
    n <- n + 1L
  }
  expect_gt(n, 10L)
})

test_that("a records header names each column and its label", {
  d <- data.frame(id = c("a", "b"), v = c(1.5, 2))
  attr(d$v, "label") <- "Value"
  v <- table_records_to_model(table_model_records(
    list(rowname = "id", format = "number"), d
  ))
  expect_identical(v$stub, 0L)
  expect_identical(v$head[[1L]][[1L]], list(text = "id", col = 0L,
                                            stub = TRUE))
  expect_identical(v$head[[1L]][[2L]], list(text = "v", sub = "Value",
                                            col = 1L))
  expect_true(v$cols[[2L]]$num)
  expect_identical(as.vector(v$cols[[2L]]$disp), c("1.5", "2"))
  expect_length(v$widths, 2L)
})

test_that("a one-row table's columns are arrays on the wire", {
  d <- data.frame(id = "a", v = 1.5)
  v <- table_records_to_model(table_model_records(list(rowname = "id"), d))
  json <- jsonlite::toJSON(v, auto_unbox = TRUE)
  back <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_type(back$cols[[2L]]$disp, "list")
  expect_type(back$widths, "list")
  s <- table_structured_to_model(table_nest(
    data.frame(g = "x", id = "a", v = 1), "g", "id"
  ))
  back <- jsonlite::fromJSON(jsonlite::toJSON(s, auto_unbox = TRUE),
                             simplifyVector = FALSE)
  expect_type(back$cols[[1L]]$disp, "list")
})
