# The table block's cards look (R/table-cards.R): the argument, the card
# model the engines push, and the exhibit the downloads draw.

cards_df <- function() {
  data.frame(
    ARM = c("A", "A", "A", "B", "B", "C"),
    SEV = factor(c("mild", "severe", "mild", "mild", "mild", "severe"),
                 levels = c("mild", "severe")),
    AGE = c(60, 70, 80, 50, 55, 65),
    BASE = c(140, 150, 145, 130, 135, 150),
    AVAL = c(130, 150, 140, 135, 135, 140),
    WK = c(1, 2, 3, 1, 2, 1),
    stringsAsFactors = FALSE
  )
}

cards_summaries <- function() {
  list(
    list(type = "simple", name = "Rows", func = "count", show = "bar"),
    list(type = "simple", name = "Events", func = "count", color = "SEV",
         show = "bar"),
    list(type = "dist", name = "Age", col = "AGE", style = "box",
         summary = "median_q1_q3"),
    list(type = "pair", name = "SBP", x = "BASE", xend = "AVAL"),
    list(type = "series", name = "Trend", x = "WK", col = "AGE"),
    list(type = "simple", name = "N", func = "count", show = "number"),
    list(type = "spans", name = "Spans", x = "WK", xend = "WK"),
    list(type = "expr", name = "Code", expr = "n()")
  )
}

# The groups model in the cards look, and what the view adds to the table
# (table_groups_payload()): the TableModel with the card model on it.
cards_model_of <- function(data, by = "ARM", summaries = cards_summaries(),
                           look = "cards", ...) {
  sum_model(data, summaries = summaries, by = by, sort_by = "alpha",
            sort_dir = "asc", look = look, ...)
}

cards_view_of <- function(m, look = "cards") {
  v <- list(look = look, column_axis = TRUE)
  src <- list(scale_map = function() NULL)
  table_groups_payload(m, v, NULL, src)$model
}

test_that("look takes table, compact or cards; compact = TRUE still works", {
  look_of <- function(b) blockr.core::blockr_ser(b)$payload$look
  expect_identical(look_of(new_table_block()), "table")
  expect_identical(look_of(new_table_block(look = "cards")), "cards")
  expect_error(new_table_block(look = "nonsense"), "look")
  expect_warning(b <- new_table_block(compact = TRUE), "compact")
  expect_identical(look_of(b), "compact")
  expect_warning(b <- new_table_block(rotate_titles = TRUE), "rotate_titles")
  expect_identical(look_of(b), "compact")
  p <- blockr.core::blockr_ser(b)$payload
  expect_false("compact" %in% names(p))
})

test_that("a board saved with `compact` restores in the compact look", {
  s0 <- blockr.core::blockr_ser(new_table_block())
  saved <- list(
    object = s0$object,
    payload = list(layout = "groups", by = "ARM", compact = TRUE,
                   summaries = list(list(type = "simple", func = "count",
                                         show = "tile"))),
    constructor = s0$constructor
  )
  p <- blockr.core::blockr_ser(blockr.core::blockr_deser(saved))$payload
  expect_identical(p$look, "compact")
  expect_identical(p$layout, "groups")
})

test_that("the groups view carries one card per drawable summary", {
  m <- cards_model_of(cards_df())
  model <- cards_view_of(m)
  cards <- model$cards
  expect_identical(cards$n, 3L)
  expect_true(cards$multi)
  expect_identical(as.character(cards$label), c("A", "B", "C"))
  labels <- vapply(cards$cols, `[[`, "", "label")
  # spans and expr have no card form; the gear names them.
  expect_identical(labels, c("Rows", "Events", "Age", "SBP", "Trend", "N"))
  expect_identical(as.character(cards$skipped), c("Spans", "Code"))

  col <- function(nm) cards$cols[[match(nm, labels)]]
  expect_identical(as.character(col("Rows")$main), c("3", "2", "1"))
  # A glyph points at the column of the table the card draws it from, so a
  # column of cards shares that column's scale. It is the column's index in
  # the view, where the stub is 0.
  for (nm in c("Rows", "Events", "Age", "SBP", "Trend")) {
    k <- col(nm)$glyph
    expect_true(is.numeric(k))
    expect_identical(m$cells$cols[[k]]$kind,
                     c(Rows = "bar", Events = "barsplit", Age = "box",
                       SBP = "pair", Trend = "sparkline")[[nm]])
    expect_identical(model$cols[[k + 1L]]$mark$kind,
                     c(Rows = "bar", Events = "barsplit", Age = "box",
                       SBP = "pair", Trend = "sparkline")[[nm]])
  }
  expect_null(col("N")$glyph)
  # The grey line is a statistic the summary holds.
  expect_identical(col("Age")$word, "Q1–Q3")
  expect_identical(as.character(col("Age")$sub[[1L]]), "65.0 to 75.0")
  # The pair's "to" value, and its "from" one on the grey line.
  expect_identical(as.character(col("SBP")$main[[1L]]), "150")
  expect_identical(as.character(col("SBP")$sub[[1L]]), "from 140")
  expect_identical(as.character(col("Trend")$sub[[1L]]), "from 60")
  # A colour split brings its legend.
  expect_identical(vapply(col("Events")$legend, `[[`, "", "label"),
                   c("mild", "severe"))
  expect_false(is.null(col("Age")$range))
})

test_that("a summary with a facet draws one card per level", {
  m <- cards_model_of(
    cards_df(), summaries = list(list(type = "simple", name = "Rows",
                                      func = "count", facet = "SEV",
                                      show = "bar"))
  )
  cards <- cards_view_of(m)$cards
  expect_identical(vapply(cards$cols, `[[`, "", "label"),
                   c("Rows · mild", "Rows · severe"))
})

test_that("a faceted count's head names the count scale, not the share", {
  # The bar is drawn as a share of its level's N; the card's number is the
  # count, so the head reads in counts.
  m <- cards_model_of(
    cards_df(), summaries = list(list(type = "simple", name = "Rows",
                                      func = "count", facet = "SEV",
                                      show = "bar"))
  )
  rng <- vapply(cards_view_of(m)$cards$cols, `[[`, "", "range")
  expect_false(any(grepl("%", rng, fixed = TRUE)))
  # mild: 4 rows, at most 2 per arm.
  expect_identical(rng[[1L]], "0 to 2")
})

test_that("without `by` the cards draw one row and no drill", {
  m <- cards_model_of(cards_df(), by = character(), drill = "ARM")
  cards <- cards_view_of(m)$cards
  expect_identical(cards$n, 1L)
  expect_false(cards$multi)
  expect_identical(as.character(cards$cols[[1L]]$main), "6")
  expect_false(isTRUE(m$cells$pick))
})

test_that("only the cards look carries cards", {
  m <- cards_model_of(cards_df(), look = "table")
  expect_null(cards_view_of(m, look = "table")$cards)
  expect_false(is.null(cards_view_of(m, look = "cards")$cards))
})

test_that("the records model draws each column as a plain number", {
  d <- data.frame(Arm = c("P", "X"), N = c(1200, 96), Change = c(-1.25, 0.5),
                  Note = c("a", NA))
  m <- table_cards_records(d, digits = 1L, row_hex = c("#111111", "#222222"))
  expect_identical(m$n, 2L)
  expect_true(m$multi)
  expect_identical(m$label, c("P", "X"))
  expect_identical(vapply(m$cols, `[[`, "", "label"),
                   c("N", "Change", "Note"))
  expect_identical(m$cols[[1L]]$main, c("1,200.0", "96.0"))
  expect_identical(m$cols[[2L]]$main, c("−1.2", "0.5"))
  expect_true(m$cols[[3L]]$text)
  expect_identical(m$cols[[3L]]$main, c("a", ""))
  expect_identical(m$hex, c("#111111", "#222222"))

  big <- data.frame(id = seq_len(80), v = seq_len(80))
  mb <- table_cards_records(big)
  expect_identical(mb$n, CARDS_MAX_ROWS)
  expect_match(mb$fold, "first 60 of 80")

  js <- jsonlite::toJSON(cards_payload(table_cards_records(d[1, ])),
                         auto_unbox = TRUE)
  # One row still ships arrays.
  expect_match(js, "\"label\":[\"P\"]", fixed = TRUE)
})

test_that("the records view carries cards in the records layout only", {
  d <- data.frame(Arm = c("P", "X"), N = c(12, 9))
  v <- list(look = "cards", digits = "number", format = "number", unit = "",
            collapsible = TRUE, drill = NULL, color = NULL, by = character(),
            rowname = NULL, columns = NULL)
  src <- list(scale_map = function() NULL)
  p <- table_frame_view(v, d, src, "records")
  expect_identical(p$model$cards$cols[[1L]]$label, "N")
  expect_identical(p$model$cards$n, 2L)
  # Another layout draws the frame without them.
  expect_null(table_frame_view(v, d, src, "variables")$model$cards)
  # The table look has none either.
  v$look <- "table"
  expect_null(table_frame_view(v, d, src, "records")$model$cards)
})

test_that("the cards exhibit writes html with the model and a painted slide", {
  e <- static_summarize_table(cards_df(), summaries = cards_summaries(),
                              by = "ARM", look = "cards", title = "T")
  expect_false(is.null(e$cards))
  ce <- cards_exhibit(e)
  expect_s3_class(ce, "cards_exhibit")
  f <- tempfile(fileext = ".html")
  on.exit(unlink(f), add = TRUE)
  write_exhibit_html(ce, f, title = "T")
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(html, "data-cards-static", fixed = TRUE)
  expect_match(html, "BlockrTableCards", fixed = TRUE)
  expect_match(html, "\"label\":\"Rows\"", fixed = TRUE)

  skip_if_not(summarize_paint_ready())
  skip_if_not_installed("officer")
  g <- cards_paint_grob(ce, width_in = 10)
  expect_gt(g$height, 1)
  pp <- tempfile(fileext = ".pptx")
  on.exit(unlink(pp), add = TRUE)
  write_exhibit_pptx(ce, pp, title = "T")
  expect_true(file.exists(pp))
  r <- cards_exhibit(table_cards_records(data.frame(a = "x", n = 3)))
  expect_length(cards_paint_pages(r, width_in = 8, max_height = 5), 1L)
})
