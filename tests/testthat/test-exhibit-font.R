# The floor the pptx paginator shrinks to before it splits a table, and the
# board option that sets it. One number, both axes: lowering it is the whole
# of "keep this table on one slide".

fit_df <- function(n = 26L) {
  out <- data.frame(
    .label = sprintf("Preferred term %d", seq_len(n)),
    .indent = 0L,
    Placebo = sprintf("%d (%.1f%%)", seq_len(n), seq_len(n) / 2),
    Drug = sprintf("%d (%.1f%%)", seq_len(n), seq_len(n) / 3),
    check.names = FALSE
  )
  attr(out, "label") <- "Adverse events"
  out
}

# The slides a table takes, and whatever it said on the way.
deck_of <- function(x, ...) {
  notes <- list()
  f <- tempfile(fileext = ".pptx")
  on.exit(unlink(f), add = TRUE)

  withCallingHandlers(
    write_exhibit_pptx(x, f, title = "Adverse events", ...),
    blockr_exhibit_split = function(c) {
      notes[[length(notes) + 1L]] <<- c
      invokeRestart("muffleMessage")
    }
  )

  list(slides = length(officer::read_pptx(f)), notes = notes)
}

test_that("the floor resolves argument, then board, then option, then 11", {
  withr::local_options(blockr.viz.ft_min_font_size = NULL)

  expect_identical(exhibit_min_font_size(), 11)
  expect_identical(exhibit_min_font_size(9), 9)
  # A select input answers in characters.
  expect_identical(exhibit_min_font_size("8"), 8)

  withr::local_options(blockr.viz.ft_min_font_size = 9)
  expect_identical(exhibit_min_font_size(), 9)
  expect_identical(exhibit_min_font_size(12), 12)

  # Whole points, since the ladders step by one, and never below what a
  # projector can carry however a board is configured.
  expect_identical(exhibit_min_font_size(10.5), 10)
  expect_identical(exhibit_min_font_size(2), MIN_FONT_FLOOR)
  expect_identical(exhibit_min_font_size("not a size"), 9)
})

test_that("a lower floor keeps a table that would have split on one slide", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  skip_if_not_installed("systemfonts")

  x <- fit_df()

  high <- deck_of(x, min_font_size = 13)
  low <- deck_of(x, min_font_size = 8)

  # The same table, split at the house floor and whole a few points down --
  # which is the whole of what a reader asking for one slide buys.
  expect_gt(high$slides, 1L)
  expect_identical(low$slides, 1L)
})

test_that("a table that still does not fit says what would have fitted", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  skip_if_not_installed("systemfonts")

  got <- deck_of(fit_df(), min_font_size = 13)

  expect_length(got$notes, 1L)

  note <- got$notes[[1L]]
  expect_identical(note$pages, got$slides)
  expect_identical(note$floor, 13)
  # The actionable half: a size, below the floor that was in force, at which
  # the table is one slide. Confirmed by rendering at it.
  #
  # Whether such a size EXISTS is a metric question, not a mechanical one:
  # under a face wider than Inter the estimate falls through pptx_fit_size()'s
  # floor and the note carries no size at all. The note itself -- its pages,
  # its floor, that it was raised -- is asserted above and holds under any
  # face; only this half needs the deck's own typeface to be the one measured.
  skip_if_font_substituted()
  expect_true(is.numeric(note$fit_size))
  expect_lt(note$fit_size, 13)
  expect_identical(deck_of(fit_df(), min_font_size = note$fit_size)$slides, 1L)
})

test_that("a table that fits says nothing", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  skip_if_not_installed("systemfonts")

  expect_length(deck_of(fit_df(3L))$notes, 0L)
})

test_that("the floor reaches the painted summarize table too", {
  skip_if_not_installed("officer")
  skip_if_not(rank_paint_ready())

  set.seed(42)
  data <- data.frame(
    grp = paste0("term ", sprintf("%03d", seq_len(600L) %% 30L)),
    val = stats::rnorm(600L, 10, 3),
    stringsAsFactors = FALSE
  )
  ex <- static_summarize_table(
    data, by = "grp",
    summaries = list(list(type = "simple", func = "count", show = "bar",
                          name = "n"))
  )

  slides <- function(...) {
    doc <- suppressMessages(
      pptx_add_exhibit(officer::read_pptx(), ex, title = "Long", ...)
    )
    length(doc)
  }

  # A picture pages by row height like a typeset table does, so the same
  # floor has to move it: a summarize table that split on a deck built to
  # keep its tables whole would read as the setting not working.
  expect_gt(slides(min_font_size = 11), 1L)
  expect_identical(slides(min_font_size = 5), 1L)
})

test_that("the board option carries the floor into an export", {
  expect_s3_class(new_exhibit_font_option(), "board_option")

  sess <- shiny::MockShinySession$new()
  sess$userData$board_options <- list(
    exhibit_min_font_size = shiny::reactiveVal(8)
  )

  # Read at the bottom of the stack, where every export passes: a deck, a
  # table block's download and a summarize block's cannot then disagree
  # about how small a table may go.
  expect_identical(
    shiny::withReactiveDomain(sess, shiny::isolate(exhibit_min_font_size())),
    8
  )

  # A caller that names a size still wins over the board.
  expect_identical(
    shiny::withReactiveDomain(sess, shiny::isolate(exhibit_min_font_size(12))),
    12
  )
})

test_that("a board with a table on it offers the setting", {
  # Contributed by the block, like blockr.io's data directory: the reader who
  # wants a deck that does not split its tables is not the person who wrote
  # the app, so the option cannot depend on an app author adding it.
  opts <- blockr.core::board_options(new_table_block())

  expect_true("exhibit_min_font_size" %in% names(opts))

  opts <- blockr.core::board_options(new_summarize_table_block())

  expect_true("exhibit_min_font_size" %in% names(opts))
})
