# The data-push payload (R/table-push.R): the cell model the block server
# ships over "blockr-viz-table-data" instead of rendering the body through
# Shiny. dt_flat_build() is shared with dt_table_tag(), so these tests pin
# the payload shape AND the tag/payload agreement (same displays, same raw
# values, same NA handling) -- the parity contract table.js's row assembler
# relies on. See dev/table-data-push-design.md.

test_that("flat payload carries the head, cell vectors and NA nulls", {
  df <- data.frame(
    name = c("a", "b", NA),
    x    = c(1.5, NA, 3),
    lab  = c("p <5", "q & r", "s"),
    stringsAsFactors = FALSE
  )
  p <- dt_build_payload(df, label_col = "name", drill = "name")

  expect_identical(p$kind, "flat")
  expect_identical(p$n, 3L)
  # Stub + 2 value columns, rendered order.
  expect_length(p$cols, 3L)
  expect_identical(p$cols[[1]]$cls, "blockr-stub")
  expect_identical(p$cols[[2]]$cls, "blockr-data dt-num")
  expect_identical(p$cols[[3]]$cls, "blockr-data dt-txt")

  # Display strings are PLAIN (JS escapes at assembly): no entities.
  expect_identical(as.character(p$cols[[3]]$disp), c("p <5", "q & r", "s"))
  # NA value cells are NA in the model (JSON null -> em-dash cell in JS)...
  expect_true(is.na(p$cols[[2]]$disp[2]))
  # ...but the NA stub keeps the historical literal "NA".
  expect_identical(as.character(p$cols[[1]]$disp), c("a", "b", "NA"))

  # The drill column ships raw values (NA where the click must be a no-op),
  # non-drill columns ship none.
  expect_identical(as.character(p$cols[[1]]$raw), c("a", "b", NA))
  expect_null(p$cols[[2]]$raw)
  # The NA drill key marks its row nodrill (0-based).
  expect_identical(as.integer(p$nodrill), 2L)

  # The head is a complete empty-bodied <table> carrying the gear state.
  expect_match(p$head, "^<table", fixed = FALSE)
  expect_match(p$head, "<tbody></tbody>", fixed = TRUE)
  expect_match(p$head, "data-dt-onclick-col=\"name\"", fixed = TRUE)
  expect_match(p$head, "<colgroup>", fixed = TRUE)
})

test_that("payload and dt_table_tag agree on displays and cell markup", {
  df <- data.frame(
    grp = c("x", "y"),
    val = c(1.25, 2),
    stringsAsFactors = FALSE
  )
  p   <- dt_build_payload(df, label_col = "grp", digits = 2L)
  tag <- dt_table_tag(df, label_col = "grp", digits = 2L)
  html <- as.character(tag)

  # Every payload display string appears verbatim in the rendered cells.
  expect_match(html, ">1.25</td>", fixed = TRUE)
  expect_match(html, ">2</td>", fixed = TRUE)
  expect_identical(as.character(p$cols[[2]]$disp), c("1.25", "2"))
  # The payload head equals the tag's markup minus the tbody content: same
  # attributes, same colgroup, same thead.
  strip_tbody <- function(h) sub("<tbody>.*</tbody>", "<tbody></tbody>", h)
  expect_identical(p$head, strip_tbody(html))
})

test_that("shaded columns ship per-cell style chunks", {
  df <- data.frame(k = c("a", "b"), v = c(-1, 1), stringsAsFactors = FALSE)
  p <- dt_build_payload(
    df, label_col = "k",
    shadings = list(list(mode = "diverging", cols = "v"))
  )
  st <- as.character(p$cols[[2]]$style)
  expect_length(st, 2L)
  expect_match(st[1], "^ style=\"background:")
  # The same chunks appear verbatim in the rendered tag.
  html <- as.character(dt_table_tag(
    df, label_col = "k",
    shadings = list(list(mode = "diverging", cols = "v"))
  ))
  expect_true(all(vapply(st, grepl, logical(1L), x = html, fixed = TRUE)))
})

test_that("structured and message states ship as kind 'html'", {
  # Structured ("Table 1") frame -> the full existing markup.
  sdf <- data.frame(
    .group1_level = c("A", "A"),
    .label = c("n", "Mean"),
    Arm1 = c("10", "1.2"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  ps <- dt_build_payload(sdf)
  expect_identical(ps$kind, "html")
  expect_match(ps$html, "blockr-section-header", fixed = TRUE)
  expect_match(ps$html, "data-dt-structured=\"1\"", fixed = TRUE)

  # Non-renderable state -> the message table, gear attrs intact.
  pm <- dt_build_payload(data.frame(a = 1)[0, , drop = FALSE])
  expect_identical(pm$kind, "html")
  expect_match(pm$html, "No rows to display", fixed = TRUE)
  expect_match(pm$html, "data-dt-cols=", fixed = TRUE)
})

test_that("dt_payload_json keeps 1-row columns as arrays and NA as null", {
  df <- data.frame(k = "a", v = 1.5, stringsAsFactors = FALSE)
  j <- dt_payload_json(dt_build_payload(df, label_col = "k", drill = "k"))
  p <- jsonlite::fromJSON(j, simplifyVector = FALSE)
  expect_identical(p$kind, "flat")
  # A 1-row table's disp/raw stay JSON arrays (the auto_unbox trap).
  expect_true(is.list(p$cols[[1]]$disp))
  expect_length(p$cols[[1]]$disp, 1L)
  expect_true(is.list(p$cols[[1]]$raw))

  dfn <- data.frame(k = c("a", NA), v = c(1, NA), stringsAsFactors = FALSE)
  jn <- dt_payload_json(dt_build_payload(dfn, label_col = "k", drill = "k"))
  pn <- jsonlite::fromJSON(jn, simplifyVector = FALSE)
  # NA disp / raw arrive as JSON null (JS renders the em-dash / no-op click).
  expect_null(pn$cols[[2]]$disp[[2]])
  expect_null(pn$cols[[1]]$raw[[2]])
  # The NA stub display stays the literal "NA" string.
  expect_identical(pn$cols[[1]]$disp[[2]], "NA")
})

test_that("the ctrl stamp lands on the flat head and the html tag alike", {
  stamp <- function(tag) {
    htmltools::tagAppendAttributes(tag, `data-dt-ctrl-target` = "tgt")
  }
  df <- data.frame(k = "a", v = 1, stringsAsFactors = FALSE)
  p <- dt_build_payload(df, label_col = "k", stamp = stamp)
  expect_match(p$head, "data-dt-ctrl-target=\"tgt\"", fixed = TRUE)

  pm <- dt_build_payload(data.frame(a = 1)[0, , drop = FALSE], stamp = stamp)
  expect_match(pm$html, "data-dt-ctrl-target=\"tgt\"", fixed = TRUE)
})
