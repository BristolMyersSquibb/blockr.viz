# new_heatmap_block(): the prep aggregation and the rendered hmb-* markup.

hm_toy <- function() {
  data.frame(
    USUBJID = c("s1", "s1", "s1", "s2", "s2", "s3"),
    AEDECOD = c("RASH", "RASH", "NAUSEA", "NAUSEA", "NAUSEA", "RASH"),
    AESEV = factor(c("MILD", "MODERATE", "MILD", "MODERATE", "MODERATE",
                     "SEVERE"),
                   levels = c("MILD", "MODERATE", "SEVERE"), ordered = TRUE),
    ARM = c("A", "A", "A", "B", "B", "B"),
    stringsAsFactors = FALSE
  )
}

test_that("heatmap_prep counts events and keeps the worst level per cell", {
  p <- heatmap_prep(hm_toy(), "USUBJID", "AEDECOD", "AESEV", "ARM", 25)
  expect_null(p$err)
  expect_identical(p$levels, c("MILD", "MODERATE", "SEVERE"))
  # s1 RASH: 2 events, worst MODERATE (index 2)
  expect_identical(p$count["s1", "RASH"], 2L)
  expect_identical(p$worst["s1", "RASH"], 2L)
  # s3 NAUSEA: no events -> NA in both
  expect_true(is.na(p$count["s3", "NAUSEA"]))
  # group order (A first), burden desc inside: s1 (3) before nothing else in A
  expect_identical(p$rows[1], "s1")
  expect_identical(unname(vapply(p$groups, `[[`, "", "label")),
                   c("A", "B"))
})

test_that("heatmap_prep caps columns at top_n by frequency", {
  p <- heatmap_prep(hm_toy(), "USUBJID", "AEDECOD", "AESEV", NULL, 1)
  # NAUSEA and RASH tie at 3; table() order breaks the tie deterministically
  expect_length(p$terms, 1L)
  expect_identical(p$n_terms_total, 2L)
  expect_null(p$groups)
})

test_that("heatmap_prep reports config states as messages", {
  expect_identical(heatmap_prep(hm_toy(), NULL, "AEDECOD")$err,
                   "Pick the row and column identities")
  expect_match(heatmap_prep(hm_toy(), "NOPE", "AEDECOD")$err, "not in the data")
  expect_identical(heatmap_prep(hm_toy()[0, ], "USUBJID", "AEDECOD")$err,
                   "No data")
})

test_that("heatmap_html renders legend, rail, toolbar, and no dashes", {
  html <- as.character(heatmap_html(
    hm_toy(), row = "USUBJID", col = "AEDECOD", color = "AESEV",
    group = "ARM", top_n = 25, elem_id = "hm1"
  ))
  expect_match(html, "hmb-legend", fixed = TRUE)
  expect_match(html, "MODERATE", fixed = TRUE)     # legend decodes levels
  expect_match(html, 'class="hmb-rail" rowspan="2"', fixed = TRUE)
  # Design-system controls, not bespoke ones: a number field (never a
  # slider -- "top 25" is typed, not dragged) and a real checkbox.
  expect_match(html, "blockr-num-input hmb-topn", fixed = TRUE)
  expect_no_match(html, 'type="range"', fixed = TRUE)
  expect_match(html, "blockr-checkbox hmb-nums", fixed = TRUE)
  expect_match(html, "blockr-checkbox__box", fixed = TRUE)
  expect_false(grepl("&mdash;", html, fixed = TRUE))
  # empty cell renders as a bare tile, no text span
  expect_match(html, '<td class="hmb-c"></td>', fixed = TRUE)
})

test_that("heatmap_html paints by the worst level, not the count", {
  d <- hm_toy()
  html <- as.character(heatmap_html(
    d, row = "USUBJID", col = "AEDECOD", color = "AESEV", top_n = 25,
    elem_id = "hm2"
  ))
  cells <- regmatches(html,
    gregexpr('<td class="hmb-c"[^>]*><span>[0-9]+</span></td>', html))[[1]]
  # s3 RASH: 1 event, SEVERE -> the level-3 (darkest) paint carries fg white
  sev_cell <- grep(">1<", grep("color:#ffffff", cells, value = TRUE),
                   value = TRUE)
  expect_length(sev_cell, 1L)
})

test_that("the Top n field is bounded by the terms actually available", {
  d <- hm_toy()
  html <- as.character(heatmap_html(
    d, row = "USUBJID", col = "AEDECOD", top_n = 25, elem_id = "hm-n"
  ))
  # two distinct terms in the toy frame, so 25 clamps to 2 -- the field
  # can never ask for columns that do not exist
  expect_match(html, 'max="2"', fixed = TRUE)
  expect_match(html, 'value="2"', fixed = TRUE)
})

test_that("cell_numbers = FALSE marks the wrapper", {
  html <- as.character(heatmap_html(
    hm_toy(), row = "USUBJID", col = "AEDECOD", cell_numbers = FALSE,
    top_n = 25, elem_id = "hm3"
  ))
  expect_match(html, "hmb-block hmb-nonum", fixed = TRUE)
})

test_that("new_heatmap_block constructs and carries its state", {
  blk <- new_heatmap_block(row = "USUBJID", col = "AEDECOD",
                           color = "AESEV", group = "ARM",
                           top_n = 10, drill = TRUE)
  expect_s3_class(blk, "heatmap_block")
})

# --- paint source: the board's declared level colours beat the ramp -------

hm_graded <- function() {
  data.frame(
    USUBJID = c("s1", "s1", "s2", "s3"),
    AEDECOD = c("RASH", "RASH", "NAUSEA", "RASH"),
    AETOXGR = c(1, 3, 5, 2),
    stringsAsFactors = FALSE
  )
}

hm_map <- function() {
  list(AETOXGR = list(color = c(
    "1" = "#43978D", "2" = "#264D59", "3" = "#C49102",
    "4" = "#D46C4E", "5" = "#FF0000"
  )))
}

test_that("without a scale map the paint is the sequential ramp", {
  d <- hm_graded()
  p <- heatmap_prep(d, "USUBJID", "AEDECOD", "AETOXGR", NULL, 25)
  bg <- hmb_paint(p, d, NULL)(seq_along(p$levels))$bg
  expect_length(bg, length(p$levels))
  expect_false(any(toupper(bg) %in% c("#43978D", "#FF0000")))
})

test_that("a bound scale map paints each level its declared colour", {
  d <- hm_graded()
  p <- heatmap_prep(d, "USUBJID", "AEDECOD", "AETOXGR", NULL, 25)
  # grade 4 is absent from the data; the levels in view are 1, 2, 3, 5 and
  # each keeps ITS colour (not a ramp position), so a filter that drops a
  # grade never recolours the ones that remain.
  expect_identical(p$levels, c("1", "2", "3", "5"))
  res <- hmb_paint(p, d, hm_map())(seq_along(p$levels))
  expect_identical(res$bg, c("#43978D", "#264D59", "#C49102", "#FF0000"))
  # contrast rule shared with the ramp: white text on the dark swatches
  expect_identical(res$fg[3], "#111827")
  expect_identical(res$fg[4], "#ffffff")
})

test_that("a partly declared map keeps its colours and fills the rest", {
  d <- hm_graded()
  p <- heatmap_prep(d, "USUBJID", "AEDECOD", "AETOXGR", NULL, 25)
  partial <- list(AETOXGR = list(color = c("1" = "#43978D", "2" = "#264D59")))
  bg <- hmb_paint(p, d, partial)(seq_along(p$levels))$bg
  # blockr.theme completes the palette (declared levels + stable fills), so
  # the whole matrix stays on the categorical scale rather than reverting
  # to ramp positions that would move with the filter.
  # bg is indexed by the block's SORTED levels (1, 2, 3, 5), not by the
  # resolver's own order -- the lookup is by name, which is what keeps a
  # cell's colour tied to its grade.
  expect_identical(bg[1], "#43978D")                # level 1, declared
  expect_identical(bg[2], "#264D59")                # level 2, declared
  expect_false(bg[3] %in% c("#43978D", "#264D59"))  # level 3, filled
})

test_that("the rendered legend and cells use the declared colours", {
  d <- hm_graded()
  html <- as.character(heatmap_html(
    d, row = "USUBJID", col = "AEDECOD", color = "AETOXGR",
    top_n = 25, elem_id = "hm-map", scale_map = hm_map()
  ))
  expect_match(html, "#FF0000", fixed = TRUE)   # grade 5 swatch + cell
  expect_match(html, "#43978D", fixed = TRUE)
})

# ---- chrome / body split ---------------------------------------------------
# The chrome is what keeps the panel on screen while the dock churns: it is
# mounted once, from config alone, and the matrix arrives over the data
# channel. A chrome that needed the data would blank the whole block on the
# transient `on_screen=[]` the dock publishes while it arranges.

test_that("the chrome mounts from config alone, with no data in reach", {
  html <- as.character(hmb_chrome(
    elem_id = "hm-chrome", cell_numbers = TRUE, drill = TRUE, top_n = 25L,
    cfg_json = hmb_cfg_json(row = "USUBJID", col = "AEDECOD", top_n = 25L)
  ))
  # the shell: toolbar, gear-bearing attributes, and the slots the body
  # lands in
  expect_match(html, "hmb-toolbar", fixed = TRUE)
  expect_match(html, "blockr-num-input hmb-topn", fixed = TRUE)
  expect_match(html, "hmb-legend-slot", fixed = TRUE)
  expect_match(html, "hmb-scroll", fixed = TRUE)
  expect_match(html, "hmb-footer", fixed = TRUE)
  expect_match(html, 'data-hmb-elem-id="hm-chrome"', fixed = TRUE)
  expect_match(html, "USUBJID", fixed = TRUE)      # config, not data
  # and nothing that could only come from a frame
  expect_no_match(html, 'class="hmb-c"', fixed = TRUE)
  expect_no_match(html, "hmb-rail", fixed = TRUE)
  expect_no_match(html, "hmb-legend\"", fixed = TRUE)
})

test_that("the body carries the matrix, the legend and the frame's bounds", {
  b <- hmb_body(hm_toy(), row = "USUBJID", col = "AEDECOD", color = "AESEV",
                group = "ARM", top_n = 25L)
  expect_null(b$err)
  expect_match(b$table, "hmb-table", fixed = TRUE)
  expect_match(b$table, 'class="hmb-rail" rowspan="2"', fixed = TRUE)
  expect_match(b$legend, "hmb-legend", fixed = TRUE)
  expect_match(b$legend, "MODERATE", fixed = TRUE)
  # two distinct terms in the toy frame, so the ceiling is 2 whatever was
  # asked for -- the Top n field can never offer columns that do not exist
  expect_identical(b$top_max, 2L)
  expect_identical(b$top_val, 2L)
  expect_identical(b$row_col, "USUBJID")
  expect_match(b$count, "of 2 AEDECOD", fixed = TRUE)
  expect_match(b$cols, "USUBJID", fixed = TRUE)
})

test_that("an unrenderable frame reports through the body, not the chrome", {
  b <- hmb_body(hm_toy(), row = NULL, col = "AEDECOD")
  expect_identical(b$err, "Pick the row and column identities")
  expect_null(b$table)
  # the chrome still stands, so the gear that fixes the config is reachable
  html <- as.character(hmb_chrome(elem_id = "hm-err", body = b))
  expect_match(html, "hmb-toolbar", fixed = TRUE)
  expect_match(html, "hmb-empty", fixed = TRUE)
  expect_match(html, "Pick the row and column identities", fixed = TRUE)
})

test_that("chrome plus body is what the standalone render emits", {
  args <- list(hm_toy(), row = "USUBJID", col = "AEDECOD", color = "AESEV",
               group = "ARM", top_n = 25L)
  whole <- as.character(do.call(heatmap_html, c(args, elem_id = "hm-w")))
  b <- do.call(hmb_body, args)
  # every piece the client would otherwise be sent is present inline
  expect_match(whole, b$count, fixed = TRUE)
  expect_true(grepl("hmb-table", whole, fixed = TRUE))
  expect_match(whole, "hmb-legend", fixed = TRUE)
})

# ---- the two assemblers must not drift -------------------------------------
# hmb_cell_model() has two renderers: hmb_assemble_rows() in R (standalone,
# tests) and assembleRows() in heatmap-block.js (the block, off the pushed
# model). The whole point of the model is that both emit the same matrix, so
# the markup is compared byte for byte -- classes, attribute order, escaping.

hm_js_rows <- function(model) {
  node <- Sys.which("node")
  testthat::skip_if(!nzchar(node), "node not available")
  mj <- tempfile(fileext = ".json")
  out <- tempfile(fileext = ".html")
  writeLines(as.character(jsonlite::toJSON(hmb_model_payload(model),
                                           auto_unbox = TRUE)), mj)
  res <- system2(node, c(
    shQuote(testthat::test_path("js", "assemble-rows.js")),
    shQuote(system.file("js", "heatmap-block.js", package = "blockr.viz")),
    shQuote(mj), shQuote(out)
  ), stdout = TRUE, stderr = TRUE)
  testthat::expect_equal(attr(res, "status") %||% 0L, 0L)
  paste(readLines(out, warn = FALSE), collapse = "\n")
}

test_that("the JS assembler emits the same rows as the R one", {
  b <- hmb_body(hm_toy(), row = "USUBJID", col = "AEDECOD", color = "AESEV",
                group = "ARM", top_n = 25L)
  expect_identical(hm_js_rows(b$model), hmb_assemble_rows(b$model))
})

test_that("the assemblers agree without groups, and on the count ramp", {
  # no `color`, so the paint is the sequential count ramp rather than
  # levels -- the palette is keyed by distinct count, the other mode
  b <- hmb_body(hm_toy(), row = "USUBJID", col = "AEDECOD", top_n = 25L)
  expect_identical(hm_js_rows(b$model), hmb_assemble_rows(b$model))
})

test_that("the assemblers agree on markup that has to be escaped", {
  d <- hm_toy()
  d$AEDECOD <- ifelse(d$AEDECOD == d$AEDECOD[1], "R&D <lab>", d$AEDECOD)
  d$USUBJID <- paste0(d$USUBJID, " <&>")
  d$ARM <- paste0(d$ARM, " & co")
  b <- hmb_body(d, row = "USUBJID", col = "AEDECOD", color = "AESEV",
                group = "ARM", top_n = 25L)
  rows <- hmb_assemble_rows(b$model)
  expect_match(rows, "&lt;&amp;&gt;", fixed = TRUE)
  expect_identical(hm_js_rows(b$model), rows)
})
