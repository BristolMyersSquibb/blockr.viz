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
  expect_match(html, "hmb-topn", fixed = TRUE)     # Top-n slider
  expect_match(html, "hmb-nums", fixed = TRUE)     # cell-numbers toggle
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
