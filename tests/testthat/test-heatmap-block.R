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
  p <- heatmap_prep(hm_toy(), "USUBJID", "AEDECOD", "AESEV", "ARM")
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

test_that("heatmap_prep draws every column, most events first", {
  d <- hm_toy()
  d <- rbind(d, transform(d[1, ], AEDECOD = "COUGH"))
  p <- heatmap_prep(d, "USUBJID", "AEDECOD", "AESEV")
  # No cap in the engine: a cap is the prepare script's business.
  expect_identical(p$n_terms_total, 3L)
  expect_identical(p$terms[3], "COUGH")
  expect_null(p$groups)
})

test_that("heatmap_prep reads the columns' labels for the exhibit", {
  d <- hm_toy()
  attr(d$USUBJID, "label") <- "Unique Subject Identifier"
  attr(d$AESEV, "label") <- "Severity/Intensity"
  p <- heatmap_prep(d, "USUBJID", "AEDECOD", "AESEV", "ARM")
  expect_identical(p$row_label, "Unique Subject Identifier")
  expect_identical(p$color_label, "Severity/Intensity")
  # no label: the name
  expect_identical(p$col_label, "AEDECOD")
  expect_identical(p$group_label, "ARM")
})

test_that("heatmap_prep reports config states as messages", {
  expect_identical(heatmap_prep(hm_toy(), NULL, "AEDECOD")$err,
                   "Pick a row and a column in the settings.")
  expect_identical(heatmap_prep(hm_toy(), NULL, "AEDECOD")$err_kind, "empty")
  bad <- heatmap_prep(hm_toy(), "NOPE", "AEDECOD")
  expect_match(bad$err, "not in the data")
  expect_identical(bad$err_kind, "danger")
  expect_identical(heatmap_prep(hm_toy()[0, ], "USUBJID", "AEDECOD")$err,
                   "No data")
})

test_that("heatmap_html renders legend, group rows, header row, no dashes", {
  html <- as.character(heatmap_html(
    hm_toy(), row = "USUBJID", col = "AEDECOD", color = "AESEV",
    group = "ARM", elem_id = "hm1"
  ))
  expect_match(html, "hmb-legend", fixed = TRUE)
  expect_match(html, "MODERATE", fixed = TRUE)     # legend decodes levels
  # a group opens with a section-title row spanning the matrix
  expect_match(html, '<tr class="hmb-grp"><td colspan="3"><span class="hmb-gt">A</span><span class="hmb-gn">1</span></td></tr>', fixed = TRUE)
  expect_no_match(html, "hmb-rail", fixed = TRUE)
  # the header row: title slots, the search tool, no controls on the face
  expect_match(html, "hmb-head", fixed = TRUE)
  expect_match(html, "hmb-search-btn", fixed = TRUE)
  expect_match(html, 'data-blockr-tooltip="Search"', fixed = TRUE)
  expect_no_match(html, "hmb-topn", fixed = TRUE)
  expect_no_match(html, "hmb-nums", fixed = TRUE)
  expect_no_match(html, " title=", fixed = TRUE)
  expect_false(grepl("&mdash;", html, fixed = TRUE))
  # empty cell renders as a bare tile, no text span
  expect_match(html, '<td class="hmb-c"></td>', fixed = TRUE)
})

test_that("heatmap_html paints by the worst level, not the count", {
  d <- hm_toy()
  html <- as.character(heatmap_html(
    d, row = "USUBJID", col = "AEDECOD", color = "AESEV",
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
    elem_id = "hm3"
  ))
  expect_match(html, "hmb-block hmb-nonum", fixed = TRUE)
})

test_that("new_heatmap_block constructs and carries its state", {
  blk <- new_heatmap_block(row = "USUBJID", col = "AEDECOD",
                           color = "AESEV", group = "ARM",
                           drill = TRUE, subtitle = "{label(@col)}")
  expect_s3_class(blk, "heatmap_block")
})

test_that("a legacy top_n becomes the script's top_n value", {
  blk <- new_heatmap_block(row = "USUBJID", col = "AEDECOD", top_n = 10)
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      st <- session$returned$state
      expect_identical(st$values()$top_n, 10L)
      expect_null(st$top_n())
      expect_null(st$max_height())
    },
    args = list(x = blk, data = list(data = function() hm_toy()))
  )
})

test_that("a script value set from the sentence reaches the script", {
  blk <- new_heatmap_block(
    row = "USUBJID", col = "AEDECOD",
    script = paste(
      "top_n <- 2  #| number(min = 1, max = 60)",
      "top <- dplyr::count(data, AEDECOD, sort = TRUE)",
      "dplyr::filter(data, AEDECOD %in% utils::head(top$AEDECOD, top_n))",
      sep = "\n"
    )
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      expr_scope <- session$makeScope("expr")
      session$flushReact()
      expr_scope$setInputs(heatmap_block_action = list(
        action = "config", param = "sv_top_n", value = 1
      ))
      session$flushReact()
      expect_identical(session$returned$state$values()$top_n, 1)
      ex <- do.call(bquote, list(session$returned$expr(),
                                 list(data = quote(d))))
      out <- eval(ex, list(d = hm_toy()))
      # the matrix, cut to the one term the script kept
      expect_identical(names(out)[-1L], "NAUSEA")
    },
    args = list(x = blk, data = list(data = function() hm_toy()))
  )
})

test_that("the script's values are words in the sentence", {
  sc <- paste(
    "top_n <- 1  #| number(min = 1, max = 60)",
    "top <- dplyr::count(data, AEDECOD, sort = TRUE)",
    "dplyr::filter(data, AEDECOD %in% utils::head(top$AEDECOD, top_n))",
    sep = "\n"
  )
  parsed <- cb_parse(sc)
  specs <- cb_specs(parsed, hm_toy())
  a <- script_title_args(list(col = "AEDECOD"), specs, list())
  expect_identical(a$top_n, 1)
  parts <- block_title_parts("Top {@top_n} {@col}", hm_toy(), args = a)
  expect_identical(vapply(parts, function(p) p$arg %||% "", ""),
                   c("", "top_n", "", "col"))
  run <- dd_prepare_run(hm_toy(), parsed, specs, list())
  expect_null(run$error)
  expect_length(unique(run$data$AEDECOD), 1L)
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
  p <- heatmap_prep(d, "USUBJID", "AEDECOD", "AETOXGR", NULL)
  bg <- hmb_paint(p, d, NULL)(seq_along(p$levels))$bg
  expect_length(bg, length(p$levels))
  expect_false(any(toupper(bg) %in% c("#43978D", "#FF0000")))
})

test_that("a bound scale map paints each level its declared colour", {
  d <- hm_graded()
  p <- heatmap_prep(d, "USUBJID", "AEDECOD", "AETOXGR", NULL)
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
  p <- heatmap_prep(d, "USUBJID", "AEDECOD", "AETOXGR", NULL)
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
    elem_id = "hm-map", scale_map = hm_map()
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
    elem_id = "hm-chrome", cell_numbers = TRUE,
    cfg_json = hmb_cfg_json(row = "USUBJID", col = "AEDECOD")
  ))
  # the shell: header row, gear-bearing attributes, and the slots the body
  # lands in
  expect_match(html, "hmb-head", fixed = TRUE)
  expect_match(html, "hmb-title", fixed = TRUE)
  expect_match(html, "hmb-legend-slot", fixed = TRUE)
  expect_match(html, "hmb-scroll", fixed = TRUE)
  expect_match(html, "hmb-caption", fixed = TRUE)
  expect_match(html, "hmb-footer", fixed = TRUE)
  expect_match(html, 'data-hmb-elem-id="hm-chrome"', fixed = TRUE)
  expect_match(html, "USUBJID", fixed = TRUE)      # config, not data
  # a click only ever sends: no Reset, and no rows take clicks until the
  # board answers with a drill filter
  expect_no_match(html, "hmb-reset", fixed = TRUE)
  expect_match(html, 'data-hmb-drill="0"', fixed = TRUE)
  # and nothing that could only come from a frame
  expect_no_match(html, 'class="hmb-c"', fixed = TRUE)
  expect_no_match(html, "hmb-grp", fixed = TRUE)
  expect_no_match(html, "hmb-legend\"", fixed = TRUE)
})

test_that("the config carries the sentence and the script to the client", {
  cfg <- jsonlite::fromJSON(hmb_cfg_json(
    row = "USUBJID", col = "AEDECOD",
    titles = list(title = NULL, subtitle = "{@col}",
                  resolved = list(subtitle = "AEDECOD",
                                  parts = list(subtitle = list(
                                    list(text = "AEDECOD", arg = "col",
                                         by = "name"))))),
    script = list(text = "top_n <- 25", inputs = list(),
                  cfg = list(sv_top_n = 25))
  ), simplifyVector = FALSE)
  expect_null(cfg$title)
  expect_identical(cfg$subtitle, "{@col}")
  expect_identical(cfg$subtitle_parts[[1]]$arg, "col")
  expect_identical(cfg$script, "top_n <- 25")
  expect_identical(cfg$sv_top_n, 25L)
  expect_identical(cfg$cell_numbers, "on")
})

test_that("the body carries the matrix, the legend and the count", {
  b <- hmb_body(hm_toy(), row = "USUBJID", col = "AEDECOD", color = "AESEV",
                group = "ARM")
  expect_null(b$err)
  expect_match(b$table, "hmb-table", fixed = TRUE)
  expect_match(b$table, 'class="hmb-grp"', fixed = TRUE)
  expect_match(b$legend, "hmb-legend", fixed = TRUE)
  expect_match(b$legend, "MODERATE", fixed = TRUE)
  expect_match(b$legend, "Numbers: events", fixed = TRUE)
  expect_identical(b$row_col, "USUBJID")
  expect_identical(b$count, "3 USUBJID \u00d7 2 AEDECOD")
  expect_match(b$cols, "USUBJID", fixed = TRUE)
})

test_that("an unrenderable frame reports through the body, not the chrome", {
  b <- hmb_body(hm_toy(), row = NULL, col = "AEDECOD")
  expect_identical(b$err, "Pick a row and a column in the settings.")
  expect_null(b$table)
  # the chrome still stands, so the gear that fixes the config is reachable
  html <- as.character(hmb_chrome(elem_id = "hm-err", body = b))
  expect_match(html, "hmb-head", fixed = TRUE)
  expect_match(html, "blockr-empty", fixed = TRUE)
  expect_match(html, "Pick a row and a column", fixed = TRUE)
  # a column that is gone is an error, in the message style
  b2 <- hmb_body(hm_toy(), row = "NOPE", col = "AEDECOD")
  html2 <- as.character(hmb_chrome(elem_id = "hm-err2", body = b2))
  expect_match(html2, "hmb-msg hmb-msg--danger", fixed = TRUE)
})

test_that("chrome plus body is what the standalone render emits", {
  args <- list(hm_toy(), row = "USUBJID", col = "AEDECOD", color = "AESEV",
               group = "ARM")
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
                group = "ARM")
  expect_identical(hm_js_rows(b$model), hmb_assemble_rows(b$model))
})

test_that("the assemblers agree without groups, and on the count ramp", {
  # no `color`, so the paint is the sequential count ramp rather than
  # levels -- the palette is keyed by distinct count, the other mode
  b <- hmb_body(hm_toy(), row = "USUBJID", col = "AEDECOD")
  expect_identical(hm_js_rows(b$model), hmb_assemble_rows(b$model))
})

test_that("the assemblers agree on markup that has to be escaped", {
  d <- hm_toy()
  d$AEDECOD <- ifelse(d$AEDECOD == d$AEDECOD[1], "R&D <lab>", d$AEDECOD)
  d$USUBJID <- paste0(d$USUBJID, " <&>")
  d$ARM <- paste0(d$ARM, " & co")
  b <- hmb_body(d, row = "USUBJID", col = "AEDECOD", color = "AESEV",
                group = "ARM")
  rows <- hmb_assemble_rows(b$model)
  expect_match(rows, "&lt;&amp;&gt;", fixed = TRUE)
  expect_identical(hm_js_rows(b$model), rows)
})

test_that("the chrome renders before the upstream data exists", {
  # The chrome is rendered once, isolated. A read in it that throws on a
  # missing upstream (the prepare script, the titles) leaves an empty panel
  # that never comes back -- what a clinical board showed.
  blk <- new_heatmap_block(row = "USUBJID", col = "AEDECOD",
                           script = "top_n <- 2\ndata")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      expr_scope <- session$makeScope("expr")
      html <- as.character(expr_scope$output$heatmap_result$html)
      expect_match(html, "hmb-block", fixed = TRUE)
      expect_match(html, "hmb-head", fixed = TRUE)
    },
    args = list(x = blk, data = list(data = function() shiny::req(FALSE)))
  )
})

test_that("heatmap_matrix is the matrix the block draws", {
  d <- hm_toy()
  attr(d$USUBJID, "label") <- "Unique Subject Identifier"
  m <- heatmap_matrix(d, row = "USUBJID", col = "AEDECOD", group = "ARM")
  expect_identical(names(m), c("USUBJID", "ARM", "NAUSEA", "RASH"))
  expect_identical(as.character(m$USUBJID), c("s1", "s2", "s3"))
  expect_identical(m$RASH, c(2L, 0L, 1L))
  expect_identical(attr(m$USUBJID, "label"), "Unique Subject Identifier")
  expect_error(heatmap_matrix(d, row = "NOPE", col = "AEDECOD"),
               "not in the data")
})

test_that("the block's result is the matrix, after the script", {
  blk <- new_heatmap_block(
    row = "USUBJID", col = "AEDECOD", group = "ARM",
    script = paste(
      "top_n <- 1  #| number(min = 1, max = 60)",
      "top <- dplyr::count(data, AEDECOD, sort = TRUE)",
      "dplyr::filter(data, AEDECOD %in% utils::head(top$AEDECOD, top_n))",
      sep = "\n"
    )
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      ex <- session$returned$expr()
      expect_identical(ex[[1L]], quote(blockr.viz::heatmap_matrix))
      ex <- do.call(bquote, list(ex, list(data = quote(d))))
      out <- eval(ex, list(d = hm_toy()))
      expect_identical(ncol(out), 3L)   # the row, the group, one term
    },
    args = list(x = blk, data = list(data = function() hm_toy()))
  )
})

test_that("the old drill arguments are legacy and restore quietly", {
  blk <- new_heatmap_block(row = "USUBJID", col = "AEDECOD", drill = TRUE,
                           filter_column = "USUBJID", filter_values = "s1",
                           ctrl_target = "pt_drill")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      st <- session$returned$state
      for (k in c("drill", "filter_column", "filter_values", "ctrl_target",
                  "ctrl_table")) {
        expect_null(st[[k]]())
      }
      # a restored filter no longer filters: the result is the whole matrix
      session$flushReact()
      ex <- do.call(bquote, list(session$returned$expr(),
                                 list(data = quote(d))))
      expect_identical(nrow(eval(ex, list(d = hm_toy()))), 3L)
    },
    args = list(x = blk, data = list(data = function() hm_toy()))
  )
})

test_that("the download frame carries the cell paint into every format (#166)", {
  p <- heatmap_prep(hm_toy(), "USUBJID", "AEDECOD", "AESEV", "ARM")
  ex <- hmb_exhibit_frame(p, hm_toy())
  # s1 RASH worst MODERATE, s3 RASH worst SEVERE, s3 NAUSEA empty
  s1 <- ex$USUBJID == "s1"
  s3 <- ex$USUBJID == "s3"
  expect_false(is.na(ex[[".bg:RASH"]][s1]))
  expect_false(identical(ex[[".bg:RASH"]][s1], ex[[".bg:RASH"]][s3]))
  expect_true(is.na(ex[[".bg:NAUSEA"]][s3]))
  # the companions are structure, not columns
  expect_false(any(grepl("^\\.(bg|fg):", names(as_plain_df(ex)))))

  hexes <- toupper(sub("^#", "", unique(stats::na.omit(ex[[".bg:RASH"]]))))

  skip_if_not_installed("openxlsx")
  fx <- withr::local_tempfile(fileext = ".xlsx")
  write_annotated_xlsx(ex, fx)
  td <- withr::local_tempdir()
  utils::unzip(fx, exdir = td)
  st <- paste(readLines(file.path(td, "xl/styles.xml"), warn = FALSE),
              collapse = "")
  for (h in hexes) expect_match(st, h, fixed = TRUE)

  fh <- withr::local_tempfile(fileext = ".html")
  write_exhibit_html(ex, fh)
  h <- paste(readLines(fh, warn = FALSE), collapse = "")
  for (hex in unique(stats::na.omit(ex[[".bg:RASH"]]))) {
    expect_match(h, paste0("background:", hex), fixed = TRUE)
  }
  expect_false(grepl(".bg:", h, fixed = TRUE))

  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  fp <- withr::local_tempfile(fileext = ".pptx")
  write_exhibit_pptx(ex, fp)
  tp <- withr::local_tempdir()
  utils::unzip(fp, exdir = tp)
  sl <- paste(unlist(lapply(
    list.files(file.path(tp, "ppt/slides"), "xml$", full.names = TRUE),
    readLines, warn = FALSE
  )), collapse = "")
  for (h in hexes) expect_match(toupper(sl), h, fixed = TRUE)
})
