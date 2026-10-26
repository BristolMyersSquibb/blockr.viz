# The compact look of the groups layout draws a narrow tile matrix: a tile
# column is as wide as a square tile, the table is as wide as its cells, and
# the export picture is as wide as the table. The width is decided by the
# stylesheet and the browser's table layout, so the test measures a real
# browser. The table look of the same summary keeps filling the panel.

compact_fixture <- function() {
  set.seed(3)
  subj <- sprintf("S-%03d", 1:24)
  arm <- rep(c("Placebo", "Low", "High"), each = 8)
  terms <- c("Headache", "Dizziness", "Nausea", "Vomiting", "Rash",
             "Pruritus", "Cough", "Fatigue")
  d <- data.frame(USUBJID = rep(subj, 4), ARM = rep(arm, 4))
  d$AEDECOD <- sample(terms, nrow(d), replace = TRUE)
  d$AESEV <- factor(sample(c("MILD", "MODERATE"), nrow(d), replace = TRUE),
                    levels = c("MILD", "MODERATE"))
  d
}

compact_payload <- function(d, compact) {
  look <- if (compact) "compact" else "table"
  summarize_payload_json(summarize_build_payload(
    d, by = c("ARM", "USUBJID"),
    summaries = list(list(type = "simple", func = "count", show = "tile",
                          facet = "AEDECOD", fill = "AESEV", name = "AEs")),
    cfg = list(compact = compact, look = look)
  ))
}

test_that("a compact tile matrix is as wide as its cells, on screen and in the picture", {
  skip_on_cran()
  skip_if_not(chromote_works(), "no headless browser here")
  js_path <- system.file("js", "summarize-table.js", package = "blockr.viz")
  css_path <- system.file("css", "table.css", package = "blockr.viz")
  skip_if(!file.exists(js_path) || !file.exists(css_path),
          "summarize-table.js or table.css not found")

  d <- compact_fixture()
  css <- paste(html_table_shared_css_fallback(), summarize_table_css(),
               sep = "\n")
  container <- function(id) c(
    paste0("<div class='blockr-summarize-container' ",
           "data-summarize-elem-id='", id, "'>"),
    "<div class='dd-table-titles'><div class='dd-table-title'></div>",
    "<div class='dd-table-subtitle'></div></div>",
    "<div class='blockr-summarize-legend'></div>",
    "<div class='blockr-table-wrapper'></div>",
    "<div class='dd-table-caption'></div>",
    "<div class='blockr-summarize-footer'>",
    "<span class='blockr-summarize-note'></span>",
    "<div class='dd-status-footer'></div></div></div>"
  )
  page <- tempfile(fileext = ".html")
  writeLines(c(
    "<!doctype html><html><head>",
    paste0("<link rel='stylesheet' href='file://", css_path, "'>"),
    # Bootstrap sizes every box by its border, as on a board.
    "<style>", css, "*, *::before, *::after { box-sizing: border-box; }",
    "body { margin: 0; width: 1400px; }</style>",
    "</head><body>",
    container("compact"), container("table"),
    "<script>window.__h = {}; window.__in = {};",
    "window.Shiny = {addCustomMessageHandler: function (n, f) {",
    "  window.__h[n] = f },",
    "  setInputValue: function (k, v) { window.__in[k] = v }};</script>",
    paste0("<script src='file://", js_path, "'></script>"),
    "</body></html>"
  ), page)

  sess <- chromote::ChromoteSession$new(width = 1400, height = 900)
  on.exit(try(sess$close(), silent = TRUE), add = TRUE)
  sess$Page$navigate(paste0("file://", page))
  Sys.sleep(1.5)

  json <- list(compact = compact_payload(d, TRUE),
               table = compact_payload(d, FALSE))
  for (id in names(json)) {
    sess$Runtime$evaluate(paste0(
      "window.__h['blockr-viz-summarize-data']({id: '", id, "', rev: 1, ",
      "payload: ", jsonlite::toJSON(json[[id]], auto_unbox = TRUE), "})"
    ))
  }
  Sys.sleep(0.5)

  measure <- function(id) {
    sess$Runtime$evaluate(paste0(
      "(function () {",
      "var c = document.querySelector(\"[data-summarize-elem-id='", id,
      "']\");",
      "var w = c.querySelector('.blockr-table-wrapper');",
      "var t = c.querySelector('table.blockr-summarize-table');",
      "var tile = t.querySelector('.blockr-summarize-tile').",
      "getBoundingClientRect();",
      "return {wrap: w.getBoundingClientRect().width,",
      " table: t.getBoundingClientRect().width,",
      " left: t.getBoundingClientRect().left - w.getBoundingClientRect().left,",
      " cols: Array.from(t.querySelectorAll('thead th.blockr-col-header'))",
      "  .map(function (h) { return h.getBoundingClientRect().width }),",
      " tile: [tile.width, tile.height]};",
      "})()"
    ), returnByValue = TRUE)$result$value
  }

  m <- measure("compact")
  cols <- unlist(m$cols)
  expect_length(cols, 8L)
  expect_true(all(abs(cols - 28) < 1), info = paste(cols, collapse = " "))
  expect_equal(m$tile[[1]], m$tile[[2]], tolerance = 0.5)
  expect_lt(m$tile[[1]], 28)
  expect_equal(m$left, 0, tolerance = 0.5)
  expect_lt(m$table, 0.5 * m$wrap)

  # The table look of the same summary still fills the panel.
  m_tab <- measure("table")
  expect_equal(m_tab$table, m_tab$wrap, tolerance = 1)

  # The export picture of a compact matrix is as wide as its content, the
  # table look's keeps the panel's width.
  picture_width <- function(id) {
    sess$Runtime$evaluate(paste0(
      "new Promise(function (resolve) {",
      "window.__in = {};",
      "window.__h['blockr-viz-summarize-capture']({req: '", id, "', ",
      "payload: ", jsonlite::toJSON(json[[id]], auto_unbox = TRUE), ", ",
      "width: 1400, ratio: 1, css: null});",
      "var poll = setInterval(function () {",
      "  var r = window.__in.blockr_viz_capture_result;",
      "  if (r) { clearInterval(poll); resolve(r.error || r.width) }",
      "}, 50);",
      "})"
    ), awaitPromise = TRUE, returnByValue = TRUE)$result$value
  }
  w_compact <- picture_width("compact")
  expect_true(is.numeric(w_compact), info = as.character(w_compact))
  expect_lt(w_compact, m$table + 60)
  expect_gte(w_compact, m$table)
  expect_equal(picture_width("table"), 1400)
})
