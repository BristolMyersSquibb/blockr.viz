# Rank table: the data-push cell model, and the R/JS drift guard.
#
# The payload exists so the body ships as ~40 KB of numbers instead of ~780 KB
# of HTML, which only works if rank-table.js assembles exactly the markup
# rank_cells_html() pastes. The last test in this file runs the REAL JS
# assembler in a headless browser and compares the two strings byte for byte --
# without it the two renderers would drift silently.

push_fixture <- function() {
  subj <- sprintf("S%03d", seq_len(30))
  rows <- do.call(rbind, lapply(seq_len(4), function(i) {
    n <- 20L - 4L * i
    data.frame(
      USUBJID = subj[seq_len(n)],
      TERM = if (i == 1L) "T <1> & co" else paste0("T", i),
      SOC = if (i <= 2L) "SOC A" else "SOC B",
      SEV = rep(c("MILD", "MODERATE"), length.out = n),
      AVAL = seq_len(n),
      stringsAsFactors = FALSE
    )
  }))
  rows$ARM <- factor(rep(c("Placebo", "Active"), length.out = nrow(rows)),
                     levels = c("Placebo", "Active"))
  rows$SEV <- factor(rows$SEV, levels = c("MILD", "MODERATE"))
  rows
}

test_that("the flat payload carries the head and the per-column vectors", {
  ae <- push_fixture()
  p <- rank_build_payload(ae, group = "TERM", func = "count_distinct",
                          id_var = "USUBJID", drill = "TERM")

  expect_identical(p$kind, "flat")
  expect_identical(p$n, 4L)
  expect_true(p$pick)
  # head is the <table> with its data attributes and an EMPTY tbody: the gear
  # keeps reading its state off those attributes.
  expect_match(p$head, "<tbody></tbody></table>$")
  expect_match(p$head, "blockr-rank-table")
  # Row meta, full length and aligned.
  expect_length(p$label, 4L)
  # A constant vector is omitted, not shipped: a flat table has no levels, no
  # parents and (unfiltered) no active row, which is ~14 KB at 790 rows. The
  # assembler reads an absent vector as all-false / level 0.
  expect_null(p$level)
  expect_null(p$parent)
  expect_null(p$parent_row)
  expect_null(p$on)
  # One plan column by default: the bar, which carries its own value label
  # (disp + pct + the column's ONE label-slot width).
  expect_identical(vapply(p$cols, function(c) c$kind, ""), "bar")
  expect_length(p$cols[[1]]$w, 4L)
  expect_length(p$cols[[1]]$disp, 4L)
  expect_length(p$cols[[1]]$pct, 4L)
  expect_true(is.numeric(p$cols[[1]]$dw))
})

test_that("labels ship PLAIN and are escaped by each consumer, not the payload", {
  ae <- push_fixture()
  p <- rank_build_payload(ae, group = "TERM", func = "count")
  expect_true("T <1> & co" %in% p$label)     # raw in the payload
  html <- as.character(htmltools::renderTags(
    rank_table(ae, group = "TERM", func = "count")
  )$html)
  expect_match(html, "T &lt;1&gt; &amp; co", fixed = TRUE)
})

test_that("nested tables DO ship the level and parent vectors", {
  ae <- push_fixture()
  p <- rank_build_payload(ae, group = "TERM", parent = "SOC", func = "count")
  expect_true(any(as.integer(p$level) > 0L))
  expect_true(any(as.logical(p$parent_row)))
  expect_true("SOC A" %in% as.character(p$parent))
})

test_that("a split column ships one width vector per series", {
  ae <- push_fixture()
  p <- rank_build_payload(ae, group = "TERM", color = "SEV", func = "count",
                          bar_mode = "percent")
  c1 <- p$cols[[1]]
  expect_identical(c1$kind, "barsplit")
  expect_identical(c1$mode, "percent")
  expect_identical(as.character(c1$names), c("MILD", "MODERATE"))
  expect_length(c1$seg, 2L)
  # percent mode: the two segments fill each row.
  sums <- c1$seg[[1]] + c1$seg[[2]]
  expect_true(all(abs(sums - 100) < 0.02))
})

test_that("a non-renderable state ships as kind html, not a cell model", {
  ae <- push_fixture()
  p <- rank_build_payload(ae, group = NULL)
  expect_identical(p$kind, "html")
  expect_match(p$html, "Pick a Group column")
  # The chrome still travels: the footer and legend slots get cleared.
  expect_true(!is.null(p$chrome))
})

test_that("the chrome rides on the payload so the container is never rebuilt", {
  ae <- push_fixture()
  p <- rank_build_payload(
    ae, chrome = list(title = "Ranked", subtitle = "N = 30", caption = "src"),
    group = "TERM", facet = "ARM", color = "SEV", func = "count_distinct",
    id_var = "USUBJID"
  )
  expect_identical(p$chrome$title, "Ranked")
  expect_identical(p$chrome$caption, "src")
  # The legend maps the COLOUR levels; a plain facet carries none (its
  # column headers already name the levels).
  expect_identical(p$chrome$legend$title, "SEV")
  expect_length(p$chrome$legend$items, 2L)
  expect_match(p$chrome$foot$count, "of 4 rows")
  plain <- rank_build_payload(ae, group = "TERM", facet = "ARM",
                              func = "count_distinct", id_var = "USUBJID")
  expect_null(plain$chrome$legend)
})

test_that("the payload is smaller than the markup it replaces", {
  ae <- push_fixture()
  args <- list(group = "TERM", facet = "ARM", func = "count_distinct",
               id_var = "USUBJID")
  json <- rank_payload_json(do.call(rank_build_payload, c(list(ae), args)))
  html <- as.character(htmltools::renderTags(
    do.call(rank_table, c(list(ae), args))
  )$html)
  expect_lt(nchar(json, type = "bytes"), nchar(html, type = "bytes"))
})

test_that("json keeps single-row columns as arrays", {
  ae <- push_fixture()
  one <- ae[ae$TERM == "T3", , drop = FALSE]
  p <- rank_build_payload(one, group = "TERM", func = "count")
  j <- rank_payload_json(p)
  # A length-1 column must stay [x], never x: the JS assembler indexes it.
  expect_match(j, '"label":\\[')
  expect_match(j, '"w":\\[')
})

# --- the drift guard --------------------------------------------------------

test_that("rank-table.js assembles byte-identical markup to rank_cells_html", {
  skip_on_cran()
  skip_if_not(chromote_works(), "no headless browser here")

  ae <- push_fixture()
  # One case per bar shape, so every branch of the assembler is compared.
  cases <- list(
    plain = list(group = "TERM", func = "count_distinct", id_var = "USUBJID"),
    stacked = list(group = "TERM", color = "SEV", func = "count"),
    grouped = list(group = "TERM", color = "SEV", func = "count",
                   bar_mode = "grouped"),
    percent = list(group = "TERM", color = "SEV", func = "count",
                   bar_mode = "percent"),
    facet = list(group = "TERM", facet = "ARM", func = "count_distinct",
                 id_var = "USUBJID"),
    compare = list(group = "TERM", facet = "ARM", compare = "Placebo",
                   func = "count_distinct", id_var = "USUBJID"),
    facet_color = list(group = "TERM", facet = "ARM", color = "SEV",
                       func = "count"),
    identity = list(group = "USUBJID", func = "identity", value = "AVAL",
                    fields = c("SOC", "AVAL")),
    sep_cols = list(group = "TERM", func = "count", cols = c("n", "pct")),
    nested = list(group = "TERM", parent = "SOC", func = "count"),
    capped = list(group = "TERM", func = "count", top_n = 2L)
  )

  js_path <- system.file("js", "rank-table.js", package = "blockr.viz")
  skip_if(!nzchar(js_path) || !file.exists(js_path), "rank-table.js not found")

  sess <- chromote::ChromoteSession$new()
  on.exit(try(sess$close(), silent = TRUE), add = TRUE)
  page <- tempfile(fileext = ".html")
  writeLines(c(
    "<!doctype html><html><body>",
    "<div class='blockr-rank-container' data-rank-elem-id='t1'>",
    "<div class='dd-table-titles'><div class='dd-table-title'></div>",
    "<div class='dd-table-subtitle'></div></div>",
    "<div class='blockr-rank-legend'></div>",
    "<div class='blockr-table-wrapper'></div>",
    "<div class='dd-table-caption'></div>",
    "<div class='blockr-rank-footer'><span class='blockr-rank-count'></span>",
    "<span class='blockr-rank-note'></span>",
    "<span class='blockr-rank-status'><span class='blockr-rank-status-text'>",
    "</span></span></div></div>",
    "<script>window.Shiny={addCustomMessageHandler:function(n,f){",
    "window.__h=f}};</script>",
    paste0("<script src='file://", js_path, "'></script>"),
    "</body></html>"
  ), page)
  sess$Page$navigate(paste0("file://", page))
  Sys.sleep(1.5)

  for (nm in names(cases)) {
    args <- cases[[nm]]
    payload <- do.call(rank_build_payload, c(list(ae), args))
    r_html <- as.character(do.call(rank_table_html, list(
      do.call(rank_prepare, c(list(ae), args))
    )))
    json <- rank_payload_json(payload)
    sess$Runtime$evaluate(paste0(
      "window.__h({id:'t1', rev:", which(names(cases) == nm), ", payload:",
      jsonlite::toJSON(json, auto_unbox = TRUE), "})"
    ))
    js_html <- sess$Runtime$evaluate(
      "document.querySelector('.blockr-table-wrapper').innerHTML",
      returnByValue = TRUE
    )$result$value
    # The browser normalises the head tag's attribute quoting, so compare the
    # part the assembler builds: the tbody.
    r_body <- sub(".*<tbody>", "", sub("</tbody>.*", "", r_html))
    r_body <- sess$Runtime$evaluate(paste0(
      "(function(){var t=document.createElement('table');",
      "t.innerHTML='<tbody>' + ", jsonlite::toJSON(r_body, auto_unbox = TRUE),
      " + '</tbody>';",
      "return t.querySelector('tbody').innerHTML})()"
    ), returnByValue = TRUE)$result$value
    js_body <- sub(".*<tbody>", "", sub("</tbody>.*", "", js_html))
    expect_identical(js_body, r_body, info = nm)
  }
})
