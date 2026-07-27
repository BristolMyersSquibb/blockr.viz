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
  # Lane-mark columns: a duration with decimals (box / pointrange /
  # sparkline value), interval start/end spans, and a band around DUR.
  rows$DUR <- rows$AVAL * 1.5 + 0.25
  rows$SDY <- rows$AVAL * 3L
  rows$EDY <- rows$AVAL * 3L + 5L
  rows$LO <- rows$DUR - 1
  rows$HI <- rows$DUR + 1
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

# --- the lane marks ----------------------------------------------------------

test_that("a box column ships pre-rounded positions AND widths", {
  ae <- push_fixture()
  p <- rank_build_payload(ae, group = "TERM", mark = "box", value = "DUR")
  c1 <- p$cols[[1]]
  expect_identical(c1$kind, "box")
  for (nm in c("wl", "w1", "bl", "bw", "bc", "b2", "w2", "wh", "nn", "tip")) {
    expect_length(c1[[nm]], 4L)
  }
  # The trap the spec calls out: the domain runs to the widest WHISKER, so
  # the global max whisker sits exactly at the track edge and nothing ever
  # renders past it.
  expect_equal(max(as.numeric(c1$wh)), 100)
  for (nm in c("wl", "bl", "bc", "b2", "wh")) {
    expect_true(all(as.numeric(c1[[nm]]) <= 100))
  }
  # Widths are shipped, never re-derived: left whisker ends where the box
  # starts (within the 2dp rounding the payload carries).
  expect_true(all(abs(as.numeric(c1$wl) + as.numeric(c1$w1) -
                        as.numeric(c1$bl)) <= 0.02))
  # The tooltip carries the statistic's own words.
  expect_match(c1$tip[[1]], "Median .* Q1–Q3 .* 1\\.5×IQR")
})

test_that("a pointrange with n = 1 ships NA bounds and a center-only cell", {
  ae <- push_fixture()
  one <- ae[!duplicated(ae$TERM), , drop = FALSE]   # one row per term
  p <- rank_build_payload(one, group = "TERM", mark = "pointrange",
                          value = "DUR", summary = "mean_ci95")
  c1 <- p$cols[[1]]
  expect_identical(c1$kind, "pointrange")
  expect_true(all(is.na(as.numeric(c1$rw))))
  expect_false(anyNA(as.numeric(c1$c)))
  # Tips ship pre-escaped (both consumers paste them into an attribute).
  expect_match(c1$tip[[1]], "undefined (n &lt; 2)", fixed = TRUE)
  # And the emitter draws the dot alone.
  html <- rank_cells_html(rank_cells(rank_prepare(
    one, group = "TERM", mark = "pointrange", value = "DUR",
    summary = "mean_ci95"
  )))
  expect_match(html, "lane-ctr")
  expect_false(grepl("lane-rng", html))
})

test_that("an interval column ships per-row segments on the observed domain", {
  ae <- push_fixture()
  p <- rank_build_payload(ae, group = "USUBJID", mark = "interval",
                          x = "SDY", xend = "EDY", color = "SEV")
  c1 <- p$cols[[1]]
  expect_identical(c1$kind, "interval")
  expect_length(c1$segs, p$n)
  expect_length(c1$tips, p$n)
  # Subjects appearing in all four terms carry four spans.
  expect_true(any(lengths(c1$segs) > 1L))
  # Fill index points into the level fills; tips carry the level name.
  expect_identical(as.character(c1$fills),
                   unname(rank_level_colors(NULL, "SEV",
                                            c("MILD", "MODERATE"))))
  expect_match(c1$tips[[1]][[1]], "^(MILD|MODERATE) · ")
  # The domain is the observed span range, not zero-based.
  expect_equal(c1$d0, min(ae$SDY))
  expect_equal(c1$d1, max(ae$EDY))
  # An Events count column rides beside the lane.
  expect_identical(p$cols[[2]]$kind, "num")
})

test_that("a sparkline column ships pre-printed geometry plus hover values", {
  ae <- push_fixture()
  p <- rank_build_payload(ae, group = "TERM", mark = "sparkline",
                          x = "AVAL", value = "DUR", lo = "LO", hi = "HI")
  c1 <- p$cols[[1]]
  expect_identical(c1$kind, "sparkline")
  expect_length(c1$pl, 4L)
  expect_match(c1$pl[[1]], "^[0-9.,]+( [0-9.,]+)+$")
  expect_false(anyNA(c1$bd))          # the band columns are complete
  expect_match(c1$xs[[1]], ",")       # hover snap data
  # Sort value = the last y per row.
  expect_equal(as.numeric(c1$v),
               round(unname(vapply(split(ae, ae$TERM), function(d) {
                 d$DUR[order(d$AVAL)][nrow(d)]
               }, numeric(1))[p$label]), 4))
})

test_that("a sparkline with func gains a companion rank bar", {
  ae <- push_fixture()
  p <- rank_build_payload(ae, group = "TERM", mark = "sparkline",
                          x = "AVAL", value = "DUR", func = "mean")
  expect_identical(vapply(p$cols, function(c) c$kind, ""),
                   c("bar", "sparkline"))
  # The bar ranks the rows: row order = mean(DUR) per term, descending.
  means <- vapply(split(ae$DUR, ae$TERM), mean, numeric(1))
  expect_identical(as.character(p$label),
                   names(sort(means, decreasing = TRUE)))
  # The sparkline column itself still sorts by LAST value, not the mean.
  expect_false(identical(as.numeric(p$cols[[2]]$v),
                         as.numeric(p$cols[[1]]$v)))
  # A counting func means no bar (the constructor's bar-era default).
  p2 <- rank_build_payload(ae, group = "TERM", mark = "sparkline",
                           x = "AVAL", value = "DUR", func = "count")
  expect_identical(vapply(p2$cols, function(c) c$kind, ""), "sparkline")
})

test_that("negative lows extend the distribution domain below zero", {
  d <- data.frame(g = rep(c("a", "b"), each = 6),
                  v = c(-5, -2, 0, 1, 2, 3, 1, 2, 3, 4, 5, 6))
  prep <- rank_prepare(d, group = "g", mark = "box", value = "v")
  expect_lt(prep$bar_min, 0)
  m <- rank_cells(prep)
  c1 <- m$cols[[1]]
  # Everything still renders inside the track.
  expect_true(all(as.numeric(c1$wl) >= 0, na.rm = TRUE))
  expect_true(all(as.numeric(c1$wh) <= 100, na.rm = TRUE))
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
    capped = list(group = "TERM", func = "count", top_n = 2L),
    # One case per lane mark, so their emitters are byte-compared too.
    box = list(group = "TERM", mark = "box", value = "DUR"),
    box_facet = list(group = "TERM", mark = "box", value = "DUR",
                     facet = "ARM"),
    box_nested = list(group = "TERM", parent = "SOC", mark = "box",
                      value = "DUR"),
    pointrange = list(group = "TERM", mark = "pointrange", value = "DUR",
                      summary = "mean_ci95"),
    pr_n1 = list(group = "USUBJID", mark = "pointrange", value = "DUR",
                 summary = "mean_ci95"),
    interval = list(group = "USUBJID", mark = "interval", x = "SDY",
                    xend = "EDY", color = "SEV"),
    sparkline = list(group = "TERM", mark = "sparkline", x = "AVAL",
                     value = "DUR", lo = "LO", hi = "HI"),
    sparkline_bar = list(group = "TERM", mark = "sparkline", x = "AVAL",
                         value = "DUR", func = "mean"),
    # The summarize-table path: every row type in one heterogeneous table,
    # and the facet + pooled + field composition.
    summaries_mixed = list(by = "TERM", summaries = list(
      list(type = "simple", name = "Subjects", func = "count_distinct",
           col = "USUBJID", show = "bar"),
      list(type = "dist", name = "Duration", col = "DUR", show = "box"),
      list(type = "dist", name = "Mean", col = "DUR", stat = "mean_ci95",
           show = "text"),
      list(type = "field", name = "Arms", col = "ARM"),
      list(type = "spans", name = "Episodes", x = "SDY", xend = "EDY",
           color = "SEV"),
      list(type = "series", name = "Traj", x = "AVAL", col = "DUR",
           band = c("LO", "HI")),
      list(type = "expr", name = "CV", expr = "round(sd(DUR)/mean(DUR), 2)")
    )),
    summaries_facet = list(by = "TERM", facet = "ARM", summaries = list(
      list(type = "simple", name = "Subjects", func = "count_distinct",
           col = "USUBJID", show = "bar"),
      list(type = "dist", name = "Overall", col = "DUR", stat = "mean_se",
           show = "text", scope = "pooled"),
      list(type = "field", name = "Arms", col = "ARM")
    ))
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
