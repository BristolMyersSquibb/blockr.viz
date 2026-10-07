# Summarize table: the data-push cell model, and the R/JS drift guard.
#
# The payload exists so the body ships as ~40 KB of numbers instead of ~780 KB
# of HTML, which only works if summarize-table.js assembles exactly the markup
# summarize_cells_html() pastes. The last test in this file runs the REAL JS
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
  # A SIGNED measure (a change from baseline), so the diverging bar has a
  # fixture: the group means straddle zero (T1 positive, T2-T4 negative).
  rows$CHG <- rows$AVAL - 10L
  # A subject-level FACT: constant within USUBJID, unlike SEV and ARM which
  # both vary within a subject here. Grouping by USUBJID and colouring by it
  # is the degenerate split -- one level per row, nothing to lay side by side.
  rows$COHORT <- factor(
    ifelse(as.integer(sub("^S", "", rows$USUBJID)) %% 2L == 0L, "C1", "C2"),
    levels = c("C1", "C2")
  )
  rows
}

test_that("the flat payload carries the head and the per-column vectors", {
  ae <- push_fixture()
  p <- summarize_build_payload(ae, group = "TERM", func = "count_distinct",
                               id_var = "USUBJID", drill = "TERM")

  expect_identical(p$kind, "flat")
  expect_identical(p$n, 4L)
  expect_true(p$pick)
  # head is the <table> with its data attributes and an EMPTY tbody: the gear
  # keeps reading its state off those attributes.
  expect_match(p$head, "<tbody></tbody></table>$")
  expect_match(p$head, "blockr-summarize-table")
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
  p <- summarize_build_payload(ae, group = "TERM", func = "count")
  expect_true("T <1> & co" %in% p$label)     # raw in the payload
  html <- as.character(htmltools::renderTags(
    summarize_table(ae, group = "TERM", func = "count")
  )$html)
  expect_match(html, "T &lt;1&gt; &amp; co", fixed = TRUE)
})

test_that("nested tables DO ship the level and parent vectors", {
  ae <- push_fixture()
  p <- summarize_build_payload(ae, group = "TERM", parent = "SOC",
                               func = "count")
  expect_true(any(as.integer(p$level) > 0L))
  expect_true(any(as.logical(p$parent_row)))
  expect_true("SOC A" %in% as.character(p$parent))
})

test_that("a split column ships one width vector per series", {
  ae <- push_fixture()
  p <- summarize_build_payload(ae, group = "TERM", color = "SEV",
                               func = "count", bar_mode = "percent")
  c1 <- p$cols[[1]]
  expect_identical(c1$kind, "barsplit")
  expect_identical(c1$mode, "percent")
  expect_identical(as.character(c1$names), c("MILD", "MODERATE"))
  expect_length(c1$seg, 2L)
  # percent mode: the two segments fill each row.
  sums <- c1$seg[[1]] + c1$seg[[2]]
  expect_true(all(abs(sums - 100) < 0.02))
})

test_that("a grouped split with one level per row is stacked instead", {
  ae <- push_fixture()
  # COHORT is constant within USUBJID, and `max` is non-additive, so the plan
  # asks for side-by-side lanes -- with nothing to put beside anything. Every
  # row would draw one bar and one EMPTY lane, at twice the height.
  deg <- summarize_build_payload(ae, by = "USUBJID", summaries = list(
    list(type = "simple", func = "max", col = "AVAL", show = "bar",
         color = "COHORT")
  ))
  c1 <- deg$cols[[1]]
  expect_identical(c1$kind, "barsplit")
  expect_identical(c1$mode, "stacked")
  # Stacked omits an empty segment, so the cell is one bar in the row's own
  # colour -- and the colours are still the level colours, not a flat fill.
  html <- summarize_cells_html(summarize_cells(summarize_prepare(
    ae, by = "USUBJID", summaries = list(
      list(type = "simple", func = "max", col = "AVAL", show = "bar",
           color = "COHORT")
    )
  )))
  row1 <- regmatches(html,
                     regexpr("<tr class=\"blockr-summarize-row.*?</tr>", html))
  expect_length(gregexpr("blockr-summarize-fill", row1)[[1]], 1L)
  expect_identical(gregexpr("width:0%", row1)[[1]][[1]], -1L)
  expect_identical(gregexpr("is-tall", row1)[[1]][[1]], -1L)

  # A REAL split is untouched: SEV varies within a term, so the lanes carry
  # a comparison and grouped stays grouped.
  keep <- summarize_build_payload(ae, by = "TERM", summaries = list(
    list(type = "simple", func = "max", col = "AVAL", show = "bar",
         color = "SEV")
  ))
  expect_identical(keep$cols[[1]]$mode, "grouped")
})

test_that("the degenerate collapse leaves widths and the domain alone", {
  ae <- push_fixture()
  cfg <- list(type = "simple", func = "max", col = "AVAL", show = "bar")
  plain <- summarize_build_payload(ae, by = "USUBJID", summaries = list(cfg))
  split <- summarize_build_payload(ae, by = "USUBJID", summaries = list(
    c(cfg, list(color = "COHORT"))
  ))
  # One level per row means the segment IS the bar: same numbers, same axis,
  # only the colour differs from the uncoloured column.
  expect_equal(as.numeric(split$cols[[1]]$v), as.numeric(plain$cols[[1]]$v))
  seg_w <- Reduce(`+`, lapply(split$cols[[1]]$seg, as.numeric))
  expect_equal(seg_w, as.numeric(plain$cols[[1]]$w), tolerance = 1e-8)
})

test_that("a non-renderable state ships as kind html, not a cell model", {
  ae <- push_fixture()
  p <- summarize_build_payload(ae, group = NULL)
  expect_identical(p$kind, "html")
  expect_match(p$html, "Pick a Group column")
  # The chrome still travels: the footer and legend slots get cleared.
  expect_true(!is.null(p$chrome))
})

test_that("the chrome rides on the payload so the container is never rebuilt", {
  ae <- push_fixture()
  p <- summarize_build_payload(
    ae, chrome = list(title = "Ranked", subtitle = "N = 30", caption = "src"),
    group = "TERM", facet = "ARM", color = "SEV", func = "count_distinct",
    id_var = "USUBJID"
  )
  expect_identical(p$chrome$title, "Ranked")
  expect_identical(p$chrome$caption, "src")
  # The legend maps the COLOUR levels; a plain facet carries none (its
  # column headers already name the levels).
  expect_identical(p$chrome$legend$groups[[1L]]$title, "SEV")
  expect_length(p$chrome$legend$groups[[1L]]$items, 2L)
  # No row count in the footer: the fold row speaks for a Top N cut.
  expect_null(p$chrome$foot$count)
  plain <- summarize_build_payload(ae, group = "TERM", facet = "ARM",
                                   func = "count_distinct", id_var = "USUBJID")
  expect_null(plain$chrome$legend)
})

test_that("the payload is smaller than the markup it replaces", {
  ae <- push_fixture()
  args <- list(group = "TERM", facet = "ARM", func = "count_distinct",
               id_var = "USUBJID")
  json <- summarize_payload_json(
    do.call(summarize_build_payload, c(list(ae), args))
  )
  html <- as.character(htmltools::renderTags(
    do.call(summarize_table, c(list(ae), args))
  )$html)
  expect_lt(nchar(json, type = "bytes"), nchar(html, type = "bytes"))
})

test_that("json keeps single-row columns as arrays", {
  ae <- push_fixture()
  one <- ae[ae$TERM == "T3", , drop = FALSE]
  p <- summarize_build_payload(one, group = "TERM", func = "count")
  j <- summarize_payload_json(p)
  # A length-1 column must stay [x], never x: the JS assembler indexes it.
  expect_match(j, '"label":\\[')
  expect_match(j, '"w":\\[')
})

# --- the lane marks ----------------------------------------------------------

test_that("a box column ships pre-rounded positions AND widths", {
  ae <- push_fixture()
  p <- summarize_build_payload(ae, group = NULL, by = "TERM", summaries = list(
    list(type = "dist", col = "DUR", show = "box")
  ))
  c1 <- p$cols[[1]]
  expect_identical(c1$kind, "box")
  for (nm in c("wl", "w1", "bl", "bw", "bc", "b2", "w2", "wh", "nn", "tip")) {
    expect_length(c1[[nm]], 4L)
  }
  # The domain runs to the widest WHISKER (never to `n`, which the column
  # also carries for the tooltip), plus a hair of padding so the extreme
  # glyph is not flush against the cell edge -- so the widest whisker lands
  # just inside the track and nothing ever renders past it.
  expect_lt(max(as.numeric(c1$wh)), 100)
  expect_gt(max(as.numeric(c1$wh)), 90)
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
  p <- summarize_build_payload(one, group = NULL, by = "TERM", summaries = list(
    list(type = "dist", col = "DUR", stat = "mean_ci95",
         show = "pointrange")
  ))
  c1 <- p$cols[[1]]
  expect_identical(c1$kind, "pointrange")
  expect_true(all(is.na(as.numeric(c1$rw))))
  expect_false(anyNA(as.numeric(c1$c)))
  # Tips ship pre-escaped (both consumers paste them into an attribute).
  expect_match(c1$tip[[1]], "undefined (n &lt; 2)", fixed = TRUE)
  # And the emitter draws the dot alone.
  html <- summarize_cells_html(summarize_cells(summarize_prepare(
    one, group = NULL, by = "TERM", summaries = list(
      list(type = "dist", col = "DUR", stat = "mean_ci95",
           show = "pointrange")
    )
  )))
  expect_match(html, "lane-ctr")
  expect_false(grepl("lane-rng", html))
})

test_that("an interval column ships per-row segments on the observed domain", {
  ae <- push_fixture()
  p <- summarize_build_payload(ae, group = NULL, by = "USUBJID",
                               summaries = list(
    list(type = "spans", x = "SDY", xend = "EDY", color = "SEV"),
    list(type = "simple", name = "Events", func = "count", show = "number")
  ))
  c1 <- p$cols[[1]]
  expect_identical(c1$kind, "interval")
  expect_length(c1$segs, p$n)
  expect_length(c1$tips, p$n)
  # Subjects appearing in all four terms carry four spans.
  expect_true(any(lengths(c1$segs) > 1L))
  # Fill index points into the level fills; tips carry the level name.
  expect_identical(as.character(c1$fills),
                   unname(summarize_level_colors(NULL, "SEV",
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
  p <- summarize_build_payload(ae, group = NULL, by = "TERM", summaries = list(
    list(type = "series", x = "AVAL", col = "DUR", band = c("LO", "HI"))
  ))
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

test_that("a leading rank bar beside a trajectory ranks the rows", {
  ae <- push_fixture()
  p <- summarize_build_payload(ae, group = NULL, by = "TERM", summaries = list(
    list(type = "simple", func = "mean", col = "DUR", show = "bar"),
    list(type = "series", x = "AVAL", col = "DUR")
  ))
  expect_identical(vapply(p$cols, function(c) c$kind, ""),
                   c("bar", "sparkline"))
  # The bar ranks the rows: row order = mean(DUR) per term, descending.
  means <- vapply(split(ae$DUR, ae$TERM), mean, numeric(1))
  expect_identical(as.character(p$label),
                   names(sort(means, decreasing = TRUE)))
  # The sparkline column itself still sorts by LAST value, not the mean.
  expect_false(identical(as.numeric(p$cols[[2]]$v),
                         as.numeric(p$cols[[1]]$v)))
})

test_that("negative lows extend the distribution domain below zero", {
  d <- data.frame(g = rep(c("a", "b"), each = 6),
                  v = c(-5, -2, 0, 1, 2, 3, 1, 2, 3, 4, 5, 6))
  prep <- summarize_prepare(d, group = NULL, by = "g", summaries = list(
    list(type = "dist", col = "v", show = "box")
  ))
  # The domain rides on the plan entry now (per-summary scales).
  expect_lt(prep$plan[[1]]$dmin, 0)
  m <- summarize_cells(prep)
  c1 <- m$cols[[1]]
  # Everything still renders inside the track.
  expect_true(all(as.numeric(c1$wl) >= 0, na.rm = TRUE))
  expect_true(all(as.numeric(c1$wh) <= 100, na.rm = TRUE))
})

# --- the drift guard --------------------------------------------------------

test_that("a signed simple column becomes a zero-centred diverging bar", {
  mk <- function(vals) {
    d <- data.frame(g = paste0("r", seq_along(vals)), v = vals)
    summarize_build_payload(d, group = NULL, by = "g", summaries = list(
      list(type = "simple", func = "median", col = "v", show = "bar")
    ))$cols[[1]]
  }
  # No negatives: the plain bar is untouched, length from a zero baseline.
  c1 <- mk(c(30, 20, 10))
  expect_identical(c1$kind, "bar")
  # Widths ship rounded to 2dp so both consumers print the same string.
  expect_equal(as.numeric(c1$w), c(100, 66.67, 33.33))
  expect_null(c1$pos)

  # All negative: every bar used to be EMPTY (abs() over a max of -5).
  c2 <- mk(c(-5, -10, -20))
  expect_identical(c2$kind, "bardiv")
  expect_equal(as.numeric(c2$w), c(12.5, 25, 50))
  expect_false(any(c2$pos))

  # Mixed: -30 used to draw the same full width as the +10 maximum. It is now
  # half the track wide (the maximum magnitude) and on the other side of zero.
  c3 <- mk(c(10, 0, -30))
  expect_identical(c3$kind, "bardiv")
  expect_equal(as.numeric(c3$w), c(16.67, 0, 50))
  expect_identical(as.logical(c3$pos), c(TRUE, TRUE, FALSE))

  # Equal magnitudes, opposite signs: same length, opposite polarity.
  c4 <- mk(c(5, -5))
  expect_equal(as.numeric(c4$w), c(50, 50))
  expect_identical(as.logical(c4$pos), c(TRUE, FALSE))

  # An all-zero column has no sign to show and stays a plain bar.
  expect_identical(mk(c(0, 0, 0))$kind, "bar")
})

test_that("the diverging bar's axis is centred on zero at the column max", {
  d <- data.frame(g = c("a", "b", "c"), v = c(10, 0, -30))
  prep <- summarize_prepare(d, group = NULL, by = "g", summaries = list(
    list(type = "simple", func = "median", col = "v", show = "bar")
  ))
  p <- prep$plan[[1]]
  expect_identical(p$kind, "bardiv")
  # The axis prints the same numbers the geometry is scaled with.
  expect_equal(p$dmax, 30)
  expect_equal(p$dmin, -30)
  dom <- summarize_axis_domain(p, prep)
  expect_equal(dom$d0, -30)
  expect_equal(dom$d1, 30)
})

test_that("the JS assembles byte-identical markup to summarize_cells_html", {
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
    facet_color = list(group = "TERM", facet = "ARM", color = "SEV",
                       func = "count"),
    identity = list(group = "USUBJID", func = "identity", value = "AVAL",
                    fields = c("SOC", "AVAL")),
    sep_cols = list(group = "TERM", func = "count", cols = c("n", "pct")),
    nested = list(group = "TERM", parent = "SOC", func = "count"),
    capped = list(group = "TERM", func = "count", top_n = 2L),
    # One case per glyph, so every emitter is byte-compared too.
    box = list(by = "TERM", summaries = list(
      list(type = "dist", col = "DUR", show = "box")
    )),
    box_facet = list(by = "TERM", facet = "ARM", summaries = list(
      list(type = "dist", col = "DUR", show = "box")
    )),
    box_nested = list(by = c("SOC", "TERM"), summaries = list(
      list(type = "dist", col = "DUR", show = "box")
    )),
    # The colour dimension: one glyph per level inside the cell. Both
    # assemblers must skip an absent level identically, or the lanes drift.
    box_color = list(by = "TERM", summaries = list(
      list(type = "dist", col = "DUR", show = "box", color = "SEV")
    )),
    pointrange_color = list(by = "TERM", summaries = list(
      list(type = "dist", col = "DUR", stat = "mean_ci95",
           show = "pointrange", color = "ARM")
    )),
    pointrange = list(by = "TERM", summaries = list(
      list(type = "dist", col = "DUR", stat = "mean_ci95",
           show = "pointrange")
    )),
    pr_n1 = list(by = "USUBJID", summaries = list(
      list(type = "dist", col = "DUR", stat = "mean_ci95",
           show = "pointrange")
    )),
    dot = list(by = "TERM", summaries = list(
      list(type = "simple", func = "mean", col = "DUR", show = "dot")
    )),
    # A signed measure: the bar column becomes the diverging bar, so both
    # assemblers must agree about polarity as well as width.
    bardiv_signed = list(by = "TERM", summaries = list(
      list(type = "simple", func = "mean", col = "CHG", show = "bar")
    )),
    # A grouped split with one level per row, collapsed to stacked: both
    # assemblers have to DROP the empty segments, or one draws lanes the
    # other does not.
    barsplit_grouped_vals = list(by = "TERM", summaries = list(
      list(type = "simple", func = "mean", col = "DUR", show = "bar",
           color = "SEV")
    )),
    barsplit_degenerate = list(by = "USUBJID", summaries = list(
      list(type = "simple", func = "max", col = "AVAL", show = "bar",
           color = "COHORT")
    )),
    interval = list(by = "USUBJID", summaries = list(
      list(type = "spans", x = "SDY", xend = "EDY", color = "SEV"),
      list(type = "simple", name = "Events", func = "count",
           show = "number")
    )),
    sparkline = list(by = "TERM", summaries = list(
      list(type = "series", x = "AVAL", col = "DUR", band = c("LO", "HI"))
    )),
    sparkline_bar = list(by = "TERM", summaries = list(
      list(type = "simple", func = "mean", col = "DUR", show = "bar"),
      list(type = "series", x = "AVAL", col = "DUR")
    )),
    sparkline_ref = list(by = "TERM", summaries = list(
      list(type = "series", x = "AVAL", col = "DUR", ref = "mean_sd")
    )),
    interval_rich = list(by = "USUBJID", summaries = list(
      list(type = "spans", x = "SDY", xend = "EDY", color = "SEV",
           label = "TERM", fields = "ARM", size = "lg")
    )),
    # The pair (dumbbell): band, reference, colour, dash and an open end.
    pair = list(by = "USUBJID", summaries = list(
      list(type = "pair", from = "AVAL", from_func = "min", to = "DUR",
           to_func = "max", lo = 5, hi = "HI", ref = 10, dash = "SEV",
           color = "ARM")
    )),
    pair_plain = list(by = c("SOC", "TERM"), summaries = list(
      list(type = "pair", from = "CHG", to = "AVAL", to_func = "mean")
    )),
    # The colour split: one dumbbell per level, repeated per arm.
    pair_split = list(by = "TERM", summaries = list(
      list(type = "pair", from = "SDY", from_func = "mean", to = "EDY",
           to_func = "mean", ref = 20, color = "SEV", facet = "ARM")
    )),
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

  js_path <- system.file("js", "summarize-table.js", package = "blockr.viz")
  skip_if(!nzchar(js_path) || !file.exists(js_path),
          "summarize-table.js not found")

  sess <- chromote::ChromoteSession$new()
  on.exit(try(sess$close(), silent = TRUE), add = TRUE)
  page <- tempfile(fileext = ".html")
  writeLines(c(
    "<!doctype html><html><body>",
    "<div class='blockr-summarize-container' data-summarize-elem-id='t1'>",
    "<div class='dd-table-titles'><div class='dd-table-title'></div>",
    "<div class='dd-table-subtitle'></div></div>",
    "<div class='blockr-summarize-legend'></div>",
    "<div class='blockr-table-wrapper'></div>",
    "<div class='dd-table-caption'></div>",
    "<div class='blockr-summarize-footer'>",
    "<span class='blockr-summarize-note'></span>",
    "<div class='dd-status-footer'></div></div></div>",
    "<script>window.Shiny={addCustomMessageHandler:function(n,f){",
    "window.__h=f}};</script>",
    paste0("<script src='file://", js_path, "'></script>"),
    "</body></html>"
  ), page)
  sess$Page$navigate(paste0("file://", page))
  Sys.sleep(1.5)

  for (nm in names(cases)) {
    args <- cases[[nm]]
    payload <- do.call(summarize_build_payload, c(list(ae), args))
    r_html <- as.character(do.call(summarize_table_html, list(
      do.call(summarize_prepare, c(list(ae), args))
    )))
    json <- summarize_payload_json(payload)
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

test_that("a pair column ships both ends, the band, the ref and the flags", {
  ae <- push_fixture()
  prep <- summarize_prepare(ae, by = "USUBJID", summaries = list(
    list(type = "pair", from = "AVAL", from_func = "min", to = "DUR",
         to_func = "max", lo = 5, ref = 10, dash = "SEV")
  ))
  expect_null(prep$err)
  p <- prep$plan[[1]]
  expect_identical(p$kind, "pair")
  expect_match(p$sub_label, "dashed: SEV")
  c1 <- summarize_cells(prep)$cols[[1]]
  expect_identical(c1$kind, "pair")
  rows <- prep$rows
  a <- rows[[p$cols[["a"]]]]
  b <- rows[[p$cols[["b"]]]]
  expect_true(all(b >= a))
  # A `to` below the band's low end is drawn open.
  expect_identical(c1$open, !is.na(b) & b < 5)
  expect_true(is.finite(c1$rf))
  # Every change is >= 0, so no "+": a column of durations reads "8.2".
  expect_match(c1$disp[[1]], "^[0-9]")
})

test_that("a pair column with a negative change signs every label", {
  ae <- push_fixture()
  t2 <- ae$TERM == "T2"
  ae$EDY[t2] <- ae$SDY[t2] - 50
  prep <- summarize_prepare(ae, by = "TERM", summaries = list(
    list(type = "pair", from = "SDY", from_func = "mean", to = "EDY",
         to_func = "mean")
  ))
  d <- summarize_cells(prep)$cols[[1]]$disp
  d <- d[nzchar(d)]
  expect_true(all(grepl("^[+-]", d)))
})

test_that("a coloured pair column draws one dumbbell per level", {
  ae <- push_fixture()
  prep <- summarize_prepare(ae, by = "TERM", summaries = list(
    list(type = "pair", from = "SDY", from_func = "mean", to = "EDY",
         to_func = "mean", color = "SEV")
  ))
  expect_null(prep$err)
  p <- prep$plan[[1]]
  expect_length(p$lcols, 2L)
  rows <- prep$rows
  # Each level's ends come from that level's rows only.
  for (j in 1:2) {
    lv <- c("MILD", "MODERATE")[[j]]
    want <- vapply(rows$TERM, function(t) {
      mean(ae$SDY[ae$TERM == t & ae$SEV == lv])
    }, numeric(1))
    expect_equal(rows[[p$lcols[[j]][["a"]]]], unname(want), info = lv)
  }
  # The pooled pair stays the sort value; the column domain takes in
  # every level's ends.
  ends <- unlist(lapply(c(list(p$cols), p$lcols), function(cn) {
    unlist(rows[cn[c("a", "b")]])
  }))
  expect_true(p$dmin <= min(ends) && p$dmax >= max(ends))
  c1 <- summarize_cells(prep)$cols[[1]]
  expect_true(isTRUE(c1$multi))
  expect_identical(c1$levels, c("MILD", "MODERATE"))
  expect_length(c1$lv, 2L)
  expect_true(all(c1$lv[[1]]$fill == c1$fills[[1]]))
  expect_match(c1$lv[[2]]$tip[[1]], "^MODERATE \u00b7 ")
  # No single number for the cell: each level prints its own change beside
  # its dumbbell, in the column's one slot width.
  expect_null(c1$disp)
  expect_true(c1$dw >= 1L)
  for (j in 1:2) {
    g <- c1$lv[[j]]
    drawn <- !is.na(g$a) & !is.na(g$b)
    expect_true(all(nzchar(g$disp[drawn])), info = j)
  }
  html <- summarize_cells_html(summarize_cells(prep))
  expect_match(html, "blockr-summarize-multi", fixed = TRUE)
  expect_match(html, "blockr-summarize-pacell", fixed = TRUE)
  expect_match(html, paste0("blockr-summarize-lv blockr-summarize-barwrap",
                            "[^>]*>.*?blockr-summarize-barval"))
})

test_that("a coloured pair of one level per row labels that level", {
  ae <- push_fixture()
  ae <- ae[!duplicated(ae$USUBJID), ]
  prep <- summarize_prepare(ae, by = "USUBJID", summaries = list(
    list(type = "pair", from = "AVAL", to = "DUR", color = "ARM")
  ))
  c1 <- summarize_cells(prep)$cols[[1]]
  expect_true(isTRUE(c1$multi))
  labs <- Reduce(function(x, g) ifelse(nzchar(g$disp), g$disp, x),
                 c1$lv, rep("", length(c1$v)))
  expect_true(all(nzchar(labs)))
})

test_that("value_labels = FALSE drops every value label", {
  ae <- push_fixture()
  prep <- summarize_prepare(ae, by = "TERM", summaries = list(
    list(type = "simple", func = "count", show = "bar"),
    list(type = "dist", col = "DUR", show = "box"),
    list(type = "pair", from = "SDY", from_func = "mean", to = "EDY",
         to_func = "mean", color = "SEV")
  ))
  on <- summarize_cells(prep)
  off <- summarize_cells(prep, cfg = list(value_labels = FALSE))
  expect_false(is.null(on$cols[[1]]$disp))
  for (c in off$cols) {
    expect_null(c$disp)
    expect_null(c$dw)
    for (g in c$lv) expect_null(g$disp)
  }
  html <- summarize_cells_html(off)
  expect_false(grepl("blockr-summarize-barval", html, fixed = TRUE))
  # The payload ships no label for the browser to draw either.
  pl <- summarize_flat_payload(off)
  expect_false(any(vapply(pl$cols, function(c) !is.null(c$dw), logical(1))))
})

test_that("a grouped split bar prints one value per level", {
  ae <- push_fixture()
  prep <- summarize_prepare(ae, by = "TERM", summaries = list(
    list(type = "simple", func = "mean", col = "DUR", show = "bar",
         color = "SEV")
  ))
  c1 <- summarize_cells(prep)$cols[[1]]
  expect_identical(c1$mode, "grouped")
  expect_null(c1$disp)
  expect_length(c1$ldisp, 2L)
  for (j in 1:2) {
    has <- c1$segv[[j]] > 0
    expect_true(all(nzchar(c1$ldisp[[j]][has])), info = j)
  }
  html <- summarize_cells_html(summarize_cells(prep))
  expect_match(html, "blockr-summarize-track is-lv", fixed = TRUE)
  # Off: back to the stacked rows with no number.
  off <- summarize_cells(prep, cfg = list(value_labels = FALSE))$cols[[1]]
  expect_null(off$ldisp)
  expect_null(off$disp)
})

test_that("value labels print one decimal finer than the column axis", {
  # Ticks every 10 (0..50): one decimal.
  expect_identical(summarize_val_digits(c(17.6667, 4.33), 0, 50), 1L)
  expect_identical(summarize_val_str(c(17.6667, NA), 1L), c("17.7", ""))
  # Ticks every 0.5: two.
  expect_identical(summarize_val_digits(c(1.234, 2.5), 0, 2), 2L)
  # Whole numbers stay whole, whatever the axis.
  expect_identical(summarize_val_digits(c(3, 12, 40), 0, 1), 0L)
  # No axis: lane_fmt's significant digits.
  expect_true(is.na(summarize_val_digits(c(1.23456), NULL, NULL)))
  expect_identical(summarize_val_str(1.23456, NA_integer_), lane_fmt(1.23456))
})

test_that("a pair row names a missing column", {
  ae <- push_fixture()
  prep <- summarize_prepare(ae, by = "USUBJID", summaries = list(
    list(type = "pair", from = "AVAL", to = "NOPE")
  ))
  expect_match(prep$err, "NOPE")
})

test_that("a pair column's dash levels come from the full data, not the facet", {
  ae <- push_fixture()
  # ARM = Placebo rows are all MILD, so the Placebo copy's own levels would
  # be MILD alone and MODERATE rows elsewhere would be numbered wrongly.
  ae$SEV[ae$ARM == "Placebo"] <- "MILD"
  prep <- summarize_prepare(ae, by = "USUBJID", summaries = list(
    list(type = "pair", from = "AVAL", to = "DUR", dash = "SEV",
         facet = "ARM")
  ))
  expect_null(prep$err)
  rows <- prep$rows
  for (p in prep$plan) {
    d <- rows[[sub("_to$", "_d", p$key)]]
    lv <- p$flevel
    sev <- vapply(rows$USUBJID, function(u) {
      x <- as.character(ae$SEV[ae$USUBJID == u & ae$ARM == lv])
      if (length(x)) x[[1]] else NA_character_
    }, character(1))
    expect_identical(d, match(sev, c("MILD", "MODERATE")), info = lv)
  }
})

test_that("a nested pair's parent row summarises its own rows", {
  ae <- push_fixture()
  prep <- summarize_prepare(ae, by = c("SOC", "TERM"), summaries = list(
    list(type = "pair", from = "SDY", from_func = "mean", to = "EDY",
         to_func = "mean")
  ))
  expect_null(prep$err)
  p <- prep$plan[[1]]
  rows <- prep$rows
  # Over the SOC's own rows, not the mean of its terms' means (SOC A has
  # terms of 16 and 12 rows, so the two differ).
  par <- rows[is.na(rows$.parent), , drop = FALSE]
  expect_identical(par$.label, c("SOC A", "SOC B"))
  for (k in seq_len(nrow(par))) {
    soc <- par$.label[[k]]
    expect_equal(par[[p$cols[["a"]]]][[k]], mean(ae$SDY[ae$SOC == soc]),
                 info = soc)
    expect_equal(par[[p$cols[["b"]]]][[k]], mean(ae$EDY[ae$SOC == soc]),
                 info = soc)
  }
})

test_that("a zero draws an empty track, a small value still shows", {
  # The fill's 2px floor is for small values; on a zero it read as a little.
  t <- summarize_track_html(c(0, 0.4, NA))
  expect_false(grepl("blockr-summarize-fill", t[[1]]))
  expect_match(t[[2]], "blockr-summarize-fill")
  expect_false(grepl("blockr-summarize-fill", t[[3]]))

  grouped <- summarize_split_html(list(
    v = c(3, 2), names = c("MILD", "SEVERE"), mode = "grouped",
    seg = list(c(60, 40), c(0, 10)), segv = list(c(3, 2), c(0, 1)),
    fills = c("#111111", "#222222")
  ))
  # Row 1: MILD drawn, SEVERE has no value there and gets no track.
  expect_identical(lengths(regmatches(grouped, gregexpr("blockr-summarize-row3",
                                                        grouped))), c(1L, 2L))
  expect_identical(lengths(regmatches(grouped, gregexpr("blockr-summarize-fill",
                                                        grouped))), c(1L, 2L))

  dv <- summarize_dv_html(c(0, 30), c(TRUE, FALSE))
  expect_false(grepl("blockr-summarize-fill", dv[[1]]))
  expect_match(dv[[2]], "is-neg")
})
