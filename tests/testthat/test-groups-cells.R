# Summarize table: the cell model of the groups layout (groups_cells())
# and what the view makes of it. The cell model is what table_groups_to_model()
# turns into marks; the rules below pin its numbers, labels and flags.

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

# One bar column, as the gear writes it.
push_bar <- function(func = "count", col = NULL, ...) {
  c(list(type = "simple", func = func), if (!is.null(col)) list(col = col),
    list(show = "bar"), list(...))
}

test_that("a flat table has a row per group and a bar with its own label", {
  ae <- push_fixture()
  m <- sum_model(
    ae, by = "TERM", summaries = list(push_bar("count_distinct", "USUBJID")),
    drill = "TERM"
  )
  cells <- m$cells
  expect_identical(cells$n, 4L)
  expect_true(cells$pick)
  expect_length(cells$label, 4L)
  # A flat table has no levels, no parent rows and (unfiltered) no active row.
  expect_false(cells$nested)
  expect_false(any(cells$level > 0L))
  expect_false(any(cells$parent_row))
  expect_false(any(cells$on))
  # One plan column by default: the bar, which carries its own value label
  # (disp + pct + the column's ONE label-slot width).
  expect_identical(vapply(cells$cols, function(c) c$kind, ""), "bar")
  c1 <- cells$cols[[1]]
  expect_length(c1$w, 4L)
  expect_length(c1$disp, 4L)
  expect_length(c1$pct, 4L)
  expect_true(is.numeric(c1$dw))
  # The view: the stub then the bar, one claim per row with the drill on.
  v <- table_groups_to_model(m)
  expect_identical(v$n, 4L)
  expect_length(v$cols, 2L)
  expect_identical(v$cols[[2]]$mark$kind, "bar")
  expect_length(v$cols[[2]]$mark$w, 4L)
  expect_length(v$rows$keys, 4L)
})

test_that("a nested table has levels, parent rows and parent labels", {
  ae <- push_fixture()
  m <- sum_model(ae, by = c("SOC", "TERM"), summaries = list(push_bar()))
  cells <- m$cells
  expect_true(cells$nested)
  expect_true(any(as.integer(cells$level) > 0L))
  expect_true(any(as.logical(cells$parent_row)))
  expect_true("SOC A" %in% as.character(cells$parent))
  # The view indents the child rows and marks the parent rows strong.
  rows <- table_groups_to_model(m)$rows
  expect_identical(as.integer(rows$indent), as.integer(cells$level))
  expect_identical(as.logical(rows$strong), as.logical(cells$parent_row))
})

test_that("a split column has one width vector per series", {
  ae <- push_fixture()
  c1 <- sum_model(ae, by = "TERM", summaries = list(
    push_bar(color = "SEV", position = "fill")
  ))$cells$cols[[1]]
  expect_identical(c1$kind, "barsplit")
  expect_identical(c1$mode, "percent")
  expect_identical(as.character(c1$names), c("MILD", "MODERATE"))
  expect_length(c1$seg, 2L)
  # percent mode: the two segments fill each row.
  sums <- c1$seg[[1]] + c1$seg[[2]]
  expect_true(all(abs(sums - 100) < 0.02))
})

test_that("the degenerate collapse leaves widths and the domain alone", {
  ae <- push_fixture()
  cfg <- list(type = "simple", func = "max", col = "AVAL", show = "bar")
  plain <- sum_model(ae, by = "USUBJID", summaries = list(cfg))$cells
  split <- sum_model(ae, by = "USUBJID", summaries = list(
    c(cfg, list(color = "COHORT"))
  ))$cells
  # One level per row means the segment IS the bar: same numbers, same axis,
  # only the colour differs from the uncoloured column.
  expect_equal(as.numeric(split$cols[[1]]$v), as.numeric(plain$cols[[1]]$v))
  seg_w <- Reduce(`+`, lapply(split$cols[[1]]$seg, as.numeric))
  expect_equal(seg_w, as.numeric(plain$cols[[1]]$w), tolerance = 1e-8)
})

# --- the lane marks ----------------------------------------------------------

test_that("a box column has pre-rounded positions AND widths", {
  ae <- push_fixture()
  c1 <- sum_model(ae, by = "TERM", summaries = list(
    list(type = "dist", col = "DUR", show = "box")
  ))$cells$cols[[1]]
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
  # Widths are given, never re-derived: left whisker ends where the box
  # starts (within the 2dp rounding the model carries).
  expect_true(all(abs(as.numeric(c1$wl) + as.numeric(c1$w1) -
                        as.numeric(c1$bl)) <= 0.02))
  # The tooltip carries the statistic's own words.
  expect_match(c1$tip[[1]], "Median .* Q1–Q3 .* 1\\.5×IQR")
})

test_that("an interval column has per-row segments on the observed domain", {
  ae <- push_fixture()
  cells <- sum_model(ae, by = "USUBJID", summaries = list(
    list(type = "spans", x = "SDY", xend = "EDY", color = "SEV"),
    list(type = "simple", name = "Events", func = "count", show = "number")
  ))$cells
  c1 <- cells$cols[[1]]
  expect_identical(c1$kind, "interval")
  expect_length(c1$segs, cells$n)
  expect_length(c1$tips, cells$n)
  # Subjects appearing in all four terms carry four spans.
  expect_true(any(lengths(c1$segs) > 1L))
  # Fill index points into the level fills; tips carry the level name.
  expect_identical(as.character(c1$fills),
                   unname(groups_level_colors(NULL, "SEV",
                                                 c("MILD", "MODERATE"))))
  expect_match(c1$tips[[1]][[1]], "^SEV (MILD|MODERATE) · ")
  # Nonnegative timelines use a readable study-day scale.
  expect_equal(c1$d0, 0)
  expect_equal(c1$d1, 60)
  # An Events count column rides beside the lane.
  expect_identical(cells$cols[[2]]$kind, "num")
})

test_that("a sparkline column has pre-printed geometry plus hover values", {
  ae <- push_fixture()
  cells <- sum_model(ae, by = "TERM", summaries = list(
    list(type = "sparkline", x = "AVAL", col = "DUR", ymin = "LO", ymax = "HI")
  ))$cells
  c1 <- cells$cols[[1]]
  expect_identical(c1$kind, "sparkline")
  expect_length(c1$pl, 4L)
  expect_match(c1$pl[[1]], "^[0-9.,]+( [0-9.,]+)+$")
  expect_false(anyNA(c1$bd))          # the band columns are complete
  expect_match(c1$xs[[1]], ",")       # hover snap data
  # Sort value = the last y per row.
  expect_equal(as.numeric(c1$v),
               round(unname(vapply(split(ae, ae$TERM), function(d) {
                 d$DUR[order(d$AVAL)][nrow(d)]
               }, numeric(1))[cells$label]), 4))
})

test_that("a leading rank bar beside a trajectory ranks the rows", {
  ae <- push_fixture()
  cells <- sum_model(ae, by = "TERM", summaries = list(
    list(type = "simple", func = "mean", col = "DUR", show = "bar"),
    list(type = "sparkline", x = "AVAL", col = "DUR")
  ))$cells
  expect_identical(vapply(cells$cols, function(c) c$kind, ""),
                   c("bar", "sparkline"))
  # The bar ranks the rows: row order = mean(DUR) per term, descending.
  means <- vapply(split(ae$DUR, ae$TERM), mean, numeric(1))
  expect_identical(as.character(cells$label),
                   names(sort(means, decreasing = TRUE)))
  # The sparkline column itself still sorts by LAST value, not the mean.
  expect_false(identical(as.numeric(cells$cols[[2]]$v),
                         as.numeric(cells$cols[[1]]$v)))
})

test_that("negative lows extend the distribution domain below zero", {
  d <- data.frame(g = rep(c("a", "b"), each = 6),
                  v = c(-5, -2, 0, 1, 2, 3, 1, 2, 3, 4, 5, 6))
  prep <- groups_prepare(d, by = "g", summaries = list(
    list(type = "dist", col = "v", show = "box")
  ))
  # The domain rides on the plan entry now (per-summary scales).
  expect_lt(prep$plan[[1]]$dmin, 0)
  m <- groups_cells(prep)
  c1 <- m$cols[[1]]
  # Everything still renders inside the track.
  expect_true(all(as.numeric(c1$wl) >= 0, na.rm = TRUE))
  expect_true(all(as.numeric(c1$wh) <= 100, na.rm = TRUE))
})

# --- the drift guard --------------------------------------------------------

test_that("a signed simple column becomes a zero-centred diverging bar", {
  mk <- function(vals) {
    d <- data.frame(g = paste0("r", seq_along(vals)), v = vals)
    sum_model(d, by = "g", summaries = list(
      list(type = "simple", func = "median", col = "v", show = "bar")
    ))$cells$cols[[1]]
  }
  # No negatives: the plain bar is untouched, length from a zero baseline.
  c1 <- mk(c(30, 20, 10))
  expect_identical(c1$kind, "bar")
  # Widths are rounded to 2dp so both consumers print the same string.
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
  prep <- groups_prepare(d, by = "g", summaries = list(
    list(type = "simple", func = "median", col = "v", show = "bar")
  ))
  p <- prep$plan[[1]]
  expect_identical(p$kind, "bardiv")
  # The axis prints the same numbers the geometry is scaled with.
  expect_equal(p$dmax, 30)
  expect_equal(p$dmin, -30)
  dom <- cells_axis_domain(p, prep)
  expect_equal(dom$d0, -30)
  expect_equal(dom$d1, 30)
})

test_that("a pair column has both ends, the band, the ref and the flags", {
  ae <- push_fixture()
  prep <- groups_prepare(ae, by = "USUBJID", summaries = list(
    list(type = "pair", x = "AVAL", x_func = "min", xend = "DUR",
         xend_func = "max", ref_min = 5, x_lines = 10, linetype = "SEV")
  ))
  expect_null(prep$err)
  p <- prep$plan[[1]]
  expect_identical(p$kind, "pair")
  expect_match(p$sub_label, "dashed: SEV")
  c1 <- groups_cells(prep)$cols[[1]]
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
  prep <- groups_prepare(ae, by = "TERM", summaries = list(
    list(type = "pair", x = "SDY", x_func = "mean", xend = "EDY",
         xend_func = "mean")
  ))
  d <- groups_cells(prep)$cols[[1]]$disp
  d <- d[nzchar(d)]
  expect_true(all(grepl("^[+-]", d)))
})

test_that("a coloured pair of one level per row labels that level", {
  ae <- push_fixture()
  ae <- ae[!duplicated(ae$USUBJID), ]
  prep <- groups_prepare(ae, by = "USUBJID", summaries = list(
    list(type = "pair", x = "AVAL", xend = "DUR", color = "ARM")
  ))
  c1 <- groups_cells(prep)$cols[[1]]
  expect_true(isTRUE(c1$multi))
  labs <- Reduce(function(x, g) ifelse(nzchar(g$disp), g$disp, x),
                 c1$lv, rep("", length(c1$v)))
  expect_true(all(nzchar(labs)))
})

test_that("value labels print one decimal finer than the column axis", {
  # Ticks every 10 (0..50): one decimal.
  expect_identical(cells_val_digits(c(17.6667, 4.33), 0, 50), 1L)
  expect_identical(cells_val_str(c(17.6667, NA), 1L), c("17.7", ""))
  # Ticks every 0.5: two.
  expect_identical(cells_val_digits(c(1.234, 2.5), 0, 2), 2L)
  # Whole numbers stay whole, whatever the axis.
  expect_identical(cells_val_digits(c(3, 12, 40), 0, 1), 0L)
  # No axis: groups_fmt's significant digits.
  expect_true(is.na(cells_val_digits(c(1.23456), NULL, NULL)))
  expect_identical(cells_val_str(1.23456, NA_integer_), groups_fmt(1.23456))
})

test_that("a pair row names a missing column", {
  ae <- push_fixture()
  prep <- groups_prepare(ae, by = "USUBJID", summaries = list(
    list(type = "pair", x = "AVAL", xend = "NOPE")
  ))
  expect_match(prep$err, "NOPE")
})

test_that("a pair column's dash levels come from the full data, not the facet", {
  ae <- push_fixture()
  # ARM = Placebo rows are all MILD, so the Placebo copy's own levels would
  # be MILD alone and MODERATE rows elsewhere would be numbered wrongly.
  ae$SEV[ae$ARM == "Placebo"] <- "MILD"
  prep <- groups_prepare(ae, by = "USUBJID", summaries = list(
    list(type = "pair", x = "AVAL", xend = "DUR", linetype = "SEV",
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

test_that("a nested pair's parent row is the mean of its rows' ends", {
  ae <- push_fixture()
  # Baseline to peak per subject, read at the arm: the mean of the subjects'
  # baselines to the mean of their peaks, not the first subject's baseline to
  # the arm's single highest value.
  prep <- groups_prepare(ae, by = c("ARM", "USUBJID"), summaries = list(
    list(type = "pair", x = "AVAL", xend = "DUR", ref_min = "LO", ref_max = "HI")
  ))
  expect_null(prep$err)
  p <- prep$plan[[1]]
  rows <- prep$rows
  par <- rows[rows$.is_parent, , drop = FALSE]
  expect_setequal(par$.label, c("Placebo", "Active"))
  for (k in seq_len(nrow(par))) {
    arm <- par$.label[[k]]
    d <- ae[ae$ARM == arm, , drop = FALSE]
    subj <- unique(d$USUBJID)
    base <- vapply(subj, function(u) d$AVAL[d$USUBJID == u][[1]], numeric(1))
    peak <- vapply(subj, function(u) max(d$DUR[d$USUBJID == u]), numeric(1))
    lo <- vapply(subj, function(u) mean(d$LO[d$USUBJID == u]), numeric(1))
    expect_equal(par[[p$cols[["a"]]]][[k]], mean(base), info = arm)
    expect_equal(par[[p$cols[["b"]]]][[k]], mean(peak), info = arm)
    expect_equal(par[[p$cols[["lo"]]]][[k]], mean(lo), info = arm)
    expect_identical(par[[p$ncol]][[k]], length(subj), info = arm)
  }
  # The leaf rows keep the row's own functions and carry no count.
  leaf <- rows[!rows$.is_parent, , drop = FALSE]
  s1 <- leaf[leaf$.parent == "Placebo" & leaf$.label == "S001", , drop = FALSE]
  d1 <- ae[ae$ARM == "Placebo" & ae$USUBJID == "S001", , drop = FALSE]
  expect_equal(s1[[p$cols[["a"]]]], d1$AVAL[[1]])
  expect_equal(s1[[p$cols[["b"]]]], max(d1$DUR))
  expect_true(all(is.na(leaf[[p$ncol]])))

  # The card says what the parent's ends are.
  c1 <- groups_cells(prep)$cols[[1]]
  n_par <- par[[p$ncol]]
  expect_match(c1$tip[rows$.is_parent], "mean of [0-9]+ rows$")
  expect_match(c1$tip[rows$.is_parent][[1]],
               paste0(" \u00b7 mean of ", n_par[[1]], " rows$"))
  expect_false(any(grepl("mean of", c1$tip[!rows$.is_parent])))
})

test_that("a colour-split nested pair averages each level's rows", {
  ae <- push_fixture()
  prep <- groups_prepare(ae, by = c("SOC", "TERM"), summaries = list(
    list(type = "pair", x = "SDY", x_func = "mean", xend = "EDY",
         xend_func = "max", color = "SEV")
  ))
  expect_null(prep$err)
  p <- prep$plan[[1]]
  rows <- prep$rows
  par <- rows[rows$.is_parent, , drop = FALSE]
  for (j in seq_along(p$levels)) {
    cn <- p$lcols[[j]]
    for (k in seq_len(nrow(par))) {
      d <- ae[ae$SOC == par$.label[[k]] & ae$SEV == p$levels[[j]], ,
              drop = FALSE]
      terms <- unique(d$TERM)
      to <- vapply(terms, function(t) max(d$EDY[d$TERM == t]), numeric(1))
      expect_equal(par[[cn[["b"]]]][[k]], mean(to))
      expect_identical(par[[p$lncol[[j]]]][[k]], length(terms))
    }
  }
  c1 <- groups_cells(prep)$cols[[1]]
  expect_true(isTRUE(c1$multi))
  expect_match(c1$lv[[1]]$tip[rows$.is_parent], "mean of [0-9]+ rows$")
})
