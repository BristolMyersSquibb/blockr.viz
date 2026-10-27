# Table model: cells of the lane marks, the box, the point range and the pair
# (dumbbell), each plain or colour-split. Every geometric number (positions
# AND widths) is rounded here; the emitters only print. See lane-prepare.R
# for the cell's statistics and _blockr.design/open/lane-chart/spec.md for the
# glyph.

# The distribution lanes. `b` is the per-entry scale from cells_bounds().
cells_lane <- function(ctx, p, b) {
  rows <- ctx$rows
  wd <- p$words %||% list(center = "Center", range = "Range")
  # Column-level, not row-level: TRUE when this column's mark draws NO
  # outer range, and so has nothing spanning the cell to be read
  # against. Those lanes keep a hairline; every other lane is bare,
  # because the whisker (or the dot style's fence band) IS the rail
  # (see LANE_DIST_STYLES). Derived from the leaf columns when the plan
  # does not say, so the lane chart's own marks follow the same rule.
  bare <- if (is.null(p$bare)) {
    !("wl" %in% names(p$cols %||% character()))
  } else {
    isTRUE(p$bare)
  }
  # The value label: the centre, at one precision for the whole column
  # (every colour level's centres together).
  lab_on <- ctx$vals_on && isTRUE(p$show_val)
  dig <- if (lab_on) {
    centres <- unlist(lapply(c(list(p$cols), p$lcols), function(cn) {
      if (!is.na(cn["bc"])) rows[[cn[["bc"]]]]
    }))
    summarize_val_digits(centres, p$dmin, p$dmax)
  }
  lane <- list(wd = wd, bare = bare, lab_on = lab_on, dig = dig)
  base <- cells_lane_glyph(ctx, p, b, lane, p$cols)
  if (is.null(p$lcols)) {
    base
  } else {
    # Colour split: one lane per level inside the cell, in level order,
    # sharing the column's scale. The pooled glyph still supplies the
    # sort value; its own lane is not drawn. Each level carries its own
    # value label beside its lane; the column keeps one slot width.
    lv <- lapply(seq_along(p$lcols), function(j) {
      g <- cells_lane_glyph(ctx, p, b, lane, p$lcols[[j]], p$levels[[j]])
      g$dw <- NULL
      g$kind <- NULL
      g$v <- NULL
      g
    })
    c(list(kind = p$kind, multi = TRUE, levels = as.character(p$levels),
           fills = as.character(p$fills), v = base$v, lv = lv),
      if (lab_on) list(dw = summarize_multi_dw(lv)))
  }
}

# A missing stat column (the single-value "dot": a pointrange with no
# interval) reads as all-NA, which the emitters draw as center-only.
cells_col_or_na <- function(rows, cn, nm) {
  if (!is.na(cn[nm]) && !is.null(rows[[cn[[nm]]]])) {
    rows[[cn[[nm]]]]
  } else {
    rep(NA_real_, nrow(rows))
  }
}

# One glyph from one set of stat columns. Called once for a plain
# column, once per level for a colour-split one (`p$lcols`), so both
# shapes ship exactly the same geometry per glyph.
cells_lane_glyph <- function(ctx, p, b, lane, cn, lvl = NULL) {
  rows <- ctx$rows
  bc <- cells_col_or_na(rows, cn, "bc")
  bl <- cells_col_or_na(rows, cn, "bl")
  bh <- cells_col_or_na(rows, cn, "bh")
  nn <- cells_col_or_na(rows, cn, "n")
  lab <- if (lane$lab_on) {
    disp <- summarize_val_str(bc, lane$dig)
    list(disp = disp, dw = max(c(1L, nchar(disp))))
  }
  # A split glyph's tooltip names its level first: colour is never the
  # only thing saying which level this is.
  pre <- if (is.null(lvl)) "" else paste0(lvl, " · ")
  wl <- cells_col_or_na(rows, cn, "wl")
  wh <- cells_col_or_na(rows, cn, "wh")
  tip <- cells_lane_tip(pre, lane$wd, nn, bc, bl, bh, wl, wh)
  r <- cells_lane_stats(list(bc = bc, bl = bl, bh = bh, wl = wl, wh = wh))
  pos_w <- b$pos
  span_w <- b$span
  if (identical(p$kind, "box")) {
    # Whisker segments live OUTSIDE the body; a degenerate side (whisker
    # meets the box) ships NA and draws nothing.
    list(kind = "box", bare = lane$bare,
         wl = pos_w(wl), w1 = span_w(wl, bl),
         bl = pos_w(bl), bw = span_w(bl, bh), bc = pos_w(bc),
         b2 = pos_w(bh), w2 = span_w(bh, wh), wh = pos_w(wh),
         nn = nn, tip = tip, v = cells_sortv(bc),
         r = r) |> c(lab)
  } else {
    # The dot style: the outer range as a fence band BEHIND the inner
    # bar (ol/ow), the inner range as the bar (l/rw), the centre as the
    # dot. Any of the three may be absent.
    list(kind = "pointrange", bare = lane$bare,
         ol = pos_w(wl), ow = span_w(wl, wh),
         c = pos_w(bc), l = pos_w(bl), rw = span_w(bl, bh),
         nn = nn, tip = tip, v = cells_sortv(bc),
         r = r) |> c(lab)
  }
}

# One tooltip for both styles: the pieces that are drawn, in the order
# they are drawn. A piece that is off is absent, never named as
# "none"; NA bounds (the n < 2 CI) say so rather than showing a
# zero-width interval, which would read as certainty.
cells_lane_clause <- function(word, lo, hi) {
  if (is.null(word)) return("")
  paste0(" · ", word, " ",
         ifelse(is.na(lo) | is.na(hi), "undefined (n < 2)",
                paste0(lane_fmt(lo), "–", lane_fmt(hi))))
}

cells_lane_tip <- function(pre, wd, nn, bc, bl, bh, wl, wh) {
  ifelse(is.na(bc), "", summarize_esc(paste0(
    pre, ifelse(is.na(nn), "", paste0("n=", nn, " · ")),
    wd$center, " ", lane_fmt(bc),
    cells_lane_clause(wd$range, bl, bh), cells_lane_clause(wd$whisk, wl, wh)
  )))
}

# The statistics themselves, for the hover card (summarize-table.js): the
# geometry above is in percent of the lane and cannot be read back.
cells_lane_stats <- function(st) {
  lapply(st[vapply(st, function(x) any(!is.na(x)), logical(1L))],
         function(x) round(x, 4L))
}

# The dumbbell: `from` and `to` as positions, the segment between them
# as [left, width] (either direction), the band per row, the reference
# line once per column. Every number rounded here; emitters only print.
cells_pair <- function(ctx, p, b) {
  rows <- ctx$rows
  wd <- p$words %||% list(from = "From", to = "To")
  fills <- as.character(p$fills %||% character())
  # The value label is the change from `from` to `to`, at one precision
  # for the whole column. A "+" only where the column also holds a
  # negative change: a column of durations reads "18", not "+18".
  lab_on <- ctx$vals_on && isTRUE(p$show_val)
  deltas <- unlist(lapply(c(list(p$cols), p$lcols), function(cn) {
    rows[[cn[["b"]]]] - rows[[cn[["a"]]]]
  }))
  dig <- if (lab_on) summarize_val_digits(deltas, p$dmin, p$dmax)
  signed <- any(deltas < 0, na.rm = TRUE)
  pr <- list(dmn = p$dmin %||% ctx$mn, dmx = p$dmax %||% ctx$mx, wd = wd,
             lab_on = lab_on, dig = dig, signed = signed)
  fi <- rows[[p$fidx]]
  base <- cells_pair_bell(ctx, p, b, pr, p$cols, p$didx, if (length(fills)) {
    ifelse(is.na(fi), NA_character_, fills[pmax(1L, fi)])
  } else {
    rep(NA_character_, ctx$n)
  }, ncnt = p$ncol)
  if (is.null(p$lcols)) {
    base
  } else {
    # Colour split: one dumbbell per level, in level order, on the
    # column's one scale; a level with no rows in the group draws none.
    # The pooled pair supplies the sort value. Each level carries its own
    # change beside its dumbbell; the column keeps one slot width.
    lv <- lapply(seq_along(p$lcols), function(j) {
      g <- cells_pair_bell(ctx, p, b, pr, p$lcols[[j]], p$ldidx[[j]],
                           rep(fills[[j]], ctx$n), p$levels[[j]],
                           ncnt = p$lncol[j])
      g[c("kind", "rf", "v", "dw")] <- NULL
      g
    })
    list(kind = "pair", multi = TRUE,
         levels = as.character(p$levels), fills = fills,
         rf = base$rf, v = base$v, lv = lv,
         dw = if (lab_on) summarize_multi_dw(lv))
  }
}

# One dumbbell from one set of end columns, like the glyph above:
# once for a plain column, once per level for a colour-split one.
cells_pair_bell <- function(ctx, p, b, pr, cn, didx, fill, lvl = NULL,
                            ncnt = NULL) {
  rows <- ctx$rows
  pos_w <- b$pos
  span_w <- b$span
  a <- rows[[cn[["a"]]]]
  bb <- rows[[cn[["b"]]]]
  lo <- rows[[cn[["lo"]]]]
  hi <- rows[[cn[["hi"]]]]
  band <- !(is.na(lo) & is.na(hi))
  blo <- ifelse(is.na(lo), pr$dmn, lo)
  bhi <- ifelse(is.na(hi), pr$dmx, hi)
  both <- !is.na(a) & !is.na(bb)
  di <- rows[[didx]]
  nn <- if (!is.null(ncnt) && !is.null(rows[[ncnt]])) {
    rows[[ncnt]]
  } else {
    rep(NA_integer_, ctx$n)
  }
  # A `to` outside the band is drawn open: the range is the question
  # the band asks, so the mark answers it without a tooltip.
  open <- band & !is.na(bb) & (bb < blo | bb > bhi)
  delta <- bb - a
  pre <- if (is.null(lvl)) "" else paste0(lvl, " · ")
  tip <- cells_pair_tip(pre, pr$wd, a, bb, lo, hi, nn, band, both, delta)
  disp <- if (pr$lab_on) {
    ifelse(both, paste0(ifelse(pr$signed & delta >= 0, "+", ""),
                        summarize_val_str(delta, pr$dig)), "")
  }
  ab <- cells_pair_ends(a, bb, both, delta)
  list(kind = "pair",
       a = pos_w(a), b = pos_w(bb),
       l = ifelse(both, pos_w(pmin(a, bb)), NA_real_),
       w = ifelse(both, span_w(pmin(a, bb), pmax(a, bb)), NA_real_),
       bl = ifelse(band, pos_w(blo), NA_real_),
       bw = ifelse(band, span_w(blo, bhi), NA_real_),
       rf = if (!is.null(p$ref)) pos_w(p$ref) else NA_real_,
       fill = fill,
       dash = !is.na(di) & di > 1L,
       open = open, tip = tip, ab = ab, v = cells_sortv(bb),
       disp = disp, dw = if (pr$lab_on) max(c(1L, nchar(disp))))
}

# Plain text, unlike the glyph's tip: the hover card reads it from
# the payload and escapes it, the cell escapes it into its attribute.
cells_pair_tip <- function(pre, wd, a, b, lo, hi, nn, band, both, delta) {
  ifelse(is.na(a) & is.na(b), "", paste0(
    pre,
    ifelse(is.na(a), "", paste0(wd$from, " ", lane_fmt(a))),
    ifelse(both, " → ", ""),
    ifelse(is.na(b), "", paste0(wd$to, " ", lane_fmt(b))),
    ifelse(both, paste0(" (", ifelse(delta >= 0, "+", ""),
                        lane_fmt(delta), ")"), ""),
    ifelse(band, paste0(" · range ",
                        ifelse(is.na(lo), "", lane_fmt(lo)), "–",
                        ifelse(is.na(hi), "", lane_fmt(hi))), ""),
    # A parent row is the mean of the rows under it
    # (lane_pair_fill()), so its card says over how many.
    ifelse(is.na(nn), "", paste0(" · mean of ", nn,
                                 ifelse(nn == 1L, " row", " rows")))
  ))
}

# The ends without their words, for the split card's one line per
# level ("30.5 -> 53 (+22.5)"); the words head the card once.
cells_pair_ends <- function(a, b, both, delta) {
  ifelse(is.na(a) & is.na(b), "", paste0(
    ifelse(is.na(a), "", lane_fmt(a)),
    ifelse(both, " → ", ""),
    ifelse(is.na(b), "", lane_fmt(b)),
    ifelse(both, paste0(" (", ifelse(delta >= 0, "+", ""),
                        lane_fmt(delta), ")"), "")
  ))
}
