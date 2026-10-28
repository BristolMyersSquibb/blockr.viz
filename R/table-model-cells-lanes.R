# The box and point-range lanes. `b` is the per-entry scale from cells_bounds().
cells_lane <- function(ctx, p, b) {
  rows <- ctx$rows
  wd <- p$words %||% list(center = "Center", range = "Range")
  # Column-level: TRUE when the mark draws no outer range, so the lane keeps a
  # hairline. Other lanes are bare because the whisker or fence band is the
  # rail (LANE_DIST_STYLES). Derived from the leaf columns when the plan is silent.
  bare <- if (is.null(p$bare)) {
    !("wl" %in% names(p$cols %||% character()))
  } else {
    isTRUE(p$bare)
  }
  # The value label: the centre, at one precision for the whole column.
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
    # Colour split: one lane per level on the column's scale. The pooled glyph
    # supplies the sort value; each level has its own label, the column one
    # slot width.
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

# A missing stat column (the single-value dot) reads as all-NA: center-only.
cells_col_or_na <- function(rows, cn, nm) {
  if (!is.na(cn[nm]) && !is.null(rows[[cn[[nm]]]])) {
    rows[[cn[[nm]]]]
  } else {
    rep(NA_real_, nrow(rows))
  }
}

# One glyph from one set of stat columns; once per level for a colour split.
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
  # A split glyph's tooltip names its level first.
  pre <- if (is.null(lvl)) "" else paste0(lvl, " \u00b7 ")
  wl <- cells_col_or_na(rows, cn, "wl")
  wh <- cells_col_or_na(rows, cn, "wh")
  tip <- cells_lane_tip(pre, lane$wd, nn, bc, bl, bh, wl, wh)
  r <- cells_lane_stats(list(bc = bc, bl = bl, bh = bh, wl = wl, wh = wh))
  pos_w <- b$pos
  span_w <- b$span
  if (identical(p$kind, "box")) {
    # A degenerate whisker side ships NA and draws nothing.
    list(kind = "box", bare = lane$bare,
         wl = pos_w(wl), w1 = span_w(wl, bl),
         bl = pos_w(bl), bw = span_w(bl, bh), bc = pos_w(bc),
         b2 = pos_w(bh), w2 = span_w(bh, wh), wh = pos_w(wh),
         nn = nn, tip = tip, v = cells_sortv(bc),
         r = r) |> c(lab)
  } else {
    # Dot style: the outer range as a fence band behind the inner bar (ol/ow),
    # the inner range as the bar (l/rw), the centre as the dot. Any may be absent.
    list(kind = "pointrange", bare = lane$bare,
         ol = pos_w(wl), ow = span_w(wl, wh),
         c = pos_w(bc), l = pos_w(bl), rw = span_w(bl, bh),
         nn = nn, tip = tip, v = cells_sortv(bc),
         r = r) |> c(lab)
  }
}

# One tooltip for both styles. NA bounds (n < 2) read "undefined" instead of
# a zero-width interval.
cells_lane_clause <- function(word, lo, hi) {
  if (is.null(word)) return("")
  paste0(" \u00b7 ", word, " ",
         ifelse(is.na(lo) | is.na(hi), "undefined (n < 2)",
                paste0(lane_fmt(lo), "\u2013", lane_fmt(hi))))
}

cells_lane_tip <- function(pre, wd, nn, bc, bl, bh, wl, wh) {
  ifelse(is.na(bc), "", summarize_esc(paste0(
    pre, ifelse(is.na(nn), "", paste0("n=", nn, " \u00b7 ")),
    wd$center, " ", lane_fmt(bc),
    cells_lane_clause(wd$range, bl, bh), cells_lane_clause(wd$whisk, wl, wh)
  )))
}

# The statistics for the hover card; the geometry is percent of the lane and
# cannot be read back.
cells_lane_stats <- function(st) {
  lapply(st[vapply(st, function(x) any(!is.na(x)), logical(1L))],
         function(x) round(x, 4L))
}

# The dumbbell: `from`/`to` positions, the segment between them as
# [left, width], the band per row, the reference line once per column.
cells_pair <- function(ctx, p, b) {
  rows <- ctx$rows
  wd <- p$words %||% list(from = "From", to = "To")
  fills <- as.character(p$fills %||% character())
  # The value label is the change from `from` to `to`, at one precision for the
  # column; "+" only where the column holds a negative change.
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
    # Colour split: one dumbbell per level on the column's scale; the pooled
    # pair supplies the sort value.
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

# One dumbbell from one set of end columns; once per level for a split.
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
  # A `to` outside the band is drawn open.
  open <- band & !is.na(bb) & (bb < blo | bb > bhi)
  delta <- bb - a
  pre <- if (is.null(lvl)) "" else paste0(lvl, " \u00b7 ")
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

# Plain text: the hover card and the cell attribute each escape it.
cells_pair_tip <- function(pre, wd, a, b, lo, hi, nn, band, both, delta) {
  ifelse(is.na(a) & is.na(b), "", paste0(
    pre,
    ifelse(is.na(a), "", paste0(wd$from, " ", lane_fmt(a))),
    ifelse(both, " \u2192 ", ""),
    ifelse(is.na(b), "", paste0(wd$to, " ", lane_fmt(b))),
    ifelse(both, paste0(" (", ifelse(delta >= 0, "+", ""),
                        lane_fmt(delta), ")"), ""),
    ifelse(band, paste0(" \u00b7 range ",
                        ifelse(is.na(lo), "", lane_fmt(lo)), "\u2013",
                        ifelse(is.na(hi), "", lane_fmt(hi))), ""),
    # A parent row is the mean of its rows (lane_pair_fill()); say over how many.
    ifelse(is.na(nn), "", paste0(" \u00b7 mean of ", nn,
                                 ifelse(nn == 1L, " row", " rows")))
  ))
}

# The ends without their words, for the split card's one line per level.
cells_pair_ends <- function(a, b, both, delta) {
  ifelse(is.na(a) & is.na(b), "", paste0(
    ifelse(is.na(a), "", lane_fmt(a)),
    ifelse(both, " \u2192 ", ""),
    ifelse(is.na(b), "", lane_fmt(b)),
    ifelse(both, paste0(" (", ifelse(delta >= 0, "+", ""),
                        lane_fmt(delta), ")"), "")
  ))
}
