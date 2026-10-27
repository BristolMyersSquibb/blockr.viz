# Table model: cells of the bar, tile, split-bar, diverging-bar, text and
# number columns. Each cells_<kind>() takes the shared context (see
# summarize_cells()) and one plan entry and returns that column of the cell
# model. The lane marks (box, point range, pair) are in
# table-model-cells-lanes.R, the swimlane and sparkline in
# table-model-cells-marks.R.

cells_bar <- function(ctx, p) {
  vraw <- ctx$rows[[p$key]]
  lab <- cells_val_parts(ctx, p, vraw)
  v <- vraw
  if (!is.null(p$denom) && is.finite(p$denom) && p$denom > 0) {
    v <- v / p$denom * 100
  }
  c(list(kind = "bar", w = cells_pct_w(v, p$dmax %||% ctx$mx),
         v = cells_sortv(v), fill = p$fill, sub = ctx$rows$.level > 0L), lab)
}

cells_tile <- function(ctx, p) {
  # The matrix cell. An empty count is an empty cell: no square, no "0".
  v <- ctx$rows[[p$key]]
  paint <- lane_tile_paint(p, ctx$rows)
  disp <- if (ctx$vals_on) {
    ifelse(paint$has, lane_fmt(v), "")
  } else {
    rep("", ctx$n)
  }
  list(kind = "tile", v = cells_sortv(v), bg = paint$bg, fg = paint$fg,
       disp = disp)
}

cells_barsplit <- function(ctx, p) {
  # Per-entry scale, like the plain bar: on the summaries path every
  # column owns its domain, and the prep-level bar_max is 0 there.
  emx <- p$dmax %||% ctx$mx
  mat <- cells_split_matrix(ctx, p)
  # `segv` (the segment tooltips) stays in the measure's own unit; only
  # the WIDTHS are scaled -- against the column max, or the facet's own N
  # when the plan carries a width denominator.
  segv <- lapply(seq_along(p$series), function(j) mat[, j])
  if (!is.null(p$denom) && is.finite(p$denom) && p$denom > 0) {
    mat <- mat / p$denom * 100
  }
  tot <- rowSums(mat)
  seg <- cells_split_seg(p, mat, tot, emx)
  vraw <- if (!is.null(p$key)) ctx$rows[[p$key]] else rowSums(
    vapply(segv, identity, numeric(ctx$n)))
  lab <- cells_split_label(ctx, p, segv, vraw)
  v <- if (is.null(p$key)) {
    tot
  } else {
    if (!is.null(p$denom) && is.finite(p$denom) && p$denom > 0) {
      vraw / p$denom * 100
    } else {
      vraw
    }
  }
  fills <- as.character(p$fills %||%
                          unname(ctx$prep$palette[as.character(p$series)]))
  inside <- cells_split_inside(ctx, p, seg, segv)
  c(list(kind = "barsplit", mode = p$mode %||% "stacked",
         names = as.character(p$series),
         # The plan's own colours win: a summaries column resolves them
         # through the same resolver as every other mark, so a split bar
         # and a split box agree about which level is which.
         fills = fills,
         seg = seg, segv = segv,
         slab = inside, sink = if (!is.null(inside)) summarize_ink(fills),
         v = cells_sortv(v)), lab)
}

# The row-by-series value matrix of a split bar; a series the rows do not
# carry is all zero, a non-finite value counts as zero.
cells_split_matrix <- function(ctx, p) {
  prefix <- p$prefix %||% ".s_"
  n <- ctx$n
  mat <- vapply(p$series, function(lv) {
    x <- ctx$rows[[paste0(prefix, lv)]]
    if (is.null(x)) rep(0, n) else as.numeric(x)
  }, numeric(n))
  mat <- matrix(mat, nrow = n, dimnames = list(NULL, p$series))
  mat[!is.finite(mat)] <- 0
  mat
}

# Segment widths: stacked scales the row total against the column max,
# percent normalises each row to 100, grouped scales each series
# independently. Computed here so both consumers print the same numbers.
cells_split_seg <- function(p, mat, tot, emx) {
  if (identical(p$mode, "grouped")) {
    lapply(seq_along(p$series), function(j) cells_pct_w(mat[, j], emx))
  } else {
    scale <- if (identical(p$mode, "percent")) {
      ifelse(tot > 0, 100, 0)
    } else if (is.finite(emx) && emx > 0) {
      tot / emx * 100
    } else {
      tot * 0
    }
    share <- ifelse(tot > 0, 1, 0) * mat / ifelse(tot > 0, tot, 1)
    lapply(seq_along(p$series), function(j) round(share[, j] * scale, 2L))
  }
}

# Grouped: the levels sit side by side, each its own bar, so each
# prints its own value beside it (`ldisp`, one vector per level) in the
# column's one slot width. Stacked and 100% keep the one total.
cells_split_label <- function(ctx, p, segv, vraw) {
  if (identical(p$mode, "grouped") && ctx$vals_on && isTRUE(p$show_val)) {
    dom <- summarize_axis_domain(p, ctx$prep)
    dig <- summarize_val_digits(unlist(segv), dom$d0, dom$d1)
    ldisp <- lapply(segv, function(v) {
      ifelse(!is.na(v) & v > 0, summarize_val_str(v, dig), "")
    })
    list(ldisp = ldisp,
         dw = max(c(1L, nchar(unlist(ldisp)))))
  } else {
    cells_val_parts(ctx, p, vraw)
  }
}

# Stacked / 100%: each segment carries its own number INSIDE it (the
# total stays at the end). The stylesheet hides one its segment is too
# narrow for, the painter measures; the ink is white or dark against
# the level's fill.
cells_split_inside <- function(ctx, p, seg, segv) {
  if (!identical(p$mode, "grouped") && ctx$vals_on && isTRUE(p$show_val)) {
    if (identical(p$mode, "percent")) {
      lapply(seg, function(s) {
        ifelse(!is.na(s) & s > 0, paste0(round(s), "%"), "")
      })
    } else {
      dom <- summarize_axis_domain(p, ctx$prep)
      dig <- summarize_val_digits(unlist(segv), dom$d0, dom$d1)
      lapply(segv, function(v) {
        ifelse(!is.na(v) & v > 0, summarize_val_str(v, dig), "")
      })
    }
  }
}

cells_bardiv <- function(ctx, p) {
  v <- ctx$rows[[p$key]]
  lab <- cells_val_parts(ctx, p, v, signed = TRUE)
  # Per-entry scale, like the plain bar: on the summaries path every
  # column owns its domain and the prep-level bar_max is 0 there, so
  # reading it would zero every width.
  dmx <- p$dmax %||% ctx$mx
  w <- if (is.finite(dmx) && dmx > 0) pmin(50, abs(v) / dmx * 50) else v * 0
  w[!is.finite(w)] <- 0
  c(list(kind = "bardiv", w = round(w, 2L), v = cells_sortv(v),
         pos = !is.na(v) & v >= 0), lab)
}

cells_text_raw <- function(ctx, p) {
  # A raw text field: the value IS the display (escaped once, here, so
  # both consumers paste it as-is), and the sort key is the text itself.
  v <- as.character(ctx$rows[[p$key]])
  v[is.na(v)] <- ""
  c(list(kind = "num", text = TRUE, v = v, disp = summarize_esc(v)),
    if (!is.null(p$failed)) list(failed = TRUE))
}

cells_dist_text <- function(ctx, p) {
  # A distribution shown as TEXT: "10.4 (7.6–13.2)". Sorts numerically
  # by the center (a text sort would rank "9" above "10").
  cc <- ctx$rows[[p$key]]
  ll <- ctx$rows[[p$lo]]
  hh <- ctx$rows[[p$hi]]
  disp <- ifelse(
    is.na(cc), "",
    paste0(lane_fmt(cc),
           ifelse(is.na(ll) | is.na(hh), "",
                  paste0(" (", lane_fmt(ll), "–", lane_fmt(hh), ")")))
  )
  list(kind = "num", v = cells_sortv(cc), disp = disp)
}

cells_alt_text <- function(ctx, p) {
  # An expr summary that evaluated to text: fall back to the text
  # column, sorted as text.
  v <- as.character(ctx$rows[[p$alt_text]])
  v[is.na(v)] <- ""
  list(kind = "num", text = TRUE, v = v, disp = summarize_esc(v))
}

cells_num <- function(ctx, p) {
  prep <- ctx$prep
  v <- ctx$rows[[p$key]]
  parts <- summarize_num_parts(v, denom = p$denom,
                               combined = isTRUE(p$combined),
                               signed = isTRUE(p$signed),
                               pct_only = isTRUE(p$pct_only),
                               fmt = if (!is.null(prep$format)) {
                                 function(x) dt_format_num(x, prep$format)
                               })
  c(list(kind = "num", v = cells_sortv(v)), parts)
}
