# The swimlane in the static picture (marks.js interval()): the main lane,
# its thin lanes above, full bars below, their names in a gutter at the left.

# The swimlane. Segments are [left%, width%, fill index] on the column's
# shared x domain; each is at least 2px wide so a one-day event stays visible.
# A row with thin lanes stacks them above the main lane, as marks.js does.
paint_interval <- function(c, i, x, w, ytop, gl) {
  px <- gl$px
  all <- paint_at(c$sl, i) %||% list()
  full <- Filter(function(l) isTRUE(l$f), all)
  sub <- Filter(function(l) !isTRUE(l$f), all)
  y <- paint_lane_y(ytop, gl)
  thin <- list()
  names <- list()
  even <- isTRUE(c$eq)
  # Named bars: the names in a gutter at the left, the lanes after it.
  if (!is.null(c$mn)) {
    g <- paint_iv_gutter(c$lw, gl)
    name <- function(t, yy, main) {
      paint_txt(gl, t, x + g - 6 * px, yy, just = "right",
                size = 10 * 0.75 * gl$fs / 9, bold = main,
                col = if (main) PAINT_TEXT else PAINT_MUTED)
    }
    x <- x + g
    w <- w - g
  }
  if (length(all)) {
    tot <- paint_iv_stack(length(sub), px, even, length(full))
    y0 <- ytop + (gl$row_h - tot) / 2
    pitch <- paint_iv_pitch(even)
    thin <- unlist(lapply(seq_along(sub), function(j) {
      paint_iv_thin(c, sub[[j]], x, w, y0 + (j - 1L) * pitch * px, gl, even)
    }), recursive = FALSE)
    # Even lanes: the overlays sit across the middle of the stack.
    y <- if (even) {
      y0 + (tot - gl$lane) / 2
    } else {
      y0 + length(sub) * pitch * px + 2 * px
    }
    # Full bars below the main lane, at its height.
    thin <- c(thin, unlist(lapply(seq_along(full), function(j) {
      yj <- y + j * (gl$lane + 2 * px)
      c(paint_iv_full(c, full[[j]], x, w, yj, gl),
        if (!is.null(c$mn)) list(name(full[[j]]$n, yj + gl$lane / 2, FALSE)))
    }), recursive = FALSE))
  }
  if (!is.null(c$mn)) names <- list(name(c$mn, y + gl$lane / 2, TRUE))
  mid <- gl$H - (y + gl$lane / 2)
  segs <- lapply(c$segs[[i]], function(sg) {
    paint_iv_seg(sg, c$fills[[sg[[3L]]]], x, w, y, gl$lane, gl)
  })
  # The overlays, as marks.js draws them: surface ticks, the dark line, the
  # open end's arrow, then the events on top.
  tk <- lapply(paint_at(c$tk, i), function(v) {
    paint_rect(paint_lane_x(v, x, w) - px * 0.75, px * 1.5, y, gl$lane,
               "#ffffff", gl)
  })
  bd <- lapply(paint_at(c$bd, i), function(b) {
    paint_rect(paint_lane_x(b[[1L]], x, w), b[[2L]] / 100 * w,
               y + gl$lane / 2 - px, px * 2, PAINT_TEXT, gl)
  })
  op <- paint_at(c$op, i)
  op <- if (length(op) && !is.na(op)) {
    paint_iv_arrow(paint_lane_x(op, x, w), mid, 7, 5, PAINT_TEXT, px)
  }
  ev <- lapply(paint_at(c$ev, i), function(e) {
    paint_shape(e[[2L]], paint_lane_x(e[[1L]], x, w), mid, px)
  })
  lab <- paint_iv_labels(c$segs[[i]], paint_at(c$bl, i), paint_at(c$op, i),
                         x, w, y + gl$lane / 2, gl)
  c(thin, segs, tk, bd, list(op), ev, lab, names)
}

# The gutter of named bars: `lw` characters of the names' type, and a gap.
paint_iv_gutter <- function(lw, gl) {
  size <- 10 * 0.75 * gl$fs / 9
  ft_text_widths(strrep("0", lw), gl$family, size) + 8 * gl$px
}

# A full bar below the main lane from `y`: its segments at the main lane's
# height, its open end, its events and its bar labels.
paint_iv_full <- function(c, l, x, w, y, gl) {
  px <- gl$px
  mid <- gl$H - (y + gl$lane / 2)
  fills <- vapply(l$s, function(sg) c$fills[[sg[[3L]]]], "")
  segs <- lapply(seq_along(l$s), function(j) {
    paint_iv_seg(l$s[[j]], fills[[j]], x, w, y, gl$lane, gl)
  })
  op <- if (!is.null(l$o) && !is.na(l$o)) {
    paint_iv_arrow(paint_lane_x(l$o, x, w), mid, 7, 5, PAINT_TEXT, px)
  }
  ev <- lapply(l$e, function(e) {
    paint_shape(e[[2L]], paint_lane_x(e[[1L]], x, w), mid, px)
  })
  lab <- paint_iv_labels(l$s, l$l, l$o, x, w, y + gl$lane / 2, gl)
  c(segs, list(op), ev, lab)
}
