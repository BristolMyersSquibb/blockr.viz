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
    paint_iv_arrow(paint_lane_x(op, x, w), mid, 7, 5, PAINT_EV_INK, px)
  }
  ev <- lapply(paint_at(c$ev, i), function(e) {
    paint_event(e, paint_lane_x(e[[1L]], x, w), mid, gl)
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
  op <- if (!is.null(l$o) && !is.na(l$o) && length(fills)) {
    paint_iv_arrow(paint_lane_x(l$o, x, w), mid, 7, 5, PAINT_EV_INK, px)
  }
  ev <- lapply(l$e, function(e) {
    paint_event(e, paint_lane_x(e[[1L]], x, w), mid, gl)
  })
  lab <- paint_iv_labels(l$s, l$l, l$o, x, w, y + gl$lane / 2, gl)
  c(segs, list(op), ev, lab)
}

# Bar labels as marks.js fitLabels() places them: on the bar when the text
# fits, after it when the gap to the lane's next bar holds it, else none.
# `ymid` is the lane's middle, measured from the top.
paint_iv_labels <- function(segs, labels, open, x, w, ymid, gl) {
  if (!length(labels)) return(list())
  px <- gl$px
  size <- 10.5 * 0.75 * gl$fs / 9
  x0 <- vapply(segs, function(g) g[[1L]], 0)
  x1 <- x0 + vapply(segs, function(g) g[[2L]], 0)
  out <- list()
  for (j in seq_along(segs)) {
    t <- as.character(labels[[j]] %||% "")
    if (!nzchar(t)) next
    nx <- min(c(100, x0[x0 >= x1[[j]] - 0.01]))
    tw <- ft_text_widths(t, gl$family, size)
    a <- paint_lane_x(x0[[j]], x, w)
    b <- paint_lane_x(x1[[j]], x, w)
    gap <- if (length(open) && !is.na(open) && abs(open - x1[[j]]) < 0.5) {
      10 * px
    } else {
      3 * px
    }
    at <- if (tw + 6 * px <= b - a) {
      a + 3 * px
    } else if (b + gap + tw + 3 * px <= paint_lane_x(nx, x, w)) {
      b + gap
    }
    if (!is.null(at)) {
      out[[length(out) + 1L]] <- paint_txt(gl, t, at, ymid, size = size)
    }
  }
  out
}

# A thin lane is 5px with a 3px line; the main lane sits 2px under the last.
# Even lanes (no main lane) are 10px with an 8px bar.
paint_iv_pitch <- function(even = FALSE) if (even) 10 else 5

# The height of a stack of `k` thin lanes over the main lane and `full` full
# bars under it, or of `k` even lanes.
paint_iv_stack <- function(k, px, even = FALSE, full = 0) {
  if (even) k * 10 * px else (k * paint_iv_pitch() + 2 + 12 + full * 14) * px
}

# One lane from `ytop`: its segments on its bar, its open end's arrow in the
# colour of its last segment, its events.
paint_iv_thin <- function(c, l, x, w, ytop, gl, even = FALSE) {
  px <- gl$px
  bar <- if (even) 8 else 3
  mid <- gl$H - (ytop + (bar / 2 + 1) * px)
  fills <- vapply(l$s, function(sg) c$fills[[sg[[3L]]]], "")
  segs <- lapply(seq_along(l$s), function(j) {
    paint_iv_seg(l$s[[j]], fills[[j]], x, w, ytop + px, bar * px, gl)
  })
  op <- if (!is.null(l$o) && !is.na(l$o) && length(fills)) {
    half <- if (even) 5 else 3.5
    paint_iv_arrow(paint_lane_x(l$o, x, w), mid, half * 10 / 7, half,
                   fills[[length(fills)]], px)
  }
  ev <- lapply(l$e, function(e) {
    paint_event(e, paint_lane_x(e[[1L]], x, w), mid, gl)
  })
  # A thin lane is too low for text; an even lane has room.
  lab <- if (even) {
    paint_iv_labels(l$s, l$l, l$o, x, w, ytop + (bar / 2 + 1) * px, gl)
  }
  c(segs, list(op), ev, lab)
}

# A segment `sg` in its style (marks.js seg()): solid, dashed, hollow, a ring
# at its start (dot), a down step at its start or outlined.
paint_iv_seg <- function(sg, fill, x, w, y, h, gl) {
  px <- gl$px
  x0 <- paint_lane_x(sg[[1L]], x, w)
  wd <- max(sg[[2L]] / 100 * w, px * 2)
  st <- if (length(sg) >= 5L) as.integer(sg[[5L]]) else 1L
  mid <- gl$H - (y + h / 2)
  switch(as.character(st),
    "2" = grid::gList(
      paint_rect(x0, wd, y, h, "#ffffff", gl),
      paint_ln(c(x0, x0 + wd), c(mid, mid), fill, 1.4, lty = "22")
    ),
    "3" = grid::rectGrob(
      x = paint_in(x0), y = paint_in(gl$H - y + px), width = paint_in(wd),
      height = paint_in(h + 2 * px), just = c("left", "top"),
      gp = grid::gpar(fill = "#ffffff", col = fill, lwd = 0.8)
    ),
    "4" = paint_circle(x0, mid, px * 2.5, "#ffffff", fill, 0.9),
    "6" = grid::rectGrob(
      x = paint_in(x0), y = paint_in(gl$H - y), width = paint_in(wd),
      height = paint_in(h), just = c("left", "top"),
      gp = grid::gpar(fill = fill, col = PAINT_TEXT, lwd = 0.8)
    ),
    "5" = grid::polygonGrob(
      x = paint_in(x0 + c(-3, 3, 0) * px),
      y = paint_in(mid + c(3.5, 3.5, -0.5) * px),
      gp = grid::gpar(fill = PAINT_MUTED, col = NA)
    ),
    "7" = paint_rect(x0, wd, y + h * 0.3, h * 0.4, fill, gl),
    "8" = paint_rect(x0, wd, y + h / 2 - px / 2, px, fill, gl),
    paint_rect(x0, wd, y, h, fill, gl)
  )
}

# The open end's arrowhead at `x0`, `len` long and `half` high either side.
paint_iv_arrow <- function(x0, mid, len, half, fill, px) {
  x0 <- x0 + px
  grid::polygonGrob(x = paint_in(x0 + c(0, len, 0) * px),
                    y = paint_in(mid + c(half, 0, -half) * px),
                    gp = grid::gpar(fill = fill, col = NA))
}
