# The swimlane in the static picture (marks.js interval()): the main lane,
# its thin lanes above, full bars below, their names in a gutter at the left.

# The swimlane. Segments are [left%, width%, fill index] on the column's
# shared x domain; each is at least 2px wide so a one-day event stays visible.
# A row with thin lanes stacks them above the main lane, as marks.js does.
paint_interval <- function(c, i, x, w, ytop, gl) {
  px <- gl$px
  all <- paint_at(c$sl, i) %||% list()
  even <- paint_iv_even(c, i)
  # In an even row a full bar is one more even lane.
  full <- if (even) list() else Filter(function(l) isTRUE(l$f), all)
  sub <- if (even) all else Filter(function(l) !isTRUE(l$f), all)
  y <- paint_lane_y(ytop, gl)
  thin <- list()
  names <- list()
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
    # Full bars below the main lane, at its height, 4px apart.
    thin <- c(thin, unlist(lapply(seq_along(full), function(j) {
      yj <- y + j * (gl$lane + 4 * px)
      c(paint_iv_full(c, full[[j]], x, w, yj, gl),
        if (!is.null(c$mn)) list(name(full[[j]]$n, yj + gl$lane / 2, FALSE)))
    }), recursive = FALSE))
  }
  if (!is.null(c$mn) && !even) names <- list(name(c$mn, y + gl$lane / 2, TRUE))
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
  c(thin, segs, tk, bd, list(op), ev, names)
}

# The gutter of named bars: `lw` characters of the names' type, and a gap.
paint_iv_gutter <- function(lw, gl) {
  size <- 10 * 0.75 * gl$fs / 9
  ft_text_widths(strrep("0", lw), gl$family, size) + 8 * gl$px
}

# A full bar below the main lane from `y`: its segments at the main lane's
# height, its open end and its events.
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
  c(segs, list(op), ev)
}

# A thin lane is 5px with a 3px line; the main lane sits 2px under the last.
# Even lanes (no main lane) are 10px with an 8px bar.
paint_iv_pitch <- function(even = FALSE) if (even) 10 else 5

# Whether row i's lanes are even: the column has no main-lane record, or the
# row has lanes and no main-lane record of its own (marks.js interval()).
paint_iv_even <- function(c, i) {
  isTRUE(c$eq) ||
    (length(paint_at(c$sl, i)) > 0L && !length(c$segs[[i]]))
}

# The height of a stack of `k` thin lanes over the main lane and `full` full
# bars under it, or of `k` even lanes.
paint_iv_stack <- function(k, px, even = FALSE, full = 0) {
  if (even) k * 10 * px else (k * paint_iv_pitch() + 2 + 12 + full * 16) * px
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
  c(segs, list(op), ev)
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
