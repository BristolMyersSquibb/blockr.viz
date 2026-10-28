# The groups table's marks as grid grobs: colour tokens, text measuring,
# drawing primitives and one painter per glyph kind. The table picture
# (R/paint-table.R) and the cards picture (R/cards-exhibit.R) draw with them.
#
# Everything is in inches from the top-left. The vertical constants are the
# table view's CSS ones (12px lane, 1px whisker, 3px cap inset, 2px median,
# 8px fence / dot) scaled by one `px` factor. Needs grid and systemfonts
# (Suggests; callers check).

# --- constants -----------------------------------------------------------
# Colour tokens resolved to literals, from the table view's CSS.
PAINT_FILL  <- "#2a78d6"                      # --vz-table-groups-fill
PAINT_TRACK <- "#dad9d5"                      # mix(#eeeeea 80%, #898781)
PAINT_TICK  <- "#c3c2b7"                      # --vz-table-groups-tick
PAINT_RULE  <- "#e1e0d9"                      # --blockr-color-border, hairlines
PAINT_TEXT  <- "#1c1b18"
PAINT_MUTED <- "#6f6d66"

# Type size of the value labels and of a number inside a stacked segment, as a
# share of the table's (11px and 10px against 13px cells).
PAINT_VAL_SIZE <- 0.85
PAINT_SEG_SIZE <- 0.77
# Indent of a child row, in inches.
PAINT_INDENT <- 0.28
# freetype refuses macOS's Helvetica.ttc below 8pt, so a width wanted below
# PAINT_W_FLOOR is measured at PAINT_W_REF and scaled (reads a few percent wide).
PAINT_W_FLOOR <- 8
PAINT_W_REF <- 12

paint_sub <- function(col) grDevices::adjustcolor(col, alpha.f = 0.45)
paint_faint <- function(col) grDevices::adjustcolor(col, alpha.f = 0.16)

# --- measuring -----------------------------------------------------------
# The widest of `txt` in inches (0 for none). Falls back to half an em per
# character when nothing can be measured.
paint_w <- function(txt, fs, family = "sans", bold = FALSE) {
  txt <- as.character(txt[!is.na(txt)])
  if (!length(txt)) return(0)
  measure <- function(size) {
    tryCatch(
      systemfonts::string_width(txt, family = family, size = size,
                                bold = bold),
      error = function(e) NULL
    )
  }
  w <- if (fs >= PAINT_W_FLOOR) measure(fs)
  if (is.null(w)) {
    ref <- measure(PAINT_W_REF)
    w <- if (!is.null(ref)) ref * (fs / PAINT_W_REF)
  }
  w <- w[is.finite(w)]
  if (!length(w)) return(max(nchar(txt)) * fs * 0.5 / 72)
  max(w) / 72
}

paint_ch <- function(fs, family = "sans") paint_w("0", fs, family)


# --- primitives ----------------------------------------------------------
# `gl` is the geometry list: H (picture height), W (width), px (one CSS px in
# inches), lane, hair, row_h, fs, family. `y` arguments count down from the
# top, `x` from the left, both in inches. The bare grob helpers (paint_ln,
# paint_circle, paint_seg, paint_sq) take a y already measured up from the bottom.

paint_in <- function(v) grid::unit(v, "in")

paint_txt <- function(gl, s, x, y, just = "left", size = gl$fs, col = PAINT_TEXT,
                   bold = FALSE, vjust = "centre", rot = 0) {
  grid::textGrob(s, x = paint_in(x), y = paint_in(gl$H - y),
                 just = c(just, vjust), rot = rot,
                 gp = grid::gpar(fontsize = size, col = col,
                                 fontfamily = gl$family,
                                 fontface = if (bold) "bold" else "plain"))
}

paint_ln <- function(x, y, col, lwd, ...) {
  grid::linesGrob(x = paint_in(x), y = paint_in(y),
                  gp = grid::gpar(col = col, lwd = lwd, ...))
}

paint_rule <- function(gl, y, col = PAINT_RULE, lwd = 0.6) {
  paint_ln(c(0, gl$W), rep(gl$H - y, 2L), col, lwd)
}

paint_seg <- function(x1, y1, x2, y2, col, lwd, lty) {
  grid::segmentsGrob(paint_in(x1), paint_in(y1), paint_in(x2), paint_in(y2),
                     gp = grid::gpar(col = col, lwd = lwd, lty = lty))
}

paint_circle <- function(x, y, r, fill, col, lwd) {
  grid::circleGrob(x = paint_in(x), y = paint_in(y), r = paint_in(r),
                   gp = grid::gpar(fill = fill, col = col, lwd = lwd))
}

paint_sq <- function(x, y, side, fill, just = "centre") {
  grid::rectGrob(x = paint_in(x), y = paint_in(y), width = paint_in(side),
                 height = paint_in(side), just = just,
                 gp = grid::gpar(fill = fill, col = NA))
}

paint_rect <- function(x0, w, ytop, h, fill, gl) {
  if (is.na(x0) || is.na(w)) return(NULL)
  grid::rectGrob(x = paint_in(x0), y = paint_in(gl$H - ytop),
                 width = paint_in(max(w, gl$hair)), height = paint_in(h),
                 just = c("left", "top"),
                 gp = grid::gpar(fill = fill, col = NA))
}

paint_lane_x <- function(pct, x, w) x + pct / 100 * w

# Top of a lane of height `h` centred in the row.
paint_lane_y <- function(ytop, gl, h = gl$lane) ytop + (gl$row_h - h) / 2

# A mark with a positive size (a zero draws no fill: paint_rect()'s hairline floor
# would read as a little).
paint_pos <- function(v) !is.na(v) && v > 0

# A rect from `l`% over `wd`% of the lane at x / w.
paint_lane_rect <- function(l, wd, x, w, y, h, fill, gl) {
  paint_rect(paint_lane_x(l, x, w), wd / 100 * w, y, h, fill, gl)
}

# The hairline behind a bare box or point range.
paint_bare <- function(x, w, mid, gl) {
  paint_rect(x, w, mid - gl$hair / 2, gl$hair, PAINT_RULE, gl)
}

# --- the marks -----------------------------------------------------------
# One function per kind, taking the cell box in inches and the row's geometry
# in percent of the lane, as the browser gets it.

paint_bar <- function(c, i, x, w, ytop, gl) {
  h <- gl$lane
  y <- paint_lane_y(ytop, gl)
  fill <- (if (length(c$fill)) c$fill[[min(i, length(c$fill))]]) %||% PAINT_FILL
  if (isTRUE(c$child[[min(i, length(c$child))]])) fill <- paint_sub(fill)
  list(paint_rect(x, w, y, h, PAINT_TRACK, gl),
       if (paint_pos(c$w[[i]])) paint_rect(x, c$w[[i]] / 100 * w, y, h, fill, gl))
}

paint_barsplit <- function(c, i, x, w, ytop, gl, xr = NULL) {
  if (identical(c$mode, "grouped")) {
    return(paint_barsplit_grouped(c, i, x, w, ytop, gl, xr))
  }
  h <- gl$lane
  y <- paint_lane_y(ytop, gl)
  size <- gl$fs * PAINT_SEG_SIZE
  out <- list(paint_rect(x, w, y, h, PAINT_TRACK, gl))
  at <- x
  for (j in seq_along(c$names)) {
    seg <- c$seg[[j]][[i]]
    if (!paint_pos(seg)) next
    sw <- seg / 100 * w
    # The segment's own number, inside it where it fits.
    lb <- if (!is.null(c$slab)) c$slab[[j]][[i]] else ""
    fits <- nzchar(lb) &&
      paint_w(lb, size, gl$family, bold = TRUE) + gl$px * 4 <= sw
    out <- c(out, list(
      paint_rect(at, sw, y, h, c$fills[[j]], gl),
      if (fits) {
        paint_txt(gl, lb, at + sw / 2, y + h / 2, just = "centre", size = size,
               col = c$sink[[j]], bold = TRUE)
      }
    ))
    at <- at + sw
  }
  out
}

# One 10px row per level with a value in this row, stacked in the cell. With
# value labels each level also owns a text line beside its row.
paint_barsplit_grouped <- function(c, i, x, w, ytop, gl, xr) {
  lv <- which(vapply(seq_along(c$names), function(j) paint_pos(c$segv[[j]][[i]]),
                     logical(1L)))
  h <- gl$px * 10
  lab <- !is.null(c$ldisp) && !is.null(xr)
  pitch <- paint_pitch(gl$px, gl$fs, lab, gl$px * 14)
  tot <- max(length(lv) - 1L, 0L) * pitch + h
  y0 <- ytop + (gl$row_h - tot) / 2
  unlist(lapply(seq_along(lv), function(r) {
    j <- lv[[r]]
    y <- y0 + (r - 1) * pitch
    seg <- c$seg[[j]][[i]]
    list(paint_rect(x, w, y, h, PAINT_TRACK, gl),
         if (paint_pos(seg)) paint_rect(x, seg / 100 * w, y, h, c$fills[[j]], gl),
         if (lab && nzchar(c$ldisp[[j]][[i]])) {
           paint_txt(gl, c$ldisp[[j]][[i]], xr, y + h / 2, just = "right",
                  size = gl$fs * PAINT_VAL_SIZE, col = PAINT_MUTED)
         })
  }), recursive = FALSE)
}

paint_bardiv <- function(c, i, x, w, ytop, gl) {
  h <- gl$lane
  y <- paint_lane_y(ytop, gl)
  mid <- x + w / 2
  ww <- c$w[[i]] / 100 * w / 2
  list(paint_rect(x, w, y, h, PAINT_TRACK, gl),
       if (paint_pos(c$w[[i]])) {
         paint_rect(if (isTRUE(c$pos[[i]])) mid else mid - ww, ww, y, h, PAINT_FILL,
                 gl)
       },
       paint_rect(mid - gl$hair / 2, gl$hair, y, h, PAINT_TICK, gl))
}

# Two whisker segments (never through the body), caps, a translucent IQR body
# and a solid median tick. CSS: .vz-table-groups-boxcell.
paint_box <- function(c, i, x, w, ytop, gl, fill = PAINT_FILL) {
  if (is.na(c$bc[[i]])) return(list())
  h <- gl$lane
  px <- gl$px
  y <- paint_lane_y(ytop, gl)
  mid <- y + h / 2
  wh <- function(l, ww) {
    if (is.na(ww)) return(NULL)
    paint_lane_rect(l, ww, x, w, mid - gl$hair / 2, gl$hair, fill, gl)
  }
  cap <- function(l, ww) {
    if (is.na(ww)) return(NULL)
    paint_rect(paint_lane_x(l, x, w) - gl$hair / 2, gl$hair, y + px * 3, h - px * 6,
            fill, gl)
  }
  list(
    if (isTRUE(c$bare)) paint_bare(x, w, mid, gl),
    wh(c$wl[[i]], c$w1[[i]]),
    wh(c$b2[[i]], c$w2[[i]]),
    cap(c$wl[[i]], c$w1[[i]]),
    cap(c$wh[[i]], c$w2[[i]]),
    if (!is.na(c$bw[[i]])) {
      paint_lane_rect(c$bl[[i]], c$bw[[i]], x, w, y + px, h - 2 * px,
                   paint_sub(fill), gl)
    },
    paint_rect(paint_lane_x(c$bc[[i]], x, w) - px, px * 2, y, h, fill, gl)
  )
}

# Fence band (a tint), inner range, ringed centre dot. The dot is a circle
# sized off the row height, never off the lane width.
paint_pr <- function(c, i, x, w, ytop, gl, fill = PAINT_FILL) {
  if (is.na(c$c[[i]])) return(list())
  px <- gl$px
  mid <- paint_lane_y(ytop, gl) + gl$lane / 2
  list(
    if (isTRUE(c$bare)) paint_bare(x, w, mid, gl),
    if (!is.null(c$ow) && !is.na(c$ow[[i]])) {
      paint_lane_rect(c$ol[[i]], c$ow[[i]], x, w, mid - px * 4, px * 8,
                   paint_faint(fill), gl)
    },
    if (!is.na(c$rw[[i]])) {
      paint_lane_rect(c$l[[i]], c$rw[[i]], x, w, mid - px * 2, px * 4, fill, gl)
    },
    paint_circle(paint_lane_x(c$c[[i]], x, w), gl$H - mid, px * 4, fill, "#ffffff",
              1.2)
  )
}

# The dumbbell: band, reference line, link, a hollow diamond at `from` and a
# dot at `to`, open when it falls outside the band.
paint_pair <- function(c, i, x, w, ytop, gl) {
  h <- gl$lane
  y <- paint_lane_y(ytop, gl)
  mid <- y + h / 2
  fill <- c$fill[[i]]
  if (is.na(fill)) fill <- PAINT_FILL
  lx <- function(p) paint_lane_x(p, x, w)
  # A thinned split lane (three or more levels) takes smaller marks.
  r <- min(gl$px * 4, h * 0.45)
  list(
    if (!is.na(c$bw[[i]])) {
      paint_lane_rect(c$bl[[i]], c$bw[[i]], x, w, y, h, "#eeede8", gl)
    },
    if (!is.na(c$rf)) {
      paint_seg(lx(c$rf), gl$H - ytop, lx(c$rf), gl$H - ytop - gl$row_h,
             "#8d8b84", 0.8, "dashed")
    },
    if (!is.na(c$w[[i]])) {
      x1 <- lx(c$l[[i]])
      paint_seg(x1, gl$H - mid, x1 + c$w[[i]] / 100 * w, gl$H - mid, fill, 1.6,
             if (isTRUE(c$dash[[i]])) "dashed" else "solid")
    },
    if (!is.na(c$a[[i]])) {
      grid::polygonGrob(
        x = paint_in(lx(c$a[[i]]) + c(0, r, 0, -r)),
        y = paint_in(gl$H - mid + c(r, 0, -r, 0)),
        gp = grid::gpar(fill = "#ffffff", col = fill, lwd = 1.2))
    },
    if (!is.na(c$b[[i]])) {
      paint_circle(lx(c$b[[i]]), gl$H - mid, r,
                if (isTRUE(c$open[[i]])) "#ffffff" else fill, fill, 1.2)
    }
  )
}

# The swimlane. Segments are [left%, width%, fill index] on the column's
# shared x domain; each is at least 2px wide so a one-day event stays visible.
paint_interval <- function(c, i, x, w, ytop, gl) {
  y <- paint_lane_y(ytop, gl)
  lapply(c$segs[[i]], function(sg) {
    paint_rect(paint_lane_x(sg[[1L]], x, w),
            max(sg[[2L]] / 100 * w, gl$px * 2), y, gl$lane,
            c$fills[[sg[[3L]]]], gl)
  })
}

# Parse a pre-printed SVG point string ("x,y x,y ...", viewBox 0 0 100 36,
# y downward) into a matrix, so the slide shows the trajectory the screen drew.
paint_sp_pts <- function(s) {
  if (!length(s) || is.na(s) || !nzchar(s)) return(NULL)
  p <- do.call(rbind, lapply(strsplit(strsplit(s, " ", fixed = TRUE)[[1L]],
                                      ",", fixed = TRUE),
                             as.numeric))
  if (!is.matrix(p) || nrow(p) < 1L) NULL else p
}

paint_sparkline <- function(c, i, x, w, ytop, gl, fill = PAINT_FILL) {
  h <- gl$lane
  y <- paint_lane_y(ytop, gl)
  X <- function(v) x + v / 100 * w
  Y <- function(v) gl$H - (y + v / 36 * h)
  bd <- paint_sp_pts(c$bd[[i]])
  pl <- paint_sp_pts(c$pl[[i]])
  list(
    if (!is.na(c$rby)) {
      paint_rect(x, w, y + c$rby / 36 * h, c$rbh / 36 * h, paint_faint(fill), gl)
    },
    if (!is.null(bd)) {
      grid::polygonGrob(x = paint_in(X(bd[, 1L])), y = paint_in(Y(bd[, 2L])),
                        gp = grid::gpar(fill = PAINT_TRACK, col = NA))
    },
    if (!is.na(c$rc)) {
      paint_ln(c(x, x + w), rep(Y(c$rc), 2L), PAINT_TICK, 0.8, lty = "22")
    },
    if (!is.null(pl) && nrow(pl) > 1L) {
      paint_ln(X(pl[, 1L]), Y(pl[, 2L]), fill, 1.3, lineend = "round",
            linejoin = "round")
    },
    if (!is.na(c$dx[[i]])) {
      paint_circle(X(c$dx[[i]]), gl$H - (y + c$dy[[i]] / 100 * h), gl$px * 3,
                fill, "#ffffff", 1)
    }
  )
}

# A colour-split cell stacks only the levels the row has, so a row of one
# level draws one full-height lane.
paint_multi <- function(c, i, x, w, ytop, gl, xr = NULL) {
  drawn <- paint_multi_drawn(c, i)
  k <- length(drawn)
  if (!k) return(list())
  lab <- !is.null(c$dw) && !is.null(xr)
  h <- gl$lane
  pitch <- paint_pitch(gl$px, gl$fs, lab, gl$px * 12 + gl$px * 2)
  tot <- (k - 1) * pitch + h
  y0 <- ytop + (gl$row_h - tot) / 2
  unlist(lapply(seq_len(k), function(jj) {
    lv <- c$lv[[drawn[[jj]]]]
    yy <- y0 + (jj - 1) * pitch - (gl$row_h - h) / 2
    marks <- if (identical(c$kind, "pair")) {
      lv$rf <- c$rf
      paint_pair(lv, i, x, w, yy, gl)
    } else if (identical(c$kind, "box")) {
      paint_box(lv, i, x, w, yy, gl, lv$fill %||% PAINT_FILL)
    } else {
      paint_pr(lv, i, x, w, yy, gl, lv$fill %||% PAINT_FILL)
    }
    c(marks, if (lab && !is.null(lv$disp) && nzchar(lv$disp[[i]])) {
      list(paint_txt(gl, lv$disp[[i]], xr, yy + gl$row_h / 2, just = "right",
                  size = gl$fs * PAINT_VAL_SIZE, col = PAINT_MUTED))
    })
  }), recursive = FALSE)
}

# The levels a colour-split cell draws in row i: the ones the row has.
paint_multi_drawn <- function(c, i) {
  Filter(function(j) {
    g <- c$lv[[j]]
    if (identical(c$kind, "pair")) {
      !is.na(g$a[[i]]) || !is.na(g$b[[i]])
    } else {
      !is.na((if (identical(c$kind, "box")) g$bc else g$c)[[i]])
    }
  }, seq_along(c$lv))
}

# Distance between two levels' lanes: the lane `h` plus its gap, or a line of
# the label type when each carries a number and that is larger.
paint_pitch <- function(px, fs, lab, h) {
  if (lab) max(h, fs * PAINT_VAL_SIZE / 72 * 1.25) else h
}
