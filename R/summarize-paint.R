# Paint a summarize table as ONE grid picture ---------------------------------
#
# A third consumer of summarize_cells(), beside the HTML markup and the JSON
# payload. A PowerPoint table cell holds text runs only and cannot hold a
# glyph, so on a slide the exhibit is one picture (text not selectable).
# Export-only: nothing on the block's render path calls it.
#
# Everything is computed in inches from the top-left. The vertical constants
# are the CSS ones (12px lane, 1px whisker, 3px cap inset, 2px median, 8px
# fence / dot) scaled by one `px` factor. Needs grid and systemfonts
# (Suggests; callers check) and ragg to rasterise.

# --- constants -----------------------------------------------------------
# Colour tokens resolved to literals. Values from R/summarize-table-css.R.
RP_FILL  <- "#2a78d6"                      # --blockr-summarize-fill
RP_TRACK <- "#dad9d5"                      # mix(#eeeeea 80%, #898781)
RP_TICK  <- "#c3c2b7"                      # --blockr-summarize-tick
RP_RULE  <- "#e1e0d9"                      # --blockr-color-border, hairlines
RP_TEXT  <- "#1c1b18"
RP_MUTED <- "#6f6d66"

# Type size of the value labels and of a number inside a stacked segment, as a
# share of the table's (11px and 10px against 13px cells).
RP_VAL_SIZE <- 0.85
RP_SEG_SIZE <- 0.77
# Indent of a child row, in inches.
RP_INDENT <- 0.28
# freetype refuses macOS's Helvetica.ttc below 8pt, so a width wanted below
# RP_W_FLOOR is measured at RP_W_REF and scaled (reads a few percent wide).
RP_W_FLOOR <- 8
RP_W_REF <- 12

rp_sub <- function(col) grDevices::adjustcolor(col, alpha.f = 0.45)
rp_faint <- function(col) grDevices::adjustcolor(col, alpha.f = 0.16)

# --- measuring -----------------------------------------------------------
# The widest of `txt` in inches (0 for none). Falls back to half an em per
# character when nothing can be measured.
rp_w <- function(txt, fs, family = "sans", bold = FALSE) {
  txt <- as.character(txt[!is.na(txt)])
  if (!length(txt)) return(0)
  measure <- function(size) {
    tryCatch(
      systemfonts::string_width(txt, family = family, size = size,
                                bold = bold),
      error = function(e) NULL
    )
  }
  w <- if (fs >= RP_W_FLOOR) measure(fs)
  if (is.null(w)) {
    ref <- measure(RP_W_REF)
    w <- if (!is.null(ref)) ref * (fs / RP_W_REF)
  }
  w <- w[is.finite(w)]
  if (!length(w)) return(max(nchar(txt)) * fs * 0.5 / 72)
  max(w) / 72
}

rp_ch <- function(fs, family = "sans") rp_w("0", fs, family)

# --- layout --------------------------------------------------------------
# The stub and the number columns shrink to fit; the glyph columns share the
# slack (the `width:1%` idiom of summarize-table-css.R). A glyph column keeps
# a label slot of `dw` characters so its lanes start and end at the same x on
# every row.
rp_layout <- function(m, prep, width_in, fs = 9, family = "sans",
                      pad = 0.06, lane_min = 0.9) {

  plan <- prep$plan
  hdr <- vapply(plan, function(p) p$label %||% "", character(1L))
  sub <- vapply(plan, function(p) p$sub_label %||% "", character(1L))

  stub_lab <- summarize_label_header(prep)
  indent <- ifelse(m$level > 0L, RP_INDENT, 0)
  stub_w <- max(
    rp_w(paste0(m$label), fs, family) + max(indent),
    rp_w(stub_lab, fs, family, bold = TRUE)
  ) + 2 * pad

  kind <- vapply(m$cols, function(c) c$kind %||% "num", character(1L))
  is_glyph <- kind %in% c("bar", "barsplit", "bardiv", "box", "pointrange",
                          "pair", "interval", "sparkline")

  # Fixed columns: the widest of the header and its cells.
  fixed <- vapply(seq_along(m$cols), function(i) {
    if (is_glyph[i]) return(NA_real_)
    c <- m$cols[[i]]
    if (identical(kind[i], "tile")) {
      return(max(rp_w(c$disp, fs, family), fs / 72 * 2.2) + 2 * pad)
    }
    txt <- if (is.null(c$pct)) c$disp else paste(c$disp, c$pct)
    max(rp_w(txt, fs, family), rp_w(c(hdr[i], sub[i]), fs, family)) + 2 * pad
  }, numeric(1L))

  # The value slot a glyph column reserves right of its lane.
  slot <- vapply(seq_along(m$cols), function(i) {
    c <- m$cols[[i]]
    if (!is_glyph[i] || is.null(c$dw)) return(0)
    c$dw * rp_ch(fs * RP_VAL_SIZE, family) + pad
  }, numeric(1L))

  # A swimlane with `size = "lg"` takes 2.2 shares (CSS: 55% against 26%).
  wt <- ifelse(is_glyph, 1, 0)
  wt[vapply(m$cols, function(c) isTRUE(c$lg), logical(1L))] <- 2.2

  rest <- width_in - stub_w - sum(fixed, na.rm = TRUE)
  share <- if (sum(wt)) rest * wt / sum(wt) else rep(0, length(wt))

  wid <- fixed
  wid[is_glyph] <- pmax(share[is_glyph],
                        lane_min + slot[is_glyph] + 2 * pad)

  list(
    stub_w = stub_w, stub_lab = stub_lab, widths = wid, slot = slot,
    is_glyph = is_glyph, kind = kind, hdr = hdr, sub = sub, pad = pad,
    x = c(0, cumsum(c(stub_w, wid)))
  )
}

# --- primitives ----------------------------------------------------------
# `gl` is the geometry list: H (picture height), W (width), px (one CSS px in
# inches), lane, hair, row_h, fs, family. `y` arguments count down from the
# top, `x` from the left, both in inches. The bare grob helpers (rp_ln,
# rp_circle, rp_seg, rp_sq) take a y already measured up from the bottom.

rp_in <- function(v) grid::unit(v, "in")

rp_txt <- function(gl, s, x, y, just = "left", size = gl$fs, col = RP_TEXT,
                   bold = FALSE, vjust = "centre", rot = 0) {
  grid::textGrob(s, x = rp_in(x), y = rp_in(gl$H - y),
                 just = c(just, vjust), rot = rot,
                 gp = grid::gpar(fontsize = size, col = col,
                                 fontfamily = gl$family,
                                 fontface = if (bold) "bold" else "plain"))
}

rp_ln <- function(x, y, col, lwd, ...) {
  grid::linesGrob(x = rp_in(x), y = rp_in(y),
                  gp = grid::gpar(col = col, lwd = lwd, ...))
}

rp_rule <- function(gl, y, col = RP_RULE, lwd = 0.6) {
  rp_ln(c(0, gl$W), rep(gl$H - y, 2L), col, lwd)
}

rp_seg <- function(x1, y1, x2, y2, col, lwd, lty) {
  grid::segmentsGrob(rp_in(x1), rp_in(y1), rp_in(x2), rp_in(y2),
                     gp = grid::gpar(col = col, lwd = lwd, lty = lty))
}

rp_circle <- function(x, y, r, fill, col, lwd) {
  grid::circleGrob(x = rp_in(x), y = rp_in(y), r = rp_in(r),
                   gp = grid::gpar(fill = fill, col = col, lwd = lwd))
}

rp_sq <- function(x, y, side, fill, just = "centre") {
  grid::rectGrob(x = rp_in(x), y = rp_in(y), width = rp_in(side),
                 height = rp_in(side), just = just,
                 gp = grid::gpar(fill = fill, col = NA))
}

rp_rect <- function(x0, w, ytop, h, fill, gl) {
  if (is.na(x0) || is.na(w)) return(NULL)
  grid::rectGrob(x = rp_in(x0), y = rp_in(gl$H - ytop),
                 width = rp_in(max(w, gl$hair)), height = rp_in(h),
                 just = c("left", "top"),
                 gp = grid::gpar(fill = fill, col = NA))
}

rp_lane_x <- function(pct, x, w) x + pct / 100 * w

# Top of a lane of height `h` centred in the row.
rp_lane_y <- function(ytop, gl, h = gl$lane) ytop + (gl$row_h - h) / 2

# A mark with a positive size (a zero draws no fill: rp_rect()'s hairline floor
# would read as a little).
rp_pos <- function(v) !is.na(v) && v > 0

# A rect from `l`% over `wd`% of the lane at x / w.
rp_lane_rect <- function(l, wd, x, w, y, h, fill, gl) {
  rp_rect(rp_lane_x(l, x, w), wd / 100 * w, y, h, fill, gl)
}

# The hairline behind a bare box or point range.
rp_bare <- function(x, w, mid, gl) {
  rp_rect(x, w, mid - gl$hair / 2, gl$hair, RP_RULE, gl)
}

# --- the marks -----------------------------------------------------------
# One function per kind, taking the cell box in inches and the row's geometry
# in percent of the lane, as the browser gets it.

rp_bar <- function(c, i, x, w, ytop, gl) {
  h <- gl$lane
  y <- rp_lane_y(ytop, gl)
  fill <- (if (length(c$fill)) c$fill[[min(i, length(c$fill))]]) %||% RP_FILL
  if (isTRUE(c$sub[[min(i, length(c$sub))]])) fill <- rp_sub(fill)
  list(rp_rect(x, w, y, h, RP_TRACK, gl),
       if (rp_pos(c$w[[i]])) rp_rect(x, c$w[[i]] / 100 * w, y, h, fill, gl))
}

rp_barsplit <- function(c, i, x, w, ytop, gl, xr = NULL) {
  if (identical(c$mode, "grouped")) {
    return(rp_barsplit_grouped(c, i, x, w, ytop, gl, xr))
  }
  h <- gl$lane
  y <- rp_lane_y(ytop, gl)
  size <- gl$fs * RP_SEG_SIZE
  out <- list(rp_rect(x, w, y, h, RP_TRACK, gl))
  at <- x
  for (j in seq_along(c$names)) {
    seg <- c$seg[[j]][[i]]
    if (!rp_pos(seg)) next
    sw <- seg / 100 * w
    # The segment's own number, inside it where it fits.
    lb <- if (!is.null(c$slab)) c$slab[[j]][[i]] else ""
    fits <- nzchar(lb) &&
      rp_w(lb, size, gl$family, bold = TRUE) + gl$px * 4 <= sw
    out <- c(out, list(
      rp_rect(at, sw, y, h, c$fills[[j]], gl),
      if (fits) {
        rp_txt(gl, lb, at + sw / 2, y + h / 2, just = "centre", size = size,
               col = c$sink[[j]], bold = TRUE)
      }
    ))
    at <- at + sw
  }
  out
}

# One 10px row per level with a value in this row, stacked in the cell. With
# value labels each level also owns a text line beside its row.
rp_barsplit_grouped <- function(c, i, x, w, ytop, gl, xr) {
  lv <- which(vapply(seq_along(c$names), function(j) rp_pos(c$segv[[j]][[i]]),
                     logical(1L)))
  h <- gl$px * 10
  lab <- !is.null(c$ldisp) && !is.null(xr)
  pitch <- rp_pitch(gl$px, gl$fs, lab, gl$px * 14)
  tot <- max(length(lv) - 1L, 0L) * pitch + h
  y0 <- ytop + (gl$row_h - tot) / 2
  unlist(lapply(seq_along(lv), function(r) {
    j <- lv[[r]]
    y <- y0 + (r - 1) * pitch
    seg <- c$seg[[j]][[i]]
    list(rp_rect(x, w, y, h, RP_TRACK, gl),
         if (rp_pos(seg)) rp_rect(x, seg / 100 * w, y, h, c$fills[[j]], gl),
         if (lab && nzchar(c$ldisp[[j]][[i]])) {
           rp_txt(gl, c$ldisp[[j]][[i]], xr, y + h / 2, just = "right",
                  size = gl$fs * RP_VAL_SIZE, col = RP_MUTED)
         })
  }), recursive = FALSE)
}

rp_bardiv <- function(c, i, x, w, ytop, gl) {
  h <- gl$lane
  y <- rp_lane_y(ytop, gl)
  mid <- x + w / 2
  ww <- c$w[[i]] / 100 * w / 2
  list(rp_rect(x, w, y, h, RP_TRACK, gl),
       if (rp_pos(c$w[[i]])) {
         rp_rect(if (isTRUE(c$pos[[i]])) mid else mid - ww, ww, y, h, RP_FILL,
                 gl)
       },
       rp_rect(mid - gl$hair / 2, gl$hair, y, h, RP_TICK, gl))
}

# Two whisker segments (never through the body), caps, a translucent IQR body
# and a solid median tick. CSS: .blockr-summarize-boxcell.
rp_box <- function(c, i, x, w, ytop, gl, fill = RP_FILL) {
  if (is.na(c$bc[[i]])) return(list())
  h <- gl$lane
  px <- gl$px
  y <- rp_lane_y(ytop, gl)
  mid <- y + h / 2
  wh <- function(l, ww) {
    if (is.na(ww)) return(NULL)
    rp_lane_rect(l, ww, x, w, mid - gl$hair / 2, gl$hair, fill, gl)
  }
  cap <- function(l, ww) {
    if (is.na(ww)) return(NULL)
    rp_rect(rp_lane_x(l, x, w) - gl$hair / 2, gl$hair, y + px * 3, h - px * 6,
            fill, gl)
  }
  list(
    if (isTRUE(c$bare)) rp_bare(x, w, mid, gl),
    wh(c$wl[[i]], c$w1[[i]]),
    wh(c$b2[[i]], c$w2[[i]]),
    cap(c$wl[[i]], c$w1[[i]]),
    cap(c$wh[[i]], c$w2[[i]]),
    if (!is.na(c$bw[[i]])) {
      rp_lane_rect(c$bl[[i]], c$bw[[i]], x, w, y + px, h - 2 * px,
                   rp_sub(fill), gl)
    },
    rp_rect(rp_lane_x(c$bc[[i]], x, w) - px, px * 2, y, h, fill, gl)
  )
}

# Fence band (a tint), inner range, ringed centre dot. The dot is a circle
# sized off the row height, never off the lane width.
rp_pr <- function(c, i, x, w, ytop, gl, fill = RP_FILL) {
  if (is.na(c$c[[i]])) return(list())
  px <- gl$px
  mid <- rp_lane_y(ytop, gl) + gl$lane / 2
  list(
    if (isTRUE(c$bare)) rp_bare(x, w, mid, gl),
    if (!is.null(c$ow) && !is.na(c$ow[[i]])) {
      rp_lane_rect(c$ol[[i]], c$ow[[i]], x, w, mid - px * 4, px * 8,
                   rp_faint(fill), gl)
    },
    if (!is.na(c$rw[[i]])) {
      rp_lane_rect(c$l[[i]], c$rw[[i]], x, w, mid - px * 2, px * 4, fill, gl)
    },
    rp_circle(rp_lane_x(c$c[[i]], x, w), gl$H - mid, px * 4, fill, "#ffffff",
              1.2)
  )
}

# The dumbbell: band, reference line, link, a hollow diamond at `from` and a
# dot at `to`, open when it falls outside the band.
rp_pair <- function(c, i, x, w, ytop, gl) {
  h <- gl$lane
  y <- rp_lane_y(ytop, gl)
  mid <- y + h / 2
  fill <- c$fill[[i]]
  if (is.na(fill)) fill <- RP_FILL
  lx <- function(p) rp_lane_x(p, x, w)
  # A thinned split lane (three or more levels) takes smaller marks.
  r <- min(gl$px * 4, h * 0.45)
  list(
    if (!is.na(c$bw[[i]])) {
      rp_lane_rect(c$bl[[i]], c$bw[[i]], x, w, y, h, "#eeede8", gl)
    },
    if (!is.na(c$rf)) {
      rp_seg(lx(c$rf), gl$H - ytop, lx(c$rf), gl$H - ytop - gl$row_h,
             "#8d8b84", 0.8, "dashed")
    },
    if (!is.na(c$w[[i]])) {
      x1 <- lx(c$l[[i]])
      rp_seg(x1, gl$H - mid, x1 + c$w[[i]] / 100 * w, gl$H - mid, fill, 1.6,
             if (isTRUE(c$dash[[i]])) "dashed" else "solid")
    },
    if (!is.na(c$a[[i]])) {
      grid::polygonGrob(
        x = rp_in(lx(c$a[[i]]) + c(0, r, 0, -r)),
        y = rp_in(gl$H - mid + c(r, 0, -r, 0)),
        gp = grid::gpar(fill = "#ffffff", col = fill, lwd = 1.2))
    },
    if (!is.na(c$b[[i]])) {
      rp_circle(lx(c$b[[i]]), gl$H - mid, r,
                if (isTRUE(c$open[[i]])) "#ffffff" else fill, fill, 1.2)
    }
  )
}

# The swimlane. Segments are [left%, width%, fill index] on the column's
# shared x domain; each is at least 2px wide so a one-day event stays visible.
rp_interval <- function(c, i, x, w, ytop, gl) {
  y <- rp_lane_y(ytop, gl)
  lapply(c$segs[[i]], function(sg) {
    rp_rect(rp_lane_x(sg[[1L]], x, w),
            max(sg[[2L]] / 100 * w, gl$px * 2), y, gl$lane,
            c$fills[[sg[[3L]]]], gl)
  })
}

# Parse a pre-printed SVG point string ("x,y x,y ...", viewBox 0 0 100 36,
# y downward) into a matrix, so the slide shows the trajectory the screen drew.
rp_sp_pts <- function(s) {
  if (!length(s) || is.na(s) || !nzchar(s)) return(NULL)
  p <- do.call(rbind, lapply(strsplit(strsplit(s, " ", fixed = TRUE)[[1L]],
                                      ",", fixed = TRUE),
                             as.numeric))
  if (!is.matrix(p) || nrow(p) < 1L) NULL else p
}

rp_sparkline <- function(c, i, x, w, ytop, gl, fill = RP_FILL) {
  h <- gl$lane
  y <- rp_lane_y(ytop, gl)
  X <- function(v) x + v / 100 * w
  Y <- function(v) gl$H - (y + v / 36 * h)
  bd <- rp_sp_pts(c$bd[[i]])
  pl <- rp_sp_pts(c$pl[[i]])
  list(
    if (!is.na(c$rby)) {
      rp_rect(x, w, y + c$rby / 36 * h, c$rbh / 36 * h, rp_faint(fill), gl)
    },
    if (!is.null(bd)) {
      grid::polygonGrob(x = rp_in(X(bd[, 1L])), y = rp_in(Y(bd[, 2L])),
                        gp = grid::gpar(fill = RP_TRACK, col = NA))
    },
    if (!is.na(c$rc)) {
      rp_ln(c(x, x + w), rep(Y(c$rc), 2L), RP_TICK, 0.8, lty = "22")
    },
    if (!is.null(pl) && nrow(pl) > 1L) {
      rp_ln(X(pl[, 1L]), Y(pl[, 2L]), fill, 1.3, lineend = "round",
            linejoin = "round")
    },
    if (!is.na(c$dx[[i]])) {
      rp_circle(X(c$dx[[i]]), gl$H - (y + c$dy[[i]] / 100 * h), gl$px * 3,
                fill, "#ffffff", 1)
    }
  )
}

# A colour-split cell stacks only the levels the row has, so a row of one
# level draws one full-height lane.
rp_multi <- function(c, i, x, w, ytop, gl, xr = NULL) {
  drawn <- rp_multi_drawn(c, i)
  k <- length(drawn)
  if (!k) return(list())
  lab <- !is.null(c$dw) && !is.null(xr)
  h <- gl$lane
  pitch <- rp_pitch(gl$px, gl$fs, lab, gl$px * 12 + gl$px * 2)
  tot <- (k - 1) * pitch + h
  y0 <- ytop + (gl$row_h - tot) / 2
  unlist(lapply(seq_len(k), function(jj) {
    lv <- c$lv[[drawn[[jj]]]]
    yy <- y0 + (jj - 1) * pitch - (gl$row_h - h) / 2
    marks <- if (identical(c$kind, "pair")) {
      lv$rf <- c$rf
      rp_pair(lv, i, x, w, yy, gl)
    } else if (identical(c$kind, "box")) {
      rp_box(lv, i, x, w, yy, gl, lv$fill %||% RP_FILL)
    } else {
      rp_pr(lv, i, x, w, yy, gl, lv$fill %||% RP_FILL)
    }
    c(marks, if (lab && !is.null(lv$disp) && nzchar(lv$disp[[i]])) {
      list(rp_txt(gl, lv$disp[[i]], xr, yy + gl$row_h / 2, just = "right",
                  size = gl$fs * RP_VAL_SIZE, col = RP_MUTED))
    })
  }), recursive = FALSE)
}

# The levels a colour-split cell draws in row i: the ones the row has.
rp_multi_drawn <- function(c, i) {
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
rp_pitch <- function(px, fs, lab, h) {
  if (lab) max(h, fs * RP_VAL_SIZE / 72 * 1.25) else h
}

# Every row's height: the table's row height, grown where a colour-split
# column has more levels than that holds.
rp_row_heights <- function(m, row_h, px, fs) {
  out <- rep(row_h, m$n)
  rows <- seq_len(m$n)
  for (c in m$cols) {
    if (identical(c$kind, "barsplit") && identical(c$mode, "grouped")) {
      k <- vapply(rows, function(r) {
        sum(vapply(c$segv, function(x) rp_pos(x[[r]]), logical(1L)))
      }, numeric(1L))
      need <- (k - 1) * rp_pitch(px, fs, !is.null(c$ldisp), px * 14) +
        px * 10 + px * 8
    } else if (isTRUE(c$multi)) {
      k <- vapply(rows, function(r) length(rp_multi_drawn(c, r)), numeric(1L))
      need <- (k - 1) *
        rp_pitch(px, fs, !is.null(c$dw), px * 12 + px * 2) + px * 12 + px * 8
    } else {
      next
    }
    out <- ifelse(k >= 2L, pmax(out, need), out)
  }
  out
}

# The domain printed once per glyph column, from the numbers the marks were
# scaled with.
rp_axis <- function(p, prep, x, w, ytop, gl) {
  dom <- summarize_axis_domain(p, prep)
  if (is.null(dom)) return(list())
  t <- if (isTRUE(dom$date)) {
    summarize_axis_date_ticks(dom$d0, dom$d1)
  } else {
    at <- summarize_axis_ticks(
      dom$d0, dom$d1,
      k = if (identical(p$kind, "interval")) 7L else 4L
    )
    list(at = at, labels = lane_fmt(at))
  }
  if (length(t$at) < 2L) return(list())
  last <- length(t$at)
  if (isTRUE(dom$pct)) t$labels[[last]] <- paste0(t$labels[[last]], "%")
  pct <- (t$at - dom$d0) / (dom$d1 - dom$d0) * 100
  lapply(seq_len(last), function(k) {
    just <- if (pct[[k]] <= 8) "left" else if (pct[[k]] >= 92) "right" else "centre"
    rp_txt(gl, t$labels[[k]], rp_lane_x(pct[[k]], x, w), ytop, just = just,
           vjust = "top", size = gl$fs * 0.78, col = RP_MUTED)
  })
}

# --- the vertical budget -------------------------------------------------
# Every band's height. The painter lays the picture out from it and the pager
# decides how many rows a slide holds from it.
rp_legend_spec <- function(prep) {
  tryCatch(summarize_legend_spec(prep), error = function(e) NULL)
}

rp_has <- function(s) !is.null(s) && nzchar(s)

rp_heights <- function(m, prep, lay, fs, row_h = NULL, title = NULL,
                       subtitle = NULL, caption = NULL) {

  # One CSS px in inches, tied to the type size (13px type on a 12px lane).
  px <- (fs / 72) / 13
  # A sparkline column claims 40px against 12 for the other marks.
  has_sp <- any(vapply(m$cols, function(c) identical(c$kind, "sparkline"),
                       logical(1L)))
  row_h <- row_h %||% max(fs / 72 * 1.9, px * 22, if (has_sp) px * 44 else 0)

  line_h <- fs / 72 * 1.5
  # The second header tier is present when some column or the stub carries one.
  sub_line <- if (any(nzchar(c(lay$sub, prep$group_label %||% "")))) {
    fs / 72 * 1.25
  } else {
    0
  }
  span_off <- if (is.null(prep$facet_spans)) 0 else line_h
  axis_h <- fs / 72 * 1.1

  # Tile titles stand on their side: the band grows by the longest one.
  tile <- lay$kind == "tile"
  rot_h <- if (any(tile)) {
    min(1.8, max(rp_w(rp_tile_title(lay$hdr[tile]), fs * 0.85,
                      lay$family %||% "sans")) + 0.08)
  } else {
    0
  }
  h <- list(
    px = px, row_h = row_h, rows = rp_row_heights(m, row_h, px, fs),
    line_h = line_h, sub_line = sub_line,
    span_off = span_off, axis_h = axis_h, rot_h = rot_h,
    head_h = span_off + line_h + sub_line + axis_h + 0.06 + rot_h,
    title_h = if (rp_has(title)) fs / 72 * 2.4 else 0,
    sub_h = if (rp_has(subtitle)) fs / 72 * 1.7 else 0,
    cap_h = if (rp_has(caption)) fs / 72 * 1.8 else 0,
    leg_h = if (length(rp_legend_spec(prep)$groups) > 0L) fs / 72 * 1.9 else 0,
    fold_h = if (is.null(m$fold)) 0 else row_h
  )
  # Everything that is not rows: what a page spends before its first row.
  h$chrome <- h$title_h + h$sub_h + h$leg_h + h$head_h + h$cap_h + 0.08
  h
}

# --- the picture ---------------------------------------------------------
# The colour legend, under the subtitle (on screen it lives in the toolbar).
rp_legend <- function(prep, y, hh, gl) {
  leg <- rp_legend_spec(prep)
  fs <- gl$fs
  px <- gl$px
  sw <- px * 9
  at <- 0
  ly <- y + hh$leg_h / 2
  g <- list()
  for (grp in leg$groups) {
    if (nzchar(grp$title %||% "")) {
      g <- c(g, list(rp_txt(gl, grp$title, at, ly, size = fs * 0.8,
                            col = RP_MUTED)))
      at <- at + rp_w(grp$title, fs * 0.8, gl$family) + px * 6
    }
    for (it in grp$items) {
      g <- c(g, list(rp_sq(at, gl$H - ly, sw, it$color, c("left", "centre")),
                     rp_txt(gl, it$label, at + sw + px * 3, ly,
                            size = fs * 0.8)))
      at <- at + sw + px * 3 + rp_w(it$label, fs * 0.8, gl$family) + px * 10
    }
    at <- at + px * 8
  }
  g
}

# Facet spanners: one label over its columns, with a rule.
rp_spans <- function(spans, lay, hh, hy, gl) {
  fs <- gl$fs
  at <- spans$lead
  g <- list()
  for (grp in spans$groups) {
    x0 <- lay$x[at + 2]
    x1 <- lay$x[at + 1 + grp$n + 1]
    # The level's N sits beside the level, muted; the pair is centred.
    ny <- hy + hh$line_h * 0.5
    nlab <- if (!is.null(grp$pop)) paste0("N = ", grp$pop)
    lw <- rp_w(grp$label, fs * 0.95, gl$family, bold = TRUE)
    gap <- if (is.null(nlab)) 0 else rp_ch(fs * 0.8, gl$family)
    nw <- if (is.null(nlab)) 0 else rp_w(nlab, fs * 0.8, gl$family)
    lx <- (x0 + x1) / 2 - (lw + gap + nw) / 2
    ry <- gl$H - hy - hh$line_h * 0.92
    g <- c(g, list(
      rp_txt(gl, grp$label, lx, ny, bold = TRUE, size = fs * 0.95),
      if (!is.null(nlab)) {
        rp_txt(gl, nlab, lx + lw + gap, ny, size = fs * 0.8, col = RP_MUTED)
      },
      rp_ln(c(x0 + lay$pad, x1 - lay$pad), c(ry, ry), RP_TICK, 0.6)
    ))
    at <- at + grp$n
  }
  g
}

# The header band from `hy`: [spanner] / leaf label / second tier / axis strip.
rp_header <- function(m, prep, lay, hh, hy, gl) {
  fs <- gl$fs
  lab_y <- hy + hh$span_off
  sub_y <- lab_y + hh$line_h
  axis_y <- sub_y + hh$sub_line + 0.02
  lab <- function(s, x, just = "left") {
    rp_txt(gl, s, x, lab_y, just = just, bold = TRUE, size = fs * 0.95,
           vjust = "top")
  }
  sub <- function(s, x, just = "left") {
    rp_txt(gl, s, x, sub_y, just = just, size = fs * 0.8, col = RP_MUTED,
           vjust = "top")
  }
  g <- list(lab(lay$stub_lab, lay$pad),
            if (hh$sub_line && nzchar(prep$group_label %||% "")) {
              sub(prep$group_label, lay$pad)
            })

  for (i in seq_along(m$cols)) {
    x0 <- lay$x[i + 1]
    w <- lay$widths[i]
    glyph <- lay$is_glyph[i]
    just <- if (glyph) "left" else "right"
    xx <- if (glyph) x0 + lay$pad else x0 + w - lay$pad
    if (identical(lay$kind[i], "tile")) {
      # On its side, reading bottom-up, its foot on the header rule.
      g <- c(g, list(rp_txt(gl, rp_tile_title(lay$hdr[i]), x0 + w / 2,
                            hy + hh$head_h - 0.06, size = fs * 0.85,
                            rot = 90)))
      next
    }
    g <- c(g, list(
      lab(lay$hdr[i], xx, just),
      if (hh$sub_line && nzchar(lay$sub[i])) sub(lay$sub[i], xx, just)
    ), if (glyph) {
      rp_axis(prep$plan[[i]], prep, x0 + lay$pad,
              w - lay$slot[i] - 2 * lay$pad, axis_y, gl)
    })
  }
  if (is.null(prep$facet_spans)) return(g)
  c(g, rp_spans(prep$facet_spans, lay, hh, hy, gl))
}

# The marks of one cell; `k` is the column kind.
rp_cell_marks <- function(c, k, r, x0, lane_w, xr, ytop, gl) {
  if (isTRUE(c$multi)) return(rp_multi(c, r, x0, lane_w, ytop, gl, xr = xr))
  switch(
    k,
    bar = rp_bar(c, r, x0, lane_w, ytop, gl),
    barsplit = rp_barsplit(c, r, x0, lane_w, ytop, gl, xr = xr),
    bardiv = rp_bardiv(c, r, x0, lane_w, ytop, gl),
    box = rp_box(c, r, x0, lane_w, ytop, gl),
    pointrange = rp_pr(c, r, x0, lane_w, ytop, gl),
    pair = rp_pair(c, r, x0, lane_w, ytop, gl),
    interval = rp_interval(c, r, x0, lane_w, ytop, gl),
    sparkline = {
      gl$lane <- gl$px * 36
      rp_sparkline(c, r, x0, lane_w, ytop, gl)
    },
    NULL
  )
}

# Row `r` from `ytop`, `rh` high (a row grown for per-level numbers centres
# everything in it): the label, each cell's marks and value, the rule below.
rp_body_row <- function(m, lay, r, ytop, rh, gl) {
  fs <- gl$fs
  gl$row_h <- rh
  ind <- if (m$level[[r]] > 0L) RP_INDENT else 0
  g <- list(rp_txt(gl, m$label[[r]], lay$pad + ind, ytop + rh / 2,
                   bold = isTRUE(m$parent_row[[r]])))
  for (i in seq_along(m$cols)) {
    c <- m$cols[[i]]
    w <- lay$widths[i]
    k <- lay$kind[i]
    xr <- lay$x[i + 1] + w - lay$pad
    g <- c(g, rp_cell_marks(c, k, r, lay$x[i + 1] + lay$pad,
                            w - lay$slot[i] - 2 * lay$pad, xr, ytop, gl))
    if (identical(k, "tile")) {
      if (!is.na(c$bg[[r]])) {
        cx <- lay$x[i + 1] + w / 2
        g <- c(g, list(
          rp_sq(cx, gl$H - ytop - rh / 2, min(rh * 0.82, w - 2 * lay$pad),
                c$bg[[r]]),
          rp_txt(gl, c$disp[[r]], cx, ytop + rh / 2, just = "centre",
                 size = fs * 0.85, col = c$fg[[r]])
        ))
      }
      next
    }
    # The value: glyph columns print it grey to the right of the lane, number
    # columns are the cell.
    if (!is.null(c$disp)) {
      s <- c$disp[[min(r, length(c$disp))]]
      p <- if (is.null(c$pct)) "" else c$pct[[min(r, length(c$pct))]]
      glyph <- lay$is_glyph[i]
      g <- c(g, list(rp_txt(gl, trimws(paste(s, p)), xr, ytop + rh / 2,
                            just = "right",
                            size = if (glyph) fs * RP_VAL_SIZE else fs,
                            col = if (glyph) RP_MUTED else RP_TEXT)))
    }
  }
  c(g, list(rp_rule(gl, ytop + rh)))
}

summarize_paint_grob <- function(m, prep, width_in = 12.5, fs = 9,
                                 family = "sans", row_h = NULL, title = NULL,
                                 subtitle = NULL, caption = NULL) {

  lay <- rp_layout(m, prep, width_in, fs = fs, family = family)
  hh <- rp_heights(m, prep, lay, fs, row_h, title, subtitle, caption)
  H <- hh$chrome + sum(hh$rows) + hh$fold_h
  gl <- list(H = H, W = width_in, px = hh$px, lane = hh$px * 12, hair = hh$px,
             row_h = hh$row_h, fs = fs, family = family)

  # Tops of the title, subtitle, legend, header and body bands.
  ys <- cumsum(c(0, hh$title_h, hh$sub_h, hh$leg_h, hh$head_h))
  ytops <- ys[[5]] + cumsum(c(0, hh$rows))[seq_len(m$n)]
  y_end <- ys[[5]] + sum(hh$rows)

  g <- c(
    list(
      if (hh$title_h) {
        rp_txt(gl, title, 0, ys[[1]] + hh$title_h * 0.55, size = fs * 1.25,
               bold = TRUE)
      },
      if (hh$sub_h) {
        rp_txt(gl, subtitle, 0, ys[[2]] + hh$sub_h * 0.45, size = fs * 0.95,
               col = RP_MUTED)
      }
    ),
    rp_legend(prep, ys[[3]], hh, gl),
    rp_header(m, prep, lay, hh, ys[[4]], gl),
    list(rp_rule(gl, ys[[5]], col = "#8d8b84", lwd = 0.9)),
    unlist(lapply(seq_len(m$n), function(r) {
      rp_body_row(m, lay, r, ytops[[r]], hh$rows[[r]], gl)
    }), recursive = FALSE),
    # `top_n` never truncates silently: the fold row says what fell below it.
    if (!is.null(m$fold)) {
      list(rp_txt(gl, m$fold, lay$pad, y_end + hh$row_h / 2, size = fs * 0.9,
                  col = RP_MUTED),
           rp_rule(gl, y_end + hh$row_h))
    },
    if (hh$cap_h) {
      list(rp_txt(gl, caption, 0, y_end + hh$fold_h + hh$cap_h * 0.55,
                  size = fs * 0.85, col = RP_MUTED))
    }
  )

  list(
    grob = grid::gTree(
      children = do.call(grid::gList, Filter(Negate(is.null), g))),
    width = width_in, height = H
  )
}

# --- paging --------------------------------------------------------------
# A picture cannot reflow, so a table too tall for its slide is cut on the
# cell model before painting; a slice of rows is a valid table.

# Slice a column's per-row vectors and leave its per-column ones alone. A
# vector is per-row when its length is n. `seg`, `segv`, `ldisp` and `slab`
# hold one full-length vector per series, and `lv` one nested cell per colour
# level.
rp_slice_col <- function(c, idx, n) {
  nm <- names(c)
  out <- c
  for (k in seq_along(c)) {
    v <- c[[k]]
    if (nm[[k]] %in% c("seg", "segv", "ldisp", "slab")) {
      out[[k]] <- lapply(v, function(s) if (length(s) == n) s[idx] else s)
    } else if (identical(nm[[k]], "lv")) {
      out[[k]] <- lapply(v, rp_slice_col, idx = idx, n = n)
    } else if (length(v) == n && (is.atomic(v) || is.list(v))) {
      out[[k]] <- v[idx]
    }
  }
  out
}

rp_slice <- function(m, idx) {
  n <- m$n
  m$cols <- lapply(m$cols, rp_slice_col, idx = idx, n = n)
  for (k in c("label", "parent", "level", "parent_row", "on")) {
    if (length(m[[k]]) == n) m[[k]] <- m[[k]][idx]
  }
  m$n <- length(idx)
  m
}

# Where to cut. With uniform rows the count follows from `per_page`; with
# `heights` a page takes rows while they fit `budget`. A page never ends on a
# parent row, and a page that opens mid-group repeats the parent row, marked
# as carried over (`attr(, "carried")`).
rp_page_rows <- function(m, per_page, heights = NULL, budget = NULL) {
  n <- m$n
  if (per_page >= n) return(list(seq_len(n)))
  fit <- function(from, room) {
    if (is.null(heights)) return(room)
    used <- cumsum(heights[seq.int(from, n)])
    max(1L, sum(used <= room + 1e-9))
  }
  lvl <- as.integer(m$level %||% rep(0L, n))
  par <- as.logical(m$parent_row %||% rep(FALSE, n))
  pages <- list()
  at <- 1L
  while (at <= n) {
    # A carried-over parent costs a slot, so it is decided before the cut:
    # dropping a row afterwards can move the cursor backwards.
    lead <- integer()
    take <- fit(at, if (is.null(heights)) per_page else budget)
    if (at > 1L && lvl[[at]] > 0L && take >= 2L) {
      p <- at - 1L
      while (p >= 1L && lvl[[p]] > 0L) p <- p - 1L
      if (p >= 1L) {
        lead <- p
        take <- if (is.null(heights)) {
          per_page - 1L
        } else {
          fit(at, budget - heights[[p]])
        }
      }
    }
    end <- min(at + take - 1L, n)
    # Never below `at`, so the cursor always advances.
    while (end > at && isTRUE(par[[end]])) end <- end - 1L
    idx <- c(lead, seq.int(at, end))
    attr(idx, "carried") <- length(lead) > 0L
    pages[[length(pages) + 1L]] <- idx
    at <- end + 1L
  }
  pages
}

# How many rows fit one page at this type size, without painting anything.
summarize_paint_per_page <- function(m, prep, width_in = 12.5, max_height = 5.4,
                                     fs = 9, family = "sans", row_h = NULL,
                                     title = NULL, subtitle = NULL,
                                     caption = NULL) {

  lay <- rp_layout(m, prep, width_in, fs = fs, family = family)
  hh <- rp_heights(m, prep, lay, fs, row_h, title, subtitle, caption)

  # A page marker can push a one-line title to two, so budget for the chrome
  # of a continuation page, which is never less than page 1's.
  budget <- max_height - hh$chrome - hh$fold_h

  if (all(hh$rows == hh$row_h)) return(max(1L, floor(budget / hh$row_h)))
  max(1L, sum(cumsum(hh$rows) <= budget + 1e-9))
}

# One table as a list of pictures. `max_height` is the slide's body box. The
# title grows a "(2 of 7)" marker and the fold row rides on the last page.
summarize_paint_pages <- function(m, prep, width_in = 12.5, max_height = 5.4,
                                  fs = 9, family = "sans", row_h = NULL,
                                  title = NULL, subtitle = NULL,
                                  caption = NULL) {

  paint <- function(mk, ttl, sub, cap) {
    summarize_paint_grob(mk, prep, width_in, fs, family, row_h, ttl, sub, cap)
  }
  per_page <- summarize_paint_per_page(m, prep, width_in, max_height, fs,
                                       family, row_h, title, subtitle, caption)
  if (per_page >= m$n) return(list(paint(m, title, subtitle, caption)))

  lay <- rp_layout(m, prep, width_in, fs = fs, family = family)
  hh <- rp_heights(m, prep, lay, fs, row_h, title, subtitle, caption)
  pages <- rp_page_rows(
    m, per_page,
    heights = if (all(hh$rows == hh$row_h)) NULL else hh$rows,
    budget = max_height - hh$chrome - hh$fold_h
  )
  n_pg <- length(pages)
  lapply(seq_along(pages), function(k) {
    idx <- pages[[k]]
    mk <- rp_slice(m, as.integer(idx))
    if (k < n_pg) mk$fold <- NULL
    ttl <- if (rp_has(title)) paste0(title, "  (", k, " of ", n_pg, ")") else title
    sub <- if (isTRUE(attr(idx, "carried")) && !is.null(subtitle)) {
      paste0(subtitle, " · continued")
    } else {
      subtitle
    }
    paint(mk, ttl, sub, if (k == n_pg) caption)
  })
}

# --- output --------------------------------------------------------------
# grid and systemfonts are Suggests, like flextable and officer on the other
# pptx path; ragg is an import.
RP_SUGGESTED <- c("grid", "systemfonts")

#' @noRd
summarize_paint_ready <- function() {
  all(vapply(RP_SUGGESTED, requireNamespace, logical(1L), quietly = TRUE))
}

#' @noRd
summarize_paint_require <- function() {
  miss <- Filter(function(p) !requireNamespace(p, quietly = TRUE),
                 RP_SUGGESTED)
  if (length(miss)) {
    stop("Painting a summarize table needs: ", paste(miss, collapse = ", "),
         ".", call. = FALSE)
  }
  invisible(TRUE)
}

# One painted page as an image an rmarkdown / knitr chunk can print.
#' @noRd
summarize_paint_image <- function(x, width_in = 9, res = 300, ...) {
  summarize_paint_require()
  p <- summarize_paint_grob(x$cells, x$prep, width_in = width_in,
                            title = x$title, subtitle = x$subtitle,
                            caption = x$caption, ...)
  f <- tempfile(fileext = ".png")
  rp_write_png(p, f, res = res)
  htmltools::tags$img(src = knitr::image_uri(f),
                      style = paste0("width:", p$width, "in;max-width:100%"))
}

rp_write_png <- function(p, file, res = 300) {
  ragg::agg_png(file, width = p$width, height = p$height, units = "in",
                res = res, background = "white")
  on.exit(grDevices::dev.off())
  grid::grid.newpage()
  grid::grid.draw(p$grob)
  list(file = file, width = p$width, height = p$height)
}

# A tile column's title as printed on its side: long terms end in an ellipsis.
rp_tile_title <- function(x, max = 28L) {
  x <- as.character(x)
  ifelse(nchar(x) > max, paste0(substr(x, 1L, max - 1L), "…"), x)
}
