# Paint a summarize table as ONE grid picture ---------------------------------
#
# The third consumer of rank_cells(), beside the markup (rank_cells_html) and
# the JSON payload (rank_flat_payload): the same cell model drawn with grid,
# for the formats that cannot hold the HTML one.
#
# Why a picture and not a table. PowerPoint's DrawingML table cell (`a:tc`)
# holds text runs and nothing else, so a glyph can never live in a table cell
# there: both flextable routes (`grid_chunk()`, `as_image()`) write ZERO media
# into a pptx and the cells come out silently empty (docx does support it --
# `w:tc` takes a `w:drawing` -- which is why the same call works there). So on
# a slide the exhibit is one picture, header band and numbers and marks
# together. The format decides this, not taste.
#
# What that costs, stated once: the text becomes an image. Not selectable, not
# searchable, not editable in PowerPoint. That is why this route exists only
# for the GLYPH table -- a plain display table keeps static_table(), which
# writes real cells.
#
# This is a LEAF consumer: read-only, export-only, no JS twin, and nothing on
# the block's render path calls it. A board that never exports never runs a
# line of it, which is what keeps the marks' one source of truth (rank_cells)
# the only thing the two live consumers share.
#
# Coordinates: everything is computed in INCHES from the top-left, and the
# vertical constants are the CSS ones (12px lane, 1px whisker, 3px cap inset,
# 2px median, 8px fence / dot) scaled by one `px` factor, so the painted
# glyph keeps the on-screen proportions at any size.
#
# Needs grid + systemfonts to measure and lay out, ragg to rasterise. All
# Suggests: the callers check before they call (see summarize-exhibit.R).


# --- colours -------------------------------------------------------------
# The cascade does not exist in an export, so every token is resolved to a
# literal here. Values from R/rank-table-css.R.
RP_FILL  <- "#2a78d6"                      # --blockr-rank-fill
RP_TRACK <- "#dad9d5"                      # mix(#eeeeea 80%, #898781)
RP_TICK  <- "#c3c2b7"                      # --blockr-rank-tick
RP_RULE  <- "#e1e0d9"                      # --blockr-color-border, hairlines
RP_TEXT  <- "#1c1b18"
RP_MUTED <- "#6f6d66"

rp_sub <- function(col) grDevices::adjustcolor(col, alpha.f = 0.45)
rp_faint <- function(col) grDevices::adjustcolor(col, alpha.f = 0.16)

# --- measuring -----------------------------------------------------------
# The size some faces cannot be measured at, and one they can. freetype
# refuses macOS's Helvetica.ttc below 8pt ("freetype error 133") and "sans"
# resolves straight to it, so the shrink loop -- which walks down to
# min_font_size, as low as 5 -- measures its way into an error on any Mac.
# Advances scale linearly with size for a scalable face, so where the wanted
# size will not measure, measure at one that will and scale the answer. Per-
# size rounding makes that read a few percent wide, which is the safe
# direction: a column believed wider than it is never overflows.
RP_W_FLOOR <- 8
RP_W_REF <- 12

rp_string_width <- function(txt, fs, family, bold) {
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
    w <- if (is.null(ref)) NULL else ref * (fs / RP_W_REF)
  }
  w
}

rp_w <- function(txt, fs, family = "sans", bold = FALSE) {
  if (!length(txt)) return(0)
  txt <- txt[!is.na(txt)]
  if (!length(txt)) return(0)
  txt <- as.character(txt)
  w <- rp_string_width(txt, fs, family, bold)
  w <- w[is.finite(w)]
  # Nothing measurable at any size: a half-em per character is coarse, but a
  # layout built on -Inf is not a layout.
  if (!length(w)) return(max(nchar(txt)) * fs * 0.5 / 72)
  max(w) / 72
}

rp_ch <- function(fs, family = "sans") rp_w("0", fs, family)

# --- layout --------------------------------------------------------------
#
# Widths follow the CSS intent rather than its mechanism: the stub and the
# number columns shrink to fit, the GLYPH columns own the slack (that is the
# `width:1%` idiom in rank-table-css.R). Each glyph column keeps its label
# slot (`dw` characters, one width per column) so the lanes start and end at
# the same x on every row and stay comparable.
rp_layout <- function(m, prep, width_in, fs = 9, family = "sans",
                      pad = 0.06, lane_min = 0.9) {

  plan <- prep$plan
  hdr <- vapply(plan, function(p) p$label %||% "", character(1L))
  sub <- vapply(plan, function(p) p$sub_label %||% "", character(1L))

  stub_lab <- rank_label_header(prep)
  indent <- ifelse(m$level > 0L, 0.28, 0)
  stub_w <- max(
    rp_w(paste0(strrep(" ", 0), m$label), fs, family) + max(indent),
    rp_w(stub_lab, fs, family, bold = TRUE)
  ) + 2 * pad

  kind <- vapply(m$cols, function(c) c$kind %||% "num", character(1L))
  is_glyph <- kind %in% c("bar", "barsplit", "bardiv", "box", "pointrange",
                          "interval", "sparkline")

  # Fixed columns: the widest of the header and its cells.
  fixed <- vapply(seq_along(m$cols), function(i) {
    if (is_glyph[i]) return(NA_real_)
    c <- m$cols[[i]]
    txt <- if (is.null(c$pct)) c$disp else paste(c$disp, c$pct)
    max(rp_w(txt, fs, family), rp_w(c(hdr[i], sub[i]), fs, family)) + 2 * pad
  }, numeric(1L))

  # The value slot each glyph column reserves to the right of its lane.
  slot <- vapply(seq_along(m$cols), function(i) {
    c <- m$cols[[i]]
    if (!is_glyph[i] || is.null(c$disp)) return(0)
    (c$dw %||% 1) * rp_ch(fs, family) + pad
  }, numeric(1L))

  # A swimlane marked `size = "lg"` is the centerpiece of its table and the
  # CSS gives it 55% against the other glyphs' 26%. Weighting the share is
  # the same statement: horizontal resolution is what a span column is for.
  wt <- ifelse(is_glyph, 1, 0)
  wt[vapply(m$cols, function(c) isTRUE(c$lg), logical(1L))] <- 2.2

  rest <- width_in - stub_w - sum(fixed, na.rm = TRUE)
  share <- if (sum(wt)) rest * wt / sum(wt) else rep(0, length(wt))

  wid <- fixed
  wid[is_glyph] <- pmax(share[is_glyph],
                        lane_min + slot[is_glyph] + 2 * pad)

  list(
    stub_w = stub_w, stub_lab = stub_lab, widths = wid, slot = slot,
    is_glyph = is_glyph, kind = kind, hdr = hdr, sub = sub,
    indent = indent, pad = pad,
    x = c(0, cumsum(c(stub_w, wid)))
  )
}

# --- the marks -----------------------------------------------------------
#
# One helper per kind, each taking the cell box in inches and the row's
# geometry in PERCENT of the lane -- the same percentages the browser gets,
# never re-derived here (see the drift note at the top of R/rank-push.R).

rp_rect <- function(x0, w, ytop, h, fill, gl, alpha = NULL) {
  if (is.na(x0) || is.na(w)) return(NULL)
  grid::rectGrob(x = grid::unit(x0, "in"), y = grid::unit(gl$H - ytop, "in"),
           width = grid::unit(max(w, gl$hair), "in"), height = grid::unit(h, "in"),
           just = c("left", "top"),
           gp = grid::gpar(fill = fill, col = NA))
}

rp_lane_x <- function(pct, x, w) x + pct / 100 * w

rp_bar <- function(c, i, x, w, ytop, gl) {
  h <- gl$lane
  y <- ytop + (gl$row_h - h) / 2
  fill <- c$fill[[min(i, length(c$fill))]] %||% RP_FILL
  if (!length(c$fill)) fill <- RP_FILL
  if (isTRUE(c$sub[[min(i, length(c$sub))]])) fill <- rp_sub(fill)
  out <- list(rp_rect(x, w, y, h, RP_TRACK, gl))
  if (!is.na(c$w[[i]])) {
    out <- c(out, list(rp_rect(x, c$w[[i]] / 100 * w, y, h, fill, gl)))
  }
  out
}

rp_barsplit <- function(c, i, x, w, ytop, gl) {
  k <- length(c$names)
  grouped <- identical(c$mode, "grouped")
  if (!grouped) {
    h <- gl$lane
    y <- ytop + (gl$row_h - h) / 2
    out <- list(rp_rect(x, w, y, h, RP_TRACK, gl))
    at <- x
    for (j in seq_len(k)) {
      seg <- c$seg[[j]][[i]]
      if (is.na(seg) || seg <= 0) next
      out <- c(out, list(rp_rect(at, seg / 100 * w, y, h, c$fills[[j]], gl)))
      at <- at + seg / 100 * w
    }
    return(out)
  }
  # grouped: one thin row per level, stacked inside the cell
  h <- gl$px * 6
  gap <- gl$px * 2
  tot <- k * h + (k - 1) * gap
  y0 <- ytop + (gl$row_h - tot) / 2
  unlist(lapply(seq_len(k), function(j) {
    y <- y0 + (j - 1) * (h + gap)
    seg <- c$seg[[j]][[i]]
    list(rp_rect(x, w, y, h, RP_TRACK, gl),
         if (!is.na(seg) && seg > 0) {
           rp_rect(x, seg / 100 * w, y, h, c$fills[[j]], gl)
         })
  }), recursive = FALSE)
}

rp_bardiv <- function(c, i, x, w, ytop, gl) {
  h <- gl$lane
  y <- ytop + (gl$row_h - h) / 2
  mid <- x + w / 2
  out <- list(rp_rect(x, w, y, h, RP_TRACK, gl))
  if (!is.na(c$w[[i]])) {
    ww <- c$w[[i]] / 100 * w / 2
    out <- c(out, list(
      if (isTRUE(c$pos[[i]])) rp_rect(mid, ww, y, h, RP_FILL, gl)
      else rp_rect(mid - ww, ww, y, h, RP_FILL, gl)
    ))
  }
  c(out, list(rp_rect(mid - gl$hair / 2, gl$hair, y, h, RP_TICK, gl)))
}

# Box: two whisker segments (never through the body), caps, translucent IQR
# body, solid median tick. CSS: .blockr-rank-boxcell in R/rank-table-css.R.
rp_box <- function(c, i, x, w, ytop, gl, fill = RP_FILL) {
  if (is.na(c$bc[[i]])) return(list())
  h <- gl$lane
  y <- ytop + (gl$row_h - h) / 2
  mid <- y + h / 2
  out <- list()
  if (isTRUE(c$bare)) {
    out <- c(out, list(rp_rect(x, w, mid - gl$hair / 2, gl$hair, RP_RULE, gl)))
  }
  wh <- function(l, ww) {
    if (is.na(ww)) return(NULL)
    rp_rect(rp_lane_x(l, x, w), ww / 100 * w, mid - gl$hair / 2, gl$hair,
            fill, gl)
  }
  cap <- function(l) {
    if (is.na(l)) return(NULL)
    rp_rect(rp_lane_x(l, x, w) - gl$hair / 2, gl$hair, y + gl$px * 3,
            h - gl$px * 6, fill, gl)
  }
  c(out, list(
    wh(c$wl[[i]], c$w1[[i]]),
    wh(c$b2[[i]], c$w2[[i]]),
    if (!is.na(c$w1[[i]])) cap(c$wl[[i]]),
    if (!is.na(c$w2[[i]])) cap(c$wh[[i]]),
    if (!is.na(c$bw[[i]])) {
      rp_rect(rp_lane_x(c$bl[[i]], x, w), c$bw[[i]] / 100 * w,
              y + gl$px, h - 2 * gl$px, rp_sub(fill), gl)
    },
    rp_rect(rp_lane_x(c$bc[[i]], x, w) - gl$px, gl$px * 2, y, h, fill, gl)
  ))
}

# Point range: fence band (a tint), inner range, ringed centre dot. The dot is
# drawn as a CIRCLE, which is the one thing a percent-x / pixel-y export gets
# wrong if it stretches the cell -- so it is sized off the ROW height, never
# off the lane width.
rp_pr <- function(c, i, x, w, ytop, gl, fill = RP_FILL) {
  if (is.na(c$c[[i]])) return(list())
  h <- gl$lane
  y <- ytop + (gl$row_h - h) / 2
  mid <- y + h / 2
  out <- list()
  if (isTRUE(c$bare)) {
    out <- c(out, list(rp_rect(x, w, mid - gl$hair / 2, gl$hair, RP_RULE, gl)))
  }
  if (!is.null(c$ow) && !is.na(c$ow[[i]])) {
    out <- c(out, list(rp_rect(
      rp_lane_x(c$ol[[i]], x, w), c$ow[[i]] / 100 * w,
      mid - gl$px * 4, gl$px * 8, rp_faint(fill), gl)))
  }
  if (!is.na(c$rw[[i]])) {
    out <- c(out, list(rp_rect(
      rp_lane_x(c$l[[i]], x, w), c$rw[[i]] / 100 * w,
      mid - gl$px * 2, gl$px * 4, fill, gl)))
  }
  cx <- rp_lane_x(c$c[[i]], x, w)
  c(out, list(
    grid::circleGrob(x = grid::unit(cx, "in"), y = grid::unit(gl$H - mid, "in"),
               r = grid::unit(gl$px * 4, "in"),
               gp = grid::gpar(fill = fill, col = "#ffffff", lwd = 1.2))
  ))
}

# The swimlane. Segment tuples are [left%, width%, fill index], already
# positioned on the column's shared x domain, so the painter only converts
# percent to inches and looks the colour up. `min-width:2px` matters here:
# a one-day event on a 200-day domain is 0.5% of the lane and would vanish.
rp_interval <- function(c, i, x, w, ytop, gl) {
  segs <- c$segs[[i]]
  if (!length(segs)) return(list())
  h <- gl$lane
  y <- ytop + (gl$row_h - h) / 2
  lapply(segs, function(sg) {
    rp_rect(rp_lane_x(sg[[1L]], x, w),
            max(sg[[2L]] / 100 * w, gl$px * 2), y, h,
            c$fills[[sg[[3L]]]], gl)
  })
}

# The sparkline. The geometry arrives PRE-PRINTED as SVG point strings in the
# viewBox the browser stretches (0 0 100 36, y downward), so the painter
# parses them back rather than re-projecting the series: the trajectory a
# slide shows is the one the screen drew, off the same numbers.
rp_sp_pts <- function(s) {
  if (!length(s) || is.na(s) || !nzchar(s)) return(NULL)
  p <- do.call(rbind, lapply(strsplit(strsplit(s, " ", fixed = TRUE)[[1L]],
                                      ",", fixed = TRUE),
                             as.numeric))
  if (!is.matrix(p) || nrow(p) < 1L) NULL else p
}

rp_sparkline <- function(c, i, x, w, ytop, gl, fill = RP_FILL) {
  h <- gl$lane                      # the sparkline lane is the taller one
  y <- ytop + (gl$row_h - h) / 2
  X <- function(v) x + v / 100 * w
  Y <- function(v) gl$H - (y + v / 36 * h)   # viewBox y grows downward
  out <- list()
  if (!is.na(c$rby)) {
    out <- c(out, list(rp_rect(x, w, y + c$rby / 36 * h, c$rbh / 36 * h,
                               rp_faint(fill), gl)))
  }
  bd <- rp_sp_pts(c$bd[[i]])
  if (!is.null(bd)) {
    out <- c(out, list(grid::polygonGrob(
      x = grid::unit(X(bd[, 1L]), "in"), y = grid::unit(Y(bd[, 2L]), "in"),
      gp = grid::gpar(fill = RP_TRACK, col = NA))))
  }
  if (!is.na(c$rc)) {
    out <- c(out, list(grid::linesGrob(
      x = grid::unit(c(x, x + w), "in"), y = grid::unit(rep(Y(c$rc), 2L), "in"),
      gp = grid::gpar(col = RP_TICK, lwd = 0.8, lty = "22"))))
  }
  pl <- rp_sp_pts(c$pl[[i]])
  if (!is.null(pl) && nrow(pl) > 1L) {
    out <- c(out, list(grid::linesGrob(
      x = grid::unit(X(pl[, 1L]), "in"), y = grid::unit(Y(pl[, 2L]), "in"),
      gp = grid::gpar(col = fill, lwd = 1.3, lineend = "round",
                linejoin = "round"))))
  }
  if (!is.na(c$dx[[i]])) {
    out <- c(out, list(grid::circleGrob(
      x = grid::unit(X(c$dx[[i]]), "in"),
      y = grid::unit(gl$H - (y + c$dy[[i]] / 100 * h), "in"),
      r = grid::unit(gl$px * 3, "in"),
      gp = grid::gpar(fill = fill, col = "#ffffff", lwd = 1))))
  }
  out
}

rp_multi <- function(c, i, x, w, ytop, gl) {
  k <- length(c$lv)
  h <- if (k > 2) gl$px * 8 else gl$lane
  gap <- gl$px * 2
  tot <- k * h + (k - 1) * gap
  y0 <- ytop + (gl$row_h - tot) / 2
  unlist(lapply(seq_len(k), function(j) {
    lv <- c$lv[[j]]
    yy <- y0 + (j - 1) * (h + gap) - (gl$row_h - h) / 2
    sub <- gl
    sub$lane <- h
    if (identical(c$kind, "box")) rp_box(lv, i, x, w, yy, sub, lv$fill %||% RP_FILL)
    else rp_pr(lv, i, x, w, yy, sub, lv$fill %||% RP_FILL)
  }), recursive = FALSE)
}

# --- the header axis -----------------------------------------------------
# The domain printed once per glyph column, from the same numbers the marks
# were scaled with (rank_axis_domain(), never re-derived).
rp_axis <- function(p, prep, x, w, ytop, gl) {
  dom <- rank_axis_domain(p, prep)
  if (is.null(dom)) return(list())
  t <- if (isTRUE(dom$date)) {
    rank_axis_date_ticks(dom$d0, dom$d1)
  } else {
    at <- rank_axis_ticks(dom$d0, dom$d1)
    list(at = at, labels = lane_fmt(at))
  }
  if (length(t$at) < 2L) return(list())
  last <- length(t$at)
  if (isTRUE(dom$pct)) t$labels[[last]] <- paste0(t$labels[[last]], "%")
  pct <- (t$at - dom$d0) / (dom$d1 - dom$d0) * 100
  lapply(seq_len(last), function(k) {
    xx <- rp_lane_x(pct[[k]], x, w)
    just <- if (pct[[k]] <= 8) "left" else if (pct[[k]] >= 92) "right" else "centre"
    grid::textGrob(t$labels[[k]], x = grid::unit(xx, "in"), y = grid::unit(gl$H - ytop, "in"),
             just = c(just, "top"),
             gp = grid::gpar(fontsize = gl$fs * 0.78, col = RP_MUTED,
                       fontfamily = gl$family))
  })
}

# --- the vertical budget -------------------------------------------------
#
# Every band's height, computed in ONE place: the painter reads it to lay the
# picture out, the pager reads it to decide how many rows a slide holds. Two
# copies of this arithmetic is how a page plan ends up off by a row.
rp_heights <- function(m, prep, lay, fs, row_h = NULL, title = NULL,
                       subtitle = NULL, caption = NULL) {

  # One CSS px in inches, tied to the TYPE SIZE rather than to 1/96in: the
  # on-screen table sets 13px type against a 12px lane, so a 10pt export
  # keeps that ratio instead of shrinking the marks against the text.
  px <- (fs / 72) / 13
  # A sparkline column claims 40px against every other mark's 12: amplitude
  # is the point of it, and the CSS gives it the whole row. So the ROW grows
  # when one is present, rather than the trajectory being squeezed.
  has_sp <- any(vapply(m$cols, function(c) identical(c$kind, "sparkline"),
                       logical(1L)))
  row_h <- row_h %||% max(fs / 72 * 1.9, px * 22, if (has_sp) px * 44 else 0)

  line_h <- fs / 72 * 1.5
  # The second header tier (dt_th's `label`): the measure under the facet
  # level, the stub's column name under the group. Present only when some
  # column carries one.
  sub_line <- if (any(nzchar(c(lay$sub, prep$group_label %||% "")))) {
    fs / 72 * 1.25
  } else {
    0
  }
  span_off <- if (is.null(prep$facet_spans)) 0 else line_h
  axis_h <- fs / 72 * 1.1
  has_leg <- length(tryCatch(rank_legend_spec(prep),
                             error = function(e) NULL)$groups) > 0L

  h <- list(
    px = px, row_h = row_h, line_h = line_h, sub_line = sub_line,
    span_off = span_off, axis_h = axis_h,
    head_h = span_off + line_h + sub_line + axis_h + 0.06,
    title_h = if (is.null(title) || !nzchar(title)) 0 else fs / 72 * 2.4,
    sub_h = if (is.null(subtitle) || !nzchar(subtitle)) 0 else fs / 72 * 1.7,
    cap_h = if (is.null(caption) || !nzchar(caption)) 0 else fs / 72 * 1.8,
    leg_h = if (has_leg) fs / 72 * 1.9 else 0,
    fold_h = if (is.null(m$fold)) 0 else row_h
  )
  # Everything that is NOT rows: what a page spends before its first row.
  h$chrome <- h$title_h + h$sub_h + h$leg_h + h$head_h + h$cap_h + 0.08
  h
}

# --- the picture ---------------------------------------------------------
rank_paint_grob <- function(m, prep, width_in = 12.5, fs = 9, family = "sans",
                            row_h = NULL, title = NULL, subtitle = NULL,
                            caption = NULL) {

  n <- m$n
  lay <- rp_layout(m, prep, width_in, fs = fs, family = family)
  hh <- rp_heights(m, prep, lay, fs, row_h, title, subtitle, caption)
  px <- hh$px
  row_h <- hh$row_h
  axis_h <- hh$axis_h
  line_h <- hh$line_h
  sub_line <- hh$sub_line
  span_off <- hh$span_off
  head_h <- hh$head_h
  title_h <- hh$title_h
  sub_h <- hh$sub_h
  cap_h <- hh$cap_h
  leg_h <- hh$leg_h
  H <- hh$chrome + n * row_h + hh$fold_h

  gl <- list(H = H, px = px, lane = px * 12, hair = px, row_h = row_h,
             fs = fs, family = family)

  g <- list()
  txt <- function(s, x, y, just = "left", size = fs, col = RP_TEXT,
                  bold = FALSE, vjust = "centre") {
    grid::textGrob(s, x = grid::unit(x, "in"), y = grid::unit(H - y, "in"),
             just = c(just, vjust),
             gp = grid::gpar(fontsize = size, col = col, fontfamily = family,
                       fontface = if (bold) "bold" else "plain"))
  }
  rule <- function(y, col = RP_RULE, lwd = 0.6) {
    grid::linesGrob(x = grid::unit(c(0, width_in), "in"), y = grid::unit(c(H - y, H - y), "in"),
              gp = grid::gpar(col = col, lwd = lwd))
  }

  y <- 0
  if (title_h) {
    g <- c(g, list(txt(title, 0, y + title_h * 0.55, size = fs * 1.25,
                       bold = TRUE)))
    y <- y + title_h
  }
  if (sub_h) {
    g <- c(g, list(txt(subtitle, 0, y + sub_h * 0.45, size = fs * 0.95,
                       col = RP_MUTED)))
    y <- y + sub_h
  }

  # The colour legend. A swimlane or a split bar is unreadable without it,
  # and on screen it lives in the toolbar the picture does not have, so it
  # moves under the subtitle. Same spec the HTML legend is built from.
  leg <- tryCatch(rank_legend_spec(prep), error = function(e) NULL)
  if (!is.null(leg) && length(leg$groups)) {
    sw <- px * 9
    at <- 0
    ly <- y + leg_h / 2
    for (grp in leg$groups) {
      if (nzchar(grp$title %||% "")) {
        g <- c(g, list(txt(grp$title, at, ly, size = fs * 0.8,
                           col = RP_MUTED)))
        at <- at + rp_w(grp$title, fs * 0.8, family) + px * 6
      }
      for (it in grp$items) {
        g <- c(g, list(grid::rectGrob(
          x = grid::unit(at, "in"), y = grid::unit(H - ly, "in"),
          width = grid::unit(sw, "in"), height = grid::unit(sw, "in"),
          just = c("left", "centre"), gp = grid::gpar(fill = it$color, col = NA))),
          list(txt(it$label, at + sw + px * 3, ly, size = fs * 0.8)))
        at <- at + sw + px * 3 + rp_w(it$label, fs * 0.8, family) + px * 10
      }
      at <- at + px * 8
    }
    y <- y + leg_h
  }

  # header band: [spanner] / leaf label / second tier / axis strip
  hy <- y
  fs_spans <- prep$facet_spans
  lead <- if (is.null(fs_spans)) length(prep$plan) else fs_spans$lead
  lab_y <- hy + span_off
  sub_y <- lab_y + line_h
  axis_y <- sub_y + sub_line + 0.02
  g <- c(g, list(txt(lay$stub_lab, lay$pad, lab_y, bold = TRUE,
                     size = fs * 0.95, vjust = "top")))
  if (sub_line && nzchar(prep$group_label %||% "")) {
    g <- c(g, list(txt(prep$group_label, lay$pad, sub_y, size = fs * 0.8,
                       col = RP_MUTED, vjust = "top")))
  }

  for (i in seq_along(m$cols)) {
    x0 <- lay$x[i + 1]
    w <- lay$widths[i]
    lane_w <- w - lay$slot[i] - 2 * lay$pad
    just <- if (lay$is_glyph[i]) "left" else "right"
    xx <- if (lay$is_glyph[i]) x0 + lay$pad else x0 + w - lay$pad
    g <- c(g, list(txt(lay$hdr[i], xx, lab_y, just = just, bold = TRUE,
                       size = fs * 0.95, vjust = "top")))
    if (sub_line && nzchar(lay$sub[i])) {
      g <- c(g, list(txt(lay$sub[i], xx, sub_y, just = just, size = fs * 0.8,
                         col = RP_MUTED, vjust = "top")))
    }
    if (lay$is_glyph[i]) {
      g <- c(g, rp_axis(prep$plan[[i]], prep, x0 + lay$pad, lane_w,
                        axis_y, gl))
    }
  }
  # spanner band (facet groups): one label over its columns, with a rule
  if (!is.null(fs_spans)) {
    at <- lead
    for (grp in fs_spans$groups) {
      x0 <- lay$x[at + 2]
      x1 <- lay$x[at + 1 + grp$n + 1]
      g <- c(g, list(
        txt(grp$label, (x0 + x1) / 2, hy + line_h * 0.5, just = "centre",
            bold = TRUE, size = fs * 0.95),
        grid::linesGrob(x = grid::unit(c(x0 + lay$pad, x1 - lay$pad), "in"),
                  y = grid::unit(c(H - hy - line_h * 0.92, H - hy - line_h * 0.92), "in"),
                  gp = grid::gpar(col = RP_TICK, lwd = 0.6))
      ))
      at <- at + grp$n
    }
  }
  y <- hy + head_h
  g <- c(g, list(rule(y, col = "#8d8b84", lwd = 0.9)))

  # body
  for (r in seq_len(n)) {
    ytop <- y + (r - 1) * row_h
    ind <- if (m$level[[r]] > 0L) 0.28 else 0
    g <- c(g, list(txt(m$label[[r]], lay$pad + ind, ytop + row_h / 2,
                       bold = isTRUE(m$parent_row[[r]]))))
    for (i in seq_along(m$cols)) {
      c <- m$cols[[i]]
      x0 <- lay$x[i + 1] + lay$pad
      w <- lay$widths[i]
      lane_w <- w - lay$slot[i] - 2 * lay$pad
      k <- lay$kind[i]
      marks <- if (isTRUE(c$multi)) {
        rp_multi(c, r, x0, lane_w, ytop, gl)
      } else if (k == "bar") {
        rp_bar(c, r, x0, lane_w, ytop, gl)
      } else if (k == "barsplit") {
        rp_barsplit(c, r, x0, lane_w, ytop, gl)
      } else if (k == "bardiv") {
        rp_bardiv(c, r, x0, lane_w, ytop, gl)
      } else if (k == "box") {
        rp_box(c, r, x0, lane_w, ytop, gl)
      } else if (k == "pointrange") {
        rp_pr(c, r, x0, lane_w, ytop, gl)
      } else if (k == "interval") {
        rp_interval(c, r, x0, lane_w, ytop, gl)
      } else if (k == "sparkline") {
        gl_sp <- gl
        gl_sp$lane <- gl$px * 36
        rp_sparkline(c, r, x0, lane_w, ytop, gl_sp)
      } else {
        NULL
      }
      g <- c(g, marks)
      # the value slot: glyph columns label to the right of the lane, number
      # columns are the cell
      if (!is.null(c$disp)) {
        s <- c$disp[[min(r, length(c$disp))]]
        p <- if (is.null(c$pct)) "" else c$pct[[min(r, length(c$pct))]]
        if (lay$is_glyph[i]) {
          xr <- lay$x[i + 1] + w - lay$pad
          g <- c(g, list(txt(trimws(paste(s, p)), xr, ytop + row_h / 2,
                             just = "right", size = fs * 0.95)))
        } else {
          xr <- lay$x[i + 1] + w - lay$pad
          g <- c(g, list(txt(trimws(paste(s, p)), xr, ytop + row_h / 2,
                             just = "right",
                             col = if (isTRUE(c$text)) RP_TEXT else RP_TEXT)))
        }
      }
    }
    g <- c(g, list(rule(ytop + row_h)))
  }
  y <- y + n * row_h

  # `top_n` is never a silent truncation: what fell below the cut says so on
  # the picture too (rank_fold_text(), same string the table prints).
  if (!is.null(m$fold)) {
    g <- c(g, list(txt(m$fold, lay$pad, y + row_h / 2, size = fs * 0.9,
                       col = RP_MUTED)),
           list(rule(y + row_h)))
    y <- y + row_h
  }

  if (cap_h) {
    g <- c(g, list(txt(caption, 0, y + cap_h * 0.55, size = fs * 0.85,
                       col = RP_MUTED)))
  }

  list(
    grob = grid::gTree(
      children = do.call(grid::gList, Filter(Negate(is.null), g))),
    width = width_in, height = H
  )
}

# --- paging --------------------------------------------------------------
#
# A picture cannot reflow, so a table too tall for its slide has to be CUT
# and the pieces drawn separately. The cut happens on the cell model, before
# anything is painted, which is the only place it can: the geometry is
# already per row, so a slice of rows is a valid table with no recomputation.
#
# The flextable path (pptx_add_exhibit) does the same job by measuring
# rendered row heights. Here every row is the same height by construction,
# so the arithmetic is a division -- and `attr(ft, "row_map")` has no
# counterpart to go wrong.

# Slice a column's per-row vectors, leaving its per-COLUMN ones alone.
# Length is the test, with three names handled explicitly because they are
# the ones where a length-n vector would mean something else: `seg` / `segv`
# are one entry per SERIES (each a full column of values), and `lv` is one
# entry per colour LEVEL (each a whole nested cell).
rp_slice_col <- function(c, idx, n) {
  nm <- names(c)
  out <- c
  for (k in seq_along(c)) {
    v <- c[[k]]
    key <- nm[[k]]
    if (key %in% c("seg", "segv")) {
      out[[k]] <- lapply(v, function(s) if (length(s) == n) s[idx] else s)
    } else if (identical(key, "lv")) {
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

# Where to cut. Rows are uniform, so the count follows from the budget; the
# two adjustments are about not stranding a group:
#
#  * a page never ENDS on a parent row (its children would open the next page
#    with nothing above them saying whose they are), and
#  * a page that OPENS mid-group repeats the parent row, marked as carried
#    over -- the same rule static_table(continued = TRUE) applies to a
#    section header, for the same reason.
rp_page_rows <- function(m, per_page) {
  n <- m$n
  if (per_page >= n) return(list(seq_len(n)))
  lvl <- as.integer(m$level %||% rep(0L, n))
  par <- as.logical(m$parent_row %||% rep(FALSE, n))
  pages <- list()
  at <- 1L
  while (at <= n) {
    # A carried-over parent costs a slot, so it is decided BEFORE the cut,
    # never by dropping a row afterwards. (Dropping was the first version and
    # it could walk the cursor backwards: a one-row page whose only row was
    # replaced by its parent restarted above where it began, and the loop
    # never terminated.)
    lead <- integer()
    take <- per_page
    if (at > 1L && lvl[[at]] > 0L && per_page >= 2L) {
      p <- at - 1L
      while (p >= 1L && lvl[[p]] > 0L) p <- p - 1L
      if (p >= 1L) {
        lead <- p
        take <- per_page - 1L
      }
    }
    end <- min(at + take - 1L, n)
    # Do not END on a parent row: its children would open the next page with
    # nothing above them saying whose they are. Never below `at`, so the
    # cursor always advances.
    while (end > at && isTRUE(par[[end]])) end <- end - 1L
    idx <- c(lead, seq.int(at, end))
    attr(idx, "carried") <- length(lead) > 0L
    pages[[length(pages) + 1L]] <- idx
    at <- end + 1L
  }
  pages
}

# How many rows fit one page at this type size. Split out of the pager so
# the exporter can ask the question without painting anything: choosing the
# size that keeps a table on one slide means asking it once per candidate,
# and building the grobs to find out would paint pages nobody keeps.
rank_paint_per_page <- function(m, prep, width_in = 12.5, max_height = 5.4,
                                fs = 9, family = "sans", row_h = NULL,
                                title = NULL, subtitle = NULL,
                                caption = NULL) {

  lay <- rp_layout(m, prep, width_in, fs = fs, family = family)
  hh <- rp_heights(m, prep, lay, fs, row_h, title, subtitle, caption)

  # The page marker can push a one-line title to two, so budget for the
  # chrome a CONTINUATION page carries, which is never less than page 1's.
  budget <- max_height - hh$chrome - hh$fold_h

  max(1L, floor(budget / hh$row_h))
}

# One table, as many pictures as it takes. `max_height` is the slide's body
# box; `title` grows a "(2 of 7)" marker so a reader always knows a page is
# part of a longer table, and the fold row rides on the LAST page only.
rank_paint_pages <- function(m, prep, width_in = 12.5, max_height = 5.4,
                             fs = 9, family = "sans", row_h = NULL,
                             title = NULL, subtitle = NULL, caption = NULL) {

  per_page <- rank_paint_per_page(m, prep, width_in, max_height, fs, family,
                                  row_h, title, subtitle, caption)

  if (per_page >= m$n) {
    p <- rank_paint_grob(m, prep, width_in = width_in, fs = fs,
                         family = family, row_h = row_h, title = title,
                         subtitle = subtitle, caption = caption)
    return(list(p))
  }

  pages <- rp_page_rows(m, per_page)
  n_pg <- length(pages)
  lapply(seq_along(pages), function(k) {
    idx <- pages[[k]]
    mk <- rp_slice(m, as.integer(idx))
    # The fold row belongs to the bottom of the WHOLE table, not to page 3.
    if (k < n_pg) mk$fold <- NULL
    ttl <- if (is.null(title) || !nzchar(title)) {
      title
    } else {
      paste0(title, "  (", k, " of ", n_pg, ")")
    }
    sub <- if (isTRUE(attr(idx, "carried")) && !is.null(subtitle)) {
      paste0(subtitle, " \u00b7 continued")
    } else {
      subtitle
    }
    rank_paint_grob(mk, prep, width_in = width_in, fs = fs, family = family,
                    row_h = row_h, title = ttl, subtitle = sub,
                    caption = if (k == n_pg) caption else NULL)
  })
}

# The picture as a file. ragg because it is already a dependency of the
# ecosystem and renders text with the same shaper systemfonts measured with.
# --- what the paint path needs -------------------------------------------
#
# grid and systemfonts to lay out, ragg to rasterise. All Suggests, like
# flextable and officer on the other pptx path, so the package installs
# without them and only the export asks.
#' @noRd
rank_paint_ready <- function() {
  all(vapply(c("grid", "systemfonts", "ragg"), requireNamespace,
             logical(1L), quietly = TRUE))
}

#' @noRd
rank_paint_require <- function() {
  miss <- Filter(function(p) !requireNamespace(p, quietly = TRUE),
                 c("grid", "systemfonts", "ragg"))
  if (length(miss)) {
    stop("Painting a summarize table needs: ", paste(miss, collapse = ", "),
         ".", call. = FALSE)
  }
  invisible(TRUE)
}

# One painted page as an image an rmarkdown / knitr chunk can print.
#' @noRd
rank_paint_image <- function(x, width_in = 9, res = 300, ...) {
  rank_paint_require()
  p <- rank_paint_grob(x$cells, x$prep, width_in = width_in,
                       title = x$title, subtitle = x$subtitle,
                       caption = x$caption, ...)
  f <- tempfile(fileext = ".png")
  rp_write_png(p, f, res = res)
  htmltools::tags$img(src = knitr::image_uri(f),
                      style = paste0("width:", p$width, "in;max-width:100%"))
}

#' @noRd
rank_paint_png_file <- function(p, file, res = 300) {
  rank_paint_require()
  rp_write_png(p, file, res = res)
}

rp_write_png <- function(p, file, res = 300) {
  ragg::agg_png(file, width = p$width, height = p$height, units = "in",
                res = res, background = "white")
  on.exit(grDevices::dev.off())
  grid::grid.newpage()
  grid::grid.draw(p$grob)
  list(file = file, width = p$width, height = p$height)
}

rank_paint_png <- function(m, prep, file, width_in = 12.5, res = 300, ...) {
  invisible(rp_write_png(rank_paint_grob(m, prep, width_in = width_in, ...),
                         file, res))
}

# Every page of a table as its own file: `<stem>.png`, `<stem>-2.png`, ...
rank_paint_pngs <- function(m, prep, stem, width_in = 12.5, max_height = 5.4,
                            res = 300, ...) {
  pg <- rank_paint_pages(m, prep, width_in = width_in,
                         max_height = max_height, ...)
  lapply(seq_along(pg), function(k) {
    f <- if (k == 1L) paste0(stem, ".png") else paste0(stem, "-", k, ".png")
    rp_write_png(pg[[k]], f, res)
  })
}
