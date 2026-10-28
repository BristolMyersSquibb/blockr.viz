# The groups table as ONE grid picture, for a slide or a PNG: a PowerPoint
# cell holds text runs only, so a glyph cannot sit in one. Export only;
# nothing on the block's render path calls it. Layout, row heights, the
# header and legend, the body, and paging; the marks are in R/paint-marks.R.
# Needs ragg to rasterise.

# --- layout --------------------------------------------------------------
# The stub and the number columns shrink to fit; the glyph columns share the
# slack (the `width:1%` idiom of summarize-table-css.R). A glyph column keeps
# a label slot of `dw` characters so its lanes start and end at the same x on
# every row.
paint_layout <- function(m, prep, width_in, fs = 9, family = "sans",
                      pad = 0.06, lane_min = 0.9) {

  plan <- prep$plan
  hdr <- vapply(plan, function(p) p$label %||% "", character(1L))
  sub <- vapply(plan, function(p) p$sub_label %||% "", character(1L))

  stub_lab <- summarize_label_header(prep)
  indent <- ifelse(m$level > 0L, PAINT_INDENT, 0)
  stub_w <- max(
    paint_w(paste0(m$label), fs, family) + max(indent),
    paint_w(stub_lab, fs, family, bold = TRUE)
  ) + 2 * pad

  kind <- vapply(m$cols, function(c) c$kind %||% "num", character(1L))
  is_glyph <- kind %in% c("bar", "barsplit", "bardiv", "box", "pointrange",
                          "pair", "interval", "sparkline")

  # Fixed columns: the widest of the header and its cells.
  fixed <- vapply(seq_along(m$cols), function(i) {
    if (is_glyph[i]) return(NA_real_)
    c <- m$cols[[i]]
    if (identical(kind[i], "tile")) {
      return(max(paint_w(c$disp, fs, family), fs / 72 * 2.2) + 2 * pad)
    }
    txt <- if (is.null(c$pct)) c$disp else paste(c$disp, c$pct)
    max(paint_w(txt, fs, family), paint_w(c(hdr[i], sub[i]), fs, family)) + 2 * pad
  }, numeric(1L))

  # The value slot a glyph column reserves right of its lane.
  slot <- vapply(seq_along(m$cols), function(i) {
    c <- m$cols[[i]]
    if (!is_glyph[i] || is.null(c$dw)) return(0)
    c$dw * paint_ch(fs * PAINT_VAL_SIZE, family) + pad
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


# Every row's height: the table's row height, grown where a colour-split
# column has more levels than that holds.
paint_row_heights <- function(m, row_h, px, fs) {
  out <- rep(row_h, m$n)
  rows <- seq_len(m$n)
  for (c in m$cols) {
    if (identical(c$kind, "barsplit") && identical(c$mode, "grouped")) {
      k <- vapply(rows, function(r) {
        sum(vapply(c$segv, function(x) paint_pos(x[[r]]), logical(1L)))
      }, numeric(1L))
      need <- (k - 1) * paint_pitch(px, fs, !is.null(c$ldisp), px * 14) +
        px * 10 + px * 8
    } else if (isTRUE(c$multi)) {
      k <- vapply(rows, function(r) length(paint_multi_drawn(c, r)), numeric(1L))
      need <- (k - 1) *
        paint_pitch(px, fs, !is.null(c$dw), px * 12 + px * 2) + px * 12 + px * 8
    } else {
      next
    }
    out <- ifelse(k >= 2L, pmax(out, need), out)
  }
  out
}

# The domain printed once per glyph column, from the numbers the marks were
# scaled with.
paint_axis <- function(p, prep, x, w, ytop, gl) {
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
    paint_txt(gl, t$labels[[k]], paint_lane_x(pct[[k]], x, w), ytop, just = just,
           vjust = "top", size = gl$fs * 0.78, col = PAINT_MUTED)
  })
}

# --- the vertical budget -------------------------------------------------
# Every band's height. The painter lays the picture out from it and the pager
# decides how many rows a slide holds from it.
paint_legend_spec <- function(prep) {
  tryCatch(summarize_legend_spec(prep), error = function(e) NULL)
}

paint_has <- function(s) !is.null(s) && nzchar(s)

paint_heights <- function(m, prep, lay, fs, row_h = NULL, title = NULL,
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
    min(1.8, max(paint_w(paint_tile_title(lay$hdr[tile]), fs * 0.85,
                      lay$family %||% "sans")) + 0.08)
  } else {
    0
  }
  h <- list(
    px = px, row_h = row_h, rows = paint_row_heights(m, row_h, px, fs),
    line_h = line_h, sub_line = sub_line,
    span_off = span_off, axis_h = axis_h, rot_h = rot_h,
    head_h = span_off + line_h + sub_line + axis_h + 0.06 + rot_h,
    title_h = if (paint_has(title)) fs / 72 * 2.4 else 0,
    sub_h = if (paint_has(subtitle)) fs / 72 * 1.7 else 0,
    cap_h = if (paint_has(caption)) fs / 72 * 1.8 else 0,
    leg_h = if (length(paint_legend_spec(prep)$groups) > 0L) fs / 72 * 1.9 else 0,
    fold_h = if (is.null(m$fold)) 0 else row_h
  )
  # Everything that is not rows: what a page spends before its first row.
  h$chrome <- h$title_h + h$sub_h + h$leg_h + h$head_h + h$cap_h + 0.08
  h
}

# --- the picture ---------------------------------------------------------
# The colour legend, under the subtitle (on screen it lives in the toolbar).
paint_legend <- function(prep, y, hh, gl) {
  leg <- paint_legend_spec(prep)
  fs <- gl$fs
  px <- gl$px
  sw <- px * 9
  at <- 0
  ly <- y + hh$leg_h / 2
  g <- list()
  for (grp in leg$groups) {
    if (nzchar(grp$title %||% "")) {
      g <- c(g, list(paint_txt(gl, grp$title, at, ly, size = fs * 0.8,
                            col = PAINT_MUTED)))
      at <- at + paint_w(grp$title, fs * 0.8, gl$family) + px * 6
    }
    for (it in grp$items) {
      g <- c(g, list(paint_sq(at, gl$H - ly, sw, it$color, c("left", "centre")),
                     paint_txt(gl, it$label, at + sw + px * 3, ly,
                            size = fs * 0.8)))
      at <- at + sw + px * 3 + paint_w(it$label, fs * 0.8, gl$family) + px * 10
    }
    at <- at + px * 8
  }
  g
}

# Facet spanners: one label over its columns, with a rule.
paint_spans <- function(spans, lay, hh, hy, gl) {
  fs <- gl$fs
  at <- spans$lead
  g <- list()
  for (grp in spans$groups) {
    x0 <- lay$x[at + 2]
    x1 <- lay$x[at + 1 + grp$n + 1]
    # The level's N sits beside the level, muted; the pair is centred.
    ny <- hy + hh$line_h * 0.5
    nlab <- if (!is.null(grp$pop)) paste0("N = ", grp$pop)
    lw <- paint_w(grp$label, fs * 0.95, gl$family, bold = TRUE)
    gap <- if (is.null(nlab)) 0 else paint_ch(fs * 0.8, gl$family)
    nw <- if (is.null(nlab)) 0 else paint_w(nlab, fs * 0.8, gl$family)
    lx <- (x0 + x1) / 2 - (lw + gap + nw) / 2
    ry <- gl$H - hy - hh$line_h * 0.92
    g <- c(g, list(
      paint_txt(gl, grp$label, lx, ny, bold = TRUE, size = fs * 0.95),
      if (!is.null(nlab)) {
        paint_txt(gl, nlab, lx + lw + gap, ny, size = fs * 0.8, col = PAINT_MUTED)
      },
      paint_ln(c(x0 + lay$pad, x1 - lay$pad), c(ry, ry), PAINT_TICK, 0.6)
    ))
    at <- at + grp$n
  }
  g
}

# The header band from `hy`: [spanner] / leaf label / second tier / axis strip.
paint_header <- function(m, prep, lay, hh, hy, gl) {
  fs <- gl$fs
  lab_y <- hy + hh$span_off
  sub_y <- lab_y + hh$line_h
  axis_y <- sub_y + hh$sub_line + 0.02
  lab <- function(s, x, just = "left") {
    paint_txt(gl, s, x, lab_y, just = just, bold = TRUE, size = fs * 0.95,
           vjust = "top")
  }
  sub <- function(s, x, just = "left") {
    paint_txt(gl, s, x, sub_y, just = just, size = fs * 0.8, col = PAINT_MUTED,
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
      g <- c(g, list(paint_txt(gl, paint_tile_title(lay$hdr[i]), x0 + w / 2,
                            hy + hh$head_h - 0.06, size = fs * 0.85,
                            rot = 90)))
      next
    }
    g <- c(g, list(
      lab(lay$hdr[i], xx, just),
      if (hh$sub_line && nzchar(lay$sub[i])) sub(lay$sub[i], xx, just)
    ), if (glyph) {
      paint_axis(prep$plan[[i]], prep, x0 + lay$pad,
              w - lay$slot[i] - 2 * lay$pad, axis_y, gl)
    })
  }
  if (is.null(prep$facet_spans)) return(g)
  c(g, paint_spans(prep$facet_spans, lay, hh, hy, gl))
}

# The marks of one cell; `k` is the column kind.
paint_cell_marks <- function(c, k, r, x0, lane_w, xr, ytop, gl) {
  if (isTRUE(c$multi)) return(paint_multi(c, r, x0, lane_w, ytop, gl, xr = xr))
  switch(
    k,
    bar = paint_bar(c, r, x0, lane_w, ytop, gl),
    barsplit = paint_barsplit(c, r, x0, lane_w, ytop, gl, xr = xr),
    bardiv = paint_bardiv(c, r, x0, lane_w, ytop, gl),
    box = paint_box(c, r, x0, lane_w, ytop, gl),
    pointrange = paint_pr(c, r, x0, lane_w, ytop, gl),
    pair = paint_pair(c, r, x0, lane_w, ytop, gl),
    interval = paint_interval(c, r, x0, lane_w, ytop, gl),
    sparkline = {
      gl$lane <- gl$px * 36
      paint_sparkline(c, r, x0, lane_w, ytop, gl)
    },
    NULL
  )
}

# Row `r` from `ytop`, `rh` high (a row grown for per-level numbers centres
# everything in it): the label, each cell's marks and value, the rule below.
paint_body_row <- function(m, lay, r, ytop, rh, gl) {
  fs <- gl$fs
  gl$row_h <- rh
  ind <- if (m$level[[r]] > 0L) PAINT_INDENT else 0
  g <- list(paint_txt(gl, m$label[[r]], lay$pad + ind, ytop + rh / 2,
                   bold = isTRUE(m$parent_row[[r]])))
  for (i in seq_along(m$cols)) {
    c <- m$cols[[i]]
    w <- lay$widths[i]
    k <- lay$kind[i]
    xr <- lay$x[i + 1] + w - lay$pad
    g <- c(g, paint_cell_marks(c, k, r, lay$x[i + 1] + lay$pad,
                            w - lay$slot[i] - 2 * lay$pad, xr, ytop, gl))
    if (identical(k, "tile")) {
      if (!is.na(c$bg[[r]])) {
        cx <- lay$x[i + 1] + w / 2
        g <- c(g, list(
          paint_sq(cx, gl$H - ytop - rh / 2, min(rh * 0.82, w - 2 * lay$pad),
                c$bg[[r]]),
          paint_txt(gl, c$disp[[r]], cx, ytop + rh / 2, just = "centre",
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
      g <- c(g, list(paint_txt(gl, trimws(paste(s, p)), xr, ytop + rh / 2,
                            just = "right",
                            size = if (glyph) fs * PAINT_VAL_SIZE else fs,
                            col = if (glyph) PAINT_MUTED else PAINT_TEXT)))
    }
  }
  c(g, list(paint_rule(gl, ytop + rh)))
}

summarize_paint_grob <- function(m, prep, width_in = 12.5, fs = 9,
                                 family = "sans", row_h = NULL, title = NULL,
                                 subtitle = NULL, caption = NULL) {

  lay <- paint_layout(m, prep, width_in, fs = fs, family = family)
  hh <- paint_heights(m, prep, lay, fs, row_h, title, subtitle, caption)
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
        paint_txt(gl, title, 0, ys[[1]] + hh$title_h * 0.55, size = fs * 1.25,
               bold = TRUE)
      },
      if (hh$sub_h) {
        paint_txt(gl, subtitle, 0, ys[[2]] + hh$sub_h * 0.45, size = fs * 0.95,
               col = PAINT_MUTED)
      }
    ),
    paint_legend(prep, ys[[3]], hh, gl),
    paint_header(m, prep, lay, hh, ys[[4]], gl),
    list(paint_rule(gl, ys[[5]], col = "#8d8b84", lwd = 0.9)),
    unlist(lapply(seq_len(m$n), function(r) {
      paint_body_row(m, lay, r, ytops[[r]], hh$rows[[r]], gl)
    }), recursive = FALSE),
    # `top_n` never truncates silently: the fold row says what fell below it.
    if (!is.null(m$fold)) {
      list(paint_txt(gl, m$fold, lay$pad, y_end + hh$row_h / 2, size = fs * 0.9,
                  col = PAINT_MUTED),
           paint_rule(gl, y_end + hh$row_h))
    },
    if (hh$cap_h) {
      list(paint_txt(gl, caption, 0, y_end + hh$fold_h + hh$cap_h * 0.55,
                  size = fs * 0.85, col = PAINT_MUTED))
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
paint_slice_col <- function(c, idx, n) {
  nm <- names(c)
  out <- c
  for (k in seq_along(c)) {
    v <- c[[k]]
    if (nm[[k]] %in% c("seg", "segv", "ldisp", "slab")) {
      out[[k]] <- lapply(v, function(s) if (length(s) == n) s[idx] else s)
    } else if (identical(nm[[k]], "lv")) {
      out[[k]] <- lapply(v, paint_slice_col, idx = idx, n = n)
    } else if (length(v) == n && (is.atomic(v) || is.list(v))) {
      out[[k]] <- v[idx]
    }
  }
  out
}

paint_slice <- function(m, idx) {
  n <- m$n
  m$cols <- lapply(m$cols, paint_slice_col, idx = idx, n = n)
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
paint_page_rows <- function(m, per_page, heights = NULL, budget = NULL) {
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

  lay <- paint_layout(m, prep, width_in, fs = fs, family = family)
  hh <- paint_heights(m, prep, lay, fs, row_h, title, subtitle, caption)

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

  lay <- paint_layout(m, prep, width_in, fs = fs, family = family)
  hh <- paint_heights(m, prep, lay, fs, row_h, title, subtitle, caption)
  pages <- paint_page_rows(
    m, per_page,
    heights = if (all(hh$rows == hh$row_h)) NULL else hh$rows,
    budget = max_height - hh$chrome - hh$fold_h
  )
  n_pg <- length(pages)
  lapply(seq_along(pages), function(k) {
    idx <- pages[[k]]
    mk <- paint_slice(m, as.integer(idx))
    if (k < n_pg) mk$fold <- NULL
    ttl <- if (paint_has(title)) paste0(title, "  (", k, " of ", n_pg, ")") else title
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
PAINT_SUGGESTED <- c("grid", "systemfonts")

#' @noRd
summarize_paint_ready <- function() {
  all(vapply(PAINT_SUGGESTED, requireNamespace, logical(1L), quietly = TRUE))
}

#' @noRd
summarize_paint_require <- function() {
  miss <- Filter(function(p) !requireNamespace(p, quietly = TRUE),
                 PAINT_SUGGESTED)
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
  paint_write_png(p, f, res = res)
  htmltools::tags$img(src = knitr::image_uri(f),
                      style = paste0("width:", p$width, "in;max-width:100%"))
}

paint_write_png <- function(p, file, res = 300) {
  ragg::agg_png(file, width = p$width, height = p$height, units = "in",
                res = res, background = "white")
  on.exit(grDevices::dev.off())
  grid::grid.newpage()
  grid::grid.draw(p$grob)
  list(file = file, width = p$width, height = p$height)
}

# A tile column's title as printed on its side: long terms end in an ellipsis.
paint_tile_title <- function(x, max = 28L) {
  x <- as.character(x)
  ifelse(nchar(x) > max, paste0(substr(x, 1L, max - 1L), "…"), x)
}
