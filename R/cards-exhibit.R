# The cards look as an exhibit --------------------------------------------------
#
# The web page is the table view (table_static(), inlined by
# write_exhibit_html()); the slide and the image are pictures painted here
# with the summarize painter's glyph functions (paint_bar(), paint_box(), ... in
# R/paint-table.R). The spreadsheet never sees this object.

#' A table in the cards look, as a printable exhibit
#'
#' @param x A card model (`table_cards_groups()` / `table_cards_records()`),
#'   or a `summarize_exhibit` built with `look = "cards"`.
#' @param title,subtitle,caption Text above and below the cards.
#' @return An object of class `cards_exhibit`.
#' @noRd
cards_exhibit <- function(x, title = NULL, subtitle = NULL, caption = NULL) {
  src <- if (inherits(x, "summarize_exhibit")) x
  structure(
    list(model = if (is.null(src)) x else src$cards, cells = src$cells,
         prep = src$prep, args = src$args,
         title = title %||% src$title, subtitle = subtitle %||% src$subtitle,
         caption = caption %||% src$caption),
    class = c("cards_exhibit", "blockr_exhibit")
  )
}

#' @export
format.cards_exhibit <- function(x, ...) {
  paste0("<cards_exhibit: ", x$model$n, " rows, ", length(x$model$cols),
         " cards per row>")
}

#' @export
print.cards_exhibit <- function(x, ...) {
  cat(format(x), "\n")
  invisible(x)
}

#' @export
html_exhibit.cards_exhibit <- function(x, title = NULL, subtitle = NULL,
                                       caption = NULL, ...) {
  titles <- table_static_titles(title %||% x$title, subtitle %||% x$subtitle,
                                caption %||% x$caption)
  if (!is.null(x$cells)) {
    # A groups table's cards: their glyphs are the table's columns.
    g <- table_static_groups(x)
    g$model$cards <- cards_payload(x$model)
    g$config$look <- "cards"
    g$derived$titles <- titles
    return(table_static(g$model, g$config, g$derived))
  }
  model <- list(n = x$model$n, cols = list(), head = list(),
                cards = cards_payload(x$model))
  table_static(model, list(layout = "records", look = "cards"),
               list(titles = titles))
}

#' @export
pptx_add_exhibit.cards_exhibit <- function(doc, x, title = NULL,
                                           subtitle = NULL, caption = NULL,
                                           template = NULL, layout = NULL,
                                           master = NULL, top = NULL,
                                           font_size = 10,
                                           res = getOption(
                                             "blockr.viz.paint_res", 300),
                                           ...) {
  title <- title %||% x$title
  pptx_paint_require()
  s <- pptx_slide_context(doc, title, layout, master)
  top <- top %||% if (s$slide_title) 1.3 else 0.5
  pages <- cards_paint_pages(
    x, width_in = pptx_content_width(template) %||% (s$width - 0.8),
    max_height = pptx_body_bottom(doc, s$layout, s$master, s$height) - top,
    fs = font_size, title = if (s$slide_title) "" else title,
    subtitle = subtitle %||% x$subtitle, caption = caption %||% x$caption
  )
  pptx_add_pictures(doc, pages, s, top, title, res)
}

#' @export
write_exhibit_png.cards_exhibit <- function(x, file, width_in = NULL,
                                            res = 300, ...) {
  summarize_paint_require()
  p <- cards_paint_grob(x, width_in = width_in %||% 10, title = x$title,
                        subtitle = x$subtitle, caption = x$caption)
  paint_write_png(p, file, res = res)
  invisible(file)
}

# --- the painter -------------------------------------------------------------

# Rows of cards per page: as many as fit the height budget, at least one.
#' @noRd
cards_paint_pages <- function(x, width_in, max_height, fs = 10, ...) {
  n <- x$model$n
  one <- cards_paint_grob(x, width_in = width_in, fs = fs, ...)
  if (one$height <= max_height || n <= 1L) return(list(one))
  per <- max(1L, floor((max_height - one$chrome) / one$row_h))
  starts <- seq(1L, n, by = per)
  lapply(starts, function(s) {
    cards_paint_grob(x, width_in = width_in, fs = fs,
                     rows = seq(s, min(n, s + per - 1L)), ...)
  })
}

#' One picture of a card model: row heads, column heads, cards.
#' @noRd
cards_paint_grob <- function(x, width_in = 10, fs = 10, family = "sans",
                             rows = NULL, title = NULL, subtitle = NULL,
                             caption = NULL) {
  md <- x$model
  d <- cards_dims(md, x$cells, width_in, fs, family, rows %||% seq_len(md$n),
                  title, subtitle, caption)
  # The painters add their grobs to `acc$g` through `d$add`; `d$txt` sets one
  # line of text, `d$put` one line cut to `w` inches.
  acc <- new.env()
  acc$g <- list()
  d$add <- function(...) acc$g <- c(acc$g, list(...))
  d$txt <- function(s, x, y, just = "left", size = fs, col = PAINT_TEXT,
                    bold = FALSE, vjust = "top") {
    d$add(grid::textGrob(
      s, x = grid::unit(x, "in"), y = grid::unit(d$H - y, "in"),
      just = c(just, vjust),
      gp = grid::gpar(fontsize = size, col = col, fontfamily = family,
                      fontface = if (bold) "bold" else "plain")))
  }
  d$put <- function(s, w, x, y, size = fs, bold = FALSE, ...) {
    d$txt(cards_fit(s, w, size, family, bold), x, y, size = size, bold = bold,
          ...)
  }
  y <- d$top_pad
  if (d$title_h) {
    d$txt(title, 0, y, size = fs * 1.25, bold = TRUE)
    y <- y + d$title_h
  }
  if (d$subt_h) {
    d$txt(subtitle, 0, y, size = fs * 0.95, col = PAINT_MUTED)
    y <- y + d$subt_h
  }
  if (d$multi) {
    cards_paint_heads(d, y)
    y <- y + d$col_head_h
  }
  cards_paint_rows(d, y)
  y <- y + d$lines * d$row_h
  if (d$fold_h) {
    d$txt(md$fold, 0, y, size = fs * 0.85, col = PAINT_MUTED)
    y <- y + d$fold_h
  }
  if (d$cap_h) {
    d$txt(caption, 0, y, size = fs * 0.85, col = PAINT_MUTED)
  }
  list(
    grob = grid::gTree(children = do.call(grid::gList, acc$g)),
    width = width_in, height = d$H, chrome = d$chrome, row_h = d$row_h
  )
}

# The sizes of a card picture, in inches: one card, the band of heads, the
# lines of cards, and the page height `H`. Returned with the arguments.
#' @noRd
cards_dims <- function(md, cells, width_in, fs, family, rows, title, subtitle,
                       caption) {
  K <- length(md$cols)
  px <- (fs / 72) / 13
  pad <- px * 10
  gap <- px * 8
  multi <- isTRUE(md$multi)
  any_col <- function(f) any(vapply(md$cols, f, logical(1L)))
  # Glyph band: a sparkline wants height, every other mark a lane.
  has_sp <- any_col(function(cl) {
    !is.null(cl$glyph) && identical(cells$cols[[cl$glyph]]$kind, "sparkline")
  })
  any_legend <- any_col(function(cl) length(cl$legend) > 0)
  lab_h <- if (multi) 0 else fs / 72 * 1.4
  num_h <- fs / 72 * 2.6
  sub_h <- if (any_col(function(cl) !is.null(cl$sub))) fs / 72 * 1.3 else 0
  gly_h <- if (!any_col(function(cl) !is.null(cl$glyph))) {
    0
  } else if (has_sp) {
    px * 34
  } else {
    px * 20
  }
  leg_h <- if (!multi && any_legend) fs / 72 * 1.4 else 0
  card_h <- 2 * pad + lab_h + num_h + sub_h + gly_h + leg_h
  head_w <- if (multi) {
    min(width_in * 0.25,
        max(paint_w(md$label[rows], fs, family, bold = TRUE)) + px * 20)
  } else {
    0
  }
  # A single row wraps its cards onto lines of at most `per_line`.
  per_line <- if (multi) K else max(1L, min(K, floor((width_in + gap) / 2.2)))
  lines <- if (multi) length(rows) else ceiling(K / per_line)
  cw <- (width_in - head_w - gap * (per_line - 1L) -
           (if (multi) gap else 0)) / per_line
  col_head_h <- if (multi) fs / 72 * (if (any_legend) 4.1 else 2.8) else 0
  title_h <- if (dd_is_string(title)) fs / 72 * 2.4 else 0
  subt_h <- if (dd_is_string(subtitle)) fs / 72 * 1.7 else 0
  cap_h <- if (dd_is_string(caption)) fs / 72 * 1.8 else 0
  fold_h <- if (!is.null(md$fold)) fs / 72 * 1.6 else 0
  row_h <- card_h + gap
  top_pad <- fs / 72 * 0.4
  chrome <- top_pad + title_h + subt_h + col_head_h + cap_h + fold_h + 0.05
  H <- chrome + lines * row_h
  as.list(environment())
}

# `s` cut to fit `w` inches, with an ellipsis.
#' @noRd
cards_fit <- function(s, w, size, family, bold = FALSE) {
  s <- as.character(s)
  while (nchar(s) > 1L && paint_w(s, size, family, bold) > w) {
    s <- paste0(substr(s, 1L, nchar(s) - 2L), "\u2026")
  }
  s
}

# The column heads of a multi-row table: label, word and range, legend.
#' @noRd
cards_paint_heads <- function(d, y) {
  with(d, for (k in seq_len(K)) {
    cl <- md$cols[[k]]
    x0 <- head_w + gap + (k - 1L) * (cw + gap) + px * 2
    put(cl$label, cw, x0, y, fs * 0.9, bold = TRUE)
    head2 <- paste(c(cl$word, cl$range), collapse = " \u00b7 ")
    if (nzchar(head2)) {
      put(head2, cw, x0, y + fs / 72 * 1.35, fs * 0.8, col = PAINT_MUTED)
    }
    if (length(cl$legend)) {
      cards_paint_legend(cl$legend, x0, y + fs / 72 * 3.2, d)
    }
  })
}

# The cards of every row. A multi-row table has a head and a colour tick per
# row; a single row wraps its cards onto lines.
#' @noRd
cards_paint_rows <- function(d, y) {
  with(d, {
    if (!multi) {
      for (k in seq_len(K)) {
        cards_paint_card(d, rows[[1L]], k, ((k - 1L) %% per_line) * (cw + gap),
                         y + ((k - 1L) %/% per_line) * row_h)
      }
      return(invisible())
    }
    for (r in seq_along(rows)) {
      i <- rows[[r]]
      y0 <- y + (r - 1L) * row_h
      hex <- if (!is.null(md$hex)) md$hex[[i]] else NA
      if (!is.na(hex %||% NA)) {
        add(grid::rectGrob(
          x = grid::unit(0, "in"), y = grid::unit(H - y0 - pad, "in"),
          width = grid::unit(px * 4, "in"),
          height = grid::unit(card_h - 2 * pad, "in"),
          just = c("left", "top"), gp = grid::gpar(fill = hex, col = NA)))
      }
      put(md$label[[i]], head_w - px * 12, px * 10, y0 + card_h / 2, fs,
          bold = TRUE, vjust = "centre")
      for (k in seq_len(K)) {
        cards_paint_card(d, i, k, head_w + gap + (k - 1L) * (cw + gap), y0)
      }
    }
  })
}

# One card: frame, label, main text, sub line, glyph, legend.
#' @noRd
cards_paint_card <- function(d, i, k, x0, y0) {
  with(d, {
    cl <- md$cols[[k]]
    add(grid::roundrectGrob(
      x = grid::unit(x0, "in"), y = grid::unit(H - y0, "in"),
      width = grid::unit(cw, "in"), height = grid::unit(card_h, "in"),
      just = c("left", "top"), r = grid::unit(px * 8, "in"),
      gp = grid::gpar(fill = "#ffffff", col = PAINT_RULE, lwd = 0.8)))
    cx <- x0 + pad
    inner <- cw - 2 * pad
    cy <- y0 + pad
    if (lab_h) {
      put(cl$label, inner, cx, cy, fs * 0.85, col = PAINT_MUTED)
      cy <- cy + lab_h
    }
    plain <- isTRUE(cl$text)
    put(cl$main[[i]], inner, cx, cy, if (plain) fs * 1.1 else fs * 1.9,
        bold = !plain)
    cy <- cy + num_h
    if (sub_h) {
      s <- if (!is.null(cl$sub)) cl$sub[[i]] else ""
      if (!multi && nzchar(s) && !is.null(cl$word)) s <- paste0(cl$word, ": ", s)
      if (nzchar(s)) put(s, inner, cx, cy, fs * 0.8, col = PAINT_MUTED)
      cy <- cy + sub_h
    }
    if (gly_h && !is.null(cl$glyph)) {
      cc <- cells$cols[[cl$glyph]]
      gl <- list(H = H, px = px, lane = px * 12, hair = px, fs = fs,
                 family = family, row_h = gly_h)
      if (identical(cc$kind, "sparkline")) gl$lane <- px * 30
      do.call(add, cards_paint_glyph(cc, i, cx, inner, cy, gl))
      cy <- cy + gly_h
    }
    if (leg_h && length(cl$legend)) {
      cards_paint_legend(cl$legend, cx, cy + leg_h / 2, d)
    }
  })
}

#' One glyph, drawn by the summarize painter's function for its kind.
#' @noRd
cards_paint_glyph <- function(c, i, x, w, ytop, gl) {
  k <- c$kind %||% ""
  # The card prints its number above the glyph, so the glyph carries none.
  c[c("slab", "ldisp", "disp", "pct", "dw")] <- list(NULL)
  if (isTRUE(c$multi)) {
    c$lv <- lapply(c$lv, function(g) {
      g$disp <- NULL
      g
    })
    return(paint_multi(c, i, x, w, ytop, gl, xr = x + w))
  }
  switch(k,
    bar = paint_bar(c, i, x, w, ytop, gl),
    barsplit = paint_barsplit(c, i, x, w, ytop, gl, xr = x + w),
    bardiv = paint_bardiv(c, i, x, w, ytop, gl),
    box = paint_box(c, i, x, w, ytop, gl),
    pointrange = paint_pr(c, i, x, w, ytop, gl),
    pair = paint_pair(c, i, x, w, ytop, gl),
    sparkline = paint_sparkline(c, i, x, w, ytop, gl),
    list()
  )
}

#' A legend line: a swatch and a label per level, centred on `ymid`.
#' @noRd
cards_paint_legend <- function(items, x, ymid, d) {
  at <- x
  sw <- d$px * 8
  for (it in items) {
    d$add(grid::rectGrob(
      x = grid::unit(at, "in"), y = grid::unit(d$H - ymid, "in"),
      width = grid::unit(sw, "in"), height = grid::unit(sw, "in"),
      just = c("left", "centre"), gp = grid::gpar(fill = it$color, col = NA)))
    d$txt(it$label, at + sw + d$px * 3, ymid, size = d$fs * 0.75,
          col = PAINT_MUTED, vjust = "centre")
    at <- at + sw + d$px * 3 + paint_w(it$label, d$fs * 0.75, d$family) +
      d$px * 10
  }
}
