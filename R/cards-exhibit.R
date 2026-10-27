# The cards look as an EXHIBIT -------------------------------------------------
#
# The downloads of a table block in the cards look draw the cards: the web
# page with the browser's own renderer (table-cards.js and the summarize
# table's glyph emitters, inlined by write_exhibit_html()), the slide and the
# image as a picture painted here. The spreadsheet keeps the table's values;
# it never sees this object.
#
# The painter reuses the summarize painter's glyph functions (rp_bar(),
# rp_box(), ... in R/summarize-paint.R), which take one cell of the cell
# model and a box in inches. A card's glyph is that cell in a card-sized box.

#' A table in the cards look, as a printable exhibit
#'
#' @param x A card model (`table_cards_groups()` / `table_cards_records()`),
#'   or a `summarize_exhibit` built with `look = "cards"`.
#' @param title,subtitle,caption Text above and below the cards.
#' @return An object of class `cards_exhibit`.
#' @noRd
cards_exhibit <- function(x, title = NULL, subtitle = NULL, caption = NULL) {
  if (inherits(x, "summarize_exhibit")) {
    return(structure(
      list(model = x$cards, cells = x$cells,
           title = title %||% x$title, subtitle = subtitle %||% x$subtitle,
           caption = caption %||% x$caption),
      class = c("cards_exhibit", "blockr_exhibit")
    ))
  }
  structure(
    list(model = x, cells = NULL, title = title, subtitle = subtitle,
         caption = caption),
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
  title <- title %||% x$title
  subtitle <- subtitle %||% x$subtitle
  caption <- caption %||% x$caption
  data <- list(
    cards = cards_payload(x$model),
    # The glyphs' geometry: the summarize table's columns, which the
    # browser's emitters draw from.
    cols = if (!is.null(x$cells)) summarize_flat_payload(x$cells)$cols
  )
  json <- as.character(jsonlite::toJSON(summarize_drop_null(data),
                                        auto_unbox = TRUE, na = "null"))
  # A "</script>" inside a label must not end the block.
  json <- gsub("</", "<\\/", json, fixed = TRUE)
  htmltools::tagList(
    htmltools::tags$style(htmltools::HTML(html_table_shared_css_fallback())),
    htmltools::tags$style(htmltools::HTML(summarize_table_css())),
    summarize_table_dep(),
    table_cards_dep(),
    htmltools::tags$div(
      class = paste("blockr-html-table-container blockr-summarize-container",
                    "blockr-cards-static"),
      if (summarize_nz(title) || summarize_nz(subtitle)) {
        htmltools::tags$div(
          class = "dd-table-titles",
          if (summarize_nz(title)) {
            htmltools::tags$div(class = "dd-table-title", title)
          },
          if (summarize_nz(subtitle)) {
            htmltools::tags$div(class = "dd-table-subtitle", subtitle)
          }
        )
      },
      htmltools::tags$div(
        class = "blockr-cards-host", `data-cards-static` = "1",
        htmltools::tags$script(type = "application/json",
                               htmltools::HTML(json))
      ),
      if (summarize_nz(caption)) {
        htmltools::tags$div(class = "dd-table-caption", caption)
      }
    )
  )
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
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("pptx_add_exhibit() needs the 'officer' package.", call. = FALSE)
  }
  summarize_paint_require()
  layouts <- officer::layout_summary(doc)
  layout <- layout %||% if ("Title and Content" %in% layouts$layout) {
    "Title and Content"
  } else {
    layouts$layout[[1L]]
  }
  master <- master %||% layouts$master[match(layout, layouts$layout)]
  slide_w <- tryCatch(officer::slide_size(doc)$width,
                      error = function(e) 13.333)
  slide_h <- tryCatch(officer::slide_size(doc)$height,
                      error = function(e) 7.5)
  title <- title %||% x$title
  has_title <- is.character(title) && length(title) == 1L && nzchar(title)
  slide_title <- has_title && pptx_layout_has_title(doc, layout, master)
  top <- top %||% if (slide_title) 1.3 else 0.5
  width <- pptx_content_width(template) %||% (slide_w - 0.8)
  budget <- pptx_body_bottom(doc, layout, master, slide_h) - top

  pages <- cards_paint_pages(
    x, width_in = width, max_height = budget, fs = font_size,
    title = if (slide_title) "" else title,
    subtitle = subtitle %||% x$subtitle, caption = caption %||% x$caption
  )
  for (k in seq_along(pages)) {
    p <- pages[[k]]
    f <- tempfile(fileext = ".png")
    rp_write_png(p, f, res = res)
    doc <- officer::add_slide(doc, layout = layout, master = master)
    if (slide_title) {
      doc <- tryCatch(
        officer::ph_with(
          doc, pptx_page_title(title, c(k, length(pages))),
          location = officer::ph_location_type(type = "title")),
        error = function(e) doc)
    }
    doc <- officer::ph_with(
      doc, officer::external_img(f, width = p$width, height = p$height),
      location = officer::ph_location(
        left = (slide_w - p$width) / 2, top = top,
        width = p$width, height = p$height))
  }
  doc
}

#' @export
write_exhibit_png.cards_exhibit <- function(x, file, width_in = NULL,
                                            res = 300, ...) {
  summarize_paint_require()
  p <- cards_paint_grob(x, width_in = width_in %||% 10, title = x$title,
                        subtitle = x$subtitle, caption = x$caption)
  rp_write_png(p, file, res = res)
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
  cells <- x$cells
  rows <- rows %||% seq_len(md$n)
  K <- length(md$cols)
  px <- (fs / 72) / 13
  pad <- px * 10
  gap <- px * 8
  multi <- isTRUE(md$multi)
  # Glyph band: a sparkline wants height, every other mark a lane.
  has_sp <- any(vapply(md$cols, function(cl) {
    !is.null(cl$glyph) && identical(cells$cols[[cl$glyph + 1L]]$kind,
                                     "sparkline")
  }, logical(1L)))
  any_glyph <- any(vapply(md$cols, function(cl) !is.null(cl$glyph),
                          logical(1L)))
  any_sub <- any(vapply(md$cols, function(cl) !is.null(cl$sub), logical(1L)))
  lab_h <- if (multi) 0 else fs / 72 * 1.4
  num_h <- fs / 72 * 2.6
  sub_h <- if (any_sub) fs / 72 * 1.3 else 0
  gly_h <- if (!any_glyph) 0 else if (has_sp) px * 34 else px * 20
  leg_h <- if (!multi && any(vapply(md$cols, function(cl) length(cl$legend) > 0,
                                    logical(1L)))) fs / 72 * 1.4 else 0
  card_h <- 2 * pad + lab_h + num_h + sub_h + gly_h + leg_h

  head_w <- if (multi) {
    min(width_in * 0.25,
        max(rp_w(md$label[rows], fs, family, bold = TRUE)) + px * 20)
  } else {
    0
  }
  # A single row wraps its cards onto lines of at most `per_line`.
  per_line <- if (multi) K else max(1L, min(K, floor((width_in + gap) / 2.2)))
  lines <- if (multi) length(rows) else ceiling(K / per_line)
  cw <- (width_in - head_w - gap * (per_line - 1L) -
           (if (multi) gap else 0)) / per_line

  head_leg <- multi && any(vapply(md$cols, function(cl) length(cl$legend) > 0,
                                   logical(1L)))
  col_head_h <- if (multi) fs / 72 * (if (head_leg) 4.1 else 2.8) else 0
  title_h <- if (summarize_nz(title)) fs / 72 * 2.4 else 0
  subt_h <- if (summarize_nz(subtitle)) fs / 72 * 1.7 else 0
  cap_h <- if (summarize_nz(caption)) fs / 72 * 1.8 else 0
  fold_h <- if (!is.null(md$fold)) fs / 72 * 1.6 else 0
  row_h <- card_h + gap
  top_pad <- fs / 72 * 0.4
  chrome <- top_pad + title_h + subt_h + col_head_h + cap_h + fold_h + 0.05
  H <- chrome + lines * row_h

  g <- list()
  txt <- function(s, x, y, just = "left", size = fs, col = RP_TEXT,
                  bold = FALSE, vjust = "top") {
    grid::textGrob(s, x = grid::unit(x, "in"), y = grid::unit(H - y, "in"),
                   just = c(just, vjust),
                   gp = grid::gpar(fontsize = size, col = col,
                                   fontfamily = family,
                                   fontface = if (bold) "bold" else "plain"))
  }
  fit <- function(s, w, size, bold = FALSE) {
    s <- as.character(s)
    while (nchar(s) > 1L && rp_w(s, size, family, bold) > w) {
      s <- paste0(substr(s, 1L, nchar(s) - 2L), "\u2026")
    }
    s
  }
  gl <- list(H = H, px = px, lane = px * 12, hair = px, fs = fs,
             family = family)

  y <- top_pad
  if (title_h) {
    g <- c(g, list(txt(title, 0, y, size = fs * 1.25, bold = TRUE)))
    y <- y + title_h
  }
  if (subt_h) {
    g <- c(g, list(txt(subtitle, 0, y, size = fs * 0.95, col = RP_MUTED)))
    y <- y + subt_h
  }
  if (multi) {
    for (k in seq_len(K)) {
      cl <- md$cols[[k]]
      x0 <- head_w + gap + (k - 1L) * (cw + gap)
      g <- c(g, list(txt(fit(cl$label, cw, fs * 0.9, TRUE), x0 + px * 2, y,
                         size = fs * 0.9, bold = TRUE, col = RP_TEXT)))
      head2 <- paste(c(cl$word, cl$range), collapse = " \u00b7 ")
      if (nzchar(head2)) {
        g <- c(g, list(txt(fit(head2, cw, fs * 0.8), x0 + px * 2,
                           y + fs / 72 * 1.35, size = fs * 0.8,
                           col = RP_MUTED)))
      }
      if (length(cl$legend)) {
        g <- c(g, cards_paint_legend(cl$legend, x0 + px * 2,
                                     y + fs / 72 * 3.2, fs, px, family,
                                     H, txt))
      }
    }
    y <- y + col_head_h
  }

  card <- function(i, k, x0, y0) {
    cl <- md$cols[[k]]
    out <- list(grid::roundrectGrob(
      x = grid::unit(x0, "in"), y = grid::unit(H - y0, "in"),
      width = grid::unit(cw, "in"), height = grid::unit(card_h, "in"),
      just = c("left", "top"), r = grid::unit(px * 8, "in"),
      gp = grid::gpar(fill = "#ffffff", col = RP_RULE, lwd = 0.8)))
    cx <- x0 + pad
    inner <- cw - 2 * pad
    cy <- y0 + pad
    if (lab_h) {
      out <- c(out, list(txt(fit(cl$label, inner, fs * 0.85), cx, cy,
                             size = fs * 0.85, col = RP_MUTED)))
      cy <- cy + lab_h
    }
    main <- cl$main[[i]]
    size <- if (isTRUE(cl$text)) fs * 1.1 else fs * 1.9
    out <- c(out, list(txt(fit(main, inner, size, !isTRUE(cl$text)), cx, cy,
                           size = size, bold = !isTRUE(cl$text))))
    cy <- cy + num_h
    if (sub_h) {
      s <- if (!is.null(cl$sub)) cl$sub[[i]] else ""
      if (!multi && nzchar(s) && !is.null(cl$word)) s <- paste0(cl$word, ": ", s)
      if (nzchar(s)) {
        out <- c(out, list(txt(fit(s, inner, fs * 0.8), cx, cy,
                               size = fs * 0.8, col = RP_MUTED)))
      }
      cy <- cy + sub_h
    }
    if (gly_h && !is.null(cl$glyph)) {
      cc <- cells$cols[[cl$glyph + 1L]]
      g1 <- gl
      g1$row_h <- gly_h
      if (identical(cc$kind, "sparkline")) g1$lane <- px * 30
      out <- c(out, cards_paint_glyph(cc, i, cx, inner, cy, g1))
      cy <- cy + gly_h
    }
    if (leg_h && length(cl$legend)) {
      out <- c(out, cards_paint_legend(cl$legend, cx, cy + leg_h / 2, fs, px,
                                       family, H, txt))
    }
    out
  }

  if (multi) {
    for (r in seq_along(rows)) {
      i <- rows[[r]]
      y0 <- y + (r - 1L) * row_h
      hex <- if (!is.null(md$hex)) md$hex[[i]] else NA
      if (!is.na(hex %||% NA)) {
        g <- c(g, list(grid::rectGrob(
          x = grid::unit(0, "in"), y = grid::unit(H - y0 - pad, "in"),
          width = grid::unit(px * 4, "in"),
          height = grid::unit(card_h - 2 * pad, "in"),
          just = c("left", "top"), gp = grid::gpar(fill = hex, col = NA))))
      }
      g <- c(g, list(txt(fit(md$label[[i]], head_w - px * 12, fs, TRUE),
                         px * 10, y0 + card_h / 2, bold = TRUE,
                         vjust = "centre")))
      for (k in seq_len(K)) {
        g <- c(g, card(i, k, head_w + gap + (k - 1L) * (cw + gap), y0))
      }
    }
  } else {
    i <- rows[[1L]]
    for (k in seq_len(K)) {
      ln <- (k - 1L) %/% per_line
      pos <- (k - 1L) %% per_line
      g <- c(g, card(i, k, pos * (cw + gap), y + ln * row_h))
    }
  }
  y <- y + lines * row_h
  if (fold_h) {
    g <- c(g, list(txt(md$fold, 0, y, size = fs * 0.85, col = RP_MUTED)))
    y <- y + fold_h
  }
  if (cap_h) {
    g <- c(g, list(txt(caption, 0, y, size = fs * 0.85, col = RP_MUTED)))
  }
  list(
    grob = grid::gTree(children = do.call(grid::gList,
                                          Filter(Negate(is.null), g))),
    width = width_in, height = H, chrome = chrome, row_h = row_h
  )
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
    return(rp_multi(c, i, x, w, ytop, gl, xr = x + w))
  }
  switch(k,
    bar = rp_bar(c, i, x, w, ytop, gl),
    barsplit = rp_barsplit(c, i, x, w, ytop, gl, xr = x + w),
    bardiv = rp_bardiv(c, i, x, w, ytop, gl),
    box = rp_box(c, i, x, w, ytop, gl),
    pointrange = rp_pr(c, i, x, w, ytop, gl),
    pair = rp_pair(c, i, x, w, ytop, gl),
    sparkline = rp_sparkline(c, i, x, w, ytop, gl),
    list()
  )
}

#' A legend line: a swatch and a label per level, centred on `ymid`.
#' @noRd
cards_paint_legend <- function(items, x, ymid, fs, px, family, H, txt) {
  out <- list()
  at <- x
  sw <- px * 8
  for (it in items) {
    out <- c(out, list(
      grid::rectGrob(x = grid::unit(at, "in"), y = grid::unit(H - ymid, "in"),
                     width = grid::unit(sw, "in"), height = grid::unit(sw, "in"),
                     just = c("left", "centre"),
                     gp = grid::gpar(fill = it$color, col = NA)),
      txt(it$label, at + sw + px * 3, ymid, size = fs * 0.75, col = RP_MUTED,
          vjust = "centre")))
    at <- at + sw + px * 3 + rp_w(it$label, fs * 0.75, family) + px * 10
  }
  out
}
