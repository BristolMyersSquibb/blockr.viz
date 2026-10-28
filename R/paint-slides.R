# Painted pages on slides: the summarize table and the cards look are
# pictures, so a slide holds one image per page (see R/paint-table.R).

# The packages a painted slide needs, checked before anything is painted.
pptx_paint_require <- function() {
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("pptx_add_exhibit() needs the 'officer' package.", call. = FALSE)
  }
  summarize_paint_require()
}

# One slide per painted page, centred, at the size it was painted for. `ctx`
# is pptx_slide_context().
pptx_add_pictures <- function(doc, pages, ctx, top, title, res) {
  for (k in seq_along(pages)) {
    p <- pages[[k]]
    f <- tempfile(fileext = ".png")
    paint_write_png(p, f, res = res)
    doc <- pptx_blank_slide(
      doc, ctx, if (ctx$slide_title) pptx_page_title(title, c(k, length(pages)))
    )
    doc <- officer::ph_with(
      doc, officer::external_img(f, width = p$width, height = p$height),
      location = officer::ph_location(
        left = (ctx$width - p$width) / 2, top = top,
        width = p$width, height = p$height))
  }
  doc
}
