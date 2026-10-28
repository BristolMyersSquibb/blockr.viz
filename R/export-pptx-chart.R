# A ggplot as an exhibit. HTML gets an image, a slide gets a vector drawing
# sized from the plot's `pptx_width` / `pptx_height` attributes.

# The box a plot is drawn into, in inches: its size attributes or 8 x 4.5.
# `max_width` scales it down, keeping the aspect.
#' @noRd
gg_exhibit_size <- function(p, max_width = NULL) {

  w <- attr(p, "pptx_width") %||% 8
  h <- attr(p, "pptx_height") %||% 4.5

  if (!is.numeric(w) || !length(w) || !is.finite(w) || w <= 0) w <- 8
  if (!is.numeric(h) || !length(h) || !is.finite(h) || h <= 0) h <- 4.5

  if (!is.null(max_width) && is.finite(max_width) && max_width > 0) {
    scale <- min(1, max_width / w)
    w <- w * scale
    h <- h * scale
  }

  list(width = w, height = h)
}

# The plot's own title, or NULL when it has none.
#' @noRd
gg_title <- function(p) {
  lab <- tryCatch(p$labels$title, error = function(e) NULL)
  if (is.null(lab) || !is.character(lab) || !length(lab)) {
    return(NULL)
  }
  lab[[1L]]
}

#' @noRd
gg_write_png <- function(p, file, width, height, res = 300) {

  ragg::agg_png(file, width = width, height = height, units = "in",
                res = res, background = "white")
  on.exit(grDevices::dev.off())

  print(p)

  invisible(file)
}

#' @export
html_exhibit.gg <- function(x, title = NULL, subtitle = NULL, caption = NULL,
                            max_height = NULL,
                            default_expanded = NULL, ...) {

  # The plot is inlined as a PNG data URI.
  size <- gg_exhibit_size(x, max_width = 8)

  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)
  gg_write_png(x, f, size$width, size$height, res = 144)

  htmltools::tags$img(
    class = "blockr-exhibit-img",
    src = paste0("data:image/png;base64,", gsub(
      "\n", "", jsonlite::base64_enc(readBin(f, "raw", file.size(f))),
      fixed = TRUE
    )),
    style = "max-width:100%;height:auto;display:block;"
  )
}

# A new slide with `make(w, h)` placed centred below `top`, scaled to fill the
# slide while keeping the aspect of `size` (width and height in inches). The
# fit is capped at `cap` and multiplied by `scale`.
pptx_place_fitted <- function(doc, ctx, make, size, top, title = NULL,
                              scale = 1, cap = Inf) {
  fit <- min((ctx$width - 0.8) / size$width,
             (ctx$height - top - 0.4) / size$height, cap) * scale
  w <- size$width * fit
  h <- size$height * fit
  officer::ph_with(
    pptx_blank_slide(doc, ctx, title), make(w, h),
    location = officer::ph_location(left = (ctx$width - w) / 2, top = top,
                                    width = w, height = h)
  )
}

#' @export
pptx_add_exhibit.gg <- function(doc, x, title = NULL, subtitle = NULL,
                                caption = NULL, template = NULL, layout = NULL,
                                master = NULL, top = NULL,
                                # Accepted and ignored: a plot is one slide.
                                max_rows = NULL, max_cols = NULL,
                                min_font_size = NULL, ...) {

  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("pptx_add_exhibit() needs the 'officer' package.", call. = FALSE)
  }

  ctx <- pptx_slide_context(doc, title, layout, master)
  slide_title <- ctx$slide_title

  top <- top %||% pptx_table_top(doc, ctx, template %||% "", title,
                                 attr(x, "pptx_top"))

  # When the slide title and the plot's own title are the same line, the
  # placeholder keeps it and the plot drops its copy. Different titles both
  # stay. The subtitle is never touched.
  if (slide_title && identical(trimws(title), trimws(gg_title(x) %||% "")) &&
        requireNamespace("ggplot2", quietly = TRUE)) {
    stripped <- tryCatch(x + ggplot2::labs(title = NULL),
                         error = function(e) NULL)
    if (!is.null(stripped)) {
      # `+` drops the size attributes, so they are copied back.
      attributes(stripped) <- utils::modifyList(
        attributes(stripped),
        attributes(x)[setdiff(names(attributes(x)), names(attributes(stripped)))]
      )
      x <- stripped
    }
  }

  # The plot scales up or down to fill the box below `top`, keeping its
  # aspect. officer re-renders it at the placed size, so type stays at its
  # point size. `blockr.viz.gg_slide_fill` multiplies the fitted size.
  fill <- getOption("blockr.viz.gg_slide_fill", 1)
  if (!is.numeric(fill) || length(fill) != 1L || !is.finite(fill) ||
        fill <= 0) {
    fill <- 1
  }
  pptx_place_fitted(doc, ctx, function(w, h) x, gg_exhibit_size(x), top,
                    if (slide_title) title, scale = fill)
}

# A chart the browser drew, as one slide.
#' @export
pptx_add_exhibit.chart_capture <- function(doc, x, title = NULL,
                                           subtitle = NULL, caption = NULL,
                                           template = NULL, layout = NULL,
                                           master = NULL, top = NULL,
                                           max_rows = NULL, max_cols = NULL,
                                           min_font_size = NULL, ...) {

  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("pptx_add_exhibit() needs the 'officer' package.", call. = FALSE)
  }

  ctx <- pptx_slide_context(doc, title, layout, master)
  slide_title <- ctx$slide_title

  top <- top %||% if (slide_title) {
    pptx_title_bottom(doc, ctx$layout, ctx$master, template %||% "", title) %||% 1.1
  } else {
    1.1
  }

  # The capture was drawn at pixel ratio 2, so it may grow to twice its size
  # and no further.
  pptx_place_fitted(
    doc, ctx,
    function(w, h) {
      officer::external_img(chart_capture_file(x), width = w, height = h)
    },
    x[c("width", "height")], top, if (slide_title) title, cap = 2
  )
}
