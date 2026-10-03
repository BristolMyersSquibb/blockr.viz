# A ggplot as an exhibit -----------------------------------------------------
#
# The exhibit writers (write_exhibit_html(), write_exhibit_pptx(),
# write_exhibit_png()) take any ggplot a block hands them: the patient
# profile's static plots, say, or a ggplot block's result. A plot goes into
# HTML as an image and onto a slide as a vector drawing, sized from the
# `pptx_width` / `pptx_height` it carries, or the deck default.

# The box a plot is drawn into: the size it carries as attributes, or the
# deck default.
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

# A plot's own title, whatever ggplot2 version wrote it. Used to tell a
# duplicate slide title from a different one; NULL when the plot has none.
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

  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(file, width = width, height = height, units = "in",
                  res = res, background = "white")
  } else {
    grDevices::png(file, width = width * res, height = height * res, res = res,
                   bg = "white")
  }
  on.exit(grDevices::dev.off())

  print(p)

  invisible(file)
}

#' @export
html_exhibit.gg <- function(x, title = NULL, caption = NULL, max_height = NULL,
                            default_expanded = NULL, ...) {

  # A chart in an HTML document is an image of the rendered plot, at the size
  # the plot says it wants -- the same thing blockr.outline's deck puts on an
  # HTML slide. Inlined as a data URI, so the page carries it.
  size <- gg_exhibit_size(x, max_width = 8)

  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)
  gg_write_png(x, f, size$width, size$height, res = 144)

  htmltools::tags$img(
    class = "blockr-exhibit-img",
    src = if (requireNamespace("base64enc", quietly = TRUE)) {
      base64enc::dataURI(file = f, mime = "image/png")
    } else if (requireNamespace("knitr", quietly = TRUE)) {
      knitr::image_uri(f)
    } else {
      stop("Writing a chart to HTML needs 'base64enc' or 'knitr'.",
           call. = FALSE)
    },
    style = "max-width:100%;height:auto;display:block;"
  )
}

#' @export
pptx_add_exhibit.gg <- function(doc, x, title = NULL, subtitle = NULL,
                                caption = NULL, template = NULL, layout = NULL,
                                master = NULL, top = NULL,
                                # Accepted and ignored: the table paginator's
                                # knobs. A plot is one slide by definition.
                                max_rows = NULL, max_cols = NULL,
                                min_font_size = NULL, ...) {

  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("pptx_add_exhibit() needs the 'officer' package.", call. = FALSE)
  }

  layouts <- officer::layout_summary(doc)
  layout <- layout %||% if ("Title and Content" %in% layouts$layout) {
    "Title and Content"
  } else {
    layouts$layout[[1L]]
  }
  master <- master %||% layouts$master[match(layout, layouts$layout)]

  slide_w <- tryCatch(officer::slide_size(doc)$width, error = function(e) 13.333)
  slide_h <- tryCatch(officer::slide_size(doc)$height, error = function(e) 7.5)

  has_title <- is.character(title) && length(title) == 1L && nzchar(title)
  slide_title <- has_title && pptx_layout_has_title(doc, layout, master)

  top <- top %||% max(
    attr(x, "pptx_top") %||% 1.1,
    if (slide_title) {
      pptx_title_bottom(doc, layout, master, template %||% "", title) %||% 0
    } else {
      0
    }
  )

  # The slide's title placeholder and the plot's own title band can print the
  # same line twice, one above the other, in two sizes -- and the band costs
  # the picture the height it takes. Where they SAY THE SAME THING the
  # placeholder wins: it inherits the deck's styling and an editor can retype
  # it. Where they differ they both stay, because then they are two facts: a
  # deck titles its slides with the BLOCK NAME ("3. Chart"), and dropping the
  # chart's own title there would throw away the informative one.
  #
  # The subtitle is never touched: a placeholder holds one line, and the
  # subtitle belongs next to the marks.
  if (slide_title && identical(trimws(title), trimws(gg_title(x) %||% "")) &&
        requireNamespace("ggplot2", quietly = TRUE)) {
    stripped <- tryCatch(x + ggplot2::labs(title = NULL),
                         error = function(e) NULL)
    if (!is.null(stripped)) {
      # `+` returns a bare ggplot: the size attributes on the original do not
      # survive it, and they are the placement.
      attributes(stripped) <- utils::modifyList(
        attributes(stripped),
        attributes(x)[setdiff(names(attributes(x)), names(attributes(stripped)))]
      )
      x <- stripped
    }
  }

  size <- gg_exhibit_size(x)

  # FILL the box that is left, keeping the aspect the chart asked for.
  #
  # `pptx_width` / `pptx_height` are the size the chart wants to be READ at,
  # not the size of the slide it lands on. Placing a plot at that size and stopping left an 8in
  # figure floating in the middle of a 12.5in slide with a hand's width of
  # margin on each side -- the deck looked unfinished, and the axis labels
  # were smaller than they needed to be for no reason.
  #
  # So the plot scales UP as well as down, until the first edge of the
  # content box is reached. Officer re-renders the ggplot at the placed size
  # rather than blowing up pixels, so this is a bigger drawing, not a coarser
  # one -- type stays at its point size, which is why the relative weight of
  # the labels drops as the panel grows. `blockr.viz.gg_slide_fill` caps it
  # for a deck that wants the older, smaller figure back.
  fill <- getOption("blockr.viz.gg_slide_fill", 1)
  if (!is.numeric(fill) || length(fill) != 1L || !is.finite(fill) ||
        fill <= 0) {
    fill <- 1
  }

  fit <- min((slide_w - 0.8) / size$width,
             (slide_h - top - 0.4) / size$height) * fill
  w <- size$width * fit
  h <- size$height * fit

  doc <- officer::add_slide(doc, layout = layout, master = master)

  if (slide_title) {
    doc <- tryCatch(
      officer::ph_with(doc, title,
                       location = officer::ph_location_type(type = "title")),
      error = function(e) doc)
  }

  officer::ph_with(
    doc, x,
    location = officer::ph_location(left = (slide_w - w) / 2, top = top,
                                    width = w, height = h)
  )
}
