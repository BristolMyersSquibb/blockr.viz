# The chart as an exhibit ------------------------------------------------------
#
# A chart already had a printed form -- report_call.chart_block() compiles the
# block's state to a plain dplyr + ggplot2 pipeline, which is what a deck
# evaluates and places. What it did not have was a way to take that same thing
# out of the BLOCK: the download button in the gear header captured the live
# ECharts canvases instead, so the file you got from the block and the picture
# on the slide were two different renderings of one chart.
#
# These methods close that. `static_chart()` (or the compiled pipeline) is the
# one renderer; every target draws from its ggplot:
#
#   xlsx  the aggregated frame the plot was built from
#   html  that plot as an image, in the exhibit document shell
#   pptx  that plot on a slide, sized as the deck sizes it
#   png   that plot as an image file
#
# Same rule as the summarize table's exhibit next door: an exhibit carries its
# source and each target renders from it, rather than each caller inventing a
# rendering.

# The box a chart is drawn into. static_chart() measures it from the chart's
# own row geometry and leaves it on the plot, so a download and a slide agree
# about the aspect; a plain ggplot from anywhere else takes the deck default.
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

  size <- gg_exhibit_size(x)

  # FILL the box that is left, keeping the aspect the chart asked for.
  #
  # `pptx_width` / `pptx_height` are the size the chart wants to be READ at
  # (static_chart() derives them from its row geometry), not the size of the
  # slide it lands on. Placing a plot at that size and stopping left an 8in
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

#' The Numbers Behind a Chart
#'
#' The aggregated frame a chart was drawn from -- what `dplyr::count()` or
#' `summarise()` produced inside the compiled pipeline, which is one row per
#' mark rather than the block's input. This is what the chart block's Excel
#' download writes.
#'
#' @param p A ggplot built by [static_chart()] or [chart_expr()].
#' @return A data frame, or `NULL` when the plot carries no frame.
#' @noRd
chart_exhibit_data <- function(p) {

  d <- tryCatch(p$data, error = function(e) NULL)

  if (is.null(d) || !is.data.frame(d) || !nrow(d)) {
    return(NULL)
  }

  as.data.frame(d, stringsAsFactors = FALSE)
}

#' The chart a report would print, built here.
#'
#' The block's downloads and blockr.outline's deck must not be two renderings
#' of one chart. `report_call.chart_block()` EMITS a call (a document wants
#' code it can read); this evaluates the same thing for a caller that wants the
#' object. Both follow `getOption("blockr.viz.report_style")`, so whichever
#' route a board is on, the picture is the same picture.
#'
#' @param data The block's (filtered) input.
#' @param state The chart's print-relevant state, as `chart_report_state()`
#'   collects it.
#' @return A ggplot, or `NULL` when the state describes no chart.
#' @noRd
chart_static_exhibit <- function(data, state) {

  if (!is.data.frame(data) || !nrow(data)) {
    return(NULL)
  }

  style <- getOption("blockr.viz.report_style", "code")

  if (identical(style, "code")) {
    ex <- tryCatch(
      do.call(chart_expr, c(list(var = "data", data = data, qualify = TRUE),
                            state)),
      error = function(e) NULL
    )
    if (!is.null(ex)) {
      # Self-qualified by construction, so the only thing the evaluation
      # environment has to carry is the data itself.
      out <- tryCatch(eval(ex, list(data = data), baseenv()),
                      error = function(e) NULL)
      if (!is.null(out)) {
        return(out)
      }
    }
  }

  keep <- names(state) %in% names(formals(static_chart))

  tryCatch(do.call(static_chart, c(list(data), state[keep])),
           error = function(e) NULL)
}
