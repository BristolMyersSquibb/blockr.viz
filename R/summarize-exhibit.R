# The summarize table as an exhibit -------------------------------------------
#
# The block is a transform block whose result is its input; the table exists
# only as a payload to the browser. So the block states how to rebuild its
# table (report_call), and the rebuilt table carries what every target needs:
#
#   static_summarize_table(data, ...)   -> summarize_exhibit
#     html_exhibit()      -> the HTML table, marks and all
#     pptx_add_exhibit()  -> painted pages, one slide each
#     print()             -> the picture, for an interactive session
#
# As with static_table(), an exhibit carries its source and each target
# renders from that.

#' A Summarize Table as a Printable Exhibit
#'
#' The groups layout of the table block (`new_table_block(layout = "groups")`)
#' rebuilt outside Shiny, as an object a report, a deck or a download can
#' render. The block's own result is only its input data; this puts the table
#' itself into a document.
#'
#' [html_exhibit()] gives the same HTML table the app draws, and
#' [pptx_add_exhibit()] paints it and places it on as many slides as it needs.
#' Both read one cell model, computed here.
#'
#' On a slide the exhibit is a picture, because PowerPoint's table cells hold
#' text runs only and a distribution glyph cannot live in one. The text is
#' therefore not selectable or editable on those slides. A plain display table
#' keeps [static_table()].
#'
#' @param data A data frame (or [as_annotated_df()]-coercible object), the
#'   table's input.
#' @param ... The groups layout's arguments, as in [new_table_block()]
#'   (`by`, `summaries`, `sort_by`, `top_n`, `title`, ...). The single ranked
#'   bar's arguments (`group`, `func`, `value`, `color`, `facet`, ...) are
#'   read as the column list they map to.
#'
#' @return An object of class `summarize_exhibit`.
#' @seealso [pptx_add_exhibit()], [html_exhibit()]
#' @examplesIf requireNamespace("systemfonts", quietly = TRUE)
#' ex <- static_summarize_table(
#'   mtcars, by = "cyl",
#'   summaries = list(
#'     list(type = "simple", func = "count", show = "bar", name = "Cars"),
#'     list(type = "dist", col = "mpg", show = "box",
#'          summary = "median_q1_q3", whiskers = "p10_p90", name = "MPG")
#'   )
#' )
#' ex
#' @export
static_summarize_table <- function(data, ...) {
  args <- list(...)
  extra <- args[intersect(names(args), c(".title_args", "scale_map", "subject"))]
  state <- groups_exhibit_state(args[setdiff(names(args), names(extra))])
  m <- table_model_groups(state, data, scale_map = extra$scale_map,
                          subject = extra$subject)
  targs <- extra$.title_args %||%
    table_groups_title_args(viz_block_spec("table"), state)
  title <- function(nm, auto) {
    resolve_block_title(state[[nm]], data, auto = groups_attr(data, auto),
                        args = targs)
  }
  groups_exhibit(m, data, state, title("title", "label"),
                 title("subtitle", "subtitle"), title("caption", "caption"),
                 scale_map = extra$scale_map)
}

# The arguments of a static groups table as the block's state: old names
# translated, every unset argument at the spec's default.
groups_exhibit_state <- function(args) {
  args <- legacy_table_args(args, from = "summarize")
  args$layout <- "groups"
  table_normalize(viz_check(viz_block_spec("table"),
                            legacy_ctor_args("table", args)))
}

# The exhibit of a computed groups model (table_model_groups()): what every
# download target renders, the screen's numbers and marks.
groups_exhibit <- function(m, data, state, title = NULL, subtitle = NULL,
                           caption = NULL, scale_map = NULL) {
  structure(
    list(
      data = data, args = state, prep = m$prep, cells = m$cells,
      # The cards look's text (R/table-cards.R); the spreadsheet ignores it.
      cards = if (identical(state$look, "cards") && is.null(m$prep$err)) {
        table_cards_groups(m$cells, m$prep, data = data, scale_map = scale_map)
      },
      title = title, subtitle = subtitle, caption = caption
    ),
    class = c("summarize_exhibit", "blockr_exhibit")
  )
}

# The settings a groups title can name beyond the spec's slots: the first
# summary's roles, for the single ranked bar's sentences. `by` is the
# innermost grouping column (each row); an outer one is the parent. Unset
# roles go, so a prepare script's value of the same name can stand in.
groups_title_roles <- function(v) {
  by <- as.character(unlist(v$by %||% character()))
  s1 <- if (length(v$summaries) && is.list(v$summaries[[1L]])) {
    v$summaries[[1L]]
  } else {
    list()
  }
  out <- list(
    by = if (length(by)) by[[length(by)]],
    group = if (length(by) == 2L) by[[1L]] else if (length(by)) by[[1L]],
    parent = if (length(by) == 2L) by[[1L]],
    value = chr_state(s1[["col"]]),
    func = chr_state(s1[["func"]]),
    color = chr_state(s1[["color"]]),
    facet = chr_state(s1[["facet"]]),
    fill = chr_state(s1[["fill"]])
  )
  out[!vapply(out, is.null, logical(1L))]
}

#' @export
format.summarize_exhibit <- function(x, ...) {
  paste0(
    "<summarize_exhibit: ", x$cells$n, " rows, ",
    length(x$cells$cols), " columns (",
    paste(unique(vapply(x$cells$cols, function(c) c$kind %||% "num",
                        character(1L))), collapse = ", "), ")>"
  )
}

#' @export
print.summarize_exhibit <- function(x, ...) {
  # The picture needs an open device; otherwise print the one-line summary.
  ok <- summarize_paint_ready() &&
    !identical(grDevices::dev.cur()[[1L]], 1L)
  if (ok) {
    p <- summarize_paint_grob(x$cells, x$prep, title = x$title,
                              subtitle = x$subtitle, caption = x$caption)
    grid::grid.newpage()
    grid::grid.draw(p$grob)
  } else {
    cat(format(x), "\n")
  }
  invisible(x)
}

# The HTML table in an HTML document, the picture elsewhere.
#' @exportS3Method knitr::knit_print
knit_print.summarize_exhibit <- function(x, ...) {
  if (exhibit_html_output()) {
    return(knitr::knit_print(html_exhibit(x)))
  }
  knitr::knit_print(summarize_paint_image(x))
}

#' @export
html_exhibit.summarize_exhibit <- function(x, title = NULL, subtitle = NULL,
                                           caption = NULL,
                                           max_height = NULL,
                                           default_expanded = NULL, ...) {
  # The table view draws it from the cell model the exhibit computed, so the
  # web page carries the numbers the spreadsheet and the slides carry.
  g <- table_static_groups(x)
  g$derived$titles <- table_static_titles(title %||% x$title,
                                          subtitle %||% x$subtitle,
                                          caption %||% x$caption)
  table_static(g$model, g$config, g$derived, max_height = max_height)
}

#' @export
pptx_add_exhibit.summarize_exhibit <- function(doc, x, title = NULL,
                                               subtitle = NULL, caption = NULL,
                                               template = NULL, layout = NULL,
                                               master = NULL, top = NULL,
                                               font_size = 10,
                                               res = getOption(
                                                 "blockr.viz.paint_res", 300),
                                               # Accepted and ignored: a
                                               # painted table pages by row
                                               # height and has no column deal.
                                               max_rows = NULL, max_cols = NULL,
                                               min_font_size = NULL,
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

  slide_w <- tryCatch(officer::slide_size(doc)$width, error = function(e) 13.333)
  slide_h <- tryCatch(officer::slide_size(doc)$height, error = function(e) 7.5)

  has_title <- is.character(title) && length(title) == 1L && nzchar(title)
  slide_title <- has_title && pptx_layout_has_title(doc, layout, master)

  # The picture starts below the title's rendered text; the rest of the slide,
  # down to the top of the layout's footer, is the row budget.
  top <- top %||% max(
    1.1,
    if (slide_title) {
      pptx_title_bottom(doc, layout, master, template %||% "",
                        pptx_page_title(title, c(99L, 99L))) %||% 0
    } else {
      0
    }
  )
  width <- pptx_content_width(template) %||% (slide_w - 0.8)
  budget <- pptx_body_bottom(doc, layout, master, slide_h) - top

  common <- list(
    x$cells, x$prep,
    width_in = width, max_height = budget,
    title = if (slide_title) "" else (title %||% x$title),
    subtitle = subtitle %||% x$subtitle,
    caption = caption %||% x$caption
  )

  # One slide at a smaller size beats two at the full size, down to
  # `min_font_size`. Nothing is painted until the size is settled.
  min_font_size <- exhibit_min_font_size(min_font_size)
  fits_one <- function(fs) {
    do.call(summarize_paint_per_page, c(common, list(fs = fs))) >= x$cells$n
  }

  if (!fits_one(font_size) && min_font_size < font_size) {
    for (s in seq(font_size - 1, min_font_size)) {
      if (fits_one(s)) {
        font_size <- s
        break
      }
    }
  }

  pages <- do.call(summarize_paint_pages, c(common, list(fs = font_size)))

  if (length(pages) > 1L) {
    # Search below the floor only to tell the reader the size that would fit.
    fit <- NULL
    below <- seq_len(max(0, min(font_size, min_font_size) - MIN_FONT_FLOOR))
    for (s in min(font_size, min_font_size) - below) {
      if (fits_one(s)) {
        fit <- s
        break
      }
    }
    exhibit_split_note(title %||% x$title, pages = length(pages),
                       size = font_size, floor = min_font_size,
                       fit_size = fit)
  }

  for (k in seq_along(pages)) {
    p <- pages[[k]]
    f <- tempfile(fileext = ".png")
    paint_write_png(p, f, res = res)
    doc <- officer::add_slide(doc, layout = layout, master = master)
    if (slide_title) {
      doc <- tryCatch(
        officer::ph_with(
          doc, pptx_page_title(title, c(k, length(pages))),
          location = officer::ph_location_type(type = "title")),
        error = function(e) doc)
    }
    # Never scaled: the type was sized for this box.
    doc <- officer::ph_with(
      doc, officer::external_img(f, width = p$width, height = p$height),
      location = officer::ph_location(
        left = (slide_w - p$width) / 2, top = top,
        width = p$width, height = p$height))
  }

  doc
}
