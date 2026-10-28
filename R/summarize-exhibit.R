# The summarize table as an exhibit -------------------------------------------
#
# The table block's result is its input; the table exists only as a payload to
# the browser. static_summarize_table() rebuilds it as an exhibit that carries
# its source, and each target renders from that: html_exhibit() gives the HTML
# table, pptx_add_exhibit() painted pages, print() the picture.

#' A Summarize Table as a Printable Exhibit
#'
#' The groups layout of the table block (`new_table_block(layout = "groups")`)
#' rebuilt outside Shiny, as an object a report, a deck or a download can
#' render. The block's own result is only its input data.
#'
#' [html_exhibit()] gives the HTML table the app draws, and
#' [pptx_add_exhibit()] paints it onto as many slides as it needs. On a slide
#' the exhibit is a picture, because a PowerPoint table cell cannot hold a
#' distribution glyph; the text is not selectable there. A plain display table
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
  args <- summarize_legacy_args(list(...))
  targs <- args$.title_args %||% summarize_title_args(args)
  args$.title_args <- NULL

  prep <- do.call(groups_prepare,
                  c(list(data),
                    args[names(args) %in% names(formals(groups_prepare))]))
  cells <- groups_cells(
    prep,
    cfg = list(axis = args$axis %||% TRUE, sortable = args$sortable %||% TRUE,
               value_labels = args$value_labels %||% TRUE,
               bar_width = args$bar_width)
  )
  text <- function(role, attr = role) {
    resolve_block_title(args[[role]], data, auto = groups_attr(data, attr),
                        args = targs)
  }

  structure(
    list(
      data = data, args = args, prep = prep, cells = cells,
      # The cards look's text (R/table-cards.R).
      cards = if (identical(args$look, "cards") && is.null(prep$err)) {
        table_cards_groups(cells, prep, data = data,
                           scale_map = args$scale_map)
      },
      title = text("title", "label"), subtitle = text("subtitle"),
      caption = text("caption")
    ),
    class = c("summarize_exhibit", "blockr_exhibit")
  )
}

# The settings a title, subtitle or caption can name as `{@arg}` or
# `{label(@arg)}`. `by` is the innermost grouping column and an outer one is
# the parent; the first summary stands in for the ranked bar's block settings.
#' @noRd
summarize_title_args <- function(args) {
  by <- as.character(unlist(args$by %||% character()))
  s1 <- if (length(args$summaries) && is.list(args$summaries[[1L]])) {
    args$summaries[[1L]]
  } else {
    list()
  }
  out <- list(
    by = if (length(by)) by[[length(by)]],
    group = if (length(by)) by[[1L]],
    parent = if (length(by) == 2L) by[[1L]],
    value = chr_state(s1[["col"]]),
    func = chr_state(s1[["func"]]),
    id_var = args$id_var,
    color = chr_state(s1[["color"]]),
    facet = chr_state(s1[["facet"]]),
    fill = chr_state(s1[["fill"]]),
    sort_by = args$sort_by, top_n = args$top_n,
    drill = args$drill
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
  # The picture needs an open device; otherwise print the summary line.
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
  pptx_paint_require()
  s <- pptx_slide_context(doc, title, layout, master)
  # The picture starts below the title's rendered text.
  top <- top %||% max(
    1.1,
    if (s$slide_title) {
      pptx_title_bottom(doc, s$layout, s$master, template %||% "",
                        pptx_page_title(title, c(99L, 99L))) %||% 0
    } else {
      0
    }
  )
  common <- list(
    x$cells, x$prep,
    width_in = pptx_content_width(template) %||% (s$width - 0.8),
    max_height = pptx_body_bottom(doc, s$layout, s$master, s$height) - top,
    title = if (s$slide_title) "" else (title %||% x$title),
    subtitle = subtitle %||% x$subtitle,
    caption = caption %||% x$caption
  )
  pages <- summarize_pptx_pages(common, x$cells$n, title %||% x$title,
                                font_size, exhibit_min_font_size(min_font_size))
  pptx_add_pictures(doc, pages, s, top, title, res)
}

# The pages of a painted table. One slide at a smaller size beats two at the
# full size, down to `min_font_size`; nothing is painted until the size is set.
#' @noRd
summarize_pptx_pages <- function(common, n, title, font_size, min_font_size) {
  fits_one <- function(fs) {
    do.call(summarize_paint_per_page, c(common, list(fs = fs))) >= n
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
    exhibit_split_note(title, pages = length(pages), size = font_size,
                       floor = min_font_size, fit_size = fit)
  }
  pages
}

# The single ranked bar's arguments as the column list they map to
# (table_legacy_args()); `id_var` is passed through for the title text.
#' @noRd
summarize_legacy_args <- function(args) {
  ranked <- c("group", "value", "func", "parent", "color", "bar_mode",
              "facet", "cols", "fields")
  set <- !vapply(args, is.null, logical(1L))
  if (!any(names(args)[set] %in% ranked)) return(args[!names(args) %in% ranked])
  out <- table_legacy_args(args, from = "summarize")
  out$id_var <- out[["id"]] %||% args[["id_var"]]
  out[setdiff(names(out), c("id", "layout", "selection"))]
}
