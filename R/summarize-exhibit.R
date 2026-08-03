# The summarize table as an EXHIBIT ------------------------------------------
#
# One object, rendered by whichever back end the target needs. The block
# itself cannot be that object: `new_summarize_table_block()` is a transform
# block whose result is its (filtered) INPUT, and the table exists only as a
# push to rank-table.js. A report or a deck evaluating the board would print
# the input frame -- which is what it did before this file existed.
#
# So the block states how to rebuild its table (report_call), and the rebuilt
# table carries everything every target needs:
#
#   static_summarize_table(data, ...)   -> summarize_exhibit
#     html_exhibit()      -> the hand-rolled HTML table, marks and all
#     pptx_add_exhibit()  -> painted pages, one slide each
#     print()             -> the picture, for an interactive session
#
# Same shape as static_table(), which stashes `exhibit_data` on the flextable
# so the HTML deck can re-render it rather than screenshot it. The rule is the
# same one: an exhibit carries its SOURCE, and each target renders from that.

#' A Summarize Table as a Printable Exhibit
#'
#' The summarize table (`new_summarize_table_block()`) rebuilt outside Shiny,
#' as an object a report, a deck or a download can render. The block draws the
#' table by pushing a cell model to the browser, so its result is only the
#' data; this is what puts the table itself into a document.
#'
#' The returned object renders per target and never re-derives the marks:
#' [html_exhibit()] gives the same HTML table the app draws, and
#' [pptx_add_exhibit()] paints it and places it on as many slides as it needs.
#' Both read one cell model, computed here.
#'
#' On a slide the exhibit is a PICTURE, because PowerPoint's table cells hold
#' text runs only and a distribution glyph cannot live in one. The text is
#' therefore not selectable or editable on those slides. A plain display table
#' has no such constraint and keeps [static_table()].
#'
#' @param data A data frame (or [as_annotated_df()]-coercible object), the
#'   table's input.
#' @param ... Passed to [rank_table()]: the block's own vocabulary (`by`,
#'   `summaries`, `group`, `color`, `facet`, `sort_by`, `top_n`, `title`, ...).
#'
#' @return An object of class `summarize_exhibit`.
#' @seealso [rank_table()], [pptx_add_exhibit()], [html_exhibit()]
#' @examplesIf requireNamespace("systemfonts", quietly = TRUE)
#' ex <- static_summarize_table(
#'   mtcars, by = "cyl",
#'   summaries = list(
#'     list(type = "simple", func = "count", show = "bar", name = "Cars"),
#'     list(type = "dist", col = "mpg", style = "box",
#'          inner = "median_q1_q3", outer = "p10_p90", name = "MPG")
#'   )
#' )
#' ex
#' @export
static_summarize_table <- function(data, ...) {

  args <- list(...)

  # The block resolves its title tier against the data before calling; doing
  # it again is a no-op for a plain string and gives a direct call the same
  # automatic tier (see rank_table()).
  prep <- do.call(rank_prepare, c(list(data), args[rank_prep_args(args)]))
  cells <- rank_cells(
    prep,
    cfg = list(axis = args$axis %||% TRUE, sortable = args$sortable %||% TRUE)
  )

  structure(
    list(
      data = data,
      args = args,
      prep = prep,
      cells = cells,
      title = resolve_block_title(args$title, data,
                                  auto = rank_attr(data, "label")),
      subtitle = resolve_block_title(args$subtitle, data,
                                     auto = rank_attr(data, "subtitle")),
      caption = resolve_block_title(args$caption, data,
                                    auto = rank_attr(data, "caption"))
    ),
    class = c("summarize_exhibit", "blockr_exhibit")
  )
}

# Which of the caller's arguments rank_prepare() takes. Everything else
# (display text, heights, the search toggle) belongs to the chrome and is
# passed on to whichever renderer draws it.
#' @noRd
rank_prep_args <- function(args) {
  names(args) %in% names(formals(rank_prepare))
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
  # An interactive session gets the picture; without a device to draw on
  # (a script, a knit chunk with no plot) the one-line summary still says
  # what the object is.
  ok <- rank_paint_ready() &&
    !identical(grDevices::dev.cur()[[1L]], 1L)
  if (ok) {
    p <- rank_paint_grob(x$cells, x$prep, title = x$title,
                         subtitle = x$subtitle, caption = x$caption)
    grid::grid.newpage()
    grid::grid.draw(p$grob)
  } else {
    cat(format(x), "\n")
  }
  invisible(x)
}

# knitr prints the HTML table in an HTML document and the picture elsewhere,
# through the same routing every other exhibit uses.
#' @exportS3Method knitr::knit_print
knit_print.summarize_exhibit <- function(x, ...) {
  if (exhibit_html_output()) {
    return(knitr::knit_print(html_exhibit(x)))
  }
  knitr::knit_print(rank_paint_image(x))
}

#' @export
html_exhibit.summarize_exhibit <- function(x, title = NULL, caption = NULL,
                                           max_height = NULL,
                                           default_expanded = NULL, ...) {
  args <- x$args
  if (!is.null(title)) args$title <- title
  if (!is.null(caption)) args$caption <- caption
  # A printed exhibit is not a scroll box: the page or the slide gives it the
  # room, so the block's own max_height does not travel with it.
  args$max_height <- max_height
  args$search <- FALSE
  args$elem_id <- NULL
  # A printed table has nobody to click its chevrons, so a nested one opens
  # whole. Same call the painted page makes -- it draws every row too, and
  # the two targets must not disagree about what the table CONTAINS.
  args$expanded <- TRUE
  do.call(rank_table, c(list(x$data), args))
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
                                               # painted table has no column
                                               # deal -- its marks are
                                               # positioned in percent, so
                                               # they narrow with the column
                                               # instead of overflowing it --
                                               # and it pages by row height
                                               # rather than by a row count.
                                               # The file writer passes both
                                               # to whichever method it
                                               # reaches. `min_font_size` is
                                               # honoured: see the ladder
                                               # below.
                                               max_rows = NULL, max_cols = NULL,
                                               min_font_size = NULL,
                                               ...) {

  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("pptx_add_exhibit() needs the 'officer' package.", call. = FALSE)
  }
  rank_paint_require()

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

  # Same geometry the flextable paginator uses: the picture starts below the
  # title's own rendered text rather than at a constant that may sit inside
  # it, and what is left of the slide is the row budget.
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
  budget <- slide_h - top - 0.4

  common <- list(
    x$cells, x$prep,
    width_in = width, max_height = budget,
    title = if (slide_title) "" else (title %||% x$title),
    subtitle = subtitle %||% x$subtitle,
    caption = caption %||% x$caption
  )

  # The same trade the flextable paginator makes, and for the same reason:
  # one slide at 9pt beats two at 10pt. Here it is cheaper -- the rows are
  # uniform by construction, so a candidate size is a division rather than a
  # measured table -- and it matters as much, since a board that has asked to
  # keep its tables on one slide means the summarize tables too.
  #
  # Nothing is painted until the size is settled. `min_font_size` is the same
  # floor the typeset tables use, so the two kinds of table on one deck
  # shrink alike.
  min_font_size <- exhibit_min_font_size(min_font_size)
  fits_one <- function(fs) {
    do.call(rank_paint_per_page, c(common, list(fs = fs))) >= x$cells$n
  }

  if (!fits_one(font_size) && min_font_size < font_size) {
    for (s in seq(font_size - 1, min_font_size)) {
      if (fits_one(s)) {
        font_size <- s
        break
      }
    }
  }

  pages <- do.call(rank_paint_pages, c(common, list(fs = font_size)))

  if (length(pages) > 1L) {
    # Below the floor only to say what it would have taken: the deck still
    # gets the pages it got, and the reader gets the number to set.
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
    rank_paint_png_file(p, f, res = res)
    doc <- officer::add_slide(doc, layout = layout, master = master)
    if (slide_title) {
      doc <- tryCatch(
        officer::ph_with(
          doc, pptx_page_title(title, c(k, length(pages))),
          location = officer::ph_location_type(type = "title")),
        error = function(e) doc)
    }
    # Never scaled: the type was sized for this box, so a picture that does
    # not fit is a paging failure, not something to shrink away.
    doc <- officer::ph_with(
      doc, officer::external_img(f, width = p$width, height = p$height),
      location = officer::ph_location(
        left = (slide_w - p$width) / 2, top = top,
        width = p$width, height = p$height))
  }

  doc
}

# --- the block's report call -------------------------------------------------

#' @rdname report_call
#' @export
report_call.summarize_table_block <- function(x, var, ...) {

  # The committed block's state lives in its constructor closure, the same
  # values serialization reads (see report_call.chart_block).
  env <- environment(x[["expr_server"]])

  state <- function(nm) {
    v <- get0(nm, envir = env, ifnotfound = NULL)
    if (is.function(v)) NULL else v
  }

  # Print-relevant surface only: how the table is BUILT and what it says.
  # Interaction state (drill, the ctrl transports, the runtime filter, the
  # search and sort toggles) is not part of a printed table.
  spec <- list(
    group = NULL, parent = NULL, color = NULL, facet = NULL,
    value = ".count", func = "count", id_var = NULL,
    summaries = NULL, by = NULL, facet_layout = "by_summary",
    bar_mode = "stacked", cols = NULL, fields = NULL,
    sort_by = "value", sort_dir = "desc", top_n = NULL, axis = TRUE
  )

  args <- list()

  for (nm in names(spec)) {
    v <- state(nm)
    if (is.null(v) || (is.character(v) && !any(nzchar(v)))) next
    if (identical_default(v, spec[[nm]])) next
    args[[nm]] <- v
  }

  for (nm in c("title", "subtitle", "caption")) {
    v <- state(nm)
    if (!is.null(v)) args[[nm]] <- as.character(v)[1L]
  }

  as.call(c(
    list(call("::", as.name("blockr.viz"),
              as.name("static_summarize_table")),
         as.name(var)),
    args
  ))
}
