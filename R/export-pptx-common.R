#' Write an Exhibit to a PowerPoint Deck
#'
#' The pptx sibling of [write_exhibit_html()] and [write_annotated_xlsx()]: the
#' annotated data frame typeset by [static_table()] and placed on a slide of
#' the house template. A table too tall for one slide is carried over onto as
#' many as it needs, each repeating the header (see `max_rows`). A table too
#' wide has its columns dealt over several slides (see `max_cols`).
#'
#' The table is a native PowerPoint table: every cell is real text an editor
#' can retype, restyle and recolour.
#'
#' @section The template:
#' The reference deck supplies the slide size, the master layouts, the title
#' placeholder and the theme fonts. It is resolved, first hit wins, from:
#' `template`, `getOption("blockr.viz.pptx_template")`,
#' `getOption("blockr.outline.template")`, then blockr.outline's bundled
#' widescreen default when that package is installed. With none of them,
#' officer writes its own stock deck.
#'
#' The template's content width sizes the table's columns to fill the slide
#' (PowerPoint never autofits a flextable). Its theme's body font is the
#' table's font unless `blockr.viz.ft_font` is set.
#'
#' @param x A data frame or [as_annotated_df()]-coercible table object.
#' @param file Path to write the `.pptx` to.
#' @param title,subtitle,caption Slide title, table subtitle and footnote.
#'   `NULL` or `""` omits each. The title goes into the layout's title
#'   placeholder when the template has one and into the table's own header
#'   otherwise.
#' @param template Path to a reference `.pptx`, or `NULL` (default) to resolve
#'   one as described above.
#' @param max_rows How a table too tall for its slide is handled. `"auto"`
#'   (default) carries it onto further slides, cutting where the measured
#'   height of the rendered rows runs out of slide. A number cuts every
#'   `max_rows` input rows instead. `NULL` never splits.
#'
#'   Every page repeats the title, the column header band and the footnote.
#'   The title is marked `(2 of 3)` and a section carried across a break
#'   repeats its heading marked `(continued)`. Column widths are measured once
#'   on the whole table, so the columns line up across slides. A break never
#'   leaves one or two rows of a section at the top of a slide: the whole
#'   section moves over.
#' @param max_cols How a table too wide for its slide is handled, once the
#'   font has stepped down as far as `min_font_size` allows and the columns
#'   still do not fit. `"auto"` (default) deals them over several sets of
#'   slides, each repeating the row stub. A set is a whole number of spanner
#'   groups. A number puts at most that many data columns on a slide. `NULL`
#'   never splits and squeezes the columns as far as they go.
#'
#'   This is the last resort. The columns come apart only when the data cells
#'   no longer fit their own longest word. Everything short of that is
#'   absorbed: the font steps down to `min_font_size`, the stub wraps
#'   (`getOption("blockr.viz.ft_stub_share")` is the share of the width it
#'   claims while the data columns are short), cells wrap between words and
#'   headers break inside words. Set `getOption("blockr.viz.ft_header_break_tol")`
#'   (default 0, off) to also deal the columns when a header word is cut past
#'   that share of itself.
#' @param min_font_size Points. The floor for the two shrink passes. `NULL`
#'   (default) resolves it, first hit wins, from the board's
#'   `exhibit_min_font_size` option ([new_exhibit_font_option()]), then
#'   `getOption("blockr.viz.ft_min_font_size")`, then 11pt. The font steps
#'   down first until cells fit and headers stop breaking inside words, and
#'   `max_cols` takes over from there. A shrink that does not clear the wrap
#'   is undone unless it keeps the columns on one slide. It then steps down
#'   again if that avoids a row split.
#'
#'   A table that does not fit at the floor is split rather than shrunk past
#'   it, and signals a `blockr_exhibit_split` message naming the size that
#'   would have kept it whole.
#' @param ... Passed to [static_table()].
#'
#' @return `file`, invisibly.
#' @seealso [write_exhibit_html()], [write_annotated_xlsx()], [static_table()],
#'   [exhibit-options] for the options that tune the fit
#' @examplesIf requireNamespace("officer", quietly = TRUE) && requireNamespace("flextable", quietly = TRUE)
#' f <- tempfile(fileext = ".pptx")
#' write_exhibit_pptx(
#'   summary_table(iris, vars = "Sepal.Length", by = "Species"),
#'   f,
#'   title = "Sepal length by species"
#' )
#' unlink(f)
#' @export
write_exhibit_pptx <- function(x, file, title = NULL, subtitle = NULL,
                               caption = NULL, template = NULL,
                               max_rows = "auto", max_cols = "auto",
                               min_font_size = NULL, ...) {

  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("write_exhibit_pptx() needs the 'officer' package.", call. = FALSE)
  }

  min_font_size <- exhibit_min_font_size(min_font_size)

  template <- pptx_template(template)
  usable <- nzchar(template)

  # Size the table to the slide and set it in the deck's font, unless the app
  # has set `blockr.viz.ft_font`.
  font <- if (is.null(getOption("blockr.viz.ft_font"))) {
    pptx_body_font(template)
  }
  old <- options(c(
    list(blockr.viz.ft_fit_width = pptx_content_width(template)),
    if (!is.null(font)) list(blockr.viz.ft_font = font)
  ))
  on.exit(options(old), add = TRUE)

  doc <- if (usable) {
    # Drop the template's example slides.
    pptx_strip_slides(officer::read_pptx(template))
  } else {
    officer::read_pptx()
  }

  doc <- pptx_add_exhibit(
    doc, x,
    title = title, subtitle = subtitle, caption = caption,
    template = template,
    max_rows = max_rows, max_cols = max_cols,
    min_font_size = min_font_size,
    ...
  )

  print(doc, target = file)

  invisible(file)
}

#' Add an Exhibit's Slides to an Open Deck
#'
#' The paginating half of [write_exhibit_pptx()], without the file: it takes
#' an [officer::read_pptx()] document, appends one slide per page of the
#' table, and returns the document. Column widths, font step-down, row and
#' column splits, repeated header bands and `(2 of 8)` page titles all happen
#' here, so a caller assembling its own deck (blockr.outline's slide builder)
#' gets the same slides as a single-table download.
#'
#' @section What the caller owns:
#' The options that size and typeset the table (`blockr.viz.ft_fit_width`,
#' `blockr.viz.ft_font`) are not set here. A deck sets them once around all
#' of its slides, from its own template. [write_exhibit_pptx()] sets them for
#' the single-table case. A `template` passed here only supplies the title's
#' point size and typeface, used to measure where the title ends.
#'
#' @param doc An `rpptx` document from [officer::read_pptx()].
#' @param x A data frame, an [as_annotated_df()]-coercible table object, or a
#'   flextable built by [static_table()], which carries its source frame and
#'   so can still be paged. A flextable from anywhere else is placed whole on
#'   one slide.
#' @param layout,master Layout and master to add slides on. `NULL` (default)
#'   picks "Title and Content" when the deck has it, else the first layout.
#' @param top Distance from the top of the slide to the table, in inches.
#'   `NULL` (default) measures it from the layout's own title placeholder, so
#'   a wrapped title cannot land on the table.
#' @inheritParams write_exhibit_pptx
#'
#' @return The document, with the exhibit's slides appended.
#' @seealso [write_exhibit_pptx()], [static_table()]
#' @examplesIf requireNamespace("officer", quietly = TRUE) && requireNamespace("flextable", quietly = TRUE)
#' doc <- officer::read_pptx()
#' doc <- pptx_add_exhibit(doc, head(iris, 20), title = "Iris")
#' length(doc)
#' @export
pptx_add_exhibit <- function(doc, x, ...) {
  UseMethod("pptx_add_exhibit", x)
}

#' The default method typesets a table with [static_table()] into real
#' PowerPoint cells. An exhibit whose marks cannot be text runs, such as the
#' summarize table's distribution glyphs, has its own method (see
#' [static_summarize_table()]).
#'
#' @rdname pptx_add_exhibit
#' @export
pptx_add_exhibit.default <- function(doc, x, title = NULL, subtitle = NULL,
                                     caption = NULL, template = NULL,
                                     layout = NULL, master = NULL, top = NULL,
                                     max_rows = "auto", max_cols = "auto",
                                     min_font_size = NULL,
                                     ...) {

  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("pptx_add_exhibit() needs the 'officer' package.", call. = FALSE)
  }
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("pptx_add_exhibit() needs the 'flextable' package.", call. = FALSE)
  }

  min_font_size <- exhibit_min_font_size(min_font_size)

  template <- if (is.character(template) && length(template) == 1L) {
    template
  } else {
    ""
  }

  setup <- pptx_slide_setup(doc, layout, master)
  layout <- setup$layout
  master <- setup$master
  slide_w <- setup$width
  slide_h <- setup$height

  # Asked rather than assumed: a title put in a missing placeholder is lost,
  # and a title put in the table when the placeholder exists prints twice.
  slide_title <- pptx_has_title(title) &&
    pptx_layout_has_title(doc, layout, master)

  # A flextable from static_table() carries its source frame, and the pages
  # are rebuilt from that. Any other flextable goes onto one slide whole.
  if (inherits(x, "flextable")) {
    src <- attr(x, "exhibit_data")
    if (is.null(src)) {
      return(
        pptx_add_table_slide(
          doc, x, layout, master, slide_w,
          top %||% attr(x, "pptx_top") %||% 1.1,
          title = if (slide_title) title
        )
      )
    }
    x <- src
  }

  # Coerced once, since the table is sliced from here on.
  x <- fmt_to_wide(as_annotated_df(x))

  args <- list(...)
  build <- function(data, rows = NULL, size = NULL, plan = NULL,
                    continued = FALSE, page = NULL) {
    do.call(static_table, c(
      list(
        if (is.null(rows)) data else pptx_slice_rows(data, rows),
        title = if (slide_title) "" else pptx_page_title(title, page),
        subtitle = subtitle,
        caption = caption,
        continued = continued
      ),
      if (!is.null(size)) list(font_size = size),
      # The layout decisions made on the whole table, so every page matches.
      plan,
      args
    ))
  }

  # The table starts below the title's text, measured on the longest page
  # title so the table does not move between slides.
  top <- top %||% max(
    args$pptx_top %||% 1.1,
    if (slide_title) {
      pptx_title_bottom(doc, layout, master, template,
                        pptx_page_title(title, c(99L, 99L))) %||% 0
    } else {
      0
    }
  )
  budget <- pptx_body_bottom(doc, layout, master, slide_h) - top
  base_size <- args$font_size %||% getOption("blockr.viz.ft_font_size", 13)

  # Width first: the font steps down from `base_size` until the columns stop
  # being squeezed or `min_font_size` is reached.
  fit_size <- function(data) {
    ft <- build(data, size = base_size)
    asked <- list(ft = ft, size = base_size)
    size <- base_size
    while (pptx_width_squeezed(ft) && size > min_font_size) {
      size <- size - 1
      ft <- build(data, size = size)
    }
    # A shrink that leaves the table squeezed only makes it smaller, so go
    # back to the asked size unless the smaller one keeps the columns whole.
    if (pptx_width_squeezed(ft) && !pptx_width_broken(asked$ft)) {
      ft <- asked$ft
      size <- asked$size
    }
    list(ft = ft, size = size, fits = !pptx_width_squeezed(ft),
         whole = !pptx_width_broken(ft))
  }

  chunks <- list(x)
  if (!is.null(max_cols)) {
    if (is.numeric(max_cols)) {
      chunks <- pptx_column_chunks(x, as.integer(max_cols))
    } else if (!fit_size(x)$whole) {
      chunks <- pptx_split_columns(x, function(cx) fit_size(cx)$whole)
    }
  }

  # Plan every page of every column set before drawing any, so the slides
  # can be numbered over the whole deck.
  plans <- lapply(chunks, function(cx) {
    got <- fit_size(cx)
    ft <- got$ft
    size <- got$size

    # Height second: shrink further if that avoids a row split.
    if (!is.null(max_rows) && !pptx_fits(ft, budget) && size > min_font_size) {
      for (s in seq(size - 1, min_font_size)) {
        cand <- build(cx, size = s)
        if (pptx_fits(cand, budget)) {
          ft <- cand
          size <- s
          break
        }
      }
    }

    if (is.null(max_rows) || pptx_fits(ft, budget)) {
      return(list(data = cx, size = size, plan = attr(ft, "layout_plan"),
                  from = 1L, to = nrow(cx), cont = FALSE,
                  squeezed = pptx_width_broken(ft)))
    }

    breaks <- if (is.numeric(max_rows)) {
      pptx_fixed_breaks(nrow(cx), as.integer(max_rows))
    } else {
      pptx_page_breaks(ft, budget)
    }
    key <- pptx_section_key(cx)
    breaks <- pptx_hold_sections(breaks, key)
    from <- c(1L, utils::head(breaks, -1L) + 1L)
    # Only a page that opens inside a section is "(continued)".
    cont <- vapply(from, function(a) {
      !is.null(key) && a > 1L && identical(key[[a]], key[[a - 1L]])
    }, logical(1L))

    list(data = cx, size = size, plan = attr(ft, "layout_plan"),
         from = from, to = breaks, cont = cont,
         squeezed = pptx_width_broken(ft),
         # The size that would have kept the table whole, for the split note.
         fit_size = pptx_fit_size(ft, function(s) build(cx, size = s),
                                  budget, size))
  })

  if (any(vapply(plans, function(p) isTRUE(p$squeezed), logical(1L)))) {
    warning(
      "The table has more columns (", length(pptx_data_cols(x)),
      ") than the slide can hold at ", min_font_size,
      "pt, so some cells are cut off.",
      call. = FALSE
    )
  }

  n_page <- sum(vapply(plans, function(p) length(p$from), integer(1L)))

  # `pages` counts row pages within a set. A fit size is reported only when
  # every split set has one, and it is the smallest of them.
  split <- Filter(function(p) length(p$from) > 1L, plans)
  fits <- lapply(split, `[[`, "fit_size")
  exhibit_split_note(
    title,
    pages = max(vapply(plans, function(p) length(p$from), integer(1L))),
    sets = length(plans),
    size = min(vapply(plans, function(p) p$size, numeric(1L))),
    floor = min_font_size,
    fit_size = if (length(split) && !any(vapply(fits, is.null, logical(1L)))) {
      min(unlist(fits))
    }
  )

  at <- 0L
  for (p in plans) {
    for (i in seq_along(p$from)) {
      at <- at + 1L
      page <- if (n_page > 1L) c(at, n_page)
      ft <- build(p$data, rows = p$from[[i]]:p$to[[i]], size = p$size,
                  plan = p$plan, continued = p$cont[[i]], page = page)
      doc <- pptx_add_table_slide(
        doc, ft, layout, master, slide_w, top,
        title = if (slide_title) pptx_page_title(title, page)
      )
    }
  }

  doc
}

# One slide with one table (or page of a table), centred.
pptx_add_table_slide <- function(doc, ft, layout, master, slide_w, top,
                                 title = NULL) {

  doc <- officer::add_slide(doc, layout = layout, master = master)

  if (!is.null(title)) {
    doc <- pptx_set_title(doc, title)
  }

  dim <- tryCatch(
    flextable::flextable_dim(ft),
    error = function(e) list(widths = 11.9, heights = 3)
  )
  # `top` is measured from the layout's title and overrides static_table()'s
  # own default.
  officer::ph_with(
    doc, ft,
    location = officer::ph_location(
      left = max(0.25, (slide_w - dim$widths) / 2),
      top = top,
      width = dim$widths, height = dim$heights
    )
  )
}

# The PowerPoint download: the picture when the browser drew one, else the
# render object, else the rows as the screen rounds them.
export_pptx <- function(x, file) {
  p <- exhibit_picture(x)
  if (!is.null(p)) {
    return(write_exhibit_pptx(p, file))
  }
  t <- exhibit_titles(x)
  obj <- exhibit_render(x) %||% exhibit_data(x, display = TRUE)
  write_exhibit_pptx(obj, file, title = t$title, subtitle = t$subtitle,
                     caption = t$caption)
}

# PowerPoint in the download menu.
register_pptx_exporter <- function() {
  register_viz_exporter("pptx", "PowerPoint", export_pptx, needs = "officer",
                        order = 30)
}
