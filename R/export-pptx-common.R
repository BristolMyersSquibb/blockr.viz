#' Write an Exhibit to a PowerPoint Deck
#'
#' The pptx sibling of [write_exhibit_html()] and [write_annotated_xlsx()]: the
#' annotated data frame typeset by [static_table()] and placed on a slide of
#' the house template, at the same coordinates and in the same face the
#' outline's deck export uses. A table too tall for one slide is carried over
#' onto as many as it needs, each repeating the header (see `max_rows`); one
#' too wide has its columns dealt over several (see `max_cols`).
#'
#' The table is a NATIVE PowerPoint table, not a picture: every cell is real
#' text an editor can retype, restyle and re-colour. That is what officer buys
#' over a screenshot, and it is why this route exists next to the Excel one --
#' the spreadsheet is for pivoting, the deck is for showing.
#'
#' @section The template:
#' The reference deck supplies the slide size, the master layouts, the title
#' placeholder and the theme fonts. It is resolved, first hit wins, from:
#' `template`, `getOption("blockr.viz.pptx_template")`,
#' `getOption("blockr.outline.template")`, then blockr.outline's bundled
#' widescreen default when that package is installed. With none of them, officer
#' writes its own stock deck -- the download still succeeds, in officer's fonts.
#'
#' The template is asked for two things beyond its layouts. Its content width
#' sizes the table's columns to exactly fill the slide (PowerPoint never
#' autofits a flextable, so unset columns overflow), and its theme's body font
#' is what the table is set in unless the app has already named one through
#' `blockr.viz.ft_font` -- an app that states its exhibit face is the more
#' specific answer than the file.
#'
#' @param x A data frame or [as_annotated_df()]-coercible table object.
#' @param file Path to write the `.pptx` to.
#' @param title,subtitle,caption Slide title, table subtitle and footnote.
#'   `NULL` or `""` omits each. The title goes into the layout's own title
#'   placeholder when the template has one (so it inherits the deck's title
#'   styling and stays editable as a title), and into the table's own header
#'   otherwise.
#' @param template Path to a reference `.pptx`, or `NULL` (default) to resolve
#'   one as described above.
#' @param max_rows How a table too tall for its slide is handled.
#'   `"auto"` (default) carries it onto further slides, cutting where the
#'   measured height of the rendered rows runs out of slide rather than at a
#'   row count, since a wrapped label costs a line that no row count knows
#'   about. A number cuts every `max_rows` input rows instead. `NULL` never
#'   splits, and a long table overflows the slide.
#'
#'   Every page repeats the title, the column header band and the footnote,
#'   because a slide pulled out of the deck has to stand on its own; the title
#'   is marked `(2 of 3)` and a section carried across a break repeats its
#'   heading marked `(continued)`. Column widths are measured once on the
#'   whole table and reused on every page, so the columns line up when the
#'   reader flips between slides. Breaks avoid stranding one or two rows of a
#'   section at the top of a slide by moving the whole section over.
#' @param max_cols How a table too wide for its slide is handled, once the
#'   font has stepped down as far as `min_font_size` allows and the columns
#'   still do not fit. `"auto"` (default) deals them over several sets of
#'   slides, each repeating the row stub, so every slide is a readable table
#'   on its own; a set is a whole number of spanner groups, so an arm is never
#'   cut in half. A number puts at most that many data columns on a slide.
#'   `NULL` never splits, and the columns are squeezed as far as they will go.
#'
#'   It is the LAST resort, and a distant one. A table carried left-half then
#'   right-half is read by flipping back and forth with the stub in the
#'   middle; the same table carried over more slides is read top to bottom.
#'   So the columns come apart on one condition only: the DATA CELLS no
#'   longer fit their own longest word, which is the point at which
#'   PowerPoint starts stacking characters one per line. Thirty-six
#'   toxicity-grade columns of counts do that on a widescreen slide at any
#'   legible size.
#'
#'   Everything short of that is absorbed instead. The font steps down to
#'   `min_font_size`, the stub wraps
#'   (`getOption("blockr.viz.ft_stub_share")`, the share of the width it
#'   claims while the data columns are still short), the cells wrap between
#'   words and the headers break inside them. A wrapped header is not a
#'   reason to split unless an app sets
#'   `getOption("blockr.viz.ft_header_break_tol")` (default 0, off) to deal
#'   the columns when a header word is cut past that share of itself.
#' @param min_font_size Points. The floor for the two shrink passes. `NULL`
#'   (default) resolves it, first hit wins, from the board's
#'   `exhibit_min_font_size` option ([new_exhibit_font_option()], which is how
#'   an app lets its users say how small a table may go), then
#'   `getOption("blockr.viz.ft_min_font_size")`, then 11pt. Width first,
#'   because a table whose columns are narrower than their contents is
#'   illegible at any height and splitting rows does not help; the font steps
#'   down until the cells fit and the headers stop breaking inside words, and
#'   `max_cols` takes over from there. A shrink that does not clear the wrap
#'   is undone -- a smaller wrapped table is only smaller -- unless it is what
#'   keeps the columns on one slide. Height second: it steps down again if
#'   that avoids a row split altogether, since one slide at 11pt beats two at
#'   13pt.
#'
#'   One floor, both axes: it is the whole of "keep this table on one slide",
#'   and lowering it is what a deck that must not split its tables asks for.
#'   A table that still does not fit at the floor is split rather than shrunk
#'   past it, and signals a `blockr_exhibit_split` message naming the size
#'   that would have kept it whole.
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

  # Resolved here as well as in the method below, because both are entry
  # points and resolving twice costs nothing: the second call is handed a
  # number and returns it.
  min_font_size <- exhibit_min_font_size(min_font_size)
  # flextable is checked by the method that uses it: an exhibit that paints
  # itself needs ragg and grid instead, and refusing it for a missing
  # typesetter it never calls would be a lie.

  template <- pptx_template(template)
  usable <- nzchar(template)

  # Size the table to the slide it is going on, and set it in the deck's own
  # face -- the two things that make an exported table look like it belongs to
  # the template rather than merely sitting on it. Only when the app has not
  # already said: `blockr.viz.ft_font` is the more specific answer.
  font <- if (is.null(getOption("blockr.viz.ft_font"))) {
    pptx_body_font(template)
  }
  old <- options(c(
    list(blockr.viz.ft_fit_width = pptx_content_width(template)),
    if (!is.null(font)) list(blockr.viz.ft_font = font)
  ))
  on.exit(options(old), add = TRUE)

  doc <- if (usable) {
    # A reference deck carries EXAMPLE slides -- that is how pandoc learns the
    # styles -- and officer OPENS the file rather than reading its layouts, so
    # without this the download starts with the template's demo slides.
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
#' The paginating half of [write_exhibit_pptx()], with the file taken away: it
#' receives an [officer::read_pptx()] document, appends one slide per page of
#' the table, and returns the document. Everything the file writer does about
#' fitting a table to a slide -- measured column widths, the font step-down,
#' the row and column splits, the repeated header band, the `(2 of 8)` page
#' titles -- happens here, so a caller assembling a deck of its own gets the
#' same slides as a caller downloading one table.
#'
#' That caller is blockr.outline's slide builder, which places many blocks'
#' exhibits into one deck. A single `officer::ph_with()` would run a table
#' longer than the slide off the bottom of it.
#'
#' @section What the caller owns:
#' The two options that size and typeset the table
#' (`blockr.viz.ft_fit_width`, `blockr.viz.ft_font`) are NOT set here:
#' a deck sets them once, around all of its slides, from its own template.
#' [write_exhibit_pptx()] sets them for the single-table case. Passing a
#' `template` here is only about geometry the layout cannot report (the title's
#' point size and typeface, to measure where it ends).
#'
#' @param doc An `rpptx` document from [officer::read_pptx()].
#' @param x A data frame, an [as_annotated_df()]-coercible table object, or a
#'   flextable built by [static_table()] -- which carries the frame it was
#'   built from, so an already-rendered exhibit can still be paged. A
#'   flextable from anywhere else is placed whole on one slide, since there is
#'   nothing to re-cut.
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

#' Generic since 0.2.53. The default is this one: a table typeset by
#' [static_table()] into real PowerPoint cells. An exhibit whose marks cannot
#' be expressed as text runs -- the summarize table's distribution glyphs --
#' brings its own method instead (see [static_summarize_table()]), which is
#' also where the picture-versus-cells trade is explained.
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

  layouts <- officer::layout_summary(doc)
  layout <- layout %||% if ("Title and Content" %in% layouts$layout) {
    "Title and Content"
  } else {
    layouts$layout[[1L]]
  }
  master <- master %||% layouts$master[match(layout, layouts$layout)]

  slide_h <- tryCatch(officer::slide_size(doc)$height,
                      error = function(e) 7.5)
  slide_w <- tryCatch(officer::slide_size(doc)$width,
                      error = function(e) 13.333)

  has_title <- is.character(title) && length(title) == 1L && nzchar(title)
  # Does this layout own a title placeholder? Asked rather than assumed,
  # because the answer decides where the title is typeset: dropping it into a
  # placeholder that is not there loses it silently, and putting it in the
  # table when the placeholder IS there prints it twice.
  slide_title <- has_title && pptx_layout_has_title(doc, layout, master)

  # An exhibit that has already been RENDERED. static_table() stashes the
  # frame it was built from, which is the whole of what paging needs -- the
  # pages are rebuilt from it, not cut out of the flextable. A flextable from
  # anywhere else (a hand-built one, the topline block's) carries no such
  # frame, so it goes onto one slide as it always did: placing it whole is
  # worse than paging it and better than refusing it.
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

  # Sliced from here on, so the table has to be a data frame: a producer
  # object is coerced once, up front, rather than once per page.
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
      # The whole table's layout decisions, not just its widths: a page
      # measured on its own would pad differently, and the slides would no
      # longer match.
      plan,
      args
    ))
  }

  # The table starts below the title's own text, not at a constant that may
  # sit inside it. Measured on the LAST page's title, the longest of them, so
  # the table does not shift as the reader flips. A caller that has measured
  # its own layout says so and is taken at its word.
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

  # Width first, and it is a question about the COLUMNS, so it is settled
  # before any row is thought about: a table whose cells no longer fit their
  # columns is illegible at any height. The font steps down, and only when
  # even the smallest allowed leaves the table unreadable are the columns
  # themselves dealt over several slides -- 36 grade columns do not fit a
  # widescreen slide in any orientation, and cutting them off or stacking them
  # one character per line are not answers.
  #
  # Dealing the columns is the last resort, not the second one: two sets of
  # slides make the reader hold half a table in mind while looking at the
  # other half. A header wrapping inside a word does not.
  fit_size <- function(data) {
    ft <- build(data, size = base_size)
    asked <- list(ft = ft, size = base_size)
    size <- base_size
    while (pptx_width_squeezed(ft) && size > min_font_size) {
      size <- size - 1
      ft <- build(data, size = size)
    }
    # Shrinking that does not clear the squeeze only makes a wrapped table
    # smaller, so it goes back to the size that was asked for -- unless the
    # smaller one is what keeps the columns together.
    if (pptx_width_squeezed(ft) && !pptx_width_broken(asked$ft)) {
      ft <- asked$ft
      size <- asked$size
    }
    list(ft = ft, size = size, fits = !pptx_width_squeezed(ft),
         whole = !pptx_width_broken(ft))
  }

  chunks <- list(x)
  if (!is.null(max_cols)) {
    probe <- fit_size(x)
    if (is.numeric(max_cols)) {
      chunks <- pptx_column_chunks(x, as.integer(max_cols))
    } else if (!probe$whole) {
      chunks <- pptx_split_columns(x, function(cx) fit_size(cx)$whole)
    }
  }
  # What the deal cost is reported once, below, together with what the row
  # split cost: two messages about one table that would not fit read as two
  # problems.

  # Every page of every column set, planned before any of it is drawn, so the
  # slides can be numbered over the whole deck.
  plans <- lapply(chunks, function(cx) {
    got <- fit_size(cx)
    ft <- got$ft
    size <- got$size

    if (!is.null(max_rows) && !pptx_fits(ft, budget) && size > min_font_size) {
      # Height second: shrink further if that avoids a split, since one slide
      # at 11pt beats two at 13pt.
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
    # Only a page that opens INSIDE a section says "(continued)". One that
    # opens on a fresh heading is not a continuation of anything, and saying
    # so would be a lie the reader has no way to check.
    cont <- vapply(from, function(a) {
      !is.null(key) && a > 1L && identical(key[[a]], key[[a - 1L]])
    }, logical(1L))

    list(data = cx, size = size, plan = attr(ft, "layout_plan"),
         from = from, to = breaks, cont = cont,
         squeezed = pptx_width_broken(ft),
         # Measured here, where the table that did not fit is still in hand:
         # the size that would have kept it whole is the one thing a reader
         # asked to keep tables on one slide can act on.
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

  # Row pages within a set, since that is what a reader flips through; the
  # sets are counted beside it. A size is reported only when EVERY set that
  # split has one, and it is the smallest of them: lowering the floor to it
  # is then a promise that the whole table comes back together.
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

# One slide carrying one (page of a) table, centred on the slide.
pptx_add_table_slide <- function(doc, ft, layout, master, slide_w, top,
                                 title = NULL) {

  doc <- officer::add_slide(doc, layout = layout, master = master)

  if (!is.null(title)) {
    doc <- tryCatch(
      officer::ph_with(doc, title,
                       location = officer::ph_location_type(type = "title")),
      error = function(e) doc
    )
  }

  dim <- tryCatch(
    flextable::flextable_dim(ft),
    error = function(e) list(widths = 11.9, heights = 3)
  )
  # Centred rather than left-flush: a table sized to its own content (which is
  # what col_widths = "measured" does when the table is narrower than the
  # slide) reads as placed when it is centred and as fallen over when it is
  # not. A full-width table lands back on the template's own left margin.
  # `top` is the writer's, measured from the title actually on this layout,
  # and it wins over static_table()'s own default: the renderer sizing the
  # table does not know how far down the deck's title reaches.
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
# render object, else the rows as the screen rounds them. Reads the exhibit
# through its accessors only, so this file can move to blockr.outline as it
# is.
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

# PowerPoint in the download menu. blockr.outline calls the same
# register_viz_exporter() once it holds this file.
register_pptx_exporter <- function() {
  register_viz_exporter("pptx", "PowerPoint", export_pptx, needs = "officer",
                        order = 30)
}
