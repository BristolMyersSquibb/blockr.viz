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
#'   splits, and a long table overflows the slide as it did before.
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
#'   Thirty-six toxicity-grade columns do not fit a widescreen slide at any
#'   legible size, and the alternatives are cells cut off or headers broken
#'   inside words, which at that width becomes one character per line.
#'
#'   It is the last resort rather than the second, because a table carried
#'   left-half then right-half is read by flipping back and forth, where the
#'   same table carried over more slides is read top to bottom. Before it,
#'   the stub wraps (`getOption("blockr.viz.ft_stub_share")`, the share of
#'   the width it claims while the data columns are still short) and the
#'   headers are allowed to break inside a word, down to
#'   `getOption("blockr.viz.ft_header_break_tol")` of the longest one -- 0.6,
#'   a word losing a syllable to a second line. Lower that to hold a wide
#'   table together for longer, raise it to split sooner.
#' @param min_font_size Points. The floor for the two shrink passes, defaulting
#'   from `getOption("blockr.viz.ft_min_font_size")`. Width first, because a
#'   table whose columns are narrower than their contents is illegible at any
#'   height and splitting rows does not help; the font steps down until the
#'   cells fit and the headers stop breaking inside words, and `max_cols`
#'   takes over from there. A shrink that does not clear the wrap is undone --
#'   a smaller wrapped table is only smaller -- unless it is what keeps the
#'   columns on one slide. Height second: it steps down again if that avoids
#'   a row split altogether, since one slide at 11pt beats two at 13pt.
#' @param ... Passed to [static_table()].
#'
#' @return `file`, invisibly.
#' @seealso [write_exhibit_html()], [write_annotated_xlsx()], [static_table()]
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
                               min_font_size =
                                 getOption("blockr.viz.ft_min_font_size", 11),
                               ...) {

  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("write_exhibit_pptx() needs the 'officer' package.", call. = FALSE)
  }
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
#' exhibits into one deck. Before this seam existed it placed each table with a
#' single `officer::ph_with()`, so a table longer than the slide ran off the
#' bottom of it.
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
                                     min_font_size =
                                       getOption("blockr.viz.ft_min_font_size",
                                                 11),
                                     ...) {

  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("pptx_add_exhibit() needs the 'officer' package.", call. = FALSE)
  }
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("pptx_add_exhibit() needs the 'flextable' package.", call. = FALSE)
  }

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
  budget <- slide_h - top - 0.4
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
    if (length(chunks) > 1L) {
      message("The table is too wide for one slide; its ",
              length(pptx_data_cols(x)), " columns are dealt over ",
              length(chunks), " sets of slides.")
    }
  }

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
         squeezed = pptx_width_broken(ft))
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

# "Title (2 of 3)" on the continuation slides, plain on a table that fits.
pptx_page_title <- function(title, page = NULL) {
  if (is.null(page) || !is.character(title) || !length(title)) {
    return(title)
  }
  sprintf("%s (%d of %d)", title, page[[1L]], page[[2L]])
}

# Does the whole thing clear the vertical budget, header band and footnote
# included?
pptx_fits <- function(ft, budget) {
  h <- sum(ft_part_heights(ft, "header"), ft_part_heights(ft, "body"),
           ft_part_heights(ft, "footer"))
  !is.finite(h) || h <= budget
}

# Did the width allocator run out of slide? Either half counts: a data cell
# narrower than its own characters is cut off, and a header narrower than its
# longest word breaks inside it. Splitting ROWS helps with neither, which is
# why this is asked apart from the height, and it is what the font ladder
# tries to clear: a point smaller and nothing wraps.
pptx_width_squeezed <- function(ft) {
  sq <- attr(ft, "width_squeeze")
  isTRUE(sq[["cell"]]) || isTRUE(sq[["header"]])
}

# The narrower question, and the only one worth dealing the columns over two
# sets of slides for: are the cells cut off, or are the headers down past
# `blockr.viz.ft_header_break_tol` of their longest word? At the default 0.6 a
# word may lose a syllable to a second line; below it the word is cut into
# thirds, which is what 36 grade columns do to "Grade".
#
# A table carried over two slides is read by flipping back and forth with the
# stub in the middle; a header that breaks after a syllable is read at a
# glance. So a squeeze on its own is not enough: the columns come apart only
# when a reader could not follow them otherwise.
pptx_width_broken <- function(ft, tol = getOption(
                                "blockr.viz.ft_header_break_tol", 0.6)) {
  sq <- attr(ft, "width_squeeze")
  fit <- attr(ft, "word_fit")
  isTRUE(sq[["cell"]]) ||
    (is.numeric(fit) && length(fit) == 1L && is.finite(fit) && fit < tol)
}

# Last input row of each page, decided on the measured height of the rendered
# rows rather than on a row count. Every page re-emits the header band and the
# footnote (that is the point of splitting rather than overflowing), and every
# page after the first also re-emits the section headers it opens inside, so
# the budget for a continuation page is smaller than for the first.
pptx_page_breaks <- function(ft, budget) {

  h <- ft_part_heights(ft, "body")
  map <- attr(ft, "row_map")

  if (!length(h) || !length(map) || length(h) != length(map) ||
        !any(!is.na(map))) {
    return(max(1L, sum(!is.na(map))))
  }

  avail <- budget - sum(ft_part_heights(ft, "header")) -
    sum(ft_part_heights(ft, "footer"))
  n_lvl <- which(!is.na(map))[[1L]] - 1L
  sec_h <- if (any(is.na(map))) max(h[is.na(map)]) else 0
  avail_cont <- avail - n_lvl * sec_h

  n <- length(h)
  out <- integer(0)
  i <- 1L

  while (i <= n) {
    cap <- if (length(out)) avail_cont else avail
    used <- 0
    last <- NA_integer_
    j <- i
    while (j <= n) {
      if (used + h[[j]] > cap && !is.na(last)) break
      used <- used + h[[j]]
      if (!is.na(map[[j]])) last <- map[[j]]
      j <- j + 1L
    }
    if (is.na(last)) {
      # A single row taller than the whole slide: take it anyway rather than
      # loop forever, and let it overflow.
      last <- min(map[!is.na(map) & seq_len(n) >= i])
    }
    out <- c(out, last)
    nxt <- which(!is.na(map) & map > last)
    if (!length(nxt)) break
    i <- nxt[[1L]]
  }

  out
}

pptx_fixed_breaks <- function(n_row, per) {
  per <- max(1L, per)
  unique(c(seq(per, n_row, by = per), n_row))
}

# The section each input row belongs to, or NULL when the table has none.
#
# The key is read off the view's OWN frame, not off the input: a table whose
# only row axis is `.variable_label` has its sections named by the synthetic
# `.variable_block` column, which the view adds and the input never carries.
# Reading `x[[".variable_block"]]` there gave NULL -- a zero-length key that is
# not NULL, so every guard downstream let it through and the first
# continuation page asked it for row 11 of nothing.
pptx_section_key <- function(x) {
  view <- tryCatch(annotated_structure_view(x), error = function(e) NULL)
  cols <- view$section_cols
  if (!length(cols)) {
    return(NULL)
  }
  key <- do.call(paste, c(lapply(cols, function(cn) as.character(view$data[[cn]])),
                          list(sep = "\r")))
  if (length(key) != nrow(x)) {
    return(NULL)
  }
  key
}

# Move a break back when it would leave a section with one or two rows
# stranded at the top of the next slide. The whole section goes over instead,
# which is the same fix as not leaving a heading alone at the foot of a slide.
pptx_hold_sections <- function(breaks, key, keep = 2L) {

  if (is.null(key) || length(breaks) < 2L) {
    return(breaks)
  }

  n <- length(key)
  for (i in seq_len(length(breaks) - 1L)) {
    at <- breaks[[i]]
    if (at >= n) next
    tail_n <- sum(key[(at + 1L):n] == key[[at]] &
                    cumsum(key[(at + 1L):n] != key[[at]]) == 0L)
    if (tail_n == 0L || tail_n >= keep) next
    start <- at - sum(rev(key[seq_len(at)]) == key[[at]]) + 1L
    lower <- if (i == 1L) 1L else breaks[[i - 1L]] + 1L
    if (start > lower) breaks[[i]] <- start - 1L
  }

  # Trailing breaks can now repeat or run backwards; drop what is no longer a
  # boundary.
  breaks <- breaks[c(TRUE, diff(breaks) > 0)]
  if (utils::tail(breaks, 1L) < n) breaks <- c(breaks, n)
  breaks
}

# The data columns of an annotated frame, in order. Everything dot-prefixed is
# structure, never a column of the display table.
pptx_data_cols <- function(x) {
  nm <- names(x)
  nm[!startsWith(nm, ".")]
}

# The column groups a split may cut between: the spanner groups, so an arm is
# never dealt half onto one slide and half onto the next. Without spanners
# every column is its own group.
pptx_col_groups <- function(x) {
  cols <- pptx_data_cols(x)
  top <- vapply(strsplit(cols, "||", fixed = TRUE), function(p) {
    if (length(p) > 1L) p[[1L]] else ""
  }, character(1L))
  key <- ifelse(nzchar(top), top, cols)
  unname(split(cols, factor(key, levels = unique(key))))
}

#' Deal a table's columns over several slides
#'
#' Called when no font the exporter is allowed to use makes the columns fit:
#' 36 toxicity-grade columns do not fit a widescreen slide in any orientation,
#' and the alternatives are cells cut off or headers stacked one character per
#' line. Each set of slides repeats the row stub, so every one is a readable
#' table on its own.
#'
#' @param fits A predicate taking a candidate frame and saying whether its
#'   cells fit, which is the exporter's own width pass rather than a second
#'   copy of the arithmetic.
#' @return A list of frames, each the stub plus a contiguous run of the
#'   spanner groups.
#' @noRd
pptx_split_columns <- function(x, fits, max_sets = 12L) {

  groups <- pptx_col_groups(x)

  if (length(groups) < 2L) {
    return(list(x))
  }

  for (n in 2:min(max_sets, length(groups))) {
    at <- pptx_deal(length(groups), n)
    sets <- lapply(at, function(idx) {
      pptx_subset_cols(x, unlist(groups[idx], use.names = FALSE))
    })
    if (all(vapply(sets, fits, logical(1L)))) {
      return(sets)
    }
  }

  # Nothing worked: hand back the whole thing and let the caller say so,
  # rather than dealing it into slivers.
  list(x)
}

# Fixed-size column sets, the `max_cols = n` route.
pptx_column_chunks <- function(x, per) {
  cols <- pptx_data_cols(x)
  per <- max(1L, per)
  if (length(cols) <= per) {
    return(list(x))
  }
  groups <- pptx_col_groups(x)
  sets <- list()
  cur <- character()
  for (g in groups) {
    if (length(cur) && length(cur) + length(g) > per) {
      sets <- c(sets, list(cur))
      cur <- character()
    }
    cur <- c(cur, g)
  }
  lapply(c(sets, list(cur)), function(keep) pptx_subset_cols(x, keep))
}

# Split `n` groups into `k` contiguous runs, as evenly as the counts allow.
pptx_deal <- function(n, k) {
  size <- diff(round(seq(0, n, length.out = k + 1L)))
  ends <- cumsum(size)
  Map(seq, c(1L, utils::head(ends, -1L) + 1L), ends)
}

# Column subset that keeps the structure columns and the frame's own display
# attributes; `keep` names the data columns to hold on to.
pptx_subset_cols <- function(x, keep) {

  nm <- names(x)
  out <- x[, nm %in% keep | startsWith(nm, "."), drop = FALSE]

  for (cn in names(out)) {
    a <- attributes(x[[cn]])
    for (k in setdiff(names(a), c("names", "dim", "dimnames", "class",
                                  "levels", "row.names"))) {
      attr(out[[cn]], k) <- a[[k]]
    }
  }
  for (k in setdiff(names(attributes(x)),
                    c("names", "row.names", "class"))) {
    attr(out, k) <- attr(x, k)
  }

  out
}

# Row subset that keeps what `[.data.frame` throws away: the column labels the
# annotated-df contract carries, and the table's own title / subtitle /
# caption attributes.
pptx_slice_rows <- function(x, i) {

  out <- x[i, , drop = FALSE]

  for (nm in names(x)) {
    a <- attributes(x[[nm]])
    for (k in setdiff(names(a), c("names", "dim", "dimnames", "class",
                                  "levels", "row.names"))) {
      attr(out[[nm]], k) <- a[[k]]
    }
  }
  for (nm in setdiff(names(attributes(x)),
                     c("names", "row.names", "class"))) {
    attr(out, nm) <- attr(x, nm)
  }

  rownames(out) <- NULL
  out
}

# The reference deck this export styles against: the caller's, the app's, the
# outline's, or none. A path that no longer exists counts as none -- state
# saved on one machine and opened on another carries absolute paths that
# resolve nowhere, and silently styling against a stale one is not a thing to
# diagnose from a slide.
pptx_template <- function(template = NULL) {

  candidates <- list(
    template,
    getOption("blockr.viz.pptx_template"),
    getOption("blockr.outline.template"),
    # A soft dependency: system.file() returns "" when the package is absent,
    # so this costs nothing when it is.
    system.file("templates", "widescreen-default.pptx",
                package = "blockr.outline")
  )

  for (p in candidates) {
    if (is.character(p) && length(p) == 1L && nzchar(p) && file.exists(p)) {
      return(p)
    }
  }

  ""
}

# Remove every slide, keeping the masters, layouts and theme. Best effort: an
# officer without remove_slide(), or a deck it refuses to shrink, leaves the
# slides in place rather than failing the download.
pptx_strip_slides <- function(doc) {
  tryCatch(
    {
      for (i in rev(seq_along(doc))) {
        doc <- officer::remove_slide(doc, i)
      }
      doc
    },
    error = function(e) doc
  )
}

pptx_layout_has_title <- function(doc, layout, master) {
  tryCatch(
    {
      props <- officer::layout_properties(doc, layout = layout,
                                          master = master)
      any(grepl("title", props$type, fixed = TRUE))
    },
    error = function(e) FALSE
  )
}

# Usable table width (inches): the master's body placeholder, or a 16:9
# widescreen fallback. Read straight out of the package's XML (`ext cx` in
# EMU, 914400 = 1in) rather than through officer, so a template officer cannot
# fully parse still contributes its geometry.
pptx_content_width <- function(template) {

  fallback <- 12.0
  xml <- pptx_part(template, "ppt/slideMasters/slideMaster1.xml")

  if (is.null(xml)) {
    return(fallback)
  }

  out <- tryCatch(
    {
      body <- regmatches(
        xml,
        regexpr("type=\"body\".*?<a:ext cx=\"[0-9]+\"", xml)
      )
      cx <- regmatches(body, regexpr("cx=\"[0-9]+\"", body))
      cx <- as.numeric(gsub("\\D", "", cx))
      if (length(cx) == 1L && is.finite(cx) && cx > 0) cx / 914400 else fallback
    },
    error = function(e) fallback
  )

  if (length(out) == 1L && is.finite(out) && out > 0) out else fallback
}

# Where the title placeholder's text ends, in inches from the top of the
# slide. The table is placed below THAT rather than at a constant, because a
# title long enough to wrap runs down into the table: the BMS layout puts its
# title at 0.4in with 1.25in of box, and the exporter's old fixed 1.1in top
# sits inside it. A one-line title never showed the collision; adding a
# "(55 of 57)" page marker to an already long one does.
#
# NULL when the layout has no title placeholder or the geometry cannot be
# read, and the caller keeps its default.
pptx_title_bottom <- function(doc, layout, master, template, title,
                              gap = 0.1) {

  if (!is.character(title) || length(title) != 1L || !nzchar(title)) {
    return(NULL)
  }

  tryCatch(
    {
      props <- officer::layout_properties(doc, layout = layout,
                                          master = master)
      ph <- props[grepl("title", props$type, fixed = TRUE), , drop = FALSE]
      if (!nrow(ph)) {
        return(NULL)
      }
      size <- pptx_title_size(template)
      font <- pptx_theme_font(template, "major") %||% "Arial"
      lines <- ft_line_count(title, ph$cx[[1L]] - 0.2, font, size,
                             bold = TRUE)
      ph$offy[[1L]] + lines * size * 1.2 / 72 + gap
    },
    error = function(e) NULL
  )
}

# Point size of the layout's title text, from the master's own title style.
pptx_title_size <- function(template, fallback = 24) {

  xml <- pptx_part(template, "ppt/slideMasters/slideMaster1.xml")

  if (is.null(xml)) {
    return(fallback)
  }

  style <- regmatches(xml, regexpr("<p:titleStyle>.*?</p:titleStyle>", xml))
  sz <- regmatches(style, regexpr("sz=\"[0-9]+\"", style))

  if (!length(sz)) {
    return(fallback)
  }

  # OOXML states point sizes in hundredths.
  out <- as.numeric(gsub("\\D", "", sz)) / 100
  if (length(out) == 1L && is.finite(out) && out > 0) out else fallback
}

# The deck's body typeface (the theme's MINOR latin font; the major one is for
# titles). NULL when the template carries no readable font scheme.
#
# Worth reading at all because a flextable writes an explicit typeface on every
# run: everything the template draws itself resolves against the theme, and the
# exported table would be the one thing on the slide that does not match its
# own master -- invisibly so on the machine that authored it.
pptx_body_font <- function(template) {
  pptx_theme_font(template, "minor")
}

# Either half of the theme's font scheme: "minor" is the body face, "major"
# the one titles are set in.
pptx_theme_font <- function(template, which = c("minor", "major")) {

  which <- match.arg(which)
  xml <- pptx_part(template, "ppt/theme/theme1.xml")

  if (is.null(xml)) {
    return(NULL)
  }

  block <- regmatches(
    xml,
    regexpr(sprintf("<a:%sFont>.*?<a:latin typeface=\"[^\"]*\"", which), xml)
  )

  if (!length(block)) {
    return(NULL)
  }

  face <- regmatches(block, regexpr("typeface=\"[^\"]*\"", block))
  face <- sub("\"$", "", sub("^typeface=\"", "", face))

  if (!length(face) || !nzchar(face)) NULL else face
}

# One part of a pptx (a zip), as a single string. NULL when the file, the part
# or the unzip is not there.
pptx_part <- function(template, part) {

  if (!is.character(template) || length(template) != 1L ||
        !nzchar(template) || !file.exists(template)) {
    return(NULL)
  }

  tryCatch(
    {
      dir <- tempfile("pptx-part")
      on.exit(unlink(dir, recursive = TRUE), add = TRUE)
      utils::unzip(template, files = part, exdir = dir)
      f <- file.path(dir, part)
      if (!file.exists(f)) {
        return(NULL)
      }
      paste(readLines(f, warn = FALSE), collapse = "")
    },
    error = function(e) NULL
  )
}
