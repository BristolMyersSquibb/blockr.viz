#' Write an Exhibit to a PowerPoint Deck
#'
#' The pptx sibling of [write_exhibit_html()] and [write_annotated_xlsx()]: the
#' annotated data frame typeset by [static_table()] and placed on a slide of
#' the house template, at the same coordinates and in the same face the
#' outline's deck export uses. A table too tall for one slide is carried over
#' onto as many as it needs, each repeating the header (see `max_rows`).
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
#' @param min_font_size Points. The floor for the two shrink passes, ignored
#'   when `max_rows` is `NULL` and defaulting from
#'   `getOption("blockr.viz.ft_min_font_size")`. Width first: a table with more
#'   columns than the slide can hold at the stated size steps down until its
#'   cells fit, because columns narrower than a character are illegible at any
#'   height and no amount of splitting helps. Height second: it steps down
#'   again if that avoids a split altogether, since one slide at 11pt beats
#'   two at 13pt.
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
                               max_rows = "auto",
                               min_font_size =
                                 getOption("blockr.viz.ft_min_font_size", 11),
                               ...) {

  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("write_exhibit_pptx() needs the 'officer' package.", call. = FALSE)
  }
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("write_exhibit_pptx() needs the 'flextable' package.", call. = FALSE)
  }

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

  layouts <- officer::layout_summary(doc)
  layout <- if ("Title and Content" %in% layouts$layout) {
    "Title and Content"
  } else {
    layouts$layout[[1L]]
  }
  master <- layouts$master[match(layout, layouts$layout)]

  has_title <- is.character(title) && length(title) == 1L && nzchar(title)
  # Does this layout own a title placeholder? Asked rather than assumed,
  # because the answer decides where the title is typeset: dropping it into a
  # placeholder that is not there loses it silently, and putting it in the
  # table when the placeholder IS there prints it twice.
  slide_title <- has_title && pptx_layout_has_title(doc, layout, master)

  # Sliced from here on, so the table has to be a data frame: a producer
  # object is coerced once, up front, rather than once per page.
  x <- fmt_to_wide(as_annotated_df(x))

  args <- list(...)
  build <- function(rows = NULL, size = NULL, plan = NULL,
                    continued = FALSE, page = NULL) {
    do.call(static_table, c(
      list(
        if (is.null(rows)) x else pptx_slice_rows(x, rows),
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

  slide_h <- tryCatch(officer::slide_size(doc)$height,
                      error = function(e) 7.5)
  slide_w <- tryCatch(officer::slide_size(doc)$width,
                      error = function(e) 13.333)

  # The table starts below the title's own text, not at a constant that may
  # sit inside it. Measured on the LAST page's title, the longest of them, so
  # the table does not shift as the reader flips.
  top <- max(
    args$pptx_top %||% 1.1,
    if (slide_title) {
      pptx_title_bottom(doc, layout, master, template,
                        pptx_page_title(title, c(99L, 99L))) %||% 0
    } else {
      0
    }
  )
  budget <- slide_h - top - 0.4

  ft <- build()
  size <- args$font_size %||% getOption("blockr.viz.ft_font_size", 13)

  # Width first. A table whose CELLS no longer fit their columns is illegible
  # at any height -- past about 44 columns a count and its padding is wider
  # than the slide can give, and PowerPoint stacks the characters one per
  # line. Stepping the font down is the only thing that buys real width back,
  # so it happens before anything else is decided.
  if (!is.null(max_rows) && pptx_cell_squeezed(ft) && size > min_font_size) {
    for (s in seq(size - 1, min_font_size)) {
      ft <- build(size = s)
      size <- s
      if (!pptx_cell_squeezed(ft)) break
    }
  }

  # Past every escalation there are tables a slide simply cannot hold: sixty
  # columns need 14in of width at the smallest font allowed. Said out loud,
  # because the alternative is a deck of quietly clipped cells, which is how
  # this was found in the first place.
  if (!is.null(max_rows) && pptx_cell_squeezed(ft)) {
    warning(
      "The table has more columns (", length(ft$body$colwidths) - 1L,
      ") than the slide can hold at ", min_font_size,
      "pt, so some cells are cut off. Fewer columns, or a lower ",
      "`min_font_size`, would fit.",
      call. = FALSE
    )
  }

  # Then height: shrink further if that avoids a split, since one slide at
  # 11pt beats two at 13pt.
  if (!is.null(max_rows) && !pptx_fits(ft, budget) && size > min_font_size) {
    for (s in seq(size - 1, min_font_size)) {
      cand <- build(size = s)
      if (pptx_fits(cand, budget)) {
        ft <- cand
        size <- s
        break
      }
    }
  }

  pages <- list(ft)
  if (!is.null(max_rows) && !pptx_fits(ft, budget)) {
    breaks <- if (is.numeric(max_rows)) {
      pptx_fixed_breaks(nrow(x), as.integer(max_rows))
    } else {
      pptx_page_breaks(ft, budget)
    }
    key <- pptx_section_key(x)
    breaks <- pptx_hold_sections(breaks, key)
    plan <- attr(ft, "layout_plan")
    from <- c(1L, utils::head(breaks, -1L) + 1L)
    # Only a page that opens INSIDE a section says "(continued)". One that
    # opens on a fresh heading is not a continuation of anything, and saying
    # so would be a lie the reader has no way to check.
    cont <- vapply(from, function(a) {
      !is.null(key) && a > 1L && identical(key[[a]], key[[a - 1L]])
    }, logical(1L))
    pages <- Map(
      function(a, b, i) {
        build(rows = a:b, size = size, plan = plan,
              continued = cont[[i]], page = c(i, length(breaks)))
      },
      from, breaks, seq_along(breaks)
    )
  }

  for (i in seq_along(pages)) {
    doc <- pptx_add_table_slide(
      doc, pages[[i]], layout, master, slide_w, top,
      title = if (slide_title) {
        pptx_page_title(title, if (length(pages) > 1L) c(i, length(pages)))
      }
    )
  }

  print(doc, target = file)
  invisible(file)
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

# Did the width allocator run out of slide for the DATA cells, which will be
# cut off rather than wrapped? Splitting rows cannot help with that, which is
# why it is asked apart from the height and answered by stepping the font
# down.
pptx_cell_squeezed <- function(ft) {
  isTRUE(attr(ft, "width_squeeze")[["cell"]])
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
pptx_section_key <- function(x) {
  cols <- tryCatch(annotated_structure_view(x)$section_cols,
                   error = function(e) character())
  if (!length(cols)) {
    return(NULL)
  }
  do.call(paste, c(lapply(cols, function(cn) as.character(x[[cn]])),
                   list(sep = "\r")))
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
