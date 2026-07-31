#' Write an Exhibit to a One-Slide PowerPoint Deck
#'
#' The pptx sibling of [write_exhibit_html()] and [write_annotated_xlsx()]: the
#' annotated data frame typeset by [static_table()] and placed on a single
#' slide of the house template, at the same coordinates and in the same face
#' the outline's deck export uses.
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
                               caption = NULL, template = NULL, ...) {

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

  ft <- static_table(
    x,
    title = if (slide_title) "" else title,
    subtitle = subtitle,
    caption = caption,
    ...
  )

  doc <- officer::add_slide(doc, layout = layout, master = master)

  if (slide_title) {
    doc <- tryCatch(
      officer::ph_with(
        doc, title,
        location = officer::ph_location_type(type = "title")
      ),
      error = function(e) doc
    )
  }

  dim <- tryCatch(
    flextable::flextable_dim(ft),
    error = function(e) list(widths = 11.9, heights = 3)
  )
  doc <- officer::ph_with(
    doc, ft,
    location = officer::ph_location(
      left = attr(ft, "pptx_left") %||% 0.4,
      top = attr(ft, "pptx_top") %||% 1.1,
      width = dim$widths, height = dim$heights
    )
  )

  print(doc, target = file)
  invisible(file)
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

# The deck's body typeface (the theme's MINOR latin font; the major one is for
# titles). NULL when the template carries no readable font scheme.
#
# Worth reading at all because a flextable writes an explicit typeface on every
# run: everything the template draws itself resolves against the theme, and the
# exported table would be the one thing on the slide that does not match its
# own master -- invisibly so on the machine that authored it.
pptx_body_font <- function(template) {

  xml <- pptx_part(template, "ppt/theme/theme1.xml")

  if (is.null(xml)) {
    return(NULL)
  }

  minor <- regmatches(
    xml,
    regexpr("<a:minorFont>.*?<a:latin typeface=\"[^\"]*\"", xml)
  )

  if (!length(minor)) {
    return(NULL)
  }

  face <- regmatches(minor, regexpr("typeface=\"[^\"]*\"", minor))
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
