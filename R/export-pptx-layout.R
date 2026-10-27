# PowerPoint: how a table is cut into slides, and what the template's
# layouts offer. Used by write_exhibit_pptx() (R/export-pptx-common.R).

# "Title (2 of 3)" on the continuation slides, plain on a table that fits.
pptx_page_title <- function(title, page = NULL) {
  if (is.null(page) || !is.character(title) || !length(title)) {
    return(title)
  }
  sprintf("%s (%d of %d)", title, page[[1L]], page[[2L]])
}

# How tall the whole thing is, header band and footnote included.
pptx_table_height <- function(ft) {
  sum(ft_part_heights(ft, "header"), ft_part_heights(ft, "body"),
      ft_part_heights(ft, "footer"))
}

# Does it clear the vertical budget?
pptx_fits <- function(ft, budget) {
  h <- pptx_table_height(ft)
  !is.finite(h) || h <= budget
}

# The size at which this table WOULD have been one slide, or NULL when no
# legible size is.
#
# Asked only of a table that has already been split, and answered with one
# extra measurement rather than a walk down the ladder: heights scale with
# the type, so `size * budget / height` is a guess that is never too small --
# a smaller font also unwraps rows, which the guess does not count. One probe
# confirms it. Walking the ladder from the floor downward would build the
# table five more times to learn the same thing.
pptx_fit_size <- function(ft, build, budget, size,
                          hard = exhibit_font_floor()) {

  h <- pptx_table_height(ft)

  if (!is.finite(h) || h <= 0 || !is.finite(budget) || budget <= 0) {
    return(NULL)
  }

  est <- floor(size * budget / h)

  if (est < hard || est >= size) {
    return(NULL)
  }

  if (pptx_fits(build(est), budget)) est else NULL
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
# sets of slides for: are the DATA CELLS cut off -- narrower than their own
# longest word, so a number is broken mid-token and, below that, PowerPoint
# stacks the characters one per line?
#
# Nothing else. A table carried over two slides is read by flipping back and
# forth with the stub in the middle, and almost nothing is worth that: a
# header taking a second line is read at a glance, a header breaking after a
# syllable is read with a moment's pause, and a table that pages downward is
# read straight through. The columns come apart only when a reader could not
# follow them at all.
#
# `blockr.viz.ft_header_break_tol` defaults to 0, off. At 0.6 a header word
# losing a third of itself deals the columns, which puts seven-column tables
# on two sets of slides.
pptx_width_broken <- function(ft, tol = getOption(
                                "blockr.viz.ft_header_break_tol", 0)) {
  sq <- attr(ft, "width_squeeze")
  fit <- attr(ft, "word_fit")
  isTRUE(sq[["cell"]]) ||
    (is.numeric(tol) && tol > 0 &&
       is.numeric(fit) && length(fit) == 1L && is.finite(fit) && fit < tol)
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

  # Trailing breaks can repeat or run backwards here; keep the ones that are
  # still boundaries.
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

# How far down the slide a table may run, in inches from the top.
#
# The limit is the top of whatever the layout puts at the foot -- the
# footer, the date, the slide number -- and never below `slide_h - 0.4`. A
# constant alone does not know the template: the BMS layout starts its footer
# band at 6.95in of a 7.5in slide, so `slide_h - 0.4` would print the last
# row or two over the company line. The date and slide-number placeholders count: they are usually
# level with the footer, and a table over the page number is as wrong as a
# table over the company name.
pptx_body_bottom <- function(doc, layout, master, slide_h,
                             gap = getOption("blockr.viz.ft_footer_gap",
                                             0.05)) {

  fallback <- slide_h - 0.4

  out <- tryCatch(
    {
      ph <- officer::layout_properties(doc, layout = layout, master = master)
      foot <- ph[ph$type %in% c("ftr", "dt", "sldNum"), , drop = FALSE]
      tops <- foot$offy[is.finite(foot$offy) & foot$offy > slide_h / 2]
      # A gap, because stopping exactly ON the footer still reads as one
      # thing touching another, and because a stated row height is a floor:
      # PowerPoint grows a row whose text needs more lines than the measuring
      # font predicted, and the growth all comes out of the bottom.
      if (length(tops)) min(tops - gap, fallback) else fallback
    },
    error = function(e) fallback
  )

  if (length(out) == 1L && is.finite(out) && out > 0) out else fallback
}

# Where the title placeholder's text ends, in inches from the top of the
# slide. The table is placed below THAT rather than at a constant, because a
# title long enough to wrap runs down into the table: the BMS layout puts its
# title at 0.4in with 1.25in of box, so a fixed 1.1in top sits inside it once
# a "(55 of 57)" page marker wraps a long title.
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
