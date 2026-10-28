# PowerPoint: how a table is cut into slides, and what the template's
# layouts offer. Used by write_exhibit_pptx() (R/export-pptx-common.R).

# "Title (2 of 3)" for page = c(2, 3); the plain title when `page` is NULL.
pptx_page_title <- function(title, page = NULL) {
  if (is.null(page) || !is.character(title) || !length(title)) {
    return(title)
  }
  sprintf("%s (%d of %d)", title, page[[1L]], page[[2L]])
}

# Height in inches: header band, body and footnote.
pptx_table_height <- function(ft) {
  sum(ft_part_heights(ft, "header"), ft_part_heights(ft, "body"),
      ft_part_heights(ft, "footer"))
}

# Does the table fit within `budget` inches?
pptx_fits <- function(ft, budget) {
  h <- pptx_table_height(ft)
  !is.finite(h) || h <= budget
}

# The font size at which a split table would fit one slide, or NULL when none
# at or above `hard` does. Heights scale with the type, so `size * budget /
# height` is the estimate, confirmed by one call to `build(est)`.
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

# Did the width allocator run out of slide? True when a data cell is narrower
# than its longest word or a header breaks inside a word.
pptx_width_squeezed <- function(ft) {
  sq <- attr(ft, "width_squeeze")
  isTRUE(sq[["cell"]]) || isTRUE(sq[["header"]])
}

# The condition for dealing the columns over several slide sets: the data
# cells are cut off. A header that only wraps does not count, unless
# `blockr.viz.ft_header_break_tol` is set and a header word keeps less than
# that share of itself.
pptx_width_broken <- function(ft, tol = getOption(
                                "blockr.viz.ft_header_break_tol", 0)) {
  sq <- attr(ft, "width_squeeze")
  fit <- attr(ft, "word_fit")
  isTRUE(sq[["cell"]]) ||
    (is.numeric(tol) && tol > 0 &&
       is.numeric(fit) && length(fit) == 1L && is.finite(fit) && fit < tol)
}

# Last input row of each page, from the measured height of the rendered rows.
# Every page repeats the header band and the footnote, and every page after
# the first also repeats the section headers it opens inside, so a
# continuation page has a smaller budget.
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
      # A single row taller than the slide: take it and let it overflow.
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

# The section each input row belongs to, or NULL when the table has none. The
# key is read from the structure view's frame, because that view can add
# section columns (such as `.variable_block`) that `x` does not carry.
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

# Move a break back when it would leave fewer than `keep` rows of a section at
# the top of the next slide. The whole section goes over instead.
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

  # Drop breaks that now repeat or run backwards.
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
# never cut in half. Without spanners every column is its own group.
pptx_col_groups <- function(x) {
  cols <- pptx_data_cols(x)
  top <- st_spanner_top(cols)
  key <- ifelse(nzchar(top), top, cols)
  unname(split(cols, factor(key, levels = unique(key))))
}

# Deal a table's columns over several slides, called when no allowed font size
# makes the columns fit. Each set repeats the row stub. `fits` takes a
# candidate frame and says whether its cells fit. Returns a list of frames,
# each the stub plus a contiguous run of the spanner groups.
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

  # No split works: return the whole table and let the caller warn.
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

# Copy the column and frame attributes (labels, title, caption, ...) that
# `[.data.frame` drops from `x` onto its subset `out`.
pptx_restore_attrs <- function(out, x) {
  for (cn in names(out)) {
    a <- attributes(x[[cn]])
    for (k in setdiff(names(a), c("names", "dim", "dimnames", "class",
                                  "levels", "row.names"))) {
      attr(out[[cn]], k) <- a[[k]]
    }
  }
  for (k in setdiff(names(attributes(x)), c("names", "row.names", "class"))) {
    attr(out, k) <- attr(x, k)
  }
  out
}

# Column subset that keeps the structure columns (dot-prefixed) and `keep`,
# with the display attributes.
pptx_subset_cols <- function(x, keep) {
  nm <- names(x)
  pptx_restore_attrs(x[, nm %in% keep | startsWith(nm, "."), drop = FALSE], x)
}

# Row subset with the display attributes kept.
pptx_slice_rows <- function(x, i) {
  out <- pptx_restore_attrs(x[i, , drop = FALSE], x)
  rownames(out) <- NULL
  out
}

# The reference deck to style against: the first existing file among the
# argument, the two options and blockr.outline's bundled template, else "".
pptx_template <- function(template = NULL) {

  candidates <- list(
    template,
    getOption("blockr.viz.pptx_template"),
    getOption("blockr.outline.template"),
    # "" when blockr.outline is not installed.
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

# Remove every slide, keeping masters, layouts and theme. Best effort: when
# officer cannot remove them the slides stay.
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

# TRUE for a single non-empty string.
pptx_has_title <- function(title) {
  is.character(title) && length(title) == 1L && nzchar(title)
}

# Put `title` in the layout's title placeholder; `doc` unchanged on failure.
pptx_set_title <- function(doc, title) {
  tryCatch(
    officer::ph_with(doc, title,
                     location = officer::ph_location_type(type = "title")),
    error = function(e) doc
  )
}

# Does the layout have a title placeholder?
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

# Layout, master and slide size (inches) for a new slide, and whether `title`
# goes into the layout's title placeholder (a title put in a missing
# placeholder is lost, and one put in the table as well prints twice).
# `layout` defaults to "Title and Content" when the deck has it, else the
# first layout.
pptx_slide_context <- function(doc, title, layout = NULL, master = NULL) {
  layouts <- officer::layout_summary(doc)
  layout <- layout %||% if ("Title and Content" %in% layouts$layout) {
    "Title and Content"
  } else {
    layouts$layout[[1L]]
  }
  master <- master %||% layouts$master[match(layout, layouts$layout)]
  list(
    layout = layout, master = master,
    width = tryCatch(officer::slide_size(doc)$width, error = function(e) 13.333),
    height = tryCatch(officer::slide_size(doc)$height, error = function(e) 7.5),
    slide_title = pptx_has_title(title) &&
      pptx_layout_has_title(doc, layout, master)
  )
}

# A new slide from the context, with `title` in the title placeholder.
pptx_blank_slide <- function(doc, ctx, title = NULL) {
  doc <- officer::add_slide(doc, layout = ctx$layout, master = ctx$master)
  if (!is.null(title)) doc <- pptx_set_title(doc, title)
  doc
}

# The value of the first `attr="..."` inside the first match of `block` in a
# part of the template, or NA when the part or the match is missing.
pptx_xml_attr <- function(template, part, block, attr) {
  xml <- pptx_part(template, part)
  if (is.null(xml)) {
    return(NA_character_)
  }
  tryCatch(
    {
      blk <- regmatches(xml, regexpr(block, xml))
      m <- regmatches(blk, regexpr(sprintf("%s=\"[^\"]*\"", attr), blk))
      if (length(m) == 1L) sub("^[^\"]*\"(.*)\"$", "\\1", m) else NA_character_
    },
    error = function(e) NA_character_
  )
}

# Usable table width in inches: the master's body placeholder, or 12 when it
# cannot be read. Read from the XML (`ext cx` in EMU, 914400 per inch) so a
# template officer cannot fully parse still counts.
pptx_content_width <- function(template) {
  cx <- pptx_xml_attr(template, "ppt/slideMasters/slideMaster1.xml",
                      "type=\"body\".*?<a:ext cx=\"[0-9]+\"", "cx")
  out <- suppressWarnings(as.numeric(cx)) / 914400
  if (is.finite(out) && out > 0) out else 12.0
}

# How far down the slide a table may run, in inches from the top: the top of
# the layout's footer, date or slide-number placeholder minus `gap`, but never
# below `slide_h - 0.4`.
pptx_body_bottom <- function(doc, layout, master, slide_h,
                             gap = getOption("blockr.viz.ft_footer_gap",
                                             0.05)) {

  fallback <- slide_h - 0.4

  out <- tryCatch(
    {
      ph <- officer::layout_properties(doc, layout = layout, master = master)
      foot <- ph[ph$type %in% c("ftr", "dt", "sldNum"), , drop = FALSE]
      tops <- foot$offy[is.finite(foot$offy) & foot$offy > slide_h / 2]
      # The gap absorbs rows that PowerPoint grows beyond the measured height.
      if (length(tops)) min(tops - gap, fallback) else fallback
    },
    error = function(e) fallback
  )

  if (length(out) == 1L && is.finite(out) && out > 0) out else fallback
}

# Where the title placeholder's text ends, in inches from the top of the
# slide, allowing for wrapping. NULL when the layout has no title placeholder
# or its geometry cannot be read.
pptx_title_bottom <- function(doc, layout, master, template, title,
                              gap = 0.1) {

  if (!pptx_has_title(title)) {
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

# Point size of the title text, from the master's title style. OOXML sizes
# are in hundredths of a point.
pptx_title_size <- function(template, fallback = 24) {
  sz <- pptx_xml_attr(template, "ppt/slideMasters/slideMaster1.xml",
                      "<p:titleStyle>.*?</p:titleStyle>", "sz")
  out <- suppressWarnings(as.numeric(sz)) / 100
  if (is.finite(out) && out > 0) out else fallback
}

# Half of the theme's font scheme: "minor" is the body face, "major" the
# title face. NULL when the template has no readable font scheme.
pptx_theme_font <- function(template, which = c("minor", "major")) {
  which <- match.arg(which)
  face <- pptx_xml_attr(
    template, "ppt/theme/theme1.xml",
    sprintf("<a:%sFont>.*?<a:latin typeface=\"[^\"]*\"", which), "typeface"
  )
  if (is.na(face) || !nzchar(face)) NULL else face
}

# One part of a pptx (a zip) as a single string, or NULL when the file or the
# part is missing.
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
