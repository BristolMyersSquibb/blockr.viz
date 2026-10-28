#' Flextable Renderer for Annotated Data Frames
#'
#' Renders an [annotated data frame][as_annotated_df()] as a styled
#' [flextable][flextable::flextable()], the static table output for report and
#' deck rendering (quarto pptx, docx). flextable emits real OpenXML tables in
#' PowerPoint and Word output.
#'
#' Structure comes from the annotated-df contract (shared with `html_table()`
#' and [write_annotated_xlsx()]): `.label` is the row stub, `.group<k>_level`
#' runs and `.variable_label` blocks become bold section-header rows merged
#' across the table width, `.indent` / `.strong` / `.emph` style rows,
#' `Top||Leaf` column names become a merged spanner header row, and
#' `attr(col, "label")` carries the leaf header text (`"\n"` renders as a line
#' break). The identity columns (`.variable*`, raw `.group<k>` pairs) are
#' ignored here.
#'
#' Columns can carry emphasis, the column analogue of the row `.strong` /
#' `.emph` flags: `attr(col, "strong")` / `attr(col, "emph")`. When any column
#' is flagged, the header band follows an emphasis ramp (normal light gray,
#' `emph` dark gray and italic, `strong` an accent) and `header_bg` is
#' ignored.
#'
#' The look is a dense bordered grid, first column left-aligned and wide, data
#' columns right-aligned, optional colored header bands and manual column
#' widths, since PowerPoint never autofits.
#'
#' @param data A data frame (or an object with an [as_annotated_df()]
#'   method, e.g. a composer table, coerced on entry).
#' @param title,subtitle,caption Table title / subtitle (header lines) and
#'   caption (footer line). `NULL` (default) auto-fills from the annotated
#'   df's `label` / `subtitle` / `caption` attributes; `""` suppresses.
#' @param na_rep Character. Text for missing (`NA`) cells. Defaults to an em
#'   dash.
#' @param font,font_size,font_color Base typography, applied to all parts.
#'   `font_size` is in points and defaults from
#'   `getOption("blockr.viz.ft_font_size")`, 13 unset. `font` defaults from
#'   `getOption("blockr.viz.ft_font")`, which a theme sets
#'   (`exhibits = list(ft_font = ...)`) and which the pptx export sets from the
#'   reference deck's font scheme. Unset, the blockr default applies.
#' @param indent_width Points of left padding per `.indent` level (section
#'   headers indent at their nesting depth with the same step).
#' @param first_col_width,other_cols_width Column widths in inches, used
#'   unless `fit_width` or `auto_width` is set. Defaults fit the 13.33in
#'   widescreen template; pass smaller values for the 10in quarto default.
#' @param fit_width Optional total table width in inches (usually the slide's
#'   usable content width). When set, the columns are distributed to fill it
#'   exactly (`col_widths` says how). Defaults from
#'   `getOption("blockr.viz.ft_fit_width")`, which the pptx / docx render sets
#'   from the reference template. Unset for html / pdf.
#' @param col_widths How `fit_width` is split, `"measured"` (default) or
#'   `"even"`. Measured asks every column what its own text needs at the
#'   current font. When the slide can hold them all, the data columns get
#'   enough that no cell wraps and no header breaks inside a word, and the
#'   rest goes to the row stub. While the data columns are still short of
#'   their full headers the stub takes at most
#'   `getOption("blockr.viz.ft_stub_share")` (0.3) of the width and wraps.
#'
#'   When the slide cannot hold them all, the stub and the data columns are
#'   filled to a common level, from the width each cannot go below (its
#'   longest word) toward its whole text. A column of counts reaches its full
#'   width almost at once, and a column of several short words gives room back
#'   and wraps.
#'
#'   Even gives the stub `first_col_width` capped at half the slide and splits
#'   the remainder equally over the data columns. Defaults from
#'   `getOption("blockr.viz.ft_col_widths")`.
#'
#'   A numeric vector of widths (one per column, stub first) is used as given.
#'   [write_exhibit_pptx()] uses this to give every page of a split table the
#'   widths measured on the whole of it.
#' @param cell_padding Points of left and right padding per cell, or `NULL`
#'   (default) for the flextable default of 5. Set automatically to a tighter
#'   value when a table cannot fit any other way.
#' @param continued Logical. `TRUE` marks the section headings this table
#'   opens with as `(continued)`, for a page of a split table.
#' @param auto_width Logical. `TRUE` runs [flextable::autofit()] instead of
#'   the manual widths.
#' @param header_bg Header band colors, keyed on the column-group structure
#'   the annotated df carries. A color vector (hex or R color names):
#'   \itemize{
#'     \item **Named** entries pin a group. The name is a by-level value (the
#'       spanner top for a nested `by`, else the data-column name), and
#'       `".stub"` pins the row-stub header.
#'     \item **Unnamed** entries are the cycle pool for the remaining,
#'       unpinned groups, in order.
#'     \item The two mix: `c(.stub = "#EEE", Placebo = "grey", "#33D6F1")`.
#'   }
#'   Defaults from `getOption("blockr.viz.ft_header_bg")`; unset (or `NULL` /
#'   `"none"`) leaves the header unfilled. blockr.viz ships no palette of its
#'   own.
#' @param pptx_left,pptx_top Slide placement in inches, stashed as attributes
#'   on the result for the officer-based pptx pipeline. Inert under the quarto
#'   render.
#'
#' @return A `flextable` object (with `pptx_left` / `pptx_top` attributes).
#'
#' @examplesIf requireNamespace("flextable", quietly = TRUE)
#' tbl <- summary_table(iris, vars = "Sepal.Length", by = "Species")
#' static_table(tbl, title = "Sepal length by species")
#' @seealso [as_annotated_df()], [write_annotated_xlsx()],
#'   [exhibit-options] for everything the renderer reads from `options()`
#' @export
static_table <- function(data, title = NULL, subtitle = NULL, caption = NULL,
                     na_rep = "\u2014",
                     font = getOption("blockr.viz.ft_font", "Inter"),
                     font_size = getOption("blockr.viz.ft_font_size", 13),
                     font_color = "#444444",
                     indent_width = 20,
                     first_col_width = 5.65, other_cols_width = 3.5,
                     fit_width = getOption("blockr.viz.ft_fit_width", NULL),
                     col_widths = getOption("blockr.viz.ft_col_widths",
                                            "measured"),
                     cell_padding = NULL,
                     auto_width = FALSE, continued = FALSE,
                     header_bg = getOption("blockr.viz.ft_header_bg",
                                           viz_palette("bands")),
                     pptx_left = 0.4, pptx_top = 1.1) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("static_table() needs the 'flextable' package.", call. = FALSE)
  }

  # Table-producing objects are coerced to a data frame, and long-form
  # summary_table internals are spread to the wide display grid.
  data <- as_annotated_df(data)
  data <- fmt_to_wide(data)

  # NULL fills from the display attributes, "" suppresses.
  if (is.null(title))    title    <- attr(data, "label")
  if (is.null(subtitle)) subtitle <- attr(data, "subtitle")
  if (is.null(caption))  caption  <- attr(data, "caption")

  # Row-side structure: grouping-value axes and variable blocks.
  view <- annotated_structure_view(data)
  df <- view$data
  section_cols <- view$section_cols
  n_row <- nrow(df)

  stub_col <- if (".label" %in% names(df)) ".label" else names(df)[1]
  # Dot-prefixed columns are structure, never data cells.
  data_cols <- setdiff(names(df), stub_col)
  data_cols <- data_cols[!startsWith(data_cols, ".")]
  n_data <- length(data_cols)
  n_col <- n_data + 1L

  indent <- if (".indent" %in% names(df)) {
    iv <- suppressWarnings(as.integer(df[[".indent"]]))
    iv[is.na(iv)] <- 0L
    iv
  } else {
    rep(0L, n_row)
  }
  bold_row <- if (".strong" %in% names(df)) {
    bv <- suppressWarnings(as.logical(df[[".strong"]]))
    !is.na(bv) & bv
  } else {
    rep(FALSE, n_row)
  }
  emph_row <- if (".emph" %in% names(df)) {
    ev <- suppressWarnings(as.logical(df[[".emph"]]))
    !is.na(ev) & ev
  } else {
    rep(FALSE, n_row)
  }

  # Two-level spanners: data-column names split on "||". The leaf header text
  # comes from attr(col, "label").
  parts <- strsplit(data_cols, "||", fixed = TRUE)
  top <- vapply(parts, function(p) if (length(p) > 1L) p[[1L]] else "",
                character(1))
  leaf <- vapply(data_cols, function(cn) {
    lbl <- attr(df[[cn]], "label")
    if (is.null(lbl) || !nzchar(lbl)) {
      utils::tail(strsplit(cn, "||", fixed = TRUE)[[1L]], 1L)
    } else {
      lbl
    }
  }, character(1))
  has_spanner <- any(nzchar(top))

  # ---- interleave section-header rows -----------------------------------
  # One bold merged row per section restart, where build_html_tbody() puts its
  # colspan rows: for each data row, the outermost changed level and every
  # level below it emit a header.
  k <- length(section_cols)
  diff_from <- rep(NA_integer_, n_row)
  path_mat <- matrix(character(), nrow = n_row, ncol = 0L)
  if (k > 0L && n_row > 0L) {
    path_mat <- vapply(section_cols, function(sc) {
      v <- as.character(df[[sc]])
      v[is.na(df[[sc]])] <- "(missing)"
      v
    }, character(n_row))
    if (is.null(dim(path_mat))) path_mat <- matrix(path_mat, nrow = n_row)
    for (L in k:1L) {
      col <- path_mat[, L]
      changed <- c(TRUE, col[-1L] != col[-n_row])
      diff_from[changed] <- L
    }
  }

  n_hdr_before <- ifelse(is.na(diff_from), 0L, k - diff_from + 1L)
  data_pos <- seq_len(n_row) + cumsum(n_hdr_before)
  hdr_pos <- integer(0)
  hdr_txt <- character(0)
  hdr_lvl <- integer(0)
  for (L in seq_len(k)) {
    at <- which(!is.na(diff_from) & diff_from <= L)
    if (!length(at)) next
    txt <- path_mat[at, L]
    lbl <- attr(df[[section_cols[L]]], "label")
    if (is.character(lbl) && length(lbl) == 1L && nzchar(lbl) &&
        lbl != section_cols[L]) {
      txt <- paste0(lbl, ": ", txt)
    }
    # A page that opens inside a section says so.
    if (isTRUE(continued) && length(at) && at[[1L]] == 1L) {
      txt[[1L]] <- paste0(txt[[1L]], " (continued)")
    }
    hdr_pos <- c(hdr_pos, data_pos[at] - (k - L + 1L))
    hdr_txt <- c(hdr_txt, txt)
    hdr_lvl <- c(hdr_lvl, rep(L, length(at)))
  }

  # Body grid, all character. NA becomes `na_rep` in data rows.
  total_out <- n_row + length(hdr_pos)
  body <- matrix("", nrow = total_out, ncol = n_col,
                 dimnames = list(NULL, c(stub_col, data_cols)))
  body[data_pos, 1L] <- as.character(df[[stub_col]])
  for (j in seq_len(n_data)) {
    v <- as.character(df[[data_cols[j]]])
    v[is.na(v)] <- na_rep %||% ""
    body[data_pos, j + 1L] <- v
  }
  if (length(hdr_pos)) body[hdr_pos, 1L] <- hdr_txt
  body_df <- as.data.frame(body, check.names = FALSE,
                           stringsAsFactors = FALSE)

  ft <- flextable::flextable(body_df)

  # ---- header rows ------------------------------------------------------
  # Bottom-up: leaf labels, the merged spanner row, then subtitle and title
  # lines (add_header_lines prepends, so the subtitle goes first). The stub
  # header comes from the stub column's `label` attribute, else blank.
  stub_hdr <- stub_header_rows(df[[stub_col]], 1L + has_spanner)
  stub_label <- utils::tail(stub_hdr$label, 1L)
  # Passed through `values` so a column named `x` is not matched to a formal
  # of set_header_labels().
  ft <- flextable::set_header_labels(
    ft,
    values = stats::setNames(as.list(c(stub_label, leaf)),
                             c(stub_col, data_cols))
  )
  if (has_spanner) {
    runs <- rle(top)
    ft <- flextable::add_header_row(
      ft,
      values = c(stub_hdr$label[[1L]], runs$values),
      colwidths = c(1L, runs$lengths),
      top = TRUE
    )
  }
  has_subtitle <- !is.null(subtitle) && nzchar(subtitle)
  has_title <- !is.null(title) && nzchar(title)
  if (has_subtitle) ft <- flextable::add_header_lines(ft, subtitle, top = TRUE)
  if (has_title) ft <- flextable::add_header_lines(ft, title, top = TRUE)
  title_i <- if (has_title) 1L else integer(0)
  subtitle_i <- if (has_subtitle) as.integer(has_title) + 1L else integer(0)
  n_line_rows <- has_title + has_subtitle
  spanner_i <- if (has_spanner) n_line_rows + 1L else integer(0)
  leaf_i <- n_line_rows + has_spanner + 1L

  if (!is.null(caption) && nzchar(caption)) {
    ft <- flextable::add_footer_lines(ft, caption)
  }

  # ---- theme ------------------------------------------------------------
  inner <- flextable::fp_border_default(color = "gray", width = 0.5)
  outer <- flextable::fp_border_default(color = "black", width = 1)
  ft <- ft |>
    flextable::font(fontname = font, part = "all") |>
    flextable::fontsize(size = font_size, part = "all") |>
    flextable::color(color = font_color, part = "all") |>
    flextable::height_all(height = 0.3 * (font_size / 14), unit = "in") |>
    flextable::border_inner_h(border = inner, part = "body") |>
    flextable::border_inner_v(border = inner, part = "body") |>
    flextable::border_outer(border = outer) |>
    flextable::padding(padding.top = 0, padding.bottom = 0, part = "all") |>
    flextable::valign(valign = "center", part = "all") |>
    flextable::align(align = "left", j = 1L, part = "all")
  # Numbers are right-aligned, with the leaf header over them. Spanners are
  # centred.
  if (n_data > 0L) {
    data_j <- 1L + seq_len(n_data)
    ft <- ft |>
      flextable::align(align = "right", j = data_j, part = "body") |>
      flextable::align(align = "right", i = leaf_i, j = data_j,
                       part = "header")
    if (has_spanner) {
      ft <- flextable::align(ft, align = "center", i = spanner_i, j = data_j,
                             part = "header")
    }
  }

  # Header styling: leaf and spanner rows bold, with colored bands per column
  # group from `header_bg` (see resolve_header_bands). Per-column emphasis
  # (`attr(df[[col]], "strong")` / `"emph"`) replaces the bands with the
  # emphasis ramp, and emph columns render italic.
  col_strong <- vapply(
    data_cols, function(cn) isTRUE(as.logical(attr(df[[cn]], "strong"))),
    logical(1L)
  )
  col_emph <- vapply(
    data_cols, function(cn) isTRUE(as.logical(attr(df[[cn]], "emph"))),
    logical(1L)
  )
  emphasis_mode <- any(col_strong | col_emph)

  hdr_rows <- c(spanner_i, leaf_i)
  ft <- flextable::bold(ft, i = hdr_rows, part = "header")
  # A producer that sets `label_bold` decides for the stub cells itself.
  if (!is.null(attr(df[[stub_col]], "label_bold", exact = TRUE)) &&
        any(!stub_hdr$bold)) {
    ft <- flextable::bold(ft, i = hdr_rows[!stub_hdr$bold], j = 1L,
                          bold = FALSE, part = "header")
  }

  band <- if (emphasis_mode) {
    ft_emphasis_bands(col_strong, col_emph)
  } else {
    resolve_header_bands(header_bg, top, data_cols)
  }
  if (!is.null(band)) {
    # Bands span both the spanner and leaf rows. A column whose group has no
    # color (NA) is left unfilled.
    for (j in seq_len(n_data)) {
      if (is.na(band$bg[j])) next
      ft <- ft |>
        flextable::bg(bg = band$bg[j], i = hdr_rows, j = j + 1L,
                      part = "header") |>
        flextable::color(color = band$text[j], i = hdr_rows, j = j + 1L,
                         part = "header")
    }
    # Stub header: colored by the emphasis ramp or a ".stub" entry.
    if (!is.na(band$stub_bg)) {
      ft <- ft |>
        flextable::bg(bg = band$stub_bg, i = hdr_rows, j = 1L,
                      part = "header") |>
        flextable::color(color = band$stub_text, i = hdr_rows, j = 1L,
                         part = "header")
    }
  }
  if (emphasis_mode) {
    for (j in which(col_emph)) {
      ft <- flextable::italic(ft, i = hdr_rows, j = j + 1L, part = "header")
    }
  }

  # Title / subtitle lines are already merged across the width.
  if (has_title) {
    ft <- ft |>
      flextable::bold(i = title_i, part = "header") |>
      flextable::fontsize(size = font_size + 2, i = title_i,
                          part = "header") |>
      flextable::align(align = "center", i = title_i, part = "header")
  }
  if (has_subtitle) {
    ft <- ft |>
      flextable::italic(i = subtitle_i, part = "header") |>
      flextable::align(align = "center", i = subtitle_i, part = "header")
  }
  if (!is.null(caption) && nzchar(caption)) {
    ft <- ft |>
      flextable::italic(i = 1L, part = "footer") |>
      flextable::fontsize(size = max(font_size - 4, 8), i = 1L,
                          part = "footer") |>
      flextable::align(align = "left", i = 1L, part = "footer")
  }

  # ---- body row styling -------------------------------------------------
  # Section headers: bold, merged across the width, indented one step per
  # nesting level above 1. Data rows: .indent padding on the stub and
  # .strong / .emph across the row.
  if (length(hdr_pos)) {
    for (r in seq_along(hdr_pos)) {
      ft <- flextable::merge_h_range(ft, i = hdr_pos[r], j1 = 1L, j2 = n_col)
    }
    ft <- flextable::bold(ft, i = hdr_pos, part = "body")
    deep <- hdr_lvl > 1L
    if (any(deep)) {
      for (lvl in unique(hdr_lvl[deep])) {
        ft <- flextable::padding(
          ft,
          i = hdr_pos[hdr_lvl == lvl], j = 1L,
          padding.left = (lvl - 1L) * indent_width
        )
      }
    }
  }
  for (lvl in sort(unique(indent[indent > 0L]))) {
    ft <- flextable::padding(
      ft,
      i = data_pos[indent == lvl], j = 1L,
      padding.left = lvl * indent_width
    )
  }
  if (any(bold_row)) {
    ft <- flextable::bold(ft, i = data_pos[bold_row], part = "body")
  }
  if (any(emph_row)) {
    ft <- flextable::italic(ft, i = data_pos[emph_row], part = "body")
  }
  # Per-cell paint (`.bg:<col>` / `.fg:<col>`, see annotation_cell_paint()).
  for (j in seq_len(n_data)) {
    paint <- annotation_cell_paint(df, data_cols[j])
    if (is.null(paint)) next
    for (hex in unique(stats::na.omit(paint$bg))) {
      ft <- flextable::bg(ft, i = data_pos[which(paint$bg == hex)],
                          j = j + 1L, bg = hex, part = "body")
    }
    for (hex in unique(stats::na.omit(paint$fg))) {
      ft <- flextable::color(ft, i = data_pos[which(paint$fg == hex)],
                             j = j + 1L, color = hex, part = "body")
    }
  }

  # ---- widths -----------------------------------------------------------
  # "measured" asks each column what its text needs (see
  # ft_measured_widths()). "even" gives the stub `first_col_width` and splits
  # the rest. Without `fit_width` the raw first/other widths apply and may
  # exceed the slide.
  widths <- NULL
  if (isTRUE(auto_width)) {
    ft <- flextable::autofit(ft)
  } else {
    if (is.numeric(col_widths) && length(col_widths) == n_col) {
      # Widths handed in by the pptx writer, the same on every page.
      widths <- col_widths
    } else if (!is.null(fit_width) && n_data > 0L &&
          identical(col_widths, "measured")) {

      measure <- function(pad) {
        ft_measured_widths(
          stub = body[data_pos, 1L],
          stub_indent = indent * indent_width,
          stub_label = stub_hdr$label,
          cells = body[data_pos, -1L, drop = FALSE],
          leaf = leaf, top = top,
          font = font, font_size = font_size, total = fit_width,
          banner = c(if (has_title) title, if (has_subtitle) subtitle),
          pad = pad
        )
      }

      pad <- ft_pad_width(cell_padding)
      widths <- measure(pad)

      # Padding is given back only when the data cells no longer fit. A header
      # that merely wraps keeps the spacing.
      if (isTRUE(attr(widths, "cell_squeeze")) && is.null(cell_padding)) {
        cell_padding <- TIGHT_PAD
        pad <- ft_pad_width(cell_padding)
        widths <- measure(pad)
      }
    }
    if (is.null(widths)) {
      if (!is.null(fit_width) && n_data > 0L) {
        first_col_width <- min(first_col_width, fit_width * 0.5)
        other_cols_width <- (fit_width - first_col_width) / n_data
      }
      widths <- c(first_col_width, rep(other_cols_width, n_data))
    }
    if (!is.null(cell_padding) && n_data > 0L) {
      # The stub's left padding carries the row indents, so only its right
      # edge changes.
      ft <- ft |>
        flextable::padding(j = 1L + seq_len(n_data),
                           padding.left = cell_padding,
                           padding.right = cell_padding, part = "all") |>
        flextable::padding(j = 1L, padding.right = cell_padding, part = "all")
    }
    for (j in seq_along(widths)) {
      ft <- flextable::width(ft, j = j, width = widths[[j]])
    }
  }

  attr(ft, "pptx_left") <- pptx_left
  attr(ft, "pptx_top") <- pptx_top
  # The source frame, so pptx_add_exhibit() can rebuild the table in pages.
  attr(ft, "exhibit_data") <- data
  # The input row of each rendered body row, `NA` for synthesized section
  # headers.
  row_map <- rep(NA_integer_, total_out)
  row_map[data_pos] <- seq_len(n_row)
  attr(ft, "row_map") <- row_map
  # `cell = TRUE` means the columns are narrower than their contents, which
  # row splitting cannot fix. The pptx writer steps the font down instead.
  attr(ft, "width_squeeze") <- c(
    header = isTRUE(attr(widths, "header_squeeze")),
    cell = isTRUE(attr(widths, "cell_squeeze"))
  )
  # The share of the longest header word the tightest column keeps (1 when
  # nothing breaks).
  attr(ft, "word_fit") <- attr(widths, "word_fit") %||% 1
  # What the width pass decided, so pages of one table share the layout.
  attr(ft, "layout_plan") <- list(
    col_widths = as.numeric(widths),
    cell_padding = cell_padding
  )
  # The header row that carries the leaf labels.
  attr(ft, "leaf_row") <- leaf_i
  ft
}
