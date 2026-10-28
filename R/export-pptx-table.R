#' Flextable Renderer for Annotated Data Frames
#'
#' Renders an [annotated data frame][as_annotated_df()] as a styled
#' [flextable][flextable::flextable()], the static table output for report and
#' deck rendering (quarto pptx, docx).
#'
#' Structure comes from the annotated-df contract (shared with `html_table()`
#' and [write_annotated_xlsx()]): `.label` is the row stub, `.group<k>_level`
#' runs and `.variable_label` blocks become bold section-header rows merged
#' across the table width, `.indent` / `.strong` / `.emph` style rows,
#' `Top||Leaf` column names become a merged spanner header row, and
#' `attr(col, "label")` carries the leaf header text (`"\n"` is a line
#' break). The other identity columns are ignored here.
#'
#' Columns can carry emphasis, the column analogue of the row flags:
#' `attr(col, "strong")` / `attr(col, "emph")`. When any column is flagged,
#' the header band follows an emphasis ramp (normal light gray, `emph` dark
#' gray and italic, `strong` an accent) and `header_bg` is ignored.
#'
#' @param data A data frame (or an object with an [as_annotated_df()]
#'   method, e.g. a composer table, coerced on entry).
#' @param title,subtitle,caption Table title / subtitle (header lines) and
#'   caption (footer line). `NULL` (default) fills from the annotated df's
#'   `label` / `subtitle` / `caption` attributes; `""` suppresses.
#' @param na_rep Character. Text for missing (`NA`) cells. Defaults to an em
#'   dash.
#' @param font,font_size,font_color Base typography, applied to all parts.
#'   `font_size` is in points and defaults from
#'   `getOption("blockr.viz.ft_font_size")`, 13 unset. `font` defaults from
#'   `getOption("blockr.viz.ft_font")`, which a theme sets
#'   (`exhibits = list(ft_font = ...)`) and which the pptx export sets from the
#'   reference deck's font scheme.
#' @param indent_width Points of left padding per `.indent` level (section
#'   headers indent at their nesting depth with the same step).
#' @param first_col_width,other_cols_width Column widths in inches, used
#'   unless `fit_width` or `auto_width` is set. Defaults fit the 13.33in
#'   widescreen template.
#' @param fit_width Optional total table width in inches (usually the slide's
#'   usable content width). The columns are distributed to fill it exactly
#'   (`col_widths` says how). Defaults from
#'   `getOption("blockr.viz.ft_fit_width")`, which the pptx / docx render sets
#'   from the reference template. Unset for html / pdf.
#' @param col_widths How `fit_width` is split, `"measured"` (default) or
#'   `"even"`. Measured gives every column what its own text needs at the
#'   current font: no cell wraps and no header breaks inside a word when the
#'   slide can hold them, and the rest goes to the row stub (at most
#'   `getOption("blockr.viz.ft_stub_share")`, 0.3, of the width while the
#'   data columns are short of their full headers). When the slide cannot hold
#'   them all, the columns are filled to a common level, from their longest
#'   word toward their whole text. Even gives the stub `first_col_width`
#'   capped at half the slide and splits the remainder equally. A numeric
#'   vector of widths (one per column, stub first) is used as given;
#'   [write_exhibit_pptx()] uses it to give every page of a split table the
#'   same widths. Defaults from `getOption("blockr.viz.ft_col_widths")`.
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
#'   `"none"`) leaves the header unfilled.
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

  data <- fmt_to_wide(as_annotated_df(data))
  s <- st_structure(
    data,
    title = title %||% attr(data, "label"),
    subtitle = subtitle %||% attr(data, "subtitle"),
    caption = caption %||% attr(data, "caption"),
    na_rep = na_rep, continued = continued
  )
  ft <- st_flextable(s, font, font_size, font_color, header_bg, indent_width)

  widths <- NULL
  if (isTRUE(auto_width)) {
    ft <- flextable::autofit(ft)
  } else {
    measure_args <- c(s$measure, list(font = font, font_size = font_size,
                                      total = fit_width))
    fitted <- st_fit_widths(measure_args, length(s$data_cols), col_widths,
                            fit_width, cell_padding, first_col_width,
                            other_cols_width, indent_width)
    widths <- fitted$widths
    cell_padding <- fitted$cell_padding
    ft <- st_set_widths(ft, widths, cell_padding, length(s$data_cols))
  }

  # The source frame, so pptx_add_exhibit() can rebuild the table in pages,
  # and the input row of each rendered body row (`NA` for section headers).
  row_map <- rep(NA_integer_, nrow(s$body))
  row_map[s$sec$data_pos] <- seq_len(nrow(s$df))
  attr(ft, "pptx_left") <- pptx_left
  attr(ft, "pptx_top") <- pptx_top
  attr(ft, "exhibit_data") <- data
  attr(ft, "row_map") <- row_map
  # `cell = TRUE` means the columns are narrower than their contents, which
  # row splitting cannot fix. The pptx writer steps the font down instead.
  attr(ft, "width_squeeze") <- c(
    header = isTRUE(attr(widths, "header_squeeze")),
    cell = isTRUE(attr(widths, "cell_squeeze"))
  )
  # The share of the longest header word the tightest column keeps.
  attr(ft, "word_fit") <- attr(widths, "word_fit") %||% 1
  # What the width pass decided, so pages of one table share the layout.
  attr(ft, "layout_plan") <- list(col_widths = as.numeric(widths),
                                  cell_padding = cell_padding)
  attr(ft, "leaf_row") <- s$leaf_i
  ft
}

# The table's parsed structure: the display frame `df` and its stub and data
# columns, row flags, spanner (`top`) and `leaf` labels, section rows, the body
# grid, the stub header, the header line texts, the header row indexes and the
# arguments of ft_measured_widths() that depend only on the data.
st_structure <- function(data, title, subtitle, caption, na_rep, continued) {
  view <- annotated_structure_view(data)
  df <- view$data
  stub_col <- if (".label" %in% names(df)) ".label" else names(df)[1]
  # Dot-prefixed columns are structure, never data cells.
  data_cols <- setdiff(names(df), stub_col)
  data_cols <- data_cols[!startsWith(data_cols, ".")]
  labels <- st_column_labels(df, data_cols)
  indent <- st_dot_values(df, ".indent", as.integer, 0L)
  has_spanner <- any(nzchar(labels$top))
  sec <- st_section_rows(df, view$section_cols, continued)
  body <- st_body_grid(df, stub_col, data_cols, sec, na_rep)
  stub_hdr <- stub_header_rows(df[[stub_col]], 1L + has_spanner)
  n_line_rows <- st_has(title) + st_has(subtitle)
  list(
    df = df, stub_col = stub_col, data_cols = data_cols,
    top = labels$top, leaf = labels$leaf,
    indent = indent,
    bold_row = st_dot_values(df, ".strong", as.logical, FALSE),
    emph_row = st_dot_values(df, ".emph", as.logical, FALSE),
    sec = sec, body = body, stub_hdr = stub_hdr,
    title = title, subtitle = subtitle, caption = caption,
    spanner_i = if (has_spanner) n_line_rows + 1L else integer(0),
    leaf_i = n_line_rows + has_spanner + 1L,
    measure = list(
      stub = body[sec$data_pos, 1L],
      stub_indent = indent,
      stub_label = stub_hdr$label,
      cells = body[sec$data_pos, -1L, drop = FALSE],
      leaf = labels$leaf, top = labels$top,
      banner = c(if (st_has(title)) title, if (st_has(subtitle)) subtitle)
    )
  )
}

# The styled flextable, before widths.
st_flextable <- function(s, font, font_size, font_color, header_bg,
                         indent_width) {
  ft <- flextable::flextable(as.data.frame(s$body, check.names = FALSE,
                                           stringsAsFactors = FALSE))
  ft <- st_add_header(ft, s)
  ft <- st_theme(ft, font, font_size, font_color)
  ft <- st_align_data(ft, length(s$data_cols), s$spanner_i, s$leaf_i)
  ft <- st_style_header(ft, s, header_bg)
  ft <- st_style_lines(ft, s$title, s$subtitle, s$caption, font_size)
  st_style_body(ft, s, indent_width)
}

# TRUE for a non-empty text line.
st_has <- function(x) !is.null(x) && nzchar(x)

# A dot-prefixed structure column converted by `convert`, NA and absent as `na`.
st_dot_values <- function(df, col, convert, na) {
  if (!col %in% names(df)) {
    return(rep(na, nrow(df)))
  }
  v <- suppressWarnings(convert(df[[col]]))
  v[is.na(v)] <- na
  v
}

# The part of each `Top||Leaf` column name before the bars, else "".
st_spanner_top <- function(cols) {
  vapply(strsplit(cols, "||", fixed = TRUE),
         function(p) if (length(p) > 1L) p[[1L]] else "", character(1))
}

# Spanner and leaf header text (`attr(col, "label")`, else the part after the
# bars) per column.
st_column_labels <- function(df, data_cols) {
  list(
    top = st_spanner_top(data_cols),
    leaf = vapply(data_cols, function(cn) {
      lbl <- attr(df[[cn]], "label")
      if (is.null(lbl) || !nzchar(lbl)) {
        utils::tail(strsplit(cn, "||", fixed = TRUE)[[1L]], 1L)
      } else {
        lbl
      }
    }, character(1))
  )
}

# Where the section-header rows go. Like build_html_tbody(), each data row
# whose outermost section level changed emits a header for that level and
# every level below it. Returns the output row of each data row (`data_pos`)
# and the position, text and level of each header (`pos`, `txt`, `lvl`).
st_section_rows <- function(df, section_cols, continued = FALSE) {
  n_row <- nrow(df)
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

  n_before <- ifelse(is.na(diff_from), 0L, k - diff_from + 1L)
  data_pos <- seq_len(n_row) + cumsum(n_before)
  out <- list(data_pos = data_pos, pos = integer(0), txt = character(0),
              lvl = integer(0))
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
    if (isTRUE(continued) && at[[1L]] == 1L) {
      txt[[1L]] <- paste0(txt[[1L]], " (continued)")
    }
    out$pos <- c(out$pos, data_pos[at] - (k - L + 1L))
    out$txt <- c(out$txt, txt)
    out$lvl <- c(out$lvl, rep(L, length(at)))
  }
  out
}

# The body grid, all character. NA becomes `na_rep` in data rows.
st_body_grid <- function(df, stub_col, data_cols, sec, na_rep) {
  body <- matrix("", nrow = nrow(df) + length(sec$pos),
                 ncol = length(data_cols) + 1L,
                 dimnames = list(NULL, c(stub_col, data_cols)))
  body[sec$data_pos, 1L] <- as.character(df[[stub_col]])
  for (j in seq_along(data_cols)) {
    v <- as.character(df[[data_cols[j]]])
    v[is.na(v)] <- na_rep %||% ""
    body[sec$data_pos, j + 1L] <- v
  }
  if (length(sec$pos)) body[sec$pos, 1L] <- sec$txt
  body
}

# Header rows, bottom-up: leaf labels, the merged spanner row, then the
# subtitle and title lines (add_header_lines prepends), then the footnote.
st_add_header <- function(ft, s) {
  # Passed through `values` so a column named `x` is not matched to a formal
  # of set_header_labels().
  ft <- flextable::set_header_labels(
    ft,
    values = stats::setNames(
      as.list(c(utils::tail(s$stub_hdr$label, 1L), s$leaf)),
      c(s$stub_col, s$data_cols)
    )
  )
  if (length(s$spanner_i)) {
    runs <- rle(s$top)
    ft <- flextable::add_header_row(
      ft,
      values = c(s$stub_hdr$label[[1L]], runs$values),
      colwidths = c(1L, runs$lengths),
      top = TRUE
    )
  }
  if (st_has(s$subtitle)) {
    ft <- flextable::add_header_lines(ft, s$subtitle, top = TRUE)
  }
  if (st_has(s$title)) ft <- flextable::add_header_lines(ft, s$title, top = TRUE)
  if (st_has(s$caption)) ft <- flextable::add_footer_lines(ft, s$caption)
  ft
}

st_theme <- function(ft, font, font_size, font_color) {
  inner <- flextable::fp_border_default(color = "gray", width = 0.5)
  outer <- flextable::fp_border_default(color = "black", width = 1)
  ft |>
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
}

# Numbers are right-aligned, with the leaf header over them.
st_align_data <- function(ft, n_data, spanner_i, leaf_i) {
  if (n_data == 0L) {
    return(ft)
  }
  data_j <- 1L + seq_len(n_data)
  ft <- ft |>
    flextable::align(align = "right", j = data_j, part = "body") |>
    flextable::align(align = "right", i = leaf_i, j = data_j, part = "header")
  if (length(spanner_i)) {
    ft <- flextable::align(ft, align = "center", i = spanner_i, j = data_j,
                           part = "header")
  }
  ft
}

# Background and text color on header cells.
st_fill <- function(ft, bg, text, i, j) {
  ft |>
    flextable::bg(bg = bg, i = i, j = j, part = "header") |>
    flextable::color(color = text, i = i, j = j, part = "header")
}

# Leaf and spanner rows bold, with a colored band per column group from
# `header_bg` (see resolve_header_bands). Per-column emphasis replaces the
# bands with the emphasis ramp, and emph columns render italic.
st_style_header <- function(ft, s, header_bg) {
  df <- s$df
  data_cols <- s$data_cols
  stub_hdr <- s$stub_hdr
  hdr_rows <- c(s$spanner_i, s$leaf_i)
  col_flag <- function(key) {
    vapply(data_cols, function(cn) isTRUE(as.logical(attr(df[[cn]], key))),
           logical(1L))
  }
  col_strong <- col_flag("strong")
  col_emph <- col_flag("emph")
  emphasis_mode <- any(col_strong | col_emph)

  ft <- flextable::bold(ft, i = hdr_rows, part = "header")
  # A producer that sets `label_bold` decides for the stub cells itself.
  if (!is.null(attr(df[[s$stub_col]], "label_bold", exact = TRUE)) &&
        any(!stub_hdr$bold)) {
    ft <- flextable::bold(ft, i = hdr_rows[!stub_hdr$bold], j = 1L,
                          bold = FALSE, part = "header")
  }

  band <- if (emphasis_mode) {
    ft_emphasis_bands(col_strong, col_emph)
  } else {
    resolve_header_bands(header_bg, s$top, data_cols)
  }
  if (!is.null(band)) {
    # Bands span the spanner and leaf rows. A column whose group has no color
    # (NA) is left unfilled.
    for (j in seq_along(data_cols)) {
      if (is.na(band$bg[j])) next
      ft <- st_fill(ft, band$bg[j], band$text[j], hdr_rows, j + 1L)
    }
    if (!is.na(band$stub_bg)) {
      ft <- st_fill(ft, band$stub_bg, band$stub_text, hdr_rows, 1L)
    }
  }
  for (j in which(col_emph)) {
    ft <- flextable::italic(ft, i = hdr_rows, j = j + 1L, part = "header")
  }
  ft
}

# Title and subtitle lines (already merged across the width) and the footnote.
st_style_lines <- function(ft, title, subtitle, caption, font_size) {
  if (st_has(title)) {
    ft <- ft |>
      flextable::bold(i = 1L, part = "header") |>
      flextable::fontsize(size = font_size + 2, i = 1L, part = "header") |>
      flextable::align(align = "center", i = 1L, part = "header")
  }
  if (st_has(subtitle)) {
    i <- as.integer(st_has(title)) + 1L
    ft <- ft |>
      flextable::italic(i = i, part = "header") |>
      flextable::align(align = "center", i = i, part = "header")
  }
  if (st_has(caption)) {
    ft <- ft |>
      flextable::italic(i = 1L, part = "footer") |>
      flextable::fontsize(size = max(font_size - 4, 8), i = 1L,
                          part = "footer") |>
      flextable::align(align = "left", i = 1L, part = "footer")
  }
  ft
}

# Section headers: bold, merged across the width, indented one step per
# nesting level above 1. Data rows: `.indent` padding on the stub, `.strong` /
# `.emph` across the row, and per-cell paint (`.bg:<col>` / `.fg:<col>`, see
# annotation_cell_paint()).
st_style_body <- function(ft, s, indent_width) {
  sec <- s$sec
  data_pos <- sec$data_pos
  data_cols <- s$data_cols
  indent <- s$indent
  if (length(sec$pos)) {
    for (r in seq_along(sec$pos)) {
      ft <- flextable::merge_h_range(ft, i = sec$pos[r], j1 = 1L,
                                     j2 = length(data_cols) + 1L)
    }
    ft <- flextable::bold(ft, i = sec$pos, part = "body")
    for (lvl in unique(sec$lvl[sec$lvl > 1L])) {
      ft <- flextable::padding(ft, i = sec$pos[sec$lvl == lvl], j = 1L,
                               padding.left = (lvl - 1L) * indent_width)
    }
  }
  for (lvl in sort(unique(indent[indent > 0L]))) {
    ft <- flextable::padding(ft, i = data_pos[indent == lvl], j = 1L,
                             padding.left = lvl * indent_width)
  }
  if (any(s$bold_row)) {
    ft <- flextable::bold(ft, i = data_pos[s$bold_row], part = "body")
  }
  if (any(s$emph_row)) {
    ft <- flextable::italic(ft, i = data_pos[s$emph_row], part = "body")
  }
  for (j in seq_along(data_cols)) {
    paint <- annotation_cell_paint(s$df, data_cols[j])
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
  ft
}

# Cell padding and widths. The stub's left padding carries the row indents, so
# only its right edge changes.
st_set_widths <- function(ft, widths, cell_padding, n_data) {
  if (!is.null(cell_padding) && n_data > 0L) {
    ft <- ft |>
      flextable::padding(j = 1L + seq_len(n_data),
                         padding.left = cell_padding,
                         padding.right = cell_padding, part = "all") |>
      flextable::padding(j = 1L, padding.right = cell_padding, part = "all")
  }
  for (j in seq_along(widths)) {
    ft <- flextable::width(ft, j = j, width = widths[[j]])
  }
  ft
}

# Column widths in inches, and the cell padding they were measured with.
# "measured" asks each column what its text needs (see ft_measured_widths(),
# whose arguments are `measure_args`). "even" gives the stub `first_col_width`
# and splits the rest. Without `fit_width` the raw first / other widths apply
# and may exceed the slide.
st_fit_widths <- function(measure_args, n_data, col_widths, fit_width,
                          cell_padding, first_col_width, other_cols_width,
                          indent_width) {
  measure_args$stub_indent <- measure_args$stub_indent * indent_width
  widths <- NULL
  if (is.numeric(col_widths) && length(col_widths) == n_data + 1L) {
    # Widths handed in by the pptx writer, the same on every page.
    widths <- col_widths
  } else if (!is.null(fit_width) && n_data > 0L &&
               identical(col_widths, "measured")) {
    measure <- function(pad) {
      do.call(ft_measured_widths, c(measure_args, list(pad = pad)))
    }
    widths <- measure(ft_pad_width(cell_padding))
    # Padding is given back only when the data cells no longer fit. A header
    # that merely wraps keeps the spacing.
    if (isTRUE(attr(widths, "cell_squeeze")) && is.null(cell_padding)) {
      cell_padding <- TIGHT_PAD
      widths <- measure(ft_pad_width(cell_padding))
    }
  }
  if (is.null(widths)) {
    if (!is.null(fit_width) && n_data > 0L) {
      first_col_width <- min(first_col_width, fit_width * 0.5)
      other_cols_width <- (fit_width - first_col_width) / n_data
    }
    widths <- c(first_col_width, rep(other_cols_width, n_data))
  }
  list(widths = widths, cell_padding = cell_padding)
}
