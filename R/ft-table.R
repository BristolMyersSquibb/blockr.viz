#' Flextable Renderer for Annotated Data Frames
#'
#' Renders an [annotated data frame][as_annotated_df()] as a styled
#' [flextable][flextable::flextable()] -- the static table output for
#' report / deck rendering (quarto pptx, docx). flextable is the one table
#' engine whose `knit_print` emits real OpenXML tables in PowerPoint and
#' Word output, so this is the renderer blockr.outline emits for
#' table-kind blocks; [gt_table()] remains the HTML-oriented sibling.
#'
#' Structure comes from the annotated-df contract (shared with
#' [html_table()], [gt_table()] and [write_annotated_xlsx()]): `.label` is
#' the row stub, `.group<k>_level` runs and `.variable_label` blocks become
#' bold section-header rows interleaved into the body (merged across the
#' table width, exactly where the HTML renderer puts its colspan rows),
#' `.indent` / `.strong` / `.emph` style rows, `Top||Leaf` column names
#' become a merged spanner header row, and `attr(col, "label")` carries the
#' leaf header text (`"\n"` renders as a line break -- the Big-N
#' convention). The identity columns (`.variable*`, raw `.group<k>` pairs)
#' drive interactive drilling only and are ignored here.
#'
#' Columns can carry emphasis, the column analogue of the row `.strong` /
#' `.emph` flags: `attr(col, "strong")` / `attr(col, "emph")`. When any
#' column is flagged, the header band follows an emphasis ramp -- normal
#' light gray, `emph` dark gray (and italic), `strong` an accent -- so a
#' treatment arm stands out against a reference arm the same way row
#' emphasis works. This overrides the identity palette (`header_bg`).
#'
#' The look is the blockr.topline flextable theme: dense bordered grid,
#' first column left-aligned and wide, data columns centered, optional
#' colored header bands, manual column widths (PowerPoint never autofits --
#' size for the slide, don't reflow). Every aspect is parameterized; the
#' defaults reproduce the topline deck look.
#'
#' @param data A data frame (or an object with an [as_annotated_df()]
#'   method, e.g. a composer table -- coerced on entry).
#' @param title,subtitle,caption Table title / subtitle (header lines) and
#'   caption (footer line). `NULL` (default) auto-fills from the annotated
#'   df's `label` / `subtitle` / `caption` attributes; `""` suppresses.
#' @param na_rep Character. Text for missing (`NA`) cells. Default
#'   `"—"` (em dash), the clinical-table convention.
#' @param font,font_size,font_color Base typography, applied to all parts.
#' @param indent_width Points of left padding per `.indent` level (section
#'   headers indent at their nesting depth with the same step).
#' @param first_col_width,other_cols_width Column widths in inches, used
#'   unless `fit_width` or `auto_width` is set. Defaults fit the 13.33in
#'   widescreen template the topline decks use; pass smaller values for the
#'   10in quarto default reference doc.
#' @param fit_width Optional total table width in inches (usually the
#'   slide's usable content width). When set, the columns are distributed to
#'   fill it exactly -- the stub keeps `first_col_width` (capped at half the
#'   budget), the data columns share the rest -- so a wide table fits the
#'   slide instead of overflowing. Defaults from
#'   `getOption("blockr.viz.ft_fit_width")`, which the blockr.outline pptx /
#'   docx render sets from the reference template's slide size, so a table
#'   printed in a deck fits without the caller sizing it. Unset for html /
#'   pdf, where the natural widths apply.
#' @param auto_width Logical. `TRUE` runs [flextable::autofit()] instead of
#'   the manual widths. flextable never fits to the slide on its own, so
#'   manual widths (the default) are the safe choice for pptx.
#' @param header_bg Header band colors, keyed on the column-group structure
#'   the annotated df carries. A color vector (hex or R color names):
#'   \itemize{
#'     \item **Named** entries pin a group -- the name is a by-level value
#'       (the spanner top for a nested `by`, else the data-column name), and
#'       `".stub"` pins the row-stub header. `c(Placebo = "grey",
#'       Drug = "#33D6F1")` colors those arms explicitly, whatever their
#'       position.
#'     \item **Unnamed** entries are the cycle pool for the remaining,
#'       unpinned groups, in order -- `c("#A59F9F", "#33D6F1")` bands them.
#'     \item The two mix: `c(.stub = "#EEE", Placebo = "grey", "#33D6F1")`.
#'   }
#'   Defaults from `getOption("blockr.viz.ft_header_bg")`; unset (or `NULL` /
#'   `"none"`) leaves the header unfilled. blockr.viz ships no palette of its
#'   own -- the app supplies the colors (a deck sets the option to its house
#'   style), so nothing house-specific lives in the renderer.
#' @param pptx_left,pptx_top Slide placement in inches, stashed as
#'   `pptx_left` / `pptx_top` attributes on the result (consumed by an
#'   officer-based pptx pipeline; inert under the quarto render, where the
#'   reference template's content placeholder sets the table position).
#'
#' @return A `flextable` object (with `pptx_left` / `pptx_top` attributes).
#'
#' @examplesIf requireNamespace("flextable", quietly = TRUE)
#' tbl <- summary_table(iris, vars = "Sepal.Length", by = "Species")
#' ft_table(tbl, title = "Sepal length by species")
#' @seealso [as_annotated_df()], [gt_table()], [write_annotated_xlsx()]
#' @export
ft_table <- function(data, title = NULL, subtitle = NULL, caption = NULL,
                     na_rep = "—",
                     font = "Trebuchet MS", font_size = 14,
                     font_color = "#444444",
                     indent_width = 20,
                     first_col_width = 5.65, other_cols_width = 3.5,
                     fit_width = getOption("blockr.viz.ft_fit_width", NULL),
                     auto_width = FALSE,
                     header_bg = getOption("blockr.viz.ft_header_bg", NULL),
                     pptx_left = 0.4, pptx_top = 1.1) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("ft_table() needs the 'flextable' package.", call. = FALSE)
  }

  # Shared input contract: a data frame passes through untouched; a
  # table-producing object (composer et al.) is coerced first. Long-form
  # summary_table internals spread to the wide display grid (no-op on
  # already-wide input) -- same entry sequence as gt_table().
  data <- as_annotated_df(data)
  data <- fmt_to_wide(data)

  # NULL = auto from the annotated df's display attributes, "" = off (the
  # table block's title-tier convention).
  if (is.null(title))    title    <- attr(data, "label")
  if (is.null(subtitle)) subtitle <- attr(data, "subtitle")
  if (is.null(caption))  caption  <- attr(data, "caption")

  # Row-side structure: grouping-value axes + variable blocks, resolved the
  # same way as every other renderer of the contract.
  view <- annotated_structure_view(data)
  df <- view$data
  section_cols <- view$section_cols
  n_row <- nrow(df)

  stub_col <- if (".label" %in% names(df)) ".label" else names(df)[1]
  # ALL dot-prefixed columns are structure, never data cells (see
  # write_annotated_xlsx() -- identical rule).
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

  # Two-level column spanners: split data-column NAMES on "||"; the leaf
  # header text comes from attr(col, "label") (Big-N line included).
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
  # One bold merged row per section restart, exactly where build_html_tbody()
  # puts its colspan rows: for each data row, the outermost changed level and
  # every level below it emit a header. Same algorithm as the Excel writer.
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
    hdr_pos <- c(hdr_pos, data_pos[at] - (k - L + 1L))
    hdr_txt <- c(hdr_txt, txt)
    hdr_lvl <- c(hdr_lvl, rep(L, length(at)))
  }

  # Body grid, all character: data cells pre-formatted upstream, NA -> na_rep
  # (data rows only; header rows stay blank).
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
  # Bottom-up: leaf labels, then the merged spanner row on top, then
  # subtitle / title lines (add_header_lines prepends, so subtitle first).
  # The stub header uses the stub column's `label` attribute when the
  # producer set one (the topline block's `first_column_label`), else blank.
  stub_label <- attr(df[[stub_col]], "label")
  stub_label <- if (is.character(stub_label) && length(stub_label) == 1L) {
    stub_label
  } else {
    ""
  }
  ft <- do.call(flextable::set_header_labels,
                c(list(ft), stats::setNames(as.list(c(stub_label, leaf)),
                                            c(stub_col, data_cols))))
  if (has_spanner) {
    runs <- rle(top)
    ft <- flextable::add_header_row(
      ft,
      values = c("", runs$values),
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

  # ---- the topline theme ------------------------------------------------
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
  if (n_data > 0L) {
    ft <- flextable::align(ft, align = "center", j = 1L + seq_len(n_data),
                           part = "all")
  }

  # Column header styling: leaf + spanner rows bold and centered (stub
  # header cell stays left), colored bands per column group. Colors come
  # from `header_bg` (see resolve_header_bands): a named/unnamed color map,
  # keyed on the by-level values the annotated df already carries. blockr.viz
  # knows no palette of its own -- the app supplies the colors (option or
  # argument), so nothing house-specific lives here.
  # Per-column emphasis, the column analogue of the row .strong / .emph
  # flags: `attr(df[[col]], "strong")` / `"emph"`. When any column carries
  # one, the header band is driven by an emphasis ramp (normal light gray /
  # emph dark gray / strong accent) instead of the identity palette, and emph
  # columns render italic -- so "reference recedes, treatment stands out"
  # reads the same as the row emphasis it mirrors.
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

  band <- if (emphasis_mode) {
    ft_emphasis_bands(col_strong, col_emph)
  } else {
    resolve_header_bands(header_bg, top, data_cols)
  }
  if (!is.null(band)) {
    # Bands span BOTH the spanner and leaf rows: within a spanner group every
    # leaf shares one color, so the merged spanner cell reads as one band,
    # not a stripe. A column whose group has no color (NA) is left unfilled.
    for (j in seq_len(n_data)) {
      if (is.na(band$bg[j])) next
      ft <- ft |>
        flextable::bg(bg = band$bg[j], i = hdr_rows, j = j + 1L,
                      part = "header") |>
        flextable::color(color = band$text[j], i = hdr_rows, j = j + 1L,
                         part = "header")
    }
    # Stub header: colored when the emphasis ramp fills it, or when the map
    # pins it via a ".stub" entry.
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

  # Title / subtitle lines: add_header_lines already merges them across the
  # width; style on top of the base theme.
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
  # nesting level above 1. Data rows: .indent padding on the stub,
  # .strong / .emph across the row (the deck look bolds the whole line).
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

  # ---- widths -----------------------------------------------------------
  # PowerPoint never autofits a flextable; manual widths sized for the slide
  # are the default (the topline lesson). `fit_width` (inches, usually the
  # slide's usable content width) distributes columns to exactly fill it:
  # the stub keeps its width (capped at half the budget so data columns stay
  # legible), the data columns share the remainder equally. Unset -> the raw
  # first/other widths, which can exceed the slide (the caller's problem).
  if (isTRUE(auto_width)) {
    ft <- flextable::autofit(ft)
  } else {
    if (!is.null(fit_width) && n_data > 0L) {
      first_col_width <- min(first_col_width, fit_width * 0.5)
      other_cols_width <- (fit_width - first_col_width) / n_data
    }
    ft <- flextable::width(ft, j = 1L, width = first_col_width)
    if (n_data > 0L) {
      ft <- flextable::width(ft, j = 1L + seq_len(n_data),
                             width = other_cols_width)
    }
  }

  attr(ft, "pptx_left") <- pptx_left
  attr(ft, "pptx_top") <- pptx_top
  ft
}

# Readable text color for a fill, by luminance (Rec. 601). Any hex or R
# color name; NA in -> NA out (no fill, no text override). blockr.viz carries
# no house palette, so this is the only color knowledge here.
ft_contrast_text <- function(bg) {
  vapply(bg, function(col) {
    if (is.na(col)) return(NA_character_)
    lum <- tryCatch(
      sum(grDevices::col2rgb(col)[, 1L] * c(0.299, 0.587, 0.114)),
      error = function(e) 255
    )
    if (lum < 140) "#FFFFFF" else "#333333"
  }, character(1L), USE.NAMES = FALSE)
}

# The emphasis ramp for column .strong / .emph: normal / emph / strong ->
# light gray / dark gray / accent. Generic UI values (not a house palette),
# overridable via option -- the same guarded-fallback shape the chart uses
# for its palette (blockr.theme has no R-side token API, so these are local
# defaults; the accent doubles as the chart's first series color feel).
FT_EMPHASIS_DEFAULT <- c(normal = "#EEEEEE", emph = "#9AA3B0",
                         strong = "#2563EB")

# Per-column bg / text for emphasis mode. strong -> accent, emph -> dark gray,
# normal -> light gray; the stub takes the normal (light) band so the header
# reads as one strip. Contrast text by luminance.
ft_emphasis_bands <- function(col_strong, col_emph) {
  cols <- getOption("blockr.viz.ft_emphasis_colors", FT_EMPHASIS_DEFAULT)
  pick <- function(key) cols[[key]]
  bg <- ifelse(col_strong, pick("strong"),
               ifelse(col_emph, pick("emph"), pick("normal")))
  list(
    bg = unname(bg), text = ft_contrast_text(unname(bg)),
    stub_bg = unname(pick("normal")),
    stub_text = ft_contrast_text(unname(pick("normal")))
  )
}

# Resolve `header_bg` to per-data-column fills, keyed on the column-group
# structure the annotated df carries. `header_bg` is a color vector:
#   * NAMED entries PIN a group: the name is a by-level value (the spanner
#     top for nested `by`, else the data-column name); ".stub" pins the row-
#     stub header. So `c(Placebo = "grey", "Drug" = "#33D6F1")` colours those
#     arms explicitly (placebo grey, treatment blue) whatever their position.
#   * UNNAMED entries are the cycle pool for the remaining, unpinned groups
#     (in order of first appearance) -- `c("#A59F9F", "#33D6F1")` bands them.
#   * The two mix: `c(.stub = "#EEE", Placebo = "grey", "#33D6F1", "#FDA97C")`.
# Colors are hex or R color names -- NO palette-name lookup, so no house
# style lives in blockr.viz; the app supplies the actual values (typically via
# getOption("blockr.viz.ft_header_bg")). `NULL` / `FALSE` / `"none"` -> no
# fill. Returns per-column bg/text (length = n data cols; NA = unfilled) plus
# stub_bg/stub_text, or NULL when there is nothing to fill.
resolve_header_bands <- function(header_bg, top, data_cols) {
  n <- length(data_cols)
  if (n == 0L) return(NULL)
  if (is.null(header_bg) || isFALSE(header_bg) ||
        identical(header_bg, "none")) {
    return(NULL)
  }

  nm <- names(header_bg)
  header_bg <- as.character(header_bg)   # (drops names -- captured above)
  if (is.null(nm)) nm <- rep("", length(header_bg))
  pins <- header_bg[nzchar(nm)]
  names(pins) <- nm[nzchar(nm)]
  pool <- header_bg[!nzchar(nm)]

  # Group key per data column: the spanner top when nested, else the column
  # name itself. Unpinned groups draw from the cycle pool in appearance order.
  keys <- ifelse(nzchar(top), top, data_cols)
  groups <- unique(keys)
  gcol <- setNames(rep(NA_character_, length(groups)), groups)
  taken <- 0L
  for (g in groups) {
    if (g %in% names(pins)) {
      gcol[[g]] <- pins[[g]]
    } else if (length(pool)) {
      taken <- taken + 1L
      gcol[[g]] <- pool[[((taken - 1L) %% length(pool)) + 1L]]
    }
  }

  bg <- unname(gcol[keys])
  stub_bg <- if (".stub" %in% names(pins)) pins[[".stub"]] else NA_character_

  list(
    bg = bg, text = ft_contrast_text(bg),
    stub_bg = stub_bg, stub_text = ft_contrast_text(stub_bg)
  )
}
