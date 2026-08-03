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
#' `html_table()`, [gt_table()] and [write_annotated_xlsx()]): `.label` is
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
#' @param na_rep Character. Text for missing (`NA`) cells. Defaults to an em
#'   dash, the clinical-table convention.
#' @param font,font_size,font_color Base typography, applied to all parts.
#'   `font_size` is in points and defaults from
#'   `getOption("blockr.viz.ft_font_size")`, 13 unset: a deck table is read
#'   from a screen, and the two points under the old 14 are two more rows on
#'   the slide.
#'   `font` defaults from `getOption("blockr.viz.ft_font")`, which a theme
#'   sets (`exhibits = list(ft_font = ...)`) and which the blockr.outline pptx
#'   render sets from the reference deck's own font scheme -- so a table
#'   printed into a house deck is set in that deck's face rather than in a
#'   font the renderer picked. Unset, the blockr default applies.
#' @param indent_width Points of left padding per `.indent` level (section
#'   headers indent at their nesting depth with the same step).
#' @param first_col_width,other_cols_width Column widths in inches, used
#'   unless `fit_width` or `auto_width` is set. Defaults fit the 13.33in
#'   widescreen template the topline decks use; pass smaller values for the
#'   10in quarto default reference doc.
#' @param fit_width Optional total table width in inches (usually the
#'   slide's usable content width). When set, the columns are distributed to
#'   fill it exactly, so a wide table fits the slide instead of overflowing
#'   (`col_widths` says how). Defaults from
#'   `getOption("blockr.viz.ft_fit_width")`, which the blockr.outline pptx /
#'   docx render sets from the reference template's slide size, so a table
#'   printed in a deck fits without the caller sizing it. Unset for html /
#'   pdf, where the natural widths apply.
#' @param col_widths How `fit_width` is split, `"measured"` (default) or
#'   `"even"`. Measured asks every column what its own text needs at the
#'   current font and gives the data columns enough that a cell never wraps
#'   and a header never breaks inside a word, leaving the rest to the row
#'   stub, which is the one column whose text is prose and wraps gracefully.
#'   While the data columns are still short of their full headers the stub
#'   takes at most `getOption("blockr.viz.ft_stub_share")` (0.3) of the width
#'   and wraps; it grows past that only with what is left over.
#'   Even is the older positional rule (the stub takes `first_col_width`
#'   capped at half the slide, the data columns share the remainder equally),
#'   which is wrong whenever the stub is shorter or the data columns wider
#'   than those constants assume. Defaults from
#'   `getOption("blockr.viz.ft_col_widths")`.
#'
#'   A numeric vector of widths (one per column, stub first) is also accepted
#'   and used as given. That is how [write_exhibit_pptx()] gives every page of
#'   a split table the widths measured on the whole of it.
#' @param cell_padding Points of left and right padding per cell, or `NULL`
#'   (default) for the flextable default of 5. Set automatically to a tighter
#'   value when a table cannot fit any other way: across 36 columns the
#'   default spends 5in of a widescreen slide on padding alone.
#' @param continued Logical. `TRUE` marks the section headings this table
#'   opens with as `(continued)`, for a page of a table split across slides
#'   whose first rows carry on a section from the page before.
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
#' static_table(tbl, title = "Sepal length by species")
#' @seealso [as_annotated_df()], [gt_table()], [write_annotated_xlsx()]
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
    # A page of a split table opens in the middle of a section, and the
    # section headers it re-emits say so: the reader has to know that the
    # first block on this slide is the tail of one that began on the last.
    if (isTRUE(continued) && length(at) && at[[1L]] == 1L) {
      txt[[1L]] <- paste0(txt[[1L]], " (continued)")
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
  # Through `values`, not through `...`: the labels are keyed by COLUMN NAME,
  # and a column called `x` (or `values`) then matches set_header_labels()'s
  # own formals instead of naming a column -- so `data.frame(x = ..)` failed
  # with "supports only flextable objects", the label having been passed as
  # the flextable. The list argument is the documented way to say the names
  # are data, not arguments.
  ft <- flextable::set_header_labels(
    ft,
    values = stats::setNames(as.list(c(stub_label, leaf)),
                             c(stub_col, data_cols))
  )
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
  # slide's usable content width) distributes columns to exactly fill it.
  # "measured" asks each column what its text needs (see
  # ft_measured_widths()); "even" is the older positional split, where the
  # stub keeps `first_col_width` and the data columns share the rest. Unset
  # `fit_width` -> the raw first/other widths, which can exceed the slide
  # (the caller's problem).
  widths <- NULL
  if (isTRUE(auto_width)) {
    ft <- flextable::autofit(ft)
  } else {
    if (is.numeric(col_widths) && length(col_widths) == n_col) {
      # Widths handed in: the pptx writer measures the whole table once and
      # gives every page the same numbers, so the columns line up when the
      # reader flips between slides.
      widths <- col_widths
    } else if (!is.null(fit_width) && n_data > 0L &&
          identical(col_widths, "measured")) {

      measure <- function(pad) {
        ft_measured_widths(
          stub = body[data_pos, 1L],
          stub_indent = indent * indent_width,
          stub_label = stub_label,
          cells = body[data_pos, -1L, drop = FALSE],
          leaf = leaf, top = top,
          font = font, font_size = font_size, total = fit_width,
          banner = c(if (has_title) title, if (has_subtitle) subtitle),
          pad = pad
        )
      }

      pad <- ft_pad_width(cell_padding)
      widths <- measure(pad)

      # Padding is the last width there is to give back, and worth giving when
      # the data cells no longer fit: below that the columns are narrower than
      # a character and PowerPoint stacks the letters one per line. A header
      # that merely wraps is not worth it -- the table keeps its spacing and
      # the header takes a second line.
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
      # Only the data columns and the stub's right edge: the stub's LEFT
      # padding carries the row indents, and resetting it would flatten them.
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
  # The frame this table was built from, coerced and spread exactly as the
  # renderer saw it. A consumer holding only the rendered exhibit -- the slide
  # builder evaluates a report call and gets a flextable back -- can hand it
  # to pptx_add_exhibit() and still have the table paged over slides, because
  # the pages are rebuilt from this rather than cut out of the flextable.
  attr(ft, "exhibit_data") <- data
  # Which input row each rendered body row came from, `NA` for the section
  # headers the renderer synthesized. A consumer that has to cut the table
  # into pages (write_exhibit_pptx()) measures the rendered rows and slices
  # the input, and this is what connects the two.
  row_map <- rep(NA_integer_, total_out)
  row_map[data_pos] <- seq_len(n_row)
  attr(ft, "row_map") <- row_map
  # Whether the slide was wide enough after every escalation. `cell = TRUE`
  # means the columns are narrower than their contents even so, which no
  # amount of row splitting can help -- the pptx writer reads it and steps the
  # font down instead.
  attr(ft, "width_squeeze") <- c(
    header = isTRUE(attr(widths, "header_squeeze")),
    cell = isTRUE(attr(widths, "cell_squeeze"))
  )
  # How much of the longest header word the tightest column keeps (1 when
  # nothing breaks). A header wrapping mid-word is a blemish the writer lives
  # with; a header down to a couple of letters per line is what makes it deal
  # the columns over several slides instead.
  attr(ft, "word_fit") <- attr(widths, "word_fit") %||% 1
  # Everything the width pass decided, so a caller rendering the SAME table in
  # pieces (one page per slide) reproduces the layout instead of measuring
  # each piece on its own and drifting.
  attr(ft, "layout_plan") <- list(
    col_widths = as.numeric(widths),
    cell_padding = cell_padding
  )
  # Which header row carries the leaf labels, counting the title and subtitle
  # lines above it.
  attr(ft, "leaf_row") <- leaf_i
  ft
}

# Left plus right cell padding as a width in inches. `NULL` means the
# flextable default, which is what static_table() leaves in place.
ft_pad_width <- function(cell_padding) {
  if (is.null(cell_padding)) ft_side_padding() / 72 else 2 * cell_padding / 72
}

# Cell padding, in points, for a table that cannot fit any other way. Two
# points instead of the flextable default of five: on a 36-column table the
# default spends 5in of a 12.5in slide on padding alone, which is the
# difference between a legible table and stacked characters.
TIGHT_PAD <- 2

# ---- measured column widths ------------------------------------------------
# Give every column the width its own text needs, in this priority: a data
# cell never wraps, a header never breaks inside a word, the headers unwrap
# toward their full text, and only then does the stub grow past its share.
# The stub is the only column whose content is prose, so it is the one that
# survives wrapping; a count like "143 (41.2%)" broken over two lines costs a
# readable row and doubles its height.
#
# The stub does not hold its whole label on one line while the data columns
# are pinched: `stub_share` caps what it claims until everything else is
# served, and it takes the leftovers afterwards. A long system organ class
# reading over two lines costs a row; the same label spread over five inches
# is what pushes a table onto a second set of slides.
#
# Everything is measured through systemfonts at the table's own font and size,
# which is the same engine flextable itself measures with. When the deck's
# typeface is not installed on the machine doing the export (a house template
# naming Trebuchet MS on a Linux server, say) systemfonts substitutes and the
# numbers are approximate, which is what `blockr.viz.ft_width_slack` covers.
#
# Returns widths in inches summing to `total`, or NULL when the table cannot
# be measured -- the caller then falls back to the positional split, so a
# missing measurement never fails an export.
#
# Three attributes come back with them, all saying how close the slide came to
# being too narrow and what would help:
#   `header_squeeze` -- the headers will break inside a word. Nothing here can
#     fix that; fewer columns or a smaller font can.
#   `cell_squeeze`   -- the DATA cells no longer fit. Below that the columns
#     are narrower than a character and PowerPoint stacks them one per line,
#     which is the 36-column grade table. Only less padding or a smaller font
#     helps, because a digit and its padding is all that is left.
#   `word_fit`       -- how much of the longest header word the narrowest
#     column can hold, as a fraction. 1 is a header that never breaks, 0.5 a
#     word taking two lines, 0.1 the stacked-letters case. It is the difference
#     between a header squeeze worth living with and one that is not, and the
#     pptx writer reads it to decide whether to deal the columns over slides.
ft_measured_widths <- function(stub, stub_indent, stub_label, cells, leaf, top,
                               font, font_size, total, banner = character(),
                               pad = ft_side_padding() / 72,
                               slack = getOption("blockr.viz.ft_width_slack",
                                                 1.04),
                               stub_min = 1.2,
                               stub_share = getOption("blockr.viz.ft_stub_share",
                                                      0.3)) {

  n_data <- ncol(cells)

  if (!requireNamespace("systemfonts", quietly = TRUE) ||
        !is.numeric(total) || length(total) != 1L || !is.finite(total) ||
        total <= 0 || n_data < 1L) {
    return(NULL)
  }

  squeeze <- c(header = FALSE, cell = FALSE)
  word_fit <- 1

  out <- tryCatch(
    {
      # The stub asks for its longest label at its own indent depth. Section
      # header rows are merged across the table, so they are not in `stub`
      # and never widen the column.
      stub_cells <- ft_text_widths(stub, font, font_size) + stub_indent / 72
      # ... but never so narrow that a single word has to break, and never
      # hairline thin because the table happens to have no rows yet.
      stub_floor <- max(
        min(stub_min, total / 3),
        ft_word_width(c(stub, stub_label), font, font_size) + pad
      )
      stub_nat <- max(c(stub_cells, 0)) + pad
      stub_want <- max(stub_nat, stub_floor)
      # What the stub keeps when the table has to be squeezed: its floor, or
      # its own width when that is smaller. A stub of short codes must not
      # hold half an inch back from columns that are being cut off.
      stub_keep <- min(stub_nat, stub_floor)

      # A data column asks for two widths. `want` is the one it must have:
      # its widest cell, and its header's longest word, so the numbers never
      # wrap and the header never breaks mid-word. `lux` is the one it would
      # like: the whole header on as few lines as it was written with. A
      # header may wrap between words when the slide is tight; it should not
      # have to when the slide has room to spare.
      cell_w <- vapply(
        seq_len(n_data),
        function(j) max(c(ft_text_widths(cells[, j], font, font_size), 0)),
        numeric(1L)
      ) + pad
      head_word <- vapply(leaf, ft_word_width, numeric(1L),
                          font = font, size = font_size) + pad
      head_full <- ft_text_widths(leaf, font, font_size) + pad

      # A spanner sits over its whole group, so its demand is shared out: it
      # only widens columns when the group is narrower than the spanner.
      span_word <- rep(0, n_data)
      span_full <- rep(0, n_data)
      if (any(nzchar(top))) {
        runs <- rle(top)
        at <- 0L
        for (i in seq_along(runs$values)) {
          k <- runs$lengths[[i]]
          if (nzchar(runs$values[[i]])) {
            span_word[at + seq_len(k)] <-
              (ft_word_width(runs$values[[i]], font, font_size) + pad) / k
            span_full[at + seq_len(k)] <-
              (ft_text_widths(runs$values[[i]], font, font_size) + pad) / k
          }
          at <- at + k
        }
      }

      need <- cell_w * slack
      want <- pmax(need, head_word, span_word)
      lux <- pmax(want, head_full, span_full)

      # Columns that carry the same statistic get the same width, whatever
      # their own arm happens to need: "n (%)" under Placebo and under a
      # 200-subject arm are read across, and a table whose arms are visibly
      # different widths reads as a mistake. Leaf labels name the statistic;
      # when they are all distinct they ARE the groups (one column per arm),
      # so the whole data side is one class.
      role <- if (anyDuplicated(leaf)) leaf else rep("", n_data)
      need <- ft_group_max(need, role)
      want <- ft_group_max(want, role)
      lux <- ft_group_max(lux, role)

      if (sum(want) + stub_floor > total) {
        # Too wide even at the minimum: shrink the widest columns first and
        # accept that something wraps. Which thing wraps is worth telling the
        # caller apart: a header taking a second line is a cost, a data cell
        # narrower than its own characters is a defect.
        squeeze[["header"]] <- TRUE
        squeeze[["cell"]] <- sum(need) + stub_keep > total
        data_w <- ft_water_fill(want, max(total - stub_keep, 0), need)
        stub_w <- total - sum(data_w)
      } else {
        # Whether this table is close enough to the slide to fill it, decided
        # on the widths it needs rather than the ones it ends up with.
        fills <- stub_want + sum(want) >= 0.7 * total

        # What the stub claims before the data columns have had their turn.
        # Past this it is spending slide on a label that wraps perfectly well,
        # and the columns that pay for it are the ones a reader compares.
        stub_cap <- max(stub_floor, stub_share * total)
        stub_w <- min(stub_want, stub_cap, total - sum(want))
        data_w <- want
        room <- total - stub_w - sum(data_w)

        # First call on the room: unwrap the headers, in proportion to how
        # far each is from its own text, so no column takes the whole surplus.
        short <- pmax(lux - data_w, 0)
        if (room > 0 && sum(short) > 0) {
          data_w <- data_w + short * min(1, room / sum(short))
          room <- total - stub_w - sum(data_w)
        }

        # Second call: the stub back up to its own text, now that nothing else
        # is waiting for the room. A four-column table on a wide slide still
        # ends with its label on one line; a twelve-column one does not.
        if (room > 0 && stub_w < stub_want) {
          stub_w <- stub_w + min(room, stub_want - stub_w)
          room <- total - stub_w - sum(data_w)
        }

        if (fills) {
          # What is left over is spread evenly. The stub is already at its
          # natural width and more of it would only reopen the gap this
          # replaces.
          if (room > 0) data_w <- data_w + room / n_data
        } else {
          # A small table on a wide slide. Stretching four columns across
          # thirteen inches to honour `fit_width` gives cavernous cells with
          # a number lost in the middle of each; the table keeps its natural
          # size and the pptx writer centres it instead. It does grow far
          # enough to hold its own title line, which is merged across the
          # width and would otherwise wrap over a narrow table.
          banner_w <- max(c(
            ft_text_widths(banner, font, font_size + 2) + pad, 0
          ))
          grow <- min(banner_w, total) - (stub_w + sum(data_w))
          if (is.finite(grow) && grow > 0) {
            data_w <- data_w + grow / n_data
          }
        }
      }

      # How much of its longest word the tightest header keeps. A word that
      # loses a syllable to the next line is a blemish; one holding two
      # characters of six is the stacked-letters failure, and only the reader
      # of this number can tell them apart.
      word_need <- pmax(head_word, span_word)
      if (any(word_need > 0)) {
        word_fit <- min(1, min(data_w[word_need > 0] / word_need[word_need > 0]))
      }

      c(stub_w, data_w)
    },
    error = function(e) NULL
  )

  if (!is.numeric(out) || length(out) != n_data + 1L || any(!is.finite(out)) ||
        any(out <= 0)) {
    return(NULL)
  }

  attr(out, "header_squeeze") <- unname(squeeze[["header"]])
  attr(out, "cell_squeeze") <- unname(squeeze[["cell"]])
  attr(out, "word_fit") <- word_fit
  out
}

# ---- estimated rendered heights --------------------------------------------
# How tall each row of a part will come out, in inches, once PowerPoint has
# wrapped the text into the column widths the table carries. flextable states
# a row height, but PowerPoint treats it as a minimum and grows the row to fit
# its content, so the stated height is a floor and not an answer.
#
# Needed to cut a long table into slides: the page break has to be decided
# before the slide exists, and a wrapped stub label costs a line that a row
# count knows nothing about.
#
# Measured, not exact. The wrap is greedy on whitespace (which is what a
# renderer does) but the font may not be the one PowerPoint will use, and
# borders add a fraction of a point per row.
ft_part_heights <- function(ft, part = "body") {

  p <- ft[[part]]

  if (is.null(p) || !nrow(p$dataset) ||
        !requireNamespace("systemfonts", quietly = TRUE)) {
    return(numeric(0))
  }

  w <- ft$body$colwidths
  spans <- p$spans$rows
  n_row <- nrow(p$dataset)
  n_col <- length(w)

  sz <- p$styles$text$font.size$data
  bold <- p$styles$text$bold$data
  fam <- p$styles$text$font.family$data
  pl <- p$styles$pars$padding.left$data
  pr <- p$styles$pars$padding.right$data
  pt <- p$styles$pars$padding.top$data
  pb <- p$styles$pars$padding.bottom$data

  out <- rep(0, n_row)

  for (j in seq_len(n_col)) {
    txt <- ft_cell_text(p, j)
    span <- spans[, j]
    # A merged cell is measured across the columns it covers, and the cells
    # it swallowed are not measured at all.
    avail <- vapply(seq_len(n_row), function(i) {
      if (span[[i]] < 1) return(NA_real_)
      sum(w[j:min(n_col, j + span[[i]] - 1L)]) -
        (pl[i, j] + pr[i, j]) / 72
    }, numeric(1L))

    for (i in seq_len(n_row)) {
      if (is.na(avail[[i]])) next
      size <- sz[i, j]
      lines <- ft_line_count(txt[[i]], avail[[i]], fam[i, j], size,
                             isTRUE(bold[i, j]))
      h <- lines * size * 1.2 / 72 + (pt[i, j] + pb[i, j]) / 72
      if (h > out[[i]]) out[[i]] <- h
    }
  }

  # The stated row height is a floor, not a cap.
  stated <- p$rowheights
  if (is.numeric(stated) && length(stated) == n_row) {
    out <- pmax(out, stated)
  }

  out
}

# The text one column of a flextable part actually PRINTS, row by row.
#
# Not `part$dataset`, which for a header still holds the column keys the table
# was built from ("Placebo (N=143)||n (%)"), never the labels
# set_header_labels() put there. Measuring those would size the header band
# against strings no reader ever sees. The rendered runs live in the chunk
# structure; the dataset is the fallback for a flextable that has none.
ft_cell_text <- function(p, j) {

  n_row <- nrow(p$dataset)
  chunks <- p$content$data

  if (is.null(chunks) || ncol(chunks) < j) {
    out <- as.character(p$dataset[[j]])
    out[is.na(out)] <- ""
    return(out)
  }

  vapply(seq_len(n_row), function(i) {
    cell <- chunks[[i, j]]
    if (is.null(cell) || is.null(cell$txt)) {
      return("")
    }
    txt <- as.character(cell$txt)
    txt[is.na(txt)] <- ""
    paste(txt, collapse = "")
  }, character(1L))
}

# Lines a string takes at a given width, wrapping greedily on whitespace and
# breaking hard at "\n". A word wider than the column still gets its own line
# rather than being counted twice.
ft_line_count <- function(x, width, font, size, bold = FALSE) {

  x <- as.character(x)

  if (!length(x) || is.na(x) || !nzchar(x) || !is.finite(width) ||
        width <= 0) {
    return(1L)
  }

  weight <- if (bold) "bold" else "normal"
  wid <- function(s) {
    systemfonts::string_width(s, family = font, size = size, weight = weight,
                              res = 72) / 72
  }

  if (wid(x) <= width && !grepl("\n", x, fixed = TRUE)) {
    return(1L)
  }

  lines <- 0L
  for (para in strsplit(x, "\n", fixed = TRUE)[[1L]]) {
    words <- strsplit(para, "[[:space:]]+")[[1L]]
    words <- words[nzchar(words)]
    if (!length(words)) {
      lines <- lines + 1L
      next
    }
    ww <- wid(words)
    # Measured rather than assumed: a space is not the same width in every
    # face, and at eight columns the error adds up.
    space <- max(0, wid("x x") - wid("xx"))
    n <- 1L
    cur <- ww[[1L]]
    for (k in seq_along(words)[-1L]) {
      if (cur + space + ww[[k]] <= width) {
        cur <- cur + space + ww[[k]]
      } else {
        n <- n + 1L
        cur <- ww[[k]]
      }
    }
    lines <- lines + n
  }

  max(1L, lines)
}

# Lift every element to the largest value in its group, keeping the order.
ft_group_max <- function(x, by) {
  as.numeric(stats::ave(x, by, FUN = max))
}

# Shrink the widest columns first (max-min fair), never below `mins`, and
# rescale to the budget. When even the minimums do not fit -- forty columns on
# a widescreen slide -- the result is proportional to what each column needed,
# which is the best a fixed width can do.
ft_water_fill <- function(want, budget, mins) {
  cap <- function(x) sum(pmax(mins, pmin(want, x)))
  lo <- 0
  hi <- max(want)
  for (i in seq_len(60L)) {
    mid <- (lo + hi) / 2
    if (cap(mid) > budget) hi <- mid else lo <- mid
  }
  w <- pmax(mins, pmin(want, lo))
  w * budget / sum(w)
}

# Width in inches of each string's longest line. A cell's own "\n" is a hard
# break (the Big-N convention), so a two-line header asks only for its wider
# line. Bold throughout: the emphasis rows are the widest ones, and a table
# sized for regular weight wraps as soon as a row is bolded.
ft_text_widths <- function(x, font, size) {

  x <- as.character(x)
  x[is.na(x)] <- ""

  if (!length(x)) {
    return(numeric(0))
  }

  w <- systemfonts::string_width(x, family = font, size = size,
                                 weight = "bold", res = 72) / 72
  for (i in which(grepl("\n", x, fixed = TRUE))) {
    parts <- strsplit(x[[i]], "\n", fixed = TRUE)[[1L]]
    w[[i]] <- max(systemfonts::string_width(parts, family = font, size = size,
                                            weight = "bold", res = 72)) / 72
  }

  w
}

# Width of the widest single word: the point below which a cell stops wrapping
# and starts breaking words.
ft_word_width <- function(x, font, size) {

  words <- unlist(strsplit(as.character(x), "[[:space:]]+"))
  words <- words[!is.na(words) & nzchar(words)]

  if (!length(words)) {
    return(0)
  }

  max(systemfonts::string_width(words, family = font, size = size,
                                weight = "bold", res = 72)) / 72
}

# Left plus right cell padding in points. static_table() sets the vertical
# padding and leaves these at the flextable defaults, so this is what a cell
# spends before its first character.
ft_side_padding <- function() {
  d <- flextable::get_flextable_defaults()
  sum(vapply(c("padding.left", "padding.right"),
             function(k) if (is.numeric(d[[k]])) d[[k]] else 5,
             numeric(1L)))
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

# A theme's `bands` role, read as the emphasis triple. Themes state bands the
# way header fills want them (a `.stub` light grey then a cycle pool), which is
# the same vocabulary: the stub is the `normal` band and the first pool entry
# is the accent. Returns NULL unless the theme supplies both, so a partial
# answer falls through to FT_EMPHASIS_DEFAULT rather than half-applying.
ft_emphasis_from_theme <- function() {
  bands <- viz_palette("bands")
  if (is.null(bands)) {
    return(NULL)
  }
  nm <- names(bands) %||% rep("", length(bands))
  stub <- bands[nm == ".stub"]
  pool <- unname(bands[nm != ".stub"])
  if (!length(stub) || !length(pool)) {
    return(NULL)
  }
  c(normal = unname(stub[[1L]]),
    emph = pool[[min(2L, length(pool))]],
    strong = pool[[1L]])
}

# Per-column bg / text for emphasis mode. strong -> accent, emph -> dark gray,
# normal -> light gray; the stub takes the normal (light) band so the header
# reads as one strip. Contrast text by luminance.
ft_emphasis_bands <- function(col_strong, col_emph) {
  cols <- getOption("blockr.viz.ft_emphasis_colors", NULL) %||%
    ft_emphasis_from_theme() %||% FT_EMPHASIS_DEFAULT
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
  gcol <- stats::setNames(rep(NA_character_, length(groups)), groups)
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
