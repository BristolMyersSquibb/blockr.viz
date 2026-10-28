# Helpers of the annotated-df structure, shared by the table block's
# structured path (R/table-structured*.R) and the PowerPoint and Excel
# exporters: header text, stub header rows, section columns, drill values.

#' Plain-text header line for width estimation: honour attr(col, "label"),
#' strip HTML, take the widest line of a multi-line label.
#' @noRd
dt_header_text <- function(data, col) {
  lbl <- attr(data[[col]], "label")
  txt <- if (is.character(lbl) && length(lbl) == 1L && nzchar(lbl)) {
    lbl
  } else {
    p <- strsplit(col, "||", fixed = TRUE)[[1L]]
    p[[length(p)]]
  }
  txt <- gsub("<br */?>", "\n", txt)
  txt <- gsub("<[^>]*>", "", txt)
  lines <- trimws(strsplit(txt, "\n", fixed = TRUE)[[1L]])
  if (!length(lines)) return("")
  lines[[which.max(nchar(lines, type = "width"))]]
}

# ---------------------------------------------------------------------------
# Multi-level <thead> builder
# ---------------------------------------------------------------------------

#' Normalize per-header-row stub flags.
#' @noRd
stub_header_flags <- function(flags, n) {
  if (!is.logical(flags) || !length(flags)) {
    return(rep(FALSE, n))
  }

  flags <- rep_len(flags, n)
  flags[is.na(flags)] <- FALSE
  flags
}

#' The row-stub header text and bold flag for each of `n` header rows.
#'
#' The xlsx and pptx writers draw at most two header rows (spanner + leaf),
#' fewer than the HTML table when the column names nest deeper. When the
#' producer gave more labels than there are rows, the bottom ones are kept,
#' so the label naming the innermost rows stays next to the body. A single
#' label sits on the bottom row, where the writers always put it.
#'
#' For exporters in other blockr packages; not part of the user API.
#' @keywords internal
#' @export
stub_header_rows <- function(col, n) {
  lbl <- attr(col, "label", exact = TRUE)
  if (!is.character(lbl) || !length(lbl)) {
    return(list(label = rep("", n), bold = rep(FALSE, n)))
  }
  lbl[is.na(lbl)] <- ""
  bold <- stub_header_flags(attr(col, "label_bold", exact = TRUE), length(lbl))

  idx <- if (length(lbl) == 1L) {
    c(rep(NA_integer_, n - 1L), 1L)
  } else if (length(lbl) >= n) {
    utils::tail(seq_along(lbl), n)
  } else {
    c(seq_along(lbl), rep(NA_integer_, n - length(lbl)))
  }

  list(
    label = ifelse(is.na(idx), "", lbl[idx]),
    bold = !is.na(idx) & bold[idx] %in% TRUE
  )
}

#' Does this label text carry HTML, or is it text that merely contains a `<`?
#'
#' A `<` only opens a tag when a letter or a slash follows it, so an age band
#' named `<65` is text and `<strong>` is markup. The distinction matters
#' because markup is passed through unescaped: without it, `<65` reached the
#' browser raw (it happens to render, but only because `<6` is not a valid
#' tag).
#' @noRd
looks_like_markup <- function(x) {
  grepl("<[[:alpha:]/]", x)
}

#' Split a header label into its name lines and the quiet "N = k" line.
#'
#' Every producer of these labels appends the Big N at the END: composer
#' through the annotated-df bridge (`paste0(leaf, "\nN = ", n)`) and
#' `fmt_cells()` on the summarise path (`sprintf("%s\nN = %d", ...)`). What
#' sits in front of it is the level value as the data spells it, so it can
#' carry newlines of its own -- a long arm description the sponsor broke
#' across lines. Splitting on the FIRST newline would demote the tail of the
#' arm name into the quiet sub-line.
#'
#' So: take the N off the END, and leave everything before it as the name.
#' A label with no "N = " line has nothing to demote -- `vz-table-arm-n`
#' holds the count, not "whatever came after the first line" -- and renders
#' as one name wrapped across its own breaks.
#' @noRd
header_label_split <- function(lbl) {
  parts <- strsplit(lbl, "\n", fixed = TRUE)[[1L]]
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  if (!length(parts)) {
    return(NULL)
  }
  last <- parts[[length(parts)]]
  if (length(parts) > 1L && grepl("^N\\s*=", last)) {
    return(list(name = parts[-length(parts)], n = last))
  }
  list(name = parts, n = NULL)
}

# ---------------------------------------------------------------------------
# Section-aware <tbody> builder
# ---------------------------------------------------------------------------

#' Keep only section/var columns that actually carry a grouping value.
#'
#' A grouping-value column that is entirely NA/blank is not a real
#' grouping dimension -- rendering it would wrap every row under a single
#' "(missing)" header. Drop those so the table renders flat (or indent-only)
#' instead. A *partially* empty column is kept: its gaps still render as
#' "(missing)", which is the right label for a genuine orphan bucket. Used by
#' the table block's structured path.
#' @noRd
nonempty_section_cols <- function(data, section_cols) {
  section_cols[vapply(section_cols, function(sc) {
    v <- trimws(as.character(data[[sc]]))
    any(!is.na(v) & nzchar(v))
  }, logical(1L))]
}

#' Resolve an annotated df's row-side structure for rendering.
#'
#' The v2 contract keeps structure columnar: `.group<k>_level` columns are the
#' grouping-value axes (outermost first), and runs of `.variable_label` form
#' the innermost variable blocks of a multi-variable summary. This helper
#' turns both into the single `section_cols` vector the structured table
#' synthesizes headers from: the group `_level` columns, plus -- when any row
#' carries a `.variable_label` -- a synthetic `.variable_block` column holding
#' that label (no `label` attribute, so no "name: " prefix). All-empty axes
#' are dropped via `nonempty_section_cols()`.
#'
#' Returns `list(data, section_cols, reserved_cols)`; `reserved_cols` is every
#' annotation column (including the synthetic one) that must stay out of the
#' data cells. Shared by the table block's structured path, the
#' PowerPoint table and the Excel writer.
#'
#' For exporters in other blockr packages; not part of the user API.
#' @keywords internal
#' @export
annotated_structure_view <- function(data) {
  section_cols <- annotation_group_level_cols(data)

  vlab <- data[[".variable_label"]]
  if (!is.null(vlab) && any(!is.na(vlab) & nzchar(as.character(vlab)))) {
    data[[".variable_block"]] <- as.character(vlab)
    section_cols <- c(section_cols, ".variable_block")
  }

  reserved_cols <- unique(c(annotation_cols_in(data),
                            intersect(".variable_block", names(data))))

  list(
    data = data,
    section_cols = nonempty_section_cols(data, section_cols),
    reserved_cols = reserved_cols
  )
}

#' The values a nested records row claims for the drill column: the column
#' itself, or `.label` when the drill column is the rowname table_nest() moved
#' there. NULL when `drill_col` is a mode ("auto", "source") or names nothing
#' in the frame.
#' @noRd
dd_nest_drill_values <- function(data, drill_col) {
  if (is.null(drill_col) || !nzchar(drill_col) || startsWith(drill_col, ".")) {
    return(NULL)
  }
  if (drill_col %in% names(data)) return(as.character(data[[drill_col]]))
  if (identical(attr(data, "nest_rowname", exact = TRUE), drill_col) &&
        ".label" %in% names(data)) {
    return(as.character(data[[".label"]]))
  }
  NULL
}

# ---------------------------------------------------------------------------
# Inline CSS
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Inline JS -- idempotent, scoped to a specific wrapper id
# ---------------------------------------------------------------------------
