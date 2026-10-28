# Summarize table: the legend, number parts and table attributes the groups
# view, the exporters and the painter share. See R/summarize-table.R for the
# data half.

# The legend as DATA: a list of titled groups, always present for two or more
# series -- colour alone must not carry identity. One group is the ordinary
# case; the summarize table maps colour PER COLUMN, so it can carry several,
# and each is titled by the column it decodes.
#' @noRd
summarize_legend_spec <- function(prep) {
  if (!is.null(prep$err)) return(NULL)
  # Colour identity lives in the COLOUR mapping only: a plain facet's bars are
  # all the house blue and its column headers already name the levels, so it
  # carries no legend (chart parity -- a facet never recolours the marks).
  groups <- prep$color_groups
  if (is.null(groups)) {
    if (is.null(prep$color) || length(prep$series) < 2L) return(NULL)
    groups <- list(list(column = prep$color, title = prep$color_title,
                        levels = prep$series, palette = prep$palette))
  }
  if (!length(groups)) return(NULL)
  list(groups = lapply(groups, function(g) {
    # Positional, never by name: a colour column can carry a BLANK level (an
    # untreated subject's empty TRT01A), and `pal[[""]]` is a subscript error
    # even though the entry sits right there -- the empty string matches no
    # name in R. summarize_level_colors() returns the palette in `levels` order,
    # so the index IS the lookup. Blank reads as "(Missing)", as it does in
    # the table's group columns.
    pal <- g$palette
    list(
      # The column's variable label when it has one, as the hover card says.
      title = g$title %||% g$column,
      items = lapply(seq_along(g$levels), function(i) {
        l <- as.character(g$levels[[i]])
        list(
          label = if (nzchar(trimws(l))) l else "(Missing)",
          color = if (i <= length(pal)) unname(pal[[i]])
        )
      })
    )
  }))
}

#' @noRd
summarize_nz <- function(x) {
  !is.null(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

# Escape exactly as htmltools does for a text child -- & < > and the attribute
# quote -- because the JS row assembler applies the same rules and the two
# outputs must not drift.
#' @noRd
summarize_esc <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

# A data-frame-level display attribute, or NULL when absent / not a string.
#' @noRd
summarize_attr <- function(data, nm) {
  v <- attr(data, nm, exact = TRUE)
  if (is.character(v) && length(v) == 1L && nzchar(v)) v else NULL
}
#' The two parts of a numeric cell: the value string, and the percent string
#' when the column shows both. Data, never markup -- the consumers wrap it.
#' @noRd
summarize_num_parts <- function(v, denom = NULL, combined = FALSE,
                                signed = FALSE, pct_only = FALSE,
                                fmt = NULL) {
  if (isTRUE(signed)) {
    return(list(disp = ifelse(
      is.na(v), "",
      paste0(ifelse(v > 0, "+", ifelse(v < 0, "\u2212", "")),
             formatC(abs(v), format = "f", digits = 1L))
    )))
  }
  pct <- if (!is.null(denom) && is.finite(denom) && denom > 0) {
    v / denom * 100
  } else {
    NULL
  }
  if (isTRUE(pct_only)) {
    if (is.null(pct)) return(list(disp = rep("", length(v))))
    return(list(disp = ifelse(
      is.na(v), "", paste0(formatC(pct, format = "f", digits = 1L), "%")
    )))
  }
  # Four SIGNIFICANT digits, the same budget lane_fmt() prints a statistic at
  # (R/table-model-groups-stats.R), so a split bar's label and a box's centre read alike. "fg"
  # keeps whole numbers whole: a count of 12345 stays 12345, a mean of 155.625
  # reads 155.6 rather than spelling out a precision the estimate has not got.
  # `fmt`, when given, is the value label's own precision (one digit finer
  # than the column's axis, summarize_val_digits()).
  n <- ifelse(is.na(v), "", if (is.null(fmt)) {
    formatC(v, format = "fg", digits = 4L, big.mark = "")
  } else {
    fmt(v)
  })
  if (isTRUE(combined) && !is.null(pct)) {
    return(list(
      disp = n,
      pct = ifelse(is.na(v), "",
                   paste0("(", formatC(pct, format = "f", digits = 0L), "%)"))
    ))
  }
  list(disp = n)
}

#' @noRd
summarize_label_header <- function(prep) {
  if (isTRUE(prep$all)) return("")
  if (is.null(prep$parent)) prep$group else paste0(prep$parent, " / ", prep$group)
}

# Stamp the gear's state onto the rendered <table>. One JSON attribute rather
# than a dozen scalars: `titles` has to carry null-vs-"" (auto vs explicitly
# none), which an HTML attribute cannot say, and `cols` / `columns` are arrays.
#' @noRd
summarize_table_attrs <- function(prep, cfg) {
  if (is.null(cfg)) return("")
  # Facet levels travel too: the Compare picker offers levels, not columns.
  cfg$facet_levels <- as.list(prep$facet_levels %||% character())
  cfg$search <- if (isTRUE(cfg$search)) "on" else "off"
  cfg$sortable <- if (isTRUE(cfg$sortable %||% TRUE)) "on" else "off"
  cfg$axis <- if (isTRUE(cfg$axis %||% TRUE)) "on" else "off"
  cfg$value_labels <- if (isFALSE(cfg$value_labels)) "off" else "on"
  cfg$download <- if (isTRUE(cfg$download)) "on" else "off"
  cfg$bar_width <- summarize_bar_width(cfg$bar_width)
  compact <- isTRUE(cfg$compact)
  cfg$compact <- if (compact) "on" else "off"
  json <- as.character(jsonlite::toJSON(cfg, auto_unbox = TRUE, null = "null"))
  # A preset is a CSS hook on the table; "fit" is the stylesheet's default
  # and needs none.
  width <- if (!identical(cfg$bar_width, "fit")) {
    paste0(" data-summarize-width=\"", cfg$bar_width, "\"")
  }
  paste0(" data-summarize-cfg=\"", summarize_esc(json), "\"", width,
         if (compact) " data-summarize-compact=\"1\"")
}

#' The `bar_width` vocabulary: an unknown or empty value reads as "fit".
#' @noRd
summarize_bar_width <- function(x) {
  x <- summarize_chr1(x) %||% "fit"
  if (x %in% c("narrow", "medium", "wide", "fit")) x else "fit"
}
