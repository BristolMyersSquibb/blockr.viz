# ---------------------------------------------------------------------------
# Flat-table cell model + data-push payload (dev/table-data-push-design.md).
#
# The flat <table> body used to exist only as pasted HTML inside
# dt_table_tag(). The block server now ships the SAME content to the browser
# as a column-oriented cell model over a custom message
# ("blockr-viz-table-data"), and table.js assembles the rows client-side,
# windowing large tables so only the viewport's rows enter the DOM. To keep
# the two representations byte-identical, both consume one builder:
# dt_flat_build() (R/table-model-records.R) computes every per-column
# display/style/raw vector, and
#   - dt_flat_assemble_tag() pastes them into the historical HTML (the
#     exported drilldown_table() / static path, and the test surface), while
#   - dt_flat_payload() emits them as the JSON cell model (types.d.ts
#     VizTablePayload).
# ---------------------------------------------------------------------------

#' Paste the cell model into the historical flat `<table>` tag.
#'
#' Build the body as a single HTML string instead of one htmltools tag
#' object per cell. For a wide preview (e.g. ADaM ADSL, ~48 columns) the
#' per-cell `tags$td()` construction plus the `renderTags()` tree walk
#' dominated render time -- ~1 s for the full frame, the source of the
#' "drilldown filter takes ~2 s" lag. Column-vectorized string assembly
#' is ~100x faster and emits identical markup (inter-tag whitespace
#' aside). Text content uses htmltools' own escaper (the same one
#' `tags$td()` applies to a text child), so escaping is byte-identical:
#' & < > are escaped, quotes are not. table.js's row assembler applies the
#' same rules -- the two outputs must not drift.
#' @noRd
dt_flat_assemble_tag <- function(b) {
  esc <- function(x) htmltools::htmlEscape(as.character(x), attribute = FALSE)
  n <- b$n

  col_cells <- vector("list", length(b$cells))
  for (j in seq_along(b$cells)) {
    cc <- b$cells[[j]]
    na_cell <- paste0("<td class=\"", cc$cls, "\">&mdash;</td>")
    out_j   <- rep(na_cell, n)
    keep    <- !is.na(cc$disp)
    if (any(keep)) {
      raw <- if (is.null(cc$raw)) {
        ""
      } else {
        paste0(" data-raw=\"",
               htmltools::htmlEscape(cc$raw[keep], attribute = TRUE), "\"")
      }
      style <- if (is.null(cc$style)) "" else cc$style[keep]
      out_j[keep] <- paste0("<td class=\"", cc$cls, "\"", raw, style, ">",
                            esc(cc$disp[keep]), "</td>")
    }
    col_cells[[j]] <- out_j
  }

  stub_raw <- if (is.null(b$stub$raw)) {
    ""
  } else {
    ifelse(is.na(b$stub$raw), "", paste0(
      " data-raw=\"",
      htmltools::htmlEscape(as.character(b$stub$raw), attribute = TRUE), "\""
    ))
  }
  stub_style <- if (is.null(b$stub$style)) "" else b$stub$style
  stub_cells <- paste0("<td class=\"", b$stub$cls, "\"", stub_raw, stub_style,
                       ">", esc(b$stub$disp), "</td>")

  row_cls <- ifelse(b$nodrill, "blockr-data-row dt-row-nodrill",
                    "blockr-data-row")
  row_inner <- do.call(paste0, c(list(stub_cells), col_cells))
  rows_html <- paste0("<tr class=\"", row_cls, "\">", row_inner, "</tr>",
                      collapse = "")
  tbody <- htmltools::tags$tbody(htmltools::HTML(rows_html))

  table_tag <- dt_fixed_table_tag(b$thead, tbody, b$colgroup)
  do.call(dt_table_attrs, c(list(table_tag), b$attr_args))
}

#' Serialize a payload list ONCE, R-side. The block server sends the
#' resulting string (not the list): pre-serializing dodges Shiny's
#' auto_unbox scalar-collapse on the envelope, gives the server a plain
#' string-identity re-send guard (chart-block's last_msg pattern), and lets
#' the browser skip JSON.parse on an unchanged rev.
#' @noRd
dt_payload_json <- function(p) {
  as.character(jsonlite::toJSON(p, auto_unbox = TRUE, na = "null"))
}
