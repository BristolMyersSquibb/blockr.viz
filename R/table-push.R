# ---------------------------------------------------------------------------
# Flat-table cell model + data-push payload (dev/table-data-push-design.md).
#
# The flat <table> body used to exist only as pasted HTML inside
# dt_table_tag(). The block server now ships the SAME content to the browser
# as a column-oriented cell model over a custom message
# ("blockr-viz-table-data"), and table.js assembles the rows client-side,
# windowing large tables so only the viewport's rows enter the DOM. To keep
# the two representations byte-identical, both consume one builder:
# dt_flat_build() computes every per-column display/style/raw vector, and
#   - dt_flat_assemble_tag() pastes them into the historical HTML (the
#     exported drilldown_table() / static path, and the test surface), while
#   - dt_flat_payload() emits them as the JSON cell model (types.d.ts
#     VizTablePayload).
# ---------------------------------------------------------------------------

#' Compute the flat table's cell model: per-column display strings, shading
#' style chunks and drill raw values, plus the (small) thead/colgroup tags and
#' the data-attribute args. Returns `kind = "message"` (with the rendered
#' message-table tag) for the non-renderable states, else `kind = "flat"`.
#'
#' Vectors are FULL-LENGTH aligned to rows: `disp` is NA where the cell is NA
#' (both consumers render the em-dash cell and skip raw/style there), `raw`
#' is NULL for non-drill columns, `style` is NULL for unshaded columns and
#' "" on NA rows. Display strings are PLAIN (unescaped); each consumer
#' escapes with the htmltools rules at assembly time.
#' @noRd
dt_flat_build <- function(data, label_col = NULL, value_cols = NULL,
                          shadings = list(), drill = NULL, digits = 2L,
                          row_hex = NULL, color = NULL, toggles = list(),
                          group_cols = NULL, group = character(),
                          summaries = list(), active = NULL,
                          gear_cols = NULL) {
  # Pickable columns for the gear (data-dt-cols). `data` here is the raw
  # input except in the block server's aggregated branch, which displays a
  # projection and passes the raw schema in explicitly.
  if (is.null(gear_cols)) gear_cols <- dt_gear_cols_json(data)

  value_cols_raw <- value_cols
  if (is.null(label_col)) label_col <- names(data)[1L]
  if (is.null(value_cols)) value_cols <- setdiff(names(data), label_col)
  value_cols <- intersect(value_cols, names(data))

  # Differentiated non-renderable states (chart-empty-state parity): a
  # configured column that vanished upstream, a required mapping still
  # unconfigured, and a genuinely 0-row frame are three different problems
  # with three different fixes -- one generic "No data" hid all of them.
  msg <- dt_state_message(data, label_col, value_cols, value_cols_raw)
  if (!is.null(msg)) {
    # The gear must still offer the (current) input columns on a message
    # table -- fixing a vanished-column config happens through it.
    return(list(kind = "message",
                tag = dt_table_attrs(dt_message_table(msg), NULL, NULL,
                                     digits, color = color, toggles = toggles,
                                     gear_cols = gear_cols)))
  }

  # ---- cell visuals: value-encoding `shadings` rules ------------------
  # Repeatable rules `list(list(mode, cols))` resolved to per-column visuals
  # (see dd_shading_visuals): explicit cols claim; empty cols = all numeric
  # minus claimed (override rule, re-resolved per render so it survives
  # upstream schema changes); diverging/sequential pool one domain per rule,
  # bars normalize per column.
  shading_vis <- dd_shading_visuals(shadings, data, value_cols)

  # ---- thead ----------------------------------------------------------
  # Per-column numeric flag drives type-based alignment for both the header
  # and the body cells (numeric right, text left).
  sortable <- isTRUE(toggles$sortable %||% TRUE)
  num_flag <- vapply(data[value_cols], is.numeric, logical(1L))
  th_cells <- list(dt_th(label_col, 0L, stub = TRUE,
                         label = dt_col_label(data[[label_col]], label_col),
                         sortable = sortable))
  for (i in seq_along(value_cols)) {
    th_cells[[length(th_cells) + 1L]] <- dt_th(
      value_cols[i], i,
      label = dt_col_label(data[[value_cols[i]]], value_cols[i]),
      numeric = num_flag[i],
      sortable = sortable
    )
  }
  thead <- htmltools::tags$thead(htmltools::tags$tr(th_cells))

  # Drill-relevant columns carry each cell's RAW value (rendered as a
  # data-raw attribute, read by the click handlers instead of the displayed
  # text): numeric cells display rounded and NA renders as an em-dash, so a
  # filter built from the display would match zero rows and silently empty
  # downstream. `as.character(raw)` round-trips exactly -- comparing a
  # numeric column to a character value in the filter expr coerces through
  # the same as.character(). NA cells stay NA here -> no data-raw (the click
  # is a no-op; see the nodrill flag below).
  raw_cols <- intersect(unique(c(drill %||% character(),
                                 group_cols %||% character())),
                        c(label_col, value_cols))

  n <- nrow(data)
  cells <- vector("list", length(value_cols))
  # Display strings per column (non-NA only), kept for the server-side width
  # estimation below (same strings the cells render, no second formatting
  # pass).
  disp_by_col <- rep(list(character(0L)), length(value_cols))
  for (j in seq_along(value_cols)) {
    col  <- data[[value_cols[j]]]
    keep <- !is.na(col)
    # Type-based cell alignment matches the header (numeric right, text left).
    td_cls <- if (num_flag[j]) "blockr-data dt-num" else "blockr-data dt-txt"
    disp_full  <- rep(NA_character_, n)
    style_full <- NULL
    if (any(keep)) {
      vk <- col[keep]
      disp <- if (num_flag[j]) {
        formatC(round(as.numeric(vk), digits), format = "f", digits = digits,
                drop0trailing = TRUE, big.mark = "")
      } else {
        as.character(vk)
      }
      disp_by_col[[j]] <- disp
      disp_full[keep] <- disp
      sv <- shading_vis[[value_cols[j]]]
      if (num_flag[j] && !is.null(sv)) {
        style_full <- rep("", n)
        if (identical(sv$kind, "bar")) {
          # Data bar: left-anchored gradient, width = |v| / column-abs-max.
          # A CSS style-chunk string keeps both consumers on plain string
          # concatenation (no per-cell DOM node / object).
          style_full[keep] <- dt_bar_style(as.numeric(vk), sv$max, sv$fill)
        } else {
          # Heatmap: sv$fun is vectorized (see dt_color_fun) -- one call
          # styles the whole column, like dt_bar_style above.
          bg <- sv$fun(as.numeric(vk))
          style_full[keep] <- paste0(" style=\"background:", bg$bg,
                                     ";color:", bg$fg, ";\"")
        }
      }
    }
    raw_full <- if (value_cols[j] %in% raw_cols) {
      r <- rep(NA_character_, n)
      r[keep] <- as.character(col[keep])
      r
    }
    cells[[j]] <- list(cls = td_cls, disp = disp_full,
                       raw = raw_full, style = style_full)
  }

  # Categorical scale-map row color (e.g. SEX: F = teal, M = orange) drawn as
  # a subtle accent bar on the left of the row, matching the chart's legend
  # and reading like a selected-row indicator. `row_hex` is a per-row vector
  # (the `color` column resolved through the scale map); NA rows get no bar.
  # Drawn with an inset box-shadow so it adds no width (no layout shift) and
  # is independent of the numeric heatmap above.
  has_bar <- !is.null(row_hex) && length(row_hex) == n
  stub_disp <- as.character(data[[label_col]])
  # An NA stub has always rendered as the literal "NA" (the historical
  # paste0() semantics); make that explicit so both consumers agree.
  stub_disp[is.na(stub_disp)] <- "NA"
  stub <- list(
    cls  = if (has_bar) "blockr-stub blockr-row-bar" else "blockr-stub",
    disp = stub_disp,
    raw  = if (label_col %in% raw_cols) {
      r <- as.character(data[[label_col]])
      r
    },
    style = if (has_bar) {
      ifelse(is.na(row_hex), "",
             paste0(" style=\"box-shadow:inset 3px 0 0 0 ", row_hex, ";\""))
    }
  )

  # A row whose drill value(s) include an NA cannot emit a filter (no
  # data-raw -> the click is a no-op); mark it so it doesn't LOOK clickable.
  nodrill <- if (length(raw_cols)) {
    Reduce(`|`, lapply(raw_cols, function(cn) is.na(data[[cn]])))
  } else {
    rep(FALSE, n)
  }

  flat_labels <- c(
    dt_col_label(data[[label_col]], label_col) %||% "",
    vapply(value_cols, function(vc) {
      dt_col_label(data[[vc]], vc) %||% ""
    }, character(1L))
  )
  colgroup <- dt_colgroup(
    c(label_col, value_cols),
    c(list(as.character(data[[label_col]])), disp_by_col),
    labels = flat_labels
  )

  onclick <- dt_onclick(drill, c(label_col, value_cols))
  list(
    kind = "flat", n = n,
    stub = stub, cells = cells, nodrill = nodrill,
    thead = thead, colgroup = colgroup,
    attr_args = list(
      onclick_col = onclick$col, onclick_idx = onclick$idx, digits = digits,
      color = color, shadings = shadings, num_cols = value_cols[num_flag],
      toggles = toggles, group_cols = group_cols, group = group,
      summaries = summaries, active = active, gear_cols = gear_cols
    )
  )
}

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

#' Emit the cell model as the "flat" JSON payload (types.d.ts
#' VizTablePayload): the `<table>` head (empty tbody, all data-dt-*
#' attributes -- the gear keeps reading its state off them) plus the
#' column-oriented cell vectors. `stamp` lets the block server append its
#' ctrl-send attributes to the head tag, exactly as it stamps the full tag
#' on the render path.
#' @noRd
dt_flat_payload <- function(b, stamp = identity) {
  head_tag <- stamp(do.call(dt_table_attrs, c(
    list(dt_fixed_table_tag(b$thead, htmltools::tags$tbody(), b$colgroup)),
    b$attr_args
  )))
  # I() keeps every per-cell vector a JSON array even at length 1
  # (auto_unbox would collapse a 1-row table's columns to scalars).
  one <- function(cc) {
    out <- list(cls = cc$cls, disp = I(unname(cc$disp)))
    if (!is.null(cc$raw))   out$raw   <- I(unname(as.character(cc$raw)))
    if (!is.null(cc$style)) out$style <- I(unname(cc$style))
    out
  }
  list(
    kind = "flat",
    head = as.character(head_tag),
    n = b$n,
    cols = c(list(one(b$stub)), lapply(b$cells, one)),
    nodrill = I(which(b$nodrill) - 1L)
  )
}

#' Build the body payload for the block server: the same dispatch as
#' dt_table_tag() (structured / message / flat), but returning the message
#' payload list instead of a rendered tag. Structured ("Table 1") tables,
#' message tables and (via the server's tryCatch) error states are small, so
#' they ship as `kind = "html"` -- the COMPLETE tag rendered by the existing
#' builders, injected and wired by the existing table.js path with zero
#' markup duplication. Only the flat path (the one that scales with rows)
#' ships the cell model.
#' @noRd
dt_build_payload <- function(data, label_col = NULL, value_cols = NULL,
                             shadings = list(), drill = NULL, digits = 2L,
                             row_hex = NULL, color = NULL,
                             sortable = TRUE, collapsible = TRUE,
                             search = TRUE, excel_download = FALSE,
                             group_cols = NULL, group = character(),
                             summaries = list(), active = NULL,
                             gear_cols = NULL, stamp = identity) {
  # The inter-block currency is the wide annotated df; summary_table()'s
  # internal long dialect errors here instead of being silently pivoted.
  reject_long_form(data)
  toggles <- list(sortable = sortable, collapsible = collapsible,
                  search = search, excel_download = excel_download)
  if (dt_is_structured(data)) {
    tag <- stamp(dt_table_tag_structured(data, drill, digits, toggles,
                                         active = active))
    return(list(kind = "html", html = as.character(tag)))
  }
  b <- dt_flat_build(data, label_col, value_cols, shadings, drill, digits,
                     row_hex, color, toggles, group_cols, group, summaries,
                     active, gear_cols)
  if (identical(b$kind, "message")) {
    return(list(kind = "html", html = as.character(stamp(b$tag))))
  }
  dt_flat_payload(b, stamp)
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
