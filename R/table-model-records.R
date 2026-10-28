# The records layout's cell model: what the table shows for each row and
# column (display strings, paint, raw drill values, the stub with its row
# tint) and the column widths. table_records_view() turns it into the
# TableModel.

#' Compute the flat table's cell model: per-column display strings, paint
#' and drill raw values, and the column widths. Returns `kind = "message"`
#' (with the rendered message-table tag) for the non-renderable states, else
#' `kind = "flat"`.
#'
#' Vectors are FULL-LENGTH aligned to rows: `disp` is NA where the cell is NA,
#' `raw` is NULL for non-drill columns, `bg` / `fg` / `bar` are NULL for
#' unshaded columns and NA on unpainted rows. Display strings are plain
#' (unescaped).
#' @noRd
dt_flat_build <- function(data, label_col = NULL, value_cols = NULL,
                          shadings = list(), drill = NULL, digits = "number",
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
    return(list(kind = "message", message = msg))
  }

  # ---- cell visuals: value-encoding `shadings` rules ------------------
  # Repeatable rules `list(list(mode, cols))` resolved to per-column visuals
  # (see dd_shading_visuals): explicit cols claim; empty cols = all numeric
  # minus claimed (override rule, re-resolved per render so it survives
  # upstream schema changes); diverging/sequential pool one domain per rule,
  # bars normalize per column. Columns a rule reads as its color SOURCE are
  # paint metadata -- dropped from the display here, painted below.
  shading_vis <- dd_shading_visuals(shadings, data, value_cols)
  value_cols <- setdiff(value_cols, attr(shading_vis, "hidden"))

  # Per-column numeric flag drives type-based alignment for both the header
  # and the body cells (numeric right, text left).
  num_flag <- vapply(data[value_cols], is.numeric, logical(1L))

  raw_cols <- dt_flat_raw_cols(drill, group_cols, c(label_col, value_cols))
  body <- dt_flat_cells(data, value_cols, num_flag, shading_vis, raw_cols,
                        digits)
  onclick <- dt_onclick(drill, c(label_col, value_cols))
  widths <- dt_flat_widths(data, label_col, value_cols, body$disp_by_col,
                           toggles)
  list(
    kind = "flat", n = nrow(data),
    stub = dt_flat_stub(data, label_col, raw_cols, row_hex),
    cells = body$cells, nodrill = dt_flat_nodrill(data, raw_cols),
    widths = widths, drill = drill, onclick_col = onclick$col
  )
}

# Drill-relevant columns carry each cell's RAW value (rendered as a
# data-raw attribute, read by the click handlers instead of the displayed
# text): numeric cells display rounded and NA renders as an em-dash, so a
# filter built from the display would match zero rows and silently empty
# downstream. `as.character(raw)` round-trips exactly -- comparing a
# numeric column to a character value in the filter expr coerces through
# the same as.character(). NA cells stay NA here -> no data-raw (the click
# is a no-op; see the nodrill flag in dt_flat_nodrill()).
dt_flat_raw_cols <- function(drill, group_cols, shown) {
  intersect(unique(c(drill %||% character(), group_cols %||% character())),
            shown)
}

# The value columns' cells, and their display strings per column (non-NA
# only) for the width estimate.
dt_flat_cells <- function(data, value_cols, num_flag, shading_vis, raw_cols,
                          digits) {
  cells <- vector("list", length(value_cols))
  disp_by_col <- rep(list(character(0L)), length(value_cols))
  for (j in seq_along(value_cols)) {
    one <- dt_flat_cell(data, value_cols[j], num_flag[j],
                        shading_vis[[value_cols[j]]],
                        value_cols[j] %in% raw_cols, digits)
    cells[[j]] <- one$cell
    disp_by_col[[j]] <- one$disp
  }
  list(cells = cells, disp_by_col = disp_by_col)
}

# One value column: its display strings, raw drill values and paint, full
# length over the rows.
dt_flat_cell <- function(data, name, num, sv, raw, digits) {
  n <- nrow(data)
  col  <- data[[name]]
  keep <- !is.na(col)
  disp_full  <- rep(NA_character_, n)
  disp_out <- character(0L)
  if (any(keep)) {
    vk <- col[keep]
    disp <- if (num) {
      dt_format_num(vk, digits)
    } else {
      as.character(vk)
    }
    disp_out <- disp
    disp_full[keep] <- disp
  }
  raw_full <- if (raw) {
    r <- rep(NA_character_, n)
    r[keep] <- as.character(col[keep])
    r
  }
  paint <- if (num && !is.null(sv) && any(keep)) {
    dt_flat_paint(sv, data, name, col, keep)
  }
  list(cell = c(list(disp = disp_full, raw = raw_full, name = name,
                     label = dt_col_label(col, name), num = num),
                paint),
       disp = disp_out)
}

# A shaded numeric column's paint as values, for a browser that draws the
# cells: `bg` and `fg` per row for a heatmap, `bar` (a share of the cell, 0
# to 1) and `bar_fill` for a data bar. NA where a cell is unpainted.
dt_flat_paint <- function(sv, data, name, col, keep) {
  n <- nrow(data)
  if (identical(sv$kind, "bar")) {
    w <- rep(NA_real_, n)
    if (is.finite(sv$max) && sv$max > 0) {
      w[keep] <- round(pmax(0, pmin(1, abs(as.numeric(col[keep])) / sv$max)),
                       4L)
    }
    return(list(bar = w, bar_fill = sv$fill))
  }
  p <- dt_bg_paint(sv, data, name)
  list(bg = p$bg, fg = p$fg)
}

# The stub column.
#
# Categorical scale-map row color (e.g. SEX: F = teal, M = orange) as the
# row's `tint`, which the view draws as an accent bar on the left of the
# row. `row_hex` is a per-row vector (the `color` column resolved through
# the scale map); NA rows get no bar.
dt_flat_stub <- function(data, label_col, raw_cols, row_hex) {
  n <- nrow(data)
  has_bar <- !is.null(row_hex) && length(row_hex) == n
  stub_disp <- as.character(data[[label_col]])
  # An NA stub has always rendered as the literal "NA" (the historical
  # paste0() semantics); make that explicit so both consumers agree.
  stub_disp[is.na(stub_disp)] <- "NA"
  list(
    disp = stub_disp, name = label_col,
    label = dt_col_label(data[[label_col]], label_col),
    tint = if (has_bar) row_hex,
    raw  = if (label_col %in% raw_cols) as.character(data[[label_col]])
  )
}

# A row whose drill value(s) include an NA cannot emit a filter (no
# data-raw -> the click is a no-op); mark it so it doesn't LOOK clickable.
dt_flat_nodrill <- function(data, raw_cols) {
  if (length(raw_cols)) {
    Reduce(`|`, lapply(raw_cols, function(cn) is.na(data[[cn]])))
  } else {
    rep(FALSE, nrow(data))
  }
}

# Column widths in px, stub first, from the header labels and the cells.
dt_flat_widths <- function(data, label_col, value_cols, disp_by_col, toggles) {
  flat_labels <- c(
    dt_col_label(data[[label_col]], label_col) %||% "",
    vapply(value_cols, function(vc) {
      dt_col_label(data[[vc]], vc) %||% ""
    }, character(1L))
  )
  if (isTRUE(toggles$rotate_titles)) {
    # Rotated titles (matrix tables): header text no longer drives the
    # column width -- that is the point of rotating. Value columns size on
    # their CELL content alone (a count needs ~30px, not the 60px+ the
    # header estimator floors at), the stub keeps the standard estimate.
    stub_w <- blockr.ui::column_widths_px(
      col_names = label_col,
      col_labels = flat_labels[1L],
      formatted = list(as.character(data[[label_col]]))
    )
    val_w <- vapply(disp_by_col, function(dd) {
      w <- if (length(dd)) max(nchar(dd, type = "width"), 0L) else 0L
      max(28L, w * 8L + 12L)
    }, numeric(1))
    return(round(c(stub_w, val_w)))
  }
  blockr.ui::column_widths_px(
    col_names = c(label_col, value_cols),
    col_labels = flat_labels,
    formatted = lapply(c(list(as.character(data[[label_col]])), disp_by_col),
                       as.character),
    wrap_names = isTRUE(toggles$wrap_titles %||% TRUE)
  )
}

#' Differentiated message for the flat table's non-renderable states, or NULL
#' when the table can render. Wording mirrors the chart's empty states
#' (chart/model-common.js): a configured column no longer in the data names the
#' column and hints at an upstream rename + "re-pick it in the gear"; a
#' required mapping with nothing configured is a pick prompt; a 0-row frame is
#' just "no rows".
#' `value_cols_raw` is the caller's pre-default `value_cols` (NULL = "all but
#' the rowname"), so an EXPLICIT pick whose columns all vanished reads as a
#' config error, never as empty data. A partially-missing explicit pick keeps
#' rendering the surviving columns (the documented silent-skip rule).
#' @noRd
dt_state_message <- function(data, label_col, value_cols, value_cols_raw) {
  missing <- character()
  if (!label_col %in% names(data)) {
    missing <- paste0("Rowname = \"", label_col, "\"")
  }
  raw <- as.character(value_cols_raw %||% character())
  if (length(raw) && !length(value_cols)) {
    missing <- c(missing,
                 paste0("Value = \"", setdiff(raw, names(data)), "\""))
  }
  if (length(missing)) {
    return(paste0(
      "Mapped column not in data: ", paste(missing, collapse = ", "),
      ". A rename, flatten or pivot upstream may have changed the column ",
      "name \u2014 re-pick it in the gear."
    ))
  }
  if (nrow(data) == 0L) return("No rows to display")
  if (!length(value_cols)) return("Pick a Value column in the gear")
  NULL
}

#' Variable label for a column header, consistent with how the
#' drilldown chart / blockr.dplyr surface `attr(x, "label")`. Returns
#' `NULL` when there is no label or it just repeats the column name.
#' @noRd
dt_col_label <- function(x, name) {
  lbl <- attr(x, "label")
  if (is.null(lbl) || !is.character(lbl) || length(lbl) != 1L) return(NULL)
  lbl <- trimws(lbl)
  if (!nzchar(lbl) || identical(lbl, name)) return(NULL)
  lbl
}

#' Column metadata for the gear popover, stamped on the `<table>` as
#' `data-dt-cols` (JSON `[{name, type}]`; `"[]"` for a structured frame, whose
#' pre-formatted cells offer nothing to pick). Always the block's raw INPUT
#' columns -- NOT the displayed projection -- so the gear's column pickers stay
#' correct while the table shows an aggregated frame, and re-reading the
#' attribute at popover-open time picks up upstream schema changes
#' and server-side config edits without a chrome rebuild.
#' @noRd
dt_gear_cols_json <- function(data) {
  data <- tryCatch(fmt_to_wide(data), error = function(e) data)
  if (!is.data.frame(data) || dt_is_structured(data)) return("[]")
  cols <- lapply(names(data), function(nm) {
    list(
      name = nm,
      type = if (is.numeric(data[[nm]])) "numeric" else "categorical"
    )
  })
  as.character(jsonlite::toJSON(cols, auto_unbox = TRUE))
}

#' Drill onclick target for a row click: the column name + its 0-based index in
#' the rendered column order, or NULLs when `drill` is unset / not a column.
#' @noRd
dt_onclick <- function(drill, all_cols) {
  if (is.null(drill)) return(list(col = NULL, idx = NULL))
  m <- match(drill, all_cols)
  if (is.na(m)) return(list(col = NULL, idx = NULL))
  list(col = drill, idx = m - 1L)
}
