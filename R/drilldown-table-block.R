#' Drilldown Table
#'
#' Tabular sibling to the drilldown chart. Renders a plain rectangular
#' data frame as an interactive HTML table (sticky header, client-side
#' sort and search). Two optional capabilities, both off by default:
#'
#' - **Coloring.** When `color` is a [drilldown_table_color()] spec,
#'   numeric cells get a value-to-background scale (diverging for
#'   correlation matrices, sequential for heatmaps). Purely
#'   presentational; never changes the data.
#' - **Drill-down.** When `drill` names a column, clicking a row
#'   emits the same categorical filter message the drilldown chart
#'   emits, so existing filter links keep working.
#'
#' @param data A data frame.
#' @param label_col Row-stub column. Defaults to the first column.
#' @param value_cols Columns rendered as the table body. Defaults to
#'   every column except `label_col`.
#' @param color `NULL` (plain table) or a [drilldown_table_color()]
#'   list.
#' @param drill `NULL` (no drill-down) or a column name; clicking a
#'   row emits a categorical filter on that column's value.
#' @param elem_id Shiny namespaced id used to build the `_action`
#'   input name. Required for `drill` to do anything.
#' @param title,caption Optional strings.
#' @param digits Rounding for numeric display. Default `2`.
#' @param max_height CSS max-height of the scroll container.
#'
#' @details
#' **Structured "Table 1" input.** When `data` follows the dotted-column
#' convention that [summary_table()] emits — a tidy `.fmt` frame (numeric
#' columns + a per-row `.fmt` template + `.group`), or the already-wide
#' display grid with `.section_*` / `.var` / `.label` / `.indent` columns —
#' the renderer switches to the *structured* layout: row-side section
#' headers with collapse/expand toggles, `.indent` row stubs, and
#' multi-level column spanners parsed from `|`-delimited column names (each
#' leaf carries its header text in `attr(col, "label")`). This is the same
#' structure [html_table()] renders, folded into the interactive table so
#' one renderer covers flat *and* structured tables (drill / cell colour
#' still apply to the flat path). A plain rectangular frame renders exactly
#' as before.
#'
#' @return An [htmltools::tagList()].
#' @export
drilldown_table <- function(data,
                            label_col = NULL,
                            value_cols = NULL,
                            color = NULL,
                            drill = NULL,
                            elem_id = NULL,
                            digits = 2L,
                            max_height = "600px") {
  stopifnot(is.data.frame(data))

  # Tidy `.fmt` form (numbers + per-row template + `.group`) -> wide display
  # grid (format-then-spread), and detect the structured / sectioned form.
  # No-op on a plain flat frame.
  data <- fmt_to_wide(data)
  if (dt_is_structured(data)) {
    return(
      dt_render_structured(data, elem_id, drill, color, digits, max_height)
    )
  }

  if (is.null(label_col)) label_col <- names(data)[1L]
  if (is.null(value_cols)) value_cols <- setdiff(names(data), label_col)
  value_cols <- intersect(value_cols, names(data))

  color_mode <- if (is.null(color)) "off" else color$type

  if (nrow(data) == 0L || !label_col %in% names(data) ||
      length(value_cols) == 0L) {
    return(dt_render(dt_message_table(), max_height,
                     elem_id, drill, label_col, value_cols,
                     color_mode, digits))
  }

  # ---- color scale ----------------------------------------------------
  cell_bg <- NULL
  if (!is.null(color)) {
    num_cols <- value_cols[vapply(data[value_cols], is.numeric, logical(1L))]
    if (length(num_cols)) {
      vals <- unlist(data[num_cols], use.names = FALSE)
      vals <- vals[is.finite(vals)]
      dom <- color$domain
      if (is.null(dom) && length(vals)) dom <- range(vals)
      if (!is.null(dom) && length(vals) && diff(range(dom)) > 0) {
        cell_bg <- dt_color_fun(color$type, dom, color$palette)
      }
    }
  }

  # ---- thead ----------------------------------------------------------
  th_cells <- list(dt_th(label_col, 0L, stub = TRUE,
                         label = dt_col_label(data[[label_col]], label_col)))
  for (i in seq_along(value_cols)) {
    th_cells[[length(th_cells) + 1L]] <- dt_th(
      value_cols[i], i,
      label = dt_col_label(data[[value_cols[i]]], value_cols[i])
    )
  }
  thead <- htmltools::tags$thead(htmltools::tags$tr(th_cells))

  # ---- tbody ----------------------------------------------------------
  num_flag <- vapply(data[value_cols], is.numeric, logical(1L))
  body_rows <- vector("list", nrow(data))
  for (r in seq_len(nrow(data))) {
    cells <- list(htmltools::tags$td(
      class = "blockr-stub",
      as.character(data[[label_col]][r])
    ))
    for (j in seq_along(value_cols)) {
      v <- data[[value_cols[j]]][r]
      if (is.na(v)) {
        cells[[length(cells) + 1L]] <- htmltools::tags$td(
          class = "blockr-data", htmltools::HTML("&mdash;")
        )
        next
      }
      disp <- if (num_flag[j]) {
        format(round(as.numeric(v), digits), nsmall = 0L, trim = TRUE)
      } else {
        as.character(v)
      }
      style <- NULL
      if (!is.null(cell_bg) && num_flag[j]) {
        bg <- cell_bg(as.numeric(v))
        style <- paste0("background:", bg$bg, ";color:", bg$fg, ";")
      }
      cells[[length(cells) + 1L]] <- htmltools::tags$td(
        class = "blockr-data", style = style, disp
      )
    }
    body_rows[[r]] <- htmltools::tags$tr(class = "blockr-data-row", cells)
  }
  tbody <- htmltools::tags$tbody(body_rows)

  table_tag <- htmltools::tags$table(class = "blockr-table", thead, tbody)
  dt_render(table_tag, max_height, elem_id, drill,
            label_col, value_cols, color_mode, digits)
}

# ---------------------------------------------------------------------------
# Structured ("Table 1") rendering — section nesting, indents, spanners.
# Reuses html_table()'s thead / tbody builders so the structure matches the
# static html_table renderer exactly; only the surrounding chrome (the
# table-block wrapper, gear, drill/colour attributes) differs.
# ---------------------------------------------------------------------------

#' Does this (already wide) frame carry the dotted-column structure?
#'
#' True when it has any row-side section column (`.section_*` / `.var`) OR a
#' `.label` stub OR an `.indent` column — the signals [summary_table()] emits
#' and [html_table()] renders. A plain flat frame has none of these.
#' @noRd
dt_is_structured <- function(data) {
  any(grepl("^\\.(section_\\d+|var|label|indent)$", names(data)))
}

#' Render the structured form as the interactive table-block.
#'
#' Builds the section-aware `<tbody>` and multi-level `<thead>` with
#' [build_html_tbody()] / [build_html_thead()] (the very builders
#' [html_table()] uses), then wraps them in the table-block chrome via
#' [dt_render()] so the existing JS (gear, sort, search, collapse) attaches.
#' Cell colouring and drill are wired for completeness; the structured stub
#' column is the drill target when `drill` names it.
#' @noRd
dt_render_structured <- function(data, elem_id, drill, color, digits,
                                 max_height) {
  section_cols <- grep("^\\.(section_\\d+|var)$", names(data), value = TRUE)
  stub_col     <- if (".label" %in% names(data)) ".label" else NULL
  styling_cols <- intersect(c(".indent", ".bold", ".italic"), names(data))
  data_cols    <- setdiff(names(data),
                          c(section_cols, stub_col, styling_cols))

  if (length(data_cols) == 0L || nrow(data) == 0L) {
    return(dt_render(dt_message_table(), max_height, elem_id, NULL,
                     NULL, character(), "off", digits))
  }

  thead <- build_html_thead(data, data_cols, stub_col, stub_sortable = FALSE)
  tbody <- build_html_tbody(data, section_cols, stub_col, data_cols,
                            styling_cols = styling_cols)

  table_tag <- htmltools::tags$table(class = "blockr-table", thead, tbody)

  # The stub is column 0 in a structured table; only honour drill when it
  # names the stub (the only categorical row identity that survives the
  # section layout). Cell colour over `.fmt` strings is not meaningful, so
  # structured tables render uncoloured.
  drill_use <- if (!is.null(drill) && !is.null(stub_col) &&
                   identical(drill, stub_col)) {
    stub_col
  } else {
    NULL
  }

  dt_render(table_tag, max_height, elem_id, drill_use,
            stub_col %||% character(),
            data_cols, "off", digits,
            structured = TRUE)
}

#' Color spec for [drilldown_table()]
#'
#' @param type `"diverging"` (e.g. correlations around 0) or
#'   `"sequential"` (e.g. grade heatmaps).
#' @param domain `NULL` to infer from the data, or `c(min, max)`.
#' @param palette `NULL` for type defaults, or a character vector of
#'   hex colors (length 3 for diverging low/mid/high, length 2 for
#'   sequential low/high).
#' @return A list consumed by [drilldown_table()].
#' @export
drilldown_table_color <- function(type = c("diverging", "sequential"),
                                   domain = NULL, palette = NULL) {
  type <- match.arg(type)
  list(type = type, domain = domain, palette = palette)
}

# --- internal helpers --------------------------------------------------

#' @noRd
dt_th <- function(name, idx, stub = FALSE, label = NULL) {
  cls <- if (stub) "blockr-stub-header blockr-sortable" else
    "blockr-col-header leaf blockr-sortable"
  htmltools::tags$th(
    class = cls,
    `data-col-index` = idx,
    # Name + sort indicator share one row (name left, arrow right, same
    # baseline); the variable label sits on its own line below.
    htmltools::tags$div(
      class = "dt-th-namerow",
      htmltools::tags$span(class = "blockr-col-name", name),
      htmltools::tags$span(class = "blockr-sort-icon")
    ),
    if (!is.null(label)) {
      htmltools::tags$span(class = "blockr-col-label", label)
    }
  )
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

#' @noRd
dt_message_table <- function(msg = "No data") {
  htmltools::tags$table(
    class = "blockr-table",
    htmltools::tags$tbody(
      htmltools::tags$tr(htmltools::tags$td(class = "blockr-data", msg))
    )
  )
}

#' @noRd
dt_render <- function(table_tag, max_height, elem_id,
                      drill, label_col, value_cols,
                      color_mode = "off", digits = 2L,
                      structured = FALSE) {
  wrapper_id <- paste0(
    "blockr-dt-", sub("^file", "", basename(tempfile("")))
  )

  onclick_idx <- NULL
  if (!is.null(drill)) {
    all_cols <- c(label_col, value_cols)
    m <- match(drill, all_cols)
    if (!is.na(m)) onclick_idx <- m - 1L
  }

  shared_css <- htmltools::tags$style(
    htmltools::HTML(html_table_shared_css_fallback())
  )

  header_div <- htmltools::tags$div(
    class = "blockr-html-table-header",
    htmltools::tags$div(
      class = "blockr-html-table-toolbar",
      htmltools::tags$input(
        type = "search", class = "blockr-search",
        placeholder = "Search…", `aria-label` = "Search table"
      )
    )
  )

  scroll_style <- if (!is.null(max_height)) {
    paste0("max-height:", max_height, ";overflow:auto;")
  } else {
    "overflow:auto;"
  }

  htmltools::tagList(
    shared_css,
    # The delta CSS is the STRUCTURED Table-1 design treatment (section headers,
    # two-tier arm headers, the stub indent + medium stat/value weights). Inject
    # it only for structured tables — a flat data table should match the canonical
    # html preview (normal-weight cells, plain stub), not carry the Table-1 styling.
    if (isTRUE(structured)) {
      htmltools::tags$style(htmltools::HTML(html_table_delta_css()))
    },
    drilldown_table_dep(),
    htmltools::tags$div(
      id = wrapper_id,
      class = paste(
        "blockr-html-table-container drilldown-table-container",
        if (isTRUE(structured)) "drilldown-table-structured" else NULL
      ),
      `data-dt-elem-id` = if (!is.null(elem_id)) elem_id else NULL,
      `data-dt-onclick-col` = if (!is.null(onclick_idx)) drill else NULL,
      `data-dt-onclick-idx` = if (!is.null(onclick_idx)) onclick_idx else NULL,
      `data-dt-color-mode` = color_mode,
      `data-dt-digits` = as.character(digits),
      `data-dt-structured` = if (isTRUE(structured)) "1" else NULL,
      `data-initial-expanded` = if (isTRUE(structured)) "1" else NULL,
      header_div,
      htmltools::tags$div(
        class = "blockr-table-wrapper",
        style = scroll_style,
        table_tag
      )
    )
  )
}

#' @noRd
dt_color_fun <- function(type, domain, palette) {
  lo <- domain[1L]
  hi <- domain[2L]
  hex2rgb <- function(h) grDevices::col2rgb(h)[, 1L]
  lerp <- function(a, b, t) round(a + (b - a) * t)
  lum <- function(rgb) {
    (0.299 * rgb[1L] + 0.587 * rgb[2L] + 0.114 * rgb[3L]) / 255
  }
  to_hex <- function(rgb) {
    grDevices::rgb(rgb[1L], rgb[2L], rgb[3L], maxColorValue = 255)
  }

  if (identical(type, "diverging")) {
    pal <- palette %||% c("#99000d", "#ffffff", "#08306b")
    c1 <- hex2rgb(pal[1L]); c2 <- hex2rgb(pal[2L]); c3 <- hex2rgb(pal[3L])
    mid <- if (lo < 0 && hi > 0) 0 else (lo + hi) / 2
    function(v) {
      v <- max(min(v, hi), lo)
      if (v <= mid) {
        t <- if (mid == lo) 0 else (v - lo) / (mid - lo)
        rgb <- c(lerp(c1[1L], c2[1L], t), lerp(c1[2L], c2[2L], t),
                 lerp(c1[3L], c2[3L], t))
      } else {
        t <- if (hi == mid) 0 else (v - mid) / (hi - mid)
        rgb <- c(lerp(c2[1L], c3[1L], t), lerp(c2[2L], c3[2L], t),
                 lerp(c2[3L], c3[3L], t))
      }
      list(bg = to_hex(rgb), fg = if (lum(rgb) < 0.55) "#ffffff" else "#111827")
    }
  } else {
    pal <- palette %||% c("#eef2ff", "#1d4ed8")
    c1 <- hex2rgb(pal[1L]); c2 <- hex2rgb(pal[2L])
    function(v) {
      v <- max(min(v, hi), lo)
      t <- if (hi == lo) 0 else (v - lo) / (hi - lo)
      rgb <- c(lerp(c1[1L], c2[1L], t), lerp(c1[2L], c2[2L], t),
               lerp(c1[3L], c2[3L], t))
      list(bg = to_hex(rgb), fg = if (lum(rgb) < 0.55) "#ffffff" else "#111827")
    }
  }
}

#' @noRd
drilldown_table_dep <- function() {
  htmltools::tagList(
    # Shared blockr.dplyr CSS/JS (gear, popover, rows, Blockr.Select, icons) —
    # same dep names as the chart so they de-dupe on a page with both blocks.
    htmltools::htmlDependency(
      name = "blockr-blocks-css",
      version = paste0(utils::packageVersion("blockr.dplyr"), ".2"),
      src = system.file("css", package = "blockr.dplyr"),
      stylesheet = c("blockr-blocks.css", "blockr-select.css")
    ),
    htmltools::htmlDependency(
      name = "blockr-select-js",
      version = paste0(utils::packageVersion("blockr.dplyr"), ".2"),
      src = system.file("js", package = "blockr.dplyr"),
      script = c("blockr-core.js", "blockr-select.js")
    ),
    # Shared popover CSS (the dd-* section/row/segmented/add classes) lives in
    # drilldown-chart.css; the table's gear popover now reuses it.
    htmltools::htmlDependency(
      name = "drilldown-chart-css",
      version = paste0(utils::packageVersion("blockr.bi"), ".24"),
      src = system.file("css", package = "blockr.bi"),
      stylesheet = "drilldown-chart.css"
    ),
    htmltools::htmlDependency(
      name = "drilldown-table",
      version = utils::packageVersion("blockr.bi"),
      src = system.file(package = "blockr.bi"),
      # drilldown-config.js (the shared engine) must load before the table JS.
      script = c("js/drilldown-config.js", "js/drilldown-table.js"),
      stylesheet = "css/drilldown-table.css"
    )
  )
}

# --- block -------------------------------------------------------------

#' Build arguments metadata for the drill-down table block
#'
#' Registry/LLM metadata so the assistant and MCP can introspect the table
#' block (the chart block has this; the table was previously invisible).
#' @noRd
table_arguments <- function() {
  structure(
    c(
      rowname = paste0(
        "The single column shown as the row labels (the left-hand stub). Names ",
        "a data column; defaults to the first column."
      ),
      values = paste0(
        "The columns rendered as the table body (the data cells). When the user ",
        "names specific value/measure columns, set this to EXACTLY those ",
        "columns -- do not leave it null. Leave null only to mean 'all columns ",
        "except `rowname`'."
      ),
      cell_color = paste0(
        "Cell shading: a `drilldown_table_color()` spec, or null for a plain ",
        "table. NOTE this is a colour SPEC object, not a column name (unlike the ",
        "drill-down chart's `color`). Diverging scale for correlation matrices, ",
        "sequential for heatmaps. Presentational only; never changes the data."
      ),
      drill = paste0(
        "Column a row click filters downstream on. Optional; default null = a ",
        "click is inert. When set, clicking a row emits a categorical filter ",
        "on that column's value for the row — the same filter contract as the ",
        "drill-down chart."
      ),
      digits = "Decimal places for numeric display. Default 2."
    ),
    examples = list(
      # Populated (label_col + explicit value_cols array + drill) so the model
      # anchors on setting these to named columns, not leaving them null.
      rowname = "Region", values = list("Revenue", "Profit"),
      cell_color = NULL, drill = "Region",
      digits = 2L
    ),
    prompt = paste(
      "Interactive table (sticky header, client-side sort and search) that can",
      "also act as a click-to-filter control — the tabular sibling of the",
      "drill-down chart. Two optional capabilities, both off by default:",
      "\n- Coloring: set `cell_color` to a `drilldown_table_color()` spec to give",
      "numeric cells a value-to-background scale (diverging for correlation,",
      "sequential for heatmaps). Presentational only.",
      "\n- Drill-down: set `drill` to a column; clicking a row emits a",
      "categorical filter on that column's value, so downstream blocks filter",
      "— the same filter contract as the drill-down chart.",
      "\n`rowname`/`values` pick the row-stub and body",
      "columns: set `values` to EXACTLY the columns the user names (e.g.",
      "\"Revenue and Profit\" -> values = [\"Revenue\", \"Profit\"]); leave",
      "them null only when the user does not name specific columns (defaults to",
      "the first column plus the rest)."
    )
  )
}

#' Drilldown Table Block
#'
#' Transform block wrapping [drilldown_table()]. The visible table is
#' rendered from the upstream (pre-filter) data so every row stays
#' clickable; the block's data output is the upstream data filtered by
#' the last click, using the exact same contract as
#' [new_drilldown_chart_block()] (so existing filter links compose).
#'
#' @param rowname,values,cell_color,drill,digits,max_height
#'   Forwarded to [drilldown_table()]. The block has no in-table title:
#'   the block's own name (card header) serves that role.
#' @param filter_type,filter_column,filter_values,filter_range Click
#'   filter state (kept for contract parity with the drilldown chart;
#'   `filter_range` is unused by the table).
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#' @return A transform block of class `table_block`.
#' @export
new_table_block <- function(rowname = NULL,
                                      values = NULL,
                                      cell_color = NULL,
                                      drill = NULL,
                                      digits = 2L,
                                      max_height = "600px",
                                      filter_type = "categorical",
                                      filter_column = NULL,
                                      filter_values = NULL,
                                      filter_range = NULL,
                                      ...) {
  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        r_rowname    <- shiny::reactiveVal(rowname)
        r_values     <- shiny::reactiveVal(values)
        r_cell_color    <- shiny::reactiveVal(cell_color)
        r_drill      <- shiny::reactiveVal(drill)
        r_digits        <- shiny::reactiveVal(digits)
        r_max_height    <- shiny::reactiveVal(max_height)
        r_filter_type   <- shiny::reactiveVal(filter_type)
        r_filter_column <- shiny::reactiveVal(filter_column)
        r_filter_values <- shiny::reactiveVal(filter_values)
        r_filter_range  <- shiny::reactiveVal(filter_range)

        # Only write a reactiveVal when the value actually changes. JS echoes
        # the full config/filter on any popover change, so a blind set would
        # invalidate (and re-render the table) on every echo — the
        # "prevent R->JS->R loops" guard the chart block uses. (Mirrors
        # drilldown-chart-block.R.)
        upd <- function(rv, v) {
          if (!identical(shiny::isolate(rv()), v)) rv(v)
        }

        shiny::observeEvent(input$drilldown_table_block_action, {
          msg <- input$drilldown_table_block_action
          if (is.null(msg)) return()
          act <- msg$action %||% "config"
          if (identical(act, "filter")) {
            upd(r_filter_column, msg$column)
            upd(r_filter_values, msg$values)
            upd(r_filter_type, "categorical")
            upd(r_filter_range, NULL)
          } else if (identical(act, "config")) {
            p <- msg$param
            v <- msg$value
            if (identical(p, "color_mode")) {
              if (identical(v, "off")) {
                upd(r_cell_color, NULL)
              } else if (!is.null(cell_color) && identical(cell_color$type, v)) {
                # preserve the constructor's domain / palette
                upd(r_cell_color, cell_color)
              } else {
                upd(r_cell_color, drilldown_table_color(v))
              }
            } else if (identical(p, "drill")) {
              upd(r_drill, if (identical(v, "(none)") || !nzchar(v)) NULL else v)
            } else if (identical(p, "digits")) {
              upd(r_digits, as.integer(v))
            }
          }
        })

        output$dt_result <- shiny::renderUI({
          d <- data()
          shiny::req(is.data.frame(d))
          drilldown_table(
            d,
            label_col  = r_rowname(),
            value_cols = r_values(),
            color      = r_cell_color(),
            drill   = r_drill(),
            elem_id    = ns("drilldown_table_block"),
            digits     = r_digits(),
            max_height = r_max_height()
          )
        })

        list(
          expr = shiny::reactive({
            col  <- r_filter_column()
            vals <- r_filter_values()
            if (is.null(col) || is.null(vals) || length(vals) == 0) {
              blockr.core::bbquote(dplyr::filter(.(data), TRUE))
            } else if (length(vals) == 1) {
              blockr.core::bbquote(
                dplyr::filter(.(data), .data[[.(col)]] == .(val)),
                list(col = col, val = vals[[1]])
              )
            } else {
              blockr.core::bbquote(
                dplyr::filter(.(data), .data[[.(col)]] %in% .(vals)),
                list(col = col, vals = vals)
              )
            }
          }),
          state = list(
            rowname       = r_rowname,
            values        = r_values,
            cell_color    = r_cell_color,
            drill      = r_drill,
            digits        = r_digits,
            max_height    = r_max_height,
            filter_type   = r_filter_type,
            filter_column = r_filter_column,
            filter_values = r_filter_values,
            filter_range  = r_filter_range
          )
        )
      })
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(shiny::uiOutput(ns("dt_result")))
    },
    dat_valid = function(data) {
      if (!is.data.frame(data)) stop("Input must be a data frame")
    },
    allow_empty_state = c("rowname", "values", "cell_color", "drill",
      "filter_column", "filter_values", "filter_range"),
    external_ctrl = c("rowname", "values", "cell_color", "drill",
      "digits", "max_height", "filter_type",
      "filter_column", "filter_values", "filter_range"),
    expr_type = "bquoted",
    class = "table_block",
    ...
  )
}

#' Drilldown Table Block (deprecated alias)
#'
#' Deprecated alias for [new_table_block()]. The block was renamed from
#' "Drill-Down Table" to "Table" (the drill-down behaviour is an opt-in
#' `drill` feature, not the block's identity). Kept so existing serialized
#' boards that reference `new_drilldown_table_block` still deserialize.
#'
#' @param ... Forwarded to [new_table_block()].
#' @return A transform block of class `table_block`.
#' @export
new_drilldown_table_block <- function(...) {
  new_table_block(...)
}

# NB: no block_ui / block_output overrides. The styled drill-down table renders
# in the Controls pane (the `ui =` function above, via uiOutput("dt_result")),
# and the block falls back to the transform_block defaults for the output pane —
# so the Preview pane shows the filtered passthrough data frame as a DT, exactly
# like new_drilldown_chart_block(). (They were previously nulled out, which left
# the Preview pane blank.)
