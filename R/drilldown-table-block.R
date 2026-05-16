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
#' - **Drill-down.** When `on_click` names a column, clicking a row
#'   emits the same categorical filter message the drilldown chart
#'   emits, so existing filter links keep working.
#'
#' @param data A data frame.
#' @param label_col Row-stub column. Defaults to the first column.
#' @param value_cols Columns rendered as the table body. Defaults to
#'   every column except `label_col`.
#' @param color `NULL` (plain table) or a [drilldown_table_color()]
#'   list.
#' @param on_click `NULL` (no drill-down) or a column name; clicking a
#'   row emits a categorical filter on that column's value.
#' @param elem_id Shiny namespaced id used to build the `_action`
#'   input name. Required for `on_click` to do anything.
#' @param title,caption Optional strings.
#' @param digits Rounding for numeric display. Default `2`.
#' @param max_height CSS max-height of the scroll container.
#'
#' @return An [htmltools::tagList()].
#' @export
drilldown_table <- function(data,
                            label_col = NULL,
                            value_cols = NULL,
                            color = NULL,
                            on_click = NULL,
                            elem_id = NULL,
                            digits = 2L,
                            max_height = "600px",
                            transform = "none",
                            cor_method = "pearson") {
  stopifnot(is.data.frame(data))

  if (identical(transform, "correlation")) {
    data <- dt_correlation(data, cor_method)
    if (is.null(label_col)) label_col <- "parameter"
  }

  if (is.null(label_col)) label_col <- names(data)[1L]
  if (is.null(value_cols)) value_cols <- setdiff(names(data), label_col)
  value_cols <- intersect(value_cols, names(data))

  color_mode <- if (is.null(color)) "off" else color$type

  if (nrow(data) == 0L || !label_col %in% names(data) ||
      length(value_cols) == 0L) {
    return(dt_render(dt_message_table(), max_height,
                     elem_id, on_click, label_col, value_cols,
                     color_mode, digits, transform))
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
  dt_render(table_tag, max_height, elem_id, on_click,
            label_col, value_cols, color_mode, digits, transform)
}

#' Pairwise correlation matrix of a frame's numeric columns
#'
#' The one transform folded into the table because it is common and
#' has no dplyr verb (`stats::cor()` returns a matrix). Operates on
#' whatever numeric columns the (already-reshaped) input carries, so
#' it stays generic — upstream stock blocks do any pivoting.
#' @noRd
dt_correlation <- function(data, method = "pearson") {
  num <- vapply(data, is.numeric, logical(1L))
  cols <- names(data)[num]
  if (length(cols) < 2L) {
    return(data.frame(message = "Need >= 2 numeric columns",
                      stringsAsFactors = FALSE))
  }
  if (length(cols) > 20L) cols <- cols[seq_len(20L)]
  m <- stats::cor(data[, cols, drop = FALSE],
                  use = "pairwise.complete.obs", method = method)
  m <- round(m, 2L)
  out <- data.frame(parameter = cols, stringsAsFactors = FALSE,
                    check.names = FALSE)
  for (p in cols) out[[p]] <- as.numeric(m[, p])
  out
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
    htmltools::tags$span(class = "blockr-col-name", name),
    if (!is.null(label)) {
      htmltools::tags$span(class = "blockr-col-label", label)
    },
    htmltools::tags$span(class = "blockr-sort-icon")
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
                      on_click, label_col, value_cols,
                      color_mode = "off", digits = 2L,
                      transform = "none") {
  wrapper_id <- paste0(
    "blockr-dt-", sub("^file", "", basename(tempfile("")))
  )

  onclick_idx <- NULL
  if (!is.null(on_click)) {
    all_cols <- c(label_col, value_cols)
    m <- match(on_click, all_cols)
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
    htmltools::tags$style(htmltools::HTML(html_table_delta_css())),
    drilldown_table_dep(),
    htmltools::tags$div(
      id = wrapper_id,
      class = "blockr-html-table-container drilldown-table-container",
      `data-dt-elem-id` = if (!is.null(elem_id)) elem_id else NULL,
      `data-dt-onclick-col` = if (!is.null(onclick_idx)) on_click else NULL,
      `data-dt-onclick-idx` = if (!is.null(onclick_idx)) onclick_idx else NULL,
      `data-dt-color-mode` = color_mode,
      `data-dt-transform` = transform,
      `data-dt-digits` = as.character(digits),
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
  htmltools::htmlDependency(
    name = "drilldown-table",
    version = utils::packageVersion("blockr.bi"),
    src = system.file(package = "blockr.bi"),
    script = "js/drilldown-table.js",
    stylesheet = "css/drilldown-table.css"
  )
}

# --- block -------------------------------------------------------------

#' Drilldown Table Block
#'
#' Transform block wrapping [drilldown_table()]. The visible table is
#' rendered from the upstream (pre-filter) data so every row stays
#' clickable; the block's data output is the upstream data filtered by
#' the last click, using the exact same contract as
#' [new_drilldown_chart_block()] (so existing filter links compose).
#'
#' @param label_col,value_cols,color,on_click,digits,max_height,transform,cor_method
#'   Forwarded to [drilldown_table()]. The block has no in-table title:
#'   the block's own name (card header) serves that role. `transform =
#'   "correlation"` renders the pairwise correlation matrix of the
#'   input's numeric columns (the one dplyr-hard reshape folded in as
#'   an option, mirroring how the drilldown chart aggregates).
#' @param filter_type,filter_column,filter_values,filter_range Click
#'   filter state (kept for contract parity with the drilldown chart;
#'   `filter_range` is unused by the table).
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#' @return A transform block of class `drilldown_table_block`.
#' @export
new_drilldown_table_block <- function(label_col = NULL,
                                      value_cols = NULL,
                                      color = NULL,
                                      on_click = NULL,
                                      digits = 2L,
                                      max_height = "600px",
                                      transform = "none",
                                      cor_method = "pearson",
                                      filter_type = "categorical",
                                      filter_column = NULL,
                                      filter_values = NULL,
                                      filter_range = NULL,
                                      ...) {
  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        r_label_col     <- shiny::reactiveVal(label_col)
        r_value_cols    <- shiny::reactiveVal(value_cols)
        r_color         <- shiny::reactiveVal(color)
        r_on_click      <- shiny::reactiveVal(on_click)
        r_digits        <- shiny::reactiveVal(digits)
        r_max_height    <- shiny::reactiveVal(max_height)
        r_transform     <- shiny::reactiveVal(transform)
        r_cor_method    <- shiny::reactiveVal(cor_method)
        r_filter_type   <- shiny::reactiveVal(filter_type)
        r_filter_column <- shiny::reactiveVal(filter_column)
        r_filter_values <- shiny::reactiveVal(filter_values)
        r_filter_range  <- shiny::reactiveVal(filter_range)

        shiny::observeEvent(input$drilldown_table_block_action, {
          msg <- input$drilldown_table_block_action
          if (is.null(msg)) return()
          act <- msg$action %||% "config"
          if (identical(act, "filter")) {
            r_filter_column(msg$column)
            r_filter_values(msg$values)
            r_filter_type("categorical")
            r_filter_range(NULL)
          } else if (identical(act, "config")) {
            p <- msg$param
            v <- msg$value
            if (identical(p, "color_mode")) {
              if (identical(v, "off")) {
                r_color(NULL)
              } else if (!is.null(color) && identical(color$type, v)) {
                # preserve the constructor's domain / palette
                r_color(color)
              } else {
                r_color(drilldown_table_color(v))
              }
            } else if (identical(p, "on_click")) {
              r_on_click(if (identical(v, "(none)") || !nzchar(v)) NULL else v)
            } else if (identical(p, "digits")) {
              r_digits(as.integer(v))
            } else if (identical(p, "transform")) {
              r_transform(v)
            } else if (identical(p, "cor_method")) {
              r_cor_method(v)
            }
          }
        })

        output$dt_result <- shiny::renderUI({
          d <- data()
          shiny::req(is.data.frame(d))
          drilldown_table(
            d,
            label_col  = r_label_col(),
            value_cols = r_value_cols(),
            color      = r_color(),
            on_click   = r_on_click(),
            elem_id    = ns("drilldown_table_block"),
            digits     = r_digits(),
            max_height = r_max_height(),
            transform  = r_transform(),
            cor_method = r_cor_method()
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
            label_col     = r_label_col,
            value_cols    = r_value_cols,
            color         = r_color,
            on_click      = r_on_click,
            digits        = r_digits,
            max_height    = r_max_height,
            transform     = r_transform,
            cor_method    = r_cor_method,
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
    allow_empty_state = c("label_col", "value_cols", "color", "on_click",
      "filter_column", "filter_values", "filter_range"),
    external_ctrl = c("label_col", "value_cols", "color", "on_click",
      "digits", "max_height", "transform", "cor_method", "filter_type",
      "filter_column", "filter_values", "filter_range"),
    expr_type = "bquoted",
    class = "drilldown_table_block",
    ...
  )
}

#' @importFrom blockr.core block_ui
#' @method block_ui drilldown_table_block
#' @export
block_ui.drilldown_table_block <- function(id, x, ...) shiny::tagList()

#' @importFrom blockr.core block_output
#' @method block_output drilldown_table_block
#' @export
block_output.drilldown_table_block <- function(x, result, session) {
  shiny::renderUI(NULL)
}
