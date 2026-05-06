#' Tile Block
#'
#' Visually rich dashboard tiles driven by a ggplot-style aesthetic
#' mapping. One block, three showcases in v1: `"number"` (big number
#' with optional target / unit / status), `"spark"` (big number plus
#' inline sparkline), `"progress"` (value vs. max as a ring or bar).
#'
#' Input is any data frame. The block reduces as needed via per-
#' aesthetic stat selectors — `mean` for `value` by default, `first`
#' for `target` / `max`, `identity` for `spark_value`. Multiple stats
#' checked on `value` produce multiple cards per measure.
#'
#' Layout auto-adapts: map `rows` / `cols` for scorecard grids; leave
#' them unmapped for a single row of cards.
#'
#' @param showcase One of `"number"`, `"spark"`, `"progress"`.
#' @param state Named list of initial block state. See design spec
#'   `blockr.design/open/kpi-block-v2/` for the full schema. Fields:
#'   `aesthetics` (list of column-name mappings), `stats`
#'   (aesthetic → stat function), `formats` (aesthetic → format
#'   override).
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @return A blockr transform block whose `result` is a long tidy
#'   tibble shaped for tile rendering. [block_output.tile_block()]
#'   renders it as a grid of cards.
#'
#' @seealso [tile_shape()], [tile_demo_data()].
#' @export
new_tile_block <- function(
  showcase = c("number", "spark", "progress"),
  state = list(),
  ...
) {
  showcase <- match.arg(showcase)

  # Fill defaults for partial state.
  state <- fill_tile_state(state, showcase)

  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        r_state <- shiny::reactiveVal(state)

        # Detect column types whenever data changes.
        column_info <- shiny::reactive({
          df <- data()
          if (!is.data.frame(df) || ncol(df) == 0) {
            return(list(numeric = character(), categorical = character(),
                        ordering = character(), all = character()))
          }
          nm <- names(df)
          is_num <- vapply(df, is.numeric, logical(1))
          is_log <- vapply(df, is.logical, logical(1))
          is_date <- vapply(df, function(x) inherits(x, c("Date", "POSIXct", "POSIXlt")),
                            logical(1))
          # Low-cardinality numerics can serve as facets too.
          is_lowcard <- vapply(df, function(x) {
            length(unique(x)) <= 10
          }, logical(1))
          list(
            numeric     = nm[is_num | is_log],
            categorical = nm[!is_num | (is_num & is_lowcard) | is_log],
            ordering    = nm[is_date | is_num],
            all         = nm
          )
        })

        # Populate picker choices when data changes.
        shiny::observeEvent(column_info(), {
          ci <- column_info()
          s <- shiny::isolate(r_state())

          # Tell the panel whether facets are usable for this data.
          session$sendCustomMessage(
            "blockr-bi-tile-flags",
            list(
              ns_id = ns("settings"),
              has_categoricals = length(ci$categorical) > 0L
            )
          )

          # Numeric pickers: value (multi), target, max, spark_value.
          shiny::updateSelectizeInput(session, "aes_value",
            choices = ci$numeric,
            selected = intersect(s$aesthetics$value, ci$numeric))
          shiny::updateSelectizeInput(session, "aes_target",
            choices = c(setNames("", "\u2014"), ci$numeric),
            selected = if (s$aesthetics$target %in% ci$numeric) s$aesthetics$target else "")
          shiny::updateSelectizeInput(session, "aes_max",
            choices = c(setNames("", "\u2014"), ci$numeric),
            selected = if (s$aesthetics$max %in% ci$numeric) s$aesthetics$max else "")
          shiny::updateSelectizeInput(session, "aes_spark_value",
            choices = c(setNames("", "\u2014"), ci$numeric),
            selected = if (s$aesthetics$spark_value %in% ci$numeric) s$aesthetics$spark_value else "")

          # Categorical / facet pickers.
          shiny::updateSelectizeInput(session, "aes_rows",
            choices = c(setNames("", "\u2014"), ci$categorical),
            selected = if (s$aesthetics$rows %in% ci$categorical) s$aesthetics$rows else "")
          shiny::updateSelectizeInput(session, "aes_cols",
            choices = c(setNames("", "\u2014"), ci$categorical),
            selected = if (s$aesthetics$cols %in% ci$categorical) s$aesthetics$cols else "")
          shiny::updateSelectizeInput(session, "aes_label",
            choices = c(setNames("", "\u2014"), ci$all),
            selected = if (s$aesthetics$label %in% ci$all) s$aesthetics$label else "")
          shiny::updateSelectizeInput(session, "aes_unit",
            choices = c(setNames("", "\u2014"), ci$all),
            selected = if (s$aesthetics$unit %in% ci$all) s$aesthetics$unit else "")
          shiny::updateSelectizeInput(session, "aes_status",
            choices = c(setNames("", "\u2014"), ci$all),
            selected = if (s$aesthetics$status %in% ci$all) s$aesthetics$status else "")

          # Ordering picker for spark_x.
          shiny::updateSelectizeInput(session, "aes_spark_x",
            choices = c(setNames("", "\u2014"), ci$ordering),
            selected = if (s$aesthetics$spark_x %in% ci$ordering) s$aesthetics$spark_x else "")
        })

        # Sync UI → state.
        update_state <- function(field, sub, value) {
          s <- shiny::isolate(r_state())
          if (is.null(sub)) s[[field]] <- value
          else s[[field]][[sub]] <- value
          r_state(s)
        }

        shiny::observeEvent(input$showcase, {
          new_sc <- input$showcase
          s <- shiny::isolate(r_state())
          s$showcase <- new_sc
          # Reset the headline reduction to the showcase's natural default
          # (the picker is hidden outside Number, so users can't override).
          new_stat <- switch(new_sc,
            spark = "last",
            progress = "first",
            "mean"
          )
          s$stats$value <- new_stat
          r_state(s)
          # Sync the pill group so the active pill reflects state.
          session$sendInputMessage("stats_value", list(value = new_stat))
        }, ignoreInit = TRUE)

        # Aesthetic syncs.
        for (aes_name in c("value", "rows", "cols", "label", "unit", "status",
                           "target", "spark_value", "spark_x", "max")) {
          local({
            an <- aes_name
            shiny::observeEvent(input[[paste0("aes_", an)]], {
              s <- shiny::isolate(r_state())
              v <- input[[paste0("aes_", an)]]
              if (is.null(v)) v <- if (an == "value") character() else ""
              s$aesthetics[[an]] <- v
              r_state(s)
            }, ignoreNULL = FALSE, ignoreInit = TRUE)
          })
        }

        # Value-stat sync (single-select; only meaningful in Number).
        shiny::observeEvent(input$stats_value, {
          s <- shiny::isolate(r_state())
          v <- input$stats_value
          if (is.null(v) || length(v) == 0 || !nzchar(v)) {
            v <- switch(s$showcase %||% "number",
              spark = "last", progress = "first", "mean")
          }
          s$stats$value <- v
          r_state(s)
        }, ignoreNULL = FALSE, ignoreInit = TRUE)

        list(
          expr = shiny::reactive({
            s <- r_state()
            sc <- s$showcase %||% "number"
            bquote(
              blockr.bi::tile_shape(
                data,
                showcase   = .(sc),
                aesthetics = .(s$aesthetics),
                stats      = .(s$stats),
                formats    = .(s$formats)
              )
            )
          }),
          state = list(
            showcase = shiny::reactive(r_state()$showcase %||% "number"),
            state    = r_state
          )
        )
      })
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      all_sc <- c("number", "spark", "progress")
      shiny::tagList(
        tile_block_deps(),
        shiny::div(
          id = ns("settings"),
          class = "tile-block-settings",
          `data-showcase` = state$showcase,
          tb_pill_group(
            ns("showcase"),
            choices = c("Number" = "number", "Spark" = "spark",
                        "Progress" = "progress"),
            selected = state$showcase,
            multi = FALSE,
            class = "tb-showcase-picker"
          ),
          tb_section_header("Aesthetics"),
          # Value aesthetic + headline stat (single-select, Number only).
          aesthetic_row(ns, "value", "Value", multi = TRUE,
                        selected = state$aesthetics$value,
                        shows = all_sc),
          shiny::div(
            class = "tb-aes-row tb-stat-row",
            `data-shows` = "number",
            shiny::tags$label(
              "Stat",
              `for` = ns("stats_value"),
              class = "tb-aes-label"
            ),
            tb_pill_group(
              ns("stats_value"),
              choices = c("mean", "sum", "median", "min", "max",
                          "count", "n_distinct", "first", "last"),
              selected = state$stats$value,
              multi = FALSE,
              class = "tb-stat-pills"
            )
          ),
          # Showcase-specific numeric aesthetics.
          aesthetic_row(ns, "spark_value", "Spark value",
                        selected = state$aesthetics$spark_value,
                        shows = "spark"),
          aesthetic_row(ns, "spark_x", "Spark x",
                        selected = state$aesthetics$spark_x,
                        shows = "spark"),
          aesthetic_row(ns, "max", "Max",
                        selected = state$aesthetics$max,
                        shows = "progress"),
          aesthetic_row(ns, "target", "Target",
                        selected = state$aesthetics$target,
                        shows = "number"),
          # Display columns shared across showcases.
          aesthetic_row(ns, "label", "Label",
                        selected = state$aesthetics$label,
                        shows = all_sc),
          aesthetic_row(ns, "unit", "Unit",
                        selected = state$aesthetics$unit,
                        shows = all_sc),
          aesthetic_row(ns, "status", "Status",
                        selected = state$aesthetics$status,
                        shows = all_sc),
          # Facet section.
          shiny::div(
            class = "tb-facets-section",
            tb_section_header(
              "Facets",
              hint = "Split the input into a grid of cards"
            ),
            aesthetic_row(ns, "rows", "Rows",
                          selected = state$aesthetics$rows,
                          shows = all_sc, facet = TRUE),
            aesthetic_row(ns, "cols", "Cols",
                          selected = state$aesthetics$cols,
                          shows = all_sc, facet = TRUE)
          )
        )
      )
    },
    class = c("tile_block", "transform_block", "block"),
    allow_empty_state = TRUE,
    ...
  )
}

#' @noRd
aesthetic_row <- function(ns, name, label, multi = FALSE, selected = NULL,
                          shows = NULL, facet = FALSE) {
  classes <- c(
    "tb-aes-row",
    if (facet) "tb-facet-row"
  )
  shiny::div(
    class = paste(classes, collapse = " "),
    `data-shows` = if (length(shows)) paste(shows, collapse = " "),
    shiny::tags$label(
      label,
      `for` = ns(paste0("aes_", name)),
      class = "tb-aes-label"
    ),
    shiny::div(
      class = "tb-aes-control",
      shiny::selectizeInput(
        ns(paste0("aes_", name)),
        label = NULL,
        choices = NULL,
        selected = selected,
        multiple = multi,
        options = list(
          placeholder = "\u2014",
          plugins = if (multi) list("remove_button", "drag_drop") else NULL
        ),
        width = "100%"
      )
    )
  )
}

#' @noRd
tb_section_header <- function(label, hint = NULL) {
  shiny::div(
    class = "tb-section-header",
    shiny::tags$span(label, class = "tb-section-title"),
    if (!is.null(hint)) shiny::tags$span(hint, class = "tb-section-hint")
  )
}

#' @noRd
fill_tile_state <- function(state, showcase) {
  aes_defaults <- list(
    value = character(), rows = "", cols = "", label = "", unit = "",
    status = "", target = "", spark_value = "", spark_x = "", max = ""
  )
  value_stat_default <- switch(showcase,
    spark = "last", progress = "first", "mean")
  stat_defaults <- list(
    value = value_stat_default, target = "first", max = "first",
    spark_value = "identity", spark_x = "identity", status = "first"
  )
  fmt_defaults <- list(
    value = list(kind = NULL, digits = NULL)
  )
  state$showcase   <- state$showcase %||% showcase
  state$aesthetics <- utils::modifyList(aes_defaults, state$aesthetics %||% list())
  state$stats      <- utils::modifyList(stat_defaults, state$stats %||% list())
  state$formats    <- utils::modifyList(fmt_defaults, state$formats %||% list())
  state
}

#' @noRd
tile_block_deps <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "tile-block",
      version = utils::packageVersion("blockr.bi"),
      src = c(file = system.file(package = "blockr.bi")),
      stylesheet = "css/tile-block.css",
      script = "js/tile-block.js"
    )
  )
}

#' @noRd
tb_pill_group <- function(id, choices, selected = NULL, multi = FALSE,
                          class = NULL, style = NULL) {
  if (is.null(names(choices))) names(choices) <- choices
  selected <- intersect(selected, unname(choices))
  pills <- lapply(seq_along(choices), function(i) {
    val <- unname(choices[[i]])
    lab <- names(choices)[[i]]
    active <- val %in% selected
    shiny::tags$button(
      type = "button",
      class = paste("tb-pill", if (active) "tb-pill--active" else NULL),
      `data-value` = val,
      lab
    )
  })
  shiny::div(
    id = id,
    class = paste("tb-pill-group", class),
    style = style,
    `data-select` = if (multi) "multi" else "single",
    pills
  )
}

#' @rdname new_tile_block
#' @param id Module ID.
#' @param x Block object.
#' @export
block_ui.tile_block <- function(id, x, ...) {
  shiny::tagList(
    tile_block_deps(),
    shiny::uiOutput(shiny::NS(id, "result"),
      container = function(...) shiny::div(class = "tile-block-output", ...))
  )
}

#' @rdname new_tile_block
#' @param result Evaluation result (the shaped long frame).
#' @param session Shiny session.
#' @export
block_output.tile_block <- function(x, result, session) {
  shiny::renderUI({
    render_tiles(result)
  })
}

#' @rdname new_tile_block
#' @export
block_render_trigger.tile_block <- function(x, session = shiny::getDefaultReactiveDomain()) {
  # No extra triggers beyond result. All format / showcase info is in result.
  list()
}

#' Render a long tile frame as a grid of cards (server-side).
#' @param df Output of [tile_shape()].
#' @return A Shiny tagList.
#' @noRd
render_tiles <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(shiny::div(
      class = "tb-empty",
      style = "text-align: center; padding: 24px; color: #9ca3af;",
      "No data"
    ))
  }
  showcase <- attr(df, "showcase") %||% "number"

  # Distinct row/col facet levels.
  rows <- unique(df$.row)
  cols <- unique(df$.col)
  has_rows <- any(nzchar(rows))
  has_cols <- any(nzchar(cols))

  # Build a card per (.row, .col, .measure, .stat).
  cell_keys <- paste(df$.row, df$.col, df$.measure, df$.stat, sep = "\u0001")
  # We render all cards in a flat grid for now; facet grouping is
  # visually conveyed via the label / ordering.
  cards <- lapply(seq_len(nrow(df)), function(i) {
    row <- df[i, ]
    make_card(row, showcase, has_rows || has_cols)
  })

  shiny::div(
    class = paste("tb-grid",
                  if (showcase == "spark") "tb-grid--spark" else NULL),
    cards
  )
}

#' @noRd
make_card <- function(row, showcase, show_facet_hint) {
  label_txt <- row$.label
  if (show_facet_hint && (nzchar(row$.row) || nzchar(row$.col))) {
    facet_bits <- c(row$.row, row$.col)
    facet_bits <- facet_bits[nzchar(facet_bits)]
    if (length(facet_bits) > 0) {
      label_txt <- paste0(paste(facet_bits, collapse = " \u00b7 "),
                          " \u2014 ", label_txt)
    }
  }

  stat_suffix <- if (row$.stat != "mean" && row$.stat != "identity" && row$.stat != "first") {
    shiny::tags$span(
      class = "tb-value-stat",
      style = "margin-left: 6px; font-size: 0.7em; color: #9ca3af; font-weight: 500; text-transform: uppercase;",
      row$.stat
    )
  }

  body <- switch(showcase,
    number   = tile_body_number(row),
    spark    = tile_body_spark(row),
    progress = tile_body_progress(row)
  )

  footer <- tile_footer(row, showcase)

  status_pill <- if (!is.na(row$.status) && nzchar(row$.status)) {
    shiny::tags$span(
      class = paste0("tb-status-pill tb-status--", tolower(row$.status)),
      row$.status
    )
  }

  shiny::div(
    class = "tb-card",
    shiny::div(
      class = "tb-card-header",
      shiny::span(class = "tb-label", label_txt, stat_suffix),
      status_pill
    ),
    shiny::div(class = "tb-card-body", body),
    if (!is.null(footer)) shiny::div(class = "tb-card-footer", footer)
  )
}

tile_body_number <- function(row) {
  val_text <- format_value(row$.value, row$.format, row$.digits)
  shiny::span(class = "tb-value", val_text)
}

tile_body_progress <- function(row) {
  max_val <- row$.max
  val <- row$.value
  if (!is.finite(max_val) || max_val <= 0 || !is.finite(val)) {
    return(shiny::span(class = "tb-value", "\u2014"))
  }
  pct <- max(0, min(1, val / max_val))
  # Ring SVG (80x80).
  size <- 88
  stroke <- 8
  r <- (size - stroke) / 2
  cx <- size / 2
  cy <- size / 2
  circ <- 2 * pi * r
  dashoffset <- circ * (1 - pct)
  pct_label <- sprintf("%d%%", round(pct * 100))
  shiny::div(
    class = "tb-progress-ring",
    style = "display: flex; align-items: center; gap: 16px;",
    shiny::tags$svg(
      width = size, height = size, viewBox = sprintf("0 0 %d %d", size, size),
      shiny::tags$circle(
        cx = cx, cy = cy, r = r,
        fill = "none",
        stroke = "var(--blockr-grey-200, #e5e7eb)",
        `stroke-width` = stroke
      ),
      shiny::tags$circle(
        cx = cx, cy = cy, r = r,
        fill = "none",
        stroke = "var(--blockr-color-primary, #2563eb)",
        `stroke-width` = stroke,
        `stroke-linecap` = "round",
        `stroke-dasharray` = sprintf("%f", circ),
        `stroke-dashoffset` = sprintf("%f", dashoffset),
        transform = sprintf("rotate(-90 %d %d)", cx, cy),
        style = "transition: stroke-dashoffset 400ms ease-out;"
      ),
      shiny::tags$text(
        x = cx, y = cy + 4, `text-anchor` = "middle",
        style = "font-size: 16px; font-weight: 600; fill: var(--blockr-color-text-primary, #111827); font-variant-numeric: tabular-nums;",
        pct_label
      )
    ),
    shiny::div(
      shiny::span(class = "tb-value",
        format_value(val, row$.format, row$.digits)),
      shiny::tags$div(
        style = "font-size: 0.8125rem; color: #9ca3af; margin-top: 2px;",
        "of ",
        format_value(max_val, row$.format, row$.digits)
      )
    )
  )
}

tile_body_spark <- function(row) {
  spark <- row$.spark[[1]]
  val_text <- format_value(row$.value, row$.format, row$.digits)
  if (is.null(spark) || length(spark$y) < 2) {
    return(shiny::span(class = "tb-value", val_text))
  }
  # Build a sparkline as inline SVG so we don't need a JS widget.
  y <- spark$y
  y <- y[is.finite(y)]
  if (length(y) < 2) {
    return(shiny::span(class = "tb-value", val_text))
  }
  # Normalise to [0, 1] for viewbox.
  ymin <- min(y); ymax <- max(y)
  yrange <- max(ymax - ymin, .Machine$double.eps)
  xs <- seq(0, 100, length.out = length(y))
  ys <- 30 - (y - ymin) / yrange * 28 - 1   # top padding 1, bottom 1
  pts <- paste(sprintf("%.2f,%.2f", xs, ys), collapse = " ")
  trend_col <- if (y[length(y)] > y[1] * 1.01) {
    "var(--blockr-color-success, #10b981)"
  } else if (y[length(y)] < y[1] * 0.99) {
    "var(--blockr-color-danger, #ef4444)"
  } else {
    "var(--blockr-grey-500, #6b7280)"
  }
  shiny::tagList(
    shiny::span(class = "tb-value", val_text),
    shiny::tags$svg(
      class = "tb-spark",
      viewBox = "0 0 100 30", preserveAspectRatio = "none",
      style = "width: 100%; height: 40px; margin-top: 8px; display: block;",
      shiny::tags$polyline(
        points = pts, fill = "none",
        stroke = trend_col, `stroke-width` = "1.5",
        `stroke-linecap` = "round", `stroke-linejoin` = "round"
      ),
      # Dot at last point
      shiny::tags$circle(
        cx = sprintf("%.2f", xs[length(xs)]),
        cy = sprintf("%.2f", ys[length(ys)]),
        r = "1.8", fill = trend_col
      )
    )
  )
}

tile_footer <- function(row, showcase) {
  bits <- list()
  if (!is.na(row$.target) && showcase != "progress") {
    bits <- c(bits, list(shiny::span(
      class = "tb-target",
      "Target: ",
      format_value(row$.target, row$.format, row$.digits)
    )))
  }
  if (nzchar(row$.unit) && !is.na(row$.unit)) {
    bits <- c(bits, list(shiny::span(class = "tb-unit", row$.unit)))
  }
  if (length(bits) == 0) return(NULL)
  bits
}
