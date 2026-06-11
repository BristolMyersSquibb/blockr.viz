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
          # Single-select aesthetics: prepend "(none)" so the user can
          # explicitly clear an assignment after picking one (matches the
          # blockr.ggplot pattern). Multi-select Value uses the X chip
          # control to remove individual measures.
          shiny::updateSelectizeInput(session, "aes_value",
            choices = ci$numeric,
            selected = intersect(s$aesthetics$value, ci$numeric))
          shiny::updateSelectizeInput(session, "aes_target",
            choices = c("(none)", ci$numeric),
            selected = if (s$aesthetics$target %in% ci$numeric) s$aesthetics$target else "(none)")
          shiny::updateSelectizeInput(session, "aes_max",
            choices = c("(none)", ci$numeric),
            selected = if (s$aesthetics$max %in% ci$numeric) s$aesthetics$max else "(none)")
          shiny::updateSelectizeInput(session, "aes_spark_value",
            choices = c("(none)", ci$numeric),
            selected = if (s$aesthetics$spark_value %in% ci$numeric) s$aesthetics$spark_value else "(none)")

          # Categorical / facet pickers.
          shiny::updateSelectizeInput(session, "aes_rows",
            choices = c("(none)", ci$categorical),
            selected = if (s$aesthetics$rows %in% ci$categorical) s$aesthetics$rows else "(none)")
          shiny::updateSelectizeInput(session, "aes_cols",
            choices = c("(none)", ci$categorical),
            selected = if (s$aesthetics$cols %in% ci$categorical) s$aesthetics$cols else "(none)")
          shiny::updateSelectizeInput(session, "aes_label",
            choices = c("(none)", ci$all),
            selected = if (s$aesthetics$label %in% ci$all) s$aesthetics$label else "(none)")
          shiny::updateSelectizeInput(session, "aes_unit",
            choices = c("(none)", ci$all),
            selected = if (s$aesthetics$unit %in% ci$all) s$aesthetics$unit else "(none)")
          shiny::updateSelectizeInput(session, "aes_status",
            choices = c("(none)", ci$all),
            selected = if (s$aesthetics$status %in% ci$all) s$aesthetics$status else "(none)")

          # Ordering picker for spark_x.
          shiny::updateSelectizeInput(session, "aes_spark_x",
            choices = c("(none)", ci$ordering),
            selected = if (s$aesthetics$spark_x %in% ci$ordering) s$aesthetics$spark_x else "(none)")

          # Color-by picker: special sentinels "status" / "measure"
          # alongside actual columns. We label the sentinels for clarity
          # but keep their values as bare strings.
          color_choices <- c(
            "(none)" = "(none)",
            "Status (uses Status aesthetic)" = "status",
            "Measure (one slot per measure)" = "measure",
            stats::setNames(ci$all, ci$all)
          )
          color_sel <- s$color$by
          if (!color_sel %in% c("status", "measure", ci$all)) {
            color_sel <- "(none)"
          }
          shiny::updateSelectizeInput(session, "aes_color_by",
            choices = color_choices,
            selected = color_sel)
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

        # Template sync. Picking a recipe sets the implicit showcase
        # and a sensible default stat (the stat-pill row is hidden in
        # Trends/Progress/KPI list, so the user can't override it
        # there). Custom keeps whatever the showcase pill currently
        # says. The settings wrapper's data-template attribute drives
        # per-row visibility, so we ping the client to update that.
        shiny::observeEvent(input$template, {
          tmpl <- input$template
          if (is.null(tmpl) || !nzchar(tmpl)) return()
          s <- shiny::isolate(r_state())
          s$template <- tmpl
          implicit <- list(
            numbers   = list(showcase = "number",   stat = "mean"),
            kpi_list  = list(showcase = "number",   stat = "mean"),
            trends    = list(showcase = "spark",    stat = "last"),
            progress  = list(showcase = "progress", stat = "first"),
            scorecard = list(showcase = "number",   stat = "sum")
          )[[tmpl]]
          if (!is.null(implicit)) {
            s$showcase   <- implicit$showcase
            s$stats$value <- implicit$stat
            session$sendInputMessage("stats_value",
                                     list(value = implicit$stat))
          }
          r_state(s)
          session$sendCustomMessage(
            "blockr-bi-tile-template",
            list(ns_id = ns("settings"), template = tmpl)
          )
        }, ignoreInit = TRUE)

        # Trends template: the user picks one "Value column" via the
        # spark_value picker, but tile_shape needs both `value` (for
        # the headline number) and `spark_value` (for the line). Mirror
        # the picked column into `value` whenever we're in Trends.
        shiny::observeEvent(input$aes_spark_value, {
          s <- shiny::isolate(r_state())
          if (!identical(s$template, "trends")) return()
          v <- input$aes_spark_value
          if (is.null(v) || identical(v, "(none)") || !nzchar(v)) return()
          s$aesthetics$value <- v
          r_state(s)
        }, ignoreNULL = FALSE, ignoreInit = TRUE)

        # Aesthetic syncs.
        for (aes_name in c("value", "rows", "cols", "label", "unit", "status",
                           "target", "spark_value", "spark_x", "max")) {
          local({
            an <- aes_name
            shiny::observeEvent(input[[paste0("aes_", an)]], {
              s <- shiny::isolate(r_state())
              v <- input[[paste0("aes_", an)]]
              if (is.null(v)) v <- if (an == "value") character() else ""
              # Translate the literal "(none)" sentinel back to "" so
              # downstream shaping treats it as unmapped.
              if (an != "value" && identical(v, "(none)")) v <- ""
              s$aesthetics[[an]] <- v
              r_state(s)
            }, ignoreNULL = FALSE, ignoreInit = TRUE)
          })
        }

        # Color-by sync.
        shiny::observeEvent(input$aes_color_by, {
          s <- shiny::isolate(r_state())
          v <- input$aes_color_by
          if (is.null(v) || identical(v, "(none)")) v <- ""
          s$color$by <- v
          r_state(s)
        }, ignoreNULL = FALSE, ignoreInit = TRUE)

        # Color intensity sync.
        shiny::observeEvent(input$color_intensity, {
          s <- shiny::isolate(r_state())
          v <- input$color_intensity
          if (is.null(v) || !nzchar(v)) v <- "tint"
          s$color$intensity <- v
          r_state(s)
        }, ignoreNULL = FALSE, ignoreInit = TRUE)

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
                formats    = .(s$formats),
                color      = .(s$color)
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
      shiny::tagList(
        tile_block_deps(),
        shiny::div(
          id = ns("settings"),
          class = "tile-block-settings",
          `data-template` = state$template,
          `data-showcase` = state$showcase,
          # --- Gear button (top-right) — toggles popover ----------------
          shiny::tags$button(
            id = ns("gear_btn"),
            type = "button",
            class = "blockr-gear-btn tb-gear-btn",
            `aria-label` = "Advanced options",
            title = "Advanced options",
            shiny::HTML(tb_gear_svg())
          ),
          # --- Template picker (recipe-led UI) --------------------------
          tb_pill_group(
            ns("template"),
            choices = c("Numbers"   = "numbers",
                        "KPI list"  = "kpi_list",
                        "Trends"    = "trends",
                        "Progress"  = "progress",
                        "Scorecard" = "scorecard",
                        "Custom"    = "custom"),
            selected = state$template,
            multi = FALSE,
            class = "tb-template-picker"
          ),
          # --- Custom-only: showcase picker ----------------------------
          shiny::div(
            class = "tb-aes-row tb-showcase-row",
            `data-template-shows` = "custom",
            shiny::tags$label("Showcase", class = "tb-aes-label"),
            tb_pill_group(
              ns("showcase"),
              choices = c("Number" = "number", "Spark" = "spark",
                          "Progress" = "progress"),
              selected = state$showcase,
              multi = FALSE,
              class = "tb-showcase-picker"
            )
          ),
          # --- Value / Columns / Measures ------------------------------
          # Same input drives every template; only the label changes.
          # Hidden in Trends (the time-series Value lives in spark_value).
          aesthetic_row(ns, "value", "Columns", multi = TRUE,
                        selected = state$aesthetics$value,
                        template_shows = c("numbers", "kpi_list",
                                           "progress", "scorecard",
                                           "custom"),
                        labels = c(numbers   = "Columns",
                                   kpi_list  = "Value column",
                                   progress  = "Value column",
                                   scorecard = "Measures",
                                   custom    = "Value")),
          # --- Aggregation / Stat (Numbers + Scorecard + Custom) -------
          shiny::div(
            class = "tb-aes-row tb-stat-row",
            `data-template-shows` = "numbers scorecard custom",
            tb_template_label(c(numbers   = "Aggregation",
                                scorecard = "Aggregation",
                                custom    = "Stat")),
            tb_pill_group(
              ns("stats_value"),
              choices = c("mean", "sum", "median", "min", "max",
                          "count", "n_distinct", "first", "last"),
              selected = state$stats$value,
              multi = FALSE,
              class = "tb-stat-pills"
            )
          ),
          # --- Label (KPI list + Progress + Custom) --------------------
          aesthetic_row(ns, "label", "Label",
                        selected = state$aesthetics$label,
                        template_shows = c("kpi_list", "progress",
                                           "custom"),
                        labels = c(kpi_list = "Metric column",
                                   progress = "Metric column",
                                   custom   = "Label")),
          # --- Target (KPI list + Custom) ------------------------------
          aesthetic_row(ns, "target", "Target",
                        selected = state$aesthetics$target,
                        template_shows = c("kpi_list", "custom")),
          # --- Status (KPI list + Progress + Custom) -------------------
          aesthetic_row(ns, "status", "Status",
                        selected = state$aesthetics$status,
                        template_shows = c("kpi_list", "progress",
                                           "custom")),
          # --- Trends: time + value time-series columns ----------------
          aesthetic_row(ns, "spark_x", "Time column",
                        selected = state$aesthetics$spark_x,
                        template_shows = c("trends", "custom"),
                        labels = c(trends = "Time column",
                                   custom = "Spark x")),
          aesthetic_row(ns, "spark_value", "Value column",
                        selected = state$aesthetics$spark_value,
                        template_shows = c("trends", "custom"),
                        labels = c(trends = "Value column",
                                   custom = "Spark value")),
          # --- Progress: max / target column ---------------------------
          aesthetic_row(ns, "max", "Target / max",
                        selected = state$aesthetics$max,
                        template_shows = c("progress", "custom"),
                        labels = c(progress = "Target / max",
                                   custom   = "Max")),
          # --- Facets: Rows (Scorecard + Custom) -----------------------
          aesthetic_row(ns, "rows", "Rows",
                        selected = state$aesthetics$rows,
                        template_shows = c("scorecard", "custom"),
                        facet = TRUE),
          # --- Facets: Cols (Trends + Scorecard + Custom) --------------
          aesthetic_row(ns, "cols", "Cols",
                        selected = state$aesthetics$cols,
                        template_shows = c("trends", "scorecard",
                                           "custom"),
                        labels = c(trends    = "One card per",
                                   scorecard = "Cols",
                                   custom    = "Cols"),
                        facet = TRUE),
          # --- Popover panel (advanced; hidden by default) -------------
          # Color, intensity, and decoration aesthetics that are
          # orthogonal to the template choice live here so the main
          # panel stays focused on the recipe-relevant inputs.
          shiny::div(
            id = ns("popover"),
            class = "blockr-popover tb-popover",
            style = "display: none;",
            shiny::div(
              class = "blockr-popover-row",
              shiny::tags$label("Color by", class = "blockr-popover-label"),
              shiny::selectizeInput(
                ns("aes_color_by"), label = NULL, choices = NULL,
                width = "100%"
              )
            ),
            shiny::div(
              class = "blockr-popover-row",
              shiny::tags$label("Intensity", class = "blockr-popover-label"),
              tb_pill_group(
                ns("color_intensity"),
                choices = c("tint", "solid", "border"),
                selected = state$color$intensity,
                multi = FALSE,
                class = "tb-stat-pills"
              )
            ),
            shiny::div(
              class = "blockr-popover-row",
              shiny::tags$label("Unit", class = "blockr-popover-label"),
              shiny::selectizeInput(
                ns("aes_unit"), label = NULL, choices = NULL,
                width = "100%"
              )
            )
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
#'
#' @param template_shows Character vector of template names where this row
#'   should render. CSS hides the row when none match the active template.
#' @param labels Optional named character: `c(<template> = "Display label")`.
#'   When the active template's name is a key, that label replaces the
#'   default \u2014 the alternates are emitted as hidden spans toggled by CSS.
aesthetic_row <- function(ns, name, label, multi = FALSE, selected = NULL,
                          template_shows = NULL, facet = FALSE,
                          labels = NULL) {
  classes <- c(
    "tb-aes-row",
    if (facet) "tb-facet-row"
  )
  label_node <- if (length(labels) > 0) {
    tb_template_label(labels)
  } else {
    shiny::tags$label(
      label,
      `for` = ns(paste0("aes_", name)),
      class = "tb-aes-label"
    )
  }
  shiny::div(
    class = paste(classes, collapse = " "),
    `data-template-shows` = if (length(template_shows))
      paste(template_shows, collapse = " "),
    label_node,
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

#' Per-template aesthetic label.
#'
#' Renders one `<span data-label-for="<template>">` per template-key and
#' relies on CSS to show only the span matching the active template.
#' Used so a row can read "Aggregation" in the Numbers template and
#' "Stat" in Custom without a re-render.
#' @noRd
tb_template_label <- function(labels) {
  spans <- lapply(seq_along(labels), function(i) {
    shiny::tags$span(
      class = "tb-aes-label-text",
      `data-label-for` = names(labels)[i],
      labels[[i]]
    )
  })
  shiny::tags$label(
    class = "tb-aes-label tb-aes-label-multi",
    spans
  )
}

#' Inline gear-icon SVG (single Lucide-style sprocket).
#' @noRd
tb_gear_svg <- function() {
  paste0(
    '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" ',
    'stroke="currentColor" stroke-width="2" stroke-linecap="round" ',
    'stroke-linejoin="round" aria-hidden="true">',
    '<circle cx="12" cy="12" r="3"/>',
    '<path d="M19.4 15a1.65 1.65 0 0 0 .33 1.82l.06.06a2 2 0 1 1-2.83 ',
    '2.83l-.06-.06a1.65 1.65 0 0 0-1.82-.33 1.65 1.65 0 0 0-1 1.51V21a2 ',
    '2 0 1 1-4 0v-.09A1.65 1.65 0 0 0 9 19.4a1.65 1.65 0 0 0-1.82.33l-.06.06',
    'a2 2 0 1 1-2.83-2.83l.06-.06a1.65 1.65 0 0 0 .33-1.82 1.65 1.65 0 0 ',
    '0-1.51-1H3a2 2 0 1 1 0-4h.09A1.65 1.65 0 0 0 4.6 9a1.65 1.65 0 0 0-.33',
    '-1.82l-.06-.06a2 2 0 1 1 2.83-2.83l.06.06a1.65 1.65 0 0 0 1.82.33H9a',
    '1.65 1.65 0 0 0 1-1.51V3a2 2 0 1 1 4 0v.09a1.65 1.65 0 0 0 1 1.51 ',
    '1.65 1.65 0 0 0 1.82-.33l.06-.06a2 2 0 1 1 2.83 2.83l-.06.06a1.65 ',
    '1.65 0 0 0-.33 1.82V9a1.65 1.65 0 0 0 1.51 1H21a2 2 0 1 1 0 4h-.09a',
    '1.65 1.65 0 0 0-1.51 1z"/></svg>'
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
    value = list(kind = NULL, digits = NULL),
    measure_labels = list()
  )
  color_defaults <- list(by = "", intensity = "tint")

  state$showcase   <- state$showcase %||% showcase
  state$aesthetics <- utils::modifyList(aes_defaults, state$aesthetics %||% list())
  state$stats      <- utils::modifyList(stat_defaults, state$stats %||% list())
  state$formats    <- utils::modifyList(fmt_defaults, state$formats %||% list())
  state$color      <- utils::modifyList(color_defaults, state$color %||% list())
  # Template is the user-facing recipe. If the caller didn't specify
  # one, infer it from the aesthetics they did populate so existing
  # state (e.g. the kpi_block substitutions in blockr.insurance) opens
  # in the simplest matching template instead of falling back to Custom.
  if (is.null(state$template)) {
    state$template <- infer_template(state)
  }
  state
}

#' @noRd
infer_template <- function(state) {
  sc  <- state$showcase %||% "number"
  aes <- state$aesthetics %||% list()
  has <- function(x) length(x) > 0 && nzchar(as.character(x)[1])
  if (sc == "spark" || has(aes$spark_value)) return("trends")
  if (sc == "progress" || has(aes$max))      return("progress")
  if (has(aes$rows) || has(aes$cols))        return("scorecard")
  if (has(aes$label))                        return("kpi_list")
  "numbers"
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
  showcase    <- attr(df, "showcase") %||% "number"
  card_layout <- attr(df, "card_layout") %||% "grid"
  intensity   <- attr(df, "color_intensity") %||% "tint"

  if (card_layout == "list") {
    return(render_tile_list(df, intensity))
  }

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
    make_card(row, showcase, has_rows || has_cols, intensity)
  })

  shiny::div(
    class = paste("tb-grid",
                  if (showcase == "spark") "tb-grid--spark" else NULL),
    cards
  )
}

#' Render a long tile frame as a single card with stacked rows.
#' Triggered when `label` is mapped without explicit row/col facets
#' and the showcase is `"number"`.
#' @noRd
render_tile_list <- function(df, intensity = "tint") {
  rows <- lapply(seq_len(nrow(df)), function(i) {
    tile_row_inline(df[i, ], intensity)
  })
  shiny::div(
    class = "tb-grid tb-grid--list",
    shiny::div(
      class = "tb-card tb-card--list",
      shiny::div(class = "tb-card-rows", rows)
    )
  )
}

#' One row inside a list-layout card.
#' @noRd
tile_row_inline <- function(row, intensity = "tint") {
  label_txt <- if (nzchar(row$.label)) row$.label else row$.measure
  val_text  <- format_value(row$.value, row$.format, row$.digits)

  target_span <- if (!is.na(row$.target)) {
    shiny::tags$span(
      class = "tb-card-row-target",
      "/ ", format_value(row$.target, row$.format, row$.digits)
    )
  }

  status_pill <- if (!is.na(row$.status) && nzchar(row$.status)) {
    shiny::tags$span(
      class = paste0("tb-status-pill tb-status--", tolower(row$.status)),
      row$.status
    )
  }

  unit_span <- if (nzchar(row$.unit) && !is.na(row$.unit)) {
    shiny::tags$span(class = "tb-card-row-unit", row$.unit)
  }

  attrs <- color_attrs(row$.color_key, intensity)
  shiny::div(
    class = paste("tb-card-row", attrs$class),
    style = attrs$style,
    shiny::span(class = "tb-card-row-label", label_txt),
    shiny::div(
      class = "tb-card-row-values",
      shiny::span(class = "tb-card-row-value", val_text),
      target_span,
      unit_span
    ),
    status_pill
  )
}

#' Resolve a per-row .color_key + intensity into card class + style.
#'
#' Returns NULL/no-op when the key is empty (color is unmapped).
#' Otherwise produces a CSS variable so the same class set works for any
#' palette slot (status, measure, or arbitrary categorical column).
#' @noRd
color_attrs <- function(key, intensity) {
  empty <- list(class = NULL, style = NULL)
  if (is.null(key) || is.na(key) || !nzchar(key)) return(empty)
  if (!intensity %in% c("tint", "solid", "border")) intensity <- "tint"
  hue <- hue_for(key)
  list(
    class = paste0("tb-color tb-color--", intensity, " tb-color-hue--", hue),
    style = NULL
  )
}

#' Map a color key to one of the named palette slots.
#'
#' Status keys ("ok"/"warn"/"bad") map to the existing semantic colors;
#' anything else hashes deterministically to one of 6 categorical hues so
#' the same value always lands on the same color across re-renders.
#' @noRd
hue_for <- function(key) {
  k <- tolower(as.character(key))
  if (k %in% c("ok", "good", "success", "pass")) return("ok")
  if (k %in% c("warn", "warning", "caution"))    return("warn")
  if (k %in% c("bad", "error", "fail", "danger")) return("bad")
  # Deterministic hash → cat-1 .. cat-6.
  h <- sum(utf8ToInt(as.character(key))) %% 6L + 1L
  paste0("cat-", h)
}

#' @noRd
make_card <- function(row, showcase, show_facet_hint, intensity = "tint") {
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

  attrs <- color_attrs(row$.color_key, intensity)
  shiny::div(
    class = paste("tb-card", attrs$class),
    style = attrs$style,
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
