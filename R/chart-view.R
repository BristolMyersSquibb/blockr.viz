# What the chart sends the browser: one payload with the state as the spec
# writes it (viz_wire()), the rows the current mapping needs, and `derived`:
# what R computes for the drawing and never takes back. See design.md
# section 3. The pieces are cached on exactly their inputs, so a setting that
# changes only the drawing reuses them.

chart_view <- function(spec, state, revs, src, session) {
  rows <- viz_rows()
  band_cache <- last_value_cache()
  smoother_cache <- last_value_cache()
  meta_cache <- last_value_cache()
  board_theme <- setup_drilldown_theme_sync(session)
  scale_map <- dd_board_scale_map()
  ctrl_choices <- dd_ctrl_choices()

  band <- shiny::reactive({
    v <- chart_state_values(state)
    d <- src$plain()
    a <- chart_band_args(v, d)
    if (is.null(a)) {
      return(NULL)
    }
    band_cache(list(d[chart_args_cols(a, d)], a, v[c("ref_min", "ref_max")]),
               function() chart_band(v, d, a))
  })
  smoother <- shiny::reactive({
    v <- chart_state_values(state)
    d <- src$plain()
    a <- chart_smoother_args(v, d)
    if (is.null(a)) {
      return(NULL)
    }
    smoother_cache(list(d[chart_args_cols(a, d)], a),
                   function() chart_smoother(v, d, a))
  })
  titles <- shiny::reactive({
    d <- src$plain()
    shiny::req(is.data.frame(d))
    inp <- tryCatch(src$input(), error = function(e) NULL)
    chart_titles(spec, chart_state_values(state), d,
                 input_display_attrs(if (is.list(inp)) inp$df else NULL),
                 src$specs())
  })
  shipped <- shiny::reactive({
    d <- src$plain()
    shiny::req(is.data.frame(d))
    cols <- chart_needed_cols(chart_state_values(state), d, src$kinds())
    d[, cols, drop = FALSE]
  })

  payload <- function() {
    d <- src$plain()
    shiny::req(is.data.frame(d))
    v <- chart_state_values(state)
    viz_payload(spec, v, rows(shipped()), revs, derived = list(
      column_meta = meta_cache(d, function() dd_col_meta(d)),
      titles = titles(),
      band = band(),
      smoother = smoother(),
      hist = src$hist()$edges,
      scales = chart_config_json(dd_scales_config(
        scale_map(), v$type, color = v$color,
        group = chart_category(v, src$kinds()), data = d
      )),
      palette = as.list(dd_palette()),
      theme = board_theme(),
      ctrl_choices = dd_ctrl_choices_list(ctrl_choices()),
      script = list(inputs = dd_script_roles(src$specs()),
                    values = dd_script_cfg(src$specs(), v$script_values),
                    error = src$run()$error),
      capture_ratio = canvas_capture_ratio()
    ))
  }
  list(payload = payload, titles = titles)
}

# The columns the payload carries: the mapped ones, and those a reader can
# switch a role to from the sentence, so the switch needs no new payload.
chart_needed_cols <- function(v, d, kinds) {
  # An aggregated chart draws its category and the column it aggregates; a
  # count aggregates none.
  axes <- if ((v$type %||% "bar") %in% setdiff(CHART_AGGREGATED_TYPES,
                                               CHART_DISTRIBUTION_TYPES)) {
    c(chart_category(v, kinds), chart_value_col(v, kinds))
  } else if (identical(v$type, "histogram")) {
    c(v$x, if (length(v$x)) chart_bin_col(v$x))
  } else {
    c(v$x, v$y)
  }
  needed <- c(
    axes, v$color, v$facet, v$series, v$xend, v$label, v$tooltip,
    v$drill, v$ymin, v$ymax, v$linetype, names(v$highlight), v$id,
    v$ref_min, v$ref_max,
    if (length(v$sort_by) && !v$sort_by %in% c("value", "data", "alpha",
                                               "onset")) v$sort_by,
    if (identical(v$x, "AVISIT")) "AVISITN"
  )
  if (length(v$subtitle) && grepl("@", v$subtitle, fixed = TRUE)) {
    needed <- c(needed, names(column_kinds(d)))
  }
  needed <- c(needed, unlist(
    template_slot_choices(list(v$title, v$subtitle, v$caption)),
    use.names = FALSE
  ))
  # Sorted, so swapping roles keeps the rows the browser holds.
  needed <- sort(unique(as.character(unlist(needed))))
  needed[nzchar(needed) & needed %in% names(d)]
}

# The chart text resolved against the data, and the pieces the face draws
# as controls. Slots name the spec's arguments (`{@color}`), plus the
# prepare script's declared values.
chart_titles <- function(spec, v, d, auto, specs) {
  a <- script_title_args(viz_slot_values(spec, v), specs, v$script_values)
  attr(d, "blockr_highlight") <- chart_highlight_text(v$highlight)
  parts <- list(
    title = block_title_parts(v$title, d, auto = auto$label, args = a),
    subtitle = block_title_parts(v$subtitle, d, auto = auto$subtitle,
                                 args = a),
    caption = block_title_parts(v$caption, d, auto = auto$caption, args = a)
  )
  list(
    title = resolve_block_title(v$title, d, auto = auto$label, args = a),
    subtitle = resolve_block_title(v$subtitle, d, auto = auto$subtitle,
                                   args = a),
    caption = resolve_block_title(v$caption, d, auto = auto$caption,
                                  args = a),
    parts = parts,
    offers = lapply(parts, function(p) as.list(attr(p, "offers"))),
    sentence_args = as.list(sentence_args(parts$title, parts$subtitle,
                                          parts$caption)),
    arg_values = lapply(stats::setNames(nm = names(a)), arg_token_value,
                        args = a),
    slot_choices = lapply(
      template_slot_choices(list(v$title, v$subtitle, v$caption)), as.list
    )
  )
}

# An export can ask for a picture of the chart whether or not its panel was
# ever open: the browser draws it offscreen from this payload.
chart_capture_service <- function(session, el, payload) {
  register_chart_capture(el, function(token, width, height) {
    msg <- tryCatch(shiny::isolate(payload()), error = function(e) NULL)
    if (is.null(msg)) {
      stop("chart block '", el, "' has no data to draw yet", call. = FALSE)
    }
    session$sendCustomMessage("blockr-viz:capture", c(
      msg, list(req = token, width = width, height = height)
    ))
  })
}

# The download menu: the numbers as Excel, the picture as png, web page or
# PowerPoint. The picture is the one the browser posted when the menu
# opened; without one the capture service draws it and the next click
# finds it.
chart_downloads <- function(output, session, state, src, view, capture,
                            data) {
  ns <- session$ns
  picture <- chart_picture(session, capture, data)
  formats <- shiny::reactive({
    if (!isTRUE(state$download())) {
      return(list())
    }
    Filter(function(s) isTRUE(s$ok), list(
      list(id = "dl_xlsx", ext = "xlsx", label = "Excel",
           ok = requireNamespace("openxlsx", quietly = TRUE)),
      list(id = "dl_html", ext = "html", label = "Web page", ok = TRUE),
      list(id = "dl_pptx", ext = "pptx", label = "PowerPoint",
           ok = requireNamespace("officer", quietly = TRUE)),
      list(id = "dl_png", ext = "png", label = "Image", ok = TRUE)
    ))
  })
  output$chart_download <- shiny::renderUI(dl_control_ui(ns, formats()))

  text <- function(key, auto_key) {
    v <- chart_state_values(state)
    inp <- tryCatch(src$input(), error = function(e) NULL)
    auto <- input_display_attrs(if (is.list(inp)) inp$df else NULL)
    resolve_block_title(v[[key]], src$plain(), auto = auto[[auto_key]],
                        args = viz_slot_values(viz_block_spec("chart"), v))
  }
  handler <- function(ext, what, write) {
    shiny::downloadHandler(
      filename = function() {
        dl_filename(tryCatch(dl_dataset(data()), error = function(e) NULL),
                    tryCatch(view$titles()$title, error = function(e) NULL),
                    "chart", ext)
      },
      content = function(file) dl_guard(what, write(file))
    )
  }
  output$dl_xlsx <- handler("xlsx", "Excel", function(file) {
    v <- chart_state_values(state)
    d <- chart_xlsx_frame(
      src$plain(), chart_type = v$type,
      group = chart_category(v, src$kinds()), color = v$color,
      facet = v$facet, func = v$func,
      value = chart_value_col(v, src$kinds()), na_group = v$missing
    )
    shiny::req(is.data.frame(d))
    write_annotated_xlsx(
      d, file, title = text("title", "label"),
      subtitle = text("subtitle", "subtitle"),
      caption = download_footer_caption(text("caption", "caption"), data(),
                                        session)
    )
  })
  output$dl_html <- handler("html", "web page", function(file) {
    write_exhibit_html(picture(), file, title = text("title", "label"),
                       subtitle = NULL)
  })
  output$dl_pptx <- handler("pptx", "PowerPoint", function(file) {
    write_exhibit_pptx(picture(), file, title = text("title", "label"))
  })
  output$dl_png <- handler("png", "image", function(file) {
    chart_capture_file(picture(), file)
  })
  # A link in a display:none host is a hidden output, which Shiny would
  # suspend: the click would get a 404.
  for (id in c("chart_download", "dl_xlsx", "dl_html", "dl_pptx", "dl_png")) {
    shiny::outputOptions(output, id, suspendWhenHidden = FALSE)
  }
}

# The aggregated column of an aggregated chart: the axis opposite the
# category. NULL for a count.
chart_value_col <- function(v, kinds) {
  cat <- chart_category(v, kinds)
  if (is.null(cat) || identical(v$func %||% "count", "count")) {
    return(NULL)
  }
  if (identical(cat, v$x)) v$y else v$x
}

# The browser's picture with the board's download footer, or, when there is
# none yet, a request to the capture service and an error that says so.
chart_picture <- function(session, capture, data) {
  token <- shiny::reactiveVal(NULL)
  shiny::observe({
    tok <- token()
    shiny::req(tok)
    cap <- tryCatch(chart_capture_collect(tok), error = function(e) {
      token(NULL)
      NULL
    })
    shiny::req(cap)
    token(NULL)
    if (is.null(shiny::isolate(capture()))) capture(cap)
  })
  function() {
    cap <- capture()
    if (!is.null(cap)) {
      if (!inherits(cap, "chart_capture")) {
        cap <- new_chart_capture(chart_capture_decode(cap$png), cap$width,
                                 cap$height)
      }
      return(download_footer_stamp(cap, data(), session))
    }
    if (is.null(shiny::isolate(token()))) {
      tok <- tryCatch(chart_capture_request(session$ns("chart"), 960, 540),
                      error = function(e) NULL)
      if (is.null(tok)) {
        stop("the chart has no data yet; open its view and try again",
             call. = FALSE)
      }
      token(tok)
    }
    stop("the chart has not been drawn yet; try again in a moment",
         call. = FALSE)
  }
}

# What the Excel download holds: one row per mark for the aggregated types,
# built by the table block's engine; the rows themselves for the rest.
chart_xlsx_frame <- function(data, chart_type, group = NULL, color = NULL,
                             facet = NULL, func = "count", value = NULL,
                             na_group = "level") {
  if (!is.data.frame(data)) {
    return(NULL)
  }
  keys <- intersect(c(facet, group, color), names(data))
  if (!isTRUE(chart_type %in% c("bar", "waterfall", "pie", "treemap")) ||
        !length(intersect(group, names(data)))) {
    return(data)
  }
  func <- func %||% "count"
  summaries <- if (identical(func, "count") || !length(value) ||
                     !value %in% names(data)) {
    list(list(func = "count", cols = character()))
  } else {
    list(list(func = func, cols = value))
  }
  dd_table_aggregate(data, unique(keys), summaries,
                     na_group = na_group %||% "level")$data
}
