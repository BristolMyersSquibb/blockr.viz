# What the chart sends the browser: one payload with the state as the spec
# writes it (viz_wire()), the rows the current mapping needs, a description
# of every column of the input (the gear picks from all of them), and
# `derived`: what R computes for the drawing and never takes back. The pieces
# are cached on exactly their inputs, so a setting that changes only the
# drawing reuses them.

chart_view <- function(kit, src) {
  spec <- kit$spec
  state <- kit$state
  revs <- kit$revs
  session <- kit$session
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
    ), columns = meta_cache(d, function() viz_wire_columns(d)))
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

# The chart text (viz_titles()): slots name the spec's arguments, the
# prepare script's declared values and the highlight.
chart_titles <- function(spec, v, d, auto, specs) {
  a <- script_title_args(viz_slot_values(spec, v), specs, v$script_values)
  attr(d, "blockr_highlight") <- chart_highlight_text(v$highlight)
  viz_titles(v, d, auto, a)
}

# An export can ask for a picture of the chart whether or not its panel was
# open: the browser draws it offscreen from this payload.
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

# What the downloads write (export-registry.R): the numbers the chart shows,
# one row per mark for the aggregated types and the rows for the rest; the
# browser's picture, when it has sent one; the resolved text. Read when a
# format is clicked.
chart_exhibit <- function(kit, src, view) {
  function() {
    v <- chart_state_values(kit$state)
    kinds <- src$kinds()
    data <- chart_xlsx_frame(
      src$plain(), chart_type = v$type, group = chart_category(v, kinds),
      color = v$color, facet = v$facet, func = v$func,
      value = chart_value_col(v, kinds), na_group = v$missing
    )
    ti <- view$titles()
    text <- function(band) if (!is.null(band)) band$text
    # The footer's {dataset} and {filters} are the input's: the numbers the
    # chart shows carry neither.
    input <- tryCatch(kit$data(), error = function(e) NULL)
    new_viz_exhibit(
      data, picture = chart_picture(kit$capture()),
      title = text(ti$title), subtitle = text(ti$subtitle),
      caption = text(ti$caption), kind = "chart",
      meta = list(dataset = if (is.data.frame(input)) dl_dataset(input),
                  source = input)
    )
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

# The browser's picture as a chart_capture, or NULL.
chart_picture <- function(cap) {
  if (is.null(cap) || inherits(cap, "chart_capture")) {
    return(cap)
  }
  new_chart_capture(chart_capture_decode(cap$png), cap$width, cap$height)
}

# What the Excel download holds: one row per mark for the aggregated types
# (the table block's engine), the rows themselves for the rest.
chart_xlsx_frame <- function(data, chart_type, group = NULL, color = NULL,
                             facet = NULL, func = "count", value = NULL,
                             na_group = "level") {
  if (!is.data.frame(data)) {
    return(NULL)
  }
  if (!isTRUE(chart_type %in% c("bar", "waterfall", "pie", "treemap")) ||
        !length(intersect(group, names(data)))) {
    return(data)
  }
  keys <- intersect(c(facet, group, color), names(data))
  func <- func %||% "count"
  summary <- if (identical(func, "count") || !length(value) ||
                   !value %in% names(data)) {
    list(func = "count", cols = character())
  } else {
    list(func = func, cols = value)
  }
  table_aggregate(data, unique(keys), list(summary),
                     na_group = na_group %||% "level")$data
}
