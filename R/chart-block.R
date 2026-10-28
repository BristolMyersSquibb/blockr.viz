#' @rdname new_chart_block
#' @export
# viz: long the formals mirror the spec, one argument per line
new_chart_block <- function(
    type = "bar",
    ...,
    category = NULL,
    value = NULL,
    func = "count",
    x = NULL,
    xend = NULL,
    y = NULL,
    color = NULL,
    facet = NULL,
    series = NULL,
    label = NULL,
    orientation = NULL,
    position = "stack",
    sort_by = NULL,
    sort_dir = NULL,
    value_labels = FALSE,
    format = "number",
    unit = "",
    y_transform = "identity",
    x_transform = "identity",
    tooltip = NULL,
    drill = NULL,
    line_width = 1,
    point_size = 1,
    connect = "monotone",
    value_lines = NULL,
    x_lines = NULL,
    ymin = NULL,
    ymax = NULL,
    linetype = NULL,
    x_ranges = NULL,
    legend = "bottom",
    highlight = NULL,
    smoother = "none",
    identity_line = FALSE,
    box_points = "none",
    summary = NULL,
    whiskers = NULL,
    connect_centers = FALSE,
    band_window = "adaptive",
    band_size = 45,
    band_min_n = 12L,
    id = NULL,
    ref_min = NULL,
    ref_max = NULL,
    waterfall_totals = NULL,
    bins = 30L,
    show_n = "none",
    missing = "level",
    percent_of = "facet",
    facet_scales = "fixed",
    facet_cols = getOption("blockr.viz.facet_cols"),
    download = TRUE,
    title = NULL,
    subtitle = NULL,
    caption = getOption("blockr.viz.default_caption"),
    drill_target = "",
    drill_table = "",
    script = NULL,
    script_values = list(),
    selection = NULL) {

  spec <- viz_block_spec("chart")
  given <- mget(intersect(as.character(names(match.call())[-1L]),
                          names(spec$args)))
  dots <- list(...)
  # blockr.core's block arguments pass through; any other name is an old
  # argument name, or unknown, which viz_check() refuses.
  old <- setdiff(names(dots), VIZ_CORE_BLOCK_ARGS)
  args <- legacy_ctor_args("chart", c(given, dots[old]))
  values <- viz_check(spec, args)
  if (!"ctor" %in% names(dots)) chart_check_roles(values)
  # In code a mismatched pair is a mistake (chart-normalize.R's header).
  if (length(values$ymin) != length(values$ymax)) {
    stop("`ymin` and `ymax` must name the same number of columns.",
         call. = FALSE)
  }
  values <- chart_normalize(values)
  # blockr.core reads the starting state from this frame by argument name.
  list2env(values, environment())

  res <- blockr.core::new_transform_block(
    server = function(id, data) chart_server(id, data, values),
    ui = chart_ui,
    dat_valid = validate_annotated_df_input,
    allow_empty_state = viz_empty_ok(spec),
    external_ctrl = viz_ctrl_args(spec),
    expr_type = "bquoted",
    class = "chart_block",
    ...
  )
  # blockr.core keeps `...` as attributes and would save the old names back
  # into every board.
  for (nm in old) attr(res, nm) <- NULL
  res
}

# In code, an aggregated type mapped with `x`/`y` (the ggplot2 habit) and no
# `category` is a mistake worth naming. A restored board may hold `x`/`y`
# from a scatter it once was, so the check is for code only.
chart_check_roles <- function(values) {
  type <- values$type %||% "bar"
  if (type %in% CHART_AGGREGATED_TYPES && is.null(values$category) &&
        (length(values$x) || length(values$y))) {
    stop("new_chart_block(): a ", type, " takes `category` and `value`; ",
         "`x` and `y` are for scatter, line, area, band, histogram and ",
         "gantt.", call. = FALSE)
  }
  invisible()
}

chart_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    viz_echarts_dep(),
    drilldown_chart_dep(),
    viz_output(ns("chart"), "chart")
  )
}
