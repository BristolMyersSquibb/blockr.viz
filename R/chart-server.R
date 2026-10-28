# The chart block's server, on the kit (viz_block_server()). The kit holds the
# state, one reactiveVal per spec argument under the public names, and the
# observers that take the gear's settings, clicks and pictures into it. The
# chart adds reactives that read the state: the data it draws and the filter it
# emits (here), the payload and the exhibit for the downloads (chart-view.R),
# the data view (chart-data-view.R), and the claims a drill target receives.

chart_server <- function(id, data, init) {
  viz_block_server(
    id, viz_block_spec("chart"), data, init, "chart",
    function(kit) {
      src <- chart_sources(data, kit$state)
      view <- chart_view(kit, src)
      chart_capture_service(kit$session, kit$el, view$payload)
      expr <- chart_expr(data, kit$state, src)
      dv <- chart_data_view(kit, src, expr)
      list(
        payload = view$payload,
        expr = expr,
        exhibit = chart_exhibit(kit, src, view, dv),
        data_view = dv$derived,
        normalize = function(v) {
          chart_normalize(v, viz_values(kit$state))
        },
        claims = chart_claims(kit, src),
        script = src$prep
      )
    }
  )
}

# The data the chart draws: the input, coerced; the prepare script and its
# output (`prep`, viz_script_sources()); and the frame expanded by group
# definitions, with a histogram's bin column.
chart_sources <- function(data, state) {
  src <- list()
  src$input <- shiny::reactive({
    d <- data()
    if (is.data.frame(d)) {
      list(df = d, coerced = FALSE)
    } else {
      list(df = tryCatch(as_annotated_df(d), error = function(e) NULL),
           coerced = TRUE)
    }
  })
  # A coerced frame loses its annotation columns and keeps its filter trail.
  src$raw <- shiny::reactive({
    inp <- src$input()
    d <- inp$df
    if (!is.data.frame(d) || !inp$coerced) {
      return(if (is.data.frame(d)) d)
    }
    out <- d[, !names(d) %in% annotation_cols_in(d), drop = FALSE]
    attr(out, "blockr_filters") <- attr(d, "blockr_filters", exact = TRUE)
    out
  })
  src$prep <- viz_script_sources(src$raw, state)
  src$hist <- shiny::reactive(
    chart_hist(viz_values(state),
               tryCatch(src$prep$prepared(), error = function(e) NULL))
  )
  src$plain <- shiny::reactive({
    v <- viz_values(state)
    d <- expand_role_groups(src$prep$prepared(),
                            chart_split_cols(v))
    chart_hist_data(d, chart_hist(v, d))
  })
  src
}

# The category column of an aggregated chart. NULL on the other types.
chart_category <- function(v) {
  if ((v$type %||% "bar") %in% CHART_AGGREGATED_TYPES) v$category
}

# The columns whose group definition splits the rows: the category and the
# splitting roles; x and y themselves on a chart that draws rows.
chart_split_cols <- function(v) {
  if (identical(v$type, "histogram")) {
    return(chart_split_roles(color = v$color, facet = v$facet,
                             series = v$series, x = v$x))
  }
  cat <- chart_category(v)
  if (!is.null(cat) || (v$type %||% "bar") %in% CHART_AGGREGATED_TYPES) {
    return(chart_split_roles(group = cat, color = v$color, facet = v$facet,
                             series = v$series))
  }
  chart_split_roles(color = v$color, facet = v$facet, series = v$series,
                    x = v$x, y = v$y)
}

# The emitted filter. A click selects source rows; with a prepare script the
# filter goes inside it, except a condition on a column only the script makes.
# The same object comes back while the code is unchanged, so blockr.core skips
# a re-evaluation.
chart_expr <- function(data, state, src) {
  members <- dd_group_filters_members(
    function() chart_selection_filters(state$selection()), src$raw
  )
  last <- new.env(parent = emptyenv())
  shiny::reactive({
    d <- data()
    parts <- chart_filter_parts(viz_values(state), src, members)
    sx <- src$prep$compiled()
    names_in <- if (!is.null(sx) && length(parts$conds)) {
      tryCatch(names(src$raw()), error = function(e) NULL)
    }
    post <- dd_drill_post(parts$cols, names_in)
    ex <- chart_filter_call(parts$conds[!post])
    if (!is.null(d) && !is.data.frame(d)) ex <- wrap_plain_df_input(ex)
    out <- if (is.null(sx)) ex else dd_splice_slot(sx, ex)
    if (any(post)) {
      out <- dd_splice_slot(chart_filter_call(parts$conds[post]), out)
    }
    if (!identical(out, last$expr)) last$expr <- out
    last$expr
  })
}

# The categorical part of a selection: a named list of character vectors,
# NA for a missing value.
chart_selection_filters <- function(sel) {
  f <- if (is.list(sel)) sel$filters
  if (!length(f)) {
    return(NULL)
  }
  lapply(f, function(v) as.character(unlist(v)))
}

# The selection's conditions and the columns each reads. A click on a
# histogram bin is a range on x, since the bin column exists only in what the
# chart draws.
chart_filter_parts <- function(v, src, members) {
  sel <- v$selection
  filters <- chart_selection_filters(sel)
  bin <- NULL
  if (identical(v$type, "histogram") && length(v$x)) {
    col <- chart_bin_col(v$x)
    if (col %in% names(filters)) {
      hist <- tryCatch(src$hist(), error = function(e) NULL)
      bin <- chart_bin_cond(v$x, filters[[col]], hist$edges)
      filters[[col]] <- NULL
    }
  }
  conds <- c(if (length(filters)) chart_filter_conds(filters, members(filters)),
             if (!is.null(bin)) list(bin))
  cols <- c(as.list(names(filters)), if (!is.null(bin)) list(v$x))
  if (!identical(sel$type, "range")) {
    return(list(conds = as.list(conds), cols = cols))
  }
  rng <- list(x_col = sel$x, y_col = sel$y, x_range = sel$x_range,
              y_range = sel$y_range)
  rc <- chart_range_cond(rng, tryCatch(src$raw(), error = function(e) NULL))
  if (is.null(rc)) {
    return(list(conds = list(), cols = list()))
  }
  list(conds = c(list(rc), conds),
       cols = c(list(c(sel$x, if (!is.null(sel$y_range)) sel$y)), cols))
}

# What a drill target receives: the click's conditions. With a target set a
# click is an event (the kit's `claim`); without, the held selection is the
# claim. The kit sends them.
chart_claims <- function(kit, src) {
  shiny::reactive({
    d <- tryCatch(src$prep$prepared(), error = function(e) NULL)
    if (kit$transient()) {
      c1 <- kit$claim()
      if (is.null(c1)) {
        return(NULL)
      }
      filters <- chart_selection_filters(c1$selection)
    } else {
      sel <- kit$state$selection()
      filters <- if (!identical(sel$type, "range")) {
        chart_selection_filters(sel)
      }
    }
    ctrl_claims(d, kit$state$drill_table(), filters %||% list(),
                   subject = ctrl_subject(kit$session))
  })
}
