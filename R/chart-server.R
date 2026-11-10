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
      shown <- chart_view_selection(kit)
      own <- chart_expr(data, kit$state, src, shown)
      # Drill on and no target: the view's selection is the chart's own, so
      # its filter is the block's.
      rows_expr <- shiny::reactive({
        latched <- !kit$transient() && nzchar(kit$state$drill() %||% "")
        if (latched) expr() else own()
      })
      dv <- chart_data_view(kit, src, rows_expr, shown)
      list(
        payload = view$payload,
        expr = expr,
        exhibit = chart_exhibit(kit, src, view),
        probe = chart_exhibit_probe(kit, src),
        tables = dv$tables,
        tables_shape = dv$shape,
        data_view = dv$derived,
        normalize = function(v) {
          chart_normalize(v, kit$values())
        },
        claims = chart_claims(kit, src),
        script = src$prep
      )
    }
  )
}

# The arguments the chart's data side does not read: a click and where it
# goes. Changing one recomputes nothing the chart draws from.
CHART_FACE_ARGS <- c("selection", "drill_target", "drill_table")

# The data the chart draws: the input, coerced; the prepare script and its
# output (`prep`, viz_script_sources()); and the frame expanded by group
# definitions, with a histogram's bin column. `values` are the settings all
# of that reads (viz_values_but()).
chart_sources <- function(data, state) {
  src <- list()
  src$values <- viz_values_but(state, CHART_FACE_ARGS)
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
    chart_hist(src$values(),
               tryCatch(src$prep$prepared(), error = function(e) NULL))
  )
  src$plain <- shiny::reactive({
    v <- src$values()
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
  facet <- c(v$facet, v$facet_row)
  if (identical(v$type, "histogram")) {
    return(chart_split_roles(color = v$color, facet = facet,
                             series = v$series, x = v$x))
  }
  cat <- chart_category(v)
  if (!is.null(cat) || (v$type %||% "bar") %in% CHART_AGGREGATED_TYPES) {
    return(chart_split_roles(group = cat, color = v$color, facet = facet,
                             series = v$series))
  }
  chart_split_roles(color = v$color, facet = facet, series = v$series,
                    x = v$x, y = v$y)
}

# The emitted filter. A click selects source rows; with a prepare script the
# filter goes inside it, except a condition on a column only the script makes.
# With a drill target the chart filters nothing: a selection a saved board
# still holds (a brush latched before 0.3.0.9027) is ignored. The same object
# comes back while the code is unchanged, so blockr.core skips a
# re-evaluation.
#
# `held`, a function returning a selection, replaces the chart's own: the
# data view filters its Rows by what the chart shows as selected
# (chart_view_selection()).
chart_expr <- function(data, state, src, held = NULL) {
  if (is.null(held)) {
    held <- function() {
      target <- if (!is.null(state$drill_target)) state$drill_target()
      if (nzchar(target %||% "")) NULL else state$selection()
    }
  }
  members <- dd_group_filters_members(
    function() chart_selection_filters(held()), src$raw
  )
  last <- new.env(parent = emptyenv())
  shiny::reactive({
    d <- data()
    v <- src$values()
    v["selection"] <- list(held())
    parts <- chart_filter_parts(v, src, members)
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

# What the chart shows as selected, for its data view. Without a drill
# target, the chart's selection. With one, the last click or brush it sent,
# until the reader clicks it again or clicks empty plot space; what other
# blocks do to the target does not change it. With drill off, a brush only
# highlights: the browser sends it as `<view>_peek`, which nothing but the
# data view reads.
chart_view_selection <- function(kit) {
  peek <- shiny::reactiveVal(NULL)
  shiny::observeEvent(kit$input[[paste0(kit$view, "_peek")]], {
    peek(kit$input[[paste0(kit$view, "_peek")]]$selection)
  })
  shiny::observeEvent(kit$state$drill(), peek(NULL), ignoreInit = TRUE)
  shiny::reactive({
    if (!nzchar(kit$state$drill() %||% "")) {
      return(peek())
    }
    if (!kit$transient()) {
      return(kit$state$selection())
    }
    kit$claim()$selection
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
#
# With a target set, a clear (`selection` NULL) releases what this chart sent,
# and a range holds: a value filter takes values, not an interval, so a point
# or a brush with no drill column has nothing to send and must not clear
# either.
chart_claims <- function(kit, src) {
  shiny::reactive({
    d <- tryCatch(src$prep$prepared(), error = function(e) NULL)
    if (kit$transient()) {
      c1 <- kit$claim()
      if (is.null(c1)) {
        return(NULL)
      }
      if (identical(c1$selection$type, "range")) {
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
