# How the chart recordings talk to the reference implementation
# (feat/viz-0.3.0 @ 8651d92). Everything that knows the reference's names
# and its browser messages is in this file. A rewrite that speaks the public
# names replaces `record_target()` and keeps the recordings.

# The names of the recordings are those of api-proposal.md. One entry per
# renamed argument (`old`) or renamed value (`values`, new = old).
record_renames <- list(
  percent_of = list(old = "pct_of"),
  ref_min = list(old = "ref_lo"),
  ref_max = list(old = "ref_hi"),
  y_transform = list(old = "y_scale",
                     values = c(identity = "linear", log10 = "log")),
  x_transform = list(old = "x_scale",
                     values = c(identity = "linear", log10 = "log")),
  show_n = list(values = c(none = "off")),
  connect = list(values = c(step_start = "step-start",
                            step_middle = "step-middle",
                            step_end = "step-end"))
)

# Public (new) arguments to the reference's.
record_to_old <- function(args) {
  for (nm in intersect(names(args), names(record_renames))) {
    r <- record_renames[[nm]]
    v <- args[[nm]]
    if (!is.null(r$values) && is.character(v)) {
      hit <- v %in% names(r$values)
      v[hit] <- unname(r$values[v[hit]])
    }
    args[nm] <- list(v)
    if (!is.null(r$old)) names(args)[names(args) == nm] <- r$old
  }
  args
}

# The reference's state (or any list keyed by its argument names) to the
# public names and values.
record_to_new <- function(x) {
  for (nm in names(record_renames)) {
    r <- record_renames[[nm]]
    old <- r$old %||% nm
    if (!old %in% names(x)) next
    v <- x[[old]]
    if (!is.null(r$values) && is.character(v)) {
      hit <- v %in% r$values
      v[hit] <- names(r$values)[match(v[hit], r$values)]
    }
    x[old] <- list(v)
    names(x)[names(x) == old] <- nm
  }
  x
}

record_target <- function() {
  list(
    name = "reference",
    ctor = function(args, sc = NULL) {
      eval(as.call(c(list(quote(new_chart_block)), record_to_old(args))))
    },
    state = record_to_new,
    inputs = record_ref_inputs,
    payload = function(msgs, expr, cx) record_ref_payload(msgs),
    capture = function(x) list(drilldown_block_capture = x),
    wire = TRUE
  )
}

# A step to the browser inputs that carry it, as list(name = value). `cx`
# holds the current public state, the current data and a click counter.
record_ref_inputs <- function(step, cx) {
  act <- function(msg) list(drilldown_block_action = msg)
  switch(step$kind,
    set = record_ref_set(step$args, cx),
    click = act(c(list(action = "filter", filter_type = "categorical",
                       filters = record_ref_filters(step$args)),
                  list(nonce = cx$nonce))),
    point = act(list(
      action = "filter", filter_type = "point",
      x_col = step$x_col, y_col = step$y_col,
      x_val = record_ref_value(step$x), y_val = record_ref_value(step$y)
    )),
    brush = act(if (is.null(step$x_col)) {
      list(action = "filter", filter_type = "range")
    } else {
      list(
        action = "filter", filter_type = "range",
        x_col = step$x_col, y_col = step$y_col,
        x_range = as.list(record_ref_value(step$x_range)),
        y_range = if (!is.null(step$y_range)) {
          as.list(record_ref_value(step$y_range))
        },
        filters = if (!is.null(step$panel)) {
          record_ref_filters(step$panel)
        }
      )
    }),
    wire = step$inputs
  )
}

# Dates and times travel as ms since the epoch.
record_ref_value <- function(v) {
  if (inherits(v, "Date")) v <- as.POSIXct(v, tz = "UTC")
  if (inherits(v, "POSIXct")) return(as.numeric(v) * 1000)
  v
}

# A missing value is a JSON null.
record_ref_filters <- function(f) {
  lapply(f, function(v) lapply(v, function(x) if (is.na(x)) NULL else x))
}

# Settings to the gear's config message (and set_mults for the two sizes),
# in the browser's names and shapes.
record_ref_set <- function(a, cx) {
  a <- record_to_old(a)
  msg <- list(action = "config")
  put <- function(key, v) {
    msg[key] <- list(v)
    msg
  }
  col <- function(v) if (is.null(v)) "" else v
  on_off <- function(v) if (isTRUE(v)) "on" else "off"
  lines <- function(v) {
    if (is.null(v)) return("")
    nm <- names(v) %||% rep("", length(v))
    paste(ifelse(nzchar(nm), paste0(nm, ": ", v), v), collapse = "; ")
  }
  roles <- c("type", "x", "y", "func", "orientation")
  if (any(roles %in% names(a))) {
    cur <- cx$state
    for (nm in roles) if (nm %in% names(a)) cur[nm] <- list(a[[nm]])
    d <- cx$data
    r <- chart_roles_internal(cur$type, cur$x, cur$y, cur$func,
                              cur$orientation,
                              if (is.data.frame(d)) viz_column_kinds(d))
    msg <- put("chart_type", cur$type)
    msg <- put("func", cur$func)
    msg <- put("group", col(r$group))
    msg <- put("value", col(r$value))
    msg <- put("x", col(r$x))
    msg <- put("y", col(r$y))
    msg <- put("orientation", col(r$orientation))
  }
  # The gear always sends the roles and every key the reference reads with
  # `$` that is a prefix of another key (`x` of `x_scale`, `facet` of
  # `facet_cols`, ...); without them `$` matches the longer key. Sent at
  # their current values, as the browser last got them, they change nothing.
  if (!any(roles %in% names(a))) {
    cfg <- record_ref_last_config(cx$sent)
    msg <- put("chart_type", cfg$chart_type)
    msg <- put("func", cfg$func)
    for (k in c("group", "value", "x", "y", "orientation")) {
      msg <- put(k, col(cfg[[k]]))
    }
  }
  anchors <- list(facet = "facet", connect = "connect", hi = "ymax")
  cfg <- record_ref_last_config(cx$sent)
  for (k in names(anchors)) {
    if (!anchors[[k]] %in% names(a) && !k %in% names(a)) {
      v <- cfg[[k]]
      msg <- put(k, if (is.null(v) || !length(v)) "" else v)
    }
  }
  same <- c("color", "facet", "series", "xend", "label", "drill", "sort_by",
            "linetype", "ref_lo", "ref_hi")
  for (nm in intersect(names(a), same)) msg <- put(nm, col(a[[nm]]))
  plain <- c("sort_dir", "format", "unit", "y_scale", "x_scale", "bins",
             "connect", "smoother", "box_points", "summary", "whiskers",
             "band_window", "band_size", "band_min_n", "facet_scales",
             "legend", "title", "subtitle", "caption", "script")
  for (nm in intersect(names(a), plain)) msg <- put(nm, a[[nm]])
  for (nm in intersect(names(a), c("value_labels", "identity_line",
                                   "connect_centers", "download"))) {
    msg <- put(nm, on_off(a[[nm]]))
  }
  if ("position" %in% names(a)) {
    msg <- put("bar_mode", chart_position_internal(a$position))
  }
  if ("pct_of" %in% names(a)) msg <- put("pct_of", chart_pct_of_internal(a$pct_of))
  if ("tooltip" %in% names(a)) msg <- put("tt_fields", as.list(a$tooltip))
  if ("y_lines" %in% names(a)) msg <- put("value_lines", lines(a$y_lines))
  if ("x_lines" %in% names(a)) msg <- put("x_lines", lines(a$x_lines))
  if ("x_ranges" %in% names(a)) {
    msg <- put("x_ranges", unname(Map(function(nm, r) {
      list(name = nm, from = r[[1L]], to = r[[2L]])
    }, names(a$x_ranges), a$x_ranges)))
  }
  if ("highlight" %in% names(a)) {
    h <- a$highlight
    msg <- put("highlight_by", if (length(h)) names(h)[[1L]] else "")
    msg <- put("highlight", if (length(h)) as.list(h[[1L]]) else list())
  }
  if ("ymin" %in% names(a)) msg <- put("lo", col(a$ymin))
  if ("ymax" %in% names(a)) msg <- put("hi", col(a$ymax))
  if ("waterfall_totals" %in% names(a)) {
    msg <- put("waterfall_totals", as.list(a$waterfall_totals))
  }
  if ("show_n" %in% names(a)) msg <- put("count_on", a$show_n)
  if ("missing" %in% names(a)) msg <- put("na_group", a$missing)
  if ("id" %in% names(a)) {
    msg <- put("band_id", col(a$id))
    msg <- put("count_col", col(a$id))
  }
  if ("facet_cols" %in% names(a)) {
    msg <- put("facet_cols", if (is.null(a$facet_cols)) "" else
      as.character(a$facet_cols))
  }
  if ("drill_target" %in% names(a)) msg <- put("ctrl_target", a$drill_target)
  if ("drill_table" %in% names(a)) msg <- put("ctrl_table", a$drill_table)
  for (nm in names(a$script_values)) {
    msg <- put(paste0("sv_", nm), as.character(a$script_values[[nm]]))
  }
  out <- if (length(msg) > 1L) list(drilldown_block_action = msg)
  mults <- list(action = "set_mults")
  if ("line_width" %in% names(a)) mults$line_width_mult <- a$line_width
  if ("point_size" %in% names(a)) mults$dot_size_mult <- a$point_size
  if (length(mults) > 1L) {
    out <- c(out, list(drilldown_block_action = mults))
  }
  out
}

# The config of the last payload the browser got.
record_ref_last_config <- function(msgs) {
  cfg <- list()
  for (m in msgs) {
    if (identical(m[["type"]], "drilldown-data")) cfg <- m[["message"]][["config"]]
  }
  cfg
}

# Browser config keys that carry a public setting under another name.
record_ref_config_names <- c(
  chart_type = "type", bar_mode = "position", pct_of = "percent_of",
  y_scale = "y_transform", x_scale = "x_transform", tt_fields = "tooltip",
  value_lines = "y_lines", lo = "ymin", hi = "ymax",
  line_width_mult = "line_width", dot_size_mult = "point_size",
  count_on = "show_n", na_group = "missing", ref_lo = "ref_min",
  ref_hi = "ref_max", ctrl_target = "drill_target",
  ctrl_table = "drill_table", count_col = "id"
)

# Keys that only exist on the wire: the internal roles (the state has the
# public x / y / orientation), the click state (the state has `selection`)
# and transport flags.
record_ref_config_wire_only <- c(
  "group", "value", "x", "y", "orientation", "filter_type", "filters",
  "filter_range", "filter_column", "filter_values", "band_id",
  "value_line_names", "x_line_names", "highlight_by", "baseline",
  "capture_export", "capture_ratio"
)

# What the browser got, in public terms: the rows (when they travelled),
# the column metadata, the settings as drawn and the computed pieces.
record_ref_payload <- function(messages) {
  out <- list()
  for (m in messages) {
    if (!identical(m[["type"]], "drilldown-data")) next
    msg <- m[["message"]]
    cfg <- msg[["config"]]
    on <- c("value_labels", "identity_line", "connect_centers", "download")
    for (nm in intersect(on, names(cfg))) cfg[[nm]] <- identical(cfg[[nm]], "on")
    if (!is.null(cfg$value_lines)) {
      cfg$value_lines <- record_ref_named(cfg$value_lines, cfg$value_line_names)
    }
    if (!is.null(cfg$x_lines)) {
      cfg$x_lines <- record_ref_named(cfg$x_lines, cfg$x_line_names)
    }
    cfg$highlight <- if (length(cfg$highlight_by) && length(cfg$highlight)) {
      stats::setNames(list(unlist(cfg$highlight)), cfg$highlight_by)
    }
    cfg$bar_mode <- chart_position_public(cfg$bar_mode)
    cfg$pct_of <- chart_pct_of_public(cfg$pct_of)
    cfg <- cfg[setdiff(names(cfg), record_ref_config_wire_only)]
    hit <- names(cfg) %in% names(record_ref_config_names)
    names(cfg)[hit] <- record_ref_config_names[names(cfg)[hit]]
    out <- c(out, list(list(
      rows = record_ref_rows(msg[["data"]]),
      columns = msg[["columns"]],
      config = record_to_new(cfg)
    )))
  }
  out
}

record_ref_named <- function(v, nm) {
  v <- unlist(v)
  nm <- unlist(nm)
  if (length(nm) == length(v)) names(v) <- nm
  v
}

# The rows from their JSON, dictionary-encoded columns decoded.
record_ref_rows <- function(json) {
  if (is.null(json)) return(NULL)
  cols <- jsonlite::fromJSON(json, simplifyVector = TRUE)
  lapply(cols, function(v) {
    if (is.list(v) && identical(v[["__enc__"]], "dict")) {
      v$levels[v$codes + 1L]
    } else {
      v
    }
  })
}

# position (public) <-> bar_mode (internal)
CHART_POSITIONS <- c(stack = "stacked", dodge = "grouped", fill = "percent")

# pct_of values (public -> internal); "total" is no role: one denominator.
CHART_PCT_OF <- c(facet = "facet", category = "group", color = "color",
                  total = "total")

chart_position_public <- function(bar_mode) {
  names(CHART_POSITIONS)[match(bar_mode %||% "stacked", CHART_POSITIONS)] %|NA|%
    "stack"
}

# Unknown values pass through, so the constructor can refuse them.
chart_pct_of_public <- function(pct_of) {
  pct_of <- pct_of %||% "facet"
  out <- names(CHART_PCT_OF)[match(pct_of, CHART_PCT_OF)]
  ifelse(is.na(out), pct_of, out)
}
