#' Report Call for a Block's Printed Result
#'
#' How a block's result should be printed in a rendered document (a
#' blockr.outline report or deck). Returns a call object over `var`, the
#' variable the generated script binds the block's result to -- e.g.
#' `blockr.viz::static_chart(chart1, chart_type = "bar", group = "ARM")` -- or
#' `NULL` when the bare result prints fine as-is. Consumers (blockr.outline)
#' deparse the call into the document, so it must be self-qualified and
#' contain only literal values.
#'
#' The chart block's method rebuilds the chart server-side through
#' [static_chart()]: the interactive chart draws client-side, so its result is
#' the (filtered) data and a rendered document would otherwise print a bare
#' data frame. Only print-relevant state is emitted (mappings, layout,
#' titles, helper lines); interaction-only state (tooltips, drill, zoom,
#' runtime filter transports) is not part of the printed chart. Arguments
#' at their defaults are omitted, so the emitted call stays readable.
#'
#' @param x A block object.
#' @param var The variable name (string) holding the block's result in the
#'   generated document.
#' @param ... Reserved.
#'
#' @return A call, or `NULL` for the bare print.
#'
#' @export
report_call <- function(x, var, ...) {
  UseMethod("report_call")
}

#' @rdname report_call
#' @export
report_call.default <- function(x, var, ...) {
  NULL
}

#' @rdname report_call
#' @export
report_call.chart_block <- function(x, var, ...) {

  # The committed block's state lives in its constructor closure (the same
  # values serialization reads); absent names simply fall away.
  env <- environment(x[["expr_server"]])

  state <- function(nm) {
    v <- get0(nm, envir = env, ifnotfound = NULL)
    if (is.function(v)) NULL else v
  }

  # Print-relevant surface, in static_chart() signature order. Defaults mirror
  # static_chart()'s formals: a value equal to its default is dropped.
  spec <- list(
    chart_type = "bar",
    group = NULL,
    color = NULL,
    facet = NULL,
    value = ".count",
    func = "count",
    x = NULL,
    y = NULL,
    series = NULL,
    bar_mode = "stacked",
    orientation = "horizontal",
    sort_by = "value",
    sort_dir = NULL,
    count_on = "off",
    count_col = NULL,
    box_points = "none",
    smoother = "none",
    identity_line = FALSE,
    lo = NULL,
    hi = NULL,
    step = NULL,
    vlines = NULL,
    hlines = NULL,
    line_width_mult = 1,
    dot_size_mult = 1
  )

  args <- list()

  for (nm in names(spec)) {

    v <- state(nm)

    if (is.null(v) || (is.character(v) && !any(nzchar(v)))) {
      next
    }

    if (is.numeric(v)) {
      v <- as.numeric(v)
    }

    if (identical_default(v, spec[[nm]])) {
      next
    }

    args[[nm]] <- v
  }

  # Title band: NULL means the automatic tier (omit), "" suppresses (keep),
  # a template string resolves at render time (keep).
  for (nm in c("title", "subtitle", "caption")) {
    v <- state(nm)
    if (!is.null(v)) {
      args[[nm]] <- as.character(v)[1L]
    }
  }

  as.call(c(
    list(call("::", as.name("blockr.viz"), as.name("static_chart")),
         as.name(var)),
    args
  ))
}

# Default comparison that tolerates the length-1 / scalar and int / dbl
# drift a round-trip through state can introduce.
identical_default <- function(v, default) {

  if (is.null(default)) {
    return(FALSE)
  }

  length(v) == 1L && length(default) == 1L &&
    !is.na(v) && identical(as.character(v), as.character(default))
}
