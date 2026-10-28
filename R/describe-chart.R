# What the assistant is told about a chart block.
#
# A chart block's expr is only the click/brush filter, so its result is the
# upstream data frame and carries no trace of the chart: the mappings, the
# chart type and the drill filter are block arguments the browser consumes.
# `get_block_result` therefore cannot rescue a wrong description here the way it
# can for a data block. This method is the model's only account of what the
# user is looking at.
#
# The default method's `str()` of the constructor's 50-odd arguments is mostly
# NULL defaults, so this reports what is set, in chart vocabulary.

# Column-valued arguments, in the order a reader wants them.
chart_mapping_args <- function() {
  c("x", "y", "xend", "series", "color", "facet", "label", "ymin", "ymax",
    "id", "tooltip")
}

chart_ctor_defaults <- function() {
  fml <- formals(new_chart_block)
  fml <- fml[setdiff(names(fml), "...")]
  lapply(fml, function(d) tryCatch(eval(d), error = function(e) NULL))
}

# Scalars inline, vectors capped: a categorical filter can hold hundreds of
# levels and the whole description is capped downstream anyway.
fmt_chart_val <- function(x, max_n = 6L) {
  if (is.null(x) || (is.character(x) && length(x) == 1L && !nzchar(x))) {
    return(NULL)
  }
  if (is.list(x)) {
    x <- unlist(x, use.names = FALSE)
  }
  if (length(x) > max_n) {
    return(sprintf(
      "%s, ... (%d values)",
      paste(as.character(utils::head(x, max_n)), collapse = ", "), length(x)
    ))
  }
  paste(as.character(x), collapse = ", ")
}

chart_set_args <- function(state, nms) {
  vals <- lapply(state[intersect(nms, names(state))], fmt_chart_val)
  Filter(Negate(is.null), vals)
}

#' Describe a chart block for the assistant
#'
#' Reports what the chart is currently plotting rather than the constructor
#' argument dump the default method produces: the chart type, the column
#' mappings, the aggregation, and whether a drill filter is narrowing the data
#' it passes downstream.
#'
#' Falls back to the default method when no live state is supplied, which
#' happens for a block that was never built, or against a `blockr.assistant`
#' that does not yet pass state.
#'
#' @param x A `chart_block`.
#' @param board Board snapshot, for link metadata.
#' @param id Block id on the board.
#' @param ... Passed on.
#' @param state Live block state, or `NULL`. Argument order follows the
#'   generic, which takes it after the dots, so it is always named.
#'
#' @return Character vector of lines.
#'
#' @exportS3Method blockr.assistant::describe_block
#' @name describe_block.chart_block
describe_block.chart_block <- function(x, board, id, ..., state = NULL) {
  if (!length(state)) {
    return(NextMethod())
  }
  defaults <- chart_ctor_defaults()
  maps <- chart_set_args(state, chart_mapping_args())

  c(
    sprintf("Block id: %s", id),
    sprintf(
      "Chart block \"%s\"", coal_chr(attr(x, "block_name"), "<unnamed>")
    ),
    chart_type_line(state, defaults),
    if (length(maps)) {
      c(
        "Plots (column mappings):",
        sprintf("  %s = %s", names(maps), unlist(maps))
      )
    } else {
      "Plots: nothing mapped yet -- no column is assigned to any channel."
    },
    chart_value_line(state, defaults),
    chart_filter_lines(state),
    chart_chrome_lines(state),
    chart_other_lines(state, defaults),
    # The commonest cause of an empty chart is a mapped column the data no
    # longer has (renamed or dropped upstream). The renderer reports it inside
    # the canvas, where the assistant cannot see it, and this method has the
    # block arguments but not the data: so name the columns and say where to
    # check.
    if (length(maps)) {
      paste(
        "If the chart looks empty, check these mapped columns against the",
        "block's own data with get_block_result: a column renamed or dropped",
        "upstream leaves the block valid and the canvas blank."
      )
    },
    chart_ctrl_line(x),
    chart_link_lines(board, id)
  )
}

# The default method's line on which arguments the model may write; without it
# the model does not know which of them it is allowed to change. A chart
# declares nearly every one of its 50-odd arguments controllable, so spelling
# them out would cost more of the prompt cap than the rest of the description
# together and tell the model nothing it cannot get from the typed tool, which
# carries the full schema.
chart_ctrl_line <- function(x) {
  ctrl <- blockr.core::external_ctrl_vars(x)
  if (length(ctrl) > 12L) {
    return(sprintf(
      paste(
        "Modifiable via modify_block: all %d chart arguments, including",
        "every one named above (the typed modify_chart_block tool carries",
        "the full list with types)."
      ),
      length(ctrl)
    ))
  }
  sprintf(
    "Modifiable via modify_block: %s",
    if (identical(ctrl, "block_name")) {
      "block_name only"
    } else {
      paste(ctrl, collapse = ", ")
    }
  )
}

chart_type_line <- function(state, defaults) {
  typ <- coal_chr(state$type, defaults$type)
  extra <- c(
    if (!is.null(state$orientation)) as.character(state$orientation),
    if (!identical(state$position, defaults$position) &&
        !is.null(state$color)) {
      sprintf("position %s", state$position)
    }
  )
  paste0(
    sprintf("Chart type: %s", typ),
    if (length(extra)) sprintf(" (%s)", paste(extra, collapse = ", ")) else ""
  )
}

# Only the types that aggregate in the browser have a `func` worth reporting.
# Elsewhere (scatter, line, gantt, and the distribution marks, which summarize
# the raw values without a `func`) it sits at its default and means nothing, so
# reporting "count of row counts" on a scatter is worse than silence.
chart_value_line <- function(state, defaults) {
  typ <- coal_chr(state$type, defaults$type)
  func <- state$func
  if (!typ %in% c("bar", "histogram", "waterfall", "pie", "treemap") ||
        is.null(func)) {
    return(NULL)
  }
  # With both columns set and no orientation, which one is aggregated depends
  # on the column types, which this method does not see.
  if (length(state$x) && length(state$y) && is.null(state$orientation) &&
        !identical(func, "count") && typ %in% CHART_ORIENTED_TYPES) {
    return(sprintf(
      "Aggregation: %s of the numeric one of x (%s) and y (%s), by the other",
      func, state$x, state$y
    ))
  }
  roles <- chart_roles_internal(typ, state$x, state$y, func,
                                state$orientation)
  group <- coal_chr(roles$group, "nothing")
  if (is.null(roles$value) || identical(as.character(roles$value), ".count")) {
    return(sprintf("Aggregation: %s (row counts) by %s", func, group))
  }
  sprintf("Aggregation: %s of %s by %s", func, roles$value, group)
}

# The one piece of state that is neither creation-time config nor visible in
# the block's own arguments to a reader: a click or brush the user made, which
# silently narrows everything downstream.
chart_filter_lines <- function(state) {
  sel <- state$selection
  # The click filter keeps its columns apart: "AETERM = Rash; AESEV = MILD".
  set <- list(
    filters = fmt_chart_filters(chart_filters_state(sel$filters)),
    range = fmt_chart_val(sel$range),
    point = fmt_chart_val(sel$point)
  )
  set <- Filter(Negate(is.null), set)
  if (!length(set)) {
    return("Drill filter: none active (the block passes its input through).")
  }
  c(
    sprintf(
      "Drill filter: ACTIVE (%s) -- the data this block passes downstream is",
      coal_chr(sel$type, "categorical")
    ),
    "  narrowed by a click or brush in the chart, not by upstream code:",
    sprintf("  %s = %s", names(set), unlist(set))
  )
}

chart_chrome_lines <- function(state) {
  set <- chart_set_args(state, c("title", "subtitle", "caption"))
  if (length(set)) sprintf("%s: %s", names(set), unlist(set))
}

# Everything else that was moved off its default, named but not explained.
chart_other_lines <- function(state, defaults) {
  # `selection` is runtime interaction state, not creation-time config; it has
  # its own lines above.
  known <- c(chart_mapping_args(), "selection", "title", "subtitle", "caption",
             "type", "orientation", "position", "func")
  rest <- setdiff(intersect(names(state), names(defaults)), known)
  changed <- Filter(function(nm) !identical(state[[nm]], defaults[[nm]]), rest)
  if (!length(changed)) {
    return(NULL)
  }
  vals <- vapply(
    changed,
    function(nm) {
      sprintf("%s = %s", nm, coal_chr(fmt_chart_val(state[[nm]]), "NULL"))
    },
    character(1L)
  )
  c("Other non-default settings:", paste0("  ", vals))
}

chart_link_lines <- function(board, id) {
  links <- blockr.core::board_links(board)
  inc <- links[links$to == id]
  if (!length(inc)) {
    return("Incoming links: (none)")
  }
  c(
    "Incoming links:",
    sprintf("  %s <- %s (input: %s)", inc$id, inc$from, inc$input)
  )
}

coal_chr <- function(x, alt) {
  if (is.null(x) || !length(x) || is.na(x[[1L]])) alt else x[[1L]]
}
