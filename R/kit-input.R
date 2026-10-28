# What the browser sends a block, read and checked on arrival.
#
# Each block view sends typed inputs: `<id>_set` (the settings that
# changed), `<id>_select` (a click or brush), `<id>_capture` (a picture of
# the view) and, with a data view, `<id>_data` (whether its table is open). Shiny calls the handler registered for the input's type before
# any observer sees the value; a handler returns NULL for anything malformed,
# which observeEvent() ignores.

#' Read a settings message
#'
#' @param spec The block's `viz_spec`.
#' @param x The message as Shiny decoded it: `{rev, values: {name: value}}`.
#' @return `list(rev, values, rejected)`, or `NULL` when `x` is not a
#'   settings message. `values` holds the checked values; `rejected` names
#'   the keys that are not in the spec or failed their check.
#' @noRd
viz_read_set <- function(spec, x) {
  if (!is.list(x) || !is_whole_count(x$rev) || !is.list(x$values) ||
        (length(x$values) && is.null(names(x$values)))) {
    return(NULL)
  }
  values <- list()
  rejected <- setdiff(names(x$values), names(spec$args))
  for (nm in intersect(names(x$values), names(spec$args))) {
    v <- try_check_value(spec$args[[nm]], x$values[[nm]], nm)
    if (is.null(v)) {
      rejected <- c(rejected, nm)
    } else {
      values[nm] <- v
    }
  }
  list(rev = as.integer(x$rev), values = values, rejected = rejected)
}

is_whole_count <- function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x) && x >= 0 && x == round(x)
}

#' Read a selection message
#'
#' One shape for the chart and every table layout:
#' `{selection: {type: "categorical", filters: {col: [value or null]},
#' columns?: {col: [...]}}, nonce}`, `{selection: {type: "range", x, y?,
#' x_range: [lo, hi], y_range?: [lo, hi], filters?}, nonce}`, or
#' `{selection: null, nonce}` to clear.
#'
#' @return `list(selection, nonce)`, `selection` `NULL` for a clear, or
#'   `NULL` when `x` is malformed. Filter values are character vectors, a
#'   missing value as `NA`.
#' @noRd
viz_read_selection <- function(x) {
  if (!is.list(x) || !is_whole_count(x$nonce)) {
    return(NULL)
  }
  s <- x$selection
  # A double: the browser may send Date.now(), past the integer range.
  nonce <- as.numeric(x$nonce)
  if (is.null(s)) {
    return(list(selection = NULL, nonce = nonce))
  }
  out <- switch(
    if (is.character(s$type) && length(s$type) == 1L) s$type else "",
    categorical = read_categorical(s),
    range = read_range(s),
    NULL
  )
  if (is.null(out)) NULL else list(selection = out, nonce = nonce)
}

read_categorical <- function(s) {
  filters <- read_filters(s$filters)
  columns <- read_filters(s$columns, optional = TRUE)
  if (is.null(filters) || is.null(columns) ||
        (!length(filters) && !length(columns))) {
    return(NULL)
  }
  list(type = "categorical", filters = filters, columns = columns)
}

read_range <- function(s) {
  if (!is_name(s$x) || !is_pair(s$x_range)) {
    return(NULL)
  }
  has_y <- !is.null(s$y_range)
  if (has_y && (!is_name(s$y) || !is_pair(s$y_range))) {
    return(NULL)
  }
  filters <- read_filters(s$filters, optional = TRUE)
  if (is.null(filters)) {
    return(NULL)
  }
  list(type = "range", x = s$x, y = if (has_y) s$y,
       x_range = as.numeric(unlist(s$x_range)),
       y_range = if (has_y) as.numeric(unlist(s$y_range)),
       filters = filters)
}

# `{col: [value or null]}` as a named list of character vectors, NULL as NA;
# NULL when the shape is wrong. A missing `f` is an empty list when optional.
read_filters <- function(f, optional = FALSE) {
  if (optional && is.null(f)) {
    return(list())
  }
  if (!is.list(f) || (length(f) && (is.null(names(f)) ||
                                      any(!nzchar(names(f)))))) {
    return(NULL)
  }
  out <- list()
  for (nm in names(f)) {
    v <- f[[nm]]
    if (!is.list(v) || !length(v)) {
      return(NULL)
    }
    ok <- vapply(v, function(e) is.null(e) || is_scalar_text(e), logical(1L))
    if (!all(ok)) {
      return(NULL)
    }
    out[[nm]] <- vapply(v, function(e) {
      if (is.null(e)) NA_character_ else as.character(e)
    }, character(1L))
  }
  out
}

is_name <- function(x) is.character(x) && length(x) == 1L && nzchar(x)

is_scalar_text <- function(x) {
  (is.character(x) || is.numeric(x) || is.logical(x)) && length(x) == 1L &&
    !is.na(x)
}

is_pair <- function(x) {
  x <- if (is.list(x)) unlist(x) else x
  is.numeric(x) && length(x) == 2L && all(is.finite(x))
}

#' Read a capture message: `{png: <data URL>, width, height}`
#' @noRd
viz_read_capture <- function(x) {
  ok <- is.list(x) && is.character(x$png) && length(x$png) == 1L &&
    startsWith(x$png, "data:image/png;base64,") &&
    is_size(x$width) && is_size(x$height)
  if (!ok) {
    return(NULL)
  }
  list(png = x$png, width = as.numeric(x$width), height = as.numeric(x$height))
}

is_size <- function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x) && x > 0
}

#' Read a data view message: `{on, tab, n}`
#'
#' Whether the reader has the view's data table open, on which tab, and the
#' browser's request count: a new count asks for the table's rows again.
#' @return `list(on, tab, n)`, `tab` `"summary"` or `"rows"`, or `NULL`
#'   when malformed.
#' @noRd
viz_read_data_view <- function(x) {
  if (!is.list(x) || !is.logical(x$on) || length(x$on) != 1L ||
        is.na(x$on)) {
    return(NULL)
  }
  tab <- x$tab
  ok <- is.character(tab) && length(tab) == 1L &&
    tab %in% c("summary", "rows")
  n <- if (is_whole_count(x$n)) as.numeric(x$n) else 0
  list(on = x$on, tab = if (ok) tab else "summary", n = n)
}

#' Register the input handlers
#'
#' One handler per block spec (`blockr.viz.chart`, `blockr.viz.table`) and
#' one each for selections, captures and the data view. Called from `.onLoad`.
#'
#' @param specs A list of `viz_spec`.
#' @noRd
viz_input_handlers <- function(specs) {
  register <- function(type, read) {
    shiny::registerInputHandler(
      paste0("blockr.viz.", type),
      function(x, session, name) read(x),
      force = TRUE
    )
  }
  # lapply gives each handler its own `spec`; a for loop would share one.
  lapply(specs, function(spec) {
    register(spec$block, function(x) viz_read_set(spec, x))
  })
  register("selection", viz_read_selection)
  register("capture", viz_read_capture)
  register("data_view", viz_read_data_view)
  invisible(NULL)
}

#' The settings a message may apply
#'
#' Three gates, in order. On a locked board nothing applies: the browser can
#' forge any input, so the refusal happens here. The prepare script is R
#' code the server runs, so `script` applies only where the
#' `blockr.viz.browser_script` option allows it. Its controls' values
#' (`script_values`) are data and pass; the settings observer checks them
#' against the script's controls (viz_script_values()). A key changed on the
#' server after the revision the message was built against is stale and
#' skipped.
#'
#' @param msg A settings message from viz_read_set().
#' @param revs The block's counter from new_viz_revs().
#' @param locked Whether the board is locked.
#' @param browser_script Whether the browser may set the prepare script.
#' @return A named list of the values to apply.
#' @noRd
viz_gate_set <- function(msg, revs, locked = viz_locked(),
                         browser_script = isTRUE(
                           getOption("blockr.viz.browser_script", FALSE)
                         )) {
  if (locked) {
    return(list())
  }
  values <- msg$values
  if (!browser_script) {
    values <- values[setdiff(names(values), "script")]
  }
  values[setdiff(names(values), revs$stale(names(values), msg$rev))]
}

# Whether the board is locked. The option decides until a block server can
# ask blockr.core for its board's lock.
viz_locked <- function() {
  isTRUE(blockr.core::blockr_option("locked", FALSE))
}
