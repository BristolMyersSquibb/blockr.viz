# What a block sends the browser, written in one place.
#
# viz_wire() writes the settings from the spec, viz_wire_df() the rows. Both
# return JSON that Shiny splices into the message as it is, so the shape is
# decided here and nowhere else:
#
# - every spec key is present; NULL is `null`
# - a vector type is always an array, whatever its length
# - an empty named list is `{}`
# - NA, NaN and infinite numbers are `null`
# - numbers carry 15 significant digits

#' The settings as the browser receives them
#'
#' @param spec The block's `viz_spec`.
#' @param values A named list with every spec argument, as the state holds it.
#' @return A `json` string.
#' @noRd
viz_wire <- function(spec, values) {
  out <- list()
  for (nm in names(spec$args)) {
    out[nm] <- list(wire_value(spec$args[[nm]]$type, values[[nm]]))
  }
  wire_json(out)
}

wire_json <- function(x) {
  jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null",
                   digits = NA)
}

wire_value <- function(type, x) {
  if (is.null(x)) {
    return(NULL)
  }
  switch(
    type$name,
    columns = , enums = , strings = I(unname(x)),
    numbers = , values = wire_labelled(x),
    record = wire_record(type, x),
    records = lapply(unname(x), wire_record, type = type),
    records_by = lapply(unname(x), function(r) {
      c(r[type$key], wire_record(type$variants[[r[[type$key]]]],
                                 r[setdiff(names(r), type$key)]))
    }),
    named = if (length(x)) {
      lapply(x, wire_value, type = type$value$type)
    } else {
      structure(list(), names = character())
    },
    map = if (length(x)) {
      wire_map(x)
    } else {
      structure(list(), names = character())
    },
    x
  )
}

# A map's atomic values always as arrays, so a single NA or value keeps its
# place; map_canon() reads them back.
wire_map <- function(x) {
  if (is.list(x)) {
    return(lapply(x, wire_map))
  }
  if (is.atomic(x) && length(x)) I(x) else x
}

# Values whose names are labels, as `[{value, label}]`; no name is `null`.
wire_labelled <- function(x) {
  nms <- names(x) %||% rep("", length(x))
  lapply(seq_along(x), function(i) {
    list(value = unname(x[[i]]),
         label = if (nzchar(nms[[i]])) nms[[i]] else NULL)
  })
}

wire_record <- function(type, x) {
  out <- list()
  for (f in names(type$fields)) {
    out[f] <- list(wire_value(type$fields[[f]]$type, x[[f]]))
  }
  out
}

#' The rows as the browser receives them
#'
#' Column-oriented. A factor, and any other column whose values repeat
#' enough, travels as `{"__enc__": "dict", "levels": [...], "codes": [...]}`
#' with 0-based codes and `null` for a missing value; a factor keeps all its
#' levels, in order. Classes:
#'
#' | class | values | meta |
#' |---|---|---|
#' | character, factor | strings | `levels` for a factor |
#' | logical | `true` / `false` | |
#' | integer, double | numbers | |
#' | integer64 | numbers; above 2^53 strings, with a warning | |
#' | Date | `"YYYY-MM-DD"` | `class: "date"` |
#' | POSIXct | milliseconds since 1970 | `class: "datetime"`, `tz` |
#' | difftime, hms | numbers | `class: "duration"`, `unit` |
#'
#' Any other column (a list, complex, raw) is left out, with a warning.
#'
#' @param df A data frame.
#' @return `list(columns, data)`: `columns` a list of column meta (`name`,
#'   `class`, `label`, `n_unique` and the entries above), `data` a `json`
#'   string.
#' @noRd
viz_wire_df <- function(df) {
  w <- wire_df_parts(df, warn = TRUE)
  list(columns = w$meta,
       data = jsonlite::toJSON(w$cols, na = "null", null = "null",
                               digits = NA))
}

#' The column meta of a frame, without its rows
#'
#' What viz_wire_df() gives as `columns`, for a view that sends its own
#' rows (the table sends its model) but whose gear picks columns of the
#' input.
#' @param df A data frame, or `NULL`.
#' @return A list of column meta; `NULL` for no frame.
#' @noRd
viz_wire_columns <- function(df) {
  if (!is.data.frame(df)) return(NULL)
  wire_df_parts(df, warn = FALSE)$meta
}

wire_df_parts <- function(df, warn) {
  cols <- list()
  meta <- list()
  skipped <- character()
  for (nm in names(df)) {
    w <- wire_column(df[[nm]])
    if (is.null(w)) {
      skipped <- c(skipped, nm)
      next
    }
    cols[[nm]] <- w$values
    meta[[length(meta) + 1L]] <- c(
      list(name = nm, class = w$class,
           label = attr(df[[nm]], "label", exact = TRUE) %||% "",
           n_unique = length(unique(df[[nm]]))),
      w$meta
    )
  }
  if (warn && length(skipped)) {
    warning("Left out of the chart data, their class cannot be drawn: ",
            paste0("`", skipped, "`", collapse = ", "), ".", call. = FALSE)
  }
  list(cols = cols, meta = meta)
}

# One column as wire values, its class and its meta; NULL when it cannot
# travel.
wire_column <- function(x) {
  if (is.factor(x)) {
    return(list(values = wire_dict(as.character(x), levels(x)),
                class = "string", meta = list(levels = I(levels(x)))))
  }
  if (inherits(x, "Date")) {
    return(wire_plain(format(x, "%Y-%m-%d"), "date"))
  }
  if (inherits(x, "POSIXct")) {
    tz <- attr(x, "tzone") %||% ""
    return(wire_plain(as.numeric(x) * 1000, "datetime",
                      list(tz = if (nzchar(tz[[1L]])) tz[[1L]] else "UTC")))
  }
  if (inherits(x, "difftime")) {
    u <- units(x)
    return(wire_plain(as.numeric(x, units = u), "duration", list(unit = u)))
  }
  if (inherits(x, "integer64")) {
    return(wire_plain(wire_int64(x), "number"))
  }
  if (is.character(x)) {
    return(wire_plain(as.vector(x), "string"))
  }
  if (is.logical(x)) {
    return(wire_plain(as.vector(x), "bool"))
  }
  if (is.numeric(x)) {
    return(wire_plain(as.vector(x), "number"))
  }
  NULL
}

wire_plain <- function(v, class, meta = list()) {
  if (dict_worth_it(v)) {
    v <- wire_dict(v, sort(unique(v[!is.na(v)])))
  }
  list(values = v, class = class, meta = meta)
}

# Codes into `levels`, 0-based, NA for a missing value.
wire_dict <- function(v, levels) {
  list(`__enc__` = jsonlite::unbox("dict"), levels = I(levels),
       codes = match(v, levels) - 1L)
}

# A dictionary pays when the column is long and its values repeat: a code is
# shorter than the value it stands for, and the levels go out once.
dict_worth_it <- function(v, min_rows = 500L, max_ratio = 0.25,
                          max_levels = 4096L) {
  n <- length(v)
  if (n < min_rows) {
    return(FALSE)
  }
  k <- length(unique(v))
  k <= max_ratio * n && k <= max_levels
}

# A 64-bit integer as a double when it is exact there, else as text.
wire_int64 <- function(x) {
  d <- suppressWarnings(as.numeric(x))
  big <- !is.na(d) & abs(d) >= 2^53
  if (!any(big)) {
    return(d)
  }
  warning("Integers above 2^53 are sent as text.", call. = FALSE)
  as.character(x)
}
