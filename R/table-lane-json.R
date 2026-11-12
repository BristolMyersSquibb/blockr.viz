# A swimlane column's nested arrays, written as JSON here rather than by
# Shiny, for the reason R/table-tile-json.R gives: jsonlite dispatches once
# per list and per value. A swimmer of 120 patients holds some 40,000 of them
# (a tuple per segment, a box per strip entry, an object per lane), and
# writing them took longer than building the table. Here every string and
# number of a field goes through jsonlite in one call (tile_json_tokens()),
# and the brackets and keys around them are pasted. The text is what
# shiny_json() writes for the same column.
#
# Only the shapes cells_interval() gives are written here. A field holding
# anything else stays a list and Shiny writes it; a lane with a key that is
# not in LANE_JSON_KEYS leaves all the lanes to Shiny, so a new lane field
# needs its line there to keep this path.

# The TableModel with the swimlane columns' segments, overlays and lanes as
# `json` strings. A model without a swimlane column comes back as it is.
table_lane_json <- function(model) {
  model$cols <- lapply(model$cols, function(cc) {
    if (is.list(cc) && is.list(cc$mark) &&
          identical(cc$mark$kind, "interval")) {
      cc$mark <- lane_json_mark(cc$mark)
    }
    cc
  })
  model
}

# The fields that hold one array of tuples per row: segments [left, width,
# level, label, style], line pieces [left, width], events [x, shape] and
# strip boxes [text, style].
LANE_JSON_TUPLES <- c("segs", "band", "events", "mainStrip")

lane_json_mark <- function(mark) {
  rows <- function(js) {
    tile_json_verbatim(paste0("[", paste(js, collapse = ","), "]"))
  }
  for (nm in intersect(LANE_JSON_TUPLES, names(mark))) {
    js <- lane_json_nested(mark[[nm]])
    if (!is.null(js)) mark[[nm]] <- rows(js)
  }
  if (!is.null(mark$subLanes)) {
    js <- lane_json_lanes(mark$subLanes)
    if (!is.null(js)) mark$subLanes <- rows(js)
  }
  mark
}

# One JSON array per list of plain scalars (strings and numbers), for many
# lists at once. NULL when a value is anything else.
lane_json_arrays <- function(lists) {
  leaves <- unlist(lists, recursive = FALSE, use.names = FALSE)
  # 1 a string, 2 a number, 0 neither.
  kind <- vapply(leaves, function(x) {
    if (length(x) != 1L || !is.null(attributes(x))) {
      0L
    } else if (is.character(x)) {
      1L
    } else if (is.numeric(x)) {
      2L
    } else {
      0L
    }
  }, integer(1L))
  if (any(kind == 0L)) {
    return(NULL)
  }
  token <- character(length(leaves))
  for (k in 1:2) {
    at <- kind == k
    token[at] <- tile_json_tokens(unlist(leaves[at], use.names = FALSE))
  }
  tile_json_collapse(token, lengths(lists), "[", "]")
}

# One JSON array of arrays per list of tuples, for many lists at once.
lane_json_nested <- function(lists) {
  tuples <- unlist(lists, recursive = FALSE, use.names = FALSE)
  if (!all(vapply(tuples, is.list, logical(1L)))) {
    return(NULL)
  }
  js <- lane_json_arrays(tuples)
  if (!is.null(js)) tile_json_collapse(js, lengths(lists), "[", "]")
}

# A lane's fields in the order cells_interval_lanes() writes them, and how
# each is written: a scalar, an array of scalars or an array of tuples.
LANE_JSON_KEYS <- c(n = "scalar", s = "nested", t = "array", b = "nested",
                    l = "array", e = "nested", et = "array", o = "scalar")

# One JSON array of lane objects per row. NULL when a lane holds a key that
# is not in LANE_JSON_KEYS, or a value that does not fit.
lane_json_lanes <- function(rows) {
  lanes <- unlist(rows, recursive = FALSE, use.names = FALSE)
  keys <- names(LANE_JSON_KEYS)
  known <- vapply(lanes, function(l) {
    at <- match(names(l), keys)
    is.list(l) && length(at) > 0L && !anyNA(at) &&
      !is.unsorted(at, strictly = TRUE)
  }, logical(1L))
  if (!all(known)) {
    return(NULL)
  }
  body <- character(length(lanes))
  for (k in keys) {
    has <- vapply(lanes, function(l) !is.null(l[[k]]), logical(1L))
    if (!any(has)) next
    vals <- lapply(lanes[has], `[[`, k)
    js <- switch(
      LANE_JSON_KEYS[[k]],
      # A scalar is written as a one-value array, less its brackets.
      scalar = lane_json_unbox(lane_json_arrays(lapply(vals, list))),
      array = lane_json_arrays(vals),
      nested = lane_json_nested(vals)
    )
    if (is.null(js)) {
      return(NULL)
    }
    body[has] <- paste0(body[has], ifelse(nzchar(body[has]), ",", ""),
                        "\"", k, "\":", js)
  }
  tile_json_collapse(paste0("{", body, "}"), lengths(rows), "[", "]")
}

lane_json_unbox <- function(js) {
  if (!is.null(js)) substr(js, 2L, nchar(js) - 1L)
}
