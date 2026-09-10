# The chart's data payload, dictionary-encoded.
#
# A chart whose subtitle carries an `{@arg}` slot ships every kind-marked
# column with its data, so re-pointing colour or facet from the sentence needs
# no round trip (see needed_cols()). That is bounded by the marks but not by
# the rows, and the findings frames are the big ones: opening "VS: Records by
# Parameter" on the CEDX board sent 15.67 MB in one websocket frame -- 25
# columns x 65,032 rows -- to draw nine bars.
#
# Almost all of it is repeated low-cardinality strings: ETHNIC has 2 distinct
# values in 65,032 rows (1.57 MB), PARAM has 9 (1.77 MB), TRT has 3 (1.12 MB).
# Shipping levels once plus an integer per row removes that without changing
# anything the reader sees.
#
# Same shape as blockr.dm's `encode_crossfilter_payload()`, and for the same
# reason: httpuv does not negotiate permessage-deflate, so a byte saved here is
# a byte saved on the wire (reference_shiny_ws_uncompressed_deflate_seam).
# Deflating on top, as the crossfilter payload does, is the next step and is
# deliberately NOT taken here -- it needs the client-side inflate and a gate
# for browsers without DecompressionStream, where this needs neither.

#' Should this column be dictionary-encoded?
#'
#' Only character and factor columns: a numeric vector is already compact, and
#' codes plus levels would cost more than it saves. `min_rows` keeps the
#' encoding off small frames, where the levels array is pure overhead, and
#' `max_ratio` keeps it off columns that are near-unique (a true id column
#' encodes to one code per row plus a level per row, which is strictly worse).
#'
#' @param v A column.
#' @param min_rows Below this many rows, never encode.
#' @param max_ratio Encode only when `n_unique <= max_ratio * length(v)`.
#' @return `TRUE` when encoding pays.
#' @noRd
chart_col_dict_worth_it <- function(v, min_rows = 500L, max_ratio = 0.25) {
  if (!(is.character(v) || is.factor(v))) {
    return(FALSE)
  }
  n <- length(v)
  if (n < min_rows) {
    return(FALSE)
  }
  length(unique(v)) <= max_ratio * n
}

#' Serialize a chart's data frame for the browser
#'
#' Column-oriented, as before. A column worth encoding travels as
#' `{"__enc__":"dict","levels":[...],"codes":[...]}`, everything else exactly
#' as it did.
#'
#' The levels array is `unique(v)`, NAs included, and it is serialized by the
#' same `toJSON()` call that would have written the column itself. That is what
#' makes the round trip exact rather than approximately right: whatever a value
#' becomes on the wire today -- `null` for `NA_character_`, the string for
#' everything else -- it becomes the same thing as a level, and the client
#' reads it back by index. No NA rule is restated here, so none can drift.
#'
#' `codes` are 0-based, to be read straight as a JS array index.
#'
#' @param df The data frame to send.
#' @param ... Passed to [chart_col_dict_worth_it()].
#' @return A `json`-classed string, as [jsonlite::toJSON()] returns.
#' @noRd
chart_data_json <- function(df, ...) {
  cols <- as.list(df)
  for (nm in names(cols)) {
    v <- cols[[nm]]
    if (!chart_col_dict_worth_it(v, ...)) {
      next
    }
    lv <- unique(v)
    cols[[nm]] <- list(
      `__enc__` = jsonlite::unbox("dict"),
      levels = lv,
      codes = match(v, lv) - 1L
    )
  }
  # `digits = NA` and the column orientation are the payload's existing
  # contract; a bare list serializes column-oriented by construction.
  jsonlite::toJSON(cols, digits = NA, auto_unbox = FALSE)
}
