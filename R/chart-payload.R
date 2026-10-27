# The chart's data payload, dictionary-encoded.
#
# A chart whose subtitle carries an `{@arg}` slot ships every kind-marked
# column with its data, so re-pointing colour or facet from the sentence needs
# no round trip (see needed_cols()). That is bounded by the marks but not by
# the rows, and the findings frames are the big ones: opening "VS: Records by
# Parameter" on a clinical board sent 15.67 MB in one websocket frame -- 25
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

#' A one-slot cache keyed by identical()
#'
#' A dock panel visit re-evaluates the block and hands the chart server an
#' equal frame, which invalidates every reactive that reads it. The ones that
#' cost something (serializing the rows, the column metadata, the band and
#' smoother fits, the scale resolution) keep their last answer and return it
#' while their inputs are identical(). On the same object that is a pointer
#' comparison; on an equal copy it walks the values, which is still much
#' cheaper than the work it saves.
#'
#' @return A function `(key, compute)` that returns the last value when `key`
#'   is identical to the last key, and otherwise calls `compute()` and keeps
#'   its result. A `compute()` that errors leaves the cache as it was.
#' @noRd
last_value_cache <- function() {
  env <- new.env(parent = emptyenv())
  env$full <- FALSE
  function(key, compute) {
    if (env$full && identical(key, env$key)) {
      return(env$value)
    }
    value <- compute()
    env$key <- key
    env$value <- value
    env$full <- TRUE
    value
  }
}

#' Serialize a config entry the way Shiny would
#'
#' A `json`-classed value is spliced into the websocket message verbatim, so a
#' large config entry serialized here once is not serialized again on every
#' push. The arguments are those of Shiny's own `toJSON()`, so the browser
#' receives the same text either way.
#'
#' @param x A value for the chart's config, or `NULL`.
#' @return `NULL`, or a `json`-classed string.
#' @noRd
chart_config_json <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  jsonlite::toJSON(
    x, dataframe = "columns", null = "null", na = "null", auto_unbox = TRUE,
    digits = getOption("shiny.json.digits", I(16)), use_signif = TRUE,
    force = TRUE, POSIXt = "ISO8601", UTC = TRUE, rownames = FALSE,
    keep_vec_names = TRUE, json_verbatim = TRUE
  )
}
