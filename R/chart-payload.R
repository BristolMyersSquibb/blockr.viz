#' A one-slot cache keyed by identical()
#'
#' A dock panel visit hands the chart server an equal frame, which invalidates
#' every reactive that reads it. The costly ones (serializing the rows, the
#' column metadata, the band and smoother fits, the scale resolution) keep their
#' last answer and return it while their inputs are identical(). On the same
#' object that is a pointer comparison; on an equal copy it walks the values,
#' which is still much cheaper than the work it saves.
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

# A config entry serialized the way Shiny would, as a `json` string that is
# spliced into the websocket message verbatim, so a large entry is not
# serialized again on every push. NULL stays NULL.
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
