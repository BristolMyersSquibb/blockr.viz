# Cases for the table model and view tests: the first render of each table
# recording, with the state under the spec's names and the scenario's data.

tm_vocab <- list(args = TRUE, summary_table = TRUE)

# Scenarios whose first render drew the scenario's own data: no prepare
# script, no options, a data frame as input.
tm_cases <- function(layout) {
  out <- list()
  for (nm in names(table_golden_scenarios())) {
    f <- test_path("fixtures", "table-golden", paste0(nm, ".rds"))
    if (!file.exists(f)) next
    r <- readRDS(f)
    if (!all(c("initial", "steps") %in% names(r))) next
    sc <- table_golden_scenarios()[[nm]]
    if (!is.null(sc$args$script) || length(sc$options)) next
    data <- tryCatch(table_golden_data(sc$data %||% "df"),
                     error = function(e) NULL)
    if (!is.data.frame(data)) next
    r <- table_golden_translate(r, tm_vocab)
    st <- r$steps[[1L]]
    state <- suppressWarnings(legacy_restore("table", st$state,
                                             VIZ_REWRITE_VERSION))
    if (!identical(state$layout, layout)) next
    out[[nm]] <- list(state = state, data = data, messages = st$messages)
  }
  out
}

tm_payload <- function(messages, type) {
  hit <- Filter(function(m) identical(m$type, type), messages)
  if (length(hit)) hit[[1L]]$message$payload
}

# A payload as the browser read it: JSON, parsed without simplifying.
tm_json <- function(x) {
  jsonlite::fromJSON(dt_payload_json(x), simplifyVector = FALSE)
}
