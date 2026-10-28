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
  jsonlite::fromJSON(as.character(wire_json(x)), simplifyVector = FALSE)
}

# The groups table as the block builds it: the spec's state names in,
# table_model_groups()'s list(prep, cells) out. `cells$cols` is the cell
# model's columns, the ones table_groups_view() turns into marks. The
# summaries go in as written (some fixtures use the legacy keys the
# preparer still reads), so the state is not run through viz_check().
# Missing groups drop and the sort runs descending, as summarize_prepare()
# does by default.
sum_model <- function(data, ...) {
  st <- viz_check(viz_block_spec("table"), list(layout = "groups"))
  st$missing <- "drop"
  st$sort_dir <- "desc"
  args <- list(...)
  for (nm in names(args)) st[nm] <- list(args[[nm]])
  table_model_groups(st, data)
}
