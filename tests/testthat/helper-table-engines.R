# The table block runs two engines as sub-modules (R/table-records-server.R,
# R/table-groups-server.R). Tests of an engine's own behaviour (drill
# messages, gear state, payloads) drive the engine directly, with the
# engine's argument names, the way they drove the old blocks' servers.
records_engine <- function(...) {
  args <- list(...)
  function(id, data) {
    do.call(blockr.viz:::table_records_server, c(list(id, data), args))
  }
}

groups_engine <- function(...) {
  args <- list(...)
  function(id, data) {
    do.call(blockr.viz:::table_groups_server, c(list(id, data), args))
  }
}

# Evaluate a block's bbquote()'d expression on `data`: bind `data` and
# unwrap the `.()` placeholder, as the board does.
eval_block_expr <- function(ex, data) {
  e <- new.env(parent = globalenv())
  e$data <- data
  e$. <- function(x) x
  eval(ex, envir = e)
}
