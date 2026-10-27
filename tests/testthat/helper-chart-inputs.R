# The chart's browser inputs in tests, read by the same functions Shiny's
# input handlers call (kit-input.R).

# Settings the gear changed, as `<id>_set`.
chart_input_set <- function(scope, ..., rev = 0L) {
  scope$setInputs(chart_set = viz_read_set(
    viz_block_spec("chart"), list(rev = rev, values = list(...))
  ))
}

# A click on marks: `chart_input_click(scope, AETERM = "Rash")`; no
# arguments clears.
chart_input_click <- function(scope, ..., nonce = 1L) {
  f <- list(...)
  scope$setInputs(chart_select = viz_read_selection(list(
    selection = if (length(f)) {
      list(type = "categorical",
           filters = lapply(f, function(v) {
             lapply(v, function(x) if (is.na(x)) NULL else x)
           }))
    },
    nonce = nonce
  )))
}
