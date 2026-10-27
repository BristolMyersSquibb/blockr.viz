# Helpers for tests that run a table block's expression.

# Evaluate a block's bbquote()'d expression on `data`: bind `data` and
# unwrap the `.()` placeholder, as the board does.
eval_block_expr <- function(ex, data) {
  e <- new.env(parent = globalenv())
  e$data <- data
  e$. <- function(x) x
  eval(ex, envir = e)
}
