# A multi-select whose pool follows another control: the picks of the old
# pool are left behind, by the run and by the value set.

pool_script <- function() {
  paste(
    "level <- factor(\"pt\", levels = c(\"pt\", \"llt\"))  #| select()",
    paste0("terms <- if (identical(as.character(level), \"pt\")) ",
           "c(\"PT: Nausea\", \"PT: Rash\") else c(\"LLT: Nausea\", ",
           "\"LLT: Itch\")"),
    paste0("picks <- factor(character(), levels = terms)  #| select(",
           "multiple = TRUE, placeholder = \"all\")"),
    "data$n <- length(picks)",
    "data",
    sep = "\n"
  )
}

pool_run <- function(values) {
  parsed <- cb_parse(pool_script())
  d <- data.frame(x = 1)
  specs <- cb_specs(parsed, d, values)
  dd_prepare_run(d, parsed, specs, values)$data$n
}

test_that("a pick the pool no longer offers does not run", {
  expect_identical(pool_run(list(picks = "PT: Nausea")), 1L)
  # The level changed, the pick stayed: the word reads "all", so the run
  # takes no pick.
  v <- list(level = "llt", picks = "PT: Nausea")
  expect_identical(pool_run(v), 0L)
  specs <- cb_specs(cb_parse(pool_script()), data.frame(x = 1), v)
  expect_identical(script_title_args(list(), specs, v)$picks, "all")
  # Mixed: the offered pick runs alone.
  expect_identical(pool_run(list(level = "llt",
                                 picks = c("PT: Nausea", "LLT: Itch"))), 1L)
})

test_that("a value set drops the picks of the old pool", {
  state <- list(
    script = shiny::reactiveVal(pool_script()),
    script_values = shiny::reactiveVal(list(picks = "PT: Nausea"))
  )
  src <- viz_script_sources(shiny::reactive(data.frame(x = 1)), state)
  out <- src$settle(list(level = "llt", picks = "PT: Nausea"))
  expect_identical(out$picks, character())
  expect_identical(out$level, "llt")
  # Picks the pool offers stay.
  out <- src$settle(list(level = "pt", picks = list("PT: Nausea")))
  expect_identical(out$picks, "PT: Nausea")
  # Without data nothing is dropped.
  src <- viz_script_sources(shiny::reactive(NULL), state)
  v <- list(level = "llt", picks = "PT: Nausea")
  expect_identical(src$settle(v), v)
})
