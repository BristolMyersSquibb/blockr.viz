# `#| setting`: a script line that takes the block's own setting of that
# name, so a script can count by the column the reader grouped by.

ss_data <- data.frame(
  USUBJID = c("1", "2", "3", "1", "2", "4"),
  SOC = c("A", "A", "A", "B", "B", "B"),
  PT = c("a1", "a1", "a2", "b1", "b2", "b2")
)

# Keep the rows whose innermost grouping value has at least `min_n` subjects.
ss_script <- paste(
  "by <- c(\"SOC\", \"PT\")  #| setting",
  "min_n <- 2  #| number(min = 1)",
  ".col <- by[length(by)]",
  ".n <- tapply(data$USUBJID, data[[.col]], function(x) length(unique(x)))",
  "data[data[[.col]] %in% names(.n)[.n >= min_n], , drop = FALSE]",
  sep = "\n"
)

test_that("a setting line is no control and reads the value it is given", {
  p <- cb_parse(ss_script)
  expect_identical(cb_setting_names(p), "by")
  specs <- cb_specs(p, ss_data, list(by = "SOC"))
  kinds <- vapply(specs, `[[`, "", "kind")
  expect_identical(kinds, c("setting", "number"))
  expect_identical(
    vapply(script_control_records(specs), `[[`, "", "name"), "min_n"
  )
  expect_false("sv_by" %in% names(script_control_values(specs)))
  # The browser cannot set it through script_values.
  expect_identical(viz_script_values(list(by = "PT"), specs, list()), list())

  # The block's value is written into the script, so the code reproduces.
  ex <- cb_expr(p, specs, list(by = "SOC", min_n = 2), slot = FALSE)
  expect_match(paste(deparse(ex), collapse = " "),
               ".col <- \"SOC\"[length(\"SOC\")]", fixed = TRUE)
  # Grouped by PT, a1 and b2 have two subjects; by SOC, both SOCs have.
  run <- function(by) {
    dd_prepare_run(ss_data, p, cb_specs(p, ss_data, list(by = by)),
                   list(by = by, min_n = 2))$data
  }
  expect_setequal(run(c("SOC", "PT"))$PT, c("a1", "b2"))
  expect_identical(nrow(run("SOC")), 6L)
  # Without a value the line's own default.
  expect_setequal(dd_prepare_run(ss_data, p, cb_specs(p, ss_data),
                                 list(min_n = 2))$data$PT, c("a1", "b2"))
})

test_that("the table's script follows the table's grouping", {
  withr::local_options(blockr.viz.browser_script = NULL)
  blk <- new_table_block(
    layout = "groups", by = c("SOC", "PT"),
    summaries = list(list(type = "simple", func = "count", name = "Events")),
    script = ss_script
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      expr <- session$makeScope("expr")
      session$flushReact()
      code <- function() {
        paste(deparse(session$returned$expr()), collapse = " ")
      }
      expect_match(code(), "c(\"SOC\", \"PT\")[length(", fixed = TRUE)
      expr$setInputs(table_set = viz_read_set(
        viz_block_spec("table"),
        list(rev = 0L, values = list(by = list("SOC")))
      ))
      session$flushReact()
      expect_identical(session$returned$state$by(), "SOC")
      expect_match(code(), "\"SOC\"[length(\"SOC\")]", fixed = TRUE)
      # No script control for `by`.
      inputs <- expr$output$table$derived$script$inputs
      expect_identical(vapply(inputs, `[[`, "", "name"), "min_n")
    },
    args = list(x = blk, data = list(data = function() ss_data))
  )
})

test_that("a setting line reads a field of the table's first summary", {
  withr::local_options(blockr.viz.browser_script = NULL)
  script <- paste(
    "facet <- \"PT\"  #| setting",
    ".n <- table(data[[facet]])",
    "data[data[[facet]] %in% names(.n)[.n >= 2], , drop = FALSE]",
    sep = "\n"
  )
  blk <- new_table_block(
    layout = "groups", by = "USUBJID",
    summaries = list(list(type = "simple", func = "count", show = "tile",
                          facet = "SOC", name = "Events")),
    script = script
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      code <- paste(deparse(session$returned$expr()), collapse = " ")
      expect_match(code, "[[\"SOC\"]]", fixed = TRUE)
    },
    args = list(x = blk, data = list(data = function() ss_data))
  )
})
