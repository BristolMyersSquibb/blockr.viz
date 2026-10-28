# The prepare script's controls set from the browser (viz_script_values()):
# accepted without the browser_script option, checked against the script.

ks_script <- paste(
  "n <- 6  #| number(min = 1, max = 10)",
  "grp <- factor(\"setosa\", levels = levels(data$Species))",
  "wide <- FALSE",
  ".d <- dplyr::filter(data, Species %in% grp)",
  "if (wide) .d <- dplyr::filter(.d, Sepal.Width > 3.5)",
  "utils::head(.d, n)",
  sep = "\n"
)

ks_specs <- function() {
  cb_specs(cb_parse(ks_script), datasets::iris)
}

test_that("a script value is taken when its control could have sent it", {
  specs <- ks_specs()
  cur <- list(n = 6)
  expect_identical(viz_script_values(list(n = 2), specs, cur)$n, 2)
  expect_identical(viz_script_values(list(grp = "virginica"), specs, cur)$grp,
                   "virginica")
  expect_true(viz_script_values(list(wide = "on"), specs, cur)$wide)
  expect_true(viz_script_values(list(wide = TRUE), specs, cur)$wide)
  expect_false(viz_script_values(list(wide = "off"), specs, cur)$wide)
})

test_that("unknown names and values a control could not send are dropped", {
  specs <- ks_specs()
  cur <- list(n = 6)
  expect_identical(viz_script_values(list(zz = 1), specs, cur), cur)
  # outside number(min = 1, max = 10)
  expect_identical(viz_script_values(list(n = 20), specs, cur), cur)
  expect_identical(viz_script_values(list(n = "2"), specs, cur), cur)
  expect_identical(viz_script_values(list(grp = "mars"), specs, cur), cur)
  expect_identical(viz_script_values(list(grp = c("setosa", "virginica")),
                                     specs, cur), cur)
  expect_identical(viz_script_values(list(wide = "maybe"), specs, cur), cur)
  # the good ones of a mixed message still land
  out <- viz_script_values(list(n = 3, grp = "mars"), specs, cur)
  expect_identical(out, list(n = 3))
})

# The rows a chart payload carries: columns as one JSON object.
ks_rows <- function(payload) {
  cols <- jsonlite::fromJSON(payload$data, simplifyVector = FALSE)
  length(cols[[1L]])
}

test_that("the chart's script controls work without the browser_script option", {
  withr::local_options(blockr.viz.browser_script = NULL)
  blk <- new_chart_block(type = "scatter", x = "Sepal.Length",
                         y = "Sepal.Width", script = ks_script)
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      expr <- session$makeScope("expr")
      session$flushReact()
      expect_identical(ks_rows(expr$output$chart), 6L)

      chart_input_set(expr, script_values = list(n = 2))
      session$flushReact()
      expect_identical(session$returned$state$script_values()$n, 2)
      expect_identical(ks_rows(expr$output$chart), 2L)
      expect_identical(expr$output$chart$derived$script$values$sv_n, 2)

      # out of range: dropped, the chart keeps 2
      chart_input_set(expr, script_values = list(n = 50))
      session$flushReact()
      expect_identical(session$returned$state$script_values()$n, 2)

      # the script text itself stays behind the option
      chart_input_set(expr, script = "data")
      session$flushReact()
      expect_identical(session$returned$state$script(), ks_script)
    },
    args = list(x = blk, data = list(data = function() datasets::iris))
  )
})

test_that("the table's script controls work without the browser_script option", {
  withr::local_options(blockr.viz.browser_script = NULL)
  blk <- new_table_block(rowname = "Species", script = ks_script)
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      expr <- session$makeScope("expr")
      session$flushReact()
      n_rows <- function() expr$output$table$data$n
      expect_identical(n_rows(), 6L)

      expr$setInputs(table_set = viz_read_set(
        viz_block_spec("table"),
        list(rev = 0L, values = list(script_values = list(n = 2)))
      ))
      session$flushReact()
      expect_identical(session$returned$state$script_values()$n, 2)
      expect_identical(expr$output$table$derived$script$values$sv_n, 2)
      expect_identical(n_rows(), 2L)

      expr$setInputs(table_set = viz_read_set(
        viz_block_spec("table"),
        list(rev = 0L, values = list(script_values = list(n = 0, zz = 1)))
      ))
      session$flushReact()
      expect_identical(session$returned$state$script_values(), list(n = 2))
    },
    args = list(x = blk, data = list(data = function() datasets::iris))
  )
})
