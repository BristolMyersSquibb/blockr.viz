# Writes tests/js/fixtures/table/<scenario>.json for the table view's tests
# (inst/js/table/): for each table recording (tests/testthat/
# helper-table-golden.R), the payload the block's `table` output holds after
# each step, as Shiny writes it. A step that left the output as it was is
# null; a payload whose rows the browser already holds has no `data`.
#
# Run from the package root after changing the table's server or its model:
#
#   Rscript tests/js/fixtures/table/make-fixtures.R

pkgload::load_all(".", quiet = TRUE)
for (f in list.files("tests/testthat", "^helper", full.names = TRUE)) {
  source(f)
}

table_fixture_run <- function(sc) {
  table_golden_register_tbl()
  withr::local_options(list(blockr.viz.default_caption = NULL,
                            blockr.viz.browser_script = TRUE))
  withr::local_options(sc$options %||% list())
  src <- new.env(parent = emptyenv())
  src$d <- table_golden_data(sc$data %||% "df")
  tick <- shiny::reactiveVal(0L)
  dat <- shiny::reactive({
    tick()
    src$d
  })
  blk <- suppressWarnings(table_golden_ctor(tg_rw_args(sc$args)))
  out <- list()
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      table_golden_mock(session)
      expr <- session$makeScope("expr")
      cx <- new.env(parent = emptyenv())
      cx$data <- function() src$d
      last <- NULL
      take <- function() {
        p <- tryCatch(expr$output$table, error = function(e) NULL)
        res <- if (!identical(p, last)) p
        last <<- p
        res
      }
      session$flushReact()
      out[[1L]] <<- take()
      for (i in seq_along(sc$steps)) {
        s <- sc$steps[[i]]
        if (!is.null(s$input)) {
          vals <- tg_rw_inputs(s, session, expr, cx)
          if (length(vals)) suppressWarnings(do.call(expr$setInputs, vals))
        } else if (!is.null(s$data)) {
          src$d <- table_golden_data(s$data)
          tick(i)
        }
        session$flushReact()
        out[i + 1L] <<- list(take())
      }
    },
    args = c(
      list(x = blk, data = list(data = dat)),
      if (isFALSE(sc$inputs_ready)) {
        list(inputs_ready = shiny::reactive(FALSE))
      }
    )
  )
  out
}

dir <- file.path("tests", "js", "fixtures", "table")
for (nm in names(table_golden_scenarios())) {
  sc <- table_golden_scenarios()[[nm]]
  steps <- table_fixture_run(sc)
  json <- shiny:::toJSON(list(scenario = nm, steps = steps), pretty = TRUE)
  writeLines(json, file.path(dir, paste0(nm, ".json")))
}
message("Wrote ", length(table_golden_scenarios()), " fixtures to ", dir)
