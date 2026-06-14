# Real browser-driven interaction test for the table block's client-side
# JavaScript (inst/js/table.js). The table ships an in-table search box that
# toggles the `blockr-hidden-search` class on non-matching rows entirely in the
# browser. We launch ONE block end-to-end via blockr.core::serve() (which mounts
# the block's UI + server in a tiny shinyApp) and drive that search box with a
# real headless chromium through shinytest2.
#
# This requires a working headless browser. Where none exists (e.g. some CI
# images) it is skipped; the reactive/server logic is covered without a browser
# by test-table-block-server.R, which always runs.

test_that("table block search box filters rows in the browser (JS)", {
  skip_on_cran()
  # Skip under R CMD check: launching headless chromium there leaves temp
  # detritus (a check NOTE) and is flaky. This still runs under
  # devtools::test() / CI, which is where the JS interaction is exercised.
  skip_if(nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")),
          "browser test skipped under R CMD check")
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if(!chromote_works(), "no headless browser available in this environment")

  # shinytest2 refuses to run "On CRAN"; serve()'s app trips that guard unless
  # NOT_CRAN is set. We are explicitly not on CRAN here.
  withr::local_envvar(NOT_CRAN = "true")
  configure_chromote()

  library(shinytest2)

  df <- data.frame(
    animal = c("aardvark", "beaver", "cheetah"),
    n      = c(1L, 2L, 3L),
    stringsAsFactors = FALSE
  )

  app_obj <- blockr.core::serve(new_table_block(), data = list(data = df))

  drv <- AppDriver$new(
    app_obj,
    name         = "table-search",
    timeout      = 30000,
    load_timeout = 30000
  )
  on.exit(drv$stop(), add = TRUE)

  # The table and its search box must render.
  body <- drv$get_html("body")
  expect_true(grepl("<table", body, fixed = TRUE))
  expect_true(grepl("blockr-search", body, fixed = TRUE))
  # column headers are present
  expect_true(grepl("animal", body, fixed = TRUE))

  count_hidden <- function() {
    drv$get_js(
      "document.querySelectorAll('tbody tr.blockr-hidden-search').length"
    )
  }

  # Nothing hidden initially.
  expect_equal(count_hidden(), 0)

  # Type a query that matches exactly one row -> the others get hidden by JS.
  drv$run_js(
    paste0(
      "var i = document.querySelector('input.blockr-search');",
      "i.value = 'beaver';",
      "i.dispatchEvent(new Event('input', {bubbles: true}));"
    )
  )
  drv$wait_for_js(
    "document.querySelectorAll('tbody tr.blockr-hidden-search').length > 0",
    timeout = 5000
  )
  expect_gt(count_hidden(), 0)

  # Clearing the box restores every row (no JS-hidden rows remain).
  drv$run_js(
    paste0(
      "var i = document.querySelector('input.blockr-search');",
      "i.value = '';",
      "i.dispatchEvent(new Event('input', {bubbles: true}));"
    )
  )
  drv$wait_for_js(
    "document.querySelectorAll('tbody tr.blockr-hidden-search').length === 0",
    timeout = 5000
  )
  expect_equal(count_hidden(), 0)
})
