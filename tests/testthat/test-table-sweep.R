# The affordance sweep: every view and tab of a board with the table block in
# each layout and look, every word in a block's sentence, every "+ setting"
# offer and every control in the gear is clicked, and the click has to do
# something (apps/table-sweep/sweep.js).
#
#   DEAD   looks clickable, a click changes nothing
#   LIES   a word's pick changes the sentence but not the table
#   GEARS  a block shows more than one gear
#
# Plus no "blockr:" console errors, which the sentence painter writes when R
# marks a word that no control backs.
#
# The app loads blockr.viz from this source tree (BLOCKR_VIZ_SRC), not the
# installed version. Skipped without a headless browser and under R CMD check,
# like test-shinytest2.R. Takes a few minutes.

run_sweep <-
  run_browser_tests() &&
  requireNamespace("blockr.dock", quietly = TRUE) &&
  requireNamespace("pkgload", quietly = TRUE)

test_that("every control on the table sweep board does something", {
  skip_if_not(run_sweep, "no headless browser, or under R CMD check")
  Sys.setenv(NOT_CRAN = "true")
  withr::local_envvar(BLOCKR_VIZ_SRC = normalizePath(test_path("..", "..")))
  configure_chromote()

  app <- shinytest2::AppDriver$new(
    test_path("apps", "table-sweep"),
    name = "table-sweep", width = 1400, height = 1000,
    load_timeout = 120 * 1000, timeout = 30 * 1000
  )
  withr::defer(app$stop())
  app$wait_for_idle()

  js <- paste(readLines(test_path("apps", "table-sweep", "sweep.js")),
              collapse = "\n")
  app$run_js(js)
  out <- app$get_js(
    "blockrSweep.all().then(r => JSON.stringify({results: r, errors: blockrSweep.state.errors}))",
    timeout = 30 * 60 * 1000
  )
  val <- jsonlite::fromJSON(out)
  res <- val$results

  expect_gt(nrow(res), 50)
  bad <- res[res$effect %in% c("DEAD", "LIES", "GEARS"), , drop = FALSE]
  show <- function(d) {
    paste0(d$view, " / ", d$block, " / ", d$where, " ", d$what, ' "',
           d$label, '": ', d$effect, collapse = "\n")
  }
  expect(nrow(bad) == 0L,
         paste0(nrow(bad), " controls do nothing or mislead:\n", show(bad)))

  errs <- grep("^blockr:", unique(val$errors), value = TRUE)
  expect(length(errs) == 0L, paste(c("console errors:", errs), collapse = "\n"))
})
