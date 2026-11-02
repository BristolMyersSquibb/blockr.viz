# A `.strong` row of a composer table is bold across the row, its row label
# included, and the other rows are not (apps/table-strong-rows). 8076025e
# raised the stub rule's specificity with `:not(.vz-table-groups-table)`, so
# the stub stayed at 400 while the value cells on the row were 600. 0.2.200
# drew the whole row bold. A row over deeper rows with no values of its own
# is a heading across the row, bold.
#
# Skipped without a headless browser and under R CMD check. On the Mac,
# Chrome cannot start in Claude's sandbox: start
# blockr.dev/scripts/chrome-remote.sh outside it and source
# blockr.dev/scripts/use-remote-chrome.R first.

test_that("composer tables: a strong row is bold, an empty heading spans the row", {
  skip_without_browser()
  skip_if_not_installed("pkgload")
  Sys.setenv(NOT_CRAN = "true")
  withr::local_envvar(BLOCKR_VIZ_SRC = normalizePath(test_path("..", "..")))
  configure_chromote()
  app <- shinytest2::AppDriver$new(
    test_path("apps", "table-strong-rows"), name = "table-strong-rows",
    width = 1000, height = 800,
    load_timeout = 120 * 1000, timeout = 30 * 1000
  )
  withr::defer(app$stop())
  app$wait_for_js("document.querySelectorAll('#composer tbody tr').length === 3")
  # A row over deeper rows with no values (gtsummary's variable rows) is a
  # heading: one bold cell across the row, its statistics at 400 under it.
  heads <- app$get_js("[...document.querySelectorAll('#t1 tbody tr.vz-table-row')].map((r) => ({
    text: r.cells[0].textContent.trim(), cells: r.cells.length,
    colspan: r.cells[0].colSpan, weight: getComputedStyle(r.cells[0]).fontWeight
  }))")
  expect_identical(vapply(heads, `[[`, "", "text"),
                   c("AGE (years)", "Mean (SD)", "Min, Max", "SEX", "F", "M"))
  expect_identical(vapply(heads, `[[`, 1L, "cells"), c(1L, 3L, 3L, 1L, 3L, 3L))
  expect_identical(vapply(heads, `[[`, 1L, "colspan"), c(3L, 1L, 1L, 3L, 1L, 1L))
  expect_identical(vapply(heads, `[[`, "", "weight"),
                   c("600", "400", "400", "600", "400", "400"))

  weights <- function(label) {
    app$get_js(sprintf("(() => {
      const tr = [...document.querySelectorAll('#composer tbody tr')]
        .find((r) => r.cells[0].textContent.trim() === '%s');
      const w = (td) => getComputedStyle(td).fontWeight;
      return { stub: w(tr.cells[0]), value: w(tr.cells[1]) };
    })()", label))
  }

  bold <- weights("Alanine Aminotransferase (U/L)")
  expect_identical(bold$stub, "600")
  expect_identical(bold$value, "600")
  plain <- weights("Baseline")
  expect_identical(plain$stub, "400")
  expect_identical(plain$value, "400")
})
