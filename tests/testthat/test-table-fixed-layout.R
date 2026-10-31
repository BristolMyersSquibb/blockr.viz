# A table with R's column widths keeps them in a card narrower than the
# columns: it scrolls sideways, and the row labels stay on one line in a
# first column as wide as R made it (apps/table-width). The rewrite drew these
# tables in auto layout, which squeezed a long label such as "Absolute
# Neutrophil Count (10^9/L)" over four lines in a narrow first column.
# 0.2 set table-layout: fixed on them; table.css does now.
#
# Skipped without a headless browser and under R CMD check. On the Mac,
# Chrome cannot start in Claude's sandbox: start
# blockr.dev/scripts/chrome-remote.sh outside it and source
# blockr.dev/scripts/use-remote-chrome.R first.

test_that("the first column keeps its width in a narrow card", {
  skip_without_browser()
  skip_if_not_installed("pkgload")
  Sys.setenv(NOT_CRAN = "true")
  withr::local_envvar(BLOCKR_VIZ_SRC = normalizePath(test_path("..", "..")))
  configure_chromote()
  app <- shinytest2::AppDriver$new(
    test_path("apps", "table-width"), name = "table-width",
    width = 1000, height = 900,
    load_timeout = 120 * 1000, timeout = 30 * 1000
  )
  withr::defer(app$stop())
  app$wait_for_js("document.querySelectorAll('table.blockr-table').length === 2")

  probe <- function(id, label) {
    app$get_js(sprintf("(() => {
      const t = document.querySelector('#%s table.blockr-table');
      const td = [...t.querySelectorAll('tbody td')]
        .find((d) => d.textContent.trim() === '%s');
      return {
        layout: getComputedStyle(t).tableLayout,
        col: parseFloat(t.querySelector('col').style.width),
        width: td.getBoundingClientRect().width,
        height: td.getBoundingClientRect().height,
        line: parseFloat(getComputedStyle(td).lineHeight) || 20
      };
    })()", id, label))
  }

  for (id in c("structured", "flat")) {
    p <- probe(id, "Absolute Neutrophil Count (10^9/L)")
    expect_identical(p$layout, "fixed", label = id)
    expect_gte(p$width, p$col - 1)
    # One line: the cell is a line plus its padding, well short of two.
    expect_lt(p$height, 2 * p$line, label = id)
  }
})
