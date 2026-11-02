# A nested groups table in a block card draws its fold chevrons 14px in from
# the card's left edge, header and rows alike, as 0.2.200's summarize table
# did. The table runs to the card's edges (table.css), and the label column's
# padding did not make up for it: the chevrons sat 3px in, the header's
# fold-all 2px outside the card.
#
# Skipped without a headless browser and under R CMD check. On the Mac,
# Chrome cannot start in Claude's sandbox: start
# blockr.dev/scripts/chrome-remote.sh outside it and source
# blockr.dev/scripts/use-remote-chrome.R first.

test_that("a nested groups table keeps its chevrons off the card edge", {
  skip_without_browser()
  skip_if_not_installed("pkgload")
  skip_if_not_installed("blockr.dock")
  Sys.setenv(NOT_CRAN = "true")
  withr::local_envvar(BLOCKR_VIZ_SRC = normalizePath(test_path("..", "..")))
  configure_chromote()
  app <- shinytest2::AppDriver$new(
    test_path("apps", "table-groups-chevron"), name = "table-groups-chevron",
    width = 1000, height = 800,
    load_timeout = 120 * 1000, timeout = 30 * 1000
  )
  withr::defer(app$stop())
  app$wait_for_js(
    "document.querySelectorAll('.vz-table-groups-table tbody .vz-table-chev').length === 4"
  )

  # Offsets from the card's left edge (the dock panel).
  p <- app$get_js("(() => {
    const t = document.querySelector('.vz-table-groups-table');
    const card = t.closest('.dockview-panel').getBoundingClientRect().left;
    const left = (el) => el.getBoundingClientRect().left - card;
    const text = (el) => {
      const w = document.createTreeWalker(el, NodeFilter.SHOW_TEXT);
      let n;
      while ((n = w.nextNode()) && !n.textContent.trim());
      const r = document.createRange();
      r.selectNodeContents(n);
      return r.getClientRects()[0].left - card;
    };
    return {
      fold: left(t.querySelector('thead .vz-table-foldall .vz-table-chev')),
      rows: [...t.querySelectorAll('tbody .vz-table-chev')].map(left),
      head: text(t.querySelector('thead th.vz-table-stub-header .blockr-col-name')),
      label: text(t.querySelector('tbody tr.is-parent td.vz-table-groups-label-col'))
    };
  })()")

  expect_lt(abs(p$fold - 14), 1)
  expect_lt(max(abs(unlist(p$rows) - 14)), 1)
  # The header's names and the parents' labels share one edge.
  expect_lt(abs(p$head - p$label), 1)
})
