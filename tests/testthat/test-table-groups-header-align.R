# The lines of a groups table's column header share one edge: a chart
# column's name, its sub line and its axis start on the left, as in
# 0.2.200; a number column's name and its N line end on the right. A nested
# groups table carries .vz-table-structured, and the Table-1 rule that
# right-aligns arm headers pushed a chart column's sub line ("Onset to end ·
# N = 20") to the right, away from its name and its axis.
#
# Skipped without a headless browser and under R CMD check. On the Mac,
# Chrome cannot start in Claude's sandbox: start
# blockr.dev/scripts/chrome-remote.sh outside it and source
# blockr.dev/scripts/use-remote-chrome.R first.

test_that("a groups header's lines share one edge", {
  skip_without_browser()
  skip_if_not_installed("pkgload")
  skip_if_not_installed("blockr.dock")
  Sys.setenv(NOT_CRAN = "true")
  withr::local_envvar(BLOCKR_VIZ_SRC = normalizePath(test_path("..", "..")))
  configure_chromote()
  app <- shinytest2::AppDriver$new(
    test_path("apps", "table-groups-header-align"),
    name = "table-groups-header-align",
    width = 1000, height = 800,
    load_timeout = 120 * 1000, timeout = 30 * 1000
  )
  withr::defer(app$stop())
  app$wait_for_js(
    "document.querySelectorAll('.vz-table-groups-table thead .vz-table-groups-axis-in span').length > 0"
  )

  # Left and right edges of each header line, from the cell's own left.
  p <- app$get_js("(() => {
    const t = document.querySelector('.vz-table-groups-table');
    const ths = [...t.querySelectorAll('thead th.vz-table-col-header.leaf')];
    return ths.map((th) => {
      const x = th.getBoundingClientRect().left;
      const box = (q) => {
        const e = th.querySelector(q);
        if (!e) return null;
        const r = e.getBoundingClientRect();
        return { l: r.left - x, r: r.right - x };
      };
      return {
        txt: th.classList.contains('vz-table-col-txt'),
        name: box('.blockr-col-name'),
        sub: box('.vz-table-th-subrow .blockr-col-label'),
        axis: box('.vz-table-groups-axis-in')
      };
    });
  })()")

  chart <- Filter(function(h) h$txt && !is.null(h$axis), p)
  num <- Filter(function(h) !h$txt && !is.null(h$sub), p)
  expect_length(chart, 2L)
  expect_length(num, 1L)
  for (h in chart) {
    expect_lt(abs(h$sub$l - h$name$l), 1)
    expect_lt(abs(h$axis$l - h$name$l), 1)
  }
  expect_lt(abs(num[[1]]$sub$r - num[[1]]$name$r), 1)
})
