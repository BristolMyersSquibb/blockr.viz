# A board saved by production (blockr.viz 0.2.200), restored in a browser:
# fixtures/board-0.2.200.json, written by dev/board-0.2.200.R. It holds a
# click on a bar chart, a summarize table and a table, a scatter on dates,
# and a prepare script set away from its default. Every block has to draw
# and every click has to come back marked, named and filtering.
#
# Served by apps/viz-flows, which serves a saved board when
# BLOCKR_FLOWS_RESTORE names one, with blockr.viz from this source tree.
# Skipped without a headless browser and under R CMD check. On the Mac,
# Chrome cannot start in Claude's sandbox: start
# blockr.dev/scripts/chrome-remote.sh outside it and source
# blockr.dev/scripts/use-remote-chrome.R first.

run_restore <-
  run_browser_tests() &&
  requireNamespace("blockr.dock", quietly = TRUE) &&
  requireNamespace("blockr.dm", quietly = TRUE) &&
  requireNamespace("pkgload", quietly = TRUE)

RESTORE_JS <- "
(function () {
  if (window.restored) return;
  const txt = (e) => ((e && e.textContent) || '').replace(/\\s+/g, ' ').trim();
  const host = (id) => document.querySelector(
    '[id$=\"-block_' + id + '-expr-chart\"], [id$=\"-block_' + id + '-expr-table\"]');
  const frame = (id) => { const h = host(id); return h && h.querySelector('.vz-frame'); };
  const face = (id) => {
    const f = frame(id);
    if (!f) return null;
    const el = f.querySelector('[_echarts_instance_]');
    const inst = el && window.echarts.getInstanceByDom(el);
    let cats = [];
    if (inst) {
      const o = inst.getOption();
      const ax = [].concat(o.xAxis || [], o.yAxis || []).find((a) => a && a.type === 'category');
      cats = ax ? ax.data.map(String) : [];
    }
    return {
      drawn: !!inst, cats,
      title: txt(f.querySelector('.vz-title')), sub: txt(f.querySelector('.vz-subtitle')),
      status: txt(f.querySelector('.vz-status-text')),
      rows: f.querySelectorAll('tbody tr.vz-table-row').length,
      lit: [...f.querySelectorAll('tr.vz-table-row-active')].map((r) => txt(r.children[0])),
      script: [...f.querySelectorAll('.vz-script-band input')].map((i) => i.value)
    };
  };
  const ready = (ids) => ids.every((id) => {
    const f = face(id);
    return !!f && (f.drawn || f.rows > 0);
  });
  window.restored = { face, ready };
})();
"

restore_face <- function(app, id) {
  f <- app$get_js(sprintf("restored.face('%s')", id))
  f[c("cats", "lit", "script")] <- lapply(f[c("cats", "lit", "script")],
                                          function(x) as.character(unlist(x)))
  f
}

test_that("a board saved on production restores drawn, with its clicks", {
  skip_if_not(run_restore, "no headless browser, or under R CMD check")
  Sys.setenv(NOT_CRAN = "true")
  withr::local_envvar(
    BLOCKR_VIZ_SRC = normalizePath(test_path("..", "..")),
    BLOCKR_FLOWS_RESTORE = normalizePath(test_path("fixtures", "board-0.2.200.json"))
  )
  configure_chromote()
  app <- shinytest2::AppDriver$new(
    test_path("apps", "viz-flows"), name = "restore-0.2.200",
    width = 1400, height = 1000, load_timeout = 180 * 1000, timeout = 60 * 1000
  )
  withr::defer(app$stop())
  app$wait_for_idle()
  app$run_js(RESTORE_JS)

  ids <- c("c_bar", "c_date", "c_script", "t_grp", "t_rec")
  t0 <- Sys.time()
  while (!isTRUE(app$get_js(sprintf("restored.ready(%s)", jsonlite::toJSON(ids))))) {
    if (difftime(Sys.time(), t0, units = "secs") > 60) stop("the restored board did not draw")
    Sys.sleep(1)
  }
  app$wait_for_idle()
  Sys.sleep(1)
  result <- function(id) app$get_values(export = "result")$export$result[[id]]
  ae <- result("d")

  bar <- restore_face(app, "c_bar")
  expect_true(bar$drawn)
  expect_identical(bar$title, "AEs by AETERM")
  expect_identical(bar$status, "Filtered: AETERM = Rash")
  expect_identical(nrow(result("c_bar")), sum(ae$AETERM == "Rash"))

  expect_true(restore_face(app, "c_date")$drawn)

  sc <- restore_face(app, "c_script")
  expect_setequal(sc$cats, sort(unique(ae$AETERM))[1:2])
  expect_identical(sc$script, "2")

  grp <- restore_face(app, "t_grp")
  expect_identical(grp$sub, "By ARM, top 3")
  expect_identical(grp$status, "Filtered: ARM = Low")
  expect_identical(grp$lit, "Low")
  expect_true(all(result("t_grp")$ARM %in% "Low"))

  rec <- restore_face(app, "t_rec")
  expect_identical(rec$status, "Filtered: USUBJID = S04")
  expect_true(all(rec$lit == "S04"))
  expect_true(all(result("t_rec")$USUBJID == "S04"))
})
