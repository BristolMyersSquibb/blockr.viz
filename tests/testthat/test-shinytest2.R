# Real browser-driven e2e tests for blockr.viz, modeled on blockr.dplyr's
# test-shinytest2.R. ONE shared board app (apps/viz-e2e) is launched once and
# driven through a headless chromium with shinytest2. We exercise the
# client-side JS contract end-to-end:
#
#   * the table search box (DOM row hiding),
#   * click-to-filter drill: the `<elem>_action` message the table JS emits ->
#     reactive state -> the block re-filters its own output (the data that
#     flows downstream), read back via the board's exported `result`,
#   * a gear-popover `config` message re-rendering the table,
#   * the cell-visual data-bar markup.
#
# Server/reactive logic is covered without a browser by test-table-block-*.R;
# this layer adds the real browser + custom-message handler + re-render path.
#
# Skipped where no headless browser exists, and under R CMD check (launching
# chromium there leaves temp detritus -> a check NOTE, and is flaky). Runs
# under devtools::test() / CI, which is where the JS is meant to be exercised.

library(shinytest2)

# ---------------------------------------------------------------------------
# Shared app instance (test fixture). Built at file load like blockr.dplyr,
# but guarded: in environments with no browser (or under R CMD check) `app`
# stays NULL and every test skips via skip_if_no_app().
# ---------------------------------------------------------------------------

run_browser <-
  !nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) &&
  requireNamespace("shinytest2", quietly = TRUE) &&
  requireNamespace("chromote", quietly = TRUE) &&
  chromote_works()

app <- NULL
if (run_browser) {
  # shinytest2 refuses to run "On CRAN"; serve()'s app trips that guard unless
  # NOT_CRAN is set. We are explicitly not on CRAN here.
  Sys.setenv(NOT_CRAN = "true")
  configure_chromote()

  app <- tryCatch(
    AppDriver$new(
      test_path("apps", "viz-e2e"),
      name         = "viz-e2e",
      load_timeout = 60 * 1000,
      timeout      = 20 * 1000
    ),
    error = function(e) {
      message("viz-e2e app launch failed: ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(app)) {
    app$wait_for_idle()
    withr::defer(app$stop(), testthat::teardown_env())
  }
}

skip_if_no_app <- function() {
  testthat::skip_if(is.null(app), "viz-e2e browser app unavailable")
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Emit the `<elem>_action` message a block's JS sends on a drill/gear edit.
# Each renderer builds a root element with id `ns("<root>")` and its JS listens
# on `<that id>_action`; on a board the id resolves deterministically. We send
# that message directly — the same contract the row/bar/card-click and gear
# popover JS use. (The chart's *real canvas click* is driven separately by
# echarts_canvas_click(), which exercises the chart.js click handler too.)
send_action <- function(input_id, payload) {
  json <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  app$run_js(sprintf(
    "Shiny.setInputValue('%s', %s, {priority: 'event'});", input_id, json
  ))
  app$wait_for_idle()
}

# Per-renderer action input ids (root element + "_action").
table_action <- function(id) sprintf("board-block_%s-expr-drilldown_table_block_action", id)
chart_action <- function(id) sprintf("board-block_%s-expr-drilldown_block_action", id)
tile_action  <- function(id) sprintf("board-block_%s-expr-tile_block_action", id)

send_table_action <- function(block_id, payload) {
  send_action(table_action(block_id), payload)
}

# Fire a GENUINE click on an ECharts chart at a data point's pixel, driving the
# real chart.js `chart.on('click')` handler in headless chromium. ECharts paints
# to <canvas> and its high-level click is synthesized from native press/release
# (a bare DOM event or zrender handler.dispatch doesn't trigger it), so we use a
# true CDP Input.dispatchMouseEvent through shinytest2's own chromote session —
# no Playwright. `at` is the [value, category] data point passed to the chart's
# convertToPixel(); computed right before the click so the layout is current.
#
# NB: chart.js schedules a delayed resize() ~300ms after each (re)render, so we
# settle past it before reading coordinates, otherwise convertToPixel reads a
# stale layout and the click lands on the wrong bar.
# Scroll a chart block into view, wait for its canvas, and settle past chart.js's
# deferred resize(); returns the chart-container selector.
chart_settle <- function(block_id) {
  sel <- sprintf("#board-block_%s-expr-drilldown_block", block_id)
  app$run_js(sprintf(
    "document.querySelector('%s').scrollIntoView({block: 'center'});", sel
  ))
  app$wait_for_js(sprintf("!!document.querySelector('%s canvas')", sel),
                  timeout = 15000)
  app$wait_for_idle()
  Sys.sleep(0.8) # past chart.js's deferred resize()
  sel
}

# A true left click at viewport (x, y) via CDP, then settle.
cdp_click <- function(x, y) {
  s <- app$get_chromote_session()
  s$Input$dispatchMouseEvent(type = "mousePressed", x = x, y = y,
                             button = "left", buttons = 1, clickCount = 1)
  s$Input$dispatchMouseEvent(type = "mouseReleased", x = x, y = y,
                             button = "left", buttons = 1, clickCount = 1)
  app$wait_for_idle()
  Sys.sleep(0.4)
  app$wait_for_idle()
}

# --- Gear settings band (drilldown-config.js) ------------------------------
# The gear opens an in-flow .blockr-settings band inside the block (design-
# system pilot; no more <body>-portaled popover). The OPEN one carries the
# --open modifier. These helpers scope to that open band.
POPOVER_OPEN_JS <- "document.querySelector('.blockr-settings.blockr-settings--open')"

gear_open <- function(block_id) {
  app$run_js(sprintf(
    "document.querySelector('#board-block_%s-expr-drilldown_block .blockr-gear-btn').click();",
    block_id
  ))
  app$wait_for_idle()
  Sys.sleep(0.3)
}

popover_is_open <- function() {
  isTRUE(app$get_js(sprintf("!!(%s)", POPOVER_OPEN_JS)))
}

popover_active_type <- function() {
  app$get_js(sprintf(
    "(function(){var p=%s; if(!p) return null; var a=p.querySelector('.dd-type-btn.dd-type-active, .dd-type-tile.dd-type-active'); return a?a.textContent:null;})()",
    POPOVER_OPEN_JS
  ))
}

popover_click_type <- function(type) {
  app$run_js(sprintf(
    "(function(){var p=%s; Array.from(p.querySelectorAll('.dd-type-btn, .dd-type-tile')).filter(function(b){return b.textContent==='%s';})[0].click();})()",
    POPOVER_OPEN_JS, type
  ))
  app$wait_for_idle()
  Sys.sleep(0.5)
}

# Click an ECharts grid data point (bar / scatter / line). `at` is the
# [value, category-or-value] pair the chart's convertToPixel() maps to a pixel,
# read right before the click so the (settled) layout is current.
echarts_canvas_click <- function(block_id, at) {
  sel <- chart_settle(block_id)
  cat_or_val <- if (is.character(at[[2]])) sprintf("'%s'", at[[2]]) else at[[2]]
  pt <- jsonlite::fromJSON(app$get_js(sprintf(
    "(function(){
       var div  = document.querySelector('%s .dd-chart-grid').querySelector('div');
       var inst = window.echarts.getInstanceByDom(div);
       var px   = inst.convertToPixel({gridIndex: 0}, [%s, %s]);
       var r    = div.getBoundingClientRect();
       return JSON.stringify({x: r.left + px[0], y: r.top + px[1]});
     })()",
    sel, at[[1]], cat_or_val
  )))
  cdp_click(pt$x, pt$y)
}

# Click a pie slice by name. Pie has no convertToPixel, so we read the slice's
# laid-out geometry (center + mid-angle + radius) and click its mid-ring point
# (ECharts: cx + cos(mid)*rMid, cy + sin(mid)*rMid).
echarts_pie_click <- function(block_id, slice_name) {
  sel <- chart_settle(block_id)
  pt <- jsonlite::fromJSON(app$get_js(sprintf(
    "(function(){
       var div  = document.querySelector('%s .dd-chart-grid').querySelector('div');
       var inst = window.echarts.getInstanceByDom(div);
       var dt   = inst.getModel().getSeriesByIndex(0).getData();
       var idx = -1; for (var k = 0; k < dt.count(); k++) if (dt.getName(k) === '%s') idx = k;
       var L = dt.getItemLayout(idx);
       var mid = (L.startAngle + L.endAngle) / 2, rMid = (L.r0 + L.r) / 2;
       var r = div.getBoundingClientRect();
       return JSON.stringify({x: r.left + L.cx + Math.cos(mid)*rMid,
                              y: r.top  + L.cy + Math.sin(mid)*rMid});
     })()",
    sel, slice_name
  )))
  cdp_click(pt$x, pt$y)
}

# Programmatic rectangular brush over a data-coordinate range (no mouse drag
# needed): drives ECharts' brush component, whose brushSelected handler emits
# the chart's range filter.
echarts_brush <- function(block_id, xrange, yrange) {
  sel <- chart_settle(block_id)
  app$run_js(sprintf(
    "(function(){
       var div  = document.querySelector('%s .dd-chart-grid').querySelector('div');
       var inst = window.echarts.getInstanceByDom(div);
       inst.dispatchAction({type: 'brush', areas: [{
         brushType: 'rect', xAxisIndex: 0, yAxisIndex: 0,
         coordRange: [[%s, %s], [%s, %s]]
       }]});
     })()",
    sel, xrange[[1]], xrange[[2]], yrange[[1]], yrange[[2]]
  ))
  app$wait_for_idle()
  Sys.sleep(0.8)
  app$wait_for_idle()
}

# CSS-escaped id selector for a block's rendered table chrome (search box +
# <table> live inside it). The id has no CSS-special chars beyond hyphens.
dt_result <- function(block_id) {
  sprintf("#board-block_%s-expr-dt_result", block_id)
}

# A block's exported result data frame (the data it passes downstream).
get_block_result <- function(block_id) {
  app$get_values()$export$result[[block_id]]
}

# ===========================================================================
# SEARCH BOX (client-side row hiding)
# ===========================================================================

test_that("table search box filters rows in the browser (JS)", {
  skip_if_no_app()

  scope <- dt_result("table")
  count_hidden <- function() {
    app$get_js(sprintf(
      "document.querySelectorAll('%s tbody tr.blockr-hidden-search').length",
      scope
    ))
  }

  # Nothing hidden initially.
  expect_equal(count_hidden(), 0)

  # Type a query matching exactly one region -> the others get JS-hidden.
  app$run_js(sprintf(
    "var i = document.querySelector('%s input.blockr-search');
     i.value = 'East';
     i.dispatchEvent(new Event('input', {bubbles: true}));",
    scope
  ))
  app$wait_for_js(sprintf(
    "document.querySelectorAll('%s tbody tr.blockr-hidden-search').length > 0",
    scope
  ), timeout = 5000)
  expect_gt(count_hidden(), 0)

  # Clearing the box restores every row.
  app$run_js(sprintf(
    "var i = document.querySelector('%s input.blockr-search');
     i.value = '';
     i.dispatchEvent(new Event('input', {bubbles: true}));",
    scope
  ))
  app$wait_for_js(sprintf(
    "document.querySelectorAll('%s tbody tr.blockr-hidden-search').length === 0",
    scope
  ), timeout = 5000)
  expect_equal(count_hidden(), 0)
})

# ===========================================================================
# CLICK-TO-FILTER DRILL (action message -> re-filter -> downstream data)
# ===========================================================================

test_that("drill: a single-value filter action filters the block's output", {
  skip_if_no_app()

  send_table_action("table", list(
    action = "filter", column = "region", values = list("North")
  ))

  res <- get_block_result("table")
  expect_equal(nrow(res), 2L)
  expect_true(all(res$region == "North"))
})

test_that("drill: a multi-value filter action uses %in% and keeps all matches", {
  skip_if_no_app()

  send_table_action("table", list(
    action = "filter", column = "region", values = list("North", "South")
  ))

  res <- get_block_result("table")
  expect_equal(nrow(res), 4L)
  expect_setequal(unique(res$region), c("North", "South"))
})

test_that("drill: clearing the filter (empty values) restores all rows", {
  skip_if_no_app()

  send_table_action("table", list(
    action = "filter", column = "region", values = list()
  ))

  res <- get_block_result("table")
  expect_equal(nrow(res), 6L)
})

# ===========================================================================
# GEAR-POPOVER CONFIG (config message -> state -> re-render)
# ===========================================================================

test_that("config: setting drill via a config action updates the table markup", {
  skip_if_no_app()

  onclick_col <- function() {
    app$get_js(sprintf(
      "document.querySelector('%s table').getAttribute('data-dt-onclick-col')",
      dt_result("bars")
    ))
  }

  # `bars` starts with no drill -> no onclick column on the <table>.
  expect_null(onclick_col())

  send_table_action("bars", list(
    action = "config", param = "drill", value = "region"
  ))
  expect_equal(onclick_col(), "region")

  # "(none)" clears it back off.
  send_table_action("bars", list(
    action = "config", param = "drill", value = "(none)"
  ))
  expect_null(onclick_col())
})

# ===========================================================================
# CELL VISUALS (data bars render as a CSS gradient)
# ===========================================================================

test_that("cell visuals: numeric cells render data-bar gradients", {
  skip_if_no_app()

  scope <- dt_result("bars")

  # Every numeric cell in `bars` carries a left-anchored linear-gradient.
  n_bars <- app$get_js(sprintf(
    "document.querySelectorAll(
       '%s table tbody td[style*=\"linear-gradient\"]').length",
    scope
  ))
  expect_gt(n_bars, 0)

  # The largest revenue (North = 100, the column max) fills to 100%.
  has_full_bar <- app$get_js(sprintf(
    "Array.from(document.querySelectorAll(
       '%s table tbody td[style*=\"linear-gradient\"]'))
       .some(function(td){ return td.getAttribute('style').indexOf('100%%') > -1; })",
    scope
  ))
  expect_true(has_full_bar)
})

test_that("cell visuals: diverging heatmap paints solid cell backgrounds", {
  skip_if_no_app()

  scope <- dt_result("heat")

  # Diverging mode paints each numeric cell a solid hex background (no
  # gradient) plus a contrasting text color.
  n_bg <- app$get_js(sprintf(
    "document.querySelectorAll('%s table tbody td[style*=\"background:#\"]').length",
    scope
  ))
  expect_gt(n_bg, 0)

  # These are solid fills, not the data-bar gradient.
  n_grad <- app$get_js(sprintf(
    "document.querySelectorAll('%s table tbody td[style*=\"linear-gradient\"]').length",
    scope
  ))
  expect_equal(n_grad, 0)
})

# ===========================================================================
# CHART: click-to-filter action (message contract, not the canvas hit-test)
# ===========================================================================

test_that("chart: a filter action filters the chart block's output", {
  skip_if_no_app()

  send_action(chart_action("chart"), list(
    action = "filter", filter_type = "categorical",
    column = "region", values = list("North")
  ))

  res <- get_block_result("chart")
  expect_true(all(res$region == "North"))
  expect_lt(nrow(res), 6L)

  # Clear it again.
  send_action(chart_action("chart"), list(
    action = "filter", filter_type = "categorical",
    column = "region", values = list()
  ))
  expect_equal(nrow(get_block_result("chart")), 6L)
})

test_that("chart: a real ECharts canvas click drills the bar (no Playwright)", {
  skip_if_no_app()

  # Reset any prior filter so the baseline is the full frame.
  send_action(chart_action("chart"), list(
    action = "filter", filter_type = "categorical",
    column = "region", values = list()
  ))
  expect_equal(nrow(get_block_result("chart")), 6L)

  # Click the middle of the "North" bar (sum revenue 150; value 75 is mid-bar).
  # This runs chart.js's chart.on('click') -> _sendCategoricalFilter path for real.
  echarts_canvas_click("chart", at = list(75, "North"))

  res <- get_block_result("chart")
  expect_setequal(unique(res$region), "North")
  expect_equal(nrow(res), 2L)
})

test_that("chart: a real pie-slice click drills the group", {
  skip_if_no_app()

  echarts_pie_click("chart_pie", "North")

  res <- get_block_result("chart_pie")
  expect_setequal(unique(res$region), "North")
  expect_equal(nrow(res), 2L)
})

test_that("chart: a real scatter-point click drills the point's region", {
  skip_if_no_app()

  # Click North's (revenue=100, profit=10) point; the series is split by
  # region, so the click filters to that region.
  echarts_canvas_click("chart_scatter", at = list(100, 10))

  res <- get_block_result("chart_scatter")
  expect_setequal(unique(res$region), "North")
  expect_equal(nrow(res), 2L)
})

test_that("chart: a rectangular brush filters to the selected x/y range", {
  skip_if_no_app()

  # Brush a tight rect around the single point (revenue=100, profit=10).
  echarts_brush("chart_brush", xrange = list(90, 110), yrange = list(8, 12))

  res <- get_block_result("chart_brush")
  expect_equal(nrow(res), 1L)
  expect_equal(res$revenue, 100)
})

# ===========================================================================
# GEAR POPOVER ENGINE (drilldown-config.js) — real DOM, not the action input
# ===========================================================================

test_that("gear popover: a chart-type switch keeps it open and the block live", {
  skip_if_no_app()

  cfg <- "#board-block_chart_cfg-expr-drilldown_block"
  app$run_js(sprintf("document.querySelector('%s').scrollIntoView({block:'center'});", cfg))
  app$wait_for_js(sprintf("!!document.querySelector('%s canvas')", cfg), timeout = 15000)
  app$wait_for_idle()

  gear_open("chart_cfg")
  expect_true(popover_is_open())
  expect_equal(popover_active_type(), "bar")

  # Switch bar -> pie through the real type picker. This is where the historic
  # family-switch freeze lived: the popover must stay open (wasOpen -> reopen)
  # and the chart must re-render rather than wedge.
  popover_click_type("pie")
  expect_true(popover_is_open())
  expect_equal(popover_active_type(), "pie")
  expect_true(app$get_js(sprintf("!!document.querySelector('%s canvas')", cfg)))

  # Not wedged: a drill still flows through to the (re-filtered) output.
  send_action(chart_action("chart_cfg"), list(
    action = "filter", filter_type = "categorical",
    column = "region", values = list("South")
  ))
  res <- get_block_result("chart_cfg")
  expect_setequal(unique(res$region), "South")
  expect_equal(nrow(res), 2L)
})

# ===========================================================================
# TILE: click-to-filter action (card / matrix-row click)
# ===========================================================================

test_that("tile: a filter action filters the tile block's output", {
  skip_if_no_app()

  send_action(tile_action("tile"), list(
    action = "filter", column = "region", values = list("South")
  ))

  res <- get_block_result("tile")
  expect_true(all(res$region == "South"))
  expect_equal(nrow(res), 2L)

  send_action(tile_action("tile"), list(
    action = "filter", column = "region", values = list()
  ))
  expect_equal(nrow(get_block_result("tile")), 6L)
})

# ===========================================================================
# SUMMARY TABLE / GT TABLE: render smoke
# ===========================================================================

test_that("summary_table gear: toggling 'overall' adds a Total column to the output", {
  skip_if_no_app()

  scope <- "#board-block_summary-expr-summary_input"
  before <- get_block_result("summary")
  expect_false("Total" %in% names(before))

  # Open the gear and tick the "Overall column" checkbox (design-system band).
  # This drives summary-table-block.js's real gear -> state -> R -> output.
  app$run_js(sprintf("document.querySelector('%s .blockr-gear-btn').scrollIntoView({block:'center'});", scope))
  app$run_js(sprintf("document.querySelector('%s .blockr-gear-btn').click();", scope))
  app$wait_for_idle()
  Sys.sleep(0.3)
  app$run_js(sprintf(
    "(function(){var b=Array.from(document.querySelectorAll('%s .blockr-checkbox')).filter(function(x){return /overall/i.test(x.textContent);})[0]; if(b) b.querySelector('input').click();})()",
    scope
  ))
  app$wait_for_idle()
  Sys.sleep(0.5)
  app$wait_for_idle()

  after <- get_block_result("summary")
  expect_true("Total" %in% names(after))
  expect_gt(ncol(after), ncol(before))
})

test_that("tile: delta secondary renders and a matrix-row click drills", {
  skip_if_no_app()

  scope <- "#board-block_tile_x-expr-tile_result"
  app$run_js(sprintf("document.querySelector('%s').scrollIntoView({block:'center'});", scope))
  app$wait_for_idle()

  # Delta-styled secondaries render as colored .tk-delta spans (one per group).
  n_delta <- app$get_js(sprintf("document.querySelectorAll('%s .tk-delta').length", scope))
  expect_gt(n_delta, 0)

  # A real click on a matrix row (table layout) drills on its group.
  app$run_js(sprintf(
    "(function(){var r=Array.from(document.querySelectorAll('%s [data-group]')).filter(function(x){return x.getAttribute('data-group')==='South';})[0]; if(r) r.click();})()",
    scope
  ))
  app$wait_for_idle()
  Sys.sleep(0.4)
  app$wait_for_idle()

  res <- get_block_result("tile_x")
  expect_setequal(unique(res$region), "South")
  expect_equal(nrow(res), 2L)
})

test_that("summary_table: produces the wide annotated summary frame", {
  skip_if_no_app()

  res <- get_block_result("summary")
  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 0)
  # The wide annotated df carries per-row identity (.variable) and one baked
  # cell column per region level; variable headers are synthesized by the
  # renderer from .variable_label runs, never materialized as rows.
  expect_true(all(c(".variable", ".variable_label", "North", "South")
                  %in% names(res)))
  expect_setequal(unique(res$.variable_label), c("revenue", "profit"))
})

test_that("gt_table: registers and binds its gt output", {
  skip_if_no_app()

  # The gt widget itself renders into the (lazily-evaluated) block-output pane,
  # which a headless board leaves suspended; asserting the bound gt output is a
  # stable smoke that the block constructs, registers and wires without error.
  # Full gt rendering is covered by gt's own tests.
  bound <- app$get_js(
    "!!document.querySelector(
       '#board-block_gt-result.shiny-bound-output.gt_shiny')"
  )
  expect_true(bound)
})

# ===========================================================================
# DRILL LIFECYCLE (table / tile parity with the chart): real row clicks,
# click-to-toggle, Reset, raw-value fidelity, gear uncheck, ctor restore.
# ===========================================================================

# Click the first row of `block_id`'s table whose drill cell carries the
# given data-raw value (a REAL DOM click through table.js's tbody handler).
dt_click_raw <- function(block_id, raw) {
  app$run_js(sprintf(
    "document.querySelector('%s tbody td[data-raw=\"%s\"]')
       .closest('tr').click();",
    dt_result(block_id), raw
  ))
  app$wait_for_idle()
}

dt_status_text <- function(block_id) {
  app$get_js(sprintf(
    "(function(){var s=document.querySelector('%s .dd-status-text');
       return s ? s.textContent : null;})()",
    dt_result(block_id)
  ))
}

dt_active_rows <- function(block_id) {
  app$get_js(sprintf(
    "document.querySelectorAll('%s tbody tr.dt-row-active').length",
    dt_result(block_id)
  ))
}

test_that("table drill: a real row click filters; re-click clears (toggle)", {
  skip_if_no_app()

  # Baseline: earlier drill tests left the filter cleared.
  expect_equal(nrow(get_block_result("table")), 6L)
  expect_equal(dt_status_text("table"), "No filter active")

  dt_click_raw("table", "West")
  res <- get_block_result("table")
  expect_equal(nrow(res), 1L)
  expect_equal(res$region, "West")
  expect_equal(dt_active_rows("table"), 1)
  expect_equal(dt_status_text("table"), "Filtered: region = West")
  # A genuinely LINKED downstream block sees the filtered frame.
  expect_equal(nrow(get_block_result("table_ds")), 1L)

  # Re-click the (now active) row -> filter and highlight clear; the
  # downstream block recovers the full frame.
  dt_click_raw("table", "West")
  expect_equal(nrow(get_block_result("table")), 6L)
  expect_equal(dt_active_rows("table"), 0)
  expect_equal(dt_status_text("table"), "No filter active")
  expect_equal(nrow(get_block_result("table_ds")), 6L)
})

test_that("table drill: the status-footer Reset clears the filter", {
  skip_if_no_app()

  dt_click_raw("table", "North")
  expect_equal(nrow(get_block_result("table")), 2L)
  app$run_js(sprintf(
    "document.querySelector('%s .dd-status-reset').click();",
    dt_result("table")
  ))
  app$wait_for_idle()
  expect_equal(nrow(get_block_result("table")), 6L)
  expect_equal(dt_active_rows("table"), 0)
  expect_equal(dt_status_text("table"), "No filter active")
})

test_that("table drill: a rounded numeric cell filters on its RAW value", {
  skip_if_no_app()

  # The cell displays the rounded value but carries the raw one.
  cell <- app$get_js(sprintf(
    "(function(){var td=document.querySelector('%s tbody td[data-raw=\"0.123456\"]');
       return td ? td.textContent.trim() : null;})()",
    dt_result("table_num")
  ))
  expect_equal(cell, "0.12")

  dt_click_raw("table_num", "0.123456")
  res <- get_block_result("table_num")
  expect_equal(nrow(res), 1L)
  expect_equal(res$ratio, 0.123456)
  expect_equal(res$region, "North")
})

test_that("table drill: unchecking the gear's Drill-down clears the filter", {
  skip_if_no_app()

  scope <- dt_result("table")
  dt_click_raw("table", "East")
  expect_equal(nrow(get_block_result("table")), 1L)

  # Open the gear and uncheck the Drill-down capability section.
  app$run_js(sprintf(
    "document.querySelector('%s .blockr-gear-btn').click();", scope
  ))
  app$wait_for_idle()
  app$run_js(sprintf(
    "(function(){var t=Array.from(document.querySelectorAll(
        '%s .dd-section-title--toggle'))
       .filter(function(x){return /Drill-down/.test(x.textContent);})[0];
     t.click();})()",
    scope
  ))
  app$wait_for_idle()
  Sys.sleep(0.5)
  app$wait_for_idle()

  # Downstream recovers AND the drill config is off (clicks now inert).
  expect_equal(nrow(get_block_result("table")), 6L)
  expect_null(app$get_js(sprintf(
    "document.querySelector('%s table').getAttribute('data-dt-onclick-col')",
    scope
  )))
  expect_equal(dt_active_rows("table"), 0)
})

test_that("tile drill: re-clicking the active row clears (toggle)", {
  skip_if_no_app()

  scope <- "#board-block_tile_x-expr-tile_result"
  # The earlier tile_x test left it drilled to South; the server re-render
  # must have marked that row active (data-tk-active -> .tk-active).
  expect_equal(nrow(get_block_result("tile_x")), 2L)
  app$wait_for_js(sprintf(
    "!!document.querySelector('%s .tk-active[data-group=\"South\"]')", scope
  ), timeout = 5000)

  app$run_js(sprintf(
    "document.querySelector('%s .tk-active[data-group=\"South\"]').click();",
    scope
  ))
  app$wait_for_idle()
  Sys.sleep(0.4)
  app$wait_for_idle()

  expect_equal(nrow(get_block_result("tile_x")), 6L)
  expect_equal(
    app$get_js(sprintf(
      "document.querySelectorAll('%s .tk-active').length", scope
    )),
    0
  )
  expect_equal(
    app$get_js(sprintf(
      "document.querySelector('%s .dd-status-text').textContent", scope
    )),
    "No filter active"
  )
})

test_that("restore: a table with ctor filter state comes up filtered + marked", {
  skip_if_no_app()

  # Ctor filter args = what a board restore replays.
  res <- get_block_result("table_restored")
  expect_equal(nrow(res), 2L)
  expect_true(all(res$region == "North"))

  scope <- dt_result("table_restored")
  app$wait_for_js(sprintf(
    "document.querySelectorAll('%s tbody tr.dt-row-active').length > 0", scope
  ), timeout = 5000)
  expect_equal(dt_active_rows("table_restored"), 2)
  expect_equal(dt_status_text("table_restored"), "Filtered: region = North")
})

test_that("restore: a tile with ctor filter state marks the card + footer", {
  skip_if_no_app()

  res <- get_block_result("tile_restored")
  expect_equal(nrow(res), 2L)
  expect_true(all(res$region == "South"))

  scope <- "#board-block_tile_restored-expr-tile_result"
  app$wait_for_js(sprintf(
    "!!document.querySelector('%s .tk-active[data-group=\"South\"]')", scope
  ), timeout = 5000)
  expect_equal(
    app$get_js(sprintf(
      "document.querySelector('%s .dd-status-text').textContent", scope
    )),
    "Filtered: region = South"
  )

  # The footer's Reset clears the restored filter too.
  app$run_js(sprintf(
    "document.querySelector('%s .dd-status-reset').click();", scope
  ))
  app$wait_for_idle()
  Sys.sleep(0.4)
  app$wait_for_idle()
  expect_equal(nrow(get_block_result("tile_restored")), 6L)
  expect_equal(
    app$get_js(sprintf(
      "document.querySelectorAll('%s .tk-active').length", scope
    )),
    0
  )
})

test_that("restore: a chart with ctor filter state labels its footer", {
  skip_if_no_app()

  res <- get_block_result("chart_restored")
  expect_equal(nrow(res), 2L)
  expect_true(all(res$region == "North"))

  # The config payload now carries filter_column/filter_values, so the JS
  # restore branch re-selects the mark and the footer names the filter.
  chart_settle("chart_restored")
  txt <- app$get_js(
    "document.querySelector(
       '#board-block_chart_restored-expr-drilldown_block .dd-status-text'
     ).textContent"
  )
  expect_equal(txt, "Filtered: region = North")
})

test_that("tile drill: unchecking the gear's Drill-down clears the filter", {
  skip_if_no_app()

  scope <- "#board-block_tile-expr-tile_result"
  app$run_js(sprintf(
    "document.querySelector('%s [data-group=\"East\"]').click();", scope
  ))
  app$wait_for_idle()
  Sys.sleep(0.4)
  app$wait_for_idle()
  expect_equal(nrow(get_block_result("tile")), 1L)

  # Open the gear (the band was rebuilt by the filter re-render) and uncheck
  # the picker-less Drill-down section: the engine's off-branch must clear
  # the drill config AND the active filter, so downstream recovers.
  app$run_js(sprintf(
    "document.querySelector('%s .blockr-gear-btn').click();", scope
  ))
  app$wait_for_idle()
  Sys.sleep(0.3)
  app$run_js(sprintf(
    "(function(){var t=Array.from(document.querySelectorAll(
        '%s .dd-section-title--toggle'))
       .filter(function(x){return /Drill-down/.test(x.textContent);})[0];
     t.click();})()",
    scope
  ))
  app$wait_for_idle()
  Sys.sleep(0.5)
  app$wait_for_idle()

  expect_equal(nrow(get_block_result("tile")), 6L)
  # Drill is off: the wrapper no longer wires clicks.
  expect_null(app$get_js(sprintf(
    "document.querySelector('%s .tk-block').getAttribute('data-tk-drill')",
    scope
  )))
})
