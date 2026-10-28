# Real browser-driven e2e tests for blockr.viz, modeled on blockr.dplyr's
# test-shinytest2.R. ONE shared board app (apps/viz-e2e) is launched once and
# driven through a headless chromium with shinytest2. We exercise the
# client-side JS contract end-to-end:
#
#   * the table search box (DOM row hiding),
#   * click-to-filter drill: the `<view>_select` message a view sends on a
#     click -> reactive state -> the block re-filters its own output (the data
#     that flows downstream), read back via the board's exported `result`,
#   * a `<view>_set` settings message re-rendering the view,
#   * the cell-visual data-bar markup.
#
# Server/reactive logic is covered without a browser by test-table-block-*.R;
# this layer adds the real browser + custom-message handler + re-render path.
#
# Skipped where no headless browser exists, and under R CMD check (launching
# chromium there leaves temp detritus -> a check NOTE, and is flaky); see
# run_browser_tests() in helper-chromote.R. Runs under devtools::test() and in
# the CI `browser` job.

# ---------------------------------------------------------------------------
# Shared app instance (test fixture). Built at file load like blockr.dplyr,
# but guarded: in environments with no browser (or under R CMD check) `app`
# stays NULL and every test skips via skip_if_no_app().
# ---------------------------------------------------------------------------

run_browser <- run_browser_tests()

app <- NULL
if (run_browser) {
  # shinytest2 refuses to run "On CRAN"; serve()'s app trips that guard unless
  # NOT_CRAN is set. We are explicitly not on CRAN here.
  Sys.setenv(NOT_CRAN = "true")
  configure_chromote()

  app <- tryCatch(
    shinytest2::AppDriver$new(
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
    # A background tab gets no animation frames, and the views draw on one: a
    # remote Chrome (blockr.dev/scripts/use-remote-chrome.R) opens the app
    # behind its first tab.
    try({
      cdp <- app$get_chromote_session()
      cdp$Page$bringToFront()
      cdp$Emulation$setFocusEmulationEnabled(enabled = TRUE)
    }, silent = TRUE)
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

# The class names the views (and blockr.ui's gear tray) write, in one place:
# a rename edits this list only. The JS snippets below name them as
# `@key@`; js_get(), js_run() and js_wait() fill them in.
SEL <- c(
  # table frame and records
  frame = ".vz-table", clickable = "vz-table-clickable", row = "tr.vz-table-row",
  stub = ".vz-table-stub", cell = "td.vz-table-data", hidden = ".blockr-hidden-search",
  search = "input.vz-table-search", count = ".vz-count",
  active = ".vz-table-row-active", row_active = "tr.vz-table-row-active",
  # status footer (table and chart)
  status = ".vz-status-text", clear = ".vz-status-clear", cap = ".vz-status-note",
  # cards
  card_row = ".vz-table-card-row", card = ".vz-table-card", card_label = ".vz-table-card-rowlab",
  card_glyph = ".vz-table-card-glyph", card_box = ".vz-table-groups-boxcell",
  # chart
  chart = ".vz-chart-panel",
  # gear
  gear = ".blockr-gear-btn", tray = ".blockr-settings",
  tray_open = ".blockr-settings.blockr-settings--open",
  tray_group = ".blockr-tray-nav__group", checkbox = ".blockr-checkbox",
  section = ".vz-section", section_title = ".vz-section-title",
  tile = ".vz-tile", tile_on = ".vz-tile--on"
)

sel_fill <- function(js) {
  for (k in names(SEL)) js <- gsub(paste0("@", k, "@"), SEL[[k]], js, fixed = TRUE)
  stopifnot(!grepl("@[a-z_]+@", js))
  js
}
js_get <- function(js) app$get_js(sel_fill(js))
js_run <- function(js) app$run_js(sel_fill(js))
js_wait <- function(js, ...) app$wait_for_js(sel_fill(js), ...)

# A block's view: its output element id and a selector for it. The table
# block draws into `<ns>-expr-table`, the chart block into `<ns>-expr-chart`.
view_id <- function(id, kind = "table") {
  sprintf("board-block_%s-expr-%s", id, kind)
}
view_sel <- function(id, kind = "table") paste0("#", view_id(id, kind))

# Send a view message the way the views do (Blockr.viz.util.send(), the same
# typed inputs a click or a gear edit sends), then settle.
#
# send_select(): a click or brush, `<view>_select`. `selection` is the
# message's selection, or NULL to clear.
send_select <- function(id, selection, kind = "table") {
  json <- if (is.null(selection)) "null" else
    jsonlite::toJSON(selection, auto_unbox = TRUE, null = "null")
  js_run(sprintf(
    "Blockr.viz.util.send('%s', 'select', {selection: %s, nonce: Date.now()});",
    view_id(id, kind), json
  ))
  app$wait_for_idle()
}

# A categorical selection: rows whose `col` is one of `values`.
categorical <- function(col, values) {
  list(type = "categorical", filters = stats::setNames(list(as.list(values)), col))
}

# send_set(): a settings change, `<view>_set`, at the view's current state
# revision (the server ignores a value older than its own).
send_set <- function(id, values, kind = "table") {
  json <- jsonlite::toJSON(values, auto_unbox = TRUE, null = "null")
  js_run(sprintf(
    "(function(){var id = '%s', p = Shiny.shinyapp.$values[id];
       Blockr.viz.util.send(id, 'set', {rev: p.state_rev, values: %s}, '%s');})();",
    view_id(id, kind), json, kind
  ))
  app$wait_for_idle()
  Sys.sleep(0.3)
  app$wait_for_idle()
}

# Fire a GENUINE click on an ECharts chart at a data point's pixel, driving the
# real chart/family-aggregated.js `chart.on('click')` handler in headless chromium.
# ECharts paints to <canvas> and its high-level click is synthesized from
# native press/release (a bare DOM event or zrender handler.dispatch doesn't
# trigger it), so we use a true CDP Input.dispatchMouseEvent through
# shinytest2's own chromote session — no Playwright. `at` is the [value,
# category] data point passed to the chart's convertToPixel(); computed right
# before the click so the layout is current.
#
# NB: chart/view.js schedules a delayed resize() ~300ms after each
# (re)render, so we settle past it before reading coordinates, otherwise
# convertToPixel reads a stale layout and the click lands on the wrong bar.
# Scroll a chart block into view, wait for its canvas, and settle past the
# chart's deferred resize(); returns the chart-container selector.
chart_settle <- function(block_id) {
  sel <- view_sel(block_id, "chart")
  js_run(sprintf(
    "document.querySelector('%s').scrollIntoView({block: 'center'});", sel
  ))
  js_wait(sprintf("!!document.querySelector('%s canvas')", sel),
                  timeout = 15000)
  app$wait_for_idle()
  Sys.sleep(0.8) # past the chart's deferred resize()
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

# --- Gear settings band (core/gear) ----------------------------------------
# The gear opens an in-flow .blockr-settings band inside the block. The OPEN
# one carries the --open modifier. These helpers scope to that open band.
POPOVER_OPEN_JS <- "document.querySelector('@tray_open@')"

gear_open <- function(block_id) {
  js_run(sprintf(
    "document.querySelector('%s @gear@').click();",
    view_sel(block_id, "chart")
  ))
  app$wait_for_idle()
  Sys.sleep(0.3)
}

popover_is_open <- function() {
  isTRUE(js_get(sprintf("!!(%s)", POPOVER_OPEN_JS)))
}

popover_active_type <- function() {
  js_get(sprintf(
    "(function(){var p=%s; if(!p) return null; var a=p.querySelector('[data-role=type] @tile_on@'); return a?a.dataset.value:null;})()",
    POPOVER_OPEN_JS
  ))
}

popover_click_type <- function(type) {
  js_run(sprintf(
    "(function(){var p=%s; p.querySelector('[data-role=type] @tile@[data-value=\"%s\"]').click();})()",
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
  pt <- jsonlite::fromJSON(js_get(sprintf(
    "(function(){
       var div  = document.querySelector('%s @chart@');
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
  pt <- jsonlite::fromJSON(js_get(sprintf(
    "(function(){
       var div  = document.querySelector('%s @chart@');
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
  js_run(sprintf(
    "(function(){
       var div  = document.querySelector('%s @chart@');
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

# Selector for a table block's view (search box + <table> live inside it).
dt_result <- function(block_id) view_sel(block_id, "table")

# Cards: click the first card of the row labelled `label`; the rows marked
# selected.
card_click <- function(block_id, label) {
  js_run(sprintf(
    "Array.from(document.querySelectorAll('%s @card_row@'))
       .filter(function(r){var l = r.querySelector('@card_label@');
         return l && l.textContent.trim() === '%s';})[0]
       .querySelector('@card@').click();",
    dt_result(block_id), label
  ))
  app$wait_for_idle()
  Sys.sleep(0.4)
  app$wait_for_idle()
}

card_active <- function(block_id) {
  unlist(js_get(sprintf(
    "Array.from(document.querySelectorAll('%s @card_row@@active@'))
       .map(function(r){return r.querySelector('@card_label@').textContent.trim();})",
    dt_result(block_id)
  )))
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
  # Visible data rows, mechanism-agnostic: the flat cell-model table removes
  # non-matching rows from the DOM (model search), while the DOM path
  # (structured / static tables) class-hides them.
  count_visible <- function() {
    js_get(sprintf(
      "document.querySelectorAll(
         '%s tbody @row@:not(@hidden@)').length",
      scope
    ))
  }

  # All 6 rows visible initially.
  expect_equal(count_visible(), 6)

  # Type a query matching exactly one region -> only that row stays visible.
  js_run(sprintf(
    "var i = document.querySelector('%s @search@');
     i.value = 'East';
     i.dispatchEvent(new Event('input', {bubbles: true}));",
    scope
  ))
  js_wait(sprintf(
    "document.querySelectorAll(
       '%s tbody @row@:not(@hidden@)').length === 1",
    scope
  ), timeout = 5000)
  expect_equal(count_visible(), 1)

  # Clearing the box restores every row.
  js_run(sprintf(
    "var i = document.querySelector('%s @search@');
     i.value = '';
     i.dispatchEvent(new Event('input', {bubbles: true}));",
    scope
  ))
  js_wait(sprintf(
    "document.querySelectorAll(
       '%s tbody @row@:not(@hidden@)').length === 6",
    scope
  ), timeout = 5000)
  expect_equal(count_visible(), 6)
})

test_that("the footer counts the rows, and re-counts under a search", {
  skip_if_no_app()

  scope <- dt_result("table")
  count_text <- function() {
    js_get(sprintf(
      "(function(){var e=document.querySelector('%s @count@');
         return e ? e.textContent : null;})()", scope
    ))
  }

  expect_equal(count_text(), "6 rows")

  # A search narrows the count without touching the data: the browser owns
  # the query, so the footer has to say how much of the table is on screen.
  js_run(sprintf(
    "var i = document.querySelector('%s @search@');
     i.value = 'East';
     i.dispatchEvent(new Event('input', {bubbles: true}));",
    scope
  ))
  js_wait(sprintf(
    "document.querySelector('%s @count@').textContent === '1 of 6 rows'",
    scope
  ), timeout = 5000)
  expect_equal(count_text(), "1 of 6 rows")

  js_run(sprintf(
    "var i = document.querySelector('%s @search@');
     i.value = '';
     i.dispatchEvent(new Event('input', {bubbles: true}));",
    scope
  ))
  js_wait(sprintf(
    "document.querySelector('%s @count@').textContent === '6 rows'",
    scope
  ), timeout = 5000)
  expect_equal(count_text(), "6 rows")
})

# ===========================================================================
# CLICK-TO-FILTER DRILL (select message -> re-filter -> downstream data)
# ===========================================================================

test_that("drill: a single-value selection filters the block's output", {
  skip_if_no_app()

  send_select("table", categorical("region", "North"))

  res <- get_block_result("table")
  expect_equal(nrow(res), 2L)
  expect_true(all(res$region == "North"))
})

test_that("drill: a multi-value selection uses %in% and keeps all matches", {
  skip_if_no_app()

  send_select("table", categorical("region", c("North", "South")))

  res <- get_block_result("table")
  expect_equal(nrow(res), 4L)
  expect_setequal(unique(res$region), c("North", "South"))
})

test_that("drill: clearing the selection restores all rows", {
  skip_if_no_app()

  send_select("table", NULL)

  res <- get_block_result("table")
  expect_equal(nrow(res), 6L)
})

# ===========================================================================
# SETTINGS (set message -> state -> re-render)
# ===========================================================================

test_that("settings: setting drill updates the table markup", {
  skip_if_no_app()

  # A table whose rows filter on a click carries .vz-table-clickable.
  clickable <- function(id = "bars") {
    isTRUE(js_get(sprintf(
      "document.querySelector('%s @frame@').classList.contains('@clickable@')",
      dt_result(id)
    )))
  }

  # `bars` starts with no drill.
  expect_false(clickable())

  send_set("bars", list(drill = "region"))
  expect_true(clickable())

  # An empty drill turns it back off.
  send_set("bars", list(drill = NULL))
  expect_false(clickable())
})

# ===========================================================================
# CELL VISUALS (data bars render as a CSS gradient)
# ===========================================================================

test_that("cell visuals: numeric cells render data-bar gradients", {
  skip_if_no_app()

  scope <- dt_result("bars")

  # Every numeric cell in `bars` carries a left-anchored linear-gradient.
  n_bars <- js_get(sprintf(
    "document.querySelectorAll(
       '%s table tbody td[style*=\"linear-gradient\"]').length",
    scope
  ))
  expect_gt(n_bars, 0)

  # The largest revenue (North = 100, the column max) fills to 100%.
  has_full_bar <- js_get(sprintf(
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
  n_bg <- js_get(sprintf(
    "document.querySelectorAll('%s table tbody td[style*=\"background:#\"]').length",
    scope
  ))
  expect_gt(n_bg, 0)

  # These are solid fills, not the data-bar gradient.
  n_grad <- js_get(sprintf(
    "document.querySelectorAll('%s table tbody td[style*=\"linear-gradient\"]').length",
    scope
  ))
  expect_equal(n_grad, 0)
})

# ===========================================================================
# CHART: click-to-filter selection (message contract, not the canvas hit-test)
# ===========================================================================

test_that("chart: a selection filters the chart block's output", {
  skip_if_no_app()

  send_select("chart", categorical("region", "North"), "chart")

  res <- get_block_result("chart")
  expect_true(all(res$region == "North"))
  expect_lt(nrow(res), 6L)

  # Clear it again.
  send_select("chart", NULL, "chart")
  expect_equal(nrow(get_block_result("chart")), 6L)
})

test_that("chart: a real ECharts canvas click drills the bar (no Playwright)", {
  skip_if_no_app()

  # Reset any prior filter so the baseline is the full frame.
  send_select("chart", NULL, "chart")
  expect_equal(nrow(get_block_result("chart")), 6L)

  # Click the middle of the "North" bar (sum revenue 150; value 75 is mid-bar).
  # This runs the chart.on('click') -> selection.click() path for real.
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
# GEAR (core/gear): the real DOM, not the set message
# ===========================================================================

test_that("gear popover: a chart-type switch keeps it open and the block live", {
  skip_if_no_app()

  cfg <- view_sel("chart_cfg", "chart")
  js_run(sprintf("document.querySelector('%s').scrollIntoView({block:'center'});", cfg))
  js_wait(sprintf("!!document.querySelector('%s canvas')", cfg), timeout = 15000)
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
  expect_true(js_get(sprintf("!!document.querySelector('%s canvas')", cfg)))

  # Not wedged: a drill still flows through to the (re-filtered) output.
  send_select("chart_cfg", categorical("region", "South"), "chart")
  res <- get_block_result("chart_cfg")
  expect_setequal(unique(res$region), "South")
  expect_equal(nrow(res), 2L)
})

# ===========================================================================
# CARDS: the table block's cards look
# ===========================================================================

test_that("cards: a selection filters the block's output", {
  skip_if_no_app()

  send_select("cards", categorical("region", "South"))

  res <- get_block_result("cards")
  expect_true(all(res$region == "South"))
  expect_equal(nrow(res), 2L)

  send_select("cards", NULL)
  expect_equal(nrow(get_block_result("cards")), 6L)
})

# ===========================================================================
# SUMMARY TABLE / GT TABLE: render smoke
# ===========================================================================

test_that("summary_table gear: toggling 'overall' adds a Total column to the output", {
  skip_if_no_app()

  before <- get_block_result("summary")
  expect_false("Total" %in% names(before))

  # The block has one gear, the table's. Open it and tick "Overall column"
  # (the checkbox itself reads "Add"): this drives the gear's checkbox ->
  # state -> R -> output.
  gears <- "Array.from(document.querySelectorAll('[id^=\"board-block_summary\"] @gear@')).filter(function(x){return x.offsetParent;})"
  expect_equal(js_get(paste0(gears, ".length")), 1L)
  js_run(paste0("(function(){var g=", gears, "[0]; g.scrollIntoView({block:'center'}); g.click();})()"))
  app$wait_for_idle()
  Sys.sleep(0.3)
  js_run(
    "(function(){var b=Array.from(document.querySelectorAll('[id^=\"board-block_summary\"] input[data-gear-input=\"total\"]')).filter(function(x){return x.closest('@checkbox@').offsetParent;})[0]; if(b) b.click();})()"
  )
  app$wait_for_idle()
  Sys.sleep(0.5)
  app$wait_for_idle()

  after <- get_block_result("summary")
  expect_true("Total" %in% names(after))
  expect_gt(ncol(after), ncol(before))
})

test_that("cards: one card per summary, and a click on a row drills", {
  skip_if_no_app()

  scope <- dt_result("cards")
  js_wait(sprintf(
    "document.querySelectorAll('%s @card_row@').length === 4", scope
  ), timeout = 10000)
  # Two summaries, four regions: eight cards, each with its glyph.
  expect_equal(js_get(sprintf(
    "document.querySelectorAll('%s @card@').length", scope)), 8)
  expect_equal(js_get(sprintf(
    "document.querySelectorAll('%s @card_glyph@ @card_box@').length",
    scope)), 4)

  card_click("cards", "South")
  app$wait_for_idle()
  Sys.sleep(0.4)
  app$wait_for_idle()

  res <- get_block_result("cards")
  expect_setequal(unique(res$region), "South")
  expect_equal(nrow(res), 2L)
})

test_that("summary_table: produces the wide annotated summary frame", {
  skip_if_no_app()

  res <- get_block_result("summary")
  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 0)
  # The wide annotated df carries per-row identity (.variable) and one baked
  # cell column per region level. This block shows a single statistic, so each
  # variable is one row and the stub names the variable: no .variable_label,
  # hence no synthesized header per row (the renderer only builds those from
  # .variable_label runs).
  expect_true(all(c(".variable", "North", "South") %in% names(res)))
  expect_false(".variable_label" %in% names(res))
  expect_setequal(unique(res$.variable), c("revenue", "profit"))
  expect_setequal(res$.label, c("revenue", "profit"))
})

# ===========================================================================
# DRILL LIFECYCLE (table and cards parity with the chart): real row clicks,
# click-to-toggle, Reset, raw-value fidelity, gear uncheck, ctor restore.
# ===========================================================================

# Click the row of `block_id`'s table whose row name is `label` (a REAL DOM
# click through the records view's row handler). The view keeps each cell's
# value in its model, so the click filters on the raw value, not the text.
dt_click_row <- function(block_id, label) {
  js_run(sprintf(
    "Array.from(document.querySelectorAll('%s tbody @row@'))
       .filter(function(tr){var s = tr.querySelector('@stub@');
         return s && s.textContent.trim() === '%s';})[0].click();",
    dt_result(block_id), label
  ))
  app$wait_for_idle()
  Sys.sleep(0.3)
  app$wait_for_idle()
}

# The text of a cell: row `label`, column `col` (1 = the first after the stub).
dt_cell_text <- function(block_id, label, col) {
  js_get(sprintf(
    "(function(){var tr = Array.from(document.querySelectorAll('%s tbody @row@'))
       .filter(function(tr){var s = tr.querySelector('@stub@');
         return s && s.textContent.trim() === '%s';})[0];
       return tr ? tr.querySelectorAll('@cell@')[%d].textContent.trim() : null;})()",
    dt_result(block_id), label, col - 1L
  ))
}

# Open a block's gear on its Interaction group and untick the Drill-down
# section's "Filter on".
gear_drill_off <- function(block_id, kind = "table") {
  sel <- view_sel(block_id, kind)
  js_run(sprintf(
    "document.querySelector('%s @gear@').click();", sel
  ))
  app$wait_for_idle()
  Sys.sleep(0.3)
  js_run(sprintf(
    "(function(){var p = document.querySelector('%s @tray@');
       Array.from(p.querySelectorAll('@tray_group@'))
         .filter(function(g){return /Interaction/.test(g.textContent);})[0].click();
     })()", sel
  ))
  Sys.sleep(0.3)
  js_run(sprintf(
    "(function(){var s = Array.from(document.querySelectorAll('%s @section@'))
       .filter(function(x){var t = x.querySelector('@section_title@');
         return t && /Drill-down/.test(t.textContent);})[0];
     s.querySelector('input[type=checkbox]').click();})()",
    sel
  ))
  app$wait_for_idle()
  Sys.sleep(0.5)
  app$wait_for_idle()
}

dt_status_text <- function(block_id) {
  js_get(sprintf(
    "(function(){var s=document.querySelector('%s @status@');
       return s && s.textContent.trim() ? s.textContent : null;})()",
    dt_result(block_id)
  ))
}

dt_active_rows <- function(block_id) {
  js_get(sprintf(
    "document.querySelectorAll('%s tbody @row_active@').length",
    dt_result(block_id)
  ))
}

test_that("table drill: a real row click filters; re-click clears (toggle)", {
  skip_if_no_app()

  # Baseline: earlier drill tests left the filter cleared.
  expect_equal(nrow(get_block_result("table")), 6L)
  expect_null(dt_status_text("table"))

  dt_click_row("table", "West")
  res <- get_block_result("table")
  expect_equal(nrow(res), 1L)
  expect_equal(res$region, "West")
  expect_equal(dt_active_rows("table"), 1)
  expect_equal(dt_status_text("table"), "Filtered: region = West")
  # A genuinely LINKED downstream block sees the filtered frame.
  expect_equal(nrow(get_block_result("table_ds")), 1L)

  # Re-click the (now active) row -> filter and highlight clear; the
  # downstream block recovers the full frame.
  dt_click_row("table", "West")
  expect_equal(nrow(get_block_result("table")), 6L)
  expect_equal(dt_active_rows("table"), 0)
  expect_null(dt_status_text("table"))
  expect_equal(nrow(get_block_result("table_ds")), 6L)
})

test_that("table drill: the status line's clear and a re-click both clear", {
  skip_if_no_app()

  dt_click_row("table", "North")
  expect_equal(nrow(get_block_result("table")), 2L)
  expect_equal(dt_status_text("table"), "Filtered: region = North")
  # The footer's clear button ends the filter.
  js_run(sprintf(
    "document.querySelector('%s @clear@').click();", dt_result("table")
  ))
  app$wait_for_idle()
  Sys.sleep(0.3)
  app$wait_for_idle()
  expect_equal(nrow(get_block_result("table")), 6L)
  expect_null(dt_status_text("table"))
  # So does a second click on the selected row.
  dt_click_row("table", "North")
  expect_equal(nrow(get_block_result("table")), 2L)
  dt_click_row("table", "North")
  expect_equal(nrow(get_block_result("table")), 6L)
  expect_equal(dt_active_rows("table"), 0)
  expect_null(dt_status_text("table"))
})

test_that("table drill: a rounded numeric cell filters on its RAW value", {
  skip_if_no_app()

  # The cell displays the rounded value; the view's model keeps the raw one.
  expect_equal(dt_cell_text("table_num", "North", 1L), "0.12")

  dt_click_row("table_num", "North")
  res <- get_block_result("table_num")
  expect_equal(nrow(res), 1L)
  expect_equal(res$ratio, 0.123456)
  expect_equal(res$region, "North")
})

test_that("table drill: unchecking the gear's Drill-down clears the filter", {
  skip_if_no_app()

  scope <- dt_result("table")
  dt_click_row("table", "East")
  expect_equal(nrow(get_block_result("table")), 1L)

  # Untick the gear's Drill-down "Filter on".
  gear_drill_off("table")

  # Downstream recovers AND the drill is off (clicks now inert).
  expect_equal(nrow(get_block_result("table")), 6L)
  expect_false(js_get(sprintf(
    "document.querySelector('%s @frame@').classList.contains('@clickable@')",
    scope
  )))
  expect_equal(dt_active_rows("table"), 0)
})

test_that("cards drill: re-clicking the active row clears (toggle)", {
  skip_if_no_app()

  scope <- dt_result("cards")
  # The earlier cards test left it drilled to South.
  expect_equal(nrow(get_block_result("cards")), 2L)
  expect_equal(card_active("cards"), "South")

  card_click("cards", "South")

  expect_equal(nrow(get_block_result("cards")), 6L)
  expect_length(card_active("cards"), 0L)
  expect_null(js_get(sprintf(
    "(function(){var s=document.querySelector('%s @status@');
       return s && s.textContent.trim() ? s.textContent : null;})()", scope)))
})

test_that("restore: a table with ctor filter state comes up filtered + marked", {
  skip_if_no_app()

  # Ctor filter args = what a board restore replays.
  res <- get_block_result("table_restored")
  expect_equal(nrow(res), 2L)
  expect_true(all(res$region == "North"))

  scope <- dt_result("table_restored")
  js_wait(sprintf(
    "document.querySelectorAll('%s tbody @row_active@').length > 0", scope
  ), timeout = 5000)
  expect_equal(dt_active_rows("table_restored"), 2)
  expect_equal(dt_status_text("table_restored"), "Filtered: region = North")
})

test_that("restore: cards with a saved selection mark the row + footer", {
  skip_if_no_app()

  res <- get_block_result("cards_restored")
  expect_equal(nrow(res), 2L)
  expect_true(all(res$region == "South"))

  scope <- dt_result("cards_restored")
  js_wait(sprintf(
    "!!document.querySelector('%s @card_row@@active@')", scope
  ), timeout = 5000)
  expect_equal(card_active("cards_restored"), "South")
  expect_equal(js_get(sprintf(
    "document.querySelector('%s @status@').textContent", scope)),
    "Filtered: region = South")

  # A click on the selected card clears the restored selection.
  card_click("cards_restored", "South")
  expect_equal(nrow(get_block_result("cards_restored")), 6L)
  expect_length(card_active("cards_restored"), 0L)
})

test_that("restore: a chart with ctor filter state labels its footer", {
  skip_if_no_app()

  res <- get_block_result("chart_restored")
  expect_equal(nrow(res), 2L)
  expect_true(all(res$region == "North"))

  # The config payload now carries filter_column/filter_values, so the JS
  # restore branch re-selects the mark and the footer names the filter.
  chart_settle("chart_restored")
  txt <- js_get(sprintf(
    "document.querySelector('%s @status@').textContent",
    view_sel("chart_restored", "chart")
  ))
  expect_equal(txt, "Filtered: region = North")
})

test_that("cards drill: unchecking the gear's Drill-down clears the filter", {
  skip_if_no_app()

  scope <- dt_result("cards")
  card_click("cards", "East")
  expect_equal(nrow(get_block_result("cards")), 1L)

  # Untick the gear's Drill-down "Filter on": the drill and the active
  # filter both clear, so downstream recovers.
  gear_drill_off("cards")

  expect_equal(nrow(get_block_result("cards")), 6L)
  # Drill is off: the rows no longer take a click.
  expect_false(js_get(sprintf(
    "document.querySelector('%s @frame@').classList.contains('@clickable@')",
    scope
  )))
})

# ===========================================================================
# FACET SCALES (chart/view.js _harmoniseAxes + the shared category set)
# ===========================================================================

# Per-panel value-axis extents and category sets, read off the LIVE echarts
# models — the same objects the canvas draws from, so this asserts what the
# reader actually sees rather than what the option said before layout. A
# horizontal bar (the default) puts the value on x and the categories on y.
facet_axes <- function(block_id) {
  sel <- view_sel(block_id, "chart")
  jsonlite::fromJSON(js_get(sprintf(
    "JSON.stringify(Array.from(document.querySelectorAll('%s @chart@'))
       .map(function(d) {
         var c = echarts.getInstanceByDom(d);
         var m = c.getModel();
         return {
           value: m.getComponent('xAxis', 0).axis.scale.getExtent(),
           cats: m.getComponent('yAxis', 0).axis.scale.getTicks()
                  .map(function(t) { return t.tickValue; }).length
         };
       }))", sel
  )), simplifyVector = FALSE)
}

test_that("facet panels share one value domain and one category set", {
  skip_if_no_app()
  chart_settle("chart_facet")

  panels <- facet_axes("chart_facet")
  expect_length(panels, 4L) # North, South, East, West

  # Fixed (the default): every panel reports the SAME value extent, wide
  # enough for the biggest bar (North's product A, 100), and the same number
  # of category slots — even East/West, which have no product B.
  extents <- unique(lapply(panels, function(p) unlist(p$value)))
  expect_length(extents, 1L)
  expect_gte(extents[[1]][2], 100)
  expect_equal(unique(vapply(panels, function(p) p$cats, numeric(1))), 2)
})

test_that("facet_scales = free hands every panel back its own axis", {
  skip_if_no_app()

  send_set("chart_facet", list(facet_scales = "free"), "chart")
  chart_settle("chart_facet")

  panels <- facet_axes("chart_facet")
  extents <- unique(lapply(panels, function(p) unlist(p$value)))
  # West tops out at 30, North at 100: free panels cannot agree.
  expect_gt(length(extents), 1L)
  # ...and a panel with no product B drops that slot entirely.
  expect_gt(length(unique(vapply(panels, function(p) p$cats, numeric(1)))), 1L)

  # Back to fixed: the harmonised domain returns (no one-way state).
  send_set("chart_facet", list(facet_scales = "fixed"), "chart")
  chart_settle("chart_facet")
  expect_length(
    unique(lapply(facet_axes("chart_facet"), function(p) unlist(p$value))), 1L
  )
})

# --- Colour-split boxplot: every box ON its slot's tick ---------------------
# A colour-split boxplot is one series per level with each box already in
# its own (group, level) slot. ECharts dodges series that share a category
# axis, which pushed level 0 left of its tick and the last level right of
# it, so each level rides its own hidden category axis over the same slots.
# The boxes are solid, drawn by custom series (one group per box); what is
# asserted is the geometry: every drawn box is centred on its tick.

box_geometry <- function(block_id) {
  sel <- view_sel(block_id, "chart")
  jsonlite::fromJSON(js_get(sprintf(
    "JSON.stringify((function() {
       var c = echarts.getInstanceByDom(document.querySelector('%s @chart@'));
       var m = c.getModel();
       var out = { series: 0, x_axes: 0, shown_x_axes: 0, offsets: [] };
       m.eachComponent('xAxis', function(a) {
         out.x_axes++;
         if (a.get('show') !== false) out.shown_x_axes++;
       });
       m.eachSeries(function(s) {
         if (s.subType !== 'custom') return;
         out.series++;
         var d = s.getData();
         for (var k = 0; k < d.count(); k++) {
           var g = d.getItemGraphicEl(k);
           if (!g) continue;
           var r = g.getBoundingRect();
           var cx = r.x + r.width / 2;
           out.offsets.push(cx - c.convertToPixel({xAxisIndex: 0}, k));
         }
       });
       return out;
     })())", sel
  )), simplifyVector = TRUE)
}

test_that("a colour-split boxplot centres every box on its own slot", {
  skip_if_no_app()
  chart_settle("chart_box_split")

  geo <- box_geometry("chart_box_split")
  # One series per product level: the legend band toggles by series name.
  expect_equal(geo$series, 2L)
  # ...each on its own category axis, only the first of them drawn.
  expect_equal(geo$x_axes, 2L)
  expect_equal(geo$shown_x_axes, 1L)
  # Six (region, product) slots; each box within a pixel of its tick.
  expect_length(geo$offsets, 6L)
  expect_lt(max(abs(geo$offsets)), 1)
})

# --- Many-category axis: the thinning has to be visible ---------------------
# ECharts' category axisLabel defaults to interval:'auto', which hides any
# label that would collide with its neighbour, silently. Past
# PANEL_H_CAP / CAT_LABEL_H (4000 / 14 = 286) categories the row band is
# shorter than a label, so terms disappear off the axis with nothing on screen
# to explain it. That silence was the user's complaint, not the thinning itself:
# a 500-term axis is unreadable at any band, and PANEL_H_CAP cannot simply be
# raised (Safari's canvas limit is an AREA, and past it the chart goes blank).
# So what is asserted here is the message.

chart_cap_text <- function(block_id) {
  js_get(sprintf(
    "(function(){var s=document.querySelector('%s @cap@');
       return s ? s.textContent : null;})()",
    view_sel(block_id, "chart")
  ))
}

test_that("a 320-term horizontal bar reports the labels it cannot show", {
  skip_if_no_app()
  chart_settle("chart_many")
  txt <- chart_cap_text("chart_many")
  expect_match(txt, "Axis labels thinned: 285 of 320 term labelled", fixed = TRUE)
  expect_match(txt, "filter to fewer", fixed = TRUE)
})

test_that("a chart whose labels all fit says nothing", {
  skip_if_no_app()
  chart_settle("chart")
  expect_null(chart_cap_text("chart"))
})
