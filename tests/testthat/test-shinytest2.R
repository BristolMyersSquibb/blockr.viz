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
# popover JS use (real canvas hit-testing is left to the Playwright test).
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

test_that("summary_table: produces a tidy summary frame", {
  skip_if_no_app()

  res <- get_block_result("summary")
  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 0)
  # The tidy summary carries a variable column and computed stats.
  expect_true(all(c(".var", "mean") %in% names(res)))
  expect_setequal(unique(res$.var), c("revenue", "profit"))
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
