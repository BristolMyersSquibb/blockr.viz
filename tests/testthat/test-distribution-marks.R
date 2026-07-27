# Distribution-marks state (spec: _blockr.design/open/distribution-marks):
# the summary / whiskers / connect_centers constructor args and the
# box_points "all" removal. Rendering itself is browser-side (chart.js
# summarizeStat); what R owns is normalization, save/restore round-trips and
# the gear's config transport, so that is what these cover.

# Read one state field off a constructed chart block (same rationale as
# chart_state_field in test-chart-block.R: state only materializes in the
# server).
dist_state <- function(blk, field) {
  out <- NULL
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      out <<- session$returned$state[[field]]()
    },
    args = list(x = blk, data = list(data = function() {
      data.frame(g = c("a", "a", "b"), v = c(1, 2, 3))
    }))
  )
  out
}

test_that("distribution args restore through the constructor", {
  blk <- new_chart_block(
    chart_type = "pointrange", group = "g", value = "v",
    summary = "mean_sd", whiskers = "p5_p95", connect_centers = TRUE
  )
  expect_equal(dist_state(blk, "summary"), "mean_sd")
  expect_equal(dist_state(blk, "whiskers"), "p5_p95")
  expect_true(dist_state(blk, "connect_centers"))
})

test_that("distribution args default: NULL summary, tukey whiskers, no line", {
  blk <- new_chart_block(chart_type = "boxplot", group = "g", value = "v")
  # NULL = per-mark default resolved in the browser (median_q1_q3 for the
  # box, mean_se for pointrange) -- R must not pin one mark's default.
  expect_null(dist_state(blk, "summary"))
  expect_equal(dist_state(blk, "whiskers"), "tukey")
  expect_false(dist_state(blk, "connect_centers"))
})

test_that("connect_centers accepts the gear's \"on\"/\"off\" transport", {
  # Same contract as identity_line: the segmented control speaks "on"/"off"
  # over the wire, the R state is a plain logical.
  expect_true(dist_state(
    new_chart_block(chart_type = "pointrange", group = "g", value = "v",
                    connect_centers = "on"),
    "connect_centers"
  ))
  expect_false(dist_state(
    new_chart_block(chart_type = "pointrange", group = "g", value = "v",
                    connect_centers = "off"),
    "connect_centers"
  ))
})

test_that("box_points = \"all\" (removed) degrades to \"none\", no crash", {
  # Saved boards may carry the retired "all" strip; the decision in the spec
  # is a graceful fallback, not preservation.
  expect_equal(
    dist_state(new_chart_block(chart_type = "boxplot", group = "g",
                               value = "v", box_points = "all"),
               "box_points"),
    "none"
  )
  expect_equal(
    dist_state(new_chart_block(chart_type = "boxplot", group = "g",
                               value = "v", box_points = "outliers"),
               "box_points"),
    "outliers"
  )
})

test_that("config message updates summary, whiskers and connect_centers", {
  blk <- new_chart_block(
    chart_type = "pointrange", group = "g", value = "v"
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      expr_scope <- session$makeScope("expr")
      expr_scope$setInputs(drilldown_block_action = list(
        action = "config",
        summary = "min_max",
        whiskers = "mean_2sd",
        connect_centers = "on"
      ))
      session$flushReact()
      expect_equal(session$returned$state$summary(), "min_max")
      expect_equal(session$returned$state$whiskers(), "mean_2sd")
      expect_true(session$returned$state$connect_centers())
    },
    args = list(x = blk, data = list(data = function() {
      data.frame(g = c("a", "a", "b"), v = c(1, 2, 3))
    }))
  )
})

test_that("pointrange export stays the drill-filtered raw rows", {
  # The distribution marks summarize ON SCREEN only; the block's expr is
  # still just the click filter, so with no drill the raw rows pass through.
  df <- data.frame(g = c("a", "a", "b"), v = c(1, 2, 3))
  blk <- new_chart_block(
    chart_type = "pointrange", group = "g", value = "v",
    summary = "mean_se", connect_centers = TRUE
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_equal(session$returned$result(), df)
    },
    args = list(x = blk, data = list(data = function() df))
  )
})
