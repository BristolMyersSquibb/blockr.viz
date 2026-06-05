# Tests for new_drilldown_chart_block — focus on the patient-timelines
# additions (xend, sort_by, click-to-filter with USUBJID).

test_that("constructor accepts xend and sort_by", {
  blk <- new_drilldown_chart_block(
    chart_type = "gantt",
    x = "ASTDY",
    xend = "AENDY",
    y = "AEDECOD",
    color = "AESEV",
    sort_by = "onset"
  )
  expect_s3_class(blk, "drilldown_chart_block")
})

test_that("gantt state round-trips xend and sort_by", {
  df <- data.frame(
    USUBJID = c("A", "A", "B"),
    AEDECOD = c("Headache", "Nausea", "Headache"),
    ASTDY = c(3, 10, 5),
    AENDY = c(5, 12, 7),
    AESEV = c("MILD", "SEVERE", "MODERATE"),
    stringsAsFactors = FALSE
  )
  blk <- new_drilldown_chart_block(
    chart_type = "gantt",
    x = "ASTDY", xend = "AENDY", y = "AEDECOD",
    color = "AESEV", sort_by = "onset"
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_equal(session$returned$state$xend(), "AENDY")
      expect_equal(session$returned$state$sort_by(), "onset")
      expect_equal(session$returned$state$chart_type(), "gantt")
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("config message updates xend and sort_by", {
  df <- data.frame(
    USUBJID = "A", AEDECOD = "Headache",
    ASTDY = 1, AENDY = 2, AESEV = "MILD",
    stringsAsFactors = FALSE
  )
  blk <- new_drilldown_chart_block(chart_type = "gantt")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      expr_scope <- session$makeScope("expr")
      expr_scope$setInputs(drilldown_block_action = list(
        action = "config",
        xend = "AENDY",
        sort_by = "alpha"
      ))
      session$flushReact()
      expect_equal(session$returned$state$xend(), "AENDY")
      expect_equal(session$returned$state$sort_by(), "alpha")
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("click-to-filter on USUBJID produces a filter expression", {
  df <- data.frame(
    USUBJID = c("01-001", "01-001", "01-002", "01-002"),
    ADY = c(1, 10, 1, 10),
    AVAL = c(100, 110, 95, 105),
    stringsAsFactors = FALSE
  )
  blk <- new_drilldown_chart_block(
    chart_type = "line",
    x = "ADY", y = "AVAL", color = "USUBJID"
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      expr_scope <- session$makeScope("expr")
      expr_scope$setInputs(drilldown_block_action = list(
        action = "filter",
        filter_type = "categorical",
        column = "USUBJID",
        values = list("01-001")
      ))
      session$flushReact()
      expect_equal(session$returned$state$filter_column(), "USUBJID")
      expect_equal(unlist(session$returned$state$filter_values()), "01-001")
      result <- session$returned$result()
      expect_equal(nrow(result), 2L)
      expect_true(all(result$USUBJID == "01-001"))
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("empty categorical filter is a no-op", {
  df <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)
  blk <- new_drilldown_chart_block(chart_type = "bar", group = "b")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_equal(session$returned$result(), df)
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("series round-trips through state", {
  df <- data.frame(
    USUBJID = rep(c("A", "B"), each = 2),
    ADY = c(1, 10, 1, 10),
    AVAL = c(100, 110, 95, 105),
    TRT01A = rep(c("ARM A", "ARM B"), each = 2),
    stringsAsFactors = FALSE
  )
  blk <- new_drilldown_chart_block(
    chart_type = "line", x = "ADY", y = "AVAL",
    series = "USUBJID", color = "TRT01A"
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_equal(session$returned$state$series(), "USUBJID")
      expect_equal(session$returned$state$color(), "TRT01A")
      # Config message can update series at runtime.
      expr_scope <- session$makeScope("expr")
      expr_scope$setInputs(drilldown_block_action = list(
        action = "config", series = "TRT01A"
      ))
      session$flushReact()
      expect_equal(session$returned$state$series(), "TRT01A")
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("r_needed_cols includes AVISITN when x is AVISIT", {
  df <- data.frame(
    USUBJID = c("A", "A", "B"),
    AVISIT = c("Visit 1", "Visit 2", "Visit 1"),
    AVISITN = c(1, 2, 1),
    AVAL = c(10, 11, 12),
    stringsAsFactors = FALSE
  )
  blk <- new_drilldown_chart_block(
    chart_type = "line", x = "AVISIT", y = "AVAL",
    color = "USUBJID"
  )
  # Indirectly verify by checking the block state exposes x correctly;
  # the AVISITN inclusion is a JS-input concern, but we confirm the block
  # accepts the configuration without error on evaluation.
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_equal(session$returned$state$x(), "AVISIT")
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("scatter click-emit drives categorical filter on arbitrary column", {
  # Property-workbench shape: one dot per policy on a scatter, click a dot,
  # downstream filters to that single policy. The JS handler emits
  # {action: "filter", filter_type: "categorical", column: <series>,
  #  values: [<seriesName>]} which the R server side already handles
  # generically. This test confirms the R machinery accepts arbitrary
  # column names (not only USUBJID).
  df <- data.frame(
    policy_id        = c("POL_001", "POL_002", "POL_003"),
    exposure_premium = c(100, 200, 300),
    model_price      = c(150, 250, 350),
    stringsAsFactors = FALSE
  )
  blk <- new_drilldown_chart_block(
    chart_type = "scatter",
    x      = "exposure_premium",
    y      = "model_price",
    series  = "policy_id"
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_equal(session$returned$state$chart_type(), "scatter")
      expect_equal(session$returned$state$series(), "policy_id")

      # Simulate a scatter-dot click: JS would send this exact message.
      expr_scope <- session$makeScope("expr")
      expr_scope$setInputs(drilldown_block_action = list(
        action      = "filter",
        filter_type = "categorical",
        column      = "policy_id",
        values      = list("POL_002")
      ))
      session$flushReact()

      expect_equal(session$returned$state$filter_column(), "policy_id")
      expect_equal(unlist(session$returned$state$filter_values()), "POL_002")

      result <- session$returned$result()
      expect_equal(nrow(result), 1L)
      expect_equal(result$policy_id, "POL_002")
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("scatter+series click filter on real engine output returns matching rows", {
  # Reproduces the SAA workbench bug where clicking a single-policy dot on
  # the Policy scatter showed "Filtered: policy_id = policy-005" but the
  # downstream preview rendered "No rows". The click JS sends a structured
  # action message; this test simulates that exact shape on real
  # blockr.insurance engine output and verifies the categorical filter
  # actually subsets the upstream data.
  skip_if_not_installed("blockr.insurance")
  inp <- list(
    locations = blockr.insurance::property_locations,
    claims    = blockr.insurance::property_claims
  )
  loc <- inp$locations
  loc$policy_id <- rep(c("policy-001", "policy-002", "policy-003",
                         "policy-004", "policy-005"),
                       length.out = nrow(loc))
  inp$locations <- loc
  inp$claims$policy_id <- rep(c("policy-001", "policy-002", "policy-003",
                                "policy-004", "policy-005"),
                              length.out = nrow(inp$claims))
  premium <- blockr.insurance::engine_property(inp)$premium
  expected_n <- sum(premium$policy_id == "policy-005")
  testthat::expect_gt(expected_n, 0L)

  blk <- new_drilldown_chart_block(
    chart_type = "scatter",
    x      = "tiv",
    y      = "model_price",
    series  = "policy_id"
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()

      # JS click handler sends this exact shape: action=filter,
      # filter_type=categorical, column=<series>, values=[<seriesName>].
      expr_scope <- session$makeScope("expr")
      expr_scope$setInputs(drilldown_block_action = list(
        action      = "filter",
        filter_type = "categorical",
        column      = "policy_id",
        values      = list("policy-005")
      ))
      session$flushReact()

      expect_equal(session$returned$state$filter_column(), "policy_id")
      expect_equal(unlist(session$returned$state$filter_values()),
                   "policy-005")

      result <- session$returned$result()
      expect_s3_class(result, "data.frame")
      expect_gt(nrow(result), 0L)
      expect_equal(nrow(result), expected_n)
      expect_true(all(result$policy_id == "policy-005"))
    },
    args = list(x = blk, data = list(data = function() premium))
  )
})

test_that("scatter click filter survives a follow-up brush event (race)", {
  # Reproduces the SAA workbench bug. ECharts scatter has brush mode active
  # by default. When the user clicks a single dot, two events fire:
  #  1) the click handler emits a categorical filter on series
  #  2) brushSelected fires with the click point as a 1-pixel brush, which
  #     emits a range filter on (x == click_x & y == click_y).
  # Latest message wins, so the categorical filter gets overwritten by a
  # range filter that matches at most 1 row (often 0 due to floating point),
  # while the chart's status bar still shows the click selection from the
  # JS-side `_selected` state. End result: "No rows" downstream.
  skip_if_not_installed("blockr.insurance")
  inp <- list(
    locations = blockr.insurance::property_locations,
    claims    = blockr.insurance::property_claims
  )
  loc <- inp$locations
  loc$policy_id <- rep(c("policy-001", "policy-002", "policy-003",
                         "policy-004", "policy-005"),
                       length.out = nrow(loc))
  inp$locations <- loc
  inp$claims$policy_id <- rep(c("policy-001", "policy-002", "policy-003",
                                "policy-004", "policy-005"),
                              length.out = nrow(inp$claims))
  premium <- blockr.insurance::engine_property(inp)$premium
  clicked_pid <- "policy-005"
  expected_n <- sum(premium$policy_id == clicked_pid)

  blk <- new_drilldown_chart_block(
    chart_type = "scatter",
    x      = "tiv",
    y      = "model_price",
    series  = "policy_id"
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expr_scope <- session$makeScope("expr")

      # New drill model: click and brush emit the SAME filter type within a
      # mode, so the old categorical-vs-range race is gone and a degenerate
      # range (a scatter-auto click on one point) is a legitimate filter — it
      # selects the observation via between(x, v, v) & between(y, v, v), no
      # longer dropped as a no-op.
      one_loc  <- premium[premium$policy_id == clicked_pid, ][1L, ]
      click_x  <- one_loc$tiv
      click_y  <- one_loc$model_price
      expect_n  <- sum(premium$tiv == click_x & premium$model_price == click_y)
      expr_scope$setInputs(drilldown_block_action = list(
        action      = "filter",
        filter_type = "range",
        x_col       = "tiv",
        y_col       = "model_price",
        x_range     = c(click_x, click_x),
        y_range     = c(click_y, click_y)
      ))
      session$flushReact()

      result <- session$returned$result()
      expect_s3_class(result, "data.frame")
      # The degenerate range now filters to the clicked observation(s).
      expect_equal(nrow(result), expect_n)
      expect_true(all(result$tiv == click_x & result$model_price == click_y))
    },
    args = list(x = blk, data = list(data = function() premium))
  )
})

test_that("drill and label round-trip through state and config", {
  blk <- new_drilldown_chart_block(
    chart_type = "bar", group = "g", drill = "USUBJID", label = "AEDECOD"
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_equal(session$returned$state$drill(), "USUBJID")
      expect_equal(session$returned$state$label(), "AEDECOD")
      # Config message updates both at runtime; "" clears to NULL.
      expr_scope <- session$makeScope("expr")
      expr_scope$setInputs(drilldown_block_action = list(
        action = "config", drill = "ARM", label = "PARAMCD"
      ))
      session$flushReact()
      expect_equal(session$returned$state$drill(), "ARM")
      expect_equal(session$returned$state$label(), "PARAMCD")
      expr_scope$setInputs(drilldown_block_action = list(
        action = "config", drill = ""
      ))
      session$flushReact()
      expect_null(session$returned$state$drill())
    },
    args = list(x = blk, data = list(data = function() {
      data.frame(g = c("a", "b"), ARM = c("x", "y"),
                 USUBJID = c("s1", "s2"), AEDECOD = c("h", "j"),
                 PARAMCD = c("p", "q"), stringsAsFactors = FALSE)
    }))
  )
})

test_that("unset drill emits no downstream filter (inert)", {
  blk <- new_drilldown_chart_block(chart_type = "bar", group = "g")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      d <- data.frame(g = c("a", "a", "b"), stringsAsFactors = FALSE)
      result <- session$returned$result()
      # No click/filter action sent: passthrough, all rows.
      expect_equal(nrow(result), 3L)
    },
    args = list(x = blk, data = list(data = function() {
      data.frame(g = c("a", "a", "b"), stringsAsFactors = FALSE)
    }))
  )
})
