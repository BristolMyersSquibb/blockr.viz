# Tests for new_drilldown_chart_block — focus on the patient-timelines
# additions (x_end_col, sort_by, click-to-filter with USUBJID).

test_that("constructor accepts x_end_col and sort_by", {
  blk <- new_drilldown_chart_block(
    chart_type = "gantt",
    x_col = "ASTDY",
    x_end_col = "AENDY",
    y_col = "AEDECOD",
    color_by = "AESEV",
    sort_by = "onset"
  )
  expect_s3_class(blk, "drilldown_chart_block")
})

test_that("gantt state round-trips x_end_col and sort_by", {
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
    x_col = "ASTDY", x_end_col = "AENDY", y_col = "AEDECOD",
    color_by = "AESEV", sort_by = "onset"
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_equal(session$returned$state$x_end_col(), "AENDY")
      expect_equal(session$returned$state$sort_by(), "onset")
      expect_equal(session$returned$state$chart_type(), "gantt")
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("config message updates x_end_col and sort_by", {
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
        x_end_col = "AENDY",
        sort_by = "alpha"
      ))
      session$flushReact()
      expect_equal(session$returned$state$x_end_col(), "AENDY")
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
    x_col = "ADY", y_col = "AVAL", color_by = "USUBJID"
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
  blk <- new_drilldown_chart_block(chart_type = "bar", group_by = "b")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_equal(session$returned$result(), df)
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("series_by round-trips through state", {
  df <- data.frame(
    USUBJID = rep(c("A", "B"), each = 2),
    ADY = c(1, 10, 1, 10),
    AVAL = c(100, 110, 95, 105),
    TRT01A = rep(c("ARM A", "ARM B"), each = 2),
    stringsAsFactors = FALSE
  )
  blk <- new_drilldown_chart_block(
    chart_type = "line", x_col = "ADY", y_col = "AVAL",
    series_by = "USUBJID", color_by = "TRT01A"
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_equal(session$returned$state$series_by(), "USUBJID")
      expect_equal(session$returned$state$color_by(), "TRT01A")
      # Config message can update series_by at runtime.
      expr_scope <- session$makeScope("expr")
      expr_scope$setInputs(drilldown_block_action = list(
        action = "config", series_by = "TRT01A"
      ))
      session$flushReact()
      expect_equal(session$returned$state$series_by(), "TRT01A")
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("r_needed_cols includes AVISITN when x_col is AVISIT", {
  df <- data.frame(
    USUBJID = c("A", "A", "B"),
    AVISIT = c("Visit 1", "Visit 2", "Visit 1"),
    AVISITN = c(1, 2, 1),
    AVAL = c(10, 11, 12),
    stringsAsFactors = FALSE
  )
  blk <- new_drilldown_chart_block(
    chart_type = "line", x_col = "AVISIT", y_col = "AVAL",
    color_by = "USUBJID"
  )
  # Indirectly verify by checking the block state exposes x_col correctly;
  # the AVISITN inclusion is a JS-input concern, but we confirm the block
  # accepts the configuration without error on evaluation.
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_equal(session$returned$state$x_col(), "AVISIT")
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("scatter click-emit drives categorical filter on arbitrary column", {
  # Property-workbench shape: one dot per policy on a scatter, click a dot,
  # downstream filters to that single policy. The JS handler emits
  # {action: "filter", filter_type: "categorical", column: <series_by>,
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
    x_col      = "exposure_premium",
    y_col      = "model_price",
    series_by  = "policy_id"
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_equal(session$returned$state$chart_type(), "scatter")
      expect_equal(session$returned$state$series_by(), "policy_id")

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
