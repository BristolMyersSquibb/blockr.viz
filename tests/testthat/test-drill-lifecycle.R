# Drill-filter lifecycle (table + tile parity with the chart): raw-value
# emission (data-raw), the filter-clear path, and restore indication (the
# data-dt-active / data-tk-active stamps + the server-rendered status line).
# Browser-side behavior (click-to-toggle, Reset) rides on these contracts and
# is exercised end-to-end in test-shinytest2.R.

library(shiny)

eval_block_expr <- function(ex, data) {
  e <- new.env(parent = globalenv())
  e$data <- data
  e$. <- function(x) x
  eval(ex, envir = e)
}

drill_df <- data.frame(
  region = c("North", "South", "East"),
  ratio  = c(0.123456, 0.654321, NA),
  stringsAsFactors = FALSE
)

# --- raw-value emission (data-raw) -----------------------------------------

test_that("drill cells carry the RAW value on data-raw, not the display", {
  h <- as.character(htmltools::renderTags(
    dt_table_tag(drill_df, label_col = "region", value_cols = "ratio",
                 drill = "ratio", digits = 2L)
  )$html)
  # Displayed rounded, filtered raw: both must be present.
  expect_true(grepl(">0.12<", h))
  expect_true(grepl("data-raw=\"0.123456\"", h))
  expect_true(grepl("data-raw=\"0.654321\"", h))
})

test_that("a stub drill column carries data-raw on the stub cells", {
  h <- as.character(htmltools::renderTags(
    dt_table_tag(drill_df, label_col = "region", value_cols = "ratio",
                 drill = "region")
  )$html)
  expect_true(grepl("blockr-stub\" data-raw=\"North\"", h))
})

test_that("non-drill columns carry no data-raw", {
  h <- as.character(htmltools::renderTags(
    dt_table_tag(drill_df, label_col = "region", value_cols = "ratio")
  )$html)
  expect_false(grepl("data-raw", h))
})

test_that("NA drill cells get no data-raw and mark the row dt-row-nodrill", {
  h <- as.character(htmltools::renderTags(
    dt_table_tag(drill_df, label_col = "region", value_cols = "ratio",
                 drill = "ratio")
  )$html)
  # Two raw-valued rows, the NA row emits neither attr nor a fake "—" value.
  expect_equal(lengths(regmatches(h, gregexpr("data-raw=", h))), 2L)
  expect_equal(lengths(regmatches(h, gregexpr("dt-row-nodrill", h))), 1L)
})

test_that("grouped tables carry data-raw on every group-key column", {
  agg <- dd_table_aggregate(
    data.frame(g1 = c("A", "A", "B"), g2 = c(1.55, 1.55, 2.05),
               v = c(1, 2, 3)),
    group = c("g1", "g2"),
    summaries = list(list(func = "sum", cols = "v"))
  )
  h <- as.character(htmltools::renderTags(
    dt_table_tag(agg$data, label_col = "g1",
                 value_cols = c("g2", agg$metric_cols),
                 group_cols = agg$group, group = agg$group,
                 summaries = list(list(func = "sum", cols = "v")))
  )$html)
  expect_true(grepl("data-raw=\"A\"", h))
  expect_true(grepl("data-raw=\"1.55\"", h))
  # The metric column is not a drill key -> no data-raw there.
  expect_false(grepl("data-raw=\"3\"", h))
})

test_that("structured stub cells carry data-raw for the drill", {
  d <- data.frame(
    .label = c("Age", "Sex"),
    Total  = c("34.5", "12 (40%)"),
    stringsAsFactors = FALSE
  )
  h <- as.character(htmltools::renderTags(
    dt_table_tag(d, drill = ".label")
  )$html)
  expect_true(grepl("data-raw=\"Age\"", h))
})

# --- active-filter stamp (restore indication) ------------------------------

test_that("dd_active_filter_json serializes single and grouped filters", {
  expect_null(dd_active_filter_json(NULL))
  expect_null(dd_active_filter_json(list(col = NULL, vals = NULL)))
  expect_equal(
    dd_active_filter_json(list(col = "region", vals = list("North"))),
    "{\"column\":\"region\",\"values\":[\"North\"]}"
  )
  expect_equal(
    dd_active_filter_json(list(gcols = c("g1", "g2"), gvals = c("A", "1.55"))),
    paste0("{\"filters\":[{\"column\":\"g1\",\"value\":\"A\"},",
           "{\"column\":\"g2\",\"value\":\"1.55\"}]}")
  )
})

test_that("an active filter is stamped on the <table> as data-dt-active", {
  h <- as.character(htmltools::renderTags(
    dt_table_tag(drill_df, label_col = "region", value_cols = "ratio",
                 drill = "region",
                 active = list(col = "region", vals = list("North")))
  )$html)
  expect_true(grepl("data-dt-active=", h))
  expect_true(grepl("North", h))
})

# --- table server: clear path + restore ------------------------------------

test_that("a clear filter message (null column/values) resets the filter", {
  blk <- new_table_block(drill = "region")
  testServer(blk$expr_server, args = list(data = reactive(drill_df)), {
    session$setInputs(drilldown_table_block_action = list(
      action = "filter", column = "region", values = list("North")
    ))
    expect_equal(nrow(eval_block_expr(session$returned$expr(), drill_df)), 1L)
    # The message table.js sends for re-click / Reset / drill-uncheck.
    session$setInputs(drilldown_table_block_action = list(
      action = "filter", filter_type = "categorical"
    ))
    expect_null(session$returned$state$filter_column())
    expect_null(session$returned$state$filter_values())
    expect_equal(nrow(eval_block_expr(session$returned$expr(), drill_df)), 3L)
  })
})

test_that("a clear filter message also resets a grouped drill filter", {
  blk <- new_table_block(group = "region", drill = "auto")
  testServer(blk$expr_server, args = list(data = reactive(drill_df)), {
    session$setInputs(drilldown_table_block_action = list(
      action = "filter",
      filters = list(list(column = "region", value = "North"))
    ))
    expect_equal(nrow(eval_block_expr(session$returned$expr(), drill_df)), 1L)
    session$setInputs(drilldown_table_block_action = list(
      action = "filter", filter_type = "categorical"
    ))
    expect_null(session$returned$state$filter_group_cols())
    expect_null(session$returned$state$filter_group_vals())
    expect_equal(nrow(eval_block_expr(session$returned$expr(), drill_df)), 3L)
  })
})

test_that("a raw-value filter matches rows the DISPLAYED text would miss", {
  blk <- new_table_block(drill = "ratio", digits = 2L)
  testServer(blk$expr_server, args = list(data = reactive(drill_df)), {
    # What table.js now sends: the data-raw value, not the rounded "0.12".
    session$setInputs(drilldown_table_block_action = list(
      action = "filter", column = "ratio", values = list("0.123456")
    ))
    out <- eval_block_expr(session$returned$expr(), drill_df)
    expect_equal(nrow(out), 1L)
    expect_equal(out$region, "North")
  })
})

test_that("restored filter state renders the status line and active stamp", {
  # Restore = re-calling the ctor with the saved state (blockr.core contract).
  blk <- new_table_block(drill = "region",
                         filter_column = "region",
                         filter_values = list("North"))
  testServer(blk$expr_server, args = list(data = reactive(drill_df)), {
    session$flushReact()
    # Output already filtered on restore.
    expect_equal(nrow(eval_block_expr(session$returned$expr(), drill_df)), 1L)
    # Status line: "Filtered: region = North" + a Reset control.
    status <- as.character(output$dt_status$html)
    expect_true(grepl("Filtered: region = North", status))
    expect_true(grepl("dd-status-reset", status))
    # The table body carries the active stamp for the JS row highlight.
    tbl <- as.character(output$dt_table$html)
    expect_true(grepl("data-dt-active=", tbl))
    expect_true(grepl("data-raw=\"North\"", tbl))
  })
})

test_that("with drill on but no filter the status line reads 'No filter'", {
  blk <- new_table_block(drill = "region")
  testServer(blk$expr_server, args = list(data = reactive(drill_df)), {
    session$flushReact()
    status <- as.character(output$dt_status$html)
    expect_true(grepl("No filter active", status))
    expect_false(grepl("dd-status-reset", status))
  })
})

# --- tile: clear path + restore --------------------------------------------

tile_df <- data.frame(
  region  = c("North", "South"),
  revenue = c(100, 80),
  stringsAsFactors = FALSE
)

test_that("tile: a clear filter message resets the filter state", {
  blk <- new_tile_block(value = "revenue", group = "region", drill = TRUE)
  testServer(blk$expr_server, args = list(data = reactive(tile_df)), {
    session$setInputs(tile_block_action = list(
      action = "filter", column = "region", values = list("South")
    ))
    expect_equal(nrow(eval_block_expr(session$returned$expr(), tile_df)), 1L)
    session$setInputs(tile_block_action = list(
      action = "filter", filter_type = "categorical"
    ))
    expect_null(session$returned$state$filter_col())
    expect_equal(nrow(eval_block_expr(session$returned$expr(), tile_df)), 2L)
  })
})

test_that("tile: restored filter renders active stamp + status footer", {
  blk <- new_tile_block(value = "revenue", group = "region", drill = TRUE,
                        filter_col = "region", filter_value = list("South"))
  testServer(blk$expr_server, args = list(data = reactive(tile_df)), {
    session$flushReact()
    expect_equal(nrow(eval_block_expr(session$returned$expr(), tile_df)), 1L)
    h <- as.character(output$tile_result$html)
    # A single value must stay a flat JSON array (["South"]), never nested.
    expect_true(grepl("data-tk-active=\"\\[&quot;South&quot;\\]\"", h))
    expect_true(grepl("Filtered: region = South", h))
    expect_true(grepl("dd-status-reset", h))
  })
})

test_that("tile: drill on with no filter shows the no-filter status line", {
  blk <- new_tile_block(value = "revenue", group = "region", drill = TRUE)
  testServer(blk$expr_server, args = list(data = reactive(tile_df)), {
    session$flushReact()
    h <- as.character(output$tile_result$html)
    expect_true(grepl("No filter active", h))
    expect_false(grepl("dd-status-reset", h))
    expect_false(grepl("data-tk-active=", h))
  })
})
