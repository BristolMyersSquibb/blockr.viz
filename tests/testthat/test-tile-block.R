df <- tile_demo_data()$scorecard
reg <- tile_demo_data()$regions

# ---------------------------------------------------------------------------
# Render layer (unit) — the long-frame reshape, formatting, and per-style HTML
# ---------------------------------------------------------------------------

test_that("tile_long_frame reshapes wide and long input", {
  # long: one row per (group, measure)
  lf <- blockr.viz:::tile_long_frame(df, value = "value", measure = "value")
  expect_equal(nrow(lf), nrow(df))
  expect_setequal(lf$measure, df$value)

  # wide: each value column becomes a measure
  wf <- blockr.viz:::tile_long_frame(reg, value = c("revenue", "orders"),
                                    by = "region")
  expect_equal(nrow(wf), nrow(reg) * 2L)
  expect_setequal(unique(wf$measure), c("revenue", "orders"))
  expect_setequal(unique(wf$group), reg$region)
})

test_that("empty / missing input yields an empty cell frame", {
  expect_equal(nrow(blockr.viz:::tile_long_frame(data.frame(), value = "x")), 0L)
  expect_equal(nrow(blockr.viz:::tile_long_frame(df, value = "nope")), 0L)
})

test_that("format is predictable — no currency inference from names", {
  # plain number: separators, no $ ever (even for a column named "revenue")
  num <- blockr.viz:::tk_resolve_format("number", c(1240000))
  expect_equal(num$kind, "number")
  expect_equal(blockr.viz:::tk_format(1240000, num), "1,240,000")
  expect_equal(blockr.viz:::tk_format(-128000, num), "-128,000")
  # compact: no symbol
  cmp <- blockr.viz:::tk_resolve_format("compact", 1240000)
  expect_equal(blockr.viz:::tk_format(1240000, cmp), "1.2M")
  # percent: a fraction is scaled x100 + %
  pct <- blockr.viz:::tk_resolve_format("percent", c(0.038, 0.041))
  expect_equal(pct$kind, "percent")
  expect_equal(pct$scale, 100)
  expect_equal(blockr.viz:::tk_format(0.038, pct), "3.8%")
})

test_that("unit is a free-text label rendered next to the value", {
  ren <- function(...) paste(as.character(blockr.viz:::tile_html(...)), collapse = "")
  out <- ren(reg, value = "revenue", measure = "region", unit = "USD")
  expect_true(grepl("tk-unit", out) && grepl("USD", out))
  expect_false(grepl("\\$", out))  # never an inferred currency symbol
})

test_that("delta polarity respects good_when", {
  # increase good when up; decrease good when down
  up_pos <- blockr.viz:::tk_delta_class(0.12, "up")
  dn_neg <- blockr.viz:::tk_delta_class(-0.006, "down")
  expect_equal(up_pos, "good")
  expect_equal(dn_neg, "good")
  expect_equal(blockr.viz:::tk_delta_class(-0.03, "up"), "bad")
  expect_equal(blockr.viz:::tk_delta_class(0, "up"), "flat")
})

test_that("each style x layout renders a valid payload", {
  ren <- function(...) paste(as.character(blockr.viz:::tile_html(...)), collapse = "")
  expect_true(grepl("tk-delta", ren(df, value = "value", measure = "value",
                                    secondary = "delta", style = "delta")))
  expect_true(grepl("tk-fill__bar", ren(df, value = "value", measure = "value",
                                        secondary = "progress", style = "fill")))
  expect_true(grepl("tk-pill", ren(df, value = "value", measure = "value",
                                   secondary = "status", style = "pill")))
  # table matrix (grouped wide) — a real matrix with per-group rows
  mx <- ren(reg, value = c("revenue", "conversion", "orders"), group = "region",
            layout = "table")
  expect_true(grepl("tk-table", mx) && grepl("data-group", mx))
  # an explicit unit shows in the matrix header (no inference)
  mxu <- ren(reg, value = "revenue", group = "region", unit = "USD", layout = "table")
  expect_true(grepl("th-unit", mxu) && grepl("USD", mxu))
  # empty
  expect_true(grepl("is-empty", ren(data.frame(), value = "value")))
})

# ---------------------------------------------------------------------------
# In-block aggregation (shared with the table): grouped summaries + grand totals
# ---------------------------------------------------------------------------

test_that("summaries aggregate in place — one card cluster per group level", {
  ren <- function(...) paste(as.character(blockr.viz:::tile_html(...)), collapse = "")
  out <- ren(reg, group = "region",
             summaries = list(list(func = "sum", cols = list("revenue"))))
  # one card per region (grouped), and the drill group attribute is present
  expect_true(grepl("tk-grid", out))
  for (r in reg$region) expect_true(grepl(r, out, fixed = TRUE))
})

test_that("summaries with no group render grand-total cards (one row)", {
  agg <- blockr.viz:::dd_table_aggregate(
    reg, character(),
    list(list(func = "count", cols = list()),
         list(func = "mean", cols = list("revenue")))
  )
  expect_true(agg$aggregated)
  expect_equal(nrow(agg$data), 1L)              # a single totals row
  expect_setequal(agg$metric_cols, names(agg$data))
  # and no group -> no per-row drill keys
  expect_length(agg$group, 0L)
})

test_that("neither group nor summaries -> raw passthrough (not aggregated)", {
  agg <- blockr.viz:::dd_table_aggregate(reg, character(), list())
  expect_false(agg$aggregated)
  expect_identical(agg$data, reg)
})

# ---------------------------------------------------------------------------
# Block server — state round-trip, config actions, drill, no-wedge
# ---------------------------------------------------------------------------

test_that("block state round-trips constructor args", {
  blk <- new_tile_block(value = "value", name = "value", secondary = "delta",
                        style = "delta", good_when = "up", layout = "cards")
  expect_s3_class(blk, "tile_block")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      st <- session$returned$state
      expect_equal(st$value(), "value")
      expect_equal(st$name(), "value")
      expect_equal(st$style(), "delta")
      expect_equal(st$layout(), "cards")
      expect_false(st$drill())
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("config action switches layout; no wedge on clearing a listed field", {
  blk <- new_tile_block(value = "value", measure = "value")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      es <- session$makeScope("expr")
      es$setInputs(tile_block_action = list(action = "config",
                                            param = "layout", value = "table"))
      session$flushReact()
      expect_equal(session$returned$state$layout(), "table")

      # clear an allow_empty_state field -> result still computes (no wedge)
      es$setInputs(tile_block_action = list(action = "config",
                                            param = "measure", value = "(none)"))
      session$flushReact()
      expect_equal(session$returned$state$name(), "")
      expect_s3_class(session$returned$result(), "data.frame")
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("group + summaries config actions round-trip via the gear", {
  blk <- new_tile_block(value = "revenue")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      es <- session$makeScope("expr")
      es$setInputs(tile_block_action = list(action = "config",
                                            param = "group", value = "region"))
      es$setInputs(tile_block_action = list(action = "config", param = "summaries",
        value = '[{"func":"sum","cols":["revenue"]}]'))
      session$flushReact()
      expect_equal(session$returned$state$group(), "region")
      ms <- session$returned$state$summaries()
      expect_equal(ms[[1]]$func, "sum")
      expect_equal(ms[[1]]$cols, "revenue")
      # aggregation is a display projection: data output stays the raw frame
      expect_equal(nrow(session$returned$result()), nrow(reg))
    },
    args = list(x = blk, data = list(data = function() reg))
  )
})

test_that("no click = pass-through; a drill click filters downstream", {
  blk <- new_tile_block(value = c("revenue", "orders"), group = "region",
                        layout = "table", drill = TRUE)
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_equal(nrow(session$returned$result()), nrow(reg))

      es <- session$makeScope("expr")
      es$setInputs(tile_block_action = list(action = "filter", column = "region",
                                            values = list("EMEA")))
      session$flushReact()
      res <- session$returned$result()
      expect_equal(nrow(res), 1L)
      expect_equal(res$region, "EMEA")
    },
    args = list(x = blk, data = list(data = function() reg))
  )
})
