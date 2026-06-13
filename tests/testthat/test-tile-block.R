df <- tile_demo_data()$scorecard
reg <- tile_demo_data()$regions

# ---------------------------------------------------------------------------
# Render layer (unit) — the long-frame reshape, formatting, and per-style HTML
# ---------------------------------------------------------------------------

test_that("tile_long_frame reshapes wide and long input", {
  # long: one row per (group, measure)
  lf <- blockr.bi:::tile_long_frame(df, value = "value", measure = "metric")
  expect_equal(nrow(lf), nrow(df))
  expect_setequal(lf$measure, df$metric)

  # wide: each value column becomes a measure
  wf <- blockr.bi:::tile_long_frame(reg, value = c("revenue", "orders"),
                                    by = "region")
  expect_equal(nrow(wf), nrow(reg) * 2L)
  expect_setequal(unique(wf$measure), c("revenue", "orders"))
  expect_setequal(unique(wf$group), reg$region)
})

test_that("empty / missing input yields an empty cell frame", {
  expect_equal(nrow(blockr.bi:::tile_long_frame(data.frame(), value = "x")), 0L)
  expect_equal(nrow(blockr.bi:::tile_long_frame(df, value = "nope")), 0L)
})

test_that("format inference and explicit formats", {
  pct <- blockr.bi:::tk_resolve_format("auto", "conversion", reg$conversion)
  expect_equal(pct$kind, "percent")
  usd <- blockr.bi:::tk_resolve_format("usd", "revenue", c(1240000))
  expect_equal(usd$kind, "currency")
  expect_match(blockr.bi:::tk_format(1240000, usd), "^\\$1,240,000")
  expect_match(blockr.bi:::tk_format(-128000, usd), "^−\\$")  # minus sign
  cmp <- blockr.bi:::tk_resolve_format("compact", "x", 1240000)
  expect_equal(blockr.bi:::tk_format(1240000, cmp), "1.2M")
})

test_that("delta polarity respects good_when", {
  # increase good when up; decrease good when down
  up_pos <- blockr.bi:::tk_delta_class(0.12, "up")
  dn_neg <- blockr.bi:::tk_delta_class(-0.006, "down")
  expect_equal(up_pos, "good")
  expect_equal(dn_neg, "good")
  expect_equal(blockr.bi:::tk_delta_class(-0.03, "up"), "bad")
  expect_equal(blockr.bi:::tk_delta_class(0, "up"), "flat")
})

test_that("each style x layout renders a valid payload", {
  ren <- function(...) paste(as.character(blockr.bi:::tile_html(...)), collapse = "")
  expect_true(grepl("tk-delta", ren(df, value = "value", measure = "metric",
                                    secondary = "delta", style = "delta")))
  expect_true(grepl("tk-fill__bar", ren(df, value = "value", measure = "metric",
                                        secondary = "progress", style = "fill")))
  expect_true(grepl("tk-pill", ren(df, value = "value", measure = "metric",
                                   secondary = "status", style = "pill")))
  # table matrix (grouped wide)
  mx <- ren(reg, value = c("revenue", "conversion", "orders"), by = "region",
            layout = "table")
  expect_true(grepl("tk-table", mx) && grepl("th-unit", mx))
  # empty
  expect_true(grepl("is-empty", ren(data.frame(), value = "value")))
})

# ---------------------------------------------------------------------------
# Block server — state round-trip, config actions, drill, no-wedge
# ---------------------------------------------------------------------------

test_that("block state round-trips constructor args", {
  blk <- new_tile_block(value = "value", measure = "metric", secondary = "delta",
                        style = "delta", good_when = "up", layout = "cards")
  expect_s3_class(blk, "tile_block")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      st <- session$returned$state
      expect_equal(st$value(), "value")
      expect_equal(st$measure(), "metric")
      expect_equal(st$style(), "delta")
      expect_equal(st$layout(), "cards")
      expect_false(st$drill())
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("config action switches layout; no wedge on clearing a listed field", {
  blk <- new_tile_block(value = "value", measure = "metric")
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
      expect_equal(session$returned$state$measure(), "")
      expect_s3_class(session$returned$result(), "data.frame")
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("no click = pass-through; a drill click filters downstream", {
  blk <- new_tile_block(value = c("revenue", "orders"), by = "region",
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
