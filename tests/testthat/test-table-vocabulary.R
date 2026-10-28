# Phase 5 of the table rewrite (dev/table-block-rewrite.md): the public
# argument and summary-key vocabulary, shared with the chart block.

vocab_df <- function() {
  data.frame(
    ARM = c("A", "A", "B", "B", NA),
    SEV = c("mild", "severe", "mild", "mild", "mild"),
    VAL = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )
}

test_that("old summary keys read as the spec's", {
  dist <- legacy_summary_entry(list(type = "dist", col = "x",
                                    inner = "mean_sd", outer = "min_max"))
  expect_identical(dist$summary, "mean_sd")
  expect_identical(dist$whiskers, "min_max")
  expect_false(any(c("inner", "outer", "stat") %in% names(dist)))

  pair <- legacy_summary_entry(list(type = "pair", from = "BASE", to = "AVAL",
                                    from_func = "identity", to_func = "max",
                                    lo = "ANRLO", hi = 20))
  expect_identical(pair[c("x", "xend", "x_func", "xend_func", "ref_min",
                          "ref_max")],
                   list(x = "BASE", xend = "AVAL", x_func = "identity",
                        xend_func = "max", ref_min = "ANRLO", ref_max = "20"))
  expect_false(any(c("from", "to", "lo", "hi") %in% names(pair)))

  spans <- legacy_summary_entry(list(type = "spans", x = "S", xend = "E",
                                     fields = c("A", "B")))
  expect_identical(spans$tooltip, c("A", "B"))

  bar <- legacy_summary_entry(list(type = "simple", color = "SEV",
                                   position = "grouped"))
  expect_identical(bar$position, "dodge")
})

test_that("the engine reads the spec's keys", {
  d <- data.frame(g = c("a", "a", "b"), BASE = c(1, 2, 3), AVAL = c(2, 4, 5))
  p <- lane_prepare_summaries(d, by = "g", summaries = list(
    list(type = "pair", x = "BASE", xend = "AVAL", ref_min = 0, ref_max = 4)
  ))
  expect_null(p$err)
  q <- lane_prepare_summaries(d, by = "g", summaries = list(
    list(type = "dist", col = "AVAL", summary = "mean_sd", show = "text")
  ))
  expect_null(q$err)
  expect_identical(q$plan[[1L]]$kind, "num")
})

test_that("position sets a colour split's layout", {
  d <- vocab_df()[!is.na(vocab_df()$ARM), ]
  plan_mode <- function(pos) {
    p <- lane_prepare_summaries(d, by = "ARM", summaries = list(
      list(type = "simple", func = "count", show = "bar", color = "SEV",
           position = pos)
    ))
    p$plan[[1L]]$mode
  }
  expect_identical(plan_mode("dodge"), "grouped")
  expect_identical(plan_mode("fill"), "percent")
  expect_identical(plan_mode("stack"), "stacked")
})

test_that("pct_of = total takes a facet copy's share of the whole table", {
  d <- vocab_df()[!is.na(vocab_df()$ARM), ]
  S <- list(list(type = "simple", func = "count", show = "bar",
                 facet = "SEV"))
  facet <- lane_prepare_summaries(d, by = "ARM", summaries = S)
  total <- lane_prepare_summaries(d, by = "ARM", summaries = S,
                                  pct_of = "total")
  expect_identical(facet$plan[[1L]]$val_denom, 3L)  # three mild rows
  expect_identical(total$plan[[1L]]$val_denom, 4L)  # four rows in all
})

test_that("missing = level shows a missing group as its own row", {
  S <- list(list(type = "simple", func = "count", show = "bar"))
  drop <- summarize_prepare(vocab_df(), by = "ARM", summaries = S)
  level <- summarize_prepare(vocab_df(), by = "ARM", summaries = S,
                             missing = "level")
  expect_false("(Missing)" %in% drop$rows$.label)
  expect_true("(Missing)" %in% level$rows$.label)
})

test_that("format replaces digits", {
  expect_identical(table_format_state("number"), "number")
  expect_identical(table_format_state(0), 0L)
  expect_identical(table_format_state("1"), 1L)
  p <- blockr.core::blockr_ser(new_table_block(digits = 0L))$payload
  expect_identical(p$format, 0L)
  expect_false("digits" %in% names(p))
})

test_that("the variables layout's emitted code keeps missing levels", {
  call <- table_variables_call(list(vars = "SEV", by = "ARM",
                                    missing = "level"))
  expect_identical(call$missing, "level")
  out <- eval(call, list(data = vocab_df()))
  expect_true(any(grepl("(Missing)", names(out), fixed = TRUE)))
})

test_that("gt_table is off the block list, the table block is on it", {
  ids <- names(blockr.core::available_blocks())
  expect_false("gt_table_block" %in% ids)
  expect_true("table_block" %in% ids)
})
