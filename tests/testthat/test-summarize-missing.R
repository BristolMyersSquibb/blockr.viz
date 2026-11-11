# The old summarize table drew no row for a missing group and counted its
# rows in N (0.2.200). Restored boards and the old constructor keep that; the
# table block's own default is "(Missing)".

sm_data <- function() {
  # 1-3 have events; 1's second event lost its term to a threshold, 4's SOC
  # lost all its terms; 5 and 6 come from a population join.
  data.frame(
    USUBJID = c("1", "1", "2", "3", "4", "5", "6"),
    AEBODSYS = c("SKIN", "SKIN", "GI", "GI", NA, NA, NA),
    AEDECOD = c("RASH", NA, "NAUSEA", "NAUSEA", NA, NA, NA),
    stringsAsFactors = FALSE
  )
}

sm_view <- function(block, data = sm_data()) {
  st <- blockr.core::blockr_ser(block)$payload
  st$block_name <- NULL
  st <- table_normalize(viz_check(viz_block_spec("table"), st))
  m <- table_model_groups(st, data)
  expect_null(m$prep$err)
  table_groups_to_model(m)
}

sm_saved <- function() {
  blockr.core:::read_json(test_path("fixtures", "summarize-block-0.2.200.json"))
}

test_that("a restored 0.2 summarize table draws no missing group", {
  b <- suppressWarnings(blockr.core::blockr_deser(sm_saved()))
  expect_s3_class(b, "table_block")
  expect_identical(blockr.core::blockr_ser(b)$payload$missing, "drop")
  v <- sm_view(b)
  expect_identical(as.character(v$cols[[1L]]$disp),
                   c("GI", "NAUSEA", "SKIN", "RASH"))
})

test_that("new_summarize_table_block() drops missing groups by default", {
  s <- list(list(type = "simple", func = "count_distinct", col = "USUBJID",
                 show = "number"))
  b <- new_summarize_table_block(by = c("AEBODSYS", "AEDECOD"),
                                 summaries = s)
  expect_identical(blockr.core::blockr_ser(b)$payload$missing, "drop")
  expect_true(grepl("missing = \"drop\"",
                    paste(deparse(report_call(b, "x")), collapse = " "),
                    fixed = TRUE))
  # An explicit choice is kept; the table block's default is unchanged.
  b <- new_summarize_table_block(by = "AEDECOD", summaries = s,
                                 missing = "level")
  expect_identical(blockr.core::blockr_ser(b)$payload$missing, "level")
  b <- new_table_block(layout = "groups", by = "AEDECOD", summaries = s)
  expect_identical(blockr.core::blockr_ser(b)$payload$missing, "level")
})

test_that("dropped missing groups stay in the denominator", {
  b <- suppressWarnings(blockr.core::blockr_deser(sm_saved()))
  v <- sm_view(b)
  expect_identical(v$head[[1L]][[2L]]$sub, "N = 6")
  expect_identical(as.character(v$cols[[2L]]$disp), c("2", "2", "1", "1"))
  expect_identical(as.character(v$cols[[2L]]$pct),
                   c("(33%)", "(33%)", "(17%)", "(17%)"))
})

test_that("a board saved under the lane chart or rank names restores", {
  for (cls in list(c("lane_chart_block", "rank_block"), "rank_block")) {
    saved <- sm_saved()
    saved$object <- c(cls, saved$object[-(1:3)])
    saved$constructor$constructor <- paste0("new_", cls[[1L]])
    b <- suppressWarnings(blockr.core::blockr_deser(saved))
    expect_s3_class(b, "table_block")
    expect_false(inherits(b, "rank_block"))
    expect_identical(blockr.core::blockr_ser(b)$constructor$constructor,
                     "new_table_block")
    v <- sm_view(b)
    expect_identical(as.character(v$cols[[1L]]$disp),
                     c("GI", "NAUSEA", "SKIN", "RASH"))
  }
})
