# The summarize table's prepare script: the chart block's step, as the
# heatmap uses it (R/prepare-apply.R).

sp_toy <- function() {
  data.frame(
    USUBJID = c("1", "1", "2", "3", "3", "4"),
    AEDECOD = c("NAUSEA", "RASH", "NAUSEA", "NAUSEA", "RASH", "COUGH"),
    stringsAsFactors = FALSE
  )
}

sp_block <- function() {
  new_summarize_table_block(
    by = "AEDECOD",
    summaries = list(list(type = "simple", name = "n",
                          func = "count_distinct", col = "USUBJID",
                          show = "bar")),
    script = paste(
      "min_n <- 1  #| number(min = 1, max = 10)",
      "n <- tapply(data$USUBJID, data$AEDECOD, function(x) length(unique(x)))",
      "data[data$AEDECOD %in% names(n)[n >= min_n], , drop = FALSE]",
      sep = "\n"
    )
  )
}

test_that("the script's value is state, and the result is the prepared rows", {
  blk <- sp_block()
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      expr_scope <- session$makeScope("expr-groups")
      session$flushReact()
      st <- session$returned$state
      expect_match(st$script(), "min_n <- 1")
      expr_scope$setInputs(summarize_table_block_action = list(
        action = "config", param = "sv_min_n", value = 2
      ))
      session$flushReact()
      expect_identical(st$script_values()$min_n, 2)
      out <- eval(session$returned$expr(), list(data = sp_toy()))
      # COUGH has one patient, below the new minimum
      expect_setequal(unique(out$AEDECOD), c("NAUSEA", "RASH"))
    },
    args = list(x = blk, data = list(data = function() sp_toy()))
  )
})

test_that("a script edit from the gear replaces the script", {
  blk <- sp_block()
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      expr_scope <- session$makeScope("expr-groups")
      session$flushReact()
      expr_scope$setInputs(summarize_table_block_action = list(
        action = "config", param = "script",
        value = "data[data$AEDECOD == \"RASH\", , drop = FALSE]"
      ))
      session$flushReact()
      out <- eval(session$returned$expr(), list(data = sp_toy()))
      expect_identical(unique(out$AEDECOD), "RASH")
    },
    args = list(x = blk, data = list(data = function() sp_toy()))
  )
})

sp_age <- function() {
  data.frame(USUBJID = c("1", "2", "3", "4"), AGE = c(30, 50, 70, 40),
             SEX = c("F", "F", "M", "M"), stringsAsFactors = FALSE)
}

sp_age_block <- function(...) {
  new_summarize_table_block(
    by = "AGEGR",
    summaries = list(list(type = "simple", name = "n",
                          func = "count_distinct", col = "USUBJID",
                          show = "bar")),
    script = 'dplyr::mutate(data, AGEGR = ifelse(AGE < 45, "young", "old"))',
    drill = "AGEGR",
    ...
  )
}

test_that("a click on a column the script makes filters after the script", {
  # The restored click: the filter on AGEGR used to run on the source rows,
  # which have no AGEGR.
  blk <- sp_age_block(filter_column = "AGEGR", filter_values = list("young"))
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      out <- eval(session$returned$expr(), list(data = sp_age()))
      expect_setequal(out$USUBJID, c("1", "4"))
      expect_identical(unique(out$AGEGR), "young")
    },
    args = list(x = blk, data = list(data = function() sp_age()))
  )
  # A live click, on a path that also names an upstream column.
  blk <- sp_age_block()
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      scope <- session$makeScope("expr-groups")
      session$flushReact()
      scope$setInputs(summarize_table_block_action = list(
        action = "filter", type = "categorical",
        column = list("SEX", "AGEGR"), values = list("M", "old"), nonce = 1
      ))
      session$flushReact()
      out <- eval(session$returned$expr(), list(data = sp_age()))
      expect_identical(out$USUBJID, "3")
    },
    args = list(x = blk, data = list(data = function() sp_age()))
  )
})

test_that("a click on an upstream column still filters before the script", {
  sx <- quote(dplyr::select(.(data), USUBJID))
  ex <- summarize_drill_wrap(sx, list(SEX = "F"), src_cols = names(sp_age()))
  out <- eval(ex, list(data = sp_age()))
  expect_identical(out$USUBJID, c("1", "2"))
  # Upstream not seen yet: everything before the script, as before.
  ex <- summarize_drill_wrap(sx, list(AGEGR = "young"))
  expect_identical(ex[[1L]], quote(dplyr::select))
})
