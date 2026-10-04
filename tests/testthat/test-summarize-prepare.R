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
      expr_scope <- session$makeScope("expr")
      session$flushReact()
      st <- session$returned$state
      expect_match(st$script(), "min_n <- 1")
      expr_scope$setInputs(summarize_table_block_action = list(
        action = "config", param = "sv_min_n", value = 2
      ))
      session$flushReact()
      expect_identical(st$values()$min_n, 2)
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
      expr_scope <- session$makeScope("expr")
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
