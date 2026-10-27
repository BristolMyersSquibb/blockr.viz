# The groups layout without `by`: one row over all input rows, in every look.

no_by_df <- function() {
  data.frame(
    ARM = c("A", "A", "B", "B", "C"),
    SEV = c("mild", "severe", "mild", "mild", "severe"),
    AGE = c(60, 70, 50, 55, 65),
    stringsAsFactors = FALSE
  )
}

no_by_summaries <- function() {
  list(
    list(type = "simple", func = "count", show = "bar", name = "Rows"),
    list(type = "simple", func = "mean", col = "AGE", show = "number",
         name = "Age"),
    list(type = "simple", func = "count", show = "bar", color = "SEV",
         name = "Severity")
  )
}

test_that("without `by` the summaries are one Total row over all rows", {
  p <- lane_prepare_summaries(no_by_df(), by = character(),
                              summaries = no_by_summaries())
  expect_null(p$err)
  expect_true(p$all)
  expect_identical(as.character(p$rows$.label), "Total")
  m <- summarize_cells(p)
  expect_match(as.character(m$cols[[1L]]$disp), "^5")
  expect_identical(m$cols[[2L]]$v, 60)
  # Grouped tables are not flagged.
  expect_false(lane_prepare_summaries(no_by_df(), by = "ARM",
                                      summaries = no_by_summaries())$all)
})

test_that("the one-row payload has no row header, sort or drill", {
  p <- summarize_build_payload(
    no_by_df(), summaries = no_by_summaries(), by = character(),
    drill = "ARM", cfg = list(look = "table", sortable = TRUE)
  )
  expect_identical(p$n, 1L)
  expect_identical(as.character(p$label), "Total")
  expect_false(p$pick)
  expect_false(grepl("blockr-sortable", p$head, fixed = TRUE))
  expect_false(grepl(GROUPS_ALL_COL, p$head, fixed = TRUE))
})

test_that("the groups engine draws one row without `by`, and search and drill leave", {
  sent <- list()
  shiny::testServer(
    groups_engine(summaries = no_by_summaries(), search = TRUE,
                  drill = "ARM", filter_column = "ARM", filter_values = "A"),
    args = list(data = shiny::reactive(no_by_df())), {
      root <- session$rootScope()
      root$sendCustomMessage <- function(type, message) {
        if (identical(type, "blockr-viz-summarize-data")) {
          sent[[length(sent) + 1L]] <<- message
        }
      }
      session$flushReact()
      p <- jsonlite::fromJSON(sent[[length(sent)]]$payload,
                              simplifyVector = FALSE)
      expect_identical(p$n, 1L)
      expect_identical(p$label[[1L]], "Total")
      expect_false(p$chrome$search)
      expect_identical(p$chrome$drill, "")

      # Picking a grouping brings the rows back.
      session$setInputs(summarize_table_block_action = list(
        action = "config", param = "by", value = list("ARM")
      ))
      p <- jsonlite::fromJSON(sent[[length(sent)]]$payload,
                              simplifyVector = FALSE)
      expect_identical(p$n, 3L)
      expect_true(p$chrome$search)

      # Removing it again clears a selection made on the grouped rows.
      session$setInputs(summarize_table_block_action = list(
        action = "config", param = "by", value = list()
      ))
      expect_identical(session$returned$state$by(), character())
      expect_null(session$returned$state$filter_column())
      expect_identical(deparse(session$returned$expr()), "identity(data)")
    }
  )
})

test_that("a groups table starts with a count of rows", {
  b <- new_table_block(layout = "groups")
  s <- get("summaries", envir = environment(b$expr_server))
  expect_identical(s, table_groups_seed())

  # Switching from records to groups with no summaries seeds the same.
  b <- new_table_block()
  shiny::testServer(b$expr_server,
                    args = list(data = shiny::reactive(no_by_df())), {
    session$setInputs(`records-drilldown_table_block_action` = list(
      action = "config", param = "layout", value = "groups"
    ))
    expect_identical(session$returned$state$layout(), "groups")
    expect_identical(session$returned$state$summaries(), table_groups_seed())
  })
})

test_that("the report code and the downloads build the one-row table", {
  b <- new_table_block(layout = "groups", summaries = no_by_summaries(),
                       look = "cards")
  call <- report_call(b, "data")
  expect_false("by" %in% names(as.list(call)))
  e <- eval(call, list(data = no_by_df()))
  expect_s3_class(e, "summarize_exhibit")
  expect_identical(e$cells$n, 1L)

  df <- summarize_export_df(e$prep)
  expect_identical(nrow(df), 1L)
  expect_identical(df[[1L]], "Total")
  expect_true(all(nzchar(names(df))))

  e <- static_summarize_table(no_by_df(), summaries = no_by_summaries(),
                              look = "cards")
  expect_identical(e$cards$n, 1L)
  expect_false(e$cards$multi)
})
