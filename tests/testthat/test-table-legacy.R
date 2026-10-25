# Boards and code from before the table block's rewrite (R/table-args.R,
# dev/table-block-rewrite.md): old names translate, old summarize tables come
# back as table blocks in the groups layout.

test_that("an old table payload restores with the new names", {
  s0 <- blockr.core::blockr_ser(new_table_block())
  old <- list(
    object = s0$object,
    payload = list(
      rowname = "var", value = NULL, group = NULL, summaries = list(),
      cell_color = list(type = "diverging", domain = c(-1, 1)),
      drill = NULL, digits = 2L, max_height = "600px",
      filter_type = NULL, filter_column = NULL, filter_values = NULL,
      rotate_titles = TRUE, ctrl_target = "vf", ctrl_table = "",
      title = NULL, block_name = "Cor"
    ),
    constructor = s0$constructor
  )
  p <- blockr.core::blockr_ser(blockr.core::blockr_deser(old))$payload
  expect_identical(p$layout, "records")
  expect_identical(p$rowname, "var")
  expect_identical(p$shadings[[1L]]$mode, "diverging")
  expect_true(p$compact)
  expect_null(p$max_height)
  expect_identical(p$drill_target, "vf")
  expect_identical(p$block_name, "Cor")
  expect_false(any(c("cell_color", "rotate_titles", "ctrl_target") %in%
                     names(p)))
})

test_that("an old table with in-block aggregation restores as groups", {
  b <- new_table_block(group = "region",
                       summaries = list(list(func = "sum", cols = "revenue")))
  p <- blockr.core::blockr_ser(b)$payload
  expect_identical(p$layout, "groups")
  expect_identical(p$by, "region")
  expect_identical(p$summaries[[1L]]$type, "simple")
  expect_identical(p$summaries[[1L]]$func, "sum")
  expect_identical(p$summaries[[1L]]$col, "revenue")
})

test_that("an old argument name never lands on a new one by partial match", {
  # `value` was the old table's column list; `value_labels` is new.
  p <- blockr.core::blockr_ser(new_table_block(value = c("a", "b")))$payload
  expect_identical(p$columns, c("a", "b"))
  expect_true(p$value_labels)
})

test_that("a new payload round-trips", {
  b <- new_table_block(
    layout = "groups", by = "species", id = "subject",
    summaries = list(list(type = "simple", func = "count", show = "bar"))
  )
  s <- blockr.core::blockr_ser(b)
  p <- blockr.core::blockr_ser(blockr.core::blockr_deser(s))$payload
  expect_identical(p$summaries, s$payload$summaries)
  expect_identical(p$id, "subject")
  expect_identical(p$layout, "groups")
})

test_that("an old summarize table payload restores as a table block", {
  old <- list(payload = list(
    group = "TERM", value = ".count", func = "count", id_var = "USUBJID",
    summaries = list(), by = NULL, values = list(), ctrl_target = "pt",
    filter_column = "TERM", filter_values = list("A"), block_name = "AE"
  ))
  b <- blockr_deser.summarize_table_block(
    structure(list(), class = "summarize_table_block"), old
  )
  expect_s3_class(b, "table_block")
  p <- blockr.core::blockr_ser(b)$payload
  expect_identical(p$layout, "groups")
  expect_identical(p$by, "TERM")
  expect_identical(p$id, "USUBJID")
  expect_identical(p$drill_target, "pt")
  expect_identical(p$selection$column, "TERM")
  expect_identical(p$block_name, "AE")
})

test_that("the layout switch moves the block between engines", {
  df <- data.frame(g = c("a", "a", "b"), x = 1:3)
  b <- new_table_block(by = "g", summaries = list(
    list(type = "simple", func = "count", show = "bar")
  ))
  shiny::testServer(b$expr_server, args = list(data = shiny::reactive(df)), {
    expect_identical(session$returned$state$layout(), "records")
    session$setInputs(`records-drilldown_table_block_action` = list(
      action = "config", param = "layout", value = "groups"
    ))
    expect_identical(session$returned$state$layout(), "groups")
    session$setInputs(`groups-summarize_table_block_action` = list(
      action = "config", param = "layout", value = "records"
    ))
    expect_identical(session$returned$state$layout(), "records")
  })
})

test_that("the old summary table constructor gives the variables layout", {
  b <- new_summary_table_block(vars = c("AGE", "SEX"), by = "ARM",
                               add_overall = TRUE, id_var = "USUBJID")
  expect_s3_class(b, "table_block")
  p <- blockr.core::blockr_ser(b)$payload
  expect_identical(p$layout, "variables")
  expect_identical(p$by, "ARM")
  expect_true(p$total)
  expect_identical(p$id, "USUBJID")
  expect_identical(table_summaries_vars(p$summaries), c("AGE", "SEX"))
  # The result saves as a table block, not under the old constructor.
  ctor <- blockr.core::blockr_deser(blockr.core::blockr_ser(b)$constructor)
  expect_identical(blockr.core::ctor_name(ctor), "new_table_block")
})

test_that("auto-made variable entries become dist summaries for numbers", {
  sm <- table_vars_to_summaries(list(), c("AGE", "SEX"))
  d <- data.frame(AGE = c(30, 40), SEX = c("F", "M"))
  fixed <- table_fix_var_types(sm, d)
  expect_identical(fixed[[1L]]$type, "dist")
  expect_identical(fixed[[2L]]$type, "simple")
  expect_identical(fixed[[2L]]$color, "SEX")
})

test_that("the variables layout builds Table 1 and the drill filters it", {
  df <- data.frame(arm = c("A", "A", "B"), age = c(30, 40, 50),
                   sex = c("F", "M", "F"))
  b <- new_table_block(
    layout = "variables", by = "arm",
    summaries = list(list(type = "dist", col = "age", show = "box"),
                     list(type = "simple", func = "count", show = "bar",
                          color = "sex", name = "sex"))
  )
  shiny::testServer(b$expr_server, args = list(data = shiny::reactive(df)), {
    session$flushReact()
    ex <- session$returned$expr()
    expect_match(paste(deparse(ex), collapse = " "), "summary_table")
    e <- new.env(parent = globalenv())
    e$data <- df
    e$. <- function(x) x
    out <- eval(ex, envir = e)
    expect_true(all(c(".variable", ".label") %in% names(out)))
    expect_setequal(unique(out$.variable), c("age", "sex"))
  })
})

test_that("groups and variables share the variables and the grouping", {
  df <- data.frame(g = c("a", "a", "b"), x = c(1, 2, 3))
  b <- new_table_block(
    layout = "groups", by = "g",
    summaries = list(list(type = "dist", col = "x", show = "box"))
  )
  shiny::testServer(b$expr_server, args = list(data = shiny::reactive(df)), {
    session$setInputs(`groups-summarize_table_block_action` = list(
      action = "config", param = "layout", value = "variables"
    ))
    session$flushReact()
    expect_identical(session$returned$state$layout(), "variables")
    expect_identical(session$returned$state$by(), "g")
    ex <- paste(deparse(session$returned$expr()), collapse = " ")
    expect_match(ex, "vars = \"x\"", fixed = TRUE)
    expect_match(ex, "by = \"g\"", fixed = TRUE)
  })
})
