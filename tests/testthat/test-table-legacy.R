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
