# A drill on the "(Missing)" row (`missing = "level"`) gives the rows whose
# value is missing, in every layout.

missing_df <- function() {
  data.frame(
    arm = c("A", "A", NA, "B", NA),
    sex = c("F", NA, "M", "F", "F"),
    age = c(30, NA, 50, 41, 22),
    id = c("s1", "s2", "s3", "s4", "s5"),
    stringsAsFactors = FALSE
  )
}

block_result <- function(blk, d) {
  out <- NULL
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      out <<- session$returned$result()
    },
    args = list(x = blk, data = list(data = function() d))
  )
  out
}

test_that("groups layout: a click on (Missing) keeps the NA rows", {
  d <- missing_df()
  blk <- new_table_block(
    layout = "groups", by = "arm", missing = "level", drill = "auto",
    selection = list(column = "arm", values = "(Missing)")
  )
  res <- block_result(blk, d)
  expect_identical(res$id, c("s3", "s5"))
  # The emitted filter says so in plain dplyr.
  ex <- summarize_drill_expr(list(arm = "(Missing)"))
  expect_match(paste(deparse(ex), collapse = ""), "is.na", fixed = TRUE)
  expect_identical(eval(ex, list(data = d))$id, c("s3", "s5"))
  # Other values filter as before.
  ex <- summarize_drill_expr(list(arm = "A"))
  expect_identical(eval(ex, list(data = d))$id, c("s1", "s2"))
})

test_that("records layout: the (Missing) header of a nested table claims", {
  d <- missing_df()
  nest <- table_nest(d, by = "arm", rowname = "id", missing = "level")
  expect_identical(nest$.group1_level, c("A", "A", "(Missing)", "(Missing)",
                                         "B"))
  # "drop" keeps today's header without a value.
  expect_true(anyNA(table_nest(d, by = "arm", rowname = "id")$.group1_level))
  # The header is clickable: it carries a claim on the level.
  keys <- dd_header_drill_attrs(nest, ".group1_level", 1L,
                                c(1L, NA, 1L, NA, 1L))$keys
  expect_match(keys[[3L]], "(Missing)", fixed = TRUE)

  blk <- new_table_block(
    layout = "records", by = "arm", rowname = "id", missing = "level",
    drill = "auto",
    selection = list(group_cols = ".group1_level", group_vals = "(Missing)")
  )
  res <- block_result(blk, d)
  expect_identical(res$.label, c("s3", "s5"))
})

test_that("variables layout: (Missing) resolves to the NA source rows", {
  d <- missing_df()
  tbl <- summary_table(d, vars = c("sex", "age"), by = "arm",
                       missing = "level")
  # Numeric variables stay numeric: age is a mean, not a list of levels.
  expect_false(any(tbl$.variable == "age" & !is.na(tbl$.variable_level) &
                     tbl$.variable_level %in% c("30", "50")))
  expect_true("(Missing)" %in% tbl$.variable_level)
  expect_true(any(grepl("(Missing)", names(tbl), fixed = TRUE)))
  # The source keeps its NA; a claim on (Missing) selects those rows.
  expect_identical(attr(tbl, "source_data"), d)
  hit <- tbl[tbl$.variable == "sex" & tbl$.variable_level %in% "(Missing)", ]
  expect_identical(drill_source(hit)$id, "s2")

  blk <- new_table_block(
    layout = "variables", by = "arm", missing = "level", drill = "source",
    summaries = list(list(type = "simple", func = "count", col = "sex",
                          color = "sex")),
    selection = list(
      group_cols = c(".variable", ".variable_level"),
      group_vals = c("sex", "(Missing)")
    )
  )
  res <- block_result(blk, d)
  expect_identical(res$id, "s2")
  expect_true(is.na(res$sex))
})

test_that("the variables layout's code matches its screen", {
  cfg <- list(vars = c("sex", "age"), by = "arm", missing = "level")
  cl <- table_variables_call(cfg)
  expect_identical(cl[[2L]], quote(data))
  expect_identical(cl$missing, "level")
  d <- missing_df()
  expect_identical(eval(cl, list(data = d)),
                   do.call(summary_table, c(list(d),
                                            table_variables_args(cfg))))
})

test_that("missing defaults to level; a saved drop stays drop", {
  ser <- function(b) blockr.core::blockr_ser(b)$payload
  expect_identical(ser(new_table_block())$missing, "level")
  expect_identical(ser(new_table_block(missing = "drop"))$missing, "drop")
  # A board saved by the new table block stores its choice, so a restore
  # keeps "drop".
  saved <- blockr.core::blockr_ser(new_table_block(layout = "groups",
                                                    by = "arm",
                                                    missing = "drop"))
  back <- blockr.core::blockr_deser(saved)
  expect_identical(ser(back)$missing, "drop")
  # A summarize table saved before `missing` existed takes the new default.
  old <- blockr_deser.summarize_table_block(
    structure(list(), class = "summarize_table_block"),
    list(payload = list(group = "arm", func = "count"))
  )
  expect_identical(ser(old)$missing, "level")
})
