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
  # The output is the input rows, as without `by`.
  res <- block_result(blk, d)
  expect_identical(names(res), names(d))
  expect_identical(res$id, c("s3", "s5"))
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

test_that("a missing bar or (Missing) row sent to a filter block claims NA", {
  d <- missing_df()
  # The table's "(Missing)" row and the chart's missing value (NA) both
  # claim with the value filter's own token.
  for (v in list("(Missing)", NA_character_)) {
    cl <- dd_ctrl_claims(d, "", list(arm = v))
    expect_identical(cl[[1L]]$name, "arm")
    expect_identical(cl[[1L]]$values, "<NA>")
  }
  # With the board's subject column, the subjects behind the NA rows.
  cl <- dd_ctrl_claims(d, "", list(arm = "(Missing)"), subject = "id")
  expect_identical(cl[[1L]]$name, "id")
  expect_identical(sort(as.character(cl[[1L]]$values)), c("s3", "s5"))
  expect_identical(cl[[1L]]$label, "arm = (missing)")
  # A structured table resolves the token against its source.
  tbl <- summary_table(d, vars = "sex", by = "arm", missing = "level")
  hit <- tbl[tbl$.variable == "sex" & tbl$.variable_level %in% "(Missing)", ]
  ids <- dd_claim_subjects(
    hit, rep(TRUE, nrow(hit)),
    list(list(name = "sex", values = "<NA>")), "id"
  )
  expect_identical(ids, "s2")
  # Ordinary values claim as before.
  expect_identical(dd_ctrl_claims(d, "", list(arm = "A"))[[1L]]$values, "A")
})

test_that("records layout: a nested header claim filters the input rows", {
  d <- missing_df()
  ex <- table_nest_drill_expr("arm", ".group1_level", "A")
  expect_identical(eval(ex, list(data = d))$id, c("s1", "s2"))
  ex <- table_nest_drill_expr(c("arm", "sex"),
                              c(".group1_level", ".group2_level"),
                              c("A", "(Missing)"))
  expect_identical(eval(ex, list(data = d))$id, "s2")
  expect_identical(eval(table_nest_drill_expr("arm", NULL, NULL),
                        list(data = d)), d)
  # The footer names the column the header stands for.
  nest <- table_nest(d, by = "arm", rowname = "id")
  expect_identical(dt_claim_label(".group1_level", "A", dt_group_names(nest)),
                   "arm = A")
  expect_identical(dt_claim_label(".group1_level", "A"), "A")
})

test_that("records layout: a nested row claims its group and its drill value", {
  d <- missing_df()
  nest <- table_nest(d, by = "arm", rowname = "id")
  v <- annotated_structure_view(nest)
  keys <- dd_row_drill_attrs(v$data, v$section_cols, drill_col = "sex")$keys
  # s1 under A, sex F; s2 has no sex and claims nothing.
  expect_match(keys[[1L]], '"column":".group1_level","value":"A"', fixed = TRUE)
  expect_match(keys[[1L]], '"column":"sex","value":"F"', fixed = TRUE)
  expect_identical(keys[[2L]], "")
  # The rowname moved to `.label` and still drills under its own name.
  keys <- dd_row_drill_attrs(v$data, v$section_cols, drill_col = "id")$keys
  expect_match(keys[[1L]], '"column":"id","value":"s1"', fixed = TRUE)
  # A mode is not a column: rows stay inert, headers drill.
  expect_true(all(!nzchar(
    dd_row_drill_attrs(v$data, v$section_cols, drill_col = "auto")$keys)))

  ex <- table_nest_drill_expr("arm", c(".group1_level", "sex"), c("A", "F"))
  expect_identical(eval(ex, list(data = d))$id, "s1")
  expect_identical(dt_claim_label(c(".group1_level", "sex"), c("A", "F"),
                                  dt_group_names(nest)),
                   c("arm = A", "sex = F"))
})

test_that("variables layout: a cell claims its row and its arm", {
  d <- missing_df()
  tbl <- summary_table(d, vars = "sex", by = "arm", add_overall = TRUE)
  keys <- annotated_column_keys(tbl)
  # Each arm column names its arm; the Total column claims nothing.
  expect_identical(keys[["A"]], list(list(column = "arm", values = "A")))
  expect_null(keys[["Total"]])
  ck <- list(list(column = "arm", values = list("A")))
  expect_identical(dd_col_keys_label(ck), "arm = A")
  hit <- tbl[tbl$.variable_level %in% "F", , drop = FALSE]
  ex <- dd_col_keys_filter(quote(blockr.viz::drill_source(hit)), ck)
  expect_identical(eval(ex, list(hit = hit))$id, "s1")
  # Saved with the selection and stamped for the restore.
  json <- dd_active_filter_json(list(gcols = ".variable_level", gvals = "F",
                                     ckeys = ck))
  expect_match(json, '"col_filters":[{"column":"arm","values":["A"]}]',
               fixed = TRUE)
})
