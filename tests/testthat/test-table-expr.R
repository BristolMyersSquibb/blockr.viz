# The table's emitted filters (R/table-expr.R) from the kit's selection.

test_that("a flat records click filters its column", {
  sel <- list(type = "categorical", filters = list(cat = "b"))
  expect_identical(
    table_records_expr(table_click(sel)),
    quote(dplyr::filter(blockr.viz::as_annotated_df(.(data)),
                        .data[["cat"]] == "b"))
  )
  two <- list(type = "categorical", filters = list(cat = c("a", "c")))
  expect_identical(
    deparse(table_records_expr(table_click(two))),
    deparse(quote(dplyr::filter(blockr.viz::as_annotated_df(.(data)),
                                .data[["cat"]] %in% c("a", "c"))))
  )
})

test_that("no click keeps every row", {
  expect_identical(
    table_records_expr(table_click(NULL)),
    quote(dplyr::filter(blockr.viz::as_annotated_df(.(data)), TRUE))
  )
})

test_that("a structured click ANDs its identity keys", {
  sel <- list(type = "categorical",
              filters = list(.variable = "cat", .variable_level = "a"))
  expect_identical(
    table_records_expr(table_click(sel, structured = TRUE)),
    quote(dplyr::filter(
      blockr.viz::as_annotated_df(.(data)),
      .data[[".variable"]] == "cat" & .data[[".variable_level"]] == "a"
    ))
  )
})

test_that("the source mode wraps the filter and adds the column's arm", {
  sel <- list(type = "categorical",
              filters = list(.variable = "cat", .variable_level = "b"),
              columns = list(grp = "g2"))
  ex <- table_records_expr(table_click(sel, structured = TRUE),
                           source = TRUE)
  expect_identical(ex[[1L]], quote(dplyr::filter))
  expect_identical(ex[[2L]][[1L]], quote(blockr.viz::drill_source))
  expect_identical(
    table_records_expr(table_click(NULL), source = TRUE)[[3L]],
    "all"
  )
})

test_that("a groups click is its filters and its column claim", {
  sel <- list(type = "categorical", filters = list(grp = "g1", cat = "a"))
  expect_identical(table_selection_groups(sel),
                   list(grp = "g1", cat = "a"))
  expect_identical(table_selection_groups(NULL), list())
  cell <- list(type = "categorical", filters = list(ARM = "A"),
               columns = list(SEX = "M"))
  expect_identical(table_selection_groups(cell), list(ARM = "A", SEX = "M"))
})

test_that("a table selection saved by the reference reads in the kit's shape", {
  flat <- legacy_table_selection(list(selection = list(
    column = "cat", values = list("a", "c")
  )))
  expect_identical(flat$selection,
                   list(type = "categorical", filters = list(cat = c("a", "c")),
                        columns = list()))
  st <- legacy_table_selection(list(selection = list(
    group_cols = list(".variable", ".variable_level"),
    group_vals = list("cat", "b"),
    col_keys = list(list(column = "grp", values = list("g2")))
  )))
  expect_identical(st$selection, list(
    type = "categorical",
    filters = list(.variable = "cat", .variable_level = "b"),
    columns = list(grp = "g2")
  ))
  kept <- list(type = "categorical", filters = list(a = "1"))
  expect_identical(legacy_table_selection(list(selection = kept))$selection,
                   kept)
})
