# Settings that had no control in one layout or another, now reachable from
# the gear (dev/table-block-rewrite.md, "Gear gaps"). Each gear edit arrives
# as a config action on the engine that draws the layout.

sizes <- data.frame(
  Species = c("setosa", "versicolor", "virginica", "hybrid"),
  Sepal = c(5.006, 5.936, 6.588, 6.1),
  Petal = c(1.462, 4.26, 5.552, 4.4),
  Size = c("small", "medium", "large", NA),
  stringsAsFactors = FALSE
)

rec_edit <- function(session, param, value) {
  session$setInputs(`records-drilldown_table_block_action` = list(
    action = "config", param = param, value = value
  ))
}
grp_edit <- function(session, param, value) {
  session$setInputs(`groups-summarize_table_block_action` = list(
    action = "config", param = param, value = value
  ))
}

test_that("the records gear sets row names and columns, all of them", {
  b <- new_table_block(columns = c("Sepal", "Petal"))
  shiny::testServer(b$expr_server, args = list(data = shiny::reactive(sizes)), {
    st <- session$returned$state
    expect_identical(st$columns(), c("Sepal", "Petal"))
    rec_edit(session, "rowname", "Size")
    rec_edit(session, "columns", list("Petal"))
    expect_identical(st$rowname(), "Size")
    expect_identical(st$columns(), "Petal")
    rec_edit(session, "columns", list())
    expect_null(st$columns())
    rec_edit(session, "rowname", "")
    expect_null(st$rowname())
  })
})

test_that("a nested table takes the columns, the format and missing", {
  b <- new_table_block(by = "Size", rowname = "Species")
  shiny::testServer(b$expr_server, args = list(data = shiny::reactive(sizes)), {
    st <- session$returned$state
    rec_edit(session, "columns", list("Petal"))
    rec_edit(session, "missing", "drop")
    expect_identical(st$columns(), "Petal")
    expect_identical(st$missing(), "drop")
    # The output stays the input rows (a header click filters them); the
    # columns and missing shape only what the nested table shows.
    out <- eval_block_expr(session$returned$expr(), sizes)
    expect_identical(names(out), names(sizes))
    rec_edit(session, "missing", "level")
    expect_identical(st$missing(), "level")
  })
  shown <- table_nest(sizes, "Size", "Species", columns = "Petal")
  expect_identical(setdiff(names(shown), grep("^\\.", names(shown), value = TRUE)),
                   "Petal")
  shown <- table_nest(sizes, "Size", "Species", missing = "level")
  expect_true(MISSING_LEVEL %in% shown$.group1_level)
})

test_that("a structured frame prints its number cells in the block's format", {
  n <- table_nest(sizes, "Size", "Species")
  html <- function(...) {
    jsonlite::fromJSON(dt_payload_json(dt_build_payload(n, ...)))$html
  }
  expect_match(html(digits = "number"), ">5.01<")
  expect_match(html(digits = 0L), ">5<")
  expect_match(html(digits = 0L, unit = "cm"), "Sepal \\(cm\\)")
  # A Table 1 holds text cells: the format leaves them as they are.
  t1 <- summary_table(data.frame(ARM = rep(c("a", "b"), 5), AGE = 1:10),
                      vars = "AGE", by = "ARM")
  expect_identical(
    dt_payload_json(dt_build_payload(t1, digits = 0L)),
    sub('data-dt-format=\\\\"3\\\\"', 'data-dt-format=\\\\"0\\\\"',
        dt_payload_json(dt_build_payload(t1, digits = 3L)))
  )
  expect_false(dt_has_number_cells(t1))
  expect_true(dt_has_number_cells(sizes))
})

test_that("table_nest() keeps the columns asked for", {
  out <- table_nest(sizes, "Size", "Species", columns = "Petal")
  expect_identical(names(out),
                   c(".group1", ".group1_level", ".label", "Petal"))
})

test_that("the prepare script runs before every layout and survives a switch", {
  b <- new_table_block(
    rowname = "Species",
    script = paste0("cut <- 6  #| number(min = 5, max = 7)\n",
                    "dplyr::filter(data, Sepal > cut)")
  )
  shiny::testServer(b$expr_server, args = list(data = shiny::reactive(sizes)), {
    st <- session$returned$state
    out <- eval_block_expr(session$returned$expr(), sizes)
    expect_identical(sort(out$Species), c("hybrid", "virginica"))

    rec_edit(session, "sv_cut", 6.5)
    out <- eval_block_expr(session$returned$expr(), sizes)
    expect_identical(out$Species, "virginica")
    expect_equal(st$script_values()$cut, 6.5)

    # Groups: the same script, its click filter inside it.
    rec_edit(session, "layout", "groups")
    expect_identical(st$layout(), "groups")
    out <- eval_block_expr(session$returned$expr(), sizes)
    expect_identical(out$Species, "virginica")

    # Variables: Table 1 is built from what the script returned.
    grp_edit(session, "layout", "variables")
    expect_match(paste(deparse(session$returned$expr()), collapse = ""),
                 "Sepal > 6.5")

    # Cleared from the groups gear, gone in every layout.
    grp_edit(session, "layout", "records")
    rec_edit(session, "script", "")
    expect_identical(nrow(eval_block_expr(session$returned$expr(), sizes)), 4L)
  })
})

test_that("a click in the records layout filters the rows the script returned", {
  b <- new_table_block(
    rowname = "Species", drill = "Label",
    script = "dplyr::mutate(data, Label = toupper(Species))"
  )
  shiny::testServer(b$expr_server, args = list(data = shiny::reactive(sizes)), {
    session$setInputs(`records-drilldown_table_block_action` = list(
      action = "filter", column = "Label", values = list("SETOSA")
    ))
    out <- eval_block_expr(session$returned$expr(), sizes)
    expect_identical(out$Species, "setosa")
  })
})

test_that("the groups gear's Top caps the rows", {
  b <- new_table_block(layout = "groups", by = "Species")
  shiny::testServer(b$expr_server, args = list(data = shiny::reactive(sizes)), {
    grp_edit(session, "top_n", 2)
    expect_identical(session$returned$state$top_n(), 2L)
    grp_edit(session, "top_n", 0)
    expect_null(session$returned$state$top_n())
  })
})
