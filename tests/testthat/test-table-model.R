# The table model (R/table-model.R) against the reference's recorded
# behaviour (fixtures/table-golden): from a recorded state under the spec's
# names and the scenario's data, each layout's model gives what the block
# sent the browser on its first render.

test_that("table_model_records() gives the recorded records cells", {
  cases <- tm_cases("records")
  n <- 0L
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    want <- tm_payload(cs$messages, "blockr-viz-table-data")
    if (!identical(want$kind, "flat")) next
    tint <- table_model_tint(cs$state, cs$data)
    m <- table_model_records(cs$state, cs$data, tint = tint)
    expect_identical(m$n, want$n, label = paste(nm, "n"))
    cols <- c(list(m$stub), m$cells)
    expect_length(cols, length(want$cols))
    for (j in seq_along(cols)) {
      expect_identical(as.character(unname(cols[[j]]$disp)),
                       vapply(want$cols[[j]]$disp, function(x) as.character(x %||% NA), ""),
                       label = paste(nm, j, "disp"))
    }
    expect_identical(which(m$nodrill) - 1L, as.integer(unlist(want$nodrill)),
                     label = paste(nm, "nodrill"))
    n <- n + 1L
  }
  expect_gt(n, 10L)
})

test_that("table_model_records() hands a structured frame on", {
  st <- list(by = "grp", rowname = "id", missing = "level")
  d <- data.frame(grp = c("a", "b"), id = c("x", "y"), v = 1:2)
  m <- table_model_records(st, d)
  expect_identical(m$kind, "structured")
  expect_true(dt_is_structured(m$data))
})

test_that("table_model_groups() passes a preparation error on", {
  m <- table_model_groups(list(by = "nope", summaries = list()),
                          data.frame(a = 1))
  expect_false(is.null(m$prep$err))
  expect_null(m$cells)
})

test_that("table_model_variables() builds the recorded Table 1", {
  cases <- tm_cases("variables")
  n <- 0L
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    want <- tm_payload(cs$messages, "blockr-viz-table-data")
    if (!identical(want$kind, "html")) next
    tab <- table_model_variables(cs$state, cs$data)
    expect_true(dt_is_structured(tab), label = nm)
    # The rows the reference drew: every variable label is in its table.
    labels <- unique(as.character(tab$.label))
    for (lb in labels[nzchar(labels)]) {
      expect_true(grepl(htmltools::htmlEscape(lb), want$html, fixed = TRUE),
                  label = paste(nm, lb))
    }
    n <- n + 1L
  }
  expect_gt(n, 3L)
})

test_that("table_model_variables() returns the data without variables", {
  d <- data.frame(a = 1:2)
  expect_identical(table_model_variables(list(summaries = list()), d), d)
})

test_that("the row tint follows the color setting", {
  d <- data.frame(name = c("a", "b"), grp = c("x", "y"))
  expect_identical(table_model_tint(list(color = NULL), d),
                   list(col = NULL, hex = NULL))
  auto <- table_model_tint(list(color = "auto", rowname = "name"), d)
  expect_identical(auto$col, "name")
  col <- table_model_tint(list(color = "grp"), d)
  expect_identical(col$col, "grp")
  expect_length(col$hex, 2L)
})

test_that("color = auto tints rows from the board's scale map", {
  skip_if_not(has_blockr_theme(), "blockr.theme")
  d <- data.frame(name = c("CR", "PD", "SD"), n = 1:3)
  map <- list(name = list(color = list(CR = "#006400", PD = "#8b0000")))
  tint <- table_model_tint(list(color = "auto", rowname = "name"), d,
                           scale_map = map)
  expect_identical(tint$col, "name")
  expect_identical(tint$hex[1:2], c("#006400", "#8b0000"))
  # Without a binding for the stub column, auto paints nothing.
  expect_null(table_model_tint(list(color = "auto", rowname = "n"), d,
                               scale_map = map)$hex)
})

test_that("the groups sort direction defaults as the chart's: by value down, else up", {
  expect_identical(table_sort_dir(list()), "desc")
  expect_identical(table_sort_dir(list(sort_by = "value")), "desc")
  expect_identical(table_sort_dir(list(sort_by = "alpha")), "asc")
  expect_identical(table_sort_dir(list(sort_by = "data")), "asc")
  expect_identical(table_sort_dir(list(sort_by = "alpha", sort_dir = "desc")),
                   "desc")
  expect_null(blockr.core:::initial_block_state(
    new_table_block(layout = "groups")
  )$sort_dir)
})
