# A full-width group heading carries the number of rows listed under it
# (`rows.count` in the TableModel). Summaries on the outer rows
# (`group_rows = "summary"`) and the compact matrix have none.

hc_data <- function() {
  data.frame(
    SOC = c(rep("Cardiac", 4), rep("Skin", 6), "Eye"),
    PT = c("Palpitations", "Palpitations", "Tachycardia", "Tachycardia",
           "Rash", "Rash", "Pruritus", "Erythema", "Erythema", "Erythema",
           "Dry eye"),
    USUBJID = sprintf("S%02d", c(1, 2, 1, 3, 1, 2, 4, 2, 3, 5, 6)),
    stringsAsFactors = FALSE
  )
}

hc_model <- function(data, ...) {
  st <- list(layout = "groups", by = c("SOC", "PT"),
             summaries = list(list(type = "simple", func = "count",
                                   show = "bar", name = "Count")), ...)
  table_model_groups(st, data)
}

# The count per heading, named by the heading's outer key.
hc_counts <- function(m) {
  v <- table_groups_to_model(m)
  par <- m$cells$parent_row %in% TRUE
  stats::setNames(as.integer(v$rows$count[par]), m$cells$parent_key[par])
}

test_that("each heading counts the inner rows under it", {
  m <- hc_model(hc_data())
  expect_identical(hc_counts(m)[c("Cardiac", "Skin", "Eye")],
                   c(Cardiac = 2L, Skin = 3L, Eye = 1L))
  # Inner rows carry no count.
  v <- table_groups_to_model(m)
  expect_true(all(is.na(v$rows$count[!m$cells$parent_row])))
})

test_that("the count follows the rows that pass a filter", {
  d <- hc_data()
  d <- d[d$PT != "Pruritus", ]
  expect_identical(hc_counts(hc_model(d))[["Skin"]], 2L)
  # A top-N cut drops whole groups; the ones kept count all their rows.
  cnt <- hc_counts(hc_model(hc_data(), top_n = 2L))
  expect_length(cnt, 2L)
  expect_identical(cnt[["Skin"]], 3L)
})

test_that("summaries on the outer rows show no count", {
  m <- hc_model(hc_data(), group_rows = "summary")
  expect_null(table_groups_to_model(m)$rows$count)
})

test_that("a table without nesting or a compact matrix has no count", {
  st <- list(layout = "groups", by = "PT",
             summaries = list(list(type = "simple", func = "count")))
  expect_null(table_groups_to_model(table_model_groups(st, hc_data()))$rows$count)
  m <- table_model_groups(
    list(layout = "groups", by = c("SOC", "PT"), look = "compact",
         summaries = list(list(type = "simple", func = "count",
                               show = "tile", facet = "USUBJID"))),
    hc_data()
  )
  expect_null(m$cells$head_count)
})

test_that("the count reaches the browser as null on the other rows", {
  v <- table_groups_to_model(hc_model(hc_data()))
  js <- jsonlite::fromJSON(jsonlite::toJSON(v$rows["count"], na = "null"),
                           simplifyVector = FALSE)
  expect_true(any(vapply(js$count, is.null, logical(1L))))
})
