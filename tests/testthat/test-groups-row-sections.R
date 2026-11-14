# A nested swimlane table: section titles over the inner rows
# (`row_sections`), an outer row's own records, the axis unit (`x_unit`).

rs_data <- function() {
  data.frame(
    P = c("a", "a", "a", "a", "a", "b", "b"),
    R = c(NA, NA, "Head", "Naus", "Para", NA, "Head"),
    L = c(NA, NA, "Adverse events", "Adverse events", "Con meds", NA,
          "Adverse events"),
    C = c("Run", "Run", "Mild", "Mild", "Mild", "Run", "Mild"),
    x = c(1, 20, 2, 5, 3, 1, 2), xe = c(20, 50, 10, 9, 6, 30, 4)
  )
}

rs_model <- function(data = rs_data(), ...) {
  st <- list(
    layout = "groups", by = c("P", "R"), group_rows = "summary",
    missing = "drop", sort_by = "data", row_sections = "L",
    summaries = list(list(type = "spans", x = "x", xend = "xe", color = "C",
                          x_unit = "Day"))
  )
  dots <- list(...)
  st[names(dots)] <- dots
  st <- table_normalize(viz_check(viz_block_spec("table"), st))
  table_model_groups(st, data)
}

test_that("a section's first inner row carries its title, one section none", {
  m <- rs_model()
  expect_identical(m$cells$label, c("a", "Head", "Naus", "Para", "b", "Head"))
  expect_identical(m$cells$title,
                   c(NA, "Adverse events", NA, "Con meds", NA, NA))
  rows <- table_groups_rows(m$cells, FALSE)
  expect_identical(rows$title[[2L]], "Adverse events")
  # Without the setting, or with every patient in one section, no titles.
  expect_null(rs_model(row_sections = NULL)$cells$title)
  d <- rs_data()
  d$L[d$L %in% "Con meds"] <- "Adverse events"
  expect_null(rs_model(d)$cells$title)
})

test_that("an outer row's own records draw on it, in their colours", {
  m <- rs_model()
  cell <- m$cells$cols[[length(m$cells$cols)]]
  # "Run" is carried only by records without an inner row; it keeps its
  # colour slot and its segments draw on the outer rows.
  expect_true("Run" %in% m$prep$plan[[1L]]$levels)
  expect_gte(length(cell$segs[[1L]]), 2L)
  expect_gte(length(cell$segs[[5L]]), 1L)
})

test_that("a numeric swimlane axis sends its domain and puts its unit first", {
  m <- rs_model()
  ax <- table_groups_axis(m$prep$plan[[1L]], m$prep)
  expect_match(ax$labels[[1L]], "^Day ")
  expect_identical(ax$unit, "Day")
  expect_true(is.numeric(ax$d0) && is.numeric(ax$d1))
  # Without a unit the labels are numbers only.
  m <- rs_model(summaries = list(list(type = "spans", x = "x", xend = "xe")))
  ax <- table_groups_axis(m$prep$plan[[1L]], m$prep)
  expect_false(grepl("Day", ax$labels[[1L]]))
  expect_null(ax$unit)
})
