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

# Two episodes of one term: Diarrhoea on days 10-20 and 18-30 (overlapping),
# again 32-40 (two days after), Dizziness 10-15 the same day as the first.
ep_data <- function() {
  data.frame(
    P = c("a", "a", "a", "a"),
    R = c("Diarrhoea", "Diarrhoea", "Diarrhoea", "Dizziness"),
    L = "AE", C = c("Mild", "Moderate", "Mild", "Mild"),
    x = c(10, 18, 32, 10), xe = c(20, 30, 40, 15),
    D = c("Day 10 to 20", "Day 18 to 30", "Day 32 to 40", "Day 10 to 15")
  )
}

ep_lanes <- function(inner) {
  m <- rs_model(ep_data(), row_sections = NULL, summaries = list(list(
    type = "spans", x = "x", xend = "xe", color = "C", lane = "L",
    overlap = "worst", inner_overlap = inner, tooltip = "D",
    parent_lanes = TRUE
  )))
  cell <- m$cells$cols[[length(m$cells$cols)]]
  stats::setNames(cell$sl, m$cells$label)
}

test_that("overlapping episodes of one term stack on an inner row", {
  sl <- ep_lanes("stack")
  d <- sl$Diarrhoea
  # Two tracks: 10-20 with 32-40, and 18-30 under it, each record whole.
  expect_length(d, 2L)
  expect_identical(vapply(d, `[[`, "", "n"), c("AE", "AE"))
  expect_identical(lengths(lapply(d, `[[`, "s")), c(2L, 1L))
  # Each episode's tooltip names its own days.
  tips <- unlist(lapply(d, `[[`, "t"))
  expect_true(all(c("Day 10 to 20", "Day 32 to 40", "Day 18 to 30") %in%
                    sub(".*D: ", "", tips)))
  expect_length(sl$Dizziness, 1L)
  # The outer row keeps the worst stretch on one lane.
  expect_length(sl$a, 1L)
})

test_that("without stacking an inner row cuts overlaps as the outer row", {
  d <- ep_lanes("same")$Diarrhoea
  expect_length(d, 1L)
  # Worst level: 10-18 Mild, 18-30 Moderate, 32-40 Mild.
  expect_length(d[[1L]]$s, 3L)
})

test_that("stacked overlaps on the main lane draw their further tracks under it", {
  d <- ep_data()
  d$L <- NA_character_
  m <- rs_model(d, row_sections = NULL, summaries = list(list(
    type = "spans", x = "x", xend = "xe", color = "C", lane = "L",
    overlap = "worst", inner_overlap = "stack", tooltip = "D"
  )))
  cell <- m$cells$cols[[length(m$cells$cols)]]
  i <- which(m$cells$label == "Diarrhoea")
  # The main lane: 10-20 and 32-40; one full bar under it: 18-30.
  expect_length(cell$segs[[i]], 2L)
  expect_length(cell$sl[[i]], 1L)
  expect_true(isTRUE(cell$sl[[i]][[1L]]$f))
  expect_length(cell$sl[[i]][[1L]]$s, 1L)
  # The outer row keeps the worst stretch on its main lane, no extra bar.
  a <- which(m$cells$label == "a")
  expect_length(cell$sl[[a]] %||% list(), 0L)
})

test_that("repeats of one record do not stack", {
  d <- ep_data()
  # A second, identical Dizziness record (a repeat, say one per visit).
  d <- rbind(d, d[4L, ])
  sl <- {
    m <- rs_model(d, row_sections = NULL, summaries = list(list(
      type = "spans", x = "x", xend = "xe", color = "C", lane = "L",
      inner_overlap = "stack"
    )))
    stats::setNames(m$cells$cols[[length(m$cells$cols)]]$sl, m$cells$label)
  }
  expect_length(sl$Dizziness, 1L)
  expect_length(sl$Diarrhoea, 2L)
})

test_that("records that only touch share a track", {
  # A run of response reads: each ends on the day the next one starts.
  d <- data.frame(
    P = "a", R = "Response", L = "Response",
    C = c("SD", "PR", "CR"), x = c(1, 50, 120), xe = c(50, 120, 200)
  )
  m <- rs_model(d, row_sections = NULL, summaries = list(list(
    type = "spans", x = "x", xend = "xe", color = "C", lane = "L",
    inner_overlap = "stack"
  )))
  sl <- stats::setNames(m$cells$cols[[length(m$cells$cols)]]$sl,
                        m$cells$label)
  expect_length(sl$Response, 1L)
  expect_length(sl$Response[[1L]]$s, 3L)
})
