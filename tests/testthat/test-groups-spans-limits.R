# x_limits on a spans column: the axis from and to, either end the data's
# when NA; what lies outside goes, a bar past the right end is cut there with
# the caret.

limits_fixture <- function() {
  na <- NA_real_
  data.frame(
    id = c("a", "a", "a", "a", "b", "b", "c"),
    s = c(-20, 30, 142, 300, 5, 120, 200),
    e = c(12, 88, 260, 330, 40, na, 260),
    g = c("G1", "G2", "G2", "G3", "G1", "G2", "G1"),
    ev = c(na, na, na, 355, 170, na, na),
    open = c(NA, NA, NA, NA, NA, TRUE, NA),
    stringsAsFactors = FALSE
  )
}

limits_spans <- function(...) {
  list(type = "spans", name = "Time", x = "s", xend = "e", color = "g",
       event_x = "ev", open = "open", x_days = TRUE, ...)
}

limits_col <- function(m) m$cells$cols[[1L]]

test_that("without limits the axis is the data's, and nothing is cut", {
  m <- sum_model(limits_fixture(), by = "id",
                 summaries = list(limits_spans()))
  c1 <- limits_col(m)
  expect_equal(c1$d0, -20)
  expect_gte(c1$d1, 355)
  i <- match("a", m$cells$label)
  expect_length(c1$segs[[i]], 4L)
  expect_null(m$prep$plan[[1L]]$pin)
})

test_that("x_limits sets the axis and drops what lies outside", {
  m <- sum_model(limits_fixture(), by = "id",
                 summaries = list(limits_spans(x_limits = c(1, 180))))
  c1 <- limits_col(m)
  expect_equal(c(c1$d0, c1$d1), c(1, 180))
  ia <- match("a", m$cells$label)
  ib <- match("b", m$cells$label)
  ic <- match("c", m$cells$label)
  # Patient a: the bar from day -20 starts at the left edge, the one from
  # 142 to 260 stops at the right edge, the one from 300 goes.
  a <- c1$segs[[ia]]
  expect_length(a, 3L)
  expect_equal(a[[1L]][[1L]], 0)
  expect_equal(a[[3L]][[1L]] + a[[3L]][[2L]], 100)
  # ... with the caret at the right edge, and the event at 355 gone.
  expect_equal(c1$op[[ia]], 100)
  expect_length(c1$ev[[ia]] %||% list(), 0L)
  # Patient b: the ongoing bar runs to the edge, its event at 170 stays.
  expect_length(c1$segs[[ib]], 2L)
  expect_equal(c1$op[[ib]], 100)
  expect_length(c1$ev[[ib]], 1L)
  # Patient c: everything after the range, an empty row without a caret.
  expect_length(c1$segs[[ic]], 0L)
  expect_true(is.na(c1$op[[ic]]))
})

test_that("tooltips keep the records' own days", {
  m <- sum_model(limits_fixture(), by = "id",
                 summaries = list(limits_spans(x_limits = c(1, 180))))
  c1 <- limits_col(m)
  tips <- c1$tips[[match("a", m$cells$label)]]
  expect_true(any(grepl("142–260", tips)))
  expect_true(any(grepl("-20–12", tips)))
})

test_that("one end set leaves the other to the data", {
  m <- sum_model(limits_fixture(), by = "id",
                 summaries = list(limits_spans(x_limits = c(NA, 180))))
  c1 <- limits_col(m)
  expect_equal(c(c1$d0, c1$d1), c(-20, 180))
  m <- sum_model(limits_fixture(), by = "id",
                 summaries = list(limits_spans(x_limits = c(100, NA))))
  c1 <- limits_col(m)
  expect_equal(c1$d0, 100)
  expect_equal(c1$d1, 355)
  # The bar from 30 to 88 lies before the range.
  expect_length(c1$segs[[match("a", m$cells$label)]], 2L)
})

test_that("the start column's blockr_domain sets the ends x_limits leaves", {
  d <- limits_fixture()
  attr(d$s, "blockr_domain") <- c(NA, 200)
  m <- sum_model(d, by = "id", summaries = list(limits_spans()))
  expect_equal(c(limits_col(m)$d0, limits_col(m)$d1), c(-20, 200))
  m <- sum_model(d, by = "id",
                 summaries = list(limits_spans(x_limits = c(1, NA))))
  expect_equal(c(limits_col(m)$d0, limits_col(m)$d1), c(1, 200))
  m <- sum_model(d, by = "id",
                 summaries = list(limits_spans(x_limits = c(1, 90))))
  expect_equal(c(limits_col(m)$d0, limits_col(m)$d1), c(1, 90))
})

test_that("the axis has the set ends as ticks, and says which are set", {
  m <- sum_model(limits_fixture(), by = "id",
                 summaries = list(limits_spans(x_limits = c(1, 180))))
  ax <- table_groups_to_model(m, axis = TRUE)$cols[[2L]]$axis
  expect_identical(ax$labels[[1L]], "D1")
  expect_identical(ax$labels[[length(ax$labels)]], "D180")
  expect_equal(range(ax$at), c(0, 100))
  expect_equal(as.integer(ax$pin), c(1L, 1L))
  m <- sum_model(limits_fixture(), by = "id",
                 summaries = list(limits_spans()))
  expect_null(table_groups_to_model(m, axis = TRUE)$cols[[2L]]$axis$pin)
})

test_that("cells_axis_pin adds set ends and drops ticks next to them", {
  expect_equal(cells_axis_pin(c(1, 50, 100, 150), 1, 180, c(TRUE, TRUE)),
               c(1, 50, 100, 150, 180))
  expect_equal(cells_axis_pin(c(1, 50, 100, 170), 1, 180, c(TRUE, TRUE)),
               c(1, 50, 100, 180))
  expect_equal(cells_axis_pin(c(0, 100, 200), 0, 210, c(FALSE, TRUE)),
               c(0, 100, 210))
  expect_equal(cells_axis_pin(c(0, 100), 0, 100, NULL), c(0, 100))
})

test_that("dates set the axis of a date column", {
  d <- data.frame(
    id = c("a", "a"),
    s = as.Date(c("2024-01-01", "2024-03-01")),
    e = as.Date(c("2024-02-01", "2024-06-01"))
  )
  m <- sum_model(d, by = "id", summaries = list(list(
    type = "spans", x = "s", xend = "e",
    x_limits = c("2024-01-15", "2024-04-01")
  )))
  c1 <- limits_col(m)
  expect_equal(c(c1$d0, c1$d1),
               as.numeric(as.Date(c("2024-01-15", "2024-04-01"))))
  expect_equal(c1$op[[1L]], 100)
})

test_that("x_limits is checked as limits", {
  arg <- viz_block_spec("table")$args$summaries
  ok <- viz_check(viz_block_spec("table"), list(
    layout = "groups",
    summaries = list(list(type = "spans", x = "s", xend = "e",
                          x_limits = list(NULL, 180)))
  ))
  expect_equal(ok$summaries[[1L]]$x_limits, c(NA, 180))
  expect_error(
    viz_check(viz_block_spec("table"), list(
      layout = "groups",
      summaries = list(list(type = "spans", x = "s", xend = "e",
                            x_limits = c(200, 100)))
    )),
    "from below to"
  )
})
