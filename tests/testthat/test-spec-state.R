test_that("state holds one reactiveVal per argument", {
  st <- viz_state(list(type = "bar", color = NULL))
  expect_named(st, c("type", "color"))
  expect_identical(shiny::isolate(viz_values(st)), list(type = "bar", color = NULL))
})

test_that("viz_apply() writes only what changed, as a known value", {
  st <- viz_state(list(type = "bar", color = NULL, bins = 30L))
  revs <- new_viz_revs(shiny::isolate(viz_values(st)))
  changed <- viz_apply(st, list(type = "bar", color = "cyl", other = 1), revs)
  expect_identical(changed, "color")
  expect_identical(shiny::isolate(viz_values(st))$color, "cyl")
  expect_identical(viz_apply(st, list(color = "cyl"), revs), character())
  # the client's own write is known: tracking it bumps nothing
  expect_identical(revs$track(shiny::isolate(viz_values(st))), character())
  expect_identical(revs$now(), 0L)
  viz_apply(st, list(color = NULL), revs)
  expect_null(shiny::isolate(viz_values(st))$color)
  expect_identical(revs$track(shiny::isolate(viz_values(st))), character())
})

test_that("a change from elsewhere bumps the revision and goes stale", {
  revs <- new_viz_revs(list(x = 1, y = NULL))
  expect_identical(revs$track(list(x = 2, y = NULL)), "x")
  expect_identical(revs$now(), 1L)
  expect_identical(revs$track(list(x = 2, y = "a")), "y")
  expect_identical(revs$stale(c("x", "y", "z"), 1L), "y")
  expect_identical(revs$stale(c("x", "y"), 2L), character())
  expect_identical(revs$stale(c("x", "y"), 0L), c("x", "y"))
  expect_identical(revs$track(list(x = 2, y = "a")), character())
})
