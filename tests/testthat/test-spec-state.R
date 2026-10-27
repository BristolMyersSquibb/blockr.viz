test_that("state holds one reactiveVal per argument", {
  st <- viz_state(list(type = "bar", color = NULL))
  expect_named(st, c("type", "color"))
  expect_identical(viz_values(st), list(type = "bar", color = NULL))
})

test_that("viz_apply() writes only what changed and bumps the revision", {
  st <- viz_state(list(type = "bar", color = NULL, bins = 30L))
  revs <- new_viz_revs()
  changed <- viz_apply(st, list(type = "bar", color = "cyl", other = 1), revs)
  expect_identical(changed, "color")
  expect_identical(viz_values(st)$color, "cyl")
  expect_identical(revs$now(), 1L)
  expect_identical(viz_apply(st, list(color = "cyl"), revs), character())
  expect_identical(revs$now(), 1L)
  viz_apply(st, list(color = NULL), revs)
  expect_null(viz_values(st)$color)
})

test_that("a name changed after a message's revision is stale", {
  revs <- new_viz_revs()
  revs$bump(c("x", "y"))
  revs$bump("y")
  expect_identical(revs$stale(c("x", "y", "z"), 1L), "y")
  expect_identical(revs$stale(c("x", "y"), 2L), character())
  expect_identical(revs$stale(c("x", "y"), 0L), c("x", "y"))
})
