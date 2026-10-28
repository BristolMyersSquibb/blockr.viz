# Shading `source` (paint one column by another) + rotated column titles --
# the two table features behind the classic AE heatmap form: count displayed,
# worst grade painted, terms upright.

toy <- function() {
  data.frame(
    USUBJID = c("s1", "s2", "s3"),
    Group = c("A", "A", "B"),
    RASH = c(2, NA, 1), `RASH (sev)` = c(3, NA, 1),
    NAUSEA = c(1, 4, NA), `NAUSEA (sev)` = c(NA, 2, NA),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

tmpl_rule <- function() {
  dd_parse_shadings(list(list(
    mode = "sequential", cols = list(), source = "{col} (sev)"
  )))
}

test_that("dd_parse_shadings passes source through and normalizes domain", {
  sh <- dd_parse_shadings(list(list(
    mode = "sequential", cols = list(), source = "{col} (sev)",
    domain = list(1, 5)
  )))
  expect_identical(sh[[1]]$source, "{col} (sev)")
  expect_identical(sh[[1]]$domain, c(1, 5))
})

test_that("dd_shadings_json round-trips source (gear echo must not drop it)", {
  js <- dd_shadings_json(tmpl_rule())
  back <- dd_parse_shadings(js)
  expect_identical(back[[1]]$source, "{col} (sev)")
})

test_that("template source pairs each column with its companion and hides it", {
  d <- toy()
  vis <- dd_shading_visuals(tmpl_rule(), d, setdiff(names(d), "USUBJID"))
  expect_setequal(names(vis), c("RASH", "NAUSEA"))
  expect_identical(vis$RASH$src, "RASH (sev)")
  expect_identical(vis$NAUSEA$src, "NAUSEA (sev)")
  expect_setequal(attr(vis, "hidden"), c("RASH (sev)", "NAUSEA (sev)"))
})

test_that("plain-column source paints the rule's columns by that column", {
  d <- data.frame(g = c("a", "b"), n = c(10, 20), sev = c(3, 1))
  sh <- dd_parse_shadings(list(list(
    mode = "sequential", cols = list("n"), source = "sev"
  )))
  vis <- dd_shading_visuals(sh, d, c("n", "sev"))
  expect_identical(vis$n$src, "sev")
  expect_identical(attr(vis, "hidden"), "sev")
})

test_that("the built table displays counts, paints by source, hides companions", {
  d <- toy()
  b <- dt_flat_build(d, "USUBJID", NULL, tmpl_rule(), drill = "USUBJID",
                     digits = 0, toggles = list())
  # Companion columns never render as display columns.
  expect_identical(vapply(b$cells, `[[`, "", "name"),
                   c("Group", "RASH", "NAUSEA"))
  per_cell <- function(k) {
    unlist(lapply(b$cells, function(cc) cc[[k]] %||% rep(NA, b$n)))
  }
  bg <- per_cell("bg")
  fg <- per_cell("fg")
  disp <- per_cell("disp")
  # Exactly the three cells whose SOURCE is non-missing: s1 RASH (sev 3),
  # s2 NAUSEA (sev 2), s3 RASH (sev 1). s1 NAUSEA has a count but no grade
  # -> displayed unpainted rather than painted by its own value.
  expect_identical(sum(!is.na(bg)), 3L)
  # The darkest paint belongs to sev 3 -- the count-2 cell, not the count-4
  # one: the paint reads the SOURCE, not the displayed value.
  expect_identical(fg[which(!is.na(bg) & disp == "2")], "#ffffff")
})

test_that("rotate_titles sizes the value columns on content", {
  d <- toy()
  b <- dt_flat_build(d, "USUBJID", NULL, tmpl_rule(), drill = "USUBJID",
                     digits = 0, toggles = list(rotate_titles = TRUE))
  # Stub keeps the standard estimate; every value column collapses to the
  # 28px content floor (single-digit counts) -- header text no longer sets
  # the width, which is the point of rotating.
  expect_gt(b$widths[[1L]], 50)
  expect_true(all(b$widths[-1L] == 28))
})

test_that("rotate_titles off keeps the header-measured widths", {
  d <- toy()
  b <- dt_flat_build(d, "USUBJID", NULL, list(), drill = NULL,
                     digits = 0, toggles = list())
  expect_true(all(b$widths >= 60))   # the estimator's floor
})
