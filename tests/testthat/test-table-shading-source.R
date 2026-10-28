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
