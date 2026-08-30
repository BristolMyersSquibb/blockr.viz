# Shading `source` (paint one column by another) + rotated column titles --
# the two table features behind the CDEx AE heatmap form: count displayed,
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
  html <- as.character(dt_flat_assemble_tag(b))
  heads <- gsub("<[^>]*>", "", regmatches(
    html, gregexpr("<span class=\"blockr-col-name\">[^<]*</span>", html)
  )[[1]])
  # Companion columns never render as display columns.
  expect_setequal(heads, c("USUBJID", "Group", "RASH", "NAUSEA"))
  painted <- grep("background",
                  regmatches(html, gregexpr("<td[^>]*>[^<]*</td>", html))[[1]],
                  value = TRUE)
  # Exactly the three cells whose SOURCE is non-missing: s1 RASH (sev 3),
  # s2 NAUSEA (sev 2), s3 RASH (sev 1). s1 NAUSEA has a count but no grade
  # -> displayed unpainted rather than painted by its own value.
  expect_length(painted, 3L)
  # The darkest paint belongs to sev 3 -- the count-2 cell, not the count-4
  # one: the paint reads the SOURCE, not the displayed value.
  sev3 <- grep(">2<", painted, value = TRUE)
  expect_length(sev3, 1L)
  expect_match(sev3, "color:#ffffff", fixed = TRUE)
})

test_that("rotate_titles stamps the attribute and sizes columns on content", {
  d <- toy()
  b <- dt_flat_build(d, "USUBJID", NULL, tmpl_rule(), drill = "USUBJID",
                     digits = 0, toggles = list(rotate_titles = TRUE))
  html <- as.character(dt_flat_assemble_tag(b))
  expect_match(html, 'data-dt-rotate-titles="on"', fixed = TRUE)
  w <- as.integer(gsub("\\D", "", regmatches(
    html, gregexpr("width: [0-9]+px", html)
  )[[1]]))
  # Stub keeps the standard estimate; every value column collapses to the
  # 28px content floor (single-digit counts) -- header text no longer sets
  # the width, which is the point of rotating.
  expect_gt(w[1], 50)
  expect_true(all(w[-1] == 28L))
})

test_that("rotate_titles off keeps the header-measured widths", {
  d <- toy()
  b <- dt_flat_build(d, "USUBJID", NULL, list(), drill = NULL,
                     digits = 0, toggles = list())
  html <- as.character(dt_flat_assemble_tag(b))
  expect_match(html, 'data-dt-rotate-titles="off"', fixed = TRUE)
  w <- as.integer(gsub("\\D", "", regmatches(
    html, gregexpr("width: [0-9]+px", html)
  )[[1]]))
  expect_true(all(w >= 60L))   # the estimator's floor
})
