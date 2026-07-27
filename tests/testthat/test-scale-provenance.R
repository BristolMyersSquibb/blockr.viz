# Scale resolution follows column provenance: a column copied by the picker
# block (SEX picked into "color") carries `blockr_source`, and the copy
# inherits its source column's scale-map binding. dd_resolve_scales is the
# one seam every renderer in this package routes through.

skip_if_not_installed("blockr.theme")
skip_if_not(
  "resolve_scales_col" %in% getNamespaceExports("blockr.theme"),
  "installed blockr.theme predates resolve_scales_col()"
)

sex_map <- list(
  SEX = list(color = c(F = "#0072B2", M = "#E69F00", U = "#999999"))
)

picked <- function() {
  d <- data.frame(
    VISIT = c("W1", "W1", "W2", "W2"),
    color = c("F", "M", "F", "U"),
    AVAL = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )
  attr(d$color, "blockr_source") <- "SEX"
  d
}

test_that("a picker copy inherits its source column's binding", {
  d <- picked()
  res <- dd_resolve_scales(sex_map, "color", d$color)
  expect_equal(res$color[["F"]], "#0072B2")
  expect_equal(res$color[["M"]], "#E69F00")
  expect_equal(res$color[["U"]], "#999999")
})

test_that("a directly bound name still wins over provenance", {
  d <- picked()
  m <- c(sex_map, list(color = list(color = c(F = "#111111"))))
  res <- dd_resolve_scales(m, "color", d$color)
  expect_equal(res$color[["F"]], "#111111")
})

test_that("no binding anywhere resolves to NULL (palette cycling)", {
  d <- picked()
  attr(d$color, "blockr_source") <- "NOT_BOUND"
  expect_null(dd_resolve_scales(sex_map, "color", d$color))
})

test_that("dd_scales_config emits the CHART's var name with source colors", {
  d <- picked()
  cfg <- dd_scales_config(
    sex_map, "pointrange",
    color = "color", group = "VISIT", data = d
  )
  expect_equal(cfg$var, "color")
  expect_equal(cfg$color$F, "#0072B2")
})
