# Scale resolution follows column provenance: a column copied by the picker
# block (SEX picked into "color") carries `blockr_source`, and the copy
# inherits its source column's scale-map binding. dd_resolve_scales is the
# one seam every renderer in this package routes through.

skip_if_not_installed("blockr.theme")

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

test_that("the rank / summarize-table colors follow provenance too", {
  d <- picked()
  d$USUBJID <- c("s1", "s2", "s3", "s4")
  d$ASTDY <- c(1, 2, 3, 4)
  d$AENDY <- c(5, 6, 7, 8)

  # The colour-split bar path.
  p <- rank_prepare(d, group = "VISIT", func = "count", color = "color",
                    scale_map = sex_map)
  expect_equal(unname(p$palette[c("F", "M", "U")]),
               c("#0072B2", "#E69F00", "#999999"))

  # The swimlane fills (a spans summary) and the legend beside them.
  p <- lane_prepare_summaries(
    d, by = "VISIT",
    summaries = list(list(type = "spans", name = "Episodes", x = "ASTDY",
                          xend = "AENDY", color = "color")),
    scale_map = sex_map
  )
  fills <- p$plan[[1L]]$fills
  expect_equal(fills[seq_along(p$plan[[1L]]$levels)],
               unname(sex_map$SEX$color[p$plan[[1L]]$levels]))
  expect_equal(unname(p$palette[c("F", "M", "U")]),
               c("#0072B2", "#E69F00", "#999999"))
})
