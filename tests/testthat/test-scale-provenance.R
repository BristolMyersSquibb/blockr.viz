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

test_that("the summarize table colors follow provenance too", {
  d <- picked()
  d$USUBJID <- c("s1", "s2", "s3", "s4")
  d$ASTDY <- c(1, 2, 3, 4)
  d$AENDY <- c(5, 6, 7, 8)

  # The colour-split bar path.
  p <- groups_prepare(
    d, by = "VISIT",
    summaries = list(list(type = "simple", func = "count", show = "bar",
                          color = "color")),
    scale_map = sex_map
  )
  expect_equal(unname(p$palette[c("F", "M", "U")]),
               c("#0072B2", "#E69F00", "#999999"))

  # The swimlane fills (a spans summary) and the legend beside them.
  p <- groups_prepare_summaries(
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

test_that("two hashed levels never share a colour while the palette lasts", {
  # A bare registration: every level's colour comes from the hash.
  m <- blockr.theme::new_scale_map(blockr.theme::scale_binding("TRT"))
  arm <- c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose")
  grp <- structure(arm, blockr_source = "TRT")
  res <- dd_resolve_scales(m, "Group", grp)
  expect_length(unique(unlist(res$color)), 3L)
  expect_true(all(unlist(res$color) %in% dd_palette()))
  # A pinned colour stays, and a hashed level moves off it.
  pinned <- blockr.theme::new_scale_map(blockr.theme::scale_binding(
    "TRT", color = c(Placebo = unname(res$color[["Xanomeline Low Dose"]]))
  ))
  res2 <- dd_resolve_scales(pinned, "Group", grp)
  expect_identical(res2$color[["Placebo"]], res$color[["Xanomeline Low Dose"]])
  expect_length(unique(unlist(res2$color)), 3L)
})

test_that("a channel mixing pins and a pool dodges within the pool", {
  pool <- c("#111111", "#222222", "#333333")
  spec <- c("01-701-1015" = "#d55e00", pool)
  expect_identical(dd_spec_fixed(spec), "01-701-1015")
  expect_identical(dd_spec_pool(spec), pool)
  # All named or all unnamed read as before.
  expect_identical(dd_spec_fixed(pool), character())
  expect_identical(dd_spec_pool(pool), pool)
  expect_null(dd_spec_pool(c(a = "#111111")))
  expect_null(dd_spec_pool(NULL))

  # Older blockr.theme refuses a mixed channel.
  skip_if_not(exists("scale_spec_pool", asNamespace("blockr.theme")))
  # Two hashed patients on one pool colour: the second moves to a free pool
  # colour, not to the board palette. The pinned patient stays.
  map <- list(USUBJID = list(color = spec))
  res <- list(color = c("01-701-1015" = "#d55e00", "01-701-1023" = "#222222",
                        "01-701-1028" = "#222222"))
  out <- dd_dodge_scales(res, map, "USUBJID", c("01-701-1015", "01-701-1023",
                                                "01-701-1028"))
  expect_identical(out$color[["01-701-1015"]], "#d55e00")
  expect_identical(out$color[["01-701-1023"]], "#222222")
  expect_identical(out$color[["01-701-1028"]], "#111111")
})
