# `facet_cols`: how many facet panels sit in a row.
#
# Auto (unset) is what a faceted chart has always drawn: the canvas fits
# panels to the card width. The setting pins the row.

test_that("facet_cols_state keeps whole numbers and heals everything else", {
  expect_null(facet_cols_state(NULL))
  expect_null(facet_cols_state(""))
  expect_null(facet_cols_state("auto"))
  expect_null(facet_cols_state("0"))
  expect_null(facet_cols_state(-2))
  # The DAG clipboard's corrupted NULL (see R/state-normalize.R).
  expect_null(facet_cols_state(list()))

  expect_equal(facet_cols_state("3"), "3")
  expect_equal(facet_cols_state(3), "3")
  expect_equal(facet_cols_state(3.7), "3")
  expect_equal(facet_cols_state(list("2")), "2")

  # Capped, so a typed 300 cannot render 300 empty tracks.
  expect_equal(facet_cols_state(300), as.character(FACET_COLS_MAX))

  # Idempotent, like every other normalizer here.
  expect_equal(facet_cols_state(facet_cols_state("4")), "4")

  expect_equal(facet_cols_n("4"), 4L)
  expect_null(facet_cols_n(""))
})

test_that("the chart block stores facet_cols and takes a deployment default", {
  blk <- new_chart_block(group = "Species", facet = "Grp", facet_cols = "3")
  expect_equal(environment(blk[["expr_server"]])$facet_cols, "3")

  junk <- new_chart_block(group = "Species", facet_cols = "auto")
  expect_null(environment(junk[["expr_server"]])$facet_cols)

  withr::with_options(list(blockr.viz.facet_cols = 2), {
    dep <- new_chart_block(group = "Species")
    expect_equal(environment(dep[["expr_server"]])$facet_cols, "2")
  })

  # Auto is NULL, which wedges a block that has not declared it empty-safe.
  expect_true("facet_cols" %in% attr(blk, "allow_empty_state"))
  expect_true("facet_cols" %in% blockr.core::external_ctrl_vars(blk))
})

test_that("facet_cols is a title slot, and offers itself while on auto", {
  d <- data.frame(x = 1)
  tpl <- "AEs by term[, in {@facet_cols} columns]"

  set <- block_title_parts(tpl, d, args = list(facet_cols = "3"))
  expect_equal(resolve_block_title(tpl, d, args = list(facet_cols = "3")),
               "AEs by term, in 3 columns")
  expect_true("facet_cols" %in% vapply(set, function(p) p$arg %||% "",
                                       character(1L)))

  # Auto: the clause leaves, and the word comes back as an offer chip.
  auto <- block_title_parts(tpl, d, args = list(facet_cols = NULL))
  expect_equal(resolve_block_title(tpl, d, args = list(facet_cols = NULL)),
               "AEs by term")
  expect_equal(attr(auto, "offers"), "facet_cols")
})
