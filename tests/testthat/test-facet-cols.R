# `facet_cols`: how many facet panels sit in a row.
#
# Auto (unset) is what a faceted chart has always drawn -- the canvas fits
# panels to the card width, ggplot2 picks its own square-ish grid. The setting
# pins the row, and the point of it is that ONE value reaches both renderers,
# so the picture in a deck keeps the shape it had on screen.

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

test_that("gg_facet_ncol caps at the panel count and leaves auto alone", {
  # Auto: facet_wrap's own square-ish grid for the geometry, and NULL where
  # the answer goes to facet_wrap itself.
  expect_equal(gg_facet_ncol(9L, NULL), 3L)
  expect_null(gg_facet_ncol(9L, NULL, auto = NULL))

  expect_equal(gg_facet_ncol(9L, "2"), 2L)
  # Four columns over two panels would leave two empty tracks.
  expect_equal(gg_facet_ncol(2L, "4"), 2L)
})

test_that("static_chart pins facet_wrap's ncol, and omits it when unset", {
  skip_if_not_installed("ggplot2")
  d <- transform(datasets::iris, Grp = rep(c("A", "B"), 75))

  p <- static_chart(d, "bar", group = "Grp", facet = "Species",
                    facet_cols = 2)
  expect_equal(p$facet$params$ncol, 2L)

  auto <- static_chart(d, "bar", group = "Grp", facet = "Species")
  expect_null(auto$facet$params$ncol)

  # Capped at the panel count: iris has three species.
  wide <- static_chart(d, "bar", group = "Grp", facet = "Species",
                       facet_cols = 6)
  expect_equal(wide$facet$params$ncol, 3L)
})

test_that("the emitted code carries ncol only when the block pinned one", {
  d <- transform(datasets::iris, Grp = rep(c("A", "B"), 75))

  code <- chart_code(
    chart_expr("d", chart_type = "bar", group = "Grp", facet = "Species",
               facet_cols = 2, data = d)
  )
  expect_match(code, "facet_wrap\\(~Species, ncol = 2\\)")

  auto <- chart_code(
    chart_expr("d", chart_type = "bar", group = "Grp", facet = "Species",
               data = d)
  )
  expect_match(auto, "facet_wrap\\(~Species\\)")

  # Without a data snapshot there is no panel count to cap against, so the
  # pick travels as it was given rather than being capped to 1.
  blind <- chart_code(
    chart_expr("d", chart_type = "bar", group = "Grp", facet = "Species",
               facet_cols = 4)
  )
  expect_match(blind, "ncol = 4")
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
