skip_if_no_theme <- function() {
  testthat::skip_if_not_installed("blockr.theme")
}

test_that("vanilla is unchanged: no applied theme means the built-in colors", {
  # The regression this guards: theme_palette() answers with blockr defaults
  # whenever it is called, so gating on `requireNamespace()` alone would mean
  # that merely INSTALLING blockr.theme recolours every existing board.
  withr::local_options(list(blockr.theme.current = NULL))

  expect_identical(dd_palette(), DD_PALETTE_FALLBACK)
  expect_identical(dd_palette(1L), DD_PALETTE_FALLBACK[1L])
  expect_identical(
    viz_palette("sequential", 2L, DT_SEQUENTIAL_FALLBACK),
    DT_SEQUENTIAL_FALLBACK
  )
  expect_identical(
    viz_palette("diverging", 3L, DT_DIVERGING_FALLBACK),
    DT_DIVERGING_FALLBACK
  )
  # bands has no fallback: the table keeps its own unfilled-header behaviour
  expect_null(viz_palette("bands"))
  expect_null(ft_emphasis_from_theme())
})

test_that("an applied theme wins for every role", {
  skip_if_no_theme()

  th <- blockr.theme::blockr_theme(
    "t",
    palette_defs = list(
      Series = c("#111111", "#222222", "#333333"),
      Ramp = function(n) grDevices::colorRampPalette(c("#eeeeee", "#004400"))(n)
    ),
    palettes = list(
      categorical = "Series",
      sequential = "Ramp",
      bands = c(.stub = "#EEEEEE", "#A59F9F", "#33D6F1")
    )
  )
  withr::local_options(list(blockr.theme.current = th))

  expect_identical(dd_palette(), c("#111111", "#222222", "#333333"))
  expect_identical(dd_palette(1L), "#111111")
  expect_identical(viz_palette("sequential", 2L, DT_SEQUENTIAL_FALLBACK),
                   c("#EEEEEE", "#004400"))
  # unset role still falls back
  expect_identical(viz_palette("diverging", 3L, DT_DIVERGING_FALLBACK),
                   blockr.theme::theme_palette("diverging", 3L, th))

  # bands double as the emphasis triple: stub -> normal, pool[1] -> accent
  expect_identical(
    ft_emphasis_from_theme(),
    c(normal = "#EEEEEE", emph = "#33D6F1", strong = "#A59F9F")
  )
})

test_that("a theme with no bands does not half-apply the emphasis triple", {
  skip_if_no_theme()
  th <- blockr.theme::blockr_theme("t", palettes = list(bands = c("#A59F9F")))
  withr::local_options(list(blockr.theme.current = th))
  # pool but no .stub -> NULL, so FT_EMPHASIS_DEFAULT answers whole
  expect_null(ft_emphasis_from_theme())
})

test_that("the explicit option still beats the theme", {
  skip_if_no_theme()
  th <- blockr.theme::blockr_theme(
    "t",
    palettes = list(bands = c(.stub = "#EEEEEE", "#A59F9F", "#33D6F1"))
  )
  withr::local_options(list(
    blockr.theme.current = th,
    blockr.viz.ft_emphasis_colors = c(normal = "#1", emph = "#2", strong = "#3")
  ))
  bands <- ft_emphasis_bands(col_strong = TRUE, col_emph = FALSE)
  expect_identical(bands$bg, "#3")
})
