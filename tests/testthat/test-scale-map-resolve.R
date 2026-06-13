# Drilldown glue for the board scale map. The resolver and its pinned hash
# live in blockr.theme (Suggests); these tests cover bi's own role table,
# level extraction and JS payload assembly.

test_that("dd_colored_var follows the per-chart-type table", {
  expect_identical(dd_colored_var("bar", "AESEV", "AEDECOD"), "AESEV")
  expect_identical(dd_colored_var("gantt", "BOR", "USUBJID"), "BOR")
  expect_identical(dd_colored_var("scatter", "TRT", NULL), "TRT")
  expect_identical(dd_colored_var("line", "TRT", NULL), "TRT")
  # pie/treemap color their group slices
  expect_identical(dd_colored_var("pie", NULL, "BOR"), "BOR")
  expect_identical(dd_colored_var("treemap", NULL, "BOR"), "BOR")
  # boxplot has no colored role; bar without a color role never auto-colors
  # its category axis
  expect_null(dd_colored_var("boxplot", "TRT", "AVISIT"))
  expect_null(dd_colored_var("bar", NULL, "BOR"))
  expect_null(dd_colored_var("bar", "", "BOR"))
})

test_that("dd_levels prefers factor levels", {
  f <- factor(c("b", "a"), levels = c("b", "a"))
  expect_identical(dd_levels(f), c("b", "a"))
  expect_identical(dd_levels(c("x", "y", "x", NA)), c("x", "y"))
})

test_that("dd_scales_config builds the JS payload", {
  skip_if_not_installed("blockr.theme")

  map <- list(
    BOR = list(color = list(CR = "#006400", PD = "#8b0000"))
  )
  d <- data.frame(BOR = c("CR", "PD"), N = 1:2)

  cfg <- dd_scales_config(map, "bar", color = "BOR", group = "N", data = d)

  expect_identical(cfg$var, "BOR")
  # named vectors are converted to lists so names survive Shiny's JSON
  # encoding
  expect_identical(cfg$color, list(CR = "#006400", PD = "#8b0000"))
  expect_identical(cfg$order, list("CR", "PD"))

  # no colored role / unknown variable / no map -> NULL
  expect_null(dd_scales_config(map, "bar", color = NULL, group = "BOR",
                               data = d))
  expect_null(dd_scales_config(map, "bar", color = "XX", group = NULL,
                               data = d))
  expect_null(dd_scales_config(NULL, "bar", color = "BOR", group = NULL,
                               data = d))
})

test_that("dd_scales_config honors a board palette carried by the map", {
  skip_if_not_installed("blockr.theme")

  # Plain-list map with the reserved .palette entry, as it arrives from a
  # deserialized board option.
  map <- list(
    TRT = list(),
    .palette = list("#101010", "#202020")
  )
  d <- data.frame(TRT = c("A", "B"), N = 1:2)

  cfg <- dd_scales_config(map, "bar", color = "TRT", group = NULL, data = d)
  expect_true(all(unlist(cfg$color) %in% c("#101010", "#202020")))
})

test_that("block server runs with a scale_map option and factor column", {
  df <- data.frame(
    BOR = factor(c("CR", "PD", "PR"), levels = c("CR", "PR", "PD")),
    TRT = c("A", "A", "B")
  )
  blk <- new_chart_block(chart_type = "bar", group = "TRT",
                                   color = "BOR")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$userData$board_options <- list(
        scale_map = shiny::reactiveVal(
          list(BOR = list(color = list(CR = "#006400", PD = "#8b0000")))
        )
      )
      session$flushReact()
      expect_s3_class(session$returned$result(), "data.frame")
    },
    args = list(x = blk, data = list(data = function() df))
  )
})
