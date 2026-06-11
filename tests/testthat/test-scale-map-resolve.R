# Vendored scale-map resolver (consumer side of the convention in
# blockr.design/open/cdex-attribute-map). The agreement fixture at the bottom
# is byte-identical across packages vendoring the resolver — a drifted copy
# fails its own tests.

test_that("dd_resolve_scales: fixed values, palette fallback, order", {
  map <- list(
    BOR = list(color = list(CR = "#006400", PR = "#FFD700", PD = "#8b0000"))
  )
  pal <- c("#aaaaaa", "#bbbbbb", "#cccccc")

  r <- dd_resolve_scales(map, "BOR", levels = c("PD", "CR", "NEW"),
                         palette = pal)

  expect_identical(r$color[["PD"]], "#8b0000")
  expect_true(r$color[["NEW"]] %in% pal)
  expect_identical(r$order, c("CR", "PD", "NEW"))

  expect_null(dd_resolve_scales(map, "NOPE", levels = "a"))
  expect_null(dd_resolve_scales(NULL, "BOR", levels = "CR"))
  expect_null(dd_resolve_scales(map, "BOR", levels = character()))
})

test_that("dd_resolve_scales: hash stable across level subsets", {
  map <- list(TRT = list())
  pal <- c("#0072B2", "#D55E00", "#F0E442")

  all_lv <- dd_resolve_scales(map, "TRT", levels = c("A", "B", "C"),
                              palette = pal)
  one_lv <- dd_resolve_scales(map, "TRT", levels = "B", palette = pal)

  expect_identical(all_lv$color[["B"]], one_lv$color[["B"]])
})

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

test_that("block server runs with a scale_map option and factor column", {
  df <- data.frame(
    BOR = factor(c("CR", "PD", "PR"), levels = c("CR", "PR", "PD")),
    TRT = c("A", "A", "B")
  )
  blk <- new_drilldown_chart_block(chart_type = "bar", group = "TRT",
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

# --- scale-map convention agreement fixture ---------------------------------
# Identical in every package that vendors the resolver. Do not edit without
# updating the convention (blockr.docs) and all copies.

test_that("AGREEMENT FIXTURE: hash assignment matches the convention", {
  pal <- c("#0072B2", "#D55E00", "#F0E442", "#009E73", "#56B4E9",
           "#E69F00", "#CC79A7")
  r <- dd_resolve_scales(
    list(X = list()), "X",
    levels = c("CR", "PR", "Drug A", "Placebo", "WEEK 4", "01-701-1015"),
    palette = pal
  )

  expect_identical(r$color, c(
    "CR" = "#D55E00",
    "PR" = "#CC79A7",
    "Drug A" = "#56B4E9",
    "Placebo" = "#CC79A7",
    "WEEK 4" = "#56B4E9",
    "01-701-1015" = "#009E73"
  ))
})
