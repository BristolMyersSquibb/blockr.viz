gen_env <- function() {
  path <- test_path("..", "..", "dev", "gen-spec.R")
  skip_if_not(file.exists(path), "dev/ is not in the built package")
  env <- new.env(parent = asNamespace("blockr.viz"))
  sys.source(path, envir = env)
  env
}

gen_small_spec <- function() {
  viz_spec(
    "chart",
    type = viz_arg("enum", default = "bar", values = c("bar", "line"),
                   gear = gear("Chart type", "tiles", "Chart type",
                               labels = c("Bar", "Line")),
                   slot = TRUE),
    color = viz_arg("column", null = "no color",
                    applies = except("pie")),
    facet_cols = viz_arg("integer", default = opt("blockr.viz.facet_cols"),
                         bounds = bounds(1), null = "fit the width",
                         gear = gear("Facets", "select", "Panel columns",
                                     when = "color")),
    y_lines = viz_arg("numbers", null = "none"),
    highlight = viz_arg(type_record(column = "column",
                                    values = type_values(of = "column")),
                        null = "none"),
    script_values = viz_arg("map", default = list(), ai = FALSE)
  )
}

test_that("the JS spec carries what the gear needs", {
  g <- gen_env()
  d <- g$gen_spec_data(gen_small_spec())
  expect_identical(d$type$default, "bar")
  expect_identical(d$type$gear$labels, I(c("Bar", "Line")))
  expect_identical(d$color$applies, list(except = I("pie")))
  expect_identical(d$facet_cols$option, "blockr.viz.facet_cols")
  expect_null(d$facet_cols$default)
  expect_identical(d$facet_cols$bounds, list(min = 1, max = NULL,
                                             minOpen = FALSE))
  expect_identical(d$facet_cols$gear$when, "color")
  expect_identical(d$highlight$type$fields$values$type,
                   list(name = "values", of = "column"))
  expect_true(d$highlight$type$fields$values$nullable)
  expect_identical(d$script_values$default, setNames(list(), character()))
})

test_that("the JS spec runs and sets Blockr.viz.spec", {
  skip_if(Sys.which("node") == "", "node is not installed")
  g <- gen_env()
  f <- withr::local_tempfile(fileext = ".js")
  writeLines(c(g$gen_spec_js(gen_small_spec()),
               "console.log(JSON.stringify(globalThis.Blockr.viz.spec.chart.type))"),
             f)
  out <- system2("node", f, stdout = TRUE)
  expect_identical(jsonlite::fromJSON(out)$default, "bar")
})

test_that("the types follow the wire", {
  g <- gen_env()
  dts <- g$gen_spec_dts(gen_small_spec())
  expect_true("interface ChartConfig {" %in% dts)
  expect_true("  type: 'bar' | 'line';" %in% dts)
  expect_true("  color: string | null;" %in% dts)
  expect_true(
    "  y_lines: { value: number; label: string | null }[] | null;" %in% dts
  )
  expect_true(any(grepl("highlight: \\{ column: string \\| null; values:",
                        dts)))
  v <- viz_spec("table", summaries = viz_arg(
    type_records_by("type", list(
      simple = viz_spec("simple", col = viz_arg("column", null = "rows")),
      dist = viz_spec("dist", col = viz_arg("column", required = TRUE))
    )),
    default = list()
  ))
  expect_true(paste0(
    "  summaries: ({ type: 'simple'; col: string | null } | ",
    "{ type: 'dist'; col: string })[];"
  ) %in% g$gen_spec_dts(v))
  expect_true("  values: Partial<ChartConfig>;" %in% dts)
})

test_that("the checked-in JS spec matches the R specs", {
  g <- gen_env()
  specs <- g$gen_package_specs(asNamespace("blockr.viz"))
  dir <- withr::local_tempdir()
  for (path in g$gen_spec_files(specs, dir)) {
    shipped <- test_path("..", "..", "inst", "js", "spec", basename(path))
    expect_true(file.exists(shipped), label = basename(path))
    expect_identical(readLines(path), readLines(shipped),
                     label = basename(path))
  }
})
