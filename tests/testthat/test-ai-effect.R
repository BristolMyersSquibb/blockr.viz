# What the assistant reads about a chart or table it configured
# (blockr.ai's config_effect()).

effect <- function(block, args, data = mtcars) {
  register_assistant_effect()
  blockr.ai::config_effect(block, args, data)
}

test_that("a chart's effect names its bindings and flags columns it lacks", {
  skip_if_not_installed("blockr.ai")
  skip_if_not("config_effect" %in% getNamespaceExports("blockr.ai"))
  ok <- effect(new_chart_block(), list(
    type = "bar", category = "cyl", value = "mpg", color = "gear",
    func = "mean", drill = "auto", value_lines = list(10, 20)
  ))
  expect_match(ok, "^bar chart configured: category=cyl, value=mpg, color=gear")
  expect_match(ok, "value_lines=10,20", fixed = TRUE)
  expect_match(ok, "agg=mean", fixed = TRUE)
  expect_no_match(ok, "INVALID|drill OFF")

  bad <- effect(new_chart_block(), list(type = "scatter", x = "nope",
                                        y = "mpg"))
  expect_match(bad, "drill OFF", fixed = TRUE)
  expect_match(bad, "INVALID column binding(s): x references 'nope'",
               fixed = TRUE)
  expect_match(bad, "Available columns: mpg", fixed = TRUE)

  box <- effect(new_chart_block(), list(
    type = "boxplot", category = "cyl", value = "mpg", summary = "mean_sd",
    whiskers = "min_max", connect_centers = TRUE
  ))
  expect_match(box, "summary=mean_sd, whiskers=min_max, connect_centers=on",
               fixed = TRUE)
  expect_match(effect(new_chart_block(), list()),
               "bar chart configured: (no column bindings)", fixed = TRUE)
})

test_that("a table's effect names its layout, columns and drill", {
  skip_if_not_installed("blockr.ai")
  skip_if_not("config_effect" %in% getNamespaceExports("blockr.ai"))
  ok <- effect(new_table_block(), list(
    layout = "groups", by = "cyl", drill = "auto",
    summaries = list(list(type = "simple", func = "count"))
  ))
  expect_identical(ok, paste("table configured: layout=groups, by=cyl,",
                             "drill=auto, 1 summaries"))
  bad <- effect(new_table_block(), list(rowname = "car", columns = c("mpg",
                                        "zz"), drill = "hp"))
  expect_match(bad, "layout=records, rowname=car, columns=mpg/zz, drill=hp",
               fixed = TRUE)
  expect_match(bad, "INVALID: rowname references 'car'; columns references 'zz'",
               fixed = TRUE)
})
