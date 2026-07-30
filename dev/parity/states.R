# Shared state matrix for the static-chart parity harness.
#
# One entry per chart-block state under comparison; dev/parity/app.R builds
# the interactive (canvas) side from it and dev/parity/static.R renders the
# same states through static_chart(). Keep the two sides reading from THIS
# file so they can never drift apart.

parity_datasets <- function() {
  list(
    adsl = safetyData::adam_adsl,
    orange = datasets::Orange
  )
}

# The same two datasets as dataset-block specs. A board feeding an outline
# needs these rather than new_static_block(): the outline's Output view
# EVALUATES the generated script, and a static block emits
# `x <- get("data", envir = <environment>)`, which does not parse.
parity_dataset_specs <- function() {
  list(
    adsl = list(dataset = "adam_adsl", package = "safetyData"),
    orange = list(dataset = "Orange", package = "datasets")
  )
}

parity_states <- list(
  bar_stack = list(
    data = "adsl",
    args = list(chart_type = "bar", group = "ARM", color = "SEX")
  ),
  bar_grouped = list(
    data = "adsl",
    args = list(chart_type = "bar", group = "ARM", color = "SEX",
                bar_mode = "grouped", count_on = "axis")
  ),
  bar_pct = list(
    data = "adsl",
    args = list(chart_type = "bar", group = "ARM", color = "AGEGR1",
                bar_mode = "percent")
  ),
  bar_vert = list(
    data = "adsl",
    args = list(chart_type = "bar", group = "AGEGR1", value = "AGE",
                func = "mean", orientation = "vertical")
  ),
  bar_facet = list(
    data = "adsl",
    args = list(chart_type = "bar", group = "AGEGR1", facet = "SEX")
  ),
  box = list(
    data = "adsl",
    args = list(chart_type = "boxplot", group = "ARM", value = "AGE",
                box_points = "outliers", count_on = "axis")
  ),
  scatter = list(
    data = "adsl",
    args = list(chart_type = "scatter", x = "AGE", y = "BMIBL",
                color = "SEX", smoother = "lm")
  ),
  line = list(
    data = "orange",
    args = list(chart_type = "line", x = "age", y = "circumference",
                series = "Tree")
  )
)
