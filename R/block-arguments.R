# One word per aggregation, for composed labels ("Mean: Baseline BMI", metric
# headers, tooltips). Must mirror `AGG_WORDS` in inst/js/chart/agg.js. The one
# R home: table_metric_plan() (R/table-aggregate.R) reads it.
AGG_WORDS <- c(
  count = "Count", count_distinct = "Distinct", mean = "Mean",
  median = "Median", sum = "Sum", min = "Min", max = "Max"
)
