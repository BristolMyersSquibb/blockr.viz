# Argument metadata helpers for blockr.viz blocks.
#
# Each helper returns a `new_arg_specs()` structure (one `new_arg_spec()`
# per constructor param, carrying a description, a worked `example`, and an
# optional `arg_*()` type descriptor). blockr.core stores these in the
# registry; the assistant / MCP server read them via the
# `block_meta_arguments()` / `arg_spec_type()` accessors to surface typed
# parameter documentation to the AI. Per-block construction guidance (the old
# `prompt` attribute) now lives in the `guidance` vector of the
# `register_blocks()` call in registry.R.

# One word per aggregation, for composed labels ("Mean: Baseline BMI", metric
# headers, tooltips). MUST mirror `AGG_WORDS` in inst/js/chart/agg.js. Single R-side home -- table_metric_plan()
# (R/table-aggregate.R) reads it; do not re-inline per consumer.
AGG_WORDS <- c(
  count = "Count", count_distinct = "Distinct", mean = "Mean",
  median = "Median", sum = "Sum", min = "Min", max = "Max"
)
