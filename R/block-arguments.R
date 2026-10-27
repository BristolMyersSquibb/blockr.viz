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
# headers, tooltips). MUST mirror `AGG_WORDS` in inst/js/drilldown-agg.js (the
# same drift test guards the pair). Single R-side home -- dd_metric_plan()
# (R/table-block.R) reads it; do not re-inline per consumer.
AGG_WORDS <- c(
  count = "Count", count_distinct = "Distinct", mean = "Mean",
  median = "Median", sum = "Sum", min = "Min", max = "Max"
)

#' Build arguments metadata for the summary table block
#' @noRd
summary_table_arguments <- function() {
  new_arg_specs(
    vars = new_arg_spec(
      paste0(
        "Character, variables to summarise \u2014 each becomes a row-section. ",
        "Handles numeric, categorical, and logical columns; logicals are ",
        "rendered as a one-row TRUE count for pharma flag variables."
      ),
      example = list("AEDECOD"),
      type = arg_array(arg_string())
    ),
    sections = new_arg_spec(
      paste0(
        "Character, OUTER grouping columns that CONTAIN the `vars`, 0..N \u2014 ",
        "use ONLY for a true nesting hierarchy such as SOC containing PT; ",
        "leave empty for a flat list of variables."
      ),
      example = list("AEBODSYS"),
      type = arg_array(arg_string())
    ),
    by = new_arg_spec(
      "Character, column-split dimensions, 0..2.",
      example = list("TRT01A"),
      type = arg_array(arg_string())
    ),
    stats = new_arg_spec(
      paste0(
        "Statistics emitted for each NUMERIC variable, any combination of: ",
        "\"n\", \"n_pct\" (non-missing n and % of group rows), \"mean\", ",
        "\"sd\", \"mean_sd\", \"median\", \"median_q1_q3\", \"q1_q3\", ",
        "\"min_max\". ONE key = a single row per variable (e.g. [\"mean_sd\"] ",
        "or [\"n_pct\"]); SEVERAL keys = one row per statistic under a ",
        "variable header. Output rows follow the order above regardless of ",
        "input order. Default [\"mean_sd\"]."
      ),
      example = list("mean_sd"),
      type = arg_array(arg_enum(names(SUMMARY_STATS_CATALOG)))
    ),
    add_overall = new_arg_spec(
      "Logical, append an overall column across all `by` levels.",
      example = TRUE,
      type = arg_boolean()
    ),
    overall_label = new_arg_spec(
      "Label for the overall column, default \"Total\".",
      example = "Total",
      type = arg_string()
    ),
    indent_details = new_arg_spec(
      paste0(
        "Logical, default TRUE \u2014 indent detail rows under their variable ",
        "header; rarely changed."
      ),
      example = TRUE,
      type = arg_boolean()
    ),
    nest_hierarchies = new_arg_spec(
      paste0(
        "Logical, default FALSE \u2014 advanced row-side drill-down for adjacent ",
        "functionally-dependent categorical vars; leave FALSE unless asked."
      ),
      example = FALSE,
      type = arg_boolean()
    ),
    id_var = new_arg_spec(
      paste0(
        "OPTIONAL subject-identifier column name, e.g. \"USUBJID\": when set, ",
        "counts and percentages are over DISTINCT values of this column instead ",
        "of row counts \u2014 set it whenever the data is event-level/long, i.e. ",
        "multiple rows can belong to one subject and the user wants per-subject ",
        "counts; leave \"\" otherwise."
      ),
      example = "",
      type = arg_string()
    )
  )
}
