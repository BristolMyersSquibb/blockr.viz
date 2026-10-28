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

# Shared aggregation vocabulary for the drilldown renderers (chart, table,
# tile). This vector MUST mirror `AGG_FNS` in inst/js/drilldown-agg.js (a drift
# test guards the pair). It is the single source for the `func` enum.
AGG_FNS <- c("count", "count_distinct", "mean", "median", "sum", "min", "max")

# One word per aggregation, for composed labels ("Mean: Baseline BMI", metric
# headers, tooltips). MUST mirror `AGG_WORDS` in inst/js/drilldown-agg.js (the
# same drift test guards the pair). Single R-side home -- dd_metric_plan()
# (R/table-block.R) reads it; do not re-inline per consumer.
AGG_WORDS <- c(
  count = "Count", count_distinct = "Distinct", mean = "Mean",
  median = "Median", sum = "Sum", min = "Min", max = "Max"
)

#' Shared `group` / `value` / `func` argument triple.
#'
#' The aggregation half of the chart / table / tile registry surface. Each
#' renderer spreads these into its own `*_arguments()` builder so the assistant
#' sees the identical typed parameters. `group_multiple = TRUE` (the table)
#' makes `group` an array of columns; the chart and tile group by one.
#' @noRd
agg_block_args <- function(group_multiple = FALSE) {
  list(
    group = new_arg_spec(
      paste0(
        "Grouping column", if (group_multiple) "(s)" else "", " for the ",
        "aggregation (the categorical dimension). ",
        if (group_multiple) {
          "Multiple columns nest the groups from outer to inner. "
        } else {
          ""
        },
        "\"\" / empty = no grouping."
      ),
      example = if (group_multiple) list("region") else "region",
      type = if (group_multiple) arg_array(arg_string()) else arg_string()
    ),
    value = new_arg_spec(
      paste0(
        "Column to aggregate. Must match `func`: \".count\" with func ",
        "\"count\" (row counts; the value is ignored otherwise), any column ",
        "with \"count_distinct\" (e.g. a subject id such as USUBJID to count ",
        "patients instead of records), a numeric column with the numeric ",
        "aggregations."
      ),
      example = ".count",
      type = arg_string()
    ),
    func = new_arg_spec(
      paste0(
        "Aggregation function for `value`. One of \"count\", ",
        "\"count_distinct\", \"mean\", \"median\", \"sum\", \"min\", \"max\". ",
        "Default \"count\" (row count; ignores `value`)."
      ),
      example = "count",
      type = arg_enum(AGG_FNS)
    )
  )
}

# NOTE: pivot_table_arguments() is not currently registered (the pivot table
# block was superseded by summarize + tidyr::pivot_wider). Kept for reference.
# Construction guidance that used to live in the legacy `prompt` attribute:
#
#   This block produces a pivot (crosstab) table from a long-format data
#   frame. Mental model: Excel pivot table -- rows (y-axis), cols (x-axis,
#   1-2 nested), measures (numeric values to aggregate), agg_fun (how to
#   combine them). Dimensions not in rows or cols are aggregated away.
#   Map common user requests:
#   - "mean revenue by region and quarter" ->
#     rows=[Region], cols=[Quarter], measures=[Revenue], agg_fun="mean"
#   - "count rows by region" -> rows=[Region], measures=[], agg_fun="n"
#   - "total sales and profit by region" ->
#     rows=[Region], measures=[Sales, Profit], agg_fun="sum"
#     (when no cols dimension is given with multiple measures, the measures
#     become the column headers)
#   - "nested rows by region then country" -> rows=[Region, Country]
#   Prefer this block for the "X by Y (x Z)" pattern. For a list of variables
#   summarised by one split, prefer summary_table_block. Pass this block's
#   output to gt_table_block for styled rendering.

#' Build arguments metadata for the pivot table block
#' @noRd
pivot_table_arguments <- function() {
  new_arg_specs(
    rows = new_arg_spec(
      paste0(
        "Character vector of column names used as row headers (vertical axis). ",
        "Multiple values nest rows from outer to inner. Empty vector = no row grouping."
      ),
      example = list("Region"),
      type = arg_array(arg_string())
    ),
    cols = new_arg_spec(
      paste0(
        "Character vector of column names pivoted into column headers (horizontal axis). ",
        "If empty and multiple `measures` are selected, the measures become the columns instead."
      ),
      example = list("Category"),
      type = arg_array(arg_string())
    ),
    measures = new_arg_spec(
      paste0(
        "Character vector of numeric column names to aggregate. ",
        "Multiple measures produce multiple output columns (nested under `cols` if present). ",
        "For pure count tables use `agg_fun = \"n\"` and leave `measures` empty."
      ),
      example = list("Revenue"),
      type = arg_array(arg_string())
    ),
    agg_fun = new_arg_spec(
      paste0(
        "Aggregation function applied to each measure. ",
        "One of \"sum\", \"mean\", \"median\", \"min\", \"max\", \"n\". Default \"sum\"."
      ),
      example = "sum",
      type = arg_enum(c("sum", "mean", "median", "min", "max", "n"))
    ),
    # Decimal places given as a string OR integer -> polymorphic scalar, so
    # the type is left unset and inferred from the worked example.
    digits = new_arg_spec(
      paste0(
        "Decimal places for numeric rounding as a string (e.g. \"2\") or integer. ",
        "Empty string \"\" (default) means no rounding."
      ),
      example = "2"
    )
  )
}

#' Build arguments metadata for the gt table block
#' @noRd
gt_table_arguments <- function() {
  new_arg_specs(
    title = new_arg_spec(
      "Table title rendered above the table. Empty string for no title.",
      example = "Baseline Characteristics",
      type = arg_string()
    ),
    subtitle = new_arg_spec(
      "Subtitle rendered under the title. Empty string for no subtitle.",
      example = "ITT population",
      type = arg_string()
    ),
    full_width = new_arg_spec(
      "Logical. TRUE (default) makes the table span the container width.",
      example = TRUE,
      type = arg_boolean()
    ),
    borders = new_arg_spec(
      paste0(
        "Logical. TRUE (default) draws 2px top/bottom/heading borders in the ",
        "pharma SAP style."
      ),
      example = TRUE,
      type = arg_boolean()
    ),
    na_rep = new_arg_spec(
      paste0(
        "String used to render missing (NA) cells. Default is an em dash."
      ),
      example = "\u2014",
      type = arg_string()
    )
  )
}

#' Construction guidance for the gt table block
#' @noRd
gt_table_guidance <- function() {
  paste(
    "Render the output of summary_table_block as a styled gt table.",
    "This block is a pure renderer \u2014 it does not",
    "reshape or aggregate. Place it downstream of a table-shape block.",
    "\n\nUse `title` / `subtitle` for table captions. Use `full_width`,",
    "`borders`, and `na_rep` for layout polish."
  )
}
