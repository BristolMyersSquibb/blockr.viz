# Argument metadata helpers for blockr.bi blocks.
#
# Each helper returns a named character vector (one description per
# constructor param) with `examples` and `prompt` attributes attached via
# `structure()`. blockr.core stores these in the registry; the MCP server
# reads them via `registry_metadata(uid, "arguments")` to surface typed
# parameter documentation to the AI.

#' Build arguments metadata for the pivot table block
#' @noRd
pivot_table_arguments <- function() {
  structure(
    c(
      rows = paste0(
        "Character vector of column names used as row headers (vertical axis). ",
        "Multiple values nest rows from outer to inner. Empty vector = no row grouping."
      ),
      cols = paste0(
        "Character vector of column names pivoted into column headers (horizontal axis). ",
        "If empty and multiple `measures` are selected, the measures become the columns instead."
      ),
      measures = paste0(
        "Character vector of numeric column names to aggregate. ",
        "Multiple measures produce multiple output columns (nested under `cols` if present). ",
        "For pure count tables use `agg_fun = \"n\"` and leave `measures` empty."
      ),
      agg_fun = paste0(
        "Aggregation function applied to each measure. ",
        "One of \"sum\", \"mean\", \"median\", \"min\", \"max\", \"n\". Default \"sum\"."
      ),
      digits = paste0(
        "Decimal places for numeric rounding as a string (e.g. \"2\") or integer. ",
        "Empty string \"\" (default) means no rounding."
      )
    ),
    examples = list(
      rows = list("Region"),
      cols = list("Category"),
      measures = list("Revenue"),
      agg_fun = "sum",
      digits = "2"
    ),
    prompt = paste(
      "This block produces a pivot (crosstab) table from a long-format data",
      "frame. Mental model: Excel pivot table — rows (y-axis), cols (x-axis,",
      "1-2 nested), measures (numeric values to aggregate), agg_fun (how to",
      "combine them). Dimensions not in rows or cols are aggregated away.",
      "\n\nMap common user requests:",
      "\n- \"mean revenue by region and quarter\" ->",
      "rows=[\"Region\"], cols=[\"Quarter\"], measures=[\"Revenue\"], agg_fun=\"mean\"",
      "\n- \"count rows by region\" ->",
      "rows=[\"Region\"], measures=[], agg_fun=\"n\"",
      "\n- \"total sales and profit by region\" ->",
      "rows=[\"Region\"], measures=[\"Sales\",\"Profit\"], agg_fun=\"sum\"",
      "(when no cols dimension is given with multiple measures, the",
      "measures become the column headers)",
      "\n- \"nested rows by region then country\" ->",
      "rows=[\"Region\",\"Country\"]",
      "\n\nPrefer this block for the \"X by Y (x Z)\" pattern. For a list",
      "of variables summarised by one split, prefer summary_table_block.",
      "\n\nPass this block's output to gt_table_block for styled rendering."
    )
  )
}

#' Build arguments metadata for the summary table block
#' @noRd
summary_table_arguments <- function() {
  structure(
    c(
      state = paste0(
        "Object with fields: ",
        "`vars` (character, variables to summarise — each becomes a row-section), ",
        "`sections` (character, outer grouping columns for nested section structure, 0..N), ",
        "`by` (character, column-split dimensions, 0..2), ",
        "`stats` (\"compact\" for one-row Mean (SD) per numeric, or \"expanded\" ",
        "for the 6-row N / Mean / SD / Median / Q1,Q3 / Min,Max pharma SAP template), ",
        "`add_overall` (logical, append an overall column across all `by` levels), ",
        "`overall_label` (label for the overall column, default \"Total\"). ",
        "Handles numeric, categorical, and logical columns; logicals are rendered ",
        "as a one-row TRUE count for pharma flag variables."
      )
    ),
    examples = list(
      state = list(
        vars = list("AGE", "SEX", "RACE"),
        sections = list(),
        by = list("ARM"),
        stats = "compact",
        add_overall = TRUE,
        overall_label = "Total"
      )
    ),
    prompt = paste(
      "This block produces a multi-variable descriptive summary — the",
      "\"list of variables by Y\" (Table 1 / demographics) pattern. Each",
      "variable in `vars` becomes a row-section in the output: one row",
      "for compact numerics, six rows for expanded numerics, one row per",
      "level for categoricals, one row per flag for logicals.",
      "\n\nMap common user requests:",
      "\n- \"demographics by arm\" ->",
      "vars=[\"AGE\",\"SEX\",\"RACE\"], by=[\"ARM\"], add_overall=TRUE",
      "\n- \"table 1 with full stats\" -> stats=\"expanded\"",
      "\n- \"baseline characteristics by treatment\" ->",
      "vars=[\"AGE\",\"SEX\",\"BMIBL\"], by=[\"TRT01A\"]",
      "\n- \"AE counts by SOC and term\" ->",
      "sections=[\"AESOC\"], vars=[\"AEDECOD\"], by=[\"ARM\"]",
      "\n\nPrefer summary_table_block for \"Table 1\" style summaries.",
      "Prefer pivot_table_block for \"one measure by two dimensions\".",
      "\n\nPass this block's output to gt_table_block for styled rendering."
    )
  )
}

#' Build arguments metadata for the gt table block
#' @noRd
gt_table_arguments <- function() {
  structure(
    c(
      title = "Table title rendered above the table. Empty string for no title.",
      subtitle = "Subtitle rendered under the title. Empty string for no subtitle.",
      full_width = "Logical. TRUE (default) makes the table span the container width.",
      borders = paste0(
        "Logical. TRUE (default) draws 2px top/bottom/heading borders in the ",
        "pharma SAP style."
      ),
      indent_stat = paste0(
        "Integer pixels of indentation for stat-label rows inside numeric ",
        "sections. Default 16. Set to 0 to disable."
      )
    ),
    examples = list(
      title = "Baseline Characteristics",
      subtitle = "ITT population",
      full_width = TRUE,
      borders = TRUE,
      indent_stat = 16L
    ),
    prompt = paste(
      "Render the output of pivot_table_block or summary_table_block as a",
      "styled gt table. This block is a pure renderer — it does not",
      "reshape or aggregate. Place it downstream of a table-shape block.",
      "\n\nUse `title` / `subtitle` for table captions. Use `full_width`,",
      "`borders`, and `indent_stat` for layout polish."
    )
  )
}
