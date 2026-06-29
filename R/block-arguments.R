# Argument metadata helpers for blockr.viz blocks.
#
# Each helper returns a `new_block_args()` structure (one `new_block_arg()`
# per constructor param, carrying a description, a worked `example`, and an
# optional `arg_*()` type descriptor). blockr.core stores these in the
# registry; the assistant / MCP server read them via the
# `block_meta_arguments()` / `block_arg_type()` accessors to surface typed
# parameter documentation to the AI. Per-block construction guidance (the old
# `prompt` attribute) now lives in the `guidance` vector of the
# `register_blocks()` call in registry.R.

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
  new_block_args(
    rows = new_block_arg(
      paste0(
        "Character vector of column names used as row headers (vertical axis). ",
        "Multiple values nest rows from outer to inner. Empty vector = no row grouping."
      ),
      example = list("Region"),
      type = arg_array(arg_string())
    ),
    cols = new_block_arg(
      paste0(
        "Character vector of column names pivoted into column headers (horizontal axis). ",
        "If empty and multiple `measures` are selected, the measures become the columns instead."
      ),
      example = list("Category"),
      type = arg_array(arg_string())
    ),
    measures = new_block_arg(
      paste0(
        "Character vector of numeric column names to aggregate. ",
        "Multiple measures produce multiple output columns (nested under `cols` if present). ",
        "For pure count tables use `agg_fun = \"n\"` and leave `measures` empty."
      ),
      example = list("Revenue"),
      type = arg_array(arg_string())
    ),
    agg_fun = new_block_arg(
      paste0(
        "Aggregation function applied to each measure. ",
        "One of \"sum\", \"mean\", \"median\", \"min\", \"max\", \"n\". Default \"sum\"."
      ),
      example = "sum",
      type = arg_enum(c("sum", "mean", "median", "min", "max", "n"))
    ),
    # Decimal places given as a string OR integer -> polymorphic scalar, so
    # the type is left unset and inferred from the worked example.
    digits = new_block_arg(
      paste0(
        "Decimal places for numeric rounding as a string (e.g. \"2\") or integer. ",
        "Empty string \"\" (default) means no rounding."
      ),
      example = "2"
    )
  )
}

#' Build arguments metadata for the summary table block
#' @noRd
summary_table_arguments <- function() {
  new_block_args(
    vars = new_block_arg(
      paste0(
        "Character, variables to summarise \u2014 each becomes a row-section. ",
        "Handles numeric, categorical, and logical columns; logicals are ",
        "rendered as a one-row TRUE count for pharma flag variables."
      ),
      example = list("AEDECOD"),
      type = arg_array(arg_string())
    ),
    sections = new_block_arg(
      paste0(
        "Character, OUTER grouping columns that CONTAIN the `vars`, 0..N \u2014 ",
        "use ONLY for a true nesting hierarchy such as SOC containing PT; ",
        "leave empty for a flat list of variables."
      ),
      example = list("AEBODSYS"),
      type = arg_array(arg_string())
    ),
    by = new_block_arg(
      "Character, column-split dimensions, 0..2.",
      example = list("TRT01A"),
      type = arg_array(arg_string())
    ),
    stats = new_block_arg(
      paste0(
        "\"compact\" for one-row Mean (SD) per numeric, or \"expanded\" for the ",
        "6-row N / Mean / SD / Median / Q1,Q3 / Min,Max pharma SAP template."
      ),
      example = "compact",
      type = arg_enum(c("compact", "expanded"))
    ),
    add_overall = new_block_arg(
      "Logical, append an overall column across all `by` levels.",
      example = TRUE,
      type = arg_boolean()
    ),
    overall_label = new_block_arg(
      "Label for the overall column, default \"Total\".",
      example = "Total",
      type = arg_string()
    ),
    indent_details = new_block_arg(
      paste0(
        "Logical, default TRUE \u2014 indent detail rows under their variable ",
        "header; rarely changed."
      ),
      example = TRUE,
      type = arg_boolean()
    ),
    nest_hierarchies = new_block_arg(
      paste0(
        "Logical, default FALSE \u2014 advanced row-side drill-down for adjacent ",
        "functionally-dependent categorical vars; leave FALSE unless asked."
      ),
      example = FALSE,
      type = arg_boolean()
    ),
    id_var = new_block_arg(
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

#' Construction guidance for the summary table block
#' @noRd
summary_table_guidance <- function() {
  paste(
      "This block produces a multi-variable descriptive summary \u2014 the",
      "\"list of variables by Y\" (Table 1 / demographics, AE counts) pattern.",
      "Each variable in `vars` becomes a row-section in the output: one row",
      "for compact numerics, six rows for expanded numerics, one row per",
      "level for categoricals, one row per flag for logicals.",
      "\n\nKey distinctions:",
      "\n- `vars` = the variables being summarised (the rows).",
      "\n- `sections` = an OUTER column that CONTAINS the vars in a hierarchy",
      "(SOC contains PT). Only set it for genuine nesting; for a plain list",
      "of variables leave `sections` empty and put everything in `vars`.",
      "RULE: when two categoricals are named where one NESTS INSIDE the other",
      "\u2014 SOC inside which PTs fall, Region inside which Countries fall \u2014 put",
      "the OUTER/containing one in `sections` and the INNER/detail one in",
      "`vars`. Do NOT put both in `vars`. So \"AEs by SOC and preferred term\"",
      "= sections=[\"AEBODSYS\"], vars=[\"AEDECOD\"] (NOT vars=[both]).",
      "\n- `by` = the column split (treatment arm).",
      "\n- `id_var` = subject id for DISTINCT-subject counts. Default \"\"",
      "(count rows). Set it ONLY when the user EXPLICITLY asks for distinct",
      "patients/subjects \u2014 phrases like \"each subject counted once\", \"number",
      "of patients with ...\", \"unique subjects\". Do NOT set it merely because",
      "a subject-id column (USUBJID) exists in the data, and never for one-row-",
      "per-subject data like demographics.",
      "\n\nMap common user requests:",
      "\n- \"demographics by arm\" ->",
      "vars=[\"AGE\",\"SEX\",\"RACE\"], by=[\"TRT01A\"], add_overall=TRUE, id_var=\"\"",
      "\n- \"table 1 with full stats\" -> stats=\"expanded\"",
      "\n- \"baseline characteristics by treatment\" ->",
      "vars=[\"AGE\",\"SEX\",\"BMIBL\"], by=[\"TRT01A\"]",
      "\n- \"AE counts by SOC and term\" ->",
      "sections=[\"AEBODSYS\"], vars=[\"AEDECOD\"], by=[\"TRT01A\"]",
      "\n- \"number of PATIENTS with each AE\" (event-level data) ->",
      "vars=[\"AEDECOD\"], by=[\"TRT01A\"], id_var=\"USUBJID\"",
      "\n\nPass this block's output to gt_table_block for styled rendering."
  )
}

#' Build arguments metadata for the gt table block
#' @noRd
gt_table_arguments <- function() {
  new_block_args(
    title = new_block_arg(
      "Table title rendered above the table. Empty string for no title.",
      example = "Baseline Characteristics",
      type = arg_string()
    ),
    subtitle = new_block_arg(
      "Subtitle rendered under the title. Empty string for no subtitle.",
      example = "ITT population",
      type = arg_string()
    ),
    full_width = new_block_arg(
      "Logical. TRUE (default) makes the table span the container width.",
      example = TRUE,
      type = arg_boolean()
    ),
    borders = new_block_arg(
      paste0(
        "Logical. TRUE (default) draws 2px top/bottom/heading borders in the ",
        "pharma SAP style."
      ),
      example = TRUE,
      type = arg_boolean()
    ),
    na_rep = new_block_arg(
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
