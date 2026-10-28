# Spec entries both blocks share. viz_shared_args() is spliced into both specs
# unchanged; the builders below are for shared arguments whose `applies`,
# `ctrl` or example differ between the blocks. Entries are built inside
# functions so nothing runs while R sources the files.

# Interval statistics, shared by the chart's `summary` and `whiskers` and the
# table's dist summaries.
SPEC_STATS <- c("median_q1_q3", "mean_sd", "mean_2sd", "mean_se",
                "mean_ci95", "p5_p95", "p10_p90", "min_max")
SPEC_STAT_LABELS <- c("Median \u00b7 Q1\u2013Q3", "Mean \u00b1 SD",
                      "Mean \u00b1 2 SD", "Mean \u00b1 SE",
                      "Mean \u00b7 95% CI", "5th\u201395th percentile",
                      "10th\u201390th percentile", "Min\u2013Max")

viz_shared_args <- function() {
  list(
    title = viz_arg(
      type = "template", default = NULL, null = "the input's label",
      gear = gear("Titles", "template", "Title"),
      slot = FALSE,
      example = "Sales by {region}",
      describe = paste(
        "Text above the block. NULL uses the input's label, \"\" shows",
        "nothing, any other string is a template resolved against the data:",
        "`{col}`, `{label(col)}`, `{n}`, `{n_distinct(col)}`, `{filters}`,",
        "`{@arg}` (one of the block's settings, clickable), and `[...]` for a",
        "clause that disappears when its setting is empty."
      )
    ),

    subtitle = viz_arg(
      type = "template", default = NULL, null = "the input's subtitle",
      gear = gear("Titles", "template", "Subtitle"),
      slot = FALSE,
      example = "Mean {label(revenue)} per {region}",
      describe = "Text under the title, a template as `title`."
    ),

    caption = viz_arg(
      type = "template", default = opt("blockr.viz.default_caption"),
      null = "the input's caption",
      gear = gear("Titles", "template", "Caption"),
      slot = FALSE,
      example = "N = {n_distinct(customer_id)} customers",
      describe = paste(
        "Text below the block, a template as `title`. Defaults to the option",
        "`blockr.viz.default_caption`."
      )
    ),

    drill_target = viz_arg(
      type = "string", default = "",
      gear = gear("Drill-down", "select", "Target filter"),
      slot = FALSE,
      example = "cohort_filter",
      describe = paste(
        "Beta. The id of a filter block a click is sent to, or \"auto\" for",
        "the board's drill destination. With a target set a click is an",
        "event and the block holds no selection."
      )
    ),

    drill_table = viz_arg(
      type = "string", default = "",
      gear = gear("Drill-down", "text", "Table", when = "drill_target"),
      slot = FALSE,
      example = "adsl",
      describe = "Beta. The table in the target's dm the click applies to."
    ),

    script = viz_arg(
      type = "string", default = NULL, null = "no script",
      gear = gear("Prepare script", "script", "Prepare script"),
      slot = FALSE, ai = FALSE,
      describe = paste(
        "Experimental. R code run on the input before the block sees it. Its",
        "top-level plain-value assignments become controls on the block."
      )
    ),

    script_values = viz_arg(
      type = "map", default = list(),
      slot = FALSE, ai = FALSE,
      describe = paste(
        "Settings of the script's controls, by variable name. The script code,",
        "not the spec, makes each one a title slot."
      )
    ),

    # Not externally controllable: core freezes external_ctrl fields on a
    # locked board, and viewers must be able to drill there.
    selection = viz_arg(
      type = "map", default = NULL, null = "nothing selected",
      slot = FALSE, ctrl = FALSE, ai = FALSE,
      describe = paste(
        "What a click or brush selected. Written by the block, saved so a",
        "restored board keeps it; leave it NULL."
      )
    )
  )
}

# The chart passes `ctrl = FALSE`: `download` is not in its external_ctrl.
viz_download_arg <- function(ctrl = TRUE) {
  viz_arg(
    type = "bool", default = TRUE,
    gear = gear("Reader tools", "segmented", "Download",
                labels = c("Download", "No download")),
    slot = TRUE, ctrl = ctrl,
    describe = "TRUE offers the block for download."
  )
}

viz_format_arg <- function(applies = NULL, example = "number") {
  viz_arg(
    type = "format", default = "number", applies = applies,
    gear = gear("Numbers", "select", "Numbers"),
    slot = TRUE,
    example = example,
    describe = paste(
      "How numbers print: \"number\" (separators and up to two decimals,",
      "trailing zeros dropped; a value that would round to 0 keeps two",
      "significant digits), \"compact\" (1.2K, 3.4M), \"percent\" (a",
      "fraction times 100, with \"%\"), or a whole number of decimals from",
      "0 to 6, printed exactly."
    )
  )
}

viz_unit_arg <- function(applies = NULL, example = "CHF") {
  viz_arg(
    type = "string", default = "", applies = applies,
    gear = gear("Numbers", "text", "Unit"),
    slot = TRUE,
    example = example,
    describe = "Free text printed after the number (\"CHF\"); headers carry it once."
  )
}

viz_missing_arg <- function(applies = NULL, group = "Mapping",
                            example = "level") {
  viz_arg(
    type = "enum", default = "level", values = c("level", "drop"),
    applies = applies,
    gear = gear(group, "segmented", "Missing",
                labels = c("Own level", "Drop")),
    slot = TRUE,
    example = example,
    describe = paste(
      "Rows whose category is missing: \"level\" gives them their own",
      "(\"(Missing)\"; a click on it gives the rows with no value), \"drop\"",
      "removes them from the categories."
    )
  )
}
