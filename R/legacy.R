# LEGACY: delete with this file.
#
# Argument names and values of blockr.viz < 0.3.0, applied by
# R/legacy-restore.R. The specs (R/spec-chart.R, R/spec-table.R) carry only the
# current names.
#
# LEGACY_NAMES has one row per old name or old value:
#
#   block   "chart", "table", "summary" (a table summary entry) or "shading"
#   old     the old argument or key name
#   new     the current name; NA when the old one is read and dropped
#   values  old value = new value, as a named character vector; empty when
#           the value is taken as it is
#   rule    NA for a plain rename, else what the translation does beyond it
#   since   the version that replaced it
#
# A row with the same name in `old` and `new` maps values only.

# LEGACY: delete with this file
legacy_row <- function(block, old, new, values = character(), rule = NA,
                       since = "0.3.0") {
  list(block = block, old = old, new = new, values = values, rule = rule,
       since = since)
}

# LEGACY: delete with this file
legacy_table <- function(rows) {
  data.frame(
    block = vapply(rows, `[[`, "", "block"),
    old = vapply(rows, `[[`, "", "old"),
    new = vapply(rows, function(r) as.character(r$new), ""),
    values = I(lapply(rows, `[[`, "values")),
    rule = vapply(rows, function(r) as.character(r$rule), ""),
    since = vapply(rows, `[[`, "", "since"),
    stringsAsFactors = FALSE
  )
}

# LEGACY: delete with this file
LEGACY_NAMES <- legacy_table(list(

  # chart -------------------------------------------------------------------
  legacy_row("chart", "chart_type", "type"),
  legacy_row("chart", "baseline", "type", c(cumulative = "waterfall"),
             rule = "only on a bar; \"zero\" is dropped"),
  legacy_row("chart", "type", "type", c(radar = "bar"),
             rule = "warns: radar is no longer offered"),
  legacy_row("chart", "group", "x",
             rule = "the category column; x or y by orientation"),
  legacy_row("chart", "value", "y",
             rule = paste("the aggregated column; x or y by orientation;",
                          "\".count\" sets func = \"count\"")),
  legacy_row("chart", "metric", "y", rule = "as `value`"),
  legacy_row("chart", "agg_fn", "func"),
  legacy_row("chart", "bar_mode", "position",
             c(stacked = "stack", grouped = "dodge", percent = "fill")),
  legacy_row("chart", "tt_fields", "tooltip"),
  legacy_row("chart", "line_width_mult", "line_width"),
  legacy_row("chart", "dot_size_mult", "point_size"),
  legacy_row("chart", "value_lines", "y_lines"),
  legacy_row("chart", "lo", "ymin"),
  legacy_row("chart", "hi", "ymax"),
  legacy_row("chart", "band_id", "id", rule = "first of band_id, count_col"),
  legacy_row("chart", "count_col", "id", rule = "first of band_id, count_col"),
  legacy_row("chart", "count_on", "show_n", c(off = "none")),
  legacy_row("chart", "show_n", "show_n", c(off = "none")),
  legacy_row("chart", "na_group", "missing"),
  legacy_row("chart", "pct_of", "percent_of", c(group = "category")),
  legacy_row("chart", "percent_of", "percent_of",
             rule = "several values keep the first"),
  legacy_row("chart", "facet_cols", "facet_cols",
             rule = "a whole number saved as text reads as that integer"),
  legacy_row("chart", "ref_lo", "ref_min"),
  legacy_row("chart", "ref_hi", "ref_max"),
  legacy_row("chart", "y_scale", "y_transform",
             c(linear = "identity", log = "log10")),
  legacy_row("chart", "x_scale", "x_transform",
             c(linear = "identity", log = "log10")),
  legacy_row("chart", "connect", "connect",
             c(`step-start` = "step_start", `step-middle` = "step_middle",
               `step-end` = "step_end", off = "straight")),
  legacy_row("chart", "box_points", "box_points", c(all = "none")),
  legacy_row("chart", "value_labels", "value_labels",
             c(on = "TRUE", off = "FALSE"), rule = "to logical"),
  legacy_row("chart", "identity_line", "identity_line",
             c(on = "TRUE", off = "FALSE"), rule = "to logical"),
  legacy_row("chart", "connect_centers", "connect_centers",
             c(on = "TRUE", off = "FALSE"), rule = "to logical"),
  legacy_row("chart", "direct_labels", "legend", c(`TRUE` = "marks"),
             rule = "FALSE is dropped"),
  legacy_row("chart", "ctrl_target", "drill_target"),
  legacy_row("chart", "ctrl_table", "drill_table"),
  legacy_row("chart", "values", "script_values"),
  legacy_row("chart", "script", "script",
             rule = "saved as lines, joined by newlines"),
  legacy_row("chart", "filter_type", "selection", rule = "merged into one map"),
  legacy_row("chart", "filters", "selection", rule = "merged into one map"),
  legacy_row("chart", "filter_range", "selection", rule = "merged into one map"),
  legacy_row("chart", "filter_point", "selection", rule = "merged into one map"),
  legacy_row("chart", "filter_column", "selection", rule = "merged into one map"),
  legacy_row("chart", "filter_values", "selection", rule = "merged into one map"),
  legacy_row("chart", "func_toggle", NA, rule = "read and dropped"),

  # table -------------------------------------------------------------------
  legacy_row("table", "value", "columns"),
  legacy_row("table", "values", "columns",
             rule = "a character vector; seen in code, never saved"),
  legacy_row("table", "group", "by"),
  legacy_row("table", "cell_color", "shadings", rule = "parsed into rules"),
  legacy_row("table", "row_color", "color"),
  legacy_row("table", "rotate_titles", "look", c(`TRUE` = "compact"),
             rule = "FALSE is dropped"),
  legacy_row("table", "compact", "look", c(`TRUE` = "compact"),
             rule = "FALSE is dropped"),
  legacy_row("table", "digits", "format", rule = "via table_digits_format()"),
  legacy_row("table", "excel_download", "download",
             rule = "any of the three TRUE gives TRUE"),
  legacy_row("table", "html_download", "download",
             rule = "any of the three TRUE gives TRUE"),
  legacy_row("table", "pptx_download", "download",
             rule = "any of the three TRUE gives TRUE"),
  legacy_row("table", "ctrl_target", "drill_target"),
  legacy_row("table", "ctrl_table", "drill_table"),
  legacy_row("table", "filter_type", "selection", rule = "merged into one map"),
  legacy_row("table", "filter_column", "selection", rule = "merged into one map"),
  legacy_row("table", "filter_values", "selection", rule = "merged into one map"),
  legacy_row("table", "filter_range", "selection", rule = "merged into one map"),
  legacy_row("table", "filter_group_cols", "selection",
             rule = "merged into one map"),
  legacy_row("table", "filter_group_vals", "selection",
             rule = "merged into one map"),
  legacy_row("table", "filter_spread_col", "selection",
             rule = "merged into one map"),
  legacy_row("table", "filter_spread_from", "selection",
             rule = "merged into one map"),
  legacy_row("table", "max_height", NA, rule = "read and dropped"),
  legacy_row("table", "script", "script",
             rule = "saved as lines, joined by newlines"),
  legacy_row("table", "pct_of", "percent_of"),
  legacy_row("table", "axis", "column_axis"),
  legacy_row("table", "sort_by", "sort_by", c(label = "alpha")),
  legacy_row("table", "stats", "stats", c(compact = "mean_sd"),
             rule = paste("\"expanded\" is n, mean, sd, median, q1_q3,",
                          "min_max")),
  legacy_row("table", "color", "color",
             rule = paste("saved before 0.3.0: NULL means \"auto\" and \"\"",
                          "means NULL")),
  legacy_row("table", "summaries", "summaries",
             rule = paste("an old aggregation list(func, cols) becomes one",
                          "number column per function and column")),
  # the summary table block restores as layout = "variables"
  legacy_row("table", "vars", "summaries",
             rule = "one entry per variable; layout = \"variables\""),
  legacy_row("table", "add_overall", "total"),
  legacy_row("table", "overall_label", "total_label"),
  legacy_row("table", "nest_hierarchies", "nest"),
  legacy_row("table", "id_var", "id"),
  legacy_row("table", "indent_details", NA, rule = "read and dropped"),
  legacy_row("table", "state", NA,
             rule = "the interim single-list payload; unwrapped"),

  # summary entries -----------------------------------------------------------
  legacy_row("summary", "type", "type", c(series = "sparkline")),
  legacy_row("summary", "band", "ymin",
             rule = "band = c(low, high) gives ymin = low, ymax = high"),
  legacy_row("summary", "ref", "summary", c(median_iqr = "median_q1_q3"),
             rule = "on a series (sparkline) entry"),
  legacy_row("summary", "ref", "x_lines", rule = "on a pair entry"),
  legacy_row("summary", "ref_lo", "ref_min"),
  legacy_row("summary", "ref_hi", "ref_max"),
  legacy_row("summary", "lo", "ref_min"),
  legacy_row("summary", "hi", "ref_max"),
  legacy_row("summary", "dash", "linetype"),
  legacy_row("summary", "from", "x"),
  legacy_row("summary", "to", "xend"),
  legacy_row("summary", "from_func", "x_func"),
  legacy_row("summary", "to_func", "xend_func"),
  legacy_row("summary", "fields", "tooltip"),
  legacy_row("summary", "stat", "summary"),
  legacy_row("summary", "inner", "summary"),
  legacy_row("summary", "outer", "whiskers"),
  legacy_row("summary", "style", "show", c(dot = "pointrange", box = "box"),
             rule = "on a dist entry"),
  legacy_row("summary", "denom", "id"),
  legacy_row("summary", "position", "position",
             c(stacked = "stack", grouped = "dodge", percent = "fill")),
  legacy_row("summary", "scope", NA, rule = "read and dropped"),
  legacy_row("summary", "parent", NA, rule = "read and dropped (spans)"),
  legacy_row("summary", "facet_order", NA, rule = "read and dropped"),

  # shading rules -----------------------------------------------------------
  legacy_row("shading", "cols", "columns"),
  legacy_row("shading", "type", "mode"),
  legacy_row("shading", "palette", NA, rule = "read and dropped"),
  legacy_row("shading", "source", NA, rule = "read and dropped")
))
