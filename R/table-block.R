# What the assistant reads about the table block.
table_guidance <- function() {
  paste0(
    "The table block draws every table. `layout` says what one row is:\n",
    "- \"records\": the input rows as they are -- listings, model output, ",
    "a correlation matrix, or a finished table built in R (a frame from ",
    "dplyr, summary_table(), gtsummary converted with as_annotated_df()). ",
    "`rowname` picks the stub column; `columns` EXACTLY the columns the ",
    "user names (\"Revenue and Profit\" -> columns = [\"Revenue\", ",
    "\"Profit\"]), empty for all. `by` nests the rows under one header per ",
    "level. `shadings` colours numeric cells (diverging for a correlation ",
    "matrix, sequential for magnitudes); `format` is \"number\", ",
    "\"compact\", \"percent\" or a number of decimals, `unit` a unit ",
    "for the headers.\n",
    "- \"groups\": one row per level of `by` (at most two, outer first; the ",
    "outer is an expandable parent), one column per entry of `summaries`. ",
    "Without `by` it is one row over all input rows: the headline numbers ",
    "of a dashboard, usually with look = \"cards\". ",
    "Use it when one row is one category: most frequent adverse events ",
    "(simple count_distinct of USUBJID, show = bar), duration by term ",
    "(dist, show = box), an episode swimlane per subject (spans), a lab ",
    "trajectory (series), baseline to peak (pair). `color` on a summary ",
    "splits its cell (severity); `facet` repeats it per level (treatment ",
    "arm) on one scale; both are per column, never table-wide.\n",
    "- \"variables\": a Table 1. One row per statistic or level of each ",
    "variable in `summaries` (a dist for a numeric variable, a simple count ",
    "split by color for a categorical one), one column per level of `by`. ",
    "`stats` picks the numeric statistics (mean_sd, median_q1_q3, n_pct, ",
    "min_max, ...), `total` adds a Total column, `sections` is an OUTER ",
    "column that contains the variables (SOC over preferred term: ",
    "sections = [\"AEBODSYS\"], the term as the variable).\n",
    "A heatmap is the groups layout with look = \"compact\": by = [arm, ",
    "subject], one simple count with show = \"tile\", facet = the term, ",
    "fill = the grade (painted by the worst level). look = \"cards\" draws ",
    "one card per summary.\n",
    "Counting: only when the user asks for subjects or patients rather ",
    "than events, never for one-row-per-subject data. Groups layout: a ",
    "simple count_distinct with col = USUBJID. Variables layout: ",
    "`id` = USUBJID. ",
    "`pct_of` = \"facet\" (default) takes a count's percentage over its ",
    "arm; \"total\" over the table. `missing` = \"level\" (default) shows ",
    "missing groups as \"(Missing)\"; \"drop\" leaves them out.\n",
    "`drill` makes a row click filter downstream blocks; `drill_target` ",
    "sends the click to a filter block instead. Do NOT set `top_n` for an ",
    "interactive board (the table scrolls); it is for report exhibits.\n",
    "Map common requests: \"demographics by arm\" -> layout = \"variables\", ",
    "by = [\"TRT01A\"], summaries = AGE (dist), SEX and RACE (simple count ",
    "split by color), total = TRUE. \"most frequent AEs per arm\" -> layout ",
    "= \"groups\", by = [\"AEDECOD\"], summaries = ",
    "[simple count_distinct of USUBJID, facet = \"TRT01A\"]. For vertical ",
    "columns, lines or two measures on axes, use the chart block."
  )
}

#' @rdname new_table_block
#' @export
new_table_block <- function(layout = "records",
                            # `...` early: later arguments match exactly, so
                            # an old `value` cannot match `value_labels`.
                            ...,
                            by = character(),
                            rowname = NULL,
                            columns = NULL,
                            summaries = list(),
                            id = NULL,
                            percent_of = "facet",
                            missing = "level",
                            color = "auto",
                            shadings = list(),
                            format = "number",
                            unit = "",
                            look = "table",
                            facet_layout = "by_summary",
                            group_rows = "name",
                            open = "none",
                            sort_by = "value",
                            sort_dir = NULL,
                            top_n = NULL,
                            value_labels = TRUE,
                            column_axis = TRUE,
                            bar_width = "fit",
                            drill = NULL,
                            search = TRUE,
                            sortable = TRUE,
                            collapsible = TRUE,
                            download = TRUE,
                            stats = "mean_sd",
                            sections = character(),
                            total = FALSE,
                            total_label = "Total",
                            nest = FALSE,
                            title = NULL,
                            subtitle = NULL,
                            caption = getOption("blockr.viz.default_caption"),
                            drill_target = "",
                            drill_table = "",
                            script = NULL,
                            script_values = list(),
                            selection = NULL) {

  spec <- viz_block_spec("table")
  given <- mget(intersect(as.character(names(match.call())[-1L]),
                          names(spec$args)))
  own <- table_block_own(list(...))
  gear <- own$gear
  cls <- own$class
  dots <- own$dots
  # blockr.core's block arguments pass through; any other name is an old
  # argument name, or unknown, which viz_check() refuses.
  old <- setdiff(names(dots), VIZ_CORE_BLOCK_ARGS)
  args <- legacy_ctor_args("table", c(given, dots[old]))
  values <- table_normalize(viz_check(spec, args))
  # blockr.core reads the starting state from this frame.
  list2env(values, environment())

  core <- dots[intersect(names(dots), VIZ_CORE_BLOCK_ARGS)]
  ctrl <- table_ctrl_args(spec, core$ctor, core$ctor_pkg)
  # blockr.core takes the constructor from the call stack; through do.call()
  # it has to be told which frame is this one.
  if (is.null(core$ctor)) core$ctor <- sys.nframe()
  res <- do.call(blockr.core::new_transform_block, c(list(
    server = function(id, data) table_server(id, data, values, gear),
    ui = if (length(gear$deps)) {
      function(id) shiny::tagList(table_ui(id), gear$deps)
    } else {
      table_ui
    },
    dat_valid = validate_table_input,
    allow_empty_state = viz_empty_ok(spec),
    external_ctrl = ctrl,
    expr_type = "bquoted",
    class = c(setdiff(cls, "table_block"), "table_block")
  ), core, dots[old]))
  # Old names must not be saved back as attributes.
  for (nm in old) attr(res, nm) <- NULL
  res
}

# A block built on the table names its own gear and class in `...`. Neither
# is a setting: the block's constructor sets them on every build, and only
# the settings are saved.
table_block_own <- function(dots) {
  gear <- dots$gear
  stopifnot(is.null(gear) || inherits(gear, "viz_table_gear"))
  cls <- dots$class
  stopifnot(is.null(cls) || is.character(cls))
  dots[c("gear", "class")] <- NULL
  list(gear = gear, class = cls, dots = dots)
}

# The settings other blocks and the assistant may set. A block built on the
# table with its own constructor (a swimmer) saves and takes only that
# constructor's arguments, so only those.
table_ctrl_args <- function(spec, ctor, pkg) {
  ctrl <- viz_ctrl_args(spec)
  if (!is.character(ctor) || identical(ctor, "new_table_block") ||
        !is.character(pkg)) {
    return(ctrl)
  }
  fun <- get0(ctor, envir = asNamespace(pkg), mode = "function")
  if (is.null(fun)) {
    return(ctrl)
  }
  intersect(ctrl, names(formals(fun)))
}

table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    table_view_dep(),
    viz_output(ns("table"), "table")
  )
}
