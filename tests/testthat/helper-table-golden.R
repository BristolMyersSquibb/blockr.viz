# Recorded behaviour of the table block, for the rewrite. The chart's
# recordings (helper-chart-golden.R on refactor/chart-block) work the same
# way.
#
# A scenario is a constructor call (public argument names), a data set and a
# list of steps (what the browser sends). Running one records the
# constructor's warnings, its starting and saved state, and, after the first
# flush and after every step: the block state, the expression sent
# downstream, its result, the block's conditions, every custom message the
# server sent (JSON payloads parsed), the rendered HTML outputs, every
# control-channel send, and a downloaded file where the step is a download.
# The recordings live in fixtures/table-golden/ and are written by
# dev/table-golden-record.R; test-table-golden.R replays each scenario.
#
# State is keyed by the constructor's argument names and sorted by name.
# The steps are the reference's browser messages: their keys are the
# browser protocol (`group`, `rotate_titles`, `ctrl_target`, ...), not
# public names. A rewrite that changes the protocol translates the step,
# not the recording. Renames of public names live in one table,
# table_golden_renames(), applied when the installed blockr.viz has them.

# --- data -------------------------------------------------------------------

table_golden_df <- function() {
  d <- data.frame(
    cat = c("a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "d"),
    grp = rep(c("g1", "g2"), 6),
    fac = factor(rep(c("lo", "hi", "mid"), 4), levels = c("lo", "mid", "hi")),
    num = c(1.5, 2, 3.25, 4, 5, 6.5, 7, 8, 9.75, 10, 11, 12),
    num2 = c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8),
    int = 1:12,
    id = sprintf("S%02d", c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)),
    day = as.Date("2024-01-01") + c(0, 7, 14, 0, 7, 14, 0, 7, 14, 0, 7, 14),
    flag = c(TRUE, FALSE, NA, TRUE, FALSE, TRUE, TRUE, FALSE, NA, TRUE,
             FALSE, TRUE),
    cat_na = c("a", NA, "b", "a", "b", NA, "a", "b", "a", NA, "b", "a"),
    grade = c(1, 2, 3, 1, 1, 2, 3, 3, 2, 1, 2, 3),
    stringsAsFactors = FALSE
  )
  d$end <- d$day + c(3, 5, 2, 8, 1, 4, 6, 2, 9, 3, 5, 7)
  d$ulim <- 9
  d$llim <- 2
  attr(d$num, "label") <- "Numeric value"
  attr(d$cat, "label") <- "Category"
  d
}

table_golden_tile_df <- function() {
  data.frame(
    region = c("North", "North", "South", "East"),
    revenue = c(10, 20, 30, 15),
    orders = c(1L, 2L, 3L, 4L),
    metric = c("Revenue", "Orders", "Margin", "Churn"),
    value = c(100, 25, 0.3, 0.05),
    target = c(120, 30, 0.25, 0.04),
    stringsAsFactors = FALSE
  )
}

table_golden_data <- function(name) {
  switch(name,
    df = table_golden_df(),
    df_titled = local({
      d <- table_golden_df()
      attr(d, "label") <- "Data label"
      attr(d, "subtitle") <- "Data subtitle"
      attr(d, "caption") <- "Data caption"
      d
    }),
    df_empty = table_golden_df()[0, ],
    df_changed = local({
      d <- table_golden_df()
      d$num <- d$num * 2
      d
    }),
    df_no_grp = table_golden_df()[setdiff(names(table_golden_df()), "grp")],
    df_no_cat = table_golden_df()[setdiff(names(table_golden_df()), "cat")],
    # A finished table: what summary_table() hands the records layout.
    ann = summary_table(table_golden_df(), vars = c("num", "cat"),
                        by = "grp"),
    # A table-producing object, coerced through as_annotated_df().
    tbl = structure(
      list(df = summary_table(table_golden_df(), vars = "cat", by = "grp")),
      class = "table_golden_tbl"
    ),
    tile = table_golden_tile_df(),
    not_df = list(a = 1),
    # No block connected upstream (run with `inputs_ready = FALSE`).
    none = NULL,
    stop("unknown table golden data ", name)
  )
}

table_golden_register_tbl <- function() {
  registerS3method("as_annotated_df", "table_golden_tbl",
                   function(x, ...) x$df, envir = asNamespace("blockr.viz"))
}

# --- steps ------------------------------------------------------------------
#
#   list(input = <id>, value = <msg>)   a browser input, by its id in the
#                                       block's module ("records-...")
#   list(data = "<name>")               upstream hands over another frame
#   list(download = "<output id>")      a download link is clicked
#
# The records engine (records and variables layouts) and the groups engine
# have an action input each; the variables layout's settings widget has its
# own.

tg_rec_cfg <- function(param, value) {
  list(input = "records-drilldown_table_block_action",
       value = list(action = "config", param = param, value = value))
}
tg_rec_flt <- function(...) {
  list(input = "records-drilldown_table_block_action",
       value = list(action = "filter", ...))
}
tg_grp_cfg <- function(param, value) {
  list(input = "groups-summarize_table_block_action",
       value = list(action = "config", param = param, value = value))
}
tg_grp_click <- function(column, values, nonce = 1) {
  list(input = "groups-summarize_table_block_action",
       value = list(action = "filter", column = column, values = values,
                    nonce = nonce))
}
tg_grp_clear <- function() {
  list(input = "groups-summarize_table_block_action",
       value = list(action = "clear_filter"))
}
tg_grp_ready <- function(n = 1) {
  list(input = "groups-summarize_table_block_ready", value = n)
}
tg_grp_capture <- function() {
  list(input = "groups-summarize_table_block_capture",
       value = list(png = table_golden_capture_png(), width = 640,
                    height = 400, title = "Captured"))
}
tg_var_widget <- function(...) {
  st <- list(vars = list(), sections = list(), by = list(),
             stats = list("mean_sd"), add_overall = FALSE,
             overall_label = "Total", indent_details = TRUE,
             nest_hierarchies = FALSE, id_var = "")
  new <- list(...)
  st[names(new)] <- new
  list(input = "variables-summary_input", value = st)
}
tg_dl <- function(id) list(download = id)

table_golden_capture_png <- function() {
  # A 1x1 transparent png.
  paste0("data:image/png;base64,",
         "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR4nGNgYGD4DwABBAEAwS2OUAAAAABJRU5ErkJggg==")
}

# --- scenarios ----------------------------------------------------------------

table_golden_all_summaries <- function() {
  list(
    list(type = "simple", func = "count", show = "bar", name = "N"),
    list(type = "simple", func = "mean", col = "num", show = "number"),
    list(type = "simple", func = "sum", col = "num2", show = "dot"),
    list(type = "simple", func = "count_distinct", col = "id", show = "bar",
         name = "Subjects"),
    list(type = "dist", col = "num", show = "box", summary = "median_q1_q3",
         whiskers = "min_max"),
    list(type = "dist", col = "num2", show = "pointrange",
         summary = "mean_se", style = "dot"),
    list(type = "dist", col = "num", show = "text", summary = "mean_sd"),
    list(type = "field", col = "fac"),
    list(type = "series", x = "int", col = "num", ref = "mean",
         band = list("num2", "num")),
    list(type = "spans", x = "day", xend = "end", label = "fac",
         tooltip = list("id"), size = "lg"),
    list(type = "pair", x = "num2", xend = "num", x_func = "min",
         xend_func = "max", ref_lo = "llim", ref_hi = "ulim", ref = "5",
         dash = "grp"),
    list(type = "expr", name = "Range", expr = "max(num) - min(num)")
  )
}

table_golden_scenarios <- function() {
  list(
    # --- records layout ------------------------------------------------------
    rec_default = list(args = list()),
    rec_columns = list(args = list(
      rowname = "cat", columns = c("num", "num2", "day", "flag"),
      format = 1, unit = "kg"
    )),
    rec_format_compact = list(args = list(
      rowname = "id", columns = c("num", "int"), format = "compact"
    )),
    rec_format_percent = list(args = list(
      rowname = "id", columns = "num2", format = "percent"
    )),
    rec_shadings = list(args = list(
      rowname = "cat", columns = c("num", "num2", "int"),
      shadings = list(
        list(mode = "sequential", cols = list("num")),
        list(mode = "diverging", cols = list("num2"), domain = list(1, 9)),
        list(mode = "bar", cols = list("int"))
      ),
      color = "grp"
    )),
    rec_color_off = list(args = list(rowname = "cat", color = "")),
    rec_toggles = list(args = list(
      search = FALSE, sortable = FALSE, collapsible = FALSE,
      download = FALSE, max_height = "300px"
    )),
    rec_by = list(
      args = list(by = "grp", rowname = "id", columns = c("num", "cat_na"),
                  drill = "id"),
      steps = list(
        tg_rec_flt(filters = list(list(column = ".group1_level",
                                    value = "g2")), nonce = 1),
        tg_rec_flt(column = NULL, values = list()),
        tg_rec_cfg("group", list("cat_na", "grp")),
        tg_rec_cfg("missing", "drop"),
        tg_rec_cfg("group", list())
      )
    ),
    rec_by_missing = list(
      args = list(by = "cat_na", rowname = "id", missing = "level"),
      steps = list(
        tg_rec_flt(filters = list(list(column = ".group1_level",
                                    value = "(Missing)")), nonce = 1)
      )
    ),
    rec_drill = list(
      args = list(rowname = "cat", drill = "cat"),
      steps = list(
        tg_rec_flt(column = "cat", values = list("b")),
        tg_rec_flt(column = "cat", values = list("a", "c")),
        tg_rec_flt(column = NULL, values = list()),
        tg_rec_cfg("drill", "(none)")
      )
    ),
    rec_drill_restore = list(args = list(
      rowname = "cat", drill = "cat",
      selection = list(column = "cat", values = list("a"))
    )),
    rec_drill_target = list(
      args = list(rowname = "cat", drill = "cat", drill_target = "vf1",
                  drill_table = "adsl"),
      steps = list(
        tg_rec_flt(column = "cat", values = list("b"), nonce = 1),
        tg_rec_flt(column = "cat", values = list("b"), nonce = 2),
        tg_rec_flt(column = NULL, values = list(), nonce = 3),
        tg_rec_cfg("ctrl_target", ""),
        tg_rec_cfg("ctrl_table", " t2 ")
      )
    ),
    rec_structured = list(
      args = list(drill = "auto"),
      data = "ann",
      steps = list(
        tg_rec_flt(filters = list(list(column = ".variable", value = "cat"),
                               list(column = ".variable_level",
                                    value = "a")),
                col_filters = list(list(column = "grp",
                                        values = list("g1"))),
                nonce = 1),
        tg_rec_flt(filters = list(), col_filters = list(
          list(column = "grp", values = list("g2"))), nonce = 2),
        tg_rec_flt(column = NULL, values = list())
      )
    ),
    rec_source = list(
      args = list(drill = "source"),
      data = "ann",
      steps = list(
        tg_rec_flt(filters = list(list(column = ".variable", value = "cat"),
                               list(column = ".variable_level",
                                    value = "b")), nonce = 1),
        tg_rec_flt(filters = list(list(column = ".variable", value = "cat"),
                               list(column = ".variable_level",
                                    value = "b")),
                col_filters = list(list(column = "grp",
                                        values = list("g2"))), nonce = 2)
      )
    ),
    rec_input_tbl = list(args = list(caption = "{n} rows"), data = "tbl"),
    rec_compact = list(args = list(look = "compact", rowname = "cat",
                                   columns = c("num", "num2"))),
    rec_cards = list(args = list(look = "cards", rowname = "cat",
                                 columns = c("num", "num2"), unit = "kg")),
    rec_gear = list(
      args = list(rowname = "cat"),
      steps = list(
        tg_rec_cfg("rowname", "id"),
        tg_rec_cfg("columns", list("num", "", "grp")),
        tg_rec_cfg("color", "fac"),
        tg_rec_cfg("color", "(none)"),
        tg_rec_cfg("shadings", list(list(mode = "bar", cols = list("num")))),
        tg_rec_cfg("shadings", "[{\"mode\":\"sequential\",\"cols\":[\"num\"]}]"),
        tg_rec_cfg("format", "compact"),
        tg_rec_cfg("digits", 3),
        tg_rec_cfg("format", "bogus"),
        tg_rec_cfg("unit", " CHF "),
        tg_rec_cfg("sortable", "off"),
        tg_rec_cfg("collapsible", "off"),
        tg_rec_cfg("search", "off"),
        tg_rec_cfg("download", "off"),
        tg_rec_cfg("download", "on"),
        tg_rec_cfg("rotate_titles", "on"),
        tg_rec_cfg("look", "cards"),
        tg_rec_cfg("look", "nonsense"),
        tg_rec_cfg("drill", "cat"),
        tg_rec_cfg("rowname", "(none)"),
        tg_rec_cfg("missing", "drop"),
        tg_rec_cfg("missing", "keep"),
        tg_rec_cfg("title", "Rows: {n}"),
        tg_rec_cfg("subtitle", ""),
        tg_rec_cfg("caption", NULL)
      )
    ),
    rec_titles = list(
      args = list(title = "{n} rows of {label(cat)}",
                  subtitle = "{n_distinct(id)} ids", caption = ""),
      data = "df_titled"
    ),
    rec_titles_auto = list(args = list(), data = "df_titled"),
    rec_titles_slots = list(args = list(
      rowname = "cat", title = "By {@rowname}[, with {@columns}]",
      subtitle = "{label(@rowname)}", caption = "{@look} look"
    )),
    rec_script = list(
      args = list(
        script = c("k <- 5", "subset(data, num > k)"),
        script_values = list(k = 6), rowname = "cat", drill = "cat"
      ),
      steps = list(
        tg_rec_cfg("sv_k", "9"),
        tg_rec_flt(column = "cat", values = list("b")),
        tg_rec_cfg("script", "stop('boom')"),
        tg_rec_cfg("script", "")
      )
    ),
    rec_transport = list(
      args = list(rowname = "cat"),
      steps = list(
        list(data = "df_changed"),
        list(data = "df_changed"),
        list(data = "df_empty"),
        list(data = "df_no_cat"),
        list(data = "df")
      )
    ),
    # A deployment captions every new table (read at construction).
    rec_default_caption = list(
      args = list(), options = list(blockr.viz.default_caption = "{n} rows")
    ),
    grp_default_caption = list(
      args = list(layout = "groups", by = "cat"),
      options = list(blockr.viz.default_caption = "Source: {n} rows")
    ),
    rec_not_df = list(args = list(), data = "not_df"),
    rec_no_input = list(args = list(rowname = "cat"), data = "none",
                        inputs_ready = FALSE),
    rec_download = list(
      args = list(rowname = "cat", columns = c("num", "num2"), format = 1,
                  unit = "kg", title = "Records", subtitle = "Sub",
                  caption = "Cap",
                  shadings = list(list(mode = "sequential",
                                       cols = list("num")))),
      steps = list(tg_dl("records-dl_xlsx"), tg_dl("records-dl_html"),
                   tg_dl("records-dl_pptx"), tg_dl("records-dl_png"),
                   tg_rec_cfg("download", "off"), tg_dl("records-dl_xlsx"))
    ),
    rec_download_by = list(
      args = list(by = "grp", rowname = "id", columns = c("num"),
                  collapsible = FALSE, sortable = FALSE),
      steps = list(tg_dl("records-dl_xlsx"), tg_dl("records-dl_html"),
                   tg_dl("records-dl_pptx"))
    ),
    rec_download_cards = list(
      args = list(look = "cards", rowname = "cat", columns = "num"),
      steps = list(tg_dl("records-dl_xlsx"), tg_dl("records-dl_html"),
                   tg_dl("records-dl_pptx"))
    ),
    rec_download_structured = list(
      args = list(title = "Table 1"),
      data = "ann",
      steps = list(tg_dl("records-dl_xlsx"), tg_dl("records-dl_html"),
                   tg_dl("records-dl_pptx"))
    ),

    # --- groups layout -------------------------------------------------------
    grp_default = list(args = list(layout = "groups", by = "cat")),
    grp_no_by = list(args = list(layout = "groups")),
    grp_all_types = list(args = list(
      layout = "groups", by = "cat",
      summaries = table_golden_all_summaries()
    )),
    grp_color_split = list(args = list(
      layout = "groups", by = "cat",
      summaries = list(
        list(type = "simple", func = "count", show = "bar", color = "grp",
             position = "stack"),
        list(type = "simple", func = "sum", col = "num", show = "bar",
             color = "fac", position = "dodge"),
        list(type = "simple", func = "count", show = "bar", color = "grp",
             position = "fill")
      )
    )),
    grp_facet = list(
      args = list(
        layout = "groups", by = "cat", pct_of = "total",
        facet_layout = "by_level",
        summaries = list(
          list(type = "simple", func = "count", show = "bar", facet = "grp"),
          list(type = "simple", func = "mean", col = "num", show = "number",
               facet = "grp"),
          list(type = "field", col = "fac")
        )
      ),
      steps = list(tg_grp_cfg("pct_of", "facet"),
                   tg_grp_cfg("facet_layout", "by_summary"),
                   tg_grp_cfg("sort_by", "g2"))
    ),
    grp_two_by = list(
      args = list(layout = "groups", by = c("grp", "cat"),
                  collapsible = FALSE),
      steps = list(
        tg_grp_click(list("grp", "cat"), list("g1", "a")),
        tg_grp_click(list("grp"), list("g2"), nonce = 2),
        tg_grp_clear()
      )
    ),
    grp_sort = list(
      args = list(
        layout = "groups", by = "cat", sort_by = "alpha", sort_dir = "asc",
        top_n = 2L,
        summaries = list(list(type = "simple", func = "sum", col = "num",
                              show = "bar", name = "Total"))
      ),
      steps = list(
        tg_grp_cfg("sort_by", "data"), tg_grp_cfg("sort_by", "Total"),
        tg_grp_cfg("sort_by", "label"), tg_grp_cfg("sort_dir", "desc"),
        tg_grp_cfg("top_n", "3"), tg_grp_cfg("top_n", "0"),
        tg_grp_cfg("top_n", "junk")
      )
    ),
    grp_display = list(
      args = list(layout = "groups", by = "cat", value_labels = FALSE,
                  axis = FALSE, bar_width = "narrow", search = FALSE,
                  sortable = FALSE, max_height = "250px"),
      steps = list(
        tg_grp_cfg("value_labels", "on"), tg_grp_cfg("axis", "on"),
        tg_grp_cfg("bar_width", "wide"), tg_grp_cfg("bar_width", "huge"),
        tg_grp_cfg("search", "on"), tg_grp_cfg("sortable", "on")
      )
    ),
    grp_missing = list(
      args = list(
        layout = "groups", by = "cat_na", drill = "cat_na",
        summaries = list(list(type = "simple", func = "count", show = "bar",
                              color = "cat_na"))
      ),
      steps = list(
        tg_grp_click(list("cat_na"), list("(Missing)")),
        tg_grp_cfg("missing", "drop"),
        tg_grp_cfg("missing", "keep")
      )
    ),
    grp_format = list(args = list(
      layout = "groups", by = "cat", format = "compact", unit = "CHF",
      summaries = list(
        list(type = "simple", func = "sum", col = "num", show = "number"),
        list(type = "simple", func = "mean", col = "num2", show = "bar")
      )
    )),
    grp_format_digits = list(args = list(
      layout = "groups", by = "cat", format = 3,
      summaries = list(list(type = "simple", func = "mean", col = "num",
                            show = "number"))
    )),
    grp_compact = list(args = list(
      layout = "groups", by = c("grp", "id"), look = "compact",
      summaries = list(list(type = "simple", func = "count", show = "tile",
                            facet = "cat", fill = "grade",
                            facet_order = "count"))
    )),
    grp_cards = list(args = list(
      layout = "groups", by = "grp", look = "cards", unit = "kg",
      summaries = list(
        list(type = "simple", func = "sum", col = "num", show = "bar"),
        list(type = "simple", func = "count", show = "number"),
        list(type = "spans", x = "day", xend = "end")
      )
    )),
    grp_cards_no_by = list(args = list(
      layout = "groups", look = "cards",
      summaries = list(list(type = "simple", func = "mean", col = "num",
                            show = "number"))
    )),
    grp_drill = list(
      args = list(layout = "groups", by = "cat", drill = "auto"),
      steps = list(
        tg_grp_click(list("cat"), list("b")),
        tg_grp_click(list("cat"), list("b"), nonce = 2),
        tg_grp_click(list(), list(), nonce = 3),
        tg_grp_click(list("cat"), list("c"), nonce = 4),
        tg_grp_cfg("drill", ""),
        tg_grp_cfg("drill", "cat"),
        tg_grp_cfg("by", list())
      )
    ),
    grp_drill_restore = list(args = list(
      layout = "groups", by = "cat", drill = "cat",
      selection = list(column = "cat", values = list("a"))
    )),
    grp_drill_target = list(
      args = list(layout = "groups", by = "cat", drill = "cat",
                  drill_target = "vf1", drill_table = "adsl"),
      steps = list(
        tg_grp_click(list("cat"), list("b"), nonce = 1),
        tg_grp_click(list("cat"), list("b"), nonce = 2),
        tg_grp_clear(),
        tg_grp_cfg("ctrl_target", ""),
        tg_grp_click(list("cat"), list("a"), nonce = 3)
      )
    ),
    grp_gear = list(
      args = list(layout = "groups", by = "cat"),
      steps = list(
        tg_grp_cfg("summaries", list(
          list(type = "simple", func = "mean", col = "num", show = "number"),
          list(type = "dist", col = "num", stat = "mean_sd", show = "box")
        )),
        tg_grp_cfg("summaries", list()),
        tg_grp_cfg("by", list("grp", "", "cat")),
        tg_grp_cfg("look", "compact"),
        tg_grp_cfg("look", "cards"),
        tg_grp_cfg("format", 2),
        tg_grp_cfg("unit", " kg "),
        tg_grp_cfg("download", "off"),
        tg_grp_cfg("title", "{n} rows"),
        tg_grp_cfg("subtitle", NULL),
        tg_grp_cfg("caption", ""),
        tg_grp_cfg("ctrl_table", " adsl "),
        tg_grp_cfg("unknown_key", "x"),
        tg_grp_cfg("", "x")
      )
    ),
    grp_titles = list(
      args = list(
        layout = "groups", by = "cat", top_n = 3L,
        title = "Top {@top_n} by {label(@by)}",
        subtitle = "{@func in count, sum} [of {@col}]",
        caption = "{n} rows"
      ),
      data = "df_titled"
    ),
    grp_script = list(
      args = list(
        layout = "groups", by = "kind", drill = "kind",
        summaries = list(list(type = "simple", func = "sum", col = "num",
                              show = "number")),
        script = c("k <- 5",
                   "transform(data, kind = ifelse(num > k, 'big', 'small'))"),
        script_values = list(k = 6)
      ),
      steps = list(
        tg_grp_cfg("sv_k", "8"),
        tg_grp_click(list("kind"), list("big")),
        tg_grp_cfg("script", "stop('boom')")
      )
    ),
    grp_custom_error = list(args = list(
      layout = "groups", by = "cat",
      summaries = list(
        list(type = "expr", name = "boom", expr = "no_such_fn(num)"),
        list(type = "custom", name = "Custom",
             fn = "\\(d) dplyr::summarise(d, value = mean(num))")
      )
    )),
    grp_no_input = list(args = list(layout = "groups", by = "cat"),
                        data = "none", inputs_ready = FALSE),
    grp_transport = list(
      args = list(layout = "groups", by = "cat"),
      steps = list(
        tg_grp_ready(1), list(data = "df_changed"), list(data = "df_empty"),
        tg_grp_ready(2), list(data = "df_no_cat"), list(data = "df")
      )
    ),
    grp_download = list(
      args = list(
        layout = "groups", by = "cat", title = "Groups", subtitle = "Sub",
        caption = "Cap",
        summaries = list(
          list(type = "simple", func = "count", show = "bar"),
          list(type = "simple", func = "mean", col = "num", show = "number"),
          list(type = "dist", col = "num2", show = "box")
        )
      ),
      steps = list(
        tg_dl("groups-dl_xlsx"), tg_dl("groups-dl_html"), tg_dl("groups-dl_pptx"),
        tg_dl("groups-dl_png"), tg_grp_capture(), tg_dl("groups-dl_png"),
        tg_dl("groups-dl_pptx"), tg_grp_cfg("download", "off")
      )
    ),
    grp_download_cards = list(
      args = list(layout = "groups", by = "grp", look = "cards",
                  summaries = list(list(type = "simple", func = "sum",
                                        col = "num", show = "number"))),
      steps = list(tg_dl("groups-dl_xlsx"), tg_dl("groups-dl_html"),
                   tg_dl("groups-dl_pptx"))
    ),

    # --- variables layout ----------------------------------------------------
    var_default = list(args = list(
      layout = "variables", by = "grp",
      summaries = list(
        list(type = "dist", col = "num", show = "box"),
        list(type = "simple", func = "count", show = "bar", color = "cat",
             name = "cat")
      )
    )),
    var_options = list(args = list(
      layout = "variables", by = "grp", id = "id",
      stats = c("mean_sd", "median_q1_q3", "min_max"),
      sections = "fac", total = TRUE, total_label = "All",
      summaries = list(
        list(type = "simple", func = "count", show = "bar", color = "num",
             name = "num"),
        list(type = "simple", func = "count", show = "bar", color = "cat",
             name = "cat")
      )
    )),
    var_nest = list(args = list(
      layout = "variables", by = "grp", nest = TRUE,
      summaries = list(list(type = "simple", func = "count", show = "bar",
                            color = "cat", name = "cat"))
    )),
    var_missing = list(
      args = list(
        layout = "variables", by = "grp", missing = "level",
        summaries = list(list(type = "simple", func = "count", show = "bar",
                              color = "cat_na", name = "cat_na"))
      ),
      steps = list(tg_rec_cfg("missing", "drop"))
    ),
    var_none = list(args = list(layout = "variables", by = "grp")),
    var_no_input = list(args = list(layout = "variables", by = "grp"),
                        data = "none", inputs_ready = FALSE),
    var_widget = list(
      args = list(layout = "variables"),
      steps = list(
        tg_var_widget(vars = list("num", "cat"), by = list("grp")),
        tg_var_widget(vars = list("num", "cat"), by = list("grp"),
                   stats = list("median_q1_q3", "bogus"), add_overall = TRUE,
                   overall_label = "Everyone", id_var = "id",
                   sections = list("fac"), nest_hierarchies = TRUE),
        tg_var_widget(vars = list("cat"), by = list(), stats = list())
      )
    ),
    var_drill = list(
      args = list(
        layout = "variables", by = "grp", drill = "auto",
        summaries = list(list(type = "simple", func = "count", show = "bar",
                              color = "cat", name = "cat"))
      ),
      steps = list(
        tg_rec_flt(filters = list(list(column = ".variable", value = "cat"),
                               list(column = ".variable_level",
                                    value = "b")),
                col_filters = list(list(column = "grp",
                                        values = list("g1"))),
                nonce = 1),
        tg_rec_flt(column = NULL, values = list())
      )
    ),
    var_titles_slots = list(args = list(
      layout = "variables", by = "grp", title = "By {@by}",
      subtitle = "{@stats}", caption = "{n} rows",
      summaries = list(list(type = "dist", col = "num", show = "box"))
    )),
    var_cards = list(args = list(
      layout = "variables", by = "grp", look = "cards",
      summaries = list(list(type = "dist", col = "num", show = "box"))
    )),
    var_download = list(
      args = list(layout = "variables", by = "grp", title = "Table 1",
                  total = TRUE,
                  summaries = list(list(type = "dist", col = "num",
                                        show = "box"))),
      steps = list(tg_dl("records-dl_xlsx"), tg_dl("records-dl_html"),
                   tg_dl("records-dl_pptx"))
    ),

    # --- layout switch -------------------------------------------------------
    layout_switch = list(
      args = list(rowname = "cat", by = "grp"),
      steps = list(
        tg_rec_cfg("layout", "groups"),
        tg_grp_cfg("summaries", list(
          list(type = "dist", col = "num", show = "box"),
          list(type = "simple", func = "count", show = "bar", color = "cat",
               name = "cat")
        )),
        tg_grp_cfg("layout", "variables"),
        tg_rec_cfg("layout", "records"),
        tg_rec_cfg("layout", "tables")
      )
    ),
    layout_switch_seed = list(
      args = list(),
      steps = list(tg_rec_cfg("layout", "groups"), tg_grp_cfg("layout", "records"),
                   tg_rec_cfg("layout", "variables"))
    )
  )
}

# Constructor calls recorded without a server: what the constructor heals,
# warns about or refuses, and the state it starts from.
table_golden_ctor_cases <- function() {
  list(
    default = list(),
    bad_layout = list(layout = "group"),
    bad_look = list(look = "grid"),
    bad_pct_of = list(pct_of = "row"),
    bad_missing = list(missing = "keep"),
    bad_format = list(format = "fancy"),
    format_clamp = list(format = 9),
    format_string_number = list(format = "2"),
    sort_label = list(layout = "groups", sort_by = "label"),
    groups_seed = list(layout = "groups"),
    max_height_legacy = list(max_height = "600px"),
    list_healing = list(by = list(), rowname = list(), columns = list(),
                        color = list(), top_n = list(), drill = list(),
                        title = list(), unit = list("kg")),
    legacy_value = list(value = c("num", "num2")),
    legacy_group = list(group = "cat"),
    legacy_digits = list(digits = 3),
    legacy_compact = list(compact = TRUE),
    legacy_rotate = list(rotate_titles = TRUE),
    legacy_downloads = list(excel_download = FALSE, html_download = FALSE),
    legacy_cell_color = list(cell_color = list(type = "sequential",
                                               columns = "num")),
    legacy_ctrl = list(ctrl_target = "vf1", ctrl_table = "adsl"),
    legacy_filter = list(filter_column = "cat", filter_values = list("a")),
    agg_summaries = list(summaries = list(list(func = "mean",
                                               cols = list("num")))),
    old_summary_keys = list(
      layout = "groups",
      summaries = list(
        list(type = "dist", col = "num", stat = "mean_sd"),
        list(type = "pair", from = "num2", to = "num", lo = 1, hi = 9),
        list(type = "spans", x = "day", xend = "end", fields = "id")
      )
    ),
    unknown_arg = list(colour = "grp"),
    block_attrs = list(block_name = "My table"),
    superseded_summarize = list(.fn = "new_summarize_table_block",
                                group = "cat", value = "num",
                                func = "mean"),
    superseded_summary = list(.fn = "new_summary_table_block",
                              vars = c("num", "cat"), by = "grp",
                              add_overall = TRUE)
  )
}

# --- restore ------------------------------------------------------------------

# Saved boards: fixtures/tile-board-main.json (four tiles) and
# fixtures/table-golden/old-boards.json (the old table, summarize table and
# summary table blocks), both saved by blockr.viz 0.2.223.
table_golden_restore_cases <- function() {
  old <- function(id, data = "df") {
    list(file = c("table-golden", "old-boards.json"), id = id, data = data)
  }
  tile <- function(id) {
    list(file = "tile-board-main.json", id = id, data = "tile")
  }
  list(
    tile_by_region = tile("by_region"),
    tile_totals = tile("totals"),
    tile_kpis = tile("kpis"),
    tile_wide = tile("wide"),
    old_table_plain = old("table_plain"),
    old_table_full = old("table_full"),
    old_table_shadings = old("table_shadings"),
    old_table_group_agg = old("table_group_agg"),
    old_table_agg_total = old("table_agg_total"),
    old_table_ctrl = old("table_ctrl"),
    old_summarize_ranked = old("summarize_ranked"),
    old_summarize_ranked_count = old("summarize_ranked_count"),
    old_summarize_ranked_fields = old("summarize_ranked_fields"),
    old_summarize_summaries = old("summarize_summaries"),
    old_summarize_script = old("summarize_script"),
    old_summary_flat = old("summary_flat"),
    old_summary_sections = old("summary_sections"),
    old_summary_interim = old("summary_interim")
  )
}

table_golden_saved_block <- function(case) {
  path <- do.call(testthat::test_path, as.list(c("fixtures", case$file)))
  ser <- blockr.core:::read_json(path)
  ser$payload$blocks$payload[[case$id]]
}

# --- the public vocabulary ----------------------------------------------------
#
# Old public name -> new public name (api-proposal.md, section 2 and 4). The
# recordings are written with the reference's names; when the installed
# blockr.viz has the new ones, the scenarios' arguments and the recorded
# state are translated before they are compared. `where`: an argument of
# new_table_block(), a key of a summary entry, or an argument of
# summary_table() in the emitted code. Value changes that cannot be
# translated mechanically are listed with `new = NA`: a recording that
# depends on them needs a decision.
table_golden_renames <- function() {
  data.frame(
    where = c("arg", "arg", "summary", "summary", "summary",
              "arg", "arg",
              "summary_table", "summary_table", "summary_table",
              "summary_table", "summary_table",
              "table_exhibit", "table_exhibit", "table_exhibit"),
    old = c("pct_of", "axis", "dash", "ref_lo", "ref_hi",
            "sort_dir", "color",
            "add_overall", "overall_label", "subject_var",
            "nest_hierarchies", "missing",
            "value", "group", "digits"),
    new = c("percent_of", "column_axis", "linetype", "ref_min", "ref_max",
            NA, "color",
            "total", "total_label", "id", "nest", "missing",
            "columns", "by", NA),
    note = c("", "", "pair summary", "pair summary", "pair summary",
             paste("default \"desc\" becomes NULL: desc for sort_by =",
                   "\"value\", else asc; recordings that rely on the",
                   "default for alpha or data order need a decision"),
             paste("value change: NULL (auto) becomes \"auto\", \"\" (off)",
                   "becomes NULL"),
             "", "", "", "",
             "default \"drop\" becomes \"level\"",
             "", "", "goes; format covers it"),
    stringsAsFactors = FALSE
  )
}

# Which renames the installed blockr.viz has made. Section 2 of the proposal
# is one step (all argument and summary-key renames together), detected by
# `percent_of`; section 4 (summary_table()'s arguments) by `total`.
table_golden_vocab <- function() {
  f <- names(formals(blockr.viz::new_table_block))
  s <- names(formals(blockr.viz::summary_table))
  list(args = "percent_of" %in% f, summary_table = "total" %in% s)
}

table_golden_translate <- function(x, vocab = table_golden_vocab()) {
  rn <- table_golden_renames()
  rn <- rn[!is.na(rn$new) & rn$old != rn$new, ]
  arg_map <- if (vocab$args) {
    stats::setNames(rn$new[rn$where == "arg"], rn$old[rn$where == "arg"])
  }
  sum_map <- if (vocab$args) {
    stats::setNames(rn$new[rn$where == "summary"],
                    rn$old[rn$where == "summary"])
  }
  st_map <- if (vocab$summary_table) {
    stats::setNames(rn$new[rn$where == "summary_table"],
                    rn$old[rn$where == "summary_table"])
  }
  if (!length(arg_map) && !length(st_map)) return(x)
  ren <- function(nm, map) {
    hit <- nm %in% names(map)
    nm[hit] <- unname(map[nm[hit]])
    nm
  }
  color_value <- function(v) {
    if (is.null(v)) "auto" else if (identical(v, "")) NULL else v
  }
  walk_call <- function(e) {
    if (!is.call(e)) return(e)
    fn <- deparse(e[[1L]])
    if (length(st_map) && fn %in% c("summary_table",
                                    "blockr.viz::summary_table") &&
          !is.null(names(e))) {
      names(e) <- ren(names(e), st_map)
    }
    for (i in seq_along(e)[-1L]) {
      if (!is.null(e[[i]])) e[[i]] <- walk_call(e[[i]])
    }
    e
  }
  state <- function(s) {
    if (!is.list(s) || is.null(names(s))) return(s)
    if (length(arg_map) && "color" %in% names(s)) {
      s["color"] <- list(color_value(s[["color"]]))
    }
    if (length(sum_map) && is.list(s[["summaries"]])) {
      s$summaries <- lapply(s$summaries, function(e) {
        if (is.list(e) && !is.null(names(e))) names(e) <- ren(names(e), sum_map)
        e
      })
    }
    if (length(arg_map)) names(s) <- ren(names(s), arg_map)
    s[order(names(s))]
  }
  walk <- function(r) {
    if (is.language(r)) return(walk_call(r))
    if (!is.list(r) || is.data.frame(r)) return(r)
    for (k in intersect(names(r), c("state", "initial", "saved", "payload",
                                    "args"))) {
      r[[k]] <- state(r[[k]])
    }
    if (!is.null(r$expr)) r$expr <- walk_call(r$expr)
    for (k in setdiff(names(r) %||% seq_along(r),
                      c("state", "initial", "saved", "payload", "args",
                        "expr", "messages", "outputs"))) {
      if (is.list(r[[k]])) r[[k]] <- walk(r[[k]])
    }
    r
  }
  walk(x)
}

# --- running ------------------------------------------------------------------

# Called by name: blockr.core reads the constructor off the call.
table_golden_ctor <- function(args) {
  fn <- args$.fn %||% "new_table_block"
  args$.fn <- NULL
  eval(as.call(c(list(as.name(fn)), args)))
}

# The mock session's id counts sessions per process ("proxy3-expr-..."), so
# it depends on what ran before. Recorded as "blk-". Download names carry
# the minute they were written.
table_golden_scrub <- function(x) {
  if (is.character(x)) {
    a <- attributes(x)
    x <- gsub("proxy[0-9]+-", "blk-", x)
    # Ids minted per render.
    x <- gsub("(blockr-(html-table|dt))-[0-9a-f]{6,}(?![0-9a-z])", "\\1-<id>",
              x, perl = TRUE)
    x <- gsub("_[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{4}\\.", "_<time>.", x)
    x <- gsub("(cap-[A-Za-z0-9_-]*-)[0-9]+", "\\1<n>", x)
    attributes(x) <- a
    return(x)
  }
  if (is.list(x) && !is.data.frame(x)) {
    a <- attributes(x)
    x <- lapply(x, table_golden_scrub)
    attributes(x) <- a
  }
  x
}

table_golden_value <- function(f) {
  tryCatch(f(), error = function(e) {
    structure(list(message = conditionMessage(e)), class = "golden_error")
  })
}

# Custom messages, with a JSON payload parsed so a diff reads.
table_golden_message <- function(m) {
  msg <- m$message
  if (is.list(msg) && is.character(msg$payload) && length(msg$payload) == 1L) {
    msg$payload <- jsonlite::fromJSON(msg$payload, simplifyVector = FALSE)
  }
  list(type = m$type, message = msg)
}

# The messages of one step. They reach the browser in one batch, and the
# order between messages of different types follows Shiny's observer order,
# which varies between runs; sorted by type, in order within a type.
table_golden_messages <- function(msgs, from) {
  if (length(msgs) < from) return(list())
  m <- lapply(msgs[from:length(msgs)], table_golden_message)
  types <- vapply(m, `[[`, "", "type")
  m[order(types, seq_along(types), method = "radix")]
}

# The rendered outputs a browser receives, as HTML text.
table_golden_outputs <- c(
  "layout_css", "records-dt_result", "records-dt_status",
  "records-dt_download", "groups-summarize_chrome",
  "groups-summarize_download"
)

table_golden_conditions <- function(cnd) {
  lapply(cnd, function(x) {
    vapply(x, function(c) {
      if (inherits(c, "condition")) conditionMessage(c) else
        paste(format(c), collapse = " ")
    }, character(1L))
  })
}

table_golden_step_record <- function(session, expr, sent, sends, msg_from) {
  st <- session$returned$state
  state <- lapply(st, function(rv) table_golden_value(rv))
  list(
    state = state[order(names(state))],
    expr = table_golden_value(function() session$returned$expr()),
    result = table_golden_value(function() {
      r <- session$returned$result()
      if (is.data.frame(r)) as.data.frame(r) else r
    }),
    conditions = table_golden_value(function() {
      table_golden_conditions(session$returned$cond())
    }),
    messages = table_golden_messages(sent$msgs, msg_from),
    outputs = lapply(stats::setNames(nm = table_golden_outputs), function(o) {
      v <- table_golden_value(function() expr$output[[o]])
      if (is.list(v) && !is.null(v$html)) {
        table_golden_strip(as.character(v$html))
      } else {
        v
      }
    }),
    ctrl = sends$log
  )
}

# A downloaded file: its name, and what it holds. The spreadsheet as cell
# values and cell styles per sheet; the web page as its body without
# scripts and styles; a deck or a picture only as written, because they
# depend on the machine's fonts and zip times.
table_golden_download <- function(output, id) {
  path <- table_golden_value(function() output[[id]])
  if (inherits(path, "golden_error") || !is.character(path) ||
        !file.exists(path)) {
    return(path)
  }
  ext <- tools::file_ext(path)
  content <- switch(ext,
    xlsx = table_golden_xlsx(path),
    html = table_golden_html(path),
    paste("written:", ext)
  )
  list(file = basename(path), content = content)
}

table_golden_xlsx <- function(path) {
  wb <- openxlsx::loadWorkbook(path)
  sheets <- openxlsx::sheets(wb)
  list(
    values = lapply(stats::setNames(nm = sheets), function(s) {
      openxlsx::read.xlsx(path, sheet = s, colNames = FALSE,
                          skipEmptyRows = FALSE, skipEmptyCols = FALSE)
    }),
    styles = lapply(wb$styleObjects, function(so) {
      s <- so$style
      list(sheet = so$sheet, rows = so$rows, cols = so$cols,
           fill = s$fill, font = s$fontColour, bold = s$fontDecoration,
           num_fmt = s$numFmt, halign = s$halign, indent = s$indent,
           border = s$borderBottom)
    }),
    merged = lapply(stats::setNames(nm = sheets), function(s) {
      wb$worksheets[[match(s, sheets)]]$mergeCells
    })
  )
}

table_golden_html <- function(path) {
  x <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"),
             collapse = "\n")
  x <- sub("(?s).*<body[^>]*>", "", x, perl = TRUE)
  x <- sub("(?s)</body>.*", "", x, perl = TRUE)
  table_golden_strip(x)
}

# Markup without its stylesheets and scripts: where the CSS and JS live is
# not what a reader sees.
table_golden_strip <- function(x) {
  x <- gsub("(?s)<script.*?</script>", "", x, perl = TRUE)
  x <- gsub("(?s)<style.*?</style>", "", x, perl = TRUE)
  x
}

# The session's custom messages and control-channel sends, kept instead of
# sent.
table_golden_mock <- function(session) {
  sent <- new.env(parent = emptyenv())
  sent$msgs <- list()
  root <- session$rootScope()
  root$sendCustomMessage <- function(type, message) {
    sent$msgs <- c(sent$msgs, list(list(type = type, message = message)))
    invisible(NULL)
  }
  sends <- new.env(parent = emptyenv())
  sends$log <- list()
  log <- function(op) {
    function(target, args, author = NULL) {
      sends$log <- c(sends$log, list(list(
        op = op, target = target, args = args, author = author
      )))
      invisible(TRUE)
    }
  }
  session$userData$blockr_ctrl_send <- log("send")
  session$userData$blockr_ctrl_clear <- log("clear")
  list(sent = sent, sends = sends)
}

table_golden_run <- function(sc) {
  table_golden_register_tbl()
  withr::local_options(utils::modifyList(
    list(blockr.viz.default_caption = NULL, useFancyQuotes = FALSE),
    sc$options %||% list()
  ))
  src <- new.env(parent = emptyenv())
  src$d <- table_golden_data(sc$data %||% "df")
  tick <- shiny::reactiveVal(0L)
  dat <- shiny::reactive({
    tick()
    src$d
  })
  out <- new.env(parent = emptyenv())
  out$ctor_warnings <- character()
  out$steps <- list()
  args <- table_golden_translate(list(args = sc$args))$args
  blk <- withCallingHandlers(
    table_golden_ctor(args),
    warning = function(w) {
      out$ctor_warnings <- c(out$ctor_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  initial <- blockr.core:::initial_block_state(blk)
  saved <- blockr.core::blockr_ser(blk)$payload

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      m <- table_golden_mock(session)
      sent <- m$sent
      sends <- m$sends
      expr <- session$makeScope("expr")

      from <- 1L
      session$flushReact()
      out$steps[[1L]] <- table_golden_step_record(session, expr, sent, sends,
                                                  from)
      from <- length(sent$msgs) + 1L

      for (i in seq_along(sc$steps)) {
        s <- sc$steps[[i]]
        rec_extra <- NULL
        if (!is.null(s$input)) {
          do.call(expr$setInputs, stats::setNames(list(s$value), s$input))
        } else if (!is.null(s$data)) {
          src$d <- table_golden_data(s$data)
          tick(i)
        } else if (!is.null(s$download)) {
          session$flushReact()
          rec_extra <- table_golden_download(expr$output, s$download)
        }
        session$flushReact()
        rec <- table_golden_step_record(session, expr, sent, sends, from)
        rec$download <- rec_extra
        out$steps[[i + 1L]] <- rec
        from <- length(sent$msgs) + 1L
      }
    },
    args = c(
      list(x = blk, data = list(data = dat)),
      if (isFALSE(sc$inputs_ready)) {
        list(inputs_ready = shiny::reactive(FALSE))
      }
    )
  )
  table_golden_scrub(list(
    ctor_warnings = out$ctor_warnings,
    initial = initial[order(names(initial))],
    saved = saved[order(names(saved))],
    steps = out$steps
  ))
}

table_golden_ctor_run <- function(args) {
  withr::local_options(list(useFancyQuotes = FALSE,
                            blockr.viz.default_caption = NULL))
  args <- table_golden_translate(list(args = args))$args
  warns <- new.env(parent = emptyenv())
  warns$msg <- character()
  res <- tryCatch(
    withCallingHandlers(
      table_golden_ctor(args),
      warning = function(w) {
        warns$msg <- c(warns$msg, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      structure(list(message = conditionMessage(e)), class = "golden_error")
    }
  )
  if (!inherits(res, "golden_error")) {
    initial <- blockr.core:::initial_block_state(res)
    ser <- blockr.core::blockr_ser(res)
    back <- blockr.core::blockr_ser(blockr.core::blockr_deser(ser))
    res <- list(
      initial = initial[order(names(initial))],
      saved = ser$payload[order(names(ser$payload))],
      constructor = ser$constructor[c("constructor", "package")],
      class = class(res),
      roundtrip = identical(back$payload, ser$payload),
      ui = as.character(res$expr_ui("blk"))
    )
  }
  table_golden_scrub(list(warnings = warns$msg, value = res))
}

# A saved block restored, then run for one flush: the warnings, the state it
# saves as, and what the browser gets first.
table_golden_restore_run <- function(case) {
  withr::local_options(list(useFancyQuotes = FALSE,
                            blockr.viz.default_caption = NULL))
  ser <- table_golden_saved_block(case)
  warns <- character()
  blk <- withCallingHandlers(
    blockr.core::blockr_deser(ser),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  saved <- blockr.core::blockr_ser(blk)
  run <- table_golden_run_block(blk, case$data)
  table_golden_scrub(list(
    warnings = warns,
    class = class(blk),
    saved = saved$payload[order(names(saved$payload))],
    constructor = saved$constructor[c("constructor", "package")],
    steps = run
  ))
}

table_golden_run_block <- function(blk, data) {
  d <- table_golden_data(data)
  dat <- shiny::reactive(d)
  rec <- NULL
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      sent <- new.env(parent = emptyenv())
      sent$msgs <- list()
      root <- session$rootScope()
      root$sendCustomMessage <- function(type, message) {
        sent$msgs <- c(sent$msgs, list(list(type = type, message = message)))
        invisible(NULL)
      }
      sends <- new.env(parent = emptyenv())
      sends$log <- list()
      session$userData$blockr_ctrl_send <- function(...) invisible(TRUE)
      expr <- session$makeScope("expr")
      session$flushReact()
      rec <<- list(table_golden_step_record(session, expr, sent, sends, 1L))
    },
    args = list(x = blk, data = list(data = dat))
  )
  rec
}

table_golden_dir <- function() {
  testthat::test_path("fixtures", "table-golden")
}
