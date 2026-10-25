#' Summarize table block (superseded)
#'
#' Superseded by [new_table_block()] with `layout = "groups"`, which takes the
#' same summaries. This constructor translates its arguments and returns a
#' table block, so code and boards that use it keep working.
#'
#' @param ... Arguments of the former summarize table block (`by`,
#'   `summaries`, `id_var`, `group`, `value`, `func`, ...), translated to
#'   [new_table_block()]'s.
#'
#' @return A blockr transform block of class `table_block`.
#' @examplesIf interactive()
#' new_summarize_table_block(
#'   by = "Species",
#'   summaries = list(list(type = "simple", func = "count", show = "bar"))
#' )
#' @export
new_summarize_table_block <- function(...) {
  args <- table_legacy_args(list(...), from = "summarize")
  do.call(new_table_block, args)
}


#' Argument specs for the lane chart block
#' @noRd
summarize_arguments <- function() {
  blockr.core::new_arg_specs(
    group = new_arg_spec(
      paste0(
        "Column giving the rows: one row per level. The one required ",
        "argument (e.g. AEDECOD for most-frequent adverse events, USUBJID ",
        "for a per-subject swimlane)."
      ),
      example = "AEDECOD",
      type = arg_string()
    ),
    summaries = new_arg_spec(
      paste0(
        "The summarize-table mode: an ordered list of summary columns, one ",
        "object per column; non-empty it REPLACES the flat bar ",
        "mappings. Each object: type = simple (func + col, shown as bar, ",
        "number or dot), dist (col + stat, optional whiskers, shown as box / ",
        "pointrange / text), field (col: the group's distinct values ",
        "joined, a group-level fact), series (x + col + optional ",
        "band = [lo, hi] data columns + optional computed ref = mean / ",
        "mean_sd / median_iqr (a pooled orientation line/band): a ",
        "sparkline), spans (x + xend + optional color + optional label -- ",
        "the event-identity column, e.g. AEDECOD, headlining each ",
        "segment's hover tooltip and keying the same-event highlight -- + ",
        "optional fields (extra tooltip columns) + size = md / lg (a ",
        "wider column for the exhibit case): a swimlane), pair (from + to ",
        "columns, each summarised by from_func / to_func = identity / mean ",
        "/ median / min / max, defaults identity and max; optional lo / hi ",
        "= a number or a column for a shaded range, a `to` outside it drawn ",
        "open; optional ref = a number drawn as a dashed line; optional ",
        "dash = a categorical column whose non-first levels draw the link ",
        "dashed: a dumbbell per row, e.g. baseline to peak per subject), ",
        "expr (free R ",
        "code over the ",
        "group's rows, text), custom (fn = a function of the rows, ",
        "`\\(d) ...`, returning a data frame whose columns name what they ",
        "are: value; from, to; lo, q1, mid, q3, hi; text. One other column ",
        "is the split, one mark per level. The function runs once per level ",
        "of `by` and per facet level, with `d` grouped by the table row. ",
        "Keep the grouping: use dplyr verbs, add groups with ",
        "group_by(..., .add = TRUE), never d$col. A per-patient reduction ",
        "such as the worst grade then nests correctly, e.g. `\\(d) d |> ",
        "dplyr::arrange(dplyr::desc(GRADE)) |> dplyr::distinct(USUBJID, ",
        ".keep_all = TRUE) |> dplyr::count(GRADE, name = \"value\")`. ",
        "show picks the ",
        "mark: bar, number, dumbbell, box, pointrange, text; read off the ",
        "returned columns when absent; denom = a column whose distinct ",
        "values are the N, so a count prints \"54 (18%)\" and its header ",
        "N). ",
        "Optional per object: name (the column header), show, color (a ",
        "categorical column: the cell is split into one glyph or dumbbell ",
        "per level, a ",
        "bar into segments, a swimlane's events tinted) and facet (a ",
        "categorical column: the column repeats once per level, on one ",
        "shared scale). Colour and facet are per COLUMN, not per table, so ",
        "one column may split by severity while the next repeats per sex ",
        "and a third carries neither; map them only where they answer the ",
        "question. A field column never facets. `scope` is the retired ",
        "table-facet switch, accepted for saved boards only."
      ),
      example = list(
        list(type = "simple", name = "Subjects", func = "count_distinct",
             col = "USUBJID", show = "bar"),
        list(type = "dist", name = "Duration", col = "DUR",
             stat = "median_q1_q3", show = "box"),
        list(type = "field", name = "Arm", col = "TRT01A")
      ),
      type = arg_array(arg_object(
        type = arg_enum(c("simple", "dist", "field", "series", "spans",
                          "pair", "expr", "custom")),
        name = arg_string(required = FALSE),
        show = arg_string(required = FALSE),
        scope = arg_enum(c("cell", "pooled"), required = FALSE),
        func = arg_string(required = FALSE),
        col = arg_string(required = FALSE),
        stat = arg_string(required = FALSE),
        whiskers = arg_string(required = FALSE),
        x = arg_string(required = FALSE),
        xend = arg_string(required = FALSE),
        color = arg_string(required = FALSE),
        facet = arg_string(required = FALSE),
        band = arg_array(arg_string(), required = FALSE),
        # A series row takes one of none / mean / mean_sd / median_iqr; a
        # pair row takes a number (its dashed reference line), as a string.
        ref = arg_string(required = FALSE),
        from = arg_string(required = FALSE),
        to = arg_string(required = FALSE),
        from_func = arg_enum(c("identity", "mean", "median", "min", "max"),
                             required = FALSE),
        to_func = arg_enum(c("identity", "mean", "median", "min", "max"),
                           required = FALSE),
        # Band bounds of a pair row: a column name, or a number as a string.
        lo = arg_string(required = FALSE),
        hi = arg_string(required = FALSE),
        dash = arg_string(required = FALSE),
        label = arg_string(required = FALSE),
        fields = arg_array(arg_string(), required = FALSE),
        size = arg_enum(c("md", "lg"), required = FALSE),
        parent = arg_boolean(required = FALSE),
        expr = arg_string(required = FALSE),
        fn = arg_string(required = FALSE),
        denom = arg_string(required = FALSE)
      ))
    ),
    by = new_arg_spec(
      paste0(
        "Grouping columns for the summaries mode, outer to inner (at most ",
        "two; the outer becomes the expandable parent). One table row per ",
        "key combination."
      ),
      example = list("AEBODSYS", "AEDECOD"),
      type = arg_array(arg_string())
    ),
    facet_layout = new_arg_spec(
      paste0(
        "Facet column order in the summaries mode: by_summary = each ",
        "summary's level copies adjacent on one shared scale (comparison ",
        "reading, default); by_level = pooled columns and fields lead, ",
        "then one spanning column group per facet level (the Table-1 / ",
        "CSR reading)."
      ),
      example = "by_summary",
      type = arg_enum(c("by_summary", "by_level"))
    ),
    func = new_arg_spec(
      paste0(
        "How the measure is computed: count (rows), count_distinct ",
        "(distinct values of `id_var` -- use this for \"subjects with at ",
        "least one event\", the clinical default), sum / mean / median / ",
        "min / max of `value`, or identity (`value` as-is, no aggregation ",
        "-- for data already reduced upstream, e.g. one value per subject)."
      ),
      example = "count_distinct",
      type = arg_enum(c("count", "count_distinct", "sum", "mean", "median",
                        "min", "max", "identity"))
    ),
    value = new_arg_spec(
      paste0(
        "Numeric column reduced by sum / mean / median / min / max, or shown ",
        "as-is by identity. Unused by count."
      ),
      example = "AVAL",
      type = arg_string()
    ),
    id_var = new_arg_spec(
      paste0(
        "Subject identifier counted by count_distinct, so one subject with ",
        "three events counts once."
      ),
      example = "USUBJID",
      type = arg_string()
    ),
    parent = new_arg_spec(
      paste0(
        "Optional outer grouping column (e.g. AEBODSYS over AEDECOD): ",
        "parents become expandable rows with their children indented under ",
        "them. Each level is aggregated in its own pass, so a parent is ",
        "never the sum of its children."
      ),
      example = "AEBODSYS",
      type = arg_string()
    ),
    color = new_arg_spec(
      paste0(
        "Optional column splitting each bar into segments (e.g. AESEV). ",
        "Composes with `facet`: each facet column's bars are then split by ",
        "this column."
      ),
      example = "AESEV",
      type = arg_string()
    ),
    bar_mode = new_arg_spec(
      paste0(
        "Layout of a colour split: stacked (segments to scale), grouped ",
        "(one thin bar per level, side by side) or percent (each row ",
        "normalized to 100%). No-op without `color`. Stacking needs an ",
        "additive measure (count, count distinct, sum); a mean or median ",
        "is always grouped, since its parts do not add up."
      ),
      example = "stacked",
      type = arg_enum(c("stacked", "grouped", "percent"))
    ),
    facet = new_arg_spec(
      paste0(
        "Optional column giving one bar column per level, all on one shared ",
        "scale (e.g. TRTA -- one column per treatment arm). Composes with ",
        "`color`."
      ),
      example = "TRTA",
      type = arg_string()
    ),
    cols = new_arg_spec(
      paste0(
        "Opt-in separate numeric columns beside the bar: n, pct, or both. ",
        "Leave unset for the default -- the bar cell carries its own value ",
        "label."
      ),
      example = list("n", "pct"),
      type = arg_array(arg_enum(c("n", "pct")))
    ),
    fields = new_arg_spec(
      paste0(
        "Extra columns from the underlying row, shown as real columns ",
        "beside the bar (the chart's tooltip fields). Only meaningful with ",
        "func = \"identity\", where each group IS one row."
      ),
      example = list("ARM", "AGE"),
      type = arg_array(arg_string())
    ),
    sort_by = new_arg_spec(
      paste0(
        "Ordering: value (the measure), data (the data's own order -- factor ",
        "levels, else first appearance in the rows: use it for visits and dose ",
        "groups, which read wrong alphabetically), label (alphabetical), a ",
        "summary column name, or a facet level name."
      ),
      example = "value",
      type = arg_string()
    ),
    sort_dir = new_arg_spec(
      "Sort direction.",
      example = "desc",
      type = arg_enum(c("desc", "asc"))
    ),
    top_n = new_arg_spec(
      paste0(
        "Optional cap on the number of ranked rows, with a visible fold row ",
        "for what falls below the cut. Leave unset for the default ",
        "behaviour: every row rendered, scrolling with the panel. Set it ",
        "only for report exhibits, where there is no scrollbar."
      ),
      example = 10L,
      type = arg_integer()
    ),
    max_height = new_arg_spec(
      paste0(
        "Leave unset: the table scrolls with its panel. A CSS length ",
        "puts it in a box of that height instead."
      ),
      example = "600px",
      type = arg_string()
    ),
    sortable = new_arg_spec(
      paste0(
        "Allow click-to-sort on the column headers. FALSE freezes the ",
        "configured order -- for exhibits whose row order carries meaning ",
        "(visits, dose groups), where a stray click would scramble it."
      ),
      example = TRUE,
      type = arg_boolean()
    ),
    search = new_arg_spec(
      "Show the search input above the table.",
      example = TRUE,
      type = arg_boolean()
    ),
    axis = new_arg_spec(
      paste0(
        "Print each glyph column's domain as a tick strip under its header ",
        "(default TRUE) -- the scale named once at the top of the column ",
        "instead of a track repeated on every row. Applies to every mark: ",
        "value domain for bars, boxes and dot ranges, x domain (dates as ",
        "dates) for swimlanes and sparklines. FALSE drops them all."
      ),
      example = TRUE,
      type = arg_boolean()
    ),
    value_labels = new_arg_spec(
      paste0(
        "Print each mark's value beside it (default TRUE): a bar's value, a ",
        "box's or dot range's centre, a dumbbell's change, a sparkline's ",
        "last value; a colour-split column prints one number per level. ",
        "FALSE drops them all. Use for \"show the numbers\" / \"hide the ",
        "numbers\"."
      ),
      example = TRUE,
      type = arg_boolean()
    ),
    bar_width = new_arg_spec(
      paste0(
        "Length of the marks that carry a value label (bars, boxes, dot ",
        "ranges, sparklines): fit (default) fills the panel up to 320px; ",
        "narrow, medium and wide fix them at 90, 150 and 240px."
      ),
      example = "fit",
      type = arg_enum(c("narrow", "medium", "wide", "fit"))
    ),
    drill = new_arg_spec(
      paste0(
        "Turns the row click on (any of the grouping columns). A click ",
        "filters downstream to that row's rows: a parent row of a nested ",
        "table on its outer column, a child row on both columns. Same ",
        "filter contract as the chart and table blocks. Empty = display ",
        "only."
      ),
      example = "AEDECOD",
      type = arg_string()
    ),
    ctrl_target = new_arg_spec(
      paste0(
        "BETA. Block id of a value filter block on the SAME board: the ",
        "drill's claim (e.g. AEDECOD = PRURITUS) is also pushed into that ",
        "block over the board's control channel, so a row click filters a ",
        "pipeline this block has no data link to. Requires drill to be on ",
        "and the board to carry the control bridge extension. Empty ",
        "(default) = off. \"auto\" needs no id: it is the board's drill ",
        "destination, found by class -- the one drill filter block, else ",
        "the one value filter block. Prefer it over a typed id."
      ),
      example = "cohort_filter",
      type = arg_string()
    ),
    ctrl_table = new_arg_spec(
      paste0(
        "BETA. Only with `ctrl_target`: the table in the target's dm the ",
        "pushed conditions apply to (e.g. \"adae\"). Leave empty when the ",
        "target filters a plain data frame."
      ),
      example = "adae",
      type = arg_string()
    ),
    title = new_arg_spec(
      paste0(
        "Title above the table. Unset = auto (inherits the input data ",
        "frame's label attribute when present); \"\" = explicitly none. ",
        "Supports {...} tokens resolved against the CURRENT data on every ",
        "render: {col} = the distinct values of that column, {label(col)} = ",
        "the column's variable label, {n} = row count, {n_distinct(col)} = ",
        "distinct count. Tokens are data lookups, never code."
      ),
      example = "Most frequent adverse events \u2014 {STUDYID}",
      type = arg_string()
    ),
    subtitle = new_arg_spec(
      "Subtitle under the title, same {...} tokens as `title`.",
      example = "N = {n_distinct(USUBJID)} subjects",
      type = arg_string()
    ),
    caption = new_arg_spec(
      "Caption under the table (source / footnote line), same {...} tokens.",
      example = "Source: ADAE",
      type = arg_string()
    )
  )
}

#' Construction guidance for the lane chart block
#' @noRd
summarize_guidance <- function() {
  paste(
    "Horizontal marks as an HTML table \u2014 the third sibling of the",
    "chart and table blocks. Use it whenever one row is one category and",
    "the mark is a horizontal glyph on a shared scale: most frequent",
    "adverse events (bar), AE duration by term (box / pointrange), an",
    "AE-episode swimlane per subject (interval: x/xend, color = severity),",
    "or a lab trajectory per subject (sparkline: value over x, lo/hi band).",
    "It beats the canvas chart for many-category cases because the table",
    "form carries search, click-to-sort, exact values and an arbitrary row",
    "count, and it beats the plain table because the mark makes the",
    "pattern readable at a glance.",
    "\n- `summaries` + `by` is the config model: an ordered list of typed",
    "summary columns over one grouping (simple / dist / field / series /",
    "spans / expr), each with its own display (bar, number, dot, box,",
    "pointrange, text, swimlane, sparkline) and its own optional `color`",
    "and `facet` mappings -- both per column, never table-wide.",
    "Statistics are computed server-side; mean_ci95 is exact, qt-based.",
    "\n- Without `summaries`, the flat arguments below give the original",
    "ranked-bar table.",
    "\n- `group` is the only required argument. `func = \"count_distinct\"`",
    "with `id_var` is the clinical default (subjects, not events).",
    "\n- `func = \"identity\"` ranks a pre-computed value as-is (one row per",
    "group upstream, e.g. one value per subject with `group` the subject id),",
    "exactly like the chart block's \"None (as is)\" aggregation. With it,",
    "`fields` adds columns of the underlying row beside the bar (the chart's",
    "tooltip fields, as real columns).",
    "\n- `parent` adds an expandable second level (AEBODSYS over AEDECOD).",
    "Each level is aggregated separately, so a parent is never the sum of",
    "its children.",
    "\n- `color` splits each bar into segments (severity); `facet` gives one",
    "bar column per level (treatment arm) on a shared scale. They COMPOSE:",
    "both set = one column per facet level, each bar split by colour.",
    "\n- `drill` makes a row click filter downstream blocks.",
    "\n- Do NOT set `top_n` for an interactive board: the table renders every",
    "row and scrolls, like the table block. It is for report exhibits only.",
    "\nFor vertical columns, lines, scatter or anything with two measures on",
    "axes, use the chart block instead \u2014 this block is horizontal",
    "glyphs in table cells only."
  )
}
