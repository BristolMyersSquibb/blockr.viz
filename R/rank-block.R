#' Rank Block
#'
#' Ranked horizontal bars rendered as an HTML table: the third sibling of
#' [new_chart_block()] and [new_table_block()]. It takes the chart's
#' vocabulary (`group` / `color` / `facet` / `bar_mode` / `sort_by` / `drill`)
#' and the table's rendering (sticky header, client-side search and sort,
#' scroll at `max_height`, exact values in their own columns).
#'
#' Horizontal bars only. That constraint is what keeps the argument surface
#' small: for anything else -- vertical columns, lines, scatter, a second
#' measure on an axis -- use [new_chart_block()].
#'
#' Large inputs behave exactly like the table block: every row is rendered and
#' the container scrolls. `top_n` is opt-in, for report exhibits where there is
#' no scrollbar (a pptx slide wants ten bars, not a hundred), and always draws
#' a visible fold row.
#'
#' @param group Column to rank by: one row per level, ordered by the measure.
#' @param value,func,id_var The measure. `func` is one of `"count"`,
#'   `"count_distinct"`, `"sum"`, `"mean"`, `"median"`, `"min"`, `"max"`;
#'   `value` is the column it reduces (unused by `"count"`); `id_var` is the
#'   subject identifier `"count_distinct"` counts, which is what makes a count
#'   read as "subjects with at least one event" rather than "events".
#' @param parent Optional outer grouping column (e.g. system organ class over
#'   preferred term): parents become expandable rows with children indented
#'   under them. Each level is aggregated in its own pass, so a parent is never
#'   the sum of its children.
#' @param color Optional column splitting each bar into segments.
#' @param bar_mode `"stacked"`, `"grouped"` or `"percent"`; no-op without
#'   `color`.
#' @param facet Optional column giving one bar column per level on a shared
#'   scale (e.g. one column per treatment arm). Takes precedence over `color`.
#' @param compare With `facet`, the level to treat as the comparator: every
#'   other level gets a zero-centred difference bar in percentage points.
#' @param cols Numeric columns beside the bar: any of `"n"`, `"pct"`.
#' @param sort_by,sort_dir Server-side ordering: `"value"`, `"label"` or a
#'   facet level name, and `"desc"` / `"asc"`.
#' @param top_n Optional cap (`NULL` = off, the table scrolls instead).
#' @param max_height CSS max-height of the scroll container.
#' @param search Show the search input.
#' @param title,subtitle,caption Display text. `NULL` = auto (inherits the
#'   input's label / subtitle / caption attribute), `""` = explicitly none,
#'   else a template with the same `{...}` tokens as the chart and table
#'   blocks (see [resolve_title_template()]).
#' @param drill Column a row click filters on. The emitted filter is the same
#'   categorical contract as [new_chart_block()], so existing filter links
#'   compose.
#' @param filter_type,filter_column,filter_values Runtime filter transport,
#'   normally left at defaults: they hold the click state so it survives a
#'   board save and restore.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @return A blockr transform block of class `rank_block`.
#' @examplesIf interactive()
#' new_rank_block(group = "cyl")
#' @export
new_rank_block <- function(group = NULL,
                           value = ".count",
                           func = "count",
                           id_var = NULL,
                           parent = NULL,
                           color = NULL,
                           bar_mode = "stacked",
                           facet = NULL,
                           compare = NULL,
                           cols = c("n", "pct"),
                           sort_by = "value",
                           sort_dir = "desc",
                           top_n = NULL,
                           max_height = "600px",
                           search = TRUE,
                           title = NULL,
                           subtitle = NULL,
                           caption = NULL,
                           drill = NULL,
                           # Runtime filter transport (NOT creation-time
                           # config). MUST stay in the signature: blockr.core
                           # serializes a block from its constructor formals
                           # and restores by re-calling the constructor, so
                           # dropping these breaks filter-state round-trip.
                           filter_type = "categorical",
                           filter_column = NULL,
                           filter_values = NULL,
                           ...) {
  # Heal state poisoned by a pre-#144 DAG copy/paste (a NULL slot returning as
  # list()); see R/state-normalize.R.
  group <- chr_state(group)
  value <- chr_state(value)
  id_var <- chr_state(id_var)
  parent <- chr_state(parent)
  color <- chr_state(color)
  facet <- chr_state(facet)
  compare <- chr_state(compare)
  drill <- chr_state(drill)
  filter_column <- chr_state(filter_column)
  filter_values <- null_state(filter_values)
  cols <- chr_vec_state(cols)
  # NOT chr_state: "" is a real value (explicitly no title, as against NULL =
  # auto). See R/title-template.R.
  title <- title_state(title)
  subtitle <- title_state(subtitle)
  caption <- title_state(caption)

  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        r_group   <- shiny::reactiveVal(group)
        r_value   <- shiny::reactiveVal(value %||% ".count")
        r_func    <- shiny::reactiveVal(func %||% "count")
        r_id_var  <- shiny::reactiveVal(id_var)
        r_parent  <- shiny::reactiveVal(parent)
        r_color   <- shiny::reactiveVal(color)
        r_bar_mode <- shiny::reactiveVal(bar_mode %||% "stacked")
        r_facet   <- shiny::reactiveVal(facet)
        r_compare <- shiny::reactiveVal(compare)
        r_cols    <- shiny::reactiveVal(as.character(cols %||% c("n", "pct")))
        r_sort_by <- shiny::reactiveVal(sort_by %||% "value")
        r_sort_dir <- shiny::reactiveVal(sort_dir %||% "desc")
        r_top_n   <- shiny::reactiveVal(top_n)
        r_max_height <- shiny::reactiveVal(max_height)
        r_search  <- shiny::reactiveVal(isTRUE(search))
        r_title   <- shiny::reactiveVal(title)
        r_subtitle <- shiny::reactiveVal(subtitle)
        r_caption <- shiny::reactiveVal(caption)
        r_drill   <- shiny::reactiveVal(drill)
        r_filter_type <- shiny::reactiveVal(filter_type %||% "categorical")
        r_filter_column <- shiny::reactiveVal(filter_column)
        r_filter_values <- shiny::reactiveVal(filter_values)

        # A composer table (or anything with an as_annotated_df method) is
        # coerced once, like the table block does, so such inputs connect
        # without an explicit conversion step.
        ann_data <- shiny::reactive({
          d <- data()
          if (is.data.frame(d)) d else as_annotated_df(d)
        })

        # Auto-tier title sources: the input's own display attributes.
        r_data_titles <- shiny::reactive({
          d <- tryCatch(ann_data(), error = function(e) NULL)
          if (!is.data.frame(d)) {
            return(list(label = NULL, subtitle = NULL, caption = NULL))
          }
          att <- function(nm) {
            v <- attr(d, nm, exact = TRUE)
            if (is.character(v) && length(v) == 1L && nzchar(v)) v else NULL
          }
          list(label = att("label"), subtitle = att("subtitle"),
               caption = att("caption"))
        })

        board_scale_map <- dd_board_scale_map()

        # The gear's config params, mapped to their reactiveVals. One place, so
        # a new arg needs one line here and one role in rank-table.js.
        setters <- list(
          group = r_group, parent = r_parent, color = r_color,
          facet = r_facet, compare = r_compare, func = r_func,
          value = r_value, id_var = r_id_var, bar_mode = r_bar_mode,
          cols = r_cols, sort_by = r_sort_by, sort_dir = r_sort_dir,
          top_n = r_top_n, drill = r_drill, title = r_title,
          subtitle = r_subtitle, caption = r_caption
        )
        # Column / select roles arrive as strings, and the engine sends "" (or
        # "(none)") for a cleared pick: those become NULL, which is what the
        # renderer reads as "unset". The three text slots keep "" as a real
        # value (explicitly no title) -- the NULL-vs-"" contract.
        as_col <- function(v) {
          v <- as.character(unlist(v %||% character()))
          v <- v[!is.na(v) & nzchar(v) & v != "(none)"]
          if (!length(v)) NULL else v[[1L]]
        }

        # JS -> R: a row click, a Reset, or a gear edit. Same payload shape as
        # the chart and table blocks, so the three behave identically.
        shiny::observeEvent(input$rank_block_action, {
          act <- input$rank_block_action
          if (identical(act$action, "config")) {
            key <- as.character(act$param %||% "")[1L]
            if (!nzchar(key)) return()
            if (!identical(key, "search") && is.null(setters[[key]])) return()
            val <- act$value
            if (key %in% c("title", "subtitle", "caption")) {
              # NULL = auto, "" = explicitly none, else the template text.
              setters[[key]](if (is.null(val)) NULL else as.character(val)[[1L]])
            } else if (identical(key, "cols")) {
              setters[[key]](as.character(unlist(val %||% character())))
            } else if (identical(key, "search")) {
              r_search(identical(as.character(val)[[1L]], "on"))
            } else if (identical(key, "top_n")) {
              n <- suppressWarnings(as.integer(as.character(val)[[1L]]))
              setters[[key]](if (is.na(n) || n <= 0L) NULL else n)
            } else if (key %in% c("func", "bar_mode", "sort_by", "sort_dir")) {
              v <- as.character(unlist(val %||% character()))
              if (length(v) && nzchar(v[[1L]])) setters[[key]](v[[1L]])
            } else {
              new <- as_col(val)
              setters[[key]](new)
              # Re-aiming or clearing the drill must drop the emitted filter,
              # or downstream stays filtered forever with clicks inert.
              if (identical(key, "drill")) {
                r_filter_column(NULL)
                r_filter_values(NULL)
              }
            }
            return()
          }
          if (identical(act$action, "clear_filter")) {
            r_filter_column(NULL)
            r_filter_values(NULL)
            return()
          }
          col <- rank_chr1(act$column)
          vals <- as.character(unlist(act$values %||% character()))
          if (is.null(col) || !length(vals)) {
            r_filter_column(NULL)
            r_filter_values(NULL)
            return()
          }
          r_filter_type("categorical")
          r_filter_column(col)
          r_filter_values(vals)
        })

        output$rank_table <- shiny::renderUI({
          d <- ann_data()
          shiny::req(is.data.frame(d))
          auto <- r_data_titles()
          rank_table(
            d,
            group = r_group(), value = r_value(), func = r_func(),
            id_var = r_id_var(), parent = r_parent(), color = r_color(),
            bar_mode = r_bar_mode(), facet = r_facet(), compare = r_compare(),
            cols = r_cols(), sort_by = r_sort_by(), sort_dir = r_sort_dir(),
            top_n = r_top_n(), max_height = r_max_height(),
            search = r_search(),
            title = resolve_block_title(r_title(), d, auto = auto$label),
            subtitle = resolve_block_title(r_subtitle(), d,
                                           auto = auto$subtitle),
            caption = resolve_block_title(r_caption(), d, auto = auto$caption),
            drill = r_drill(), scale_map = board_scale_map(),
            elem_id = ns("rank_block"),
            # Isolated: a click must not rebuild the table (the JS keeps the
            # highlight live), but a fresh build -- restore, config edit, new
            # data -- re-reads the then-current state here so the highlight
            # survives those too.
            active = shiny::isolate(
              list(col = r_filter_column(), vals = r_filter_values())
            )
          )
        })

        list(
          expr = shiny::reactive({
            col <- r_filter_column()
            vals <- r_filter_values()
            # Display-only until a row is clicked: downstream receives the
            # input untouched. The expr must be a call, so identity() wraps it.
            if (is.null(col) || !length(vals)) {
              return(quote(identity(data)))
            }
            bquote(
              dplyr::filter(data, .data[[.(col)]] %in% .(as.character(vals)))
            )
          }),
          state = list(
            group = r_group, value = r_value, func = r_func,
            id_var = r_id_var, parent = r_parent, color = r_color,
            bar_mode = r_bar_mode, facet = r_facet, compare = r_compare,
            cols = r_cols, sort_by = r_sort_by, sort_dir = r_sort_dir,
            top_n = r_top_n, max_height = r_max_height, search = r_search,
            title = r_title, subtitle = r_subtitle, caption = r_caption,
            drill = r_drill, filter_type = r_filter_type,
            filter_column = r_filter_column, filter_values = r_filter_values
          )
        )
      })
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        # The dep ships with the STATIC ui, not only with the renderUI output
        # (table / chart block parity): a script that arrives with the first
        # rendered body is too late to have bound before it.
        rank_table_dep(),
        shiny::uiOutput(ns("rank_table"))
      )
    },
    class = c("rank_block", "transform_block", "block"),
    # Same input contract as the table block: a dispatch check only, so a
    # composer table (or anything with an as_annotated_df method) connects
    # directly, and a value the method refuses errors at eval time.
    dat_valid = validate_annotated_df_input,
    # Every column role can legitimately be empty (a fresh block has no
    # picks yet, and clearing a pick must not wedge the block -- see
    # reference: allow_empty_state wedge).
    allow_empty_state = c(
      "group", "value", "id_var", "parent", "color", "facet", "compare",
      "cols", "top_n", "title", "subtitle", "caption", "drill",
      "filter_column", "filter_values"
    ),
    external_ctrl = c(
      "group", "value", "func", "id_var", "parent", "color", "bar_mode",
      "facet", "compare", "cols", "sort_by", "sort_dir", "top_n",
      "max_height", "search", "title", "subtitle", "caption", "drill"
    ),
    ...
  )
}

#' Argument specs for the rank block
#' @noRd
rank_arguments <- function() {
  blockr.core::new_arg_specs(
    group = new_arg_spec(
      paste0(
        "Column to rank by: one row per level, ordered by the measure. The ",
        "one required argument (e.g. AEDECOD for most-frequent adverse ",
        "events)."
      ),
      example = "AEDECOD",
      type = arg_string()
    ),
    func = new_arg_spec(
      paste0(
        "How the measure is computed: count (rows), count_distinct ",
        "(distinct values of `id_var` -- use this for \"subjects with at ",
        "least one event\", the clinical default), or sum / mean / median / ",
        "min / max of `value`."
      ),
      example = "count_distinct",
      type = arg_enum(c("count", "count_distinct", "sum", "mean", "median",
                        "min", "max"))
    ),
    value = new_arg_spec(
      "Numeric column reduced by sum / mean / median / min / max. Unused by count.",
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
        "Ignored when `facet` is also set."
      ),
      example = "AESEV",
      type = arg_string()
    ),
    bar_mode = new_arg_spec(
      paste0(
        "Layout of a colour split: stacked (segments to scale), grouped ",
        "(one thin bar per level, side by side) or percent (each row ",
        "normalized to 100%). No-op without `color`."
      ),
      example = "stacked",
      type = arg_enum(c("stacked", "grouped", "percent"))
    ),
    facet = new_arg_spec(
      paste0(
        "Optional column giving one bar column per level, all on one shared ",
        "scale (e.g. TRTA -- one column per treatment arm). Takes ",
        "precedence over `color`."
      ),
      example = "TRTA",
      type = arg_string()
    ),
    compare = new_arg_spec(
      paste0(
        "With `facet`, the level treated as the comparator: every other ",
        "level gets a zero-centred difference bar in percentage points ",
        "(risk difference). Needs a counting measure."
      ),
      example = "Placebo",
      type = arg_string()
    ),
    cols = new_arg_spec(
      "Numeric columns beside the bar: n, pct, or both.",
      example = list("n", "pct"),
      type = arg_array(arg_enum(c("n", "pct")))
    ),
    sort_by = new_arg_spec(
      "Ordering: value (the measure), label (alphabetical), or a facet level name.",
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
        "behaviour: every row rendered, scrolling at `max_height`. Set it ",
        "only for report exhibits, where there is no scrollbar."
      ),
      example = 10L,
      type = arg_integer()
    ),
    max_height = new_arg_spec(
      "CSS max-height of the scroll container.",
      example = "600px",
      type = arg_string()
    ),
    search = new_arg_spec(
      "Show the search input above the table.",
      example = TRUE,
      type = arg_boolean()
    ),
    drill = new_arg_spec(
      paste0(
        "Column a row click filters on: downstream blocks receive that ",
        "row's rows. Same filter contract as the chart and table blocks. ",
        "Empty = display only."
      ),
      example = "AEDECOD",
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
      example = "Most frequent adverse events — {STUDYID}",
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

#' Construction guidance for the rank block
#' @noRd
rank_guidance <- function() {
  paste(
    "Ranked horizontal bars as an HTML table — the third sibling of the",
    "chart and table blocks. Use it whenever the answer is \"which levels",
    "are the biggest\": most frequent adverse events, top products, worst",
    "sites. It beats a bar chart there because the table form carries",
    "search, click-to-sort, exact values and an arbitrary row count, and it",
    "beats the plain table because the bar makes the ranking readable at a",
    "glance.",
    "\n- `group` is the only required argument. `func = \"count_distinct\"`",
    "with `id_var` is the clinical default (subjects, not events).",
    "\n- `parent` adds an expandable second level (AEBODSYS over AEDECOD).",
    "Each level is aggregated separately, so a parent is never the sum of",
    "its children.",
    "\n- `color` splits each bar into segments (severity); `facet` gives one",
    "bar column per level (treatment arm) on a shared scale. Set one or the",
    "other — facet wins if both are set.",
    "\n- `facet` + `compare` gives a zero-centred difference bar in",
    "percentage points against the comparator level (risk difference).",
    "\n- `drill` makes a row click filter downstream blocks.",
    "\n- Do NOT set `top_n` for an interactive board: the table renders every",
    "row and scrolls, like the table block. It is for report exhibits only.",
    "\nFor vertical columns, lines, scatter or anything with two measures on",
    "axes, use the chart block instead — this block is horizontal bars",
    "only."
  )
}
