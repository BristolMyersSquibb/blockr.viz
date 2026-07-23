#' Drill-Down Chart Block
#'
#' A configurable chart that can also act as a filter. Arguments are
#' grammar-of-graphics aesthetics: each one names a **data column**, never
#' a literal value. Roles are orthogonal -- `color` only colours, `series`
#' only splits into series, `label` only writes on-mark text, position is
#' `x`/`y`/`xend` or `group`.
#'
#' Interactivity is explicit and opt-in via `drill`. A click identifies a
#' mark; the mark maps to one or more source rows; the downstream filter
#' is `drill %in% (distinct values of drill over those rows)`. When
#' `drill` is unset the click is inert (no filter emitted). Brush / drag
#' range selection on scatter/line is a separate gesture and is unchanged.
#'
#' Three chart families share this block (an internal render-dispatch
#' detail that never changes what an argument means):
#' aggregated (bar, waterfall, pie, treemap, boxplot, radar), individual
#' (scatter, line), timeline (gantt). A waterfall is a bar with a cumulative
#' baseline (each bar floats from the running total). A radar puts the `group`
#' levels on the spokes,
#' draws one shape per `color` level, and each vertex is
#' `agg_fn(metric)` for that cell; clicking a shape drills on its `color`
#' value. For spokes from several numeric columns, pivot longer upstream
#' and map the name column to `group`.
#'
#' @param group Column for the categorical axis (aggregated charts)
#' @param color Column mapped to colour (optional, all families)
#' @param facet Column to facet by -- one small panel per level (optional)
#' @param value Column for the value (aggregated only). Must match
#'   `func`: ".count" for `"count"` (row count -- the value is otherwise
#'   ignored), any column for `"count_distinct"` (e.g. a subject id to count
#'   patients instead of records), a numeric column for the numeric
#'   aggregations. (Was `metric`.)
#' @param func Aggregation function: `"count"`, `"count_distinct"`, `"mean"`,
#'   `"median"`, `"sum"`, `"min"`, `"max"`, `"identity"`. With a `color` split,
#'   `"count_distinct"` counts an entity once per colour level it appears
#'   under; deduplicate upstream if segments must sum to the per-group
#'   distinct count. `"identity"` ("None (as is)") does NOT aggregate -- it
#'   plots `value` directly, one bar per row when `group` is unique (for bar
#'   heights already computed upstream); duplicate categories collapse to the
#'   first row. (Was `agg_fn`.)
#' @param chart_type Chart type: "bar", "waterfall", "scatter", "line",
#'   "pie", "treemap", "boxplot", "radar", "gantt". "waterfall" is a bar with
#'   a cumulative baseline (sugar for `bar` + `baseline = "cumulative"`).
#' @param x X-axis column (individual / timeline charts)
#' @param y Y-axis column (individual / timeline charts)
#' @param series Column whose distinct values split rows into separate
#'   series (individual: one line/scatter group per value; timeline:
#'   per-bar label). Independent of `color`. High cardinality is fine.
#' @param xend Interval end column (timeline only; NA rows render as
#'   dots at `x`)
#' @param label Column whose value is written on each mark. Default unset
#'   (no on-mark text). For `pie`/`treemap`, when unset the label falls
#'   back to `group` (a label-less pie is unusable).
#' @param tt_fields Character vector of extra column names appended to each
#'   mark's hover tooltip, beyond the mapped roles (gantt, scatter, line, bar).
#'   On a bar the value shown is the group's representative (first row) -- exact
#'   for a `"None (as is)"` bar, a representative for an aggregation. Default
#'   `NULL` (none). Display-only -- does not affect the plot; a listed column
#'   dropped upstream is silently omitted.
#' @param drill Column a click filters downstream on. Default unset (a
#'   click is inert). When set, clicking a mark emits a categorical
#'   filter on this column's value(s) for the clicked mark. For an
#'   aggregated chart this should be a column constant within a group.
#' @param sort_by Category ordering on the axis. Allowed values depend on
#'   the chart family:
#'   * Aggregated: `"value"` (default), `"alpha"`, or a column name.
#'   * Timeline: `"onset"` (default), `"alpha"`, or a column name.
#'   * Individual: ignored.
#' @param sort_dir `"asc"` or `"desc"`. Reverses the `sort_by` ordering.
#' @param orientation Bar orientation: `"horizontal"` (default; category on the
#'   y-axis, best for long labels) or `"vertical"`. Presentation property -- the
#'   mapping (Group/Metric) is unchanged. Bar charts only.
#' @param bar_mode Layout for a color-split bar: `"stacked"` (default -- color
#'   segments stack into one bar per group), `"grouped"` (segments sit
#'   side-by-side / dodged, for comparing absolute values), or `"percent"`
#'   (stacked but each group normalized to 100%, for comparing composition).
#'   No effect without a `color` split; ignored when `baseline = "cumulative"`
#'   (waterfall). Bar charts only.
#' @param filter_type,filter_column,filter_values,filter_range,filter_point
#'   Runtime click/brush filter state (transport for the emitted filter;
#'   normally left at defaults at creation).
#' @param baseline Bar baseline mode: `"zero"` (default -- every bar starts at
#'   0) or `"cumulative"` (a waterfall/bridge -- each bar floats from the
#'   running cumulative of the bars before it; the step axis honors data order,
#'   each `metric` value is a delta). `chart_type = "waterfall"` implies
#'   `"cumulative"`. Bar family only.
#' @param waterfall_totals Character vector of `group` (step) values rendered
#'   as total/subtotal bars in a cumulative-baseline bar: their baseline resets
#'   to 0 and they draw the absolute running cumulative. Default `NULL` (every
#'   bar is a relative delta).
#' @param title,subtitle,caption Chart text, rendered above (title, subtitle)
#'   and below (caption) the chart. Each is one of three tiers: `NULL`
#'   (default) = auto -- each slot falls back to the input data frame's
#'   table-level display attribute (`label` for the title, `subtitle`,
#'   `caption`) when one is present; `""` = explicitly none (suppresses the
#'   auto text); any other
#'   string = shown, with `{...}` tokens resolved against the CURRENT data on
#'   every render: `{col}` -> the distinct values of that column collapsed
#'   with ", " (an upstream value filter on ARM makes `{ARM}` read the
#'   selected arm), `{label(col)}` -> the column's variable label (the picker
#'   block carries the picked measure's label, so `{label(value)}` follows
#'   the pick), `{n}` -> row count, `{n_distinct(col)}` -> distinct count.
#'   Tokens are data lookups, never evaluated code; a token naming a missing
#'   column resolves to "".
#' @param line_width_mult Multiplier on the default line width for line
#'   charts. Default `1.0`. Range `0.5`-`3.0`. Individual family only.
#' @param dot_size_mult Multiplier on the default marker size. Default
#'   `1.0`. Range `0.5`-`3.0`. Individual family only.
#' @param step Step-line mode for line charts. `NULL` (default) draws a
#'   straight line; a step mode (e.g. `"start"`/`"middle"`/`"end"`) draws a
#'   stepped line. Consumed by the JS renderer.
#' @param vlines,hlines Helper lines at fixed positions: numeric vectors,
#'   each entry drawing one dashed guide line -- `vlines` VERTICAL (at that x),
#'   `hlines` HORIZONTAL (at that y). Plain numbers, never column names (a
#'   Hy's-Law eDish cross is `vlines = 3, hlines = 2`). Default `NULL` (none).
#' @param ref_x,ref_y LEGACY aliases for `vlines` / `hlines`, mapped on
#'   construction. Kept as formals so boards saved before the rename restore
#'   (block state restores through the constructor). Do not use in new code:
#'   the names said neither "line" nor which way it ran, and they were the only
#'   array-valued args in the registry that were not plural.
#' @param smoother Trend overlay for scatter charts: one of `"none"`
#'   (default), `"lm"`, or `"loess"`. Fit per `color`/`series` group via
#'   [compute_smoother_series()].
#' @param identity_line Identity-line overlay for scatter charts: `TRUE` draws
#'   a dashed 45-degree y = x guide line across the overlap of the x and y
#'   ranges (shift / agreement plots), `FALSE` (default) omits it. The legacy
#'   `"on"` / `"off"` strings still restore -- the gear's segmented control
#'   transports "on"/"off" over the wire, but that is a UI detail and the R
#'   value is a plain logical, as for the table block's `sortable` et al.
#' @param box_points Observation overlay for boxplots: one of `"none"`
#'   (default, box only), `"outliers"` (plot only the points beyond the
#'   1.5x IQR whiskers) or `"all"` (jittered strip of every observation on
#'   top of the box). No-op for other chart types.
#' @param lo,hi Optional lower / upper value bounds used by the renderer to
#'   clamp or annotate the value axis. Default `NULL` (auto).
#' @param ctrl_target Character(1), beta. Block id of a value filter block on
#'   the board: a categorical drill click's claim is ALSO pushed there over
#'   the board's control channel ([ctrl_send()]; the board needs the channel
#'   installed, see [new_ctrl_bridge_extension()]). Empty (the default) =
#'   off; the drill behaves exactly as before. Exposed in the gear's "Send
#'   to filter (beta)" section.
#' @param ctrl_table Character(1), beta. Name of the table in the target's
#'   `dm` the pushed conditions apply to (e.g. `"adsl"`). Leave empty when
#'   the target filters a plain data frame.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A transform block of class `chart_block`
#' @examplesIf interactive()
#' new_chart_block()
#' @export
new_chart_block <- function(
    group = NULL,
    color = NULL,
    facet = NULL,
    value = ".count",
    func = "count",
    chart_type = "bar",
    x = NULL,
    y = NULL,
    series = NULL,
    xend = NULL,
    label = NULL,
    tt_fields = NULL,
    drill = NULL,
    sort_by = NULL,
    sort_dir = NULL,
    orientation = "horizontal",
    # Color-split bar layout. "stacked" (default) = segments stack into one
    # bar per group; "grouped" = segments sit side-by-side (dodged); "percent"
    # = stacked but each group normalized to 100% (composition view). No-op
    # without a `color` split, and ignored when baseline = "cumulative".
    bar_mode = "stacked",
    # --- Runtime filter transport (NOT creation-time config) -------------
    # These five hold the emitted click/brush filter state. They are set by
    # interaction at runtime, normally left at defaults at creation. They
    # MUST stay in the constructor signature: blockr.core serializes a block
    # from its constructor formals (`block_ctor_inputs`) and restores by
    # re-calling the constructor with those saved values, so dropping them
    # from the signature would break filter-state save/restore. See the
    # `state = list(...)` block below.
    filter_type = "categorical",
    filter_column = NULL,
    filter_values = NULL,
    filter_range = NULL,
    filter_point = NULL,
    # ---------------------------------------------------------------------
    line_width_mult = 1.0,
    dot_size_mult = 1.0,
    step = NULL,
    vlines = NULL,
    hlines = NULL,
    ref_x = NULL,
    ref_y = NULL,
    smoother = "none",
    identity_line = FALSE,
    # Boxplot observation overlay: "none" (box only), "outliers" (only the
    # points past the 1.5x IQR whiskers) or "all" (jittered strip of every
    # observation). No-op for non-boxplot charts.
    box_points = "none",
    lo = NULL,
    hi = NULL,
    # Bar baseline mode. "zero" (default) = a normal bar (every bar starts at
    # 0); "cumulative" = a waterfall/bridge (each bar floats from the running
    # cumulative). `chart_type = "waterfall"` is sugar for bar + cumulative.
    # `waterfall_totals` names the steps rendered as total/subtotal bars
    # (baseline reset to 0, drawn as the absolute running cumulative).
    baseline = "zero",
    waterfall_totals = NULL,
    # Chart text (see R/title-template.R): NULL = auto (title falls back to
    # the data frame's label attribute, the gt block's existing rule), "" =
    # explicitly none, else a template resolved against the current data
    # ("{ARM}" = the distinct values, "{label(value)}" = the column's label).
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    # Observation counts appended to category-axis ticks and/or facet strip
    # labels ("Female (12)"). `count_on` picks the surface(s): "off" (default),
    # "axis" (the category axis), "facet", or "both". `count_col` is the id
    # column counted DISTINCT (e.g. USUBJID -> unique subjects); NULL / "" falls
    # back to the raw row count. The distinct counts are computed per label
    # group in the browser -- they do NOT sum from the per-cell n, since one
    # subject can span several colour / facet cells (see chart.js _labelCounts).
    count_on = "off",
    count_col = NULL,
    ctrl_target = "",
    ctrl_table = "",
    ...) {

  # ARG-RENAME (see dev/unified-arg-naming.md): `metric`/`agg_fn` are the
  # pre-rename names of `value`/`func`. Taken from `...` rather than made
  # formals -- a formal would demand a matching `state` entry and re-serialize
  # under the old name -- so saved boards (which pass the old names on restore)
  # still map onto the new args. Remove after one release cycle.
  .dep <- list(...)
  if (!is.null(.dep$metric)) {
    warning("new_chart_block(): `metric` is deprecated, use `value`.",
            call. = FALSE)
    value <- .dep$metric
  }
  if (!is.null(.dep$agg_fn)) {
    warning("new_chart_block(): `agg_fn` is deprecated, use `func`.",
            call. = FALSE)
    func <- .dep$agg_fn
  }

  # A pre-#144 DAG copy/paste wrote NULL state as `{}`, which arrives here as
  # an empty list(); a board saved in that state keeps re-emitting it (see
  # R/state-normalize.R). Heal every optional slot back to NULL, so a block
  # restored from a poisoned board is indistinguishable from a clean one --
  # the mapping row stays hidden instead of rendering an "[object Object]"
  # picker, and the render paths stop coercing their vectors to lists. Column
  # roles normalize to character; the numeric / free-form slots only collapse
  # the empty list, keeping their type.
  group <- chr_state(group)
  color <- chr_state(color)
  facet <- chr_state(facet)
  x <- chr_state(x)
  y <- chr_state(y)
  xend <- chr_state(xend)
  series <- chr_state(series)
  label <- chr_state(label)
  drill <- chr_state(drill)
  filter_column <- chr_state(filter_column)
  sort_by <- chr_state(sort_by)
  lo <- chr_state(lo)
  hi <- chr_state(hi)
  step <- chr_state(step)
  tt_fields <- chr_vec_state(tt_fields)
  waterfall_totals <- chr_vec_state(waterfall_totals)
  # NOT chr_state: "" is a real value on these (explicitly no title, as
  # against NULL = auto) and chr_state would drop it. See title-template.R.
  title <- title_state(title)
  subtitle <- title_state(subtitle)
  caption <- title_state(caption)
  # `count_col` is an optional column role (heal a DAG-poisoned list() to NULL,
  # coerce to character); `count_on` is a fixed-option select, backfilled to
  # "off" when absent.
  count_col <- chr_state(count_col)
  count_on <- count_on %||% "off"
  # Legacy aliases mapped on construction (old saved boards restore through the
  # ctor, and every chart saved before the rename carries ref_x / ref_y). The
  # new name wins when both are given: a board that already saved `vlines` is
  # newer than whatever ref_x it may also still carry.
  if (is.null(vlines)) vlines <- ref_x
  if (is.null(hlines)) hlines <- ref_y

  vlines <- num_vec_state(vlines)
  hlines <- num_vec_state(hlines)

  # The alias is CONSUMED here: the block stores only the new names, so a board
  # restored from the old ones is migrated on load and re-saves as
  # vlines/hlines. Leaving the legacy value in place would serialize it forever
  # (and a pre-#144 DAG paste hands us list(), which must heal to NULL like
  # every other slot -- see state-normalize.R).
  ref_x <- NULL
  ref_y <- NULL

  # `identity_line` is a LOGICAL. The gear's segmented control sends the
  # strings "on"/"off" (that is the control's transport, see chart.js ROLES),
  # and every board saved before this change stored that string verbatim --
  # 90 of them, all "off". Coerce both shapes to a plain logical here, the way
  # table-block's as_toggle() already does for sortable/collapsible/search.
  identity_line <- bool_state(identity_line)
  filter_values <- null_state(filter_values)
  filter_range <- null_state(filter_range)
  filter_point <- null_state(filter_point)

  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # What the render paths consume: a plain data frame passes through
        # untouched; a table-producing object (composer et al., per the
        # shared input contract) is coerced via as_plain_df(), which drops
        # the reserved `.`-annotation columns -- a chart of a structured
        # frame charts its data columns (the read-only-structure rendering
        # of principle 6 belongs to the table). Mirrors the table block's
        # ann_data reactive.
        plain_data <- shiny::reactive(coerce_plain_df(data()))

        # Auto-tier sources: the input's label / subtitle / caption
        # attributes, read from the RAW data -- as_plain_df() subsets columns
        # and base subsetting drops data-frame-level attributes (see
        # input_display_attrs).
        r_data_titles <- shiny::reactive({
          input_display_attrs(tryCatch(data(), error = function(e) NULL))
        })

        # Config state (orthogonal aesthetics)
        r_group <- shiny::reactiveVal(group)
        r_color <- shiny::reactiveVal(color)
        r_facet <- shiny::reactiveVal(facet)
        r_value <- shiny::reactiveVal(value)
        r_func <- shiny::reactiveVal(func)
        r_chart_type <- shiny::reactiveVal(chart_type)
        r_x <- shiny::reactiveVal(x)
        r_y <- shiny::reactiveVal(y)
        r_xend <- shiny::reactiveVal(xend)
        r_series <- shiny::reactiveVal(series)
        r_label <- shiny::reactiveVal(label)
        # Extra columns to append to each mark's hover tooltip (gantt). A
        # character vector or NULL. Optional display-only role, so a value
        # dropped upstream is silently omitted (not a hard invalid-state).
        r_tt_fields <- shiny::reactiveVal(tt_fields)
        r_drill <- shiny::reactiveVal(drill)
        r_sort_by <- shiny::reactiveVal(sort_by)
        r_sort_dir <- shiny::reactiveVal(sort_dir)
        r_orientation <- shiny::reactiveVal(orientation)
        r_bar_mode <- shiny::reactiveVal(bar_mode)

        # Filter state (transport for the emitted downstream filter)
        r_filter_type <- shiny::reactiveVal(filter_type)
        r_filter_column <- shiny::reactiveVal(filter_column)
        r_filter_values <- shiny::reactiveVal(filter_values)
        r_filter_range <- shiny::reactiveVal(filter_range)
        r_filter_point <- shiny::reactiveVal(filter_point)

        # External-control send (beta): the drill also pushes its claim into
        # a value filter block (gear section "Send to filter"). Empty target
        # = off, today's behaviour.
        r_ctrl_target  <- shiny::reactiveVal(ctrl_target %||% "")
        r_ctrl_table   <- shiny::reactiveVal(ctrl_table %||% "")
        r_ctrl_choices <- dd_ctrl_choices()

        # Theming state
        r_line_width_mult <- shiny::reactiveVal(line_width_mult)
        r_dot_size_mult <- shiny::reactiveVal(dot_size_mult)
        # Overlay options. `step` now has a gear-popover control (the "Step line"
        # select in the line-chart presentation section); its OFF wire value is
        # "", which chr_state() heals back to this NULL default. It flows through
        # the config payload below and via external_ctrl.
        r_step <- shiny::reactiveVal(step)
        r_vlines <- shiny::reactiveVal(vlines)
        r_hlines <- shiny::reactiveVal(hlines)
        r_smoother <- shiny::reactiveVal(smoother)
        r_identity_line <- shiny::reactiveVal(identity_line)
        r_box_points <- shiny::reactiveVal(box_points)
        r_lo <- shiny::reactiveVal(lo)
        r_hi <- shiny::reactiveVal(hi)
        # Bar baseline mode + waterfall total-bar steps (see constructor args).
        r_baseline <- shiny::reactiveVal(baseline)
        r_waterfall_totals <- shiny::reactiveVal(waterfall_totals)
        r_title <- shiny::reactiveVal(title)
        r_subtitle <- shiny::reactiveVal(subtitle)
        r_caption <- shiny::reactiveVal(caption)
        # Observation-count labels (see constructor args). `count_col` is the
        # DISTINCT id column; the browser does the counting per label group.
        r_count_on <- shiny::reactiveVal(count_on)
        r_count_col <- shiny::reactiveVal(count_col)
        r_board_theme <- setup_drilldown_theme_sync(session)
        # Board scale map (NULL when the board has no "scale_map" option);
        # resolved per data push, never stored in block state.
        r_scale_map <- dd_board_scale_map()

        # Column metadata (computed once when data changes). No nrow gate:
        # a 0-row frame still HAS columns (names/types/labels/levels), and
        # the gear pickers must stay usable while an upstream filter has
        # emptied the data.
        r_col_meta <- shiny::reactive({
          d <- plain_data()
          shiny::req(is.data.frame(d))
          lapply(names(d), function(col) {
            vals <- d[[col]]
            lbl <- attr(vals, "label")
            res <- list(
              name = col,
              type = if (is.numeric(vals)) "numeric" else "categorical",
              n_unique = length(unique(vals))
            )
            if (!is.null(lbl) && nzchar(lbl)) res$label <- lbl
            # Factor level order travels to JS as the category/legend order
            # (the data-level "order lives in factors" contract).
            if (is.factor(vals)) res$levels <- as.list(levels(vals))
            res
          })
        })

        # Columns needed by the chart (reactive -- changes when config
        # changes). drill and label are included so the browser has the
        # columns a click filters on / a mark is labelled by.
        # Columns the current mapping needs (so the whole wide flatten is
        # never shipped). Reads the config reactiveVals, so the enclosing
        # reactive (r_needed_cols below) invalidates when the mapping
        # changes -- same set as before the payload-cache refactor.
        needed_cols <- function(d) {
          # as.character(unlist(...)) so a config arg that arrives as an
          # empty list() -- e.g. a NULL aesthetic corrupted by DAG copy/paste
          # into `list()` -- does not coerce the whole vector to a list and
          # crash `d[, needed]` ("must be ... not a list").
          needed <- as.character(unlist(c(
            r_group(), r_color(), r_facet(), r_value(),
            r_x(), r_y(), r_xend(), r_series(),
            r_label(), r_tt_fields(), r_drill(), r_lo(), r_hi(),
            # Count-label id column: not a mapped aesthetic, but the browser
            # needs its values to count distinct ids per label group.
            r_count_col()
          )))
          sb <- as.character(unlist(r_sort_by()))
          if (length(sb) && !sb %in% c("onset", "alpha")) {
            needed <- c(needed, sb)
          }
          if (!is.data.frame(d)) return(character())
          if (identical(r_x(), "AVISIT") && "AVISITN" %in% names(d)) {
            needed <- c(needed, "AVISITN")
          }
          needed <- unique(needed)
          needed[!is.null(needed) & needed != "" &
            needed != ".count" & needed %in% names(d)]
        }

        # Columns the current mapping ships, as their own reactive so the
        # JSON cache below only invalidates when the shipped-column set can
        # change (data or a mapping role), never on a presentation-only
        # edit (sort_dir, orientation, bar_mode, ...).
        r_needed_cols <- shiny::reactive({
          needed_cols(plain_data())
        })

        # Serialized data payload, cached. jsonlite::toJSON over all rows is
        # the expensive part of every push, and it depends only on the data
        # and the shipped columns -- so a presentation-only gear edit
        # re-sends the SAME payload instead of re-serializing the frame.
        # `rev` ticks only when this reactive actually recomputes: shiny
        # splices the json-classed string verbatim into the websocket
        # message (json_verbatim), so the browser receives a parsed OBJECT
        # and cannot use string identity -- an unchanged rev is its signal
        # to skip the row conversion (see setData in chart.js). Read
        # exclusively inside the gated observer below: a reactive is
        # pull-based, so it stays suspended with the observer for hidden
        # panels.
        payload_rev <- 0L
        r_data_json <- shiny::reactive({
          d <- plain_data()
          shiny::req(is.data.frame(d))
          needed <- r_needed_cols()
          df_send <- if (length(needed)) {
            d[, needed, drop = FALSE]
          } else {
            d[0]
          }
          payload_rev <<- payload_rev + 1L
          list(
            rev = payload_rev,
            json = jsonlite::toJSON(df_send, dataframe = "columns",
                                    digits = NA)
          )
        })

        # Smoother overlay, cached on exactly compute_smoother_series()'s
        # inputs: the smoother kind, the data, and the x/y/color/series
        # roles. A loess fit (surface = "direct") is O(n^2), so any other
        # gear edit must reuse the cached fit. With smoother "none" the
        # early return keeps the reactive off the data dependency entirely.
        r_smoother_series <- shiny::reactive({
          sm <- r_smoother()
          if (is.null(sm) || identical(sm, "none")) return(NULL)
          tryCatch(compute_smoother_series(
            plain_data(), sm, r_x(), r_y(), r_color(), r_series()
          ), error = function(e) {
            # Keep the NULL fallback (no overlay) but surface the failure
            # so a broken smoother fit is diagnosable instead of silent.
            warning("drilldown_chart smoother computation failed: ",
                    conditionMessage(e), call. = FALSE)
            NULL
          })
        })

        # Push data + config to JS. Single observe, exactly as before the
        # roles refactor: this shape is what the blockr.dock lazy-eval
        # card-probe pairing correctly suspends for hidden panels, so
        # off-view charts do not render. (The earlier observeEvent +
        # separate config channel rewrite escaped that gating and was the
        # AE-tab freeze.) Only the columns the current mapping needs are
        # shipped, never the whole wide flatten. The expensive pieces
        # (toJSON, smoother fit) live in the cached reactives above -- read
        # only from in here, so they inherit this observer's suspension.
        shiny::observe({
          d <- plain_data()
          # No nrow gate: an upstream filter emptying the frame MUST push,
          # or the browser keeps rendering (clickable) stale marks over data
          # that no longer exists while the block's output is already the
          # empty frame. JS renders its own empty state for 0 rows.
          shiny::req(is.data.frame(d))
          # Reuse the column metadata reactive (same name/type/n_unique/label
          # shape) instead of recomputing it inline -- it is derived from the
          # same data() and was duplicated here.
          col_meta <- r_col_meta()
          payload <- r_data_json()
          # An OPTIONAL aesthetic mapped to a column not present in the current
          # data is sent as unmapped, so the chart draws no legend / no facet
          # strip instead of a single phantom "undefined" group. This is what
          # makes an upstream picker's "(none)" (which emits no such column)
          # actually turn the aesthetic off -- the picker changes data, never
          # this block's config, so the config must self-heal against the data.
          # It also drops phantom legends from a color/facet whose column was
          # removed upstream or typo'd. Mirrors needed_cols() and the scale-map
          # presence guard; REQUIRED roles (group/x/y/value) are left untouched
          # -- their absence is a broken chart, not an intentional "off".
          present_role <- function(v) {
            if (!is.null(v) && length(v) && nzchar(v) && v %in% names(d)) {
              v
            } else {
              NULL
            }
          }
          session$sendCustomMessage("drilldown-data", list(
            id = ns("drilldown_block"),
            columns = col_meta,
            data = payload$json,
            data_rev = payload$rev,
            config = list(
              group = r_group(),
              color = present_role(r_color()),
              facet = present_role(r_facet()),
              value = r_value(), func = r_func(),
              chart_type = r_chart_type(), x = r_x(), y = r_y(),
              xend = r_xend(), series = present_role(r_series()),
              label = r_label(),
              # Extra tooltip columns. as.list() keeps a length-1 vector a JSON
              # array; NULL when empty so the JS "+" role stays hidden until the
              # user adds it (an empty [] would read as "present").
              tt_fields = if (length(r_tt_fields())) as.list(r_tt_fields()) else NULL,
              drill = r_drill(), sort_by = r_sort_by(),
              # Click-filter state, ISOLATED: reading it reactively would
              # re-pump the whole data frame on every click (the render-storm
              # guard above). Only a genuine re-send (restore at session
              # start, config/data change) carries it -- exactly what the JS
              # restore branch needs to re-select the mark and label the
              # footer. as.list() so a length-1 value stays a JSON array.
              filter_column = shiny::isolate(r_filter_column()),
              filter_values = as.list(shiny::isolate(r_filter_values())),
              sort_dir = r_sort_dir(), orientation = r_orientation(),
              bar_mode = r_bar_mode(),
              line_width_mult = r_line_width_mult(),
              dot_size_mult = r_dot_size_mult(), step = r_step(),
              vlines = as.list(r_vlines()), hlines = as.list(r_hlines()),
              smoother = r_smoother(),
              # The gear's segmented control speaks "on"/"off"; the R state is
              # a logical (bool_state). Convert on the way OUT so the control
              # shows the right pill, and back again in the observer below.
              identity_line = if (isTRUE(r_identity_line())) "on" else "off",
              box_points = r_box_points(),
              # Observation-count labels: which surface(s) get the "(n)" and the
              # DISTINCT id column to count (browser-side, per label group).
              count_on = r_count_on(), count_col = r_count_col(),
              # Bar baseline mode. chart_type "waterfall" implies "cumulative"
              # on the JS side (sugar); also send the flag explicitly so a plain
              # bar can opt into the cumulative bridge, and pass the optional
              # total-bar step names. as.list() so a length-1 char vector still
              # serializes as a JSON array.
              baseline = r_baseline(),
              waterfall_totals = as.list(r_waterfall_totals()),
              # Chart text. Raw state feeds the gear's inputs (the template
              # the user typed, or null = auto / "" = none); *_resolved is
              # what the browser renders (tokens substituted against the
              # current data, auto tier applied -- title-template.R).
              title = r_title(),
              subtitle = r_subtitle(),
              caption = r_caption(),
              title_resolved = resolve_block_title(
                r_title(), d, auto = r_data_titles()$label
              ),
              subtitle_resolved = resolve_block_title(
                r_subtitle(), d, auto = r_data_titles()$subtitle
              ),
              caption_resolved = resolve_block_title(
                r_caption(), d, auto = r_data_titles()$caption
              ),
              smoother_series = r_smoother_series(),
              lo = r_lo(), hi = r_hi(),
              # Board scale map, resolved for the chart type's colored role
              # (NULL when no map / no binding / no colored role -- JS then
              # keeps palette cycling).
              scales = dd_scales_config(
                r_scale_map(), r_chart_type(),
                color = r_color(), group = r_group(), data = d
              ),
              # External-control send (beta): current target/table + the
              # board's candidate targets for the gear's picker. The choices
              # reactiveVal skips identical sets, so this re-pumps only when
              # value filter blocks come, go or get renamed.
              ctrl_target = r_ctrl_target(),
              ctrl_table = r_ctrl_table(),
              ctrl_choices = dd_ctrl_choices_list(r_ctrl_choices())
            )
            # NB: the registry _arguments() prose is intentionally NOT sent to
            # the browser. LLM prompts live in the registry only; popover help
            # is a UI-layer concern (terse labels + the live `name (label)`
            # convention). See blockr.design/open/block-config-ui.
          ))
        })

        # Send board theme to JS (separate observer so theme changes don't
        # re-pump the data frame)
        shiny::observe({
          session$sendCustomMessage("drilldown-theme", list(
            id = ns("drilldown_block"),
            theme = r_board_theme()
          ))
        })

        # JS -> R: config or filter changes.
        #
        # `_sendConfig()` echoes the FULL config on any popover change,
        # so most fields arrive unchanged every time. A reactiveVal
        # invalidates even when set to an identical value; since the
        # data-send observer transitively depends on these (via
        # r_needed_cols / the config list), a blind set would re-pump the
        # whole (potentially large) data frame and re-render every chart
        # on each echo -- a render storm that hangs heavy views. Only
        # write when the value actually changes. This is the
        # "prevent R->JS->R loops" guard from
        # blockr.docs/patterns/js-driven-blocks.md.
        upd <- function(rv, v) {
          if (!identical(shiny::isolate(rv()), v)) rv(v)
        }
        nn <- function(v) if (is.null(v) || identical(v, "")) NULL else v

        shiny::observeEvent(input$drilldown_block_action, {
          msg <- input$drilldown_block_action
          if (is.null(msg)) return()

          action <- msg$action
          if (action == "config") {
            if (!is.null(msg$group))      upd(r_group, nn(msg$group))
            if (!is.null(msg$color))      upd(r_color, nn(msg$color))
            if (!is.null(msg$facet))      upd(r_facet, nn(msg$facet))
            if (!is.null(msg$value))     upd(r_value, msg$value)
            if (!is.null(msg$func))     upd(r_func, msg$func)
            if (!is.null(msg$chart_type)) upd(r_chart_type, msg$chart_type)
            if (!is.null(msg$x))          upd(r_x, msg$x)
            if (!is.null(msg$y))          upd(r_y, msg$y)
            if (!is.null(msg$xend))       upd(r_xend, nn(msg$xend))
            if (!is.null(msg$series))     upd(r_series, nn(msg$series))
            if (!is.null(msg$label))      upd(r_label, nn(msg$label))
            if (!is.null(msg$tt_fields)) {
              # Arrives as a JSON array (possibly empty). Flatten to a
              # character vector; an empty selection becomes NULL.
              tf <- as.character(unlist(msg$tt_fields))
              tf <- tf[nzchar(tf)]
              upd(r_tt_fields, if (length(tf)) tf else NULL)
            }
            if (!is.null(msg$drill))      upd(r_drill, nn(msg$drill))
            if (!is.null(msg$sort_by))    upd(r_sort_by, nn(msg$sort_by))
            if (!is.null(msg$sort_dir))   upd(r_sort_dir, msg$sort_dir)
            if (!is.null(msg$orientation)) upd(r_orientation, msg$orientation)
            if (!is.null(msg$bar_mode))   upd(r_bar_mode, msg$bar_mode)
            if (!is.null(msg$baseline))   upd(r_baseline, msg$baseline)
            if (!is.null(msg$smoother))   upd(r_smoother, msg$smoother)
            # "on"/"off" from the gear -> logical in state (see bool_state).
            if (!is.null(msg$identity_line)) {
              upd(r_identity_line, bool_state(msg$identity_line))
            }
            # Helper lines: the numlist control sends a comma-separated string
            # ("2, 5"); num_vec_state parses it and drops non-numeric junk. ""
            # is a real value here (clearing every line), so it must reach the
            # slot as NULL rather than being skipped -- hence no nn() guard.
            if (!is.null(msg$vlines)) {
              upd(r_vlines, num_vec_state(msg$vlines))
            }
            if (!is.null(msg$hlines)) {
              upd(r_hlines, num_vec_state(msg$hlines))
            }
            if (!is.null(msg$box_points)) upd(r_box_points, msg$box_points)
            if (!is.null(msg$count_on))   upd(r_count_on, msg$count_on)
            # nn(): "" (picker cleared) means "no id column" -> row count.
            if (!is.null(msg$count_col))  upd(r_count_col, nn(msg$count_col))
            if (!is.null(msg$lo))         upd(r_lo, nn(msg$lo))
            if (!is.null(msg$hi))         upd(r_hi, nn(msg$hi))
            # Chart text: "" is a real value (explicitly no title -- it
            # suppresses the auto/data-label tier), so no nn(). null = auto
            # is preserved because JS sends null, which skips the guard.
            if (!is.null(msg$title))    upd(r_title, title_state(msg$title))
            if (!is.null(msg$subtitle)) upd(r_subtitle, title_state(msg$subtitle))
            if (!is.null(msg$caption))  upd(r_caption, title_state(msg$caption))
            # "" is a real value here (un-targeting the sender), so no nn().
            if (!is.null(msg$ctrl_target)) {
              upd(r_ctrl_target, trimws(as.character(msg$ctrl_target)))
            }
            if (!is.null(msg$ctrl_table)) {
              upd(r_ctrl_table, trimws(as.character(msg$ctrl_table)))
            }
          } else if (action == "set_mults") {
            if (!is.null(msg$line_width_mult)) {
              upd(r_line_width_mult, as.numeric(msg$line_width_mult))
            }
            if (!is.null(msg$dot_size_mult)) {
              upd(r_dot_size_mult, as.numeric(msg$dot_size_mult))
            }
          } else if (action == "filter") {
            # Filter-state writes use the same identical()-guard (`upd`) as
            # the config writes above: the data-send observer transitively
            # depends on the filter reactiveVals via the expr reactive, so a
            # blind set to an unchanged value would invalidate (and re-pump)
            # needlessly on an echoed filter. The click-vs-brush race logic
            # (the `is_point` no-op below, the `!is.null` gating) is
            # unchanged -- only the actual writes are now guarded.
            ft <- msg$filter_type %||% "categorical"

            if (ft == "categorical") {
              upd(r_filter_column, msg$column)
              upd(r_filter_values, msg$values)
              upd(r_filter_range, NULL)
              upd(r_filter_point, NULL)
              upd(r_filter_type, "categorical")
            } else if (ft == "point") {
              # Click on a single dot with no drill column: drill to the
              # observation(s) at that exact x/y coordinate.
              if (!is.null(msg$x_col) && !is.null(msg$y_col)) {
                upd(r_filter_column, NULL)
                upd(r_filter_values, NULL)
                upd(r_filter_range, NULL)
                upd(r_filter_point, list(
                  x_col = msg$x_col,
                  y_col = msg$y_col,
                  x_val = msg$x_val,
                  y_val = msg$y_val
                ))
                upd(r_filter_type, "point")
              }
            } else if (ft == "range") {
              if (!is.null(msg$x_col) && !is.null(msg$x_range)) {
                xr <- as.numeric(msg$x_range)
                yr <- if (!is.null(msg$y_range)) as.numeric(msg$y_range)
                # A degenerate range (xlo == xhi) is now legitimate: it is a
                # scatter-auto CLICK (a one-point selection -> the observation
                # via between(x, v, v)). The old click-vs-brush race that this
                # guarded against is gone -- within each drill mode click and
                # brush emit the SAME filter type (override -> categorical,
                # auto -> range), so there is nothing to clobber.
                upd(r_filter_column, NULL)
                upd(r_filter_values, NULL)
                upd(r_filter_point, NULL)
                upd(r_filter_range, list(
                  x_col = msg$x_col,
                  y_col = msg$y_col,
                  x_range = xr,
                  y_range = yr
                ))
                upd(r_filter_type, "range")
              } else {
                upd(r_filter_column, NULL)
                upd(r_filter_values, NULL)
                upd(r_filter_range, NULL)
                upd(r_filter_point, NULL)
                upd(r_filter_type, "categorical")
              }
            }
          }
        })

        # What the drill claims: only a CATEGORICAL click is a claim (one
        # value is a decision). Range / point / brush selections are
        # deliberately not claims -- same rule as drill_claim_columns(). The
        # filter state records the column actually drilled at click time
        # (JS resolves drill = "auto" there), so no user-facing claim field.
        r_ctrl_claims <- shiny::reactive({
          d <- tryCatch(plain_data(), error = function(e) NULL)
          col <- r_filter_column()
          vals <- as.character(unlist(r_filter_values()))
          filters <- if (identical(r_filter_type(), "categorical") &&
                           !is.null(col) && length(vals)) {
            stats::setNames(list(vals), col)
          } else {
            list()
          }
          dd_ctrl_claims(d, r_ctrl_table(), filters)
        })

        dd_ctrl_sender(
          r_ctrl_target,
          r_ctrl_claims,
          dd_ctrl_pristine(
            function() {
              list(r_filter_type(), r_filter_column(), r_filter_values(),
                   r_filter_range(), r_filter_point())
            },
            list(filter_type, filter_column, filter_values, filter_range,
                 filter_point)
          ),
          session
        )

        # Build filter expression. With expr_type = "bquoted", `.(data)` is
        # substituted by blockr.core with the upstream block's id at eval
        # time -- so the no-filter branch passes the upstream data frame
        # straight through, keeping the lazy eval chain intact.

        # The click/brush filter expr, over `.(data)` as-is. Kept in its own
        # function so the expr reactive below can wrap the data slot in an
        # as_plain_df() coercion for non-data-frame inputs while plain data
        # frames keep this exact (byte-identical) emitted code.
        build_filter_expr <- function() {
          ft <- r_filter_type()

          if (ft == "categorical") {
            col <- r_filter_column()
            vals <- r_filter_values()

            if (is.null(col) || is.null(vals) || length(vals) == 0) {
              blockr.core::bbquote(dplyr::filter(.(data), TRUE))
            } else if (length(vals) == 1) {
              blockr.core::bbquote(
                dplyr::filter(.(data), .data[[.(col)]] == .(val)),
                list(col = col, val = vals[[1]])
              )
            } else {
              blockr.core::bbquote(
                dplyr::filter(.(data), .data[[.(col)]] %in% .(vals)),
                list(col = col, vals = vals)
              )
            }
          } else if (ft == "point") {
            pt <- r_filter_point()
            if (is.null(pt) || is.null(pt$x_col) || is.null(pt$y_col)) {
              return(blockr.core::bbquote(dplyr::filter(.(data), TRUE)))
            }
            blockr.core::bbquote(
              dplyr::filter(.(data),
                .data[[.(xc)]] == .(xv) & .data[[.(yc)]] == .(yv)
              ),
              list(xc = pt$x_col, yc = pt$y_col,
                xv = pt$x_val, yv = pt$y_val)
            )
          } else if (ft == "range") {
            rng <- r_filter_range()
            if (is.null(rng) || is.null(rng$x_range)) {
              return(blockr.core::bbquote(dplyr::filter(.(data), TRUE)))
            }
            xc <- rng$x_col
            xr <- rng$x_range
            yr <- rng$y_range
            if (!is.null(yr) && !is.null(rng$y_col)) {
              # 2D brush (scatter): filter on both x and y
              yc <- rng$y_col
              blockr.core::bbquote(
                dplyr::filter(.(data),
                  dplyr::between(.data[[.(xc)]], .(xlo), .(xhi)) &
                    dplyr::between(.data[[.(yc)]], .(ylo), .(yhi))
                ),
                list(xc = xc, yc = yc,
                  xlo = xr[1], xhi = xr[2],
                  ylo = yr[1], yhi = yr[2])
              )
            } else {
              # 1D brush (line): filter on x only
              blockr.core::bbquote(
                dplyr::filter(.(data),
                  dplyr::between(.data[[.(xc)]], .(xlo), .(xhi))
                ),
                list(xc = xc, xlo = xr[1], xhi = xr[2])
              )
            }
          } else {
            blockr.core::bbquote(dplyr::filter(.(data), TRUE))
          }
        }

        list(
          expr = shiny::reactive({
            # The expr's only job is the data transform: the click/brush
            # filter. Aesthetic mappings (group/color/x/y/...) are NOT part of
            # it -- they are presentation config the JS renderer consumes, so a
            # mapped column that was renamed or dropped upstream leaves the
            # filter (and the downstream data) perfectly valid. That is a
            # presentation concern, surfaced by the renderer's own in-canvas
            # message (see chart.js: "Mapped column not in data ... re-pick it
            # in the gear"), NOT an expr-level failure. Validating aesthetics
            # here would fail a correct expression; a broken *filter* column,
            # by contrast, fails hard on its own when the emitted filter is
            # evaluated (caught by core's capture_conditions("eval")).
            d <- data()
            # Non-data-frame input under the shared contract (a composer
            # table et al.): the emitted code must coerce the same way the
            # renderer does, so downstream receives the same plain frame the
            # chart draws. NULL input (no upstream yet) stays unwrapped.
            coerce <- !is.null(d) && !is.data.frame(d)
            ex <- build_filter_expr()
            if (coerce) wrap_plain_df_input(ex) else ex
          }),
          state = list(
            group = r_group,
            color = r_color,
            facet = r_facet,
            value = r_value,
            func = r_func,
            chart_type = r_chart_type,
            x = r_x,
            y = r_y,
            xend = r_xend,
            series = r_series,
            label = r_label,
            tt_fields = r_tt_fields,
            drill = r_drill,
            sort_by = r_sort_by,
            sort_dir = r_sort_dir,
            orientation = r_orientation,
            bar_mode = r_bar_mode,
            filter_type = r_filter_type,
            filter_column = r_filter_column,
            filter_values = r_filter_values,
            filter_range = r_filter_range,
            filter_point = r_filter_point,
            line_width_mult = r_line_width_mult,
            dot_size_mult = r_dot_size_mult,
            step = r_step,
            vlines = r_vlines,
            hlines = r_hlines,
            # Legacy alias formals (mapped on construction): serialized as
            # NULL so restored boards re-enter through the new names;
            # blockr.core requires every ctor formal in the state.
            ref_x = function() NULL,
            ref_y = function() NULL,
            smoother = r_smoother,
            identity_line = r_identity_line,
            box_points = r_box_points,
            count_on = r_count_on,
            count_col = r_count_col,
            lo = r_lo,
            hi = r_hi,
            baseline = r_baseline,
            waterfall_totals = r_waterfall_totals,
            title = r_title,
            subtitle = r_subtitle,
            caption = r_caption,
            ctrl_target = r_ctrl_target,
            ctrl_table = r_ctrl_table
          )
        )
      })
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        viz_echarts_dep(),
        viz_block_css_dep(),
        drilldown_chart_dep(),
        shiny::div(id = ns("drilldown_block"), class = "drilldown-chart-container")
      )
    },
    # Shared input contract (see validate_annotated_df_input): a data frame,
    # or a table-producing object coerced via as_annotated_df() -- the chart
    # then charts the coerced frame's data columns (as_plain_df()).
    dat_valid = validate_annotated_df_input,
    # `value` must stay listed: the gear legitimately empties it mid-config
    # (reconcileValue in drilldown-agg.js / _ensureBoxplotMetric in chart.js
    # set value = '' when the aggregation changes and the old column no longer
    # fits) and the observer stores that verbatim -- without the entry the
    # block silently wedges (reference_blockr_allow_empty_state_wedge).
    # `func` is NOT listed: the JS side never emits it empty (a fixed-option
    # select, backfilled to "count"/"mean" wherever unset).
    allow_empty_state = c("group", "color", "facet", "filter_column",
      "filter_values", "value", "x", "y", "xend", "series", "label",
      "tt_fields", "drill", "sort_by", "sort_dir", "filter_range",
      "filter_point", "step", "vlines", "hlines", "smoother", "identity_line",
      "box_points", "lo", "hi", "waterfall_totals",
      # count_col is optional (blank = row count); count_on is a fixed-option
      # select (always "off"/"axis"/"facet"/"both"), so it is not listed here.
      "count_col",
      "title", "subtitle", "caption",
      # Legacy alias formals: permanently NULL in state (mapped onto
      # vlines/hlines at construction), so they MUST be allowed to be empty --
      # a non-allow_empty_state field holding NULL wedges the whole block
      # (state_ready never goes TRUE and result() stays NULL).
      "ref_x", "ref_y",
      "ctrl_target", "ctrl_table"),
    external_ctrl = c("group", "color", "facet", "value", "func",
      "chart_type", "x", "y", "xend", "series", "label", "tt_fields", "drill",
      "sort_by", "sort_dir", "orientation", "bar_mode", "filter_type",
      "filter_column",
      "filter_values", "filter_range", "filter_point", "line_width_mult",
      "dot_size_mult", "step", "vlines", "hlines", "smoother", "identity_line",
      "box_points", "lo", "hi", "baseline", "waterfall_totals",
      "count_on", "count_col",
      "title", "subtitle", "caption",
      "ctrl_target", "ctrl_table"),
    expr_type = "bquoted",
    class = "chart_block",
    ...
  )
}

#' Compute smoother line points per group for a scatter chart
#'
#' Returns a named list keyed by group level. Each entry is a list with
#' numeric vectors `x` and `y` covering a 100-point line across the group's
#' x range. Used by the chart's JS-side renderer to draw a regression
#' overlay without doing the math in the browser.
#'
#' @param data Data frame.
#' @param smoother One of `"none"`, `"lm"`, `"loess"`.
#' @param x_col,y_col Numeric column names.
#' @param color_by,series_by Grouping column names; smoother is fit per
#'   `series_by` if non-NULL else `color_by` else no grouping.
#' @return A named list or `NULL`.
#' @examples
#' compute_smoother_series(
#'   mtcars, "lm", "wt", "mpg",
#'   color_by = NULL, series_by = NULL
#' )
#' @keywords internal
#' @export
compute_smoother_series <- function(data, smoother, x_col, y_col,
                                     color_by, series_by) {
  if (is.null(smoother) || identical(smoother, "none")) return(NULL)
  if (is.null(data) || nrow(data) == 0) return(NULL)
  if (is.null(x_col) || is.null(y_col)) return(NULL)
  if (!all(c(x_col, y_col) %in% names(data))) return(NULL)
  if (!is.numeric(data[[x_col]]) || !is.numeric(data[[y_col]])) return(NULL)

  split_col <- series_by %||% color_by
  if (!is.null(split_col) && split_col %in% names(data)) {
    groups <- split(data, as.character(data[[split_col]]))
  } else {
    groups <- list(`__all__` = data)
  }

  fit_one <- function(d) {
    d <- d[!is.na(d[[x_col]]) & !is.na(d[[y_col]]), , drop = FALSE]
    if (nrow(d) < 3L) return(NULL)
    xv <- d[[x_col]]
    yv <- d[[y_col]]
    if (length(unique(xv)) < 2L) return(NULL)
    rng <- range(xv, na.rm = TRUE)
    xs <- seq(rng[1L], rng[2L], length.out = 100L)
    ys <- tryCatch({
      if (identical(smoother, "lm")) {
        coefs <- stats::coef(stats::lm(yv ~ xv))
        coefs[1L] + coefs[2L] * xs
      } else if (identical(smoother, "loess")) {
        fit <- stats::loess(yv ~ xv, span = 0.75,
                            control = stats::loess.control(surface = "direct"))
        as.numeric(stats::predict(fit, newdata = data.frame(xv = xs)))
      } else {
        NULL
      }
    }, error = function(e) NULL)
    if (is.null(ys) || all(is.na(ys))) return(NULL)
    list(x = as.list(xs), y = as.list(unname(ys)))
  }
  res <- lapply(groups, fit_one)
  res <- res[!vapply(res, is.null, logical(1L))]
  if (length(res) == 0L) NULL else res
}

`%||%` <- function(a, b) if (is.null(a)) b else a
