#' Summarize Table Block
#'
#' `group_by(by) |> summarise(...)` where every column is a summarising
#' function over the group's rows, rendered as text or as a glyph -- a bar
#' beside a box beside a sparkline in one interactive table. The third
#' sibling of [new_chart_block()] and [new_table_block()]: the chart's
#' vocabulary, the table's rendering (sticky header, client-side search and
#' sort, scroll at `max_height`, exact values in their own columns).
#'
#' `summaries` is the config model: an ordered list of typed summary rows
#' (see below). One row is one group, and every glyph is a horizontal mark
#' on a shared linear domain confined to a cell. With no `summaries`, the
#' block is the original ranked-bar table (the flat `group` / `func` /
#' `color` / `facet` surface below). For anything else --
#' vertical columns, lines, scatter, a second measure on an axis -- use
#' [new_chart_block()].
#'
#' Large inputs behave exactly like the table block: every row is rendered and
#' the container scrolls. `top_n` is opt-in, for report exhibits where there is
#' no scrollbar (a pptx slide wants ten bars, not a hundred), and always draws
#' a visible fold row.
#'
#' @param summaries The column list
#'   (`_blockr.design/open/summarize-table/`): an ordered list of typed
#'   summary rows, each a `list(type =, ...)`. Types:
#'   `simple` (`func`, `col`; show `"bar"`, `"number"` or `"dot"`), `dist` (`col`,
#'   `stat`, box-only `whiskers`; show `"box"`, `"pointrange"` or
#'   `"text"`), `field` (`col`; the group's distinct values joined, with a
#'   fold cap -- never an arbitrary first row), `series` (`x`, `col`,
#'   optional `band = c(lo, hi)` data columns, optional computed
#'   `ref = "mean"` / `"mean_sd"` / `"median_iqr"` -- a pooled orientation
#'   line/band over the column's values, per facet level under facet; a
#'   sparkline), `spans` (`x`, `xend`,
#'   optional `color`, optional `label` -- the event-identity column that
#'   headlines each segment's hover tooltip and keys the same-event
#'   highlight -- optional `fields` (extra tooltip columns) and
#'   `size = "md"` / `"lg"` (a WIDER column when the swimlane is the
#'   centerpiece); a swimlane), `expr`
#'   (free R code over the group's
#'   rows; text). Each row takes `name`, `show` and its OWN optional
#'   `color` (a categorical column: the cell is split into one glyph per
#'   level, a bar into segments, a swimlane's events tinted) and `facet`
#'   (a categorical column: the column repeats once per level). Both are
#'   per column, so one column may split by severity while the next
#'   repeats per sex and a third carries neither. A `field` row never
#'   facets: group facts stand outside it. (The block-level `color` /
#'   `facet` below belong to the ranked-bar surface; a board saved when
#'   the summarize table shared them opens with them fanned down onto the
#'   rows they applied to, `scope = "pooled"` meaning "no facet".)
#' @param by Grouping columns for the summaries path, outer to inner (at
#'   most two; the outer becomes the expandable parent). Unset, `group` /
#'   `parent` fill in.
#' @param facet_layout Facet column order on the summaries path:
#'   `"by_summary"` (each summary's level copies adjacent on their shared
#'   scale -- the comparison reading, the default) or `"by_level"`
#'   (unfaceted columns and fields lead, then one spanning column group per
#'   facet level -- the Table-1 reading, with a two-row header). `"by_level"`
#'   needs every faceted column to map the SAME facet column; with several
#'   there are no shared groups to span and the layout stays `"by_summary"`.
#' @param group Column to rank by: one row per level, ordered by the measure.
#' @param value,func,id_var The measure. `func` is one of `"count"`,
#'   `"count_distinct"`, `"sum"`, `"mean"`, `"median"`, `"min"`, `"max"`, or
#'   `"identity"` (the chart block's "None (as is)": `value` untouched, for
#'   data already reduced upstream -- e.g. one value per subject);
#'   `value` is the column it reduces (unused by `"count"`); `id_var` is the
#'   subject identifier `"count_distinct"` counts, which is what makes a count
#'   read as "subjects with at least one event" rather than "events".
#' @param parent Optional outer grouping column (e.g. system organ class over
#'   preferred term): parents become expandable rows with children indented
#'   under them. Each level is aggregated in its own pass, so a parent is never
#'   the sum of its children.
#' @param color Optional column splitting each bar into segments. Composes
#'   with `facet` (the chart's two independent mappings): each facet column's
#'   bars are then split by `color`.
#' @param bar_mode `"stacked"`, `"grouped"` or `"percent"`; no-op without
#'   `color`. Only a measure whose parts sum to the whole (count, count
#'   distinct, sum) can stack: a mean / median / min / max splits side by
#'   side whatever is asked for, and the table says so in its footer.
#' @param facet Optional column giving one bar column per level on a shared
#'   scale (e.g. one column per treatment arm).
#' @param cols Opt-in separate numeric columns beside the bar: any of `"n"`,
#'   `"pct"`. By default the bar cell carries its own value label instead.
#' @param fields Extra columns from the underlying row, shown as real columns
#'   beside the bar -- the chart's tooltip fields. Only meaningful with
#'   `func = "identity"`, where each group IS one row.
#' @param sort_by,sort_dir Server-side ordering: `"value"` (the measure),
#'   `"data"` (the data's own order -- factor levels, else first appearance),
#'   `"label"`, a summary column name or a facet level name; and `"desc"` /
#'   `"asc"`.
#' @param top_n Optional cap (`NULL` = off, the table scrolls instead).
#' @param max_height CSS max-height of the scroll container.
#' @param search Show the search input.
#' @param sortable Allow click-to-sort on the column headers. `FALSE` freezes
#'   the table in the configured `sort_by` order -- for exhibits whose row
#'   order carries meaning (visits, dose groups), where an accidental click
#'   would scramble it.
#' @param download Offer the table for download. `FALSE` (default) shows no
#'   control; `TRUE` adds one to the gear row, writing every format this
#'   machine can: the numbers as a spreadsheet (through
#'   [write_annotated_xlsx()], one column per statistic each mark was drawn
#'   from -- a box glyph is three or five numbers, and
#'   whoever opens an xlsx came to pivot), the table itself as a
#'   self-contained page, and the painted exhibit as a deck or an image. One
#'   writable format renders a button, several render a menu.
#' @param axis Print each glyph column's domain as a tick strip under its
#'   header (default `TRUE`). One strip per column, whatever the mark: the
#'   value domain for a box, a dot range or a bar, the zero-centred one for a
#'   difference bar, the x domain (dates as dates) for a swimlane or a
#'   sparkline. `FALSE` drops every strip, for a dense exhibit where the
#'   numbers beside the marks carry the scale.
#' @param title,subtitle,caption Display text. `NULL` = auto (inherits the
#'   input's label / subtitle / caption attribute), `""` = explicitly none,
#'   else a template with the same `{...}` tokens as the chart and table
#'   blocks (see `resolve_title_template()`).
#' @param drill Column a row click filters on. The emitted filter is the same
#'   categorical contract as [new_chart_block()], so existing filter links
#'   compose.
#' @param ctrl_target Character(1), beta. Block id of a value filter block on
#'   the same board: the drill's claim is ALSO pushed into that block over
#'   the board's control channel (same feature as the chart and table
#'   blocks), so a row click can filter a pipeline this block has no data
#'   link to. Empty (default) = off.
#' @param ctrl_table Character(1), beta. Only with `ctrl_target`: the table
#'   in the target's dm the pushed conditions apply to. Empty for a value
#'   filter fed a plain data frame.
#' @param filter_type,filter_column,filter_values Runtime filter transport,
#'   normally left at defaults: they hold the click state so it survives a
#'   board save and restore.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @return A blockr transform block of class `summarize_table_block`.
#' @examplesIf interactive()
#' new_summarize_table_block(
#'   by = "cyl",
#'   summaries = list(list(type = "simple", func = "count", show = "bar"))
#' )
#' @export
new_summarize_table_block <- function(group = NULL,
                                 value = ".count",
                                 func = "count",
                                 id_var = NULL,
                                 summaries = list(),
                                 by = NULL,
                                 facet_layout = "by_summary",
                                 parent = NULL,
                                 color = NULL,
                                 bar_mode = "stacked",
                                 facet = NULL,
                                 cols = NULL,
                                 fields = NULL,
                                 sort_by = "value",
                                 sort_dir = "desc",
                                 top_n = NULL,
                                 max_height = "600px",
                                 search = TRUE,
                                 sortable = TRUE,
                                 axis = TRUE,
                                 download = FALSE,
                                 title = NULL,
                                 subtitle = NULL,
                                 caption = NULL,
                                 drill = NULL,
                                 ctrl_target = "",
                                 ctrl_table = "",
                                 # Runtime filter transport (NOT creation-time
                                 # config). MUST stay in the signature:
                                 # blockr.core serializes a block from its
                                 # constructor formals and restores by
                                 # re-calling the constructor, so dropping
                                 # these breaks filter-state round-trip.
                                 filter_type = "categorical",
                                 filter_column = NULL,
                                 filter_values = NULL,
                                 ...) {
  # Heal state poisoned by a pre-#144 DAG copy/paste (a NULL slot returning as
  # list()); see R/state-normalize.R.
  group <- chr_state(group)
  value <- chr_state(value)
  id_var <- chr_state(id_var)
  summaries <- if (is.list(summaries)) summaries else list()
  by <- chr_vec_state(by)
  facet_layout <- chr_state(facet_layout)
  parent <- chr_state(parent)
  color <- chr_state(color)
  facet <- chr_state(facet)
  drill <- chr_state(drill)
  filter_column <- chr_state(filter_column)
  filter_values <- null_state(filter_values)
  cols <- chr_vec_state(cols)
  fields <- chr_vec_state(fields)
  # The summarize table groups by `by`; the ranked bar by `group` (+ `parent`).
  # A board that grew its column list out of the ranked-bar surface -- or one
  # saved before `by` existed -- still carries the value in the old slots, and
  # rank_prepare() falls back to them. So the table draws correctly while the
  # gear's REQUIRED "Group by" row shows empty, which reads as a broken block.
  # Move the value into the slot the summarize gear reads, and clear the old
  # ones: left in place they would silently resurrect a grouping the user
  # cleared in the gear (the same rule as the colour / facet migration below).
  if (length(summaries) && !length(by) && length(c(parent, group))) {
    by <- unique(c(parent, group))
    group <- NULL
    parent <- NULL
  }
  # Colour and facet used to be ONE table-level pair for the summarize table;
  # they are the summary's own mappings now. Migrate a saved board here, at
  # construction (which is also how a restore runs), so the fanned-down values
  # become the block's STATE: migrating at render time instead would resurrect
  # the table-level colour every time the user removed it from a column. Only
  # the field values move -- the rows keep the shape they were saved in. The
  # ranked-bar surface keeps the block-level pair.
  if (length(summaries) && (!is.null(color) || !is.null(facet))) {
    norm <- lane_norm_summaries(summaries)
    if (is.null(norm$err)) {
      mig <- lane_migrate_globals(norm, color, facet)
      for (i in seq_along(summaries)) {
        summaries[[i]]$color <- mig[[i]]$color
        summaries[[i]]$facet <- mig[[i]]$facet
      }
      color <- NULL
      facet <- NULL
    }
  }
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
        r_summaries <- shiny::reactiveVal(summaries)
        r_by      <- shiny::reactiveVal(as.character(by %||% character()))
        r_facet_layout <- shiny::reactiveVal(facet_layout %||% "by_summary")
        r_parent  <- shiny::reactiveVal(parent)
        r_color   <- shiny::reactiveVal(color)
        r_bar_mode <- shiny::reactiveVal(bar_mode %||% "stacked")
        r_facet   <- shiny::reactiveVal(facet)
        r_cols    <- shiny::reactiveVal(as.character(cols %||% character()))
        r_fields  <- shiny::reactiveVal(as.character(fields %||% character()))
        r_sort_by <- shiny::reactiveVal(sort_by %||% "value")
        r_sort_dir <- shiny::reactiveVal(sort_dir %||% "desc")
        r_top_n   <- shiny::reactiveVal(top_n)
        r_max_height <- shiny::reactiveVal(max_height)
        r_search  <- shiny::reactiveVal(isTRUE(search))
        r_sortable <- shiny::reactiveVal(isTRUE(sortable))
        r_axis    <- shiny::reactiveVal(isTRUE(axis))
        r_download <- shiny::reactiveVal(isTRUE(download))
        r_title   <- shiny::reactiveVal(title)
        r_subtitle <- shiny::reactiveVal(subtitle)
        r_caption <- shiny::reactiveVal(caption)
        r_drill   <- shiny::reactiveVal(drill)
        r_ctrl_target <- shiny::reactiveVal(rank_chr1(ctrl_target) %||% "")
        r_ctrl_table  <- shiny::reactiveVal(rank_chr1(ctrl_table) %||% "")
        # Candidate targets for the gear's "Send to filter" select: the value
        # filter blocks currently on the board (dd_ctrl_choices tracks the
        # board reactively).
        r_ctrl_choices <- dd_ctrl_choices()
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
          facet = r_facet, func = r_func,
          value = r_value, id_var = r_id_var, bar_mode = r_bar_mode,
          summaries = r_summaries, by = r_by,
          facet_layout = r_facet_layout,
          cols = r_cols, fields = r_fields, sort_by = r_sort_by,
          sort_dir = r_sort_dir, top_n = r_top_n, drill = r_drill,
          ctrl_target = r_ctrl_target, ctrl_table = r_ctrl_table,
          title = r_title, subtitle = r_subtitle, caption = r_caption
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
            if (!key %in% c("search", "sortable", "axis", "download") &&
                  is.null(setters[[key]])) return()
            val <- act$value
            if (key %in% c("title", "subtitle", "caption")) {
              # NULL = auto, "" = explicitly none, else the template text.
              setters[[key]](if (is.null(val)) NULL else as.character(val)[[1L]])
            } else if (identical(key, "summaries")) {
              # The whole column list travels as one JSON array; an empty
              # list switches back to the single-mark path.
              setters[[key]](if (is.list(val)) val else list())
            } else if (key %in% c("cols", "fields", "by")) {
              setters[[key]](as.character(unlist(val %||% character())))
            } else if (key %in% c("ctrl_target", "ctrl_table")) {
              v <- trimws(as.character(unlist(val %||% "")))
              setters[[key]](if (length(v)) v[[1L]] else "")
            } else if (identical(key, "search")) {
              r_search(identical(as.character(val)[[1L]], "on"))
            } else if (identical(key, "sortable")) {
              r_sortable(identical(as.character(val)[[1L]], "on"))
            } else if (identical(key, "axis")) {
              r_axis(identical(as.character(val)[[1L]], "on"))
            } else if (identical(key, "download")) {
              r_download(identical(as.character(val)[[1L]], "on"))
            } else if (identical(key, "top_n")) {
              n <- suppressWarnings(as.integer(as.character(val)[[1L]]))
              setters[[key]](if (is.na(n) || n <= 0L) NULL else n)
            } else if (key %in% c("func", "bar_mode", "sort_by", "sort_dir",
                                  "facet_layout")) {
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

        # External control (chart / table parity): with a ctrl_target set, the
        # drill's claim is ALSO pushed into that value filter block over the
        # board's control channel. Claims read off the block's own filter
        # state; the pristine guard keeps a restored board from re-claiming a
        # target it never touched this session.
        r_ctrl_claims <- shiny::reactive({
          d <- tryCatch(ann_data(), error = function(e) NULL)
          col <- r_filter_column()
          vals <- r_filter_values()
          filters <- if (!is.null(col) && length(vals)) {
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
            function() list(r_filter_column(), r_filter_values()),
            list(filter_column, filter_values)
          ),
          session
        )

        # The chrome is a ONE-SHOT render: container, control row, empty title
        # bands, empty scroll wrapper. It reads only what shapes the chrome, so
        # a data or mapping change never rebuilds it -- the gear, the search
        # text and the scroll position survive every body update.
        output$rank_chrome <- shiny::renderUI({
          rank_chrome_shell(
            max_height = r_max_height(), search = r_search(),
            drill = r_drill(), elem_id = ns("rank_block"),
            download = shiny::uiOutput(ns("rank_download"), inline = TRUE)
          )
        })

        # The body ships as a column-oriented cell model over a custom message
        # (dev/table-data-push-design.md), not through Shiny's renderUI: ~93-95%
        # smaller than the equivalent HTML at 790 rows, and the payload is
        # cached client-side per elem id, so a re-mounted dock panel re-renders
        # with no R round trip.
        #
        # A single plain `observe`, NOT observeEvent + channels: that exact
        # shape is what blockr.dock's lazy-eval card probe suspends for hidden
        # panels (see the chart block's push observer).
        last_msg <- new.env(parent = emptyenv())
        last_msg$json <- NULL
        last_msg$rev <- 0L

        push <- function(json) {
          session$sendCustomMessage("blockr-viz-rank-data", list(
            id = ns("rank_block"), rev = last_msg$rev, payload = json
          ))
        }

        # The client announces itself when it binds with nothing to render.
        # Shiny DROPS a custom message that has no registered handler yet, and
        # rank-table.js only loads with the first rank block UI in the page --
        # on a board whose opening view carries none, the startup payload is
        # lost and the identity guard below would never re-send it.
        shiny::observeEvent(input$rank_block_ready, {
          if (!is.null(last_msg$json)) push(last_msg$json)
        })
        shiny::observe({
          d <- tryCatch(ann_data(), error = function(e) NULL)
          shiny::req(is.data.frame(d))
          auto <- r_data_titles()
          p <- rank_build_payload(
            d,
            chrome = list(
              title = resolve_block_title(r_title(), d, auto = auto$label),
              subtitle = resolve_block_title(r_subtitle(), d,
                                             auto = auto$subtitle),
              caption = resolve_block_title(r_caption(), d, auto = auto$caption)
            ),
            drill = r_drill(),
            # Isolated: a click must not rebuild the body (the JS keeps the
            # highlight live), but any fresh build -- restore, config edit, new
            # data -- re-reads the then-current state, so the highlight
            # survives those too.
            active = shiny::isolate(
              list(col = r_filter_column(), vals = r_filter_values())
            ),
            cfg = list(
              group = r_group(), parent = r_parent(), color = r_color(),
              facet = r_facet(), func = r_func(),
              value = r_value(), id_var = r_id_var(),
              summaries = r_summaries(), by = r_by(),
              facet_layout = r_facet_layout(),
              bar_mode = r_bar_mode(), cols = r_cols(), fields = r_fields(),
              sort_by = r_sort_by(), sort_dir = r_sort_dir(),
              top_n = r_top_n(), search = r_search(),
              sortable = r_sortable(), axis = r_axis(),
              download = r_download(), drill = r_drill(),
              ctrl_target = r_ctrl_target(),
              ctrl_choices = dd_ctrl_choices_list(r_ctrl_choices()),
              titles = list(
                title = resolve_block_title(r_title(), d, auto = auto$label),
                subtitle = resolve_block_title(r_subtitle(), d,
                                               auto = auto$subtitle),
                caption = resolve_block_title(r_caption(), d,
                                              auto = auto$caption),
                title_state = r_title(), subtitle_state = r_subtitle(),
                caption_state = r_caption()
              ),
              columns = rank_gear_cols(d)
            ),
            group = r_group(), value = r_value(), func = r_func(),
            id_var = r_id_var(), parent = r_parent(), color = r_color(),
            bar_mode = r_bar_mode(), facet = r_facet(),
            summaries = r_summaries(), by = r_by(),
            facet_layout = r_facet_layout(),
            cols = r_cols(), fields = r_fields(), sort_by = r_sort_by(),
            sort_dir = r_sort_dir(), top_n = r_top_n(),
            scale_map = board_scale_map()
          )
          json <- rank_payload_json(p)
          # String-identity guard: an unchanged payload is never re-sent, and
          # `rev` ticks only on real change so the browser can skip the parse.
          if (identical(json, last_msg$json)) return()
          last_msg$json <- json
          last_msg$rev <- last_msg$rev + 1L
          push(json)
        })

        # --- downloads ------------------------------------------------------
        #
        # The block's table, taken away. Every writer starts from the SAME
        # exhibit the report path builds (static_summarize_table), so a table
        # downloaded here and the same table on a deck slide are one artifact
        # rendered twice, not two implementations that drift.
        #
        # One toggle, every format the machine can write, in menu order. A
        # format whose writer is missing is left out rather than shown
        # disabled: nobody asked for PowerPoint specifically, the download
        # toggle did, so an entry that only ever explains itself is noise.
        dl_exhibit <- function() {
          d <- ann_data()
          do.call(static_summarize_table, c(
            list(d),
            list(
              group = r_group(), parent = r_parent(), color = r_color(),
              facet = r_facet(), func = r_func(), value = r_value(),
              id_var = r_id_var(), summaries = r_summaries(), by = r_by(),
              facet_layout = r_facet_layout(), bar_mode = r_bar_mode(),
              cols = r_cols(), fields = r_fields(), sort_by = r_sort_by(),
              sort_dir = r_sort_dir(), top_n = r_top_n(), axis = r_axis(),
              sortable = r_sortable(),
              title = r_title(), subtitle = r_subtitle(),
              caption = r_caption(),
              scale_map = board_scale_map()
            )
          ))
        }

        dl_formats <- shiny::reactive({
          if (!isTRUE(r_download())) {
            return(list())
          }
          Filter(
            function(s) isTRUE(s$ok),
            list(
              list(id = "dl_xlsx", ext = "xlsx", label = "Excel (.xlsx)",
                   ok = requireNamespace("openxlsx", quietly = TRUE)),
              list(id = "dl_html", ext = "html", label = "Web page (.html)",
                   ok = TRUE),
              list(id = "dl_pptx", ext = "pptx", label = "PowerPoint (.pptx)",
                   ok = requireNamespace("officer", quietly = TRUE) &&
                     rank_paint_ready()),
              list(id = "dl_png", ext = "png", label = "Image (.png)",
                   ok = rank_paint_ready())
            )
          )
        })

        output$rank_download <- shiny::renderUI({
          specs <- dl_formats()
          if (!length(specs)) return(NULL)
          if (length(specs) == 1L) {
            return(rank_dl_link(ns, specs[[1L]]))
          }
          # <details> rather than a scripted popover, exactly as the table
          # block does it: the open / close behaviour, the keyboard handling
          # and the focus order are the browser's, so the menu needs no JS and
          # cannot fall out of step with the table's own script.
          htmltools::tags$details(
            class = "blockr-dl-menu",
            htmltools::tags$summary(
              class = "blockr-dl-xlsx", title = "Download",
              `aria-label` = "Download", rank_dl_icon()
            ),
            htmltools::tags$div(
              class = "blockr-dl-menu-list", role = "menu",
              lapply(specs, function(s) rank_dl_link(ns, s, menu = TRUE))
            )
          )
        })

        output$dl_xlsx <- shiny::downloadHandler(
          filename = function() "summarize-table.xlsx",
          content = function(file) {
            e <- dl_exhibit()
            # Values, not pictures: openxlsx anchors an image to a cell RANGE
            # rather than a cell, so images neither sort nor resize with the
            # data -- and someone opening the xlsx came to pivot.
            write_annotated_xlsx(
              rank_export_df(e$prep), file,
              title = e$title, subtitle = e$subtitle, caption = e$caption
            )
          }
        )
        output$dl_html <- shiny::downloadHandler(
          filename = function() "summarize-table.html",
          content = function(file) {
            e <- dl_exhibit()
            write_exhibit_html(
              e, file,
              title = e$title, subtitle = e$subtitle, caption = e$caption
            )
          }
        )
        output$dl_pptx <- shiny::downloadHandler(
          filename = function() "summarize-table.pptx",
          content = function(file) {
            e <- dl_exhibit()
            write_exhibit_pptx(
              e, file,
              title = e$title, subtitle = e$subtitle, caption = e$caption
            )
          }
        )
        output$dl_png <- shiny::downloadHandler(
          filename = function() "summarize-table.png",
          content = function(file) {
            write_exhibit_png(dl_exhibit(), file)
          }
        )

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
          # Every constructor formal needs a state entry: blockr.core
          # serializes from formals and restores by re-calling the
          # constructor, so a formal without one silently loses its value
          # across save/restore (the same warning as the filter_* args).
          state = list(
            group = r_group, value = r_value, func = r_func,
            id_var = r_id_var, summaries = r_summaries, by = r_by,
            facet_layout = r_facet_layout, parent = r_parent,
            color = r_color,
            bar_mode = r_bar_mode, facet = r_facet,
            cols = r_cols, fields = r_fields, sort_by = r_sort_by,
            sort_dir = r_sort_dir, top_n = r_top_n,
            max_height = r_max_height, search = r_search,
            sortable = r_sortable, axis = r_axis, download = r_download,
            title = r_title, subtitle = r_subtitle, caption = r_caption,
            drill = r_drill, ctrl_target = r_ctrl_target,
            ctrl_table = r_ctrl_table, filter_type = r_filter_type,
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
        shiny::uiOutput(ns("rank_chrome"))
      )
    },
    # `lane_chart_block` and `rank_block` stay in the class vector so S3
    # usage and saved boards from the earlier eras keep dispatching.
    class = c("summarize_table_block", "lane_chart_block", "rank_block",
              "transform_block", "block"),
    # Same input contract as the table block: a dispatch check only, so a
    # composer table (or anything with an as_annotated_df method) connects
    # directly, and a value the method refuses errors at eval time.
    dat_valid = validate_annotated_df_input,
    # Every column role can legitimately be empty (a fresh block has no
    # picks yet, and clearing a pick must not wedge the block -- see
    # reference: allow_empty_state wedge).
    allow_empty_state = c(
      "group", "value", "id_var", "summaries", "by",
      "parent", "color", "facet",
      "cols", "fields", "top_n", "title", "subtitle", "caption", "drill",
      "ctrl_target", "ctrl_table", "filter_column", "filter_values"
    ),
    external_ctrl = c(
      "group", "value", "func", "id_var", "summaries", "by", "facet_layout",
      "parent", "color", "bar_mode",
      "facet", "cols", "fields", "sort_by", "sort_dir", "top_n",
      "max_height", "search", "sortable", "axis", "download", "title",
      "subtitle",
      "caption",
      "drill",
      "ctrl_target", "ctrl_table"
    ),
    ...
  )
}

#' @rdname new_summarize_table_block
#' @description `new_lane_chart_block()` and `new_rank_block()` are the
#'   block's former names, kept as deprecated aliases so saved boards
#'   restore: they construct the same `summarize_table_block` (and record
#'   the new constructor on the next save).
#' @export
new_lane_chart_block <- function(...) {
  new_summarize_table_block(...)
}

#' @rdname new_summarize_table_block
#' @export
new_rank_block <- function(...) {
  new_summarize_table_block(...)
}

#' Argument specs for the lane chart block
#' @noRd
rank_arguments <- function() {
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
        "wider column for the exhibit case): a swimlane), expr (free R ",
        "code over the ",
        "group's rows, text). ",
        "Optional per object: name (the column header), show, color (a ",
        "categorical column: the cell is split into one glyph per level, a ",
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
                          "expr")),
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
        ref = arg_enum(c("none", "mean", "mean_sd", "median_iqr"),
                       required = FALSE),
        label = arg_string(required = FALSE),
        fields = arg_array(arg_string(), required = FALSE),
        size = arg_enum(c("md", "lg"), required = FALSE),
        expr = arg_string(required = FALSE)
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
    drill = new_arg_spec(
      paste0(
        "Column a row click filters on: downstream blocks receive that ",
        "row's rows. Same filter contract as the chart and table blocks. ",
        "Empty = display only."
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
        "(default) = off."
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
rank_guidance <- function() {
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
