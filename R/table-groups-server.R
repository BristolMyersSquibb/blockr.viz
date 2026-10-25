# The groups engine of the table block: one row per level of `by`, one column
# per summary. This is the server of the summarize table block before the
# layout switch (dev/table-block-rewrite.md), lifted out unchanged so
# new_table_block() can run it as a sub-module next to the records engine
# (R/table-records-server.R). Arguments keep the engine's own names;
# `shared` holds the block-owned reactiveVals both engines use. Returns
# list(expr, cond, state) like a block server.
#' @noRd
table_groups_server <- function(id, data,
                                      group = NULL,
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
                                      max_height = NULL,
                                      search = TRUE,
                                      sortable = TRUE,
                                      axis = TRUE,
                                      value_labels = TRUE,
                                      bar_width = "fit",
                                      download = FALSE,
                                      title = NULL,
                                      subtitle = NULL,
                                      # The chart block's default: a deployment
                                      # can caption every new table, e.g. with
                                      # "{filters}". Read once, at construction.
                                      caption = getOption(
                                        "blockr.viz.default_caption"
                                      ),
                                      drill = NULL,
                                      # The prepare step, as on the chart block:
                                      # R code run on the incoming rows before
                                      # the table is built; its top assignments
                                      # become controls on the card.
                                      script = NULL,
                                      values = list(),
                                      ctrl_target = "",
                                      ctrl_table = "",
                                      # Runtime filter transport (NOT
                                      # creation-time config). MUST stay in the
                                      # signature: blockr.core serializes a
                                      # block from its constructor formals and
                                      # restores by re-calling the constructor,
                                      # so dropping these breaks filter-state
                                      # round-trip.
                                      filter_type = "categorical",
                                      filter_column = NULL,
                                      filter_values = NULL,
                                      look = "table",
                                      shared = list()) {
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
  # The clicked row's path, outer -> inner, one value per column
  # (R/summarize-drill.R). A board saved before holds one column.
  filter_column <- chr_vec_state(filter_column)
  filter_values <- null_state(filter_values)
  script <- cb_script_text(script)
  values <- if (is.list(values)) values else list()
  # LEGACY: the old default. Every board saved before the table scrolled with
  # its panel carries it, not a choice anyone made.
  max_height <- chr_state(max_height)
  if (identical(max_height, "600px")) max_height <- NULL
  cols <- chr_vec_state(cols)
  fields <- chr_vec_state(fields)
  # The summarize table groups by `by`; the ranked bar by `group` (+ `parent`).
  # A board that grew its column list out of the ranked-bar surface -- or one
  # saved before `by` existed -- still carries the value in the old slots, and
  # summarize_prepare() falls back to them. So the table draws correctly while
  # the gear's REQUIRED "Group by" row shows empty, which reads as a broken
  # block. Move the value into the slot the summarize gear reads, and clear the
  # old ones: left in place they would silently resurrect a grouping the user
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

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    r_group   <- shiny::reactiveVal(group)
    r_value   <- shiny::reactiveVal(value %||% ".count")
    r_func    <- shiny::reactiveVal(func %||% "count")
    r_id_var  <- shared$id %||% shiny::reactiveVal(id_var)
    r_summaries <- shared$summaries %||% shiny::reactiveVal(summaries)
    r_by      <- shared$by %||% shiny::reactiveVal(as.character(by %||% character()))
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
    r_max_height <- shared$max_height %||% shiny::reactiveVal(max_height)
    r_search  <- shared$search %||% shiny::reactiveVal(isTRUE(search))
    r_sortable <- shared$sortable %||% shiny::reactiveVal(isTRUE(sortable))
    r_axis    <- shiny::reactiveVal(isTRUE(axis))
    r_value_labels <- shiny::reactiveVal(!isFALSE(value_labels))
    # The look (table, the compact matrix, cards), owned by the block and
    # shared with the records engine.
    r_look <- shared$look %||% shiny::reactiveVal(table_look(look))
    r_bar_width <- shiny::reactiveVal(summarize_bar_width(bar_width))
    r_download <- shared$download %||% shiny::reactiveVal(isTRUE(download))
    r_title   <- shared$title %||% shiny::reactiveVal(title)
    r_subtitle <- shared$subtitle %||% shiny::reactiveVal(subtitle)
    r_caption <- shared$caption %||% shiny::reactiveVal(caption)
    r_drill   <- shared$drill %||% shiny::reactiveVal(drill)
    r_ctrl_target <- shared$ctrl_target %||% shiny::reactiveVal(summarize_chr1(ctrl_target) %||% "")

    # Transient drill. With a target set, a click is an EVENT sent to that
    # block, not a selection this table holds: nothing lands in
    # `r_filter_*`, so the block does not filter its own output on a click
    # it only forwarded, the board saves no selection, and clicking the
    # same row twice sends twice instead of toggling off. The undo lives at
    # the target. NULL = no click this session: HOLD, never clear. Mirrors
    # chart-block.R / table-records-server.R.
    r_drill_claim <- shiny::reactiveVal(NULL)
    transient_drill <- function() {
      nzchar(trimws(r_ctrl_target() %||% ""))
    }
    r_ctrl_table  <- shared$ctrl_table %||% shiny::reactiveVal(summarize_chr1(ctrl_table) %||% "")
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
    raw_ann <- shiny::reactive({
      d <- data()
      if (is.data.frame(d)) d else as_annotated_df(d)
    })

    # --- prepare script ------------------------------------------------
    # The chart block's step (R/prepare-apply.R), as the heatmap uses it:
    # the script runs on the input and the table is built from what it
    # returns. Specs keep their last good value while the data is
    # momentarily gone, or a hidden panel would flip the compiled script.
    r_script <- shiny::reactiveVal(script)
    r_values <- shiny::reactiveVal(values)
    r_parsed <- shiny::reactive(cb_parse(r_script()))
    last_specs <- new.env(parent = emptyenv())
    last_specs$value <- list()
    r_specs <- shiny::reactive({
      d <- tryCatch(raw_ann(), error = function(e) NULL)
      parsed <- r_parsed()
      if (is.null(d) && length(last_specs$value)) {
        return(last_specs$value)
      }
      out <- cb_specs(parsed, d)
      if (!identical(out, last_specs$value)) last_specs$value <- out
      last_specs$value
    })
    r_prepared <- shiny::reactive({
      dd_prepare_run(raw_ann(), r_parsed(), r_specs(), r_values())
    })
    # What the table is built from. A failed script leaves an empty frame
    # with the input's columns, so the error reaches the card and the
    # gear instead of a blank block.
    ann_data <- shiny::reactive({
      prep <- r_prepared()
      if (is.null(prep$error)) return(prep$data)
      d <- tryCatch(raw_ann(), error = function(e) NULL)
      if (is.data.frame(d)) d[0, , drop = FALSE] else NULL
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

    # The title, subtitle and caption as the chart block has them: the
    # text, and the pieces the face paints, where a piece that names one
    # of the block's settings (`{label(@by)}`) is a word that opens that
    # setting, and a setting whose `[ ]` clause dropped is offered beside
    # the sentence. Resolved against the block's input, which carries the
    # filter trail `{filters}` reads.
    title_args <- shiny::reactive({
      # The prepare script's declared values are settings too, named in a
      # template by their variable: `{@min_patients}` (chart parity).
      script_title_args(
        summarize_title_args(list(
          by = r_by(), group = r_group(), parent = r_parent(),
          value = r_value(), func = r_func(), id_var = r_id_var(),
          color = r_color(), facet = r_facet(), sort_by = r_sort_by(),
          top_n = r_top_n(), drill = r_drill(), summaries = r_summaries()
        )),
        r_specs(), r_values()
      )
    })
    r_titles <- shiny::reactive({
      d <- tryCatch(ann_data(), error = function(e) NULL)
      shiny::req(is.data.frame(d))
      auto <- r_data_titles()
      a <- title_args()
      parts <- list(
        title = block_title_parts(r_title(), d, auto = auto$label,
                                  args = a),
        subtitle = block_title_parts(r_subtitle(), d,
                                     auto = auto$subtitle, args = a),
        caption = block_title_parts(r_caption(), d, auto = auto$caption,
                                    args = a)
      )
      offers <- function(p) as.list(attr(p, "offers", exact = TRUE))
      list(
        title = resolve_block_title(r_title(), d, auto = auto$label,
                                    args = a),
        subtitle = resolve_block_title(r_subtitle(), d,
                                       auto = auto$subtitle, args = a),
        caption = resolve_block_title(r_caption(), d,
                                      auto = auto$caption, args = a),
        title_parts = parts$title,
        subtitle_parts = parts$subtitle,
        caption_parts = parts$caption,
        title_offers = offers(parts$title),
        subtitle_offers = offers(parts$subtitle),
        caption_offers = offers(parts$caption),
        # A named list, so it arrives as an object (chart-block.R).
        title_arg_values = lapply(
          stats::setNames(nm = names(a)), arg_token_value, args = a
        ),
        # Every setting the text names: a script value the sentence
        # names is a word, so the control strip leaves it out.
        sentence_args = sentence_args(parts$title, parts$subtitle,
                                      parts$caption)
      )
    })

    board_scale_map <- dd_board_scale_map()

    # The gear's config params, mapped to their reactiveVals. One place, so
    # a new arg needs one line here and one role in summarize-table.js.
    setters <- list(
      group = r_group, parent = r_parent, color = r_color,
      facet = r_facet, func = r_func,
      value = r_value, id_var = r_id_var, bar_mode = r_bar_mode,
      bar_width = r_bar_width,
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
    shiny::observeEvent(input$summarize_table_block_action, {
      act <- input$summarize_table_block_action
      if (identical(act$action, "config")) {
        key <- as.character(act$param %||% "")[1L]
        if (!nzchar(key)) return()
        if (identical(key, "script")) {
          # Committed on blur or Apply, never per keystroke.
          r_script(cb_script_text(as.character(act$value %||% "")))
          return()
        }
        if (startsWith(key, "sv_")) {
          # A script value, read against the declared type.
          sv <- dd_script_values(stats::setNames(list(act$value), key),
                                 r_specs(), r_values())
          if (!identical(sv, r_values())) r_values(sv)
          return()
        }
        if (!key %in% c("search", "sortable", "axis", "value_labels",
                        "download", "look") &&
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
        } else if (identical(key, "value_labels")) {
          r_value_labels(identical(as.character(val)[[1L]], "on"))
        } else if (identical(key, "download")) {
          r_download(identical(as.character(val)[[1L]], "on"))
        } else if (identical(key, "look")) {
          v <- table_look(as.character(unlist(val))[1L])
          if (!identical(shiny::isolate(r_look()), v)) r_look(v)
        } else if (identical(key, "top_n")) {
          n <- suppressWarnings(as.integer(as.character(val)[[1L]]))
          setters[[key]](if (is.na(n) || n <= 0L) NULL else n)
        } else if (key %in% c("func", "bar_mode", "bar_width", "sort_by",
                              "sort_dir", "facet_layout")) {
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
      # The row's path: one column per level, one value per column.
      col <- as.character(unlist(act$column %||% character()))
      col <- col[!is.na(col) & nzchar(col)]
      vals <- as.character(unlist(act$values %||% character()))
      filters <- summarize_drill_filters(col, vals)
      # The event path. A clear (no column, from a re-aim or the Reset)
      # is inert: there is no local selection to clear and the target's
      # cohort is not this block's to drop.
      if (transient_drill()) {
        if (length(filters)) {
          r_drill_claim(list(filters = filters,
                             nonce = as.numeric(act$nonce %||% 0)))
        }
        return()
      }
      if (!length(filters)) {
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

      # Transient mode: the claim is the last click, and no click yet is a
      # HOLD (NULL), not an un-drill. `list()` -- the one value that clears
      # the target -- is unreachable from here by design.
      if (transient_drill()) {
        claim <- r_drill_claim()
        if (is.null(claim)) {
          return(NULL)
        }
        return(dd_ctrl_claims(d, r_ctrl_table(), claim$filters,
                             subject = dd_ctrl_subject(session)))
      }

      dd_ctrl_claims(
        d, r_ctrl_table(),
        summarize_drill_filters(r_filter_column(), r_filter_values()),
        subject = dd_ctrl_subject(session)
      )
    })
    dd_ctrl_sender(
      r_ctrl_target,
      r_ctrl_claims,
      # Startup suppression, over the LATCHED state -- which transient mode
      # never writes, so the latch would stay pristine forever and swallow
      # every send. A transient claim exists only after a real click.
      local({
        latch <- dd_ctrl_pristine(
          function() list(r_filter_column(), r_filter_values()),
          list(filter_column, filter_values)
        )
        function() {
          still <- latch()
          is.null(r_drill_claim()) && still
        }
      }),
      session,
      # Re-clicking the same row must send again; a board re-evaluation
      # must not. The counter is minted per click in the browser.
      r_nonce = function() {
        claim <- r_drill_claim()
        if (is.null(claim)) NULL else claim$nonce
      }
    )

    # The chrome is a ONE-SHOT render: container, control row, empty title
    # bands, empty scroll wrapper. Nothing in it is reactive, so no edit
    # rebuilds it -- the gear stays open, and the search text and the
    # scroll position survive. The settings it starts from (search box,
    # drill, ctrl_target, height) change later through the payload's
    # chrome, which summarize-table.js applies in place.
    output$summarize_chrome <- shiny::renderUI({
      shiny::isolate(summarize_chrome_shell(
        max_height = r_max_height(), search = r_search(),
        drill = r_drill(), elem_id = ns("summarize_table_block"),
        ctrl_target = r_ctrl_target(),
        download = shiny::uiOutput(ns("summarize_download"), inline = TRUE)
      ))
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
    cond <- shiny::reactiveValues(error = list())
    last_msg <- new.env(parent = emptyenv())
    last_msg$json <- NULL
    last_msg$rev <- 0L

    push <- function(json) {
      session$sendCustomMessage("blockr-viz-summarize-data", list(
        id = ns("summarize_table_block"), rev = last_msg$rev, payload = json
      ))
    }

    # The client announces itself when it binds with nothing to render.
    # Shiny DROPS a custom message that has no registered handler yet, and
    # summarize-table.js only loads with the first summarize table UI in the
    # page -- on a board whose opening view carries none, the startup
    # payload is lost and the identity guard below would never re-send it.
    shiny::observeEvent(input$summarize_table_block_ready, {
      if (!is.null(last_msg$json)) push(last_msg$json)
    })
    # The payload as JSON, from the current state. The push observer
    # below calls it, and so does a capture request for a table whose
    # payload was never built (a view nobody opened).
    build_json <- function() {
      d <- tryCatch(ann_data(), error = function(e) NULL)
      shiny::req(is.data.frame(d))
      tt <- r_titles()
      cards <- cards_frame(d, r_look(), r_by(), r_drill())
      p <- summarize_build_payload(
        cards$data,
        chrome = c(
          tt[setdiff(names(tt), "title_arg_values")],
          # The container's own settings, which the one-shot chrome
          # render does not follow. "" rather than NULL: the payload
          # drops NULLs, and an absent key means "leave as is".
          list(search = isTRUE(r_search()), drill = cards$drill %||% "",
               ctrl_target = r_ctrl_target() %||% "",
               max_height = r_max_height() %||% "")
        ),
        drill = cards$drill,
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
          value_labels = r_value_labels(),
          bar_width = r_bar_width(),
          compact = identical(r_look(), "compact"), look = r_look(),
          download = r_download(), drill = r_drill(),
          ctrl_target = r_ctrl_target(),
          ctrl_choices = dd_ctrl_choices_list(r_ctrl_choices()),
          titles = c(
            tt,
            list(title_state = r_title(), subtitle_state = r_subtitle(),
                 caption_state = r_caption())
          ),
          columns = summarize_gear_cols(d),
          # The export picture comes from the browser (R/chart-capture.R):
          # the table as drawn, at full height, instead of a server
          # repaint that has to match it.
          capture_export = TRUE,
          capture_ratio = canvas_capture_ratio(),
          capture_page = capture_page_box(),
          # The prepare script: its text for the gear's editor, what it
          # declared (the controls, in the gear engine's vocabulary),
          # its error, and each control's current value (sv_*).
          script = r_script() %||% "",
          script_inputs = dd_script_roles(r_specs()),
          script_error = r_prepared()$error,
          sentence_args = as.list(tt$sentence_args),
          # Named sv_<name>; summarize-table.js spreads them onto the gear's
          # config, where the engine's script roles read them.
          script_values = dd_script_cfg(r_specs(), r_values())
        ),
        group = r_group(), value = r_value(), func = r_func(),
        id_var = r_id_var(), parent = r_parent(), color = r_color(),
        bar_mode = r_bar_mode(), facet = r_facet(),
        summaries = r_summaries(), by = cards$by,
        facet_layout = r_facet_layout(),
        cols = r_cols(), fields = r_fields(), sort_by = r_sort_by(),
        sort_dir = r_sort_dir(), top_n = r_top_n(),
        scale_map = board_scale_map(),
        subject = dd_ctrl_subject(session)
      )
      # A failed custom summary is an error of the block: blockr shows
      # it with the block (the `cond` below), the table draws the rest.
      errs <- as.list(p$failed)
      if (!identical(shiny::isolate(cond$error), errs)) cond$error <- errs
      p$failed <- NULL
      summarize_payload_json(p)
    }
    shiny::observe({
      json <- build_json()
      # String-identity guard: an unchanged payload is never re-sent, and
      # `rev` ticks only on real change so the browser can skip the parse.
      if (identical(json, last_msg$json)) return()
      last_msg$json <- json
      last_msg$rev <- last_msg$rev + 1L
      push(json)
    })

    # --- the export picture ----------------------------------------------
    #
    # The browser draws the table the export carries, the way the chart
    # block's exports come off its canvas (R/chart-capture.R): the same
    # table as on screen, every row, no scroll box, as one picture. The
    # download menu posts it when it opens, so it is here by the time a
    # format is picked. The painted exhibit (summarize-paint.R) stays the
    # fallback where no browser has drawn one.
    capture <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$summarize_table_block_capture, {
      capture(chart_capture_from_msg(input$summarize_table_block_capture))
    })
    # The capture carries the board's download footer
    # (R/download-footer.R); the painted fallback does not.
    dl_picture <- function() {
      cap <- capture()
      if (is.null(cap)) {
        e <- dl_exhibit()
        return(if (!is.null(e$cards)) cards_exhibit(e) else e)
      }
      download_footer_stamp(cap, data(), session)
    }

    # A deck asks for the picture through the session's capture service,
    # at a width it chooses; the height is the table's own.
    register_chart_capture(ns("summarize_table_block"), function(token,
                                                                 width,
                                                                 height) {
      json <- last_msg$json %||% tryCatch(
        shiny::isolate(build_json()),
        error = function(e) NULL
      )
      if (is.null(json)) {
        stop("summarize table '", ns("summarize_table_block"),
             "' has no data to draw yet", call. = FALSE)
      }
      session$sendCustomMessage("blockr-viz-summarize-capture", list(
        req = token, payload = json, width = width,
        ratio = canvas_capture_ratio(),
        css = paste(html_table_shared_css_fallback(), summarize_table_css(),
                    sep = "\n")
      ))
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
    # Its caption ends with the board's download footer
    # (R/download-footer.R), for every format that writes the table
    # rather than the browser's picture.
    dl_exhibit <- function() {
      cards <- cards_frame(ann_data(), r_look(), r_by(), r_drill())
      e <- do.call(static_summarize_table, c(
        list(cards$data),
        list(
          group = r_group(), parent = r_parent(), color = r_color(),
          facet = r_facet(), func = r_func(), value = r_value(),
          id_var = r_id_var(), summaries = r_summaries(), by = cards$by,
          look = r_look(),
          facet_layout = r_facet_layout(), bar_mode = r_bar_mode(),
          cols = r_cols(), fields = r_fields(), sort_by = r_sort_by(),
          sort_dir = r_sort_dir(), top_n = r_top_n(), axis = r_axis(),
          value_labels = r_value_labels(),
          sortable = r_sortable(), bar_width = r_bar_width(),
          title = r_title(), subtitle = r_subtitle(),
          caption = r_caption(),
          .title_args = title_args(),
          scale_map = board_scale_map(),
          subject = dd_ctrl_subject(session)
        )
      ))
      e$caption <- download_footer_caption(e$caption, data(), session)
      e
    }

    dl_formats <- shiny::reactive({
      if (!isTRUE(r_download())) {
        return(list())
      }
      Filter(
        function(s) isTRUE(s$ok),
        list(
          list(id = "dl_xlsx", ext = "xlsx", label = "Excel",
               ok = requireNamespace("openxlsx", quietly = TRUE)),
          list(id = "dl_html", ext = "html", label = "Web page",
               ok = TRUE),
          list(id = "dl_pptx", ext = "pptx", label = "PowerPoint",
               ok = requireNamespace("officer", quietly = TRUE)),
          list(id = "dl_png", ext = "png", label = "Image",
               ok = TRUE)
        )
      )
    })

    output$summarize_download <- shiny::renderUI(
      dl_control_ui(ns, dl_formats())
    )

    # Named for the title the table shows (see R/download-filename.R).
    dl_name <- function(ext) {
      dl_filename(
        tryCatch(dl_dataset(data()), error = function(e) NULL),
        tryCatch(r_titles()$title, error = function(e) NULL),
        "table", ext
      )
    }

    output$dl_xlsx <- shiny::downloadHandler(
      filename = function() dl_name("xlsx"),
      content = function(file) {
        dl_guard("Excel", {
          e <- dl_exhibit()
          # Values, not pictures: openxlsx anchors an image to a cell RANGE
          # rather than a cell, so images neither sort nor resize with the
          # data -- and someone opening the xlsx came to pivot.
          write_annotated_xlsx(
            summarize_export_df(e$prep), file,
            title = e$title, subtitle = e$subtitle, caption = e$caption
          )
        })
      }
    )
    output$dl_html <- shiny::downloadHandler(
      filename = function() dl_name("html"),
      content = function(file) {
        dl_guard("web page", {
          e <- dl_exhibit()
          # The cards look downloads as cards; the spreadsheet keeps the
          # table's values.
          if (!is.null(e$cards)) e <- cards_exhibit(e)
          write_exhibit_html(
            e, file,
            title = e$title, subtitle = e$subtitle, caption = e$caption
          )
        })
      }
    )
    output$dl_pptx <- shiny::downloadHandler(
      filename = function() dl_name("pptx"),
      content = function(file) {
        dl_guard("PowerPoint", {
          e <- dl_picture()
          if (inherits(e, "chart_capture")) {
            write_exhibit_pptx(e, file)
          } else {
            write_exhibit_pptx(
              e, file,
              title = e$title, subtitle = e$subtitle, caption = e$caption
            )
          }
        })
      }
    )
    output$dl_png <- shiny::downloadHandler(
      filename = function() dl_name("png"),
      content = function(file) {
        dl_guard("image", {
          e <- dl_picture()
          if (inherits(e, "chart_capture")) {
            chart_capture_file(e, file)
          } else {
            write_exhibit_png(e, file)
          }
        })
      }
    )

    # Same reason as the chart block's, one line up from the same trap:
    # a download link inside a display:none host is a hidden output,
    # Shiny suspends it, the handler is never registered, and the click
    # comes back 404.
    shiny::outputOptions(output, "summarize_download",
                         suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "dl_xlsx", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "dl_html", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "dl_pptx", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "dl_png", suspendWhenHidden = FALSE)

    # A click on a pooled group of an overlap definition filters on the
    # group's members (dd_group_filters_members()).
    r_drill_filters <- shiny::reactive(
      summarize_drill_filters(r_filter_column(), r_filter_values())
    )
    filter_members <- dd_group_filters_members(r_drill_filters, ann_data)

    list(
      expr = shiny::reactive({
        filters <- r_drill_filters()
        # Display-only until a row is clicked: downstream receives the
        # input untouched (identity(), as the expr must be a call). A click
        # filters on every column of the row's path.
        ex <- summarize_drill_expr(filters, filter_members(filters))
        # The prepare script wraps the filter (chart parity): a click
        # filters the source rows, and downstream receives what the table
        # was built from, filtered.
        sx <- cb_expr(r_parsed(), r_specs(), r_values(), slot = TRUE)
        if (is.null(sx)) ex else dd_splice_slot(sx, ex)
      }),
      # Failed custom summaries, as blockr conditions (set in build_json).
      cond = cond,
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
        sortable = r_sortable, axis = r_axis,
        value_labels = r_value_labels, bar_width = r_bar_width,
        download = r_download,
        title = r_title, subtitle = r_subtitle, caption = r_caption,
        drill = r_drill, ctrl_target = r_ctrl_target,
        ctrl_table = r_ctrl_table, filter_type = r_filter_type,
        filter_column = r_filter_column, filter_values = r_filter_values,
        script = r_script, values = r_values
      )
    )
  })
}

#' @noRd
table_groups_ui <- function(id) {
  ns <- shiny::NS(id)
  # The dep ships with the static UI, not only with the renderUI output: a
  # script that arrives with the first rendered body is too late to bind.
  shiny::tagList(
    summarize_table_dep(),
    shiny::uiOutput(ns("summarize_chrome"))
  )
}
