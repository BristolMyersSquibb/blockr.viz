# The records engine of the table block: one row per input row, or the
# structured rendering of an annotated data frame. This is the server of the
# table block before the layout switch (dev/table-block-rewrite.md), lifted
# out unchanged so new_table_block() can run it as a sub-module next to the
# groups engine (R/table-groups-server.R).
#
# Arguments keep the engine's own names; new_table_block() maps its
# arguments onto them. `shared` holds reactiveVals that the block owns and
# both engines read and write (titles, toggles, drill), so a layout switch
# keeps them. A missing entry gets an engine-local reactiveVal.
#
# Returns list(expr, state) like a block server; `state` uses the engine's
# names.
#' @noRd
table_records_server <- function(id, data,
                                 rowname = NULL,
                                 value = NULL,
                                 group = NULL,
                                 summaries = list(),
                                 color = NULL,
                                 shadings = list(),
                                 drill = NULL,
                                 digits = "number",
                                 unit = "",
                                 max_height = NULL,
                                 filter_column = NULL,
                                 filter_values = NULL,
                                 filter_group_cols = NULL,
                                 filter_group_vals = NULL,
                                 sortable = TRUE,
                                 collapsible = TRUE,
                                 search = TRUE,
                                 download = TRUE,
                                 title = NULL,
                                 subtitle = NULL,
                                 caption = NULL,
                                 ctrl_target = "",
                                 ctrl_table = "",
                                 rotate_titles = FALSE,
                                 shared = list()) {
  # Heal state poisoned by a pre-#144 DAG copy/paste, where a NULL slot came
  # back as list() (see R/state-normalize.R). Column roles normalize to
  # character; the color slot only collapses the empty list, because "" is
  # meaningful there (explicitly off, as against NULL = smart default).
  rowname <- chr_state(rowname)
  value <- chr_state(value)
  drill <- chr_state(drill)
  filter_column <- chr_state(filter_column)
  group <- chr_vec_state(group)
  filter_group_cols <- chr_vec_state(filter_group_cols)
  color <- null_state(color)
  filter_values <- null_state(filter_values)
  filter_group_vals <- null_state(filter_group_vals)
  # NOT chr_state: "" is a real value on these (explicitly no title). See
  # R/title-template.R.
  title <- title_state(title)
  subtitle <- title_state(subtitle)
  caption <- title_state(caption)

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    r_rowname    <- shiny::reactiveVal(rowname)
    r_value     <- shiny::reactiveVal(value)
    r_group      <- shiny::reactiveVal(as.character(group))
    r_summaries    <- shiny::reactiveVal(dd_parse_summaries(summaries))
    # `color` = identity tint column (NULL = smart default by rowname,
    # "" = explicitly off); `shadings` = value-encoding rules list.
    r_color    <- shiny::reactiveVal(color)
    r_shadings <- shiny::reactiveVal(dd_parse_shadings(shadings))
    r_drill      <- shared$drill %||% shiny::reactiveVal(drill)
    # The number format (table_format_state()) and its unit.
    r_digits        <- shiny::reactiveVal(table_format_state(digits))
    r_unit          <- shared$unit %||% shiny::reactiveVal(unit)
    r_max_height    <- shared$max_height %||% shiny::reactiveVal(max_height)
    r_filter_column <- shiny::reactiveVal(filter_column)
    r_filter_values <- shiny::reactiveVal(filter_values)
    # Grouped-table drill: a row click ANDs the clicked row's group-key
    # values (aligned vectors) to filter the raw input to that group.
    r_filter_group_cols <- shiny::reactiveVal(filter_group_cols)
    r_filter_group_vals <- shiny::reactiveVal(filter_group_vals)
    # Column half of a structured click. Session-only, deliberately: it
    # has no constructor argument, so a board reload comes back with the
    # row drill restored and no column claim. Add a `filter_col_keys`
    # ctor arg (and its serialization) to make it survive a save.
    r_filter_col_keys <- shiny::reactiveVal(NULL)
    r_sortable       <- shared$sortable %||% shiny::reactiveVal(isTRUE(sortable))
    r_collapsible    <- shiny::reactiveVal(isTRUE(collapsible))
    r_search         <- shared$search %||% shiny::reactiveVal(isTRUE(search))
    r_download       <- shared$download %||% shiny::reactiveVal(isTRUE(download))
    # The block's look (table / compact / cards). This engine's rotated
    # titles are the compact look; its gear reads and writes them as such.
    r_look <- shared$look %||%
      shiny::reactiveVal(if (isTRUE(rotate_titles)) "compact" else "table")
    r_rotate_titles <- function(v) {
      if (missing(v)) return(identical(r_look(), "compact"))
      r_look(if (isTRUE(v)) "compact" else "table")
    }
    # Cards draw the records layout only: the variables layout, which this
    # engine also draws, has no card form.
    r_layout <- shared$layout %||% function() "records"
    # In the table block's records layout, the gear's "Group by" sets the
    # block's `by` (rows nested under one header per level). Standalone,
    # it is this engine's own display aggregation.
    by_mode <- function() {
      !is.null(shared$by) && identical(r_layout(), "records")
    }
    cards_on <- function() {
      identical(r_look(), "cards") && identical(r_layout(), "records")
    }
    # The card model rides on a flat payload; a structured or message table
    # stays a table.
    with_cards <- function(p, data, label_col, value_cols, digits, row_hex) {
      if (!cards_on() || !identical(p$kind, "flat")) return(p)
      p$look <- "cards"
      p$cards <- cards_payload(table_cards_records(
        data, label_col = label_col, value_cols = value_cols,
        digits = digits, unit = r_unit(), row_hex = row_hex
      ))
      p
    }
    r_title    <- shared$title %||% shiny::reactiveVal(title)
    r_subtitle <- shared$subtitle %||% shiny::reactiveVal(subtitle)
    r_caption  <- shared$caption %||% shiny::reactiveVal(caption)
    # External-control send (beta): the drill also pushes its claim into
    # a value filter block (gear section "Send to filter"). Empty target
    # = off, today's behaviour.
    r_ctrl_target  <- shared$ctrl_target %||% shiny::reactiveVal(ctrl_target %||% "")
    r_ctrl_table   <- shared$ctrl_table %||% shiny::reactiveVal(ctrl_table %||% "")
    r_ctrl_choices <- dd_ctrl_choices()

    # Transient drill. With a target set, a click is an EVENT sent to that
    # block, not a selection this table holds: nothing lands in the
    # `r_filter_*` slots, so the table does not filter its own output on a
    # click it only forwarded, the board saves no selection, `data-dt-active`
    # stays empty (which is what makes the JS restore walks inert), and
    # clicking the same row twice sends twice instead of toggling off. The
    # undo lives at the target -- the control channel has no back-edge, so
    # a selection held here would go stale the moment anyone else resets it.
    # NULL = no click this session: HOLD, never clear (see dd_ctrl_claims'
    # "cannot resolve is not an un-drill"). This sender only ever sends.
    # Mirrors chart-block.R.
    r_drill_claim <- shiny::reactiveVal(NULL)
    transient_drill <- function() {
      nzchar(trimws(r_ctrl_target() %||% ""))
    }

    # Only write a reactiveVal when the value actually changes. JS echoes
    # the full config/filter on any popover change, so a blind set would
    # invalidate (and re-render the table) on every echo -- the
    # "prevent R->JS->R loops" guard the chart block uses. (Mirrors
    # chart-block.R.)
    upd <- function(rv, v) {
      if (!identical(shiny::isolate(rv()), v)) rv(v)
    }
    # Segmented gear toggles emit "on"/"off"; constructor / restore pass a
    # logical. Accept both.
    as_toggle <- function(v) identical(v, "on") || isTRUE(v)

    # What the render paths consume: the input coerced to an annotated
    # data frame (as_annotated_df() is a passthrough for plain data
    # frames). The inner server sees `data` even while `dat_valid` is
    # failing, and a method may refuse a particular value at eval time --
    # so readers below take the tryCatch(ann_data()) + req() route rather
    # than reading `data()` directly: coercion failures then park the
    # render silently and the block condition (from dat_valid / the
    # block expr, which coerces the same way) does the explaining.
    ann_data <- shiny::reactive(as_annotated_df(data()))

    # Auto-tier sources: the input's label / subtitle / caption attributes
    # (as_annotated_df is a passthrough for plain data frames, so their
    # attributes survive; composer tables carry them on the coerced frame,
    # recovered from the gt heading + source notes).
    r_data_titles <- shiny::reactive({
      input_display_attrs(tryCatch(ann_data(), error = function(e) NULL))
    })

    shiny::observeEvent(input$drilldown_table_block_action, {
      msg <- input$drilldown_table_block_action
      if (is.null(msg)) return()
      act <- msg$action %||% "config"
      if (identical(act, "filter") && transient_drill()) {
        # The event path. A claim carries rows, columns, or both; a message
        # with neither is the gear clearing the filter after a mapping
        # change, and it is inert -- there is no local selection to clear
        # and the target's cohort is not this block's to drop.
        rows <- msg$filters
        cols <- msg$col_filters
        has_row <- length(rows) || (!is.null(msg$column) && length(msg$values))
        if (has_row || length(cols)) {
          r_drill_claim(list(
            filters    = if (length(rows)) rows,
            col_keys   = if (length(cols)) cols,
            column     = if (!is.null(msg$column)) as.character(msg$column)[[1L]],
            values     = if (length(msg$values)) as.character(unlist(msg$values)),
            nonce      = as.numeric(msg$nonce %||% 0)
          ))
        }
      } else if (identical(act, "filter")) {
        # The column half of a structured click (a header or a cell in an
        # identified column). Its own field, read on every filter message
        # so a stub click or the Reset clears it.
        upd(r_filter_col_keys, if (length(msg$col_filters)) msg$col_filters)
        if (length(msg$filters)) {
          # Grouped drill: [{column, value}, ...] -> aligned vectors, ANDed.
          cols <- vapply(msg$filters, function(f) as.character(f$column),
                         character(1L))
          vals <- vapply(msg$filters, function(f) as.character(f$value),
                         character(1L))
          upd(r_filter_group_cols, cols)
          upd(r_filter_group_vals, vals)
          upd(r_filter_column, NULL)
          upd(r_filter_values, NULL)
        } else if (!is.null(msg$filters)) {
          # A header click: columns and no rows. An empty `filters` is
          # NOT a clear -- clearing carries a null column instead.
          upd(r_filter_group_cols, NULL)
          upd(r_filter_group_vals, NULL)
          upd(r_filter_column, NULL)
          upd(r_filter_values, NULL)
        } else {
          upd(r_filter_column, msg$column)
          upd(r_filter_values, msg$values)
          upd(r_filter_group_cols, NULL)
          upd(r_filter_group_vals, NULL)
        }
      } else if (identical(act, "config")) {
        p <- msg$param
        v <- msg$value
        if (identical(p, "shadings")) {
          # JS sends the whole shadings list as a JSON string on any edit
          # (same transport as `summaries`).
          upd(r_shadings, dd_parse_shadings(v))
        } else if (identical(p, "color")) {
          # "" (explicit none from "(none)") is distinct from NULL (unset /
          # smart default), so store "" rather than collapsing to NULL.
          upd(r_color, if (identical(v, "(none)") || !nzchar(v)) "" else v)
        } else if (identical(p, "drill")) {
          upd(r_drill, if (identical(v, "(none)") || !nzchar(v)) NULL else v)
        } else if (identical(p, "group")) {
          if (by_mode()) {
            upd(shared$by, as.character(unlist(v)))
          } else {
            upd(r_group, as.character(unlist(v)))
          }
        } else if (identical(p, "summaries")) {
          # JS sends the whole summaries list as a JSON string on any edit.
          upd(r_summaries, dd_parse_summaries(v))
        } else if (p %in% c("format", "digits")) {
          # "digits" is the gear's old key for a number of decimals.
          upd(r_digits, table_format_state(v))
        } else if (identical(p, "unit")) {
          u <- as.character(unlist(v))
          upd(r_unit, if (length(u) && !is.na(u[[1L]])) trimws(u[[1L]]) else "")
        } else if (identical(p, "sortable")) {
          upd(r_sortable, as_toggle(v))
        } else if (identical(p, "collapsible")) {
          upd(r_collapsible, as_toggle(v))
        } else if (identical(p, "search")) {
          upd(r_search, as_toggle(v))
        } else if (identical(p, "download")) {
          upd(r_download, as_toggle(v))
        } else if (identical(p, "rotate_titles")) {
          upd(r_rotate_titles, as_toggle(v))
        } else if (identical(p, "look")) {
          upd(r_look, table_look(as.character(unlist(v))[1L]))
        } else if (identical(p, "ctrl_target")) {
          upd(r_ctrl_target, trimws(as.character(v %||% "")))
        } else if (identical(p, "ctrl_table")) {
          upd(r_ctrl_table, trimws(as.character(v %||% "")))
        } else if (identical(p, "title")) {
          # "" is a real value on the title slots (explicitly none --
          # it suppresses the auto/data-label tier), so store verbatim.
          upd(r_title, title_state(v))
        } else if (identical(p, "subtitle")) {
          upd(r_subtitle, title_state(v))
        } else if (identical(p, "caption")) {
          upd(r_caption, title_state(v))
        }
      }
    })

    # What the drill claims, read off the block's OWN filter state: the
    # structured drill's identity keys (all dot-prefixed -> ARD mode) or
    # the flat drill's column + values (-> source-column mode). Shared
    # code path with the chart / tile (dd_ctrl_claims / dd_ctrl_sender).
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
        tf <- if (length(claim$filters)) {
          cc <- vapply(claim$filters, function(f) as.character(f$column),
                       character(1L))
          vv <- vapply(claim$filters, function(f) as.character(f$value),
                       character(1L))
          stats::setNames(as.list(vv), cc)
        } else if (!is.null(claim$column) && length(claim$values)) {
          stats::setNames(list(claim$values), claim$column)
        } else {
          list()
        }
        tcols <- dd_col_claims(claim$col_keys, r_ctrl_table())
        trows <- dd_ctrl_claims(d, r_ctrl_table(), tf,
                                subject = dd_ctrl_subject(session))
        if (is.null(trows)) {
          return(NULL)
        }
        # A column-only claim (a header click) is a real claim, but its row
        # half is the empty `list()` that would otherwise read as an
        # un-drill; the column entries carry it.
        return(c(tcols, trows))
      }

      gc <- r_filter_group_cols()
      gv <- r_filter_group_vals()
      col <- r_filter_column()
      vals <- r_filter_values()
      filters <- if (length(gc) && length(gc) == length(gv)) {
        stats::setNames(as.list(gv), gc)
      } else if (!is.null(col) && length(vals)) {
        stats::setNames(list(vals), col)
      } else {
        list()
      }
      # The column half needs no resolving against the display frame: it
      # already names a source column. ANDed in front of the row claim.
      cols <- dd_col_claims(r_filter_col_keys(), r_ctrl_table())
      rows <- dd_ctrl_claims(d, r_ctrl_table(), filters,
                             subject = dd_ctrl_subject(session))
      if (is.null(rows)) {
        return(NULL)
      }
      c(cols, rows)
    })

    dd_ctrl_sender(
      r_ctrl_target,
      r_ctrl_claims,
      # Startup suppression, over the LATCHED state -- which transient mode
      # never writes, so the latch would stay pristine forever and swallow
      # every send. A transient claim exists only after a real click, so it
      # ends pristine on its own. The latch is still read on every pass:
      # that is what holds the dependency on the filter state.
      local({
        latch <- dd_ctrl_pristine(
          function() {
            list(r_filter_column(), r_filter_values(),
                 r_filter_group_cols(), r_filter_group_vals(),
                 r_filter_col_keys())
          },
          list(filter_column, filter_values, filter_group_cols,
               filter_group_vals, NULL)
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

    # The three writers see the same frame and the same resolved text the
    # on-screen bands show -- the point of a clinical title is the export.
    # The frame is the displayed one, with the shaded cells' paint as
    # `.bg:` / `.fg:` columns (dt_exhibit_frame), so the downloads carry
    # the colours and not the hidden colour-source columns.
    dl_exhibit <- function() {
      d <- ann_data()
      auto <- r_data_titles()
      list(
        # The unit heads the numeric columns, as on screen.
        data = dt_unit_labels(
          dt_exhibit_frame(d, r_rowname(), r_value(), r_group(),
                           r_summaries(), r_shadings()),
          r_unit()
        ),
        # The export frame is built anew and has lost the input's
        # attributes, so the file name's dataset and the download
        # footer's provenance and filters are read off the input.
        dataset = dl_dataset(d),
        source = d,
        title = resolve_block_title(r_title(), d, auto = auto$label),
        subtitle = resolve_block_title(r_subtitle(), d,
                                       auto = auto$subtitle),
        caption = resolve_block_title(r_caption(), d, auto = auto$caption),
        # The display toggles travel with the exhibit: the HTML file is the
        # table, so a table shown without collapsing (or without sorting)
        # downloads that way too. Reading them here rather than in the
        # handler keeps every writer looking at one snapshot of the block.
        collapsible = isTRUE(r_collapsible()),
        sortable = isTRUE(r_sortable()),
        # Rounded like the screen (see dt_download_control). A structured
        # frame shows its cells as they come, so it is not rounded.
        digits = if (!dt_is_structured(d)) r_digits(),
        # The cards look downloads as cards (html, pptx); the spreadsheet
        # keeps the values.
        cards = if (cards_on() && !dt_is_structured(d)) {
          cards_exhibit(table_cards_records(
            d, label_col = r_rowname(), value_cols = r_value(),
            digits = r_digits(), unit = r_unit(),
            row_hex = row_hex_for(d)$hex
          ))
        }
      )
    }

    # Downloads: a control on the chrome toolbar, on when the block's
    # `download` toggle is. The markup and the handlers are shared with
    # every other renderer that offers the same three formats (see
    # dt_download_control), so the reader meets one control.
    dl_slot <- dt_download_control(session, dl_exhibit,
                                   enabled = r_download)

    # Split render so a filter (or gear edit) re-renders ONLY the <table>,
    # never the search bar / gear / scroll container -- no whole-panel
    # blank-out, and search text + scroll position survive. `dt_result` is
    # the chrome: it depends only on whether the frame is structured (which
    # is fixed for a given block), so it renders once and is never rebuilt
    # on a filter. `dt_table` is the table body, re-rendered on every data
    # or config change; the gear/drill/colour/digits state travels on the
    # <table>'s data-attributes (see dt_table_attrs()). Mirrors the chart
    # block's "static container + reactive content" shape.
    # The chrome does NOT depend on the data. It used to wait for
    # `dt_is_structured(ann_data())`, which meant the whole control section
    # (search box, download, scroll container, status line) could not paint
    # until the upstream pipeline had evaluated -- seconds, on a big board
    # where a view switch wakes a dormant chain. `structured = NULL` builds
    # the same chrome minus the class, and table.js promotes the class off
    # the `<table>` when the body arrives.
    output$dt_result <- shiny::renderUI({
      dt_chrome(
        elem_id    = ns("drilldown_table_block"),
        structured = NULL,
        max_height = shiny::isolate(r_max_height()),
        # The body no longer renders through Shiny: table.js fills this
        # slot from the "blockr-viz-table-data" payload (see the push
        # observer below). An empty div, not a uiOutput -- there is no
        # output to bind.
        inner      = htmltools::tags$div(class = "dt-table-slot"),
        search     = isTRUE(r_search()),
        download_slot = dl_slot,
        status_slot   = shiny::uiOutput(ns("dt_status"),
                                        class = "dt-status-slot")
      )
    })

    # Active-filter status line + Reset (the chart footer's classes, so
    # both read identically). A tiny output of the filter reactiveVals
    # only -- the (potentially heavy) table body never re-renders on a
    # click. Present whenever drill is enabled ("No filter active", chart
    # parity) or a filter is still active (e.g. restored from a saved
    # board); the Reset button is wired by delegation in table.js.
    output$dt_status <- shiny::renderUI({
      gc  <- as.character(unlist(r_filter_group_cols()))
      gv  <- as.character(unlist(r_filter_group_vals()))
      col <- r_filter_column()
      vals <- as.character(unlist(r_filter_values()))
      grouped_on <- length(gc) > 0 && length(gc) == length(gv)
      single_on  <- !is.null(col) && length(vals) > 0
      drill_on   <- !is.null(r_drill()) && nzchar(r_drill())
      if (!grouped_on && !single_on && !drill_on) return(NULL)
      # Transient drill: nothing latches, so there is no steady state to
      # report and no Reset to offer. The line exists only just after a
      # click, says what was sent, and fades -- the same receipt the chart
      # shows, in the same words, so both ends of one gesture read alike.
      # No claim yet = no footer at all, rather than a permanent
      # "No filter active" that can never become anything else.
      if (transient_drill()) {
        claim <- r_drill_claim()
        if (is.null(claim)) return(NULL)
        rows <- dt_claim_label(
          vapply(claim$filters, function(f) as.character(f$column),
                 character(1L)),
          vapply(claim$filters, function(f) as.character(f$value),
                 character(1L))
        )
        if (!length(rows) && !is.null(claim$column) &&
              length(claim$values)) {
          rows <- paste0(claim$column, " = ",
                         paste(claim$values, collapse = ", "))
        }
        cols <- vapply(claim$col_keys %||% list(), function(k) {
          paste0(as.character(k[["column"]])[1L], " = ",
                 paste(as.character(unlist(k[["values"]])), collapse = ", "))
        }, character(1L))
        parts <- c(rows, cols)
        if (!length(parts)) return(NULL)
        return(htmltools::tags$div(
          class = "dd-status-footer",
          htmltools::tags$span(
            class = "dd-status-text dt-status-receipt",
            paste0("Drilled down to ", paste(parts, collapse = "; "))
          )
        ))
      }
      text <- if (grouped_on) {
        paste0("Filtered: ", paste0(dt_claim_label(gc, gv),
                                    collapse = " \u203a "))
      } else if (single_on) {
        paste0("Filtered: ", col, " = ", paste(vals, collapse = ", "))
      } else {
        "No filter active"
      }
      htmltools::tags$div(
        class = "dd-status-footer",
        htmltools::tags$span(class = "dd-status-text", text),
        if (grouped_on || single_on) {
          htmltools::tags$button(
            type = "button", class = "dd-status-reset", "Reset"
          )
        }
      )
    })

    # Board scale map (NULL when the board has none / blockr.theme absent) --
    # the same source the drilldown chart reads. Used to color the
    # categorical row-stub, matching the chart's legend.
    board_scale_map <- dd_board_scale_map()

    # The rows' tint: `color` names the column whose scale-map colours tint
    # the rows (the chart's color aesthetic applied to rows, "Color by").
    # NULL (unset) = smart default, the row-stub column, map-bound only (an
    # unbound, usually unique, rowname would rainbow every table); "" =
    # explicitly off; a name = that column, with the chart's palette
    # fallback when the map does not bind it.
    row_hex_for <- function(d) {
      rc <- r_color()
      col <- if (is.null(rc)) {
        r_rowname() %||% names(d)[1L]
      } else if (nzchar(rc)) {
        rc
      }
      hex <- if (is.null(col)) {
        NULL
      } else if (is.null(rc)) {
        dd_row_hex(board_scale_map(), col, d)
      } else {
        dd_ident_hex(board_scale_map(), col, d)
      }
      list(col = col, hex = hex)
    }

    # The ctrl config rides on the rendered <table> as data-attributes
    # (like every other gear-read state, see dt_table_attrs): stamped
    # post-hoc so the render helpers stay ctrl-agnostic. Reading the
    # choices reactiveVal here re-renders the table only when the
    # candidate set changes (identical() skip in dd_ctrl_choices).
    stamp_ctrl <- function(tag) {
      htmltools::tagAppendAttributes(
        tag,
        `data-dt-ctrl-target` = r_ctrl_target(),
        `data-dt-ctrl-table` = r_ctrl_table(),
        `data-dt-ctrl-choices` = dd_ctrl_choices_json(r_ctrl_choices())
      )
    }

    # Body payload (a pre-serialized JSON string), cached as a reactive:
    # it recomputes only when the data or a config it reads changes, and
    # it is read exclusively from the push observer below -- a reactive
    # is pull-based, so it stays suspended with the observer for hidden
    # panels (the chart block's r_data_json pattern).
    r_body_payload <- shiny::reactive({
      d <- tryCatch(ann_data(), error = function(e) NULL)
      shiny::req(is.data.frame(d))
      # Chart-parity table text, riding on the payload (title band + gear
      # state): the resolved display strings plus the raw state -- the
      # gear needs the NULL-vs-"" distinction (auto vs explicitly none),
      # which a table data-attribute cannot carry. NULL entries are
      # DROPPED before serializing: dt_payload_json has no null="null",
      # so a NULL would arrive as {} and break the JS null checks.
      titled <- function(p) {
        auto <- r_data_titles()
        tl <- list(
          title    = resolve_block_title(r_title(), d, auto = auto$label),
          subtitle = resolve_block_title(r_subtitle(), d, auto = auto$subtitle),
          caption  = resolve_block_title(r_caption(), d, auto = auto$caption),
          title_state    = r_title(),
          subtitle_state = r_subtitle(),
          caption_state  = r_caption()
          # No *_auto fields: while a slot's state is null (auto), its
          # resolved value above IS the inherited text -- the gear derives
          # the clearable auto content from exactly that (readGearState).
        )
        p$titles <- tl[!vapply(tl, is.null, logical(1L))]
        # The block's look, for the gear's look selector: the payload says
        # `look = "cards"` only while cards are drawn.
        p$look_state <- r_look()
        # The gear's "Group by": the current `by` and the input's columns,
        # which the nested table no longer shows as columns.
        if (by_mode()) {
          p$by_state <- as.list(as.character(shared$by()))
          p$by_choices <- dt_gear_cols_json(shared$input())
        }
        p
      }
      # Filter state at build time, ISOLATED: the table body must never
      # re-render on a click (the split-render perf contract) -- the JS
      # keeps the row highlight live between renders, and any fresh
      # build (restore, config edit, new data) re-reads the then-current
      # state here, so the highlight survives those too.
      act <- shiny::isolate(list(
        col   = r_filter_column(),  vals  = r_filter_values(),
        gcols = r_filter_group_cols(), gvals = r_filter_group_vals()
      ))
      # Aggregation is a DISPLAY projection: when `group` is set, draw the
      # summarised frame (one row per group + a measure). The block's data
      # output stays the raw input filtered by the click (see `expr`), and
      # a row click drills on the group keys. No group -> the raw table,
      # exactly as before.
      # The projection itself lives in dt_display_frame(), shared with the
      # downloads (dt_exhibit_frame), so both see the same frame.
      disp <- dt_display_frame(d, group = r_group(),
                               summaries = r_summaries())
      agg <- disp$agg
      if (isTRUE(agg$aggregated)) {
        gl <- length(agg$group)
        ad <- disp$data
        # COLOR applies to the AGGREGATED frame too: shadings resolve
        # against the displayed (aggregated) columns, and "Color by"
        # tints rows whose color column survived the aggregation (a
        # group key; a non-grouped column has no per-row value here and
        # silently yields no tint). Smart default (color NULL) = the
        # first group key, map-bound-only; explicit pick = chart-parity
        # palette fallback (dd_ident_hex).
        arc <- r_color()
        arc_col <- if (is.null(arc)) {
          if (gl) agg$group[1L] else NULL
        } else if (nzchar(arc)) {
          arc
        } else {
          NULL
        }
        agg_hex <- if (is.null(arc_col)) {
          NULL
        } else if (is.null(arc)) {
          dd_row_hex(board_scale_map(), arc_col, ad)
        } else {
          dd_ident_hex(board_scale_map(), arc_col, ad)
        }
        return(tryCatch(
          dt_payload_json(titled(with_cards(dt_build_payload(
            ad,
            stamp      = stamp_ctrl,
            # The displayed frame is a projection; the gear's pickers
            # must keep offering the RAW input schema (group / value /
            # drill choices act on `d`, not on the aggregate).
            gear_cols  = dt_gear_cols_json(d),
            label_col  = disp$label_col,
            value_cols = disp$value_cols,
            shadings   = r_shadings(),
            drill      = NULL,
            # Group-keys drill is OPT-IN (checkbox default off, like every
            # drill): the keys wire up only when `drill` is set -- "auto"
            # from the gear checkbox, or any truthy legacy value from a
            # saved board. Empty group_cols -> no row click wiring.
            # (Empty anyway for grand totals -- nothing to key on.)
            group_cols = if (!is.null(r_drill()) && nzchar(r_drill())) {
              agg$group
            } else {
              character()
            },
            row_hex    = agg_hex,
            color      = arc_col,
            group      = r_group(), summaries = r_summaries(),
            digits     = r_digits(),
            unit       = r_unit(),
            sortable    = isTRUE(r_sortable()),
            collapsible = isTRUE(r_collapsible()),
            search      = isTRUE(r_search()),
            download    = isTRUE(r_download()),
            rotate_titles = isTRUE(r_rotate_titles()),
            active     = act
          ), ad, disp$label_col, disp$value_cols, r_digits(), agg_hex))),
          error = function(e) {
            dt_payload_json(list(kind = "html", html = as.character(
              shiny::tags$div(
                class = "blockr-error", role = "alert",
                paste0("Table could not be rendered: ",
                       conditionMessage(e))
              )
            )))
          }
        ))
      }
      tint <- row_hex_for(d)
      rc_col <- tint$col
      row_hex <- tint$hex
      # Contain any build-time failure (formatting/spread/colour) and
      # show it ON THE PAGE as a red in-block bar instead of letting it
      # escape -- reusing the `blockr-error` style of the framework's
      # condition bar so it reads like an ordinary block error, never a
      # session crash. (Tactical guard; see blockr.core #199 / the design
      # motivation for the first-class side-effect-render seam that would
      # route this through server$conditions() automatically.)
      tryCatch(
        dt_payload_json(titled(with_cards(dt_build_payload(
          d,
          stamp      = stamp_ctrl,
          label_col  = r_rowname(),
          value_cols = r_value(),
          shadings   = r_shadings(),
          drill      = r_drill(),
          digits     = r_digits(),
          unit       = r_unit(),
          row_hex    = row_hex,
          color      = rc_col,
          group      = r_group(), summaries = r_summaries(),
          sortable    = isTRUE(r_sortable()),
          collapsible = isTRUE(r_collapsible()),
          search      = isTRUE(r_search()),
          download    = isTRUE(r_download()),
          rotate_titles = isTRUE(r_rotate_titles()),
          active     = act
        ), d, r_rowname(), r_value(), r_digits(), row_hex))),
        error = function(e) {
          dt_payload_json(list(kind = "html", html = as.character(
            shiny::tags$div(
              class = "blockr-error", role = "alert",
              paste0("Table could not be rendered: ", conditionMessage(e))
            )
          )))
        }
      )
    })

    # Ship the payload. A single plain observe, NOT observeEvent or a
    # split channel: this exact shape is what the blockr.dock lazy-eval
    # card-probe pairing suspends for hidden panels (see the chart
    # block's push observer). The string-identity guard replaces the
    # renderUI-era duplicate-render problem at the source: blockr.core's
    # data() re-delivers identical frames at startup, and an identical
    # payload is simply not sent. `rev` lets the browser skip JSON.parse
    # when a re-send does arrive (e.g. after a reconnect).
    payload_rev  <- 0L
    last_payload <- NULL
    shiny::observe({
      pj <- r_body_payload()
      if (identical(pj, last_payload)) return()
      last_payload <<- pj
      payload_rev  <<- payload_rev + 1L
      session$sendCustomMessage("blockr-viz-table-data", list(
        id = ns("drilldown_table_block"),
        rev = payload_rev,
        payload = pj
      ))
    })

    # Is `drill` the "source" MODE, or a column that happens to be
    # called that? A mode only exists where there is no column to
    # choose -- a structured or aggregated table -- so on a FLAT table
    # `drill = "source"` is a column name like any other and the mode
    # never applies. Resolving against the input rather than against
    # the literal keeps `source` usable as a real column name, the
    # thing `"auto"` never bothered to do.
    drill_is_source <- shiny::reactive({
      if (!identical(r_drill(), "source")) return(FALSE)
      d <- tryCatch(ann_data(), error = function(e) NULL)
      if (!is.data.frame(d)) return(FALSE)
      if ("source" %in% names(d)) return(FALSE)
      dt_is_structured(d) || length(r_group()) > 0L
    })

    list(
      # The expr coerces before filtering, so the block's data output (and
      # the drill filter) is always the annotated data frame -- one
      # downstream contract whether the input was a plain data frame
      # (passthrough) or a table-producing object such as a composer
      # table. Self-qualified so the emitted code stands alone.
      expr = shiny::reactive({
        gc <- r_filter_group_cols()
        gv <- r_filter_group_vals()
        col  <- r_filter_column()
        vals <- r_filter_values()
        # `drill = "source"` wraps whatever the drill built, so every
        # branch (including the undrilled `filter(TRUE)`) goes through
        # one resolution step: no claim resolves -> zero rows, never a
        # silent "everything". The wrap is a plain call around the
        # bbquote result, so the emitted code still stands alone.
        #
        # `columns` tells drill_source() which mode to resolve in, using
        # dd_ctrl_claims()'s rule: dotted names are ARD identity columns
        # (structured table -> identity mode, no `columns`), undotted
        # ones are real source columns (aggregated table -> those are
        # the claim). One convention, two call sites.
        as_output <- function(ex, claim_cols = NULL) {
          if (!drill_is_source()) return(ex)
          cols <- if (length(claim_cols)) {
            claim_cols[!startsWith(claim_cols, ".")]
          }
          call <- list(quote(blockr.viz::drill_source), ex)
          if (length(cols)) call <- c(call, list(columns = cols))
          as.call(call)
        }
        as_output(
        if (length(gc) && length(gc) == length(gv)) {
          # Grouped / structured drill: AND each clicked key. The output
          # is a pure SUBSET of the annotated df -- the identity columns
          # (`.variable`, `.variable_level`, `.group<k>*`) already carry
          # the selection, so no synthetic column is added; downstream
          # consumers read the identity columns themselves.
          cond <- dd_group_filter_call(gc, gv)
          blockr.core::bbquote(
            dplyr::filter(blockr.viz::as_annotated_df(.(data)), .(cond)),
            list(cond = cond)
          )
        } else if (is.null(col) || is.null(vals) || length(vals) == 0) {
          blockr.core::bbquote(
            dplyr::filter(blockr.viz::as_annotated_df(.(data)), TRUE)
          )
        } else if (length(vals) == 1) {
          blockr.core::bbquote(
            dplyr::filter(
              blockr.viz::as_annotated_df(.(data)),
              .data[[.(col)]] == .(val)
            ),
            list(col = col, val = vals[[1]])
          )
        } else {
          blockr.core::bbquote(
            dplyr::filter(
              blockr.viz::as_annotated_df(.(data)),
              .data[[.(col)]] %in% .(vals)
            ),
            list(col = col, vals = vals)
          )
        },
        claim_cols = if (length(gc) && length(gc) == length(gv)) {
          as.character(gc)
        } else if (!is.null(col)) {
          as.character(col)
        }
        )
      }),
      state = list(
        rowname       = r_rowname,
        value         = r_value,
        group         = r_group,
        summaries       = r_summaries,
        color         = r_color,
        shadings      = r_shadings,
        # LEGACY formals (mapped into color/shadings on construction):
        # serialized as NULL so restored boards re-enter through the new
        # args; blockr.core requires every ctor formal in the state.
        cell_color    = function() NULL,
        row_color     = function() NULL,
        drill      = r_drill,
        digits        = r_digits,
        max_height    = r_max_height,
        filter_column = r_filter_column,
        filter_values = r_filter_values,
        # LEGACY formals (never set by the table's JS -- it only emits
        # categorical filters): serialized as NULL, kept as ctor formals
        # so old saved boards restore.
        filter_type   = function() NULL,
        filter_range  = function() NULL,
        filter_group_cols = r_filter_group_cols,
        filter_group_vals = r_filter_group_vals,
        # LEGACY spread formals (the drill no longer adds a synthetic
        # column -- the output is a pure subset and the identity columns
        # are the interface): serialized as NULL, kept as ctor formals so
        # boards saved while the spread existed still restore.
        filter_spread_col  = function() NULL,
        filter_spread_from = function() NULL,
        sortable       = r_sortable,
        collapsible    = r_collapsible,
        search         = r_search,
        download       = r_download,
        rotate_titles  = r_rotate_titles,
        # LEGACY per-format download formals (one `download` toggle now):
        # serialized as NULL, kept as ctor formals so a board saved with
        # `excel_download = TRUE` still restores -- as downloads on.
        excel_download = function() NULL,
        html_download  = function() NULL,
        pptx_download  = function() NULL,
        title          = r_title,
        subtitle       = r_subtitle,
        caption        = r_caption,
        ctrl_target    = r_ctrl_target,
        ctrl_table     = r_ctrl_table
      )
    )
  })
}

#' @noRd
table_records_ui <- function(id) {
  ns <- shiny::NS(id)
  # The table dep ships with the static UI (chart-block parity), not only
  # with the chrome renderUI: the first body payload rides the same flush
  # batch as the chrome HTML, and a handler registered by an async-loading
  # script arrives too late.
  shiny::tagList(
    drilldown_table_dep(),
    shiny::uiOutput(ns("dt_result"))
  )
}
