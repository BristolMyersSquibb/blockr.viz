# new_heatmap_block(): the matrix heatmap ENGINE. Long event rows in (one
# row per event), the rendered row x column matrix out -- the block
# aggregates ITSELF (cell = event count + worst level of `color`), so no
# upstream reshape block is needed. The block's result is the matrix itself
# (heatmap_matrix()), so the assistant and a document read what is on screen.
# A row click never filters anything: it is an event sent over the control
# bridge to the board's drill filter (ctrl_auto_target()), as the composer
# table does. Renderer: R/heatmap-html.R; JS: inst/js/heatmap-block.js.
#
# The engine knows no AE words. It draws every column it is handed; a cap
# such as "the 25 most frequent terms" is the prepare script's business, and
# the AE surface ships that script. Title, sentence and gear follow the chart
# block: `{@arg}` words in the subtitle are controls, script values join them.
#
# Deliberately NOT in this package's registry: the registered surface is
# blockr.pharma::new_ae_heatmap_block() -- same formals, AE defaults, and
# it stamps ITSELF as the serialized ctor (via the ctor/ctor_pkg
# passthrough below), so study boards restore against blockr.pharma. This
# ctor stays exported for the pharma delegation and for non-pharma reuse
# from code.

#' Heatmap block
#'
#' Renders long event rows (e.g. one row per adverse event) as a row x
#' column matrix: the cell DISPLAYS the event count and is PAINTED by the
#' worst level of `color` -- two channels, the classic AE heatmap form.
#' Columns order by event count, most first; the block draws every column
#' it gets, so a cap belongs in the prepare `script`. Rows order by `group`
#' (factor order), then total burden descending; each group starts with a
#' header row.
#'
#' @param row Column identifying a matrix ROW (e.g. `"USUBJID"`).
#' @param col Column identifying a matrix COLUMN (e.g. the preferred term).
#' @param color Optional column whose WORST level per cell drives the
#'   paint. An ordered factor keeps its own level order (vocabularies live
#'   in the data); numeric grades order numerically; a bare character
#'   column falls back to sorted unique values. Empty: cells paint by
#'   count instead.
#' @param group Optional column grouping the rows (one group value per row
#'   identity, e.g. the treatment arm); each group starts with a header row.
#' @param top_n LEGACY. Folded into the prepare script: a board saved with
#'   it restores it as the script's `top_n` value.
#' @param cell_numbers Show the count in each cell (default `TRUE`). Off,
#'   the matrix reads as a pure color heatmap.
#' @param drill LEGACY. A row click always goes to the board's drill filter,
#'   when the board has one (see [ctrl_auto_target()]).
#' @param download Logical (default `FALSE`), exposed in the gear: the
#'   header row grows the shared download tool -- the matrix (row identity,
#'   group, count columns) written as xlsx / html / pptx.
#' @param filter_column,filter_values LEGACY. The block no longer filters.
#' @param max_height LEGACY. The matrix scrolls with its panel.
#' @param ctrl_target,ctrl_table LEGACY. The target is found by the board,
#'   see [ctrl_auto_target()].
#' @param title,subtitle,caption Text above and below the matrix, as in
#'   [new_chart_block()]: `NULL` = the input's own label, `""` = none, else a
#'   template. `{@arg}` in the subtitle makes the word a control.
#' @param script,values Prepare script, as in [new_chart_block()]: R code run
#'   on the incoming rows before the matrix is built, whose top assignments
#'   become settings. `values` holds their current settings.
#' @param ... Forwarded to [blockr.core::new_transform_block()]. A package
#'   building on this block passes its own `class` here (blockr.pharma's AE
#'   heatmap): it is prepended to `"heatmap_block"` at construction, which is
#'   where block metadata is resolved from the registry. It is not a formal
#'   because every constructor formal has to come back out as block state.
#' @return A transform block of class `heatmap_block`, whose result is the
#'   matrix ([heatmap_matrix()]).
#' @examplesIf interactive()
#' new_heatmap_block(row = "USUBJID", col = "AEDECOD", color = "AESEV")
#' @export
new_heatmap_block <- function(row = character(),
                              col = character(),
                              color = character(),
                              group = character(),
                              top_n = NULL,        # LEGACY: script value top_n
                              cell_numbers = TRUE,
                              drill = NULL,        # LEGACY: bridge only
                              download = FALSE,
                              filter_column = NULL, # LEGACY: no filter
                              filter_values = NULL, # LEGACY: no filter
                              max_height = NULL,   # LEGACY: panel scroll
                              ctrl_target = NULL,  # LEGACY: auto target
                              ctrl_table = NULL,   # LEGACY: auto target
                              title = NULL,
                              subtitle = NULL,
                              caption = NULL,
                              script = NULL,
                              values = list(),
                              ...) {
  args <- list(...)
  cls <- c(args[["class"]], "heatmap_block")
  args[["class"]] <- NULL

  row <- chr_state(row)
  col <- chr_state(col)
  color <- chr_state(color)
  group <- chr_state(group)
  title <- title_state(title)
  subtitle <- title_state(subtitle)
  caption <- title_state(caption)
  script <- cb_script_text(script)
  values <- if (is.list(values)) values else list()

  # LEGACY. A board saved before the cap moved into the prepare script carries
  # top_n as an argument; it becomes the script's value of the same name. A
  # script that declares no top_n ignores it.
  if (!is.null(top_n) && is.null(values$top_n)) {
    values$top_n <- as.integer(top_n)[[1L]]
  }

  do.call(blockr.core::new_transform_block, c(list(
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        raw_data <- shiny::reactive(coerce_plain_df(data()))

        r_row      <- shiny::reactiveVal(row)
        r_col      <- shiny::reactiveVal(col)
        r_color    <- shiny::reactiveVal(color)
        r_group    <- shiny::reactiveVal(group)
        r_numbers  <- shiny::reactiveVal(isTRUE(cell_numbers))
        r_download <- shiny::reactiveVal(isTRUE(download))
        r_title    <- shiny::reactiveVal(title)
        r_subtitle <- shiny::reactiveVal(subtitle)
        r_caption  <- shiny::reactiveVal(caption)
        r_script   <- shiny::reactiveVal(script)
        r_values   <- shiny::reactiveVal(values)

        # --- prepare script ------------------------------------------------
        # The chart block's step, unchanged (R/prepare-apply.R). Specs keep
        # their last good value while the data is momentarily gone, or a
        # hidden panel would flip the compiled script back and forth.
        r_parsed <- shiny::reactive(cb_parse(r_script()))
        last_specs <- new.env(parent = emptyenv())
        last_specs$value <- list()
        r_specs <- shiny::reactive({
          d <- tryCatch(raw_data(), error = function(e) NULL)
          parsed <- r_parsed()
          if (is.null(d) && length(last_specs$value)) {
            return(last_specs$value)
          }
          out <- cb_specs(parsed, d)
          if (!identical(out, last_specs$value)) last_specs$value <- out
          last_specs$value
        })
        r_prepared <- shiny::reactive({
          dd_prepare_run(raw_data(), r_parsed(), r_specs(), r_values())
        })
        # A failed script draws nothing (an empty frame with the upstream's
        # columns, not NULL, so the error still reaches the gear).
        plain_data <- shiny::reactive({
          prep <- r_prepared()
          if (is.null(prep$error)) return(prep$data)
          d <- tryCatch(raw_data(), error = function(e) NULL)
          if (is.data.frame(d)) d[0, , drop = FALSE] else NULL
        })

        # --- title, sentence, caption --------------------------------------
        r_data_titles <- shiny::reactive({
          d <- tryCatch(data(), error = function(e) NULL)
          input_display_attrs(d)
        })
        title_args <- shiny::reactive({
          script_title_args(
            list(row = r_row(), col = r_col(), color = r_color(),
                 group = r_group()),
            r_specs(), r_values()
          )
        })
        r_titles <- shiny::reactive({
          d <- plain_data()
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
          list(
            title = resolve_block_title(r_title(), d, auto = auto$label,
                                        args = a),
            subtitle = resolve_block_title(r_subtitle(), d,
                                           auto = auto$subtitle, args = a),
            caption = resolve_block_title(r_caption(), d,
                                          auto = auto$caption, args = a),
            parts = parts,
            arg_values = lapply(stats::setNames(nm = names(a)),
                                arg_token_value, args = a)
          )
        })

        # The drill, as the composer table has it. A row click is an EVENT
        # sent over the control bridge to the board's drill filter: nothing
        # latches here, the block does not filter its own result, a second
        # click on the same row sends again, and the undo lives at the target.
        # The target is the board's answer (ctrl_auto_target()); a board with
        # none leaves the rows inert.
        r_target <- shiny::reactive(ctrl_auto_target(session))
        r_drill_claim <- shiny::reactiveVal(NULL)

        upd <- function(rv, v) {
          if (!identical(shiny::isolate(rv()), v)) rv(v)
        }
        blank <- function(v) {
          v <- as.character(v %||% "")
          if (!length(v) || !nzchar(v[1L]) || identical(v[1L], "(none)")) {
            character()
          } else {
            v[1L]
          }
        }
        flag <- function(v) {
          isTRUE(v) || identical(v, "true") || identical(v, "on")
        }

        shiny::observeEvent(input$heatmap_block_action, {
          msg <- input$heatmap_block_action
          if (is.null(msg)) return()
          act <- msg$action %||% "config"
          if (identical(act, "filter")) {
            if (!is.null(msg$column) && length(msg$values)) {
              r_drill_claim(list(
                column = as.character(msg$column)[[1L]],
                values = as.character(unlist(msg$values)),
                nonce  = as.numeric(msg$nonce %||% 0)
              ))
            }
          } else if (identical(act, "config")) {
            p <- as.character(msg$param %||% "")[[1L]]
            v <- msg$value
            if (startsWith(p, "sv_")) {
              # A script value. dd_script_values() reads it against the
              # current specs, which know the declared type.
              sv <- dd_script_values(stats::setNames(list(v), p), r_specs(),
                                     r_values())
              if (!identical(sv, r_values())) r_values(sv)
              return()
            }
            switch(
              p,
              row     = upd(r_row, blank(v)),
              col     = upd(r_col, blank(v)),
              color   = upd(r_color, blank(v)),
              group   = upd(r_group, blank(v)),
              cell_numbers = upd(r_numbers, flag(v)),
              download = upd(r_download, flag(v)),
              # "" is a real value for the text (no title at all); NULL from
              # the client is the auto tier.
              title    = upd(r_title, title_state(v)),
              subtitle = upd(r_subtitle, title_state(v)),
              caption  = upd(r_caption, title_state(v)),
              # Committed on blur or Apply, never per keystroke.
              script   = upd(r_script, cb_script_text(as.character(v %||% ""))),
              NULL
            )
          }
        })

        # The claim, against the board's data model: the drill filter places
        # it (dd_ctrl_claims() with no table names the column as it stands).
        r_ctrl_claims <- shiny::reactive({
          claim <- r_drill_claim()
          if (is.null(claim)) return(NULL)
          d <- tryCatch(raw_data(), error = function(e) NULL)
          dd_ctrl_claims(d, "", stats::setNames(list(claim$values),
                                                claim$column),
                         subject = dd_ctrl_subject(session))
        })
        dd_ctrl_sender(
          r_target,
          r_ctrl_claims,
          session = session,
          r_nonce = function() {
            claim <- r_drill_claim()
            if (is.null(claim)) NULL else claim$nonce
          }
        )

        dl_exhibit <- function() {
          d <- tryCatch(plain_data(), error = function(e) NULL)
          p <- heatmap_prep(d, one_or_null(r_row()), one_or_null(r_col()),
                            one_or_null(r_color()), one_or_null(r_group()))
          if (!is.null(p$err)) {
            return(list(data = data.frame(message = p$err),
                        dataset = dl_dataset(d)))
          }
          out <- hmb_exhibit_frame(
            p, d, tryCatch(board_scale_map(), error = function(e) NULL)
          )
          tt <- tryCatch(r_titles(), error = function(e) list())
          list(data = out, title = tt$title, subtitle = tt$subtitle,
               caption = tt$caption, dataset = dl_dataset(d), source = d)
        }
        # The export picture: the heatmap as the browser drew it, posted by
        # heatmap-block.js when the download menu opens (the summarize
        # table's route, R/chart-capture.R). The deck and the image carry
        # it; without one the exhibit above is the fallback.
        capture <- shiny::reactiveVal(NULL)
        shiny::observeEvent(input$heatmap_block_capture, {
          capture(chart_capture_from_msg(input$heatmap_block_capture))
        })
        # With the board's download footer under it (R/download-footer.R).
        dl_slot <- dt_download_control(
          session, dl_exhibit,
          enabled = r_download,
          filename = "heatmap",
          picture = function() {
            download_footer_stamp(capture(), data(), session)
          }
        )

        board_scale_map <- dd_board_scale_map()

        # The status line's right half: the receipt of the last send. Its own
        # small output so a click never re-renders the matrix.
        output$heatmap_status <- shiny::renderUI({
          claim <- r_drill_claim()
          if (is.null(claim) || !nzchar(r_target())) return(NULL)
          hmb_status_tag(receipt = paste0(
            "Drilled down to ", claim$column, " = ",
            paste(claim$values, collapse = ", ")
          ))
        })

        # Read by the chrome as well, which mounts before the data exists:
        # nothing here may throw on an upstream that is not ready, or the
        # isolated render stops silently and never comes back.
        cfg_json <- function() {
          tt <- tryCatch(r_titles(), error = function(e) NULL)
          specs <- r_specs()
          script_error <- tryCatch(r_prepared()$error,
                                   error = function(e) NULL)
          hmb_cfg_json(
            row = one_or_null(r_row()), col = one_or_null(r_col()),
            color = one_or_null(r_color()), group = one_or_null(r_group()),
            cell_numbers = r_numbers(),
            download = r_download(),
            target = tryCatch(r_target(), error = function(e) ""),
            titles = list(
              title = r_title(), subtitle = r_subtitle(),
              caption = r_caption(), resolved = tt
            ),
            script = list(
              text = r_script(),
              inputs = dd_script_roles(specs),
              error = script_error,
              cfg = dd_script_cfg(specs, r_values())
            )
          )
        }

        output$heatmap_result <- shiny::renderUI({
          shiny::isolate(
            hmb_chrome(
              elem_id = ns("heatmap_block"),
              cell_numbers = r_numbers(),
              cfg_json = cfg_json(),
              row_col = one_or_null(r_row()) %||% "",
              download_slot = dl_slot,
              status = shiny::uiOutput(ns("heatmap_status"), inline = TRUE)
            )
          )
        })

        last_msg <- new.env(parent = emptyenv())
        last_msg$json <- NULL
        last_msg$rev <- 0L

        # Sent once the current flush is out. A custom message goes on the
        # socket at once, ahead of the flush's output values, and on a panel's
        # first visit those values carry the chrome that loads
        # heatmap-block.js. Shiny drops a message with no handler, so the
        # payload was lost and the `_ready` reply below re-sent it after
        # every other output of the session (about 300 ms on a first open).
        # After the flush, the client handles the chrome, waits for its
        # script, and then reads the payload.
        push <- function(json) {
          rev <- last_msg$rev
          session$onFlushed(function() {
            if (rev != last_msg$rev) return()
            session$sendCustomMessage("blockr-viz-heatmap-data", list(
              id = ns("heatmap_block"), rev = rev, payload = json
            ))
          }, once = TRUE)
        }

        # The client announces itself when it binds with nothing to render.
        # Shiny DROPS a custom message that has no registered handler yet, and
        # heatmap-block.js only loads with the first heatmap block UI in the
        # page -- on a board whose opening view carries none, the startup
        # payload is lost and the identity guard below would never re-send it.
        shiny::observeEvent(input$heatmap_block_ready, {
          if (!is.null(last_msg$json)) push(last_msg$json)
        })

        shiny::observe({
          d <- plain_data()
          shiny::req(is.data.frame(d))
          body <- hmb_body(
            d,
            row = one_or_null(r_row()), col = one_or_null(r_col()),
            color = one_or_null(r_color()), group = one_or_null(r_group()),
            scale_map = board_scale_map()
          )
          json <- as.character(jsonlite::toJSON(
            list(
              err = body$err %||% "",
              errKind = body$err_kind %||% "",
              # The gear and the header read their state off the payload: the
              # chrome is mounted once and never rebuilt, so a gear edit is a
              # config change followed by a push.
              config = cfg_json(),
              legend = body$legend %||% "",
              # The <table> shell plus its rotated header, and the sparse cell
              # model the client assembles the rows from (~7 KB where the
              # pasted HTML was ~157 KB on a 194 x 25 AE heatmap).
              head = body$head %||% "",
              model = hmb_model_payload(body$model),
              count = body$count %||% "",
              rowCol = body$row_col %||% "",
              cols = body$cols %||% "[]",
              # The cell-numbers flag and the active rows are applied by the
              # JS the instant they are clicked; they travel here so a
              # restore -- or a panel re-mount off the client-side cache --
              # comes back in the state the server holds.
              cellNumbers = isTRUE(shiny::isolate(r_numbers())),
              drill = nzchar(r_target()),
              capture = TRUE,
              captureRatio = canvas_capture_ratio(),
              capturePage = capture_page_box()
            ),
            auto_unbox = TRUE, null = "null"
          ))
          if (identical(json, last_msg$json)) return()
          last_msg$json <- json
          last_msg$rev <- last_msg$rev + 1L
          # A picture of the old matrix must not reach a download.
          capture(NULL)
          push(json)
        })

        list(
          # The matrix, built from the prepared rows: the input, coerced to a
          # plain frame when it is not one, the prepare script around it, and
          # heatmap_matrix() around that. Until the row and column are picked
          # the result is the prepared rows.
          expr = shiny::reactive({
            src <- quote(.(data))
            d <- tryCatch(data(), error = function(e) NULL)
            if (!is.null(d) && !is.data.frame(d)) {
              src <- as.call(list(quote(blockr.viz::as_plain_df), src))
            }
            sx <- cb_expr(r_parsed(), r_specs(), r_values(), slot = TRUE)
            prepared <- if (is.null(sx)) src else dd_splice_slot(sx, src)
            row <- one_or_null(r_row())
            col <- one_or_null(r_col())
            if (is.null(row) || is.null(col)) return(prepared)
            args <- list(prepared, row = row, col = col,
                         color = one_or_null(r_color()),
                         group = one_or_null(r_group()))
            as.call(c(list(quote(blockr.viz::heatmap_matrix)),
                      Filter(Negate(is.null), args)))
          }),
          state = list(
            row = r_row, col = r_col, color = r_color, group = r_group,
            top_n = function() NULL,
            cell_numbers = r_numbers, drill = function() NULL,
            download = r_download,
            filter_column = function() NULL,
            filter_values = function() NULL,
            max_height = function() NULL,
            ctrl_target = function() NULL,
            ctrl_table = function() NULL,
            title = r_title, subtitle = r_subtitle, caption = r_caption,
            script = r_script, values = r_values
          )
        )
      })
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(shiny::uiOutput(ns("heatmap_result")))
    },
    dat_valid = validate_annotated_df_input,
    allow_empty_state = c("row", "col", "color", "group", "drill",
      "top_n", "max_height", "filter_column", "filter_values", "ctrl_target",
      "ctrl_table", "title", "subtitle", "caption", "script", "values"),
    external_ctrl = c("row", "col", "color", "group", "cell_numbers",
      "download", "title", "subtitle", "caption", "script", "values"),
    expr_type = "bquoted",
    class = cls
    # `ctor`/`ctor_pkg` deliberately ride `...` and are NOT formals: the
    # framework passes them itself (registry harvest, deser restore), and a
    # formal would both collide with that injection and leak into the
    # serialized state (initial_block_state = the recorded ctor's formals).
    # blockr.pharma's new_ae_heatmap_block stamps its own identity the same
    # way -- through `...`.
  ), args))
}

#' First element or NULL -- the renderer wants a scalar or nothing.
#' @noRd
one_or_null <- function(x) {
  x <- as.character(x %||% character())
  if (length(x) && nzchar(x[1L])) x[1L] else NULL
}

#' @noRd
heatmap_block_dep <- memoise0(function() {
  htmltools::tagList(
    blockr.ui::controls_dep(),
    drilldown_shared_dep(),
    # The gear engine's styles (dd-*) live in chart.css, as for the table.
    chart_css_dep(),
    htmltools::htmlDependency(
      name = "heatmap-block",
      version = paste0(utils::packageVersion("blockr.viz"), ".5"),
      src = system.file(package = "blockr.viz"),
      script = "js/heatmap-block.js",
      stylesheet = "css/heatmap-block.css"
    )
  )
})
