# The table block's server, on the kit (kit-server.R, kit-output.R).
#
# One view, "table": the gear's settings arrive as `table_set`, clicks as
# `table_select`, the browser's picture as `table_capture`, and the output
# `table` carries the payload. The state is one reactiveVal per spec
# argument. The layout picks the part that builds the table model, the
# emitted filter, the drill claims and the exhibit (R/table-records.R,
# R/table-groups.R, R/table-variables.R); only the current layout's part
# reads the data.
#
# A part is a list of
#   shown()        reactive: the frame the table is built from, which the
#                  titles resolve against and the drill claims read
#   view()         reactive: list(model, empty, derived), the TableModel
#                  or NULL, what to say instead of a table, and extra
#                  derived entries
#   expr(sel)      the emitted filter for the held selection
#   claims(sel)    the conditions a drill target receives, NULL to hold
#   receipt(sel)   the "Filtered: ..." line, or NULL
#   exhibit(tt)    the downloads' viz_exhibit, with the resolved titles
#   title_args(v)  the settings a title can name, beyond the spec's slots

table_server <- function(id, data, init) {
  viz_block_server(
    id, viz_block_spec("table"), data, init, "table",
    function(kit) {
      src <- table_sources(data, kit$state, kit$session)
      parts <- list(
        records = table_records_part(kit, src),
        groups = table_groups_part(kit, src),
        variables = table_variables_part(kit, src)
      )
      part <- function() parts[[kit$state$layout()]]
      titles <- table_titles(kit, src, part)
      # A picture of the old table must not reach a download.
      shiny::observeEvent(
        tryCatch(part()$view()$model, error = function(e) NULL),
        kit$capture(NULL), ignoreInit = TRUE
      )
      list(
        # The rules between arguments: R/table-normalize.R.
        normalize = function(v) table_normalize(v, viz_values(kit$state)),
        payload = table_payload(kit, src, part, titles),
        expr = table_expr(kit, part),
        exhibit = function() {
          part()$exhibit(tryCatch(titles(), error = function(e) list()))
        },
        claims = table_claims(kit, part),
        cond = table_cond(kit, parts)
      )
    }
  )
}

# What every layout reads: the prepare script run on the input
# (R/table-prepare.R), its output coerced to a frame, and the board's
# scale map, subject column and drill targets.
table_sources <- function(data, state, session) {
  prep <- table_prepare(data, r_script = state$script,
                        r_values = state$script_values)
  list(
    prep = prep,
    values = table_view_values(state),
    rows = shiny::reactive({
      d <- prep$data()
      if (is.null(d) || is.data.frame(d)) d else as_annotated_df(d)
    }),
    scale_map = dd_board_scale_map(),
    subject = function() dd_ctrl_subject(session),
    ctrl_choices = dd_ctrl_choices()
  )
}

# The title bands, resolved against the frame the current layout builds its
# table from. Every layout has them, with the spec's slots and the prepare
# script's values as `{@arg}`.
table_titles <- function(kit, src, part) {
  shiny::reactive({
    d <- tryCatch(part()$shown(), error = function(e) NULL)
    shiny::req(is.data.frame(d))
    v <- table_state_values(kit$state)
    args <- script_title_args(part()$title_args(v), src$prep$specs(),
                              v$script_values)
    viz_titles(v, d, input_display_attrs(d), args)
  })
}

# The payload: the state, the current layout's table model as the rows
# (sent again only when it changes), and what R resolves for the face.
table_payload <- function(kit, src, part, titles) {
  rows <- table_model_rows()
  function() {
    v <- table_state_values(kit$state)
    built <- part()$view()
    derived <- c(
      list(
        titles = tryCatch(titles(), error = function(e) NULL),
        empty = built$empty,
        receipt = if (!kit$transient()) part()$receipt(v$selection)
      ),
      built$derived,
      list(
        script = list(inputs = dd_script_roles(src$prep$specs()),
                      values = dd_script_cfg(src$prep$specs(),
                                             v$script_values),
                      error = src$prep$prepared()$error),
        ctrl_choices = dd_ctrl_choices_list(src$ctrl_choices()),
        capture_ratio = canvas_capture_ratio()
      )
    )
    viz_payload(kit$spec, v, rows(built$model), kit$revs,
                derived = derived[!vapply(derived, is.null, logical(1L))])
  }
}

#' The table model as the payload's rows, written once per content
#'
#' The model goes into `payload.data`; its revision goes up only when the
#' model changes, so a click or a title edit sends the state and the
#' browser keeps the table it drew, with its sort and folds. No table is
#' `{}`.
#' @noRd
table_model_rows <- function() {
  last <- new.env(parent = emptyenv())
  last$rev <- 0L
  last$data <- NULL
  function(model) {
    data <- model %||% structure(list(), names = character())
    if (!identical(data, last$data)) {
      last$rev <- last$rev + 1L
      last$data <- data
    }
    list(data = data, rev = last$rev, columns = NULL)
  }
}

# The emitted filter. The same object comes back while the code is
# unchanged: blockr.core skips a re-evaluation on identity.
table_expr <- function(kit, part) {
  last <- new.env(parent = emptyenv())
  shiny::reactive({
    out <- part()$expr(kit$state$selection())
    if (!identical(out, last$expr)) last$expr <- out
    last$expr
  })
}

# What a drill target receives: the last click with a target set, else the
# held selection.
table_claims <- function(kit, part) {
  shiny::reactive({
    if (kit$transient()) {
      c1 <- kit$claim()
      if (is.null(c1)) {
        return(NULL)
      }
      return(part()$claims(c1$selection))
    }
    part()$claims(kit$state$selection())
  })
}

# The block's conditions: a failed custom summary is an error of the
# block, shown with it, while the groups table draws the other columns. It
# counts only while that layout is on.
table_cond <- function(kit, parts) {
  cond <- shiny::reactiveValues(error = list())
  shiny::observe({
    errs <- if (identical(kit$state$layout(), "groups")) {
      tryCatch(parts$groups$failed(), error = function(e) list())
    } else {
      list()
    }
    if (!identical(shiny::isolate(cond$error), errs)) cond$error <- errs
  })
  cond
}

# The browser's last picture of the table, as the exhibit's picture.
table_picture <- function(cap) {
  if (is.null(cap)) {
    return(NULL)
  }
  tryCatch(new_chart_capture(chart_capture_decode(cap$png), cap$width,
                             cap$height),
           error = function(e) NULL)
}

# The arguments that are the face's alone: a click, the title bands, the
# drill target, the search box and the download menu. The table model does
# not read them, so changing one rebuilds nothing.
TABLE_FACE_ARGS <- c("selection", "title", "subtitle", "caption",
                     "drill_target", "drill_table", "download", "search")

# The values the table model is built from: every other argument, as one
# reactive that moves only when one of them does.
table_view_values <- function(state) {
  keep <- setdiff(names(state), TABLE_FACE_ARGS)
  rv <- shiny::reactiveVal(shiny::isolate(table_state_values(state[keep])))
  shiny::observe({
    v <- table_state_values(state[keep])
    if (!identical(v, shiny::isolate(rv()))) rv(v)
  }, priority = 50)
  rv
}

# Every value of the state, read reactively (viz_values() isolates).
table_state_values <- function(state) {
  lapply(state, function(rv) rv())
}
