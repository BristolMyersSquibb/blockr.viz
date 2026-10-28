# The table block's server, on the kit (kit-server.R, kit-output.R). One view,
# "table": settings arrive as `table_set`, clicks as `table_select`, the
# browser's picture as `table_capture` (decoded into `kit$capture()`); the
# output `table` carries the payload.
# The state is one reactiveVal per spec argument. The layout picks the part
# (R/table-records.R, R/table-groups.R, R/table-variables.R); only the current
# layout's part reads the data.
#
# A part is a list of
#   frame()        reactive: the frame the table is built from (the rows,
#                  nested under `by`, for records; the Table 1 for
#                  variables; the input for groups), which the titles
#                  resolve against, the drill claims read and the
#                  downloads write
#   drawn()        reactive: list(model, empty, derived), the TableModel
#                  or NULL, what to say instead of a table, and extra
#                  derived entries
#   expr(sel)      the emitted filter for the held selection
#   claims(sel)    the conditions a drill target receives, NULL to hold
#   receipt(sel)   the "Filtered: ..." line, or NULL
#   exhibit(tt)    the downloads' viz_exhibit, with the title texts
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
      list(
        normalize = function(v) table_normalize(v, viz_values(kit$state)),
        payload = table_payload(kit, src, part, titles),
        expr = table_expr(kit, part),
        exhibit = function() {
          tt <- tryCatch(titles(), error = function(e) list())
          part()$exhibit(lapply(tt[c("title", "subtitle", "caption")],
                                function(b) b$text))
        },
        claims = table_claims(kit, part),
        cond = table_cond(kit, parts),
        script = src$prep
      )
    }
  )
}

# What every layout reads: the prepare script (`prep`, table_prepare()), the
# prepared input as a frame, the board's scale map and subject column.
table_sources <- function(data, state, session) {
  prep <- table_prepare(data, state)
  rows <- shiny::reactive({
    d <- prep$prepared()
    if (is.null(d) || is.data.frame(d)) d else as_annotated_df(d)
  })
  list(
    prep = prep,
    values = table_model_values(state),
    rows = rows,
    columns = shiny::reactive(
      viz_wire_columns(tryCatch(rows(), error = function(e) NULL))
    ),
    scale_map = dd_board_scale_map(),
    subject = function() ctrl_subject(session)
  )
}

# The title bands, resolved against the frame the current layout builds its
# table from; the spec's slots and the prepare script's values are `{@arg}`.
table_titles <- function(kit, src, part) {
  shiny::reactive({
    d <- tryCatch(part()$frame(), error = function(e) NULL)
    shiny::req(is.data.frame(d))
    v <- viz_values(kit$state)
    args <- script_title_args(part()$title_args(v), src$prep$specs(),
                              v$script_values)
    viz_titles(v, d, input_display_attrs(d), args)
  })
}

# The payload: the state, the current layout's table model as the rows
# (sent again only when it changes), and what R resolves for the face.
table_payload <- function(kit, src, part, titles) {
  rows <- viz_rows()
  function() {
    v <- viz_values(kit$state)
    built <- part()$drawn()
    derived <- c(
      list(
        titles = tryCatch(titles(), error = function(e) NULL),
        empty = built$empty,
        receipt = if (!kit$transient()) part()$receipt(v$selection)
      ),
      built$derived
    )
    # No table is `{}`; `columns` are the input's, for the gear.
    data <- built$model %||% structure(list(), names = character())
    viz_payload(kit$spec, v, rows(data, src$columns()), kit$revs,
                derived = derived[!vapply(derived, is.null, logical(1L))])
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
      # A table sends no clear with a drill target; one would hold.
      if (is.null(c1) || is.null(c1$selection)) {
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

# The arguments that are the face's alone: a click, the title bands, the
# drill target, the search box and the download menu. The table model does
# not read them, so changing one rebuilds nothing.
TABLE_FACE_ARGS <- c("selection", "title", "subtitle", "caption",
                     "drill_target", "drill_table", "download", "search")

# The values the table model is built from: every other argument, as one
# reactive that moves only when one of them does.
table_model_values <- function(state) {
  keep <- setdiff(names(state), TABLE_FACE_ARGS)
  rv <- shiny::reactiveVal(shiny::isolate(viz_values(state[keep])))
  shiny::observe({
    v <- viz_values(state[keep])
    if (!identical(v, shiny::isolate(rv()))) rv(v)
  }, priority = 50)
  rv
}
