# How the table recordings talk to the rewritten table (R/table-server.R on
# the kit). The recordings were made against the reference with its browser
# messages (helper-table-golden.R). This target feeds the same steps through
# the kit's inputs, as the new gear and table view would send them, and
# reads what the block holds, emits and draws.
#
# What is compared, per step:
#   state, starting and saved state  under the public names and the kit's
#                          selection shape (legacy_restore())
#   expr, result, conditions, ctrl, download  as recorded
#   browser                what a reader sees once the step is drawn: the
#                          number of rows, the row labels of a flat table,
#                          the title bands and the "Filtered: ..." line. The
#                          table model itself is pinned by the view tests
#                          (test-table-records.R, test-table-structured.R,
#                          test-table-groups-view.R).

# --- running ------------------------------------------------------------------

tg_rw_run <- function(sc) {
  table_golden_register_tbl()
  # The reference took the prepare script and its values from the gear; the
  # kit does so only where a deployment allows it.
  withr::local_options(utils::modifyList(
    list(blockr.viz.default_caption = NULL, useFancyQuotes = FALSE,
         blockr.viz.browser_script = TRUE),
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
  blk <- withCallingHandlers(
    table_golden_ctor(tg_rw_args(sc$args)),
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
      expr <- session$makeScope("expr")
      cx <- new.env(parent = emptyenv())
      cx$data <- function() src$d
      session$flushReact()
      out$steps[[1L]] <- tg_rw_record(session, expr, m$sends, cx)
      for (i in seq_along(sc$steps)) {
        s <- sc$steps[[i]]
        dl <- NULL
        if (!is.null(s$input)) {
          vals <- tg_rw_inputs(s, session, expr, cx)
          if (length(vals)) suppressWarnings(do.call(expr$setInputs, vals))
        } else if (!is.null(s$data)) {
          src$d <- table_golden_data(s$data)
          tick(i)
        } else if (!is.null(s$download)) {
          session$flushReact()
          dl <- tg_rw_download(table_golden_download(
          expr$output, sub("^(records|groups)-", "", s$download)
        ))
        }
        session$flushReact()
        rec <- tg_rw_record(session, expr, m$sends, cx)
        rec$download <- dl
        out$steps[[i + 1L]] <- rec
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
    initial = tg_rw_sort_dir(initial),
    saved = tg_rw_sort_dir(saved),
    steps = out$steps
  ))
}

# A scenario's arguments under the public names. `max_height` is gone
# (api-proposal.md section 2): the table scrolls with its panel.
tg_rw_args <- function(args) {
  args <- table_golden_translate(list(args = args))$args
  args$max_height <- NULL
  if (is.list(args$shadings)) {
    args$shadings <- lapply(args$shadings, legacy_shading_rule)
  }
  if (is.list(args$summaries)) {
    args$summaries <- lapply(args$summaries, legacy_summary_entry)
  }
  args
}

tg_rw_record <- function(session, expr, sends, cx) {
  st <- session$returned$state
  state <- lapply(st, function(rv) table_golden_value(rv))
  list(
    state = tg_rw_sort_dir(state),
    expr = table_golden_value(function() session$returned$expr()),
    result = table_golden_value(function() {
      r <- session$returned$result()
      if (is.data.frame(r)) as.data.frame(r) else r
    }),
    conditions = table_golden_value(function() {
      table_golden_conditions(session$returned$cond())
    }),
    browser = tg_rw_browser(expr, cx),
    ctrl = sends$log
  )
}

# What the reader sees, from the payload the output holds. A payload
# without rows keeps the table the browser drew last.
tg_rw_browser <- function(expr, cx) {
  p <- tryCatch(expr$output$table, error = function(e) NULL)
  if (is.null(p)) {
    return(cx$last_view)
  }
  if (!is.null(p[["data"]])) cx$model <- p[["data"]]
  d <- p$derived
  m <- if (is.null(d$empty)) cx$model
  labels <- if (length(m$cols) && is.null(m$sections)) {
    tg_text(m$cols[[(m$stub %||% 0L) + 1L]]$disp)
  }
  cx$last_view <- tg_view(
    n = if (length(m$n)) as.integer(m$n), labels = labels,
    titles = lapply(d$titles[c("title", "subtitle", "caption")],
                    function(b) b$text),
    receipt = d$receipt
  )
  cx$last_view
}

tg_view <- function(n = NULL, labels = NULL, titles = NULL, receipt = NULL) {
  titles <- lapply(titles %||% list(), function(x) {
    if (is.character(x) && length(x) == 1L && nzchar(x)) x
  })
  titles <- titles[!vapply(titles, is.null, logical(1L))]
  out <- list(n = n, labels = labels,
              titles = if (length(titles)) titles[sort(names(titles))],
              receipt = receipt)
  out[!vapply(out, is.null, logical(1L))]
}

tg_text <- function(x) {
  vapply(x, function(v) if (is.null(v)) NA_character_ else as.character(v),
         character(1L), USE.NAMES = FALSE)
}

# --- steps --------------------------------------------------------------------

# A reference step as the new browser sends it: a gear edit as `table_set`
# with the public name and value, a click as `table_select`, a picture as
# `table_capture`. NULL for a step the new protocol has no counterpart for
# (the old engines' ready handshake).
tg_rw_inputs <- function(s, session, expr, cx) {
  v <- s$value
  if (identical(s$input, "groups-summarize_table_block_ready")) {
    return(NULL)
  }
  if (identical(s$input, "groups-summarize_table_block_capture")) {
    return(list(table_capture = viz_read_capture(v)))
  }
  if (identical(s$input, "variables-summary_input")) {
    return(tg_rw_set(tg_rw_widget(v, session, cx), expr))
  }
  act <- v$action %||% "config"
  if (identical(act, "config")) {
    vals <- tg_rw_config(v$param, v$value, session, cx)
    return(if (!is.null(vals)) tg_rw_set(vals, expr))
  }
  sel <- if (startsWith(s$input, "groups-")) {
    tg_rw_groups_click(v)
  } else {
    tg_rw_records_click(v)
  }
  sel <- if (!is.null(sel)) {
    jsonlite::fromJSON(jsonlite::toJSON(sel, auto_unbox = TRUE),
                       simplifyVector = FALSE)
  }
  list(table_select = viz_read_selection(list(selection = sel,
                                              nonce = v$nonce %||% 0)))
}

tg_rw_set <- function(values, expr) {
  rev <- tryCatch(expr$output$table$state_rev, error = function(e) 0L)
  list(table_set = viz_read_set(viz_block_spec("table"),
                                list(rev = rev %||% 0L,
                                     values = tg_rw_json(values))))
}

# Settings as they arrive from the browser: through JSON.
tg_rw_json <- function(x) {
  spec <- viz_block_spec("table")
  out <- list()
  for (nm in names(x)) {
    arg <- spec$args[[nm]]
    w <- if (is.null(arg)) x[nm] else list(wire_value(arg$type, x[[nm]]))
    out[nm] <- list(jsonlite::fromJSON(
      wire_json(stats::setNames(w, nm)), simplifyVector = TRUE,
      simplifyDataFrame = FALSE
    )[[nm]])
  }
  out
}

# The reference's gear keys and values to the public ones. Invalid values
# go through: the new gear cannot send them and the spec refuses them, as
# the reference ignored them.
tg_rw_config <- function(param, value, session, cx) {
  param <- as.character(param %||% "")[1L]
  if (!nzchar(param)) {
    return(NULL)
  }
  st <- session$returned$state
  onoff <- function(x) identical(as.character(unlist(x))[1L], "on")
  none <- function(x) {
    x <- as.character(unlist(x))
    x <- x[!is.na(x) & nzchar(x) & x != "(none)"]
    if (length(x)) x[[1L]]
  }
  cols <- function(x) {
    x <- as.character(unlist(x))
    x[!is.na(x) & nzchar(x)]
  }
  if (startsWith(param, "sv_")) {
    sv <- shiny::isolate(st$script_values()) %||% list()
    sv[[sub("^sv_", "", param)]] <- utils::type.convert(
      as.character(unlist(value)), as.is = TRUE
    )
    return(list(script_values = sv))
  }
  switch(
    param,
    group = , by = list(by = cols(value)),
    rowname = list(rowname = none(value)),
    columns = list(columns = cols(value)),
    color = list(color = none(value)),
    drill = list(drill = none(value)),
    shadings = list(shadings = lapply(dd_parse_shadings(value),
                                      legacy_shading_rule)),
    summaries = list(summaries = lapply(value, legacy_summary_entry)),
    digits = list(format = table_digits_format(value)),
    format = list(format = tg_rw_format(value)),
    unit = list(unit = trimws(as.character(unlist(value))[1L])),
    sortable = , collapsible = , search = , download = , value_labels = ,
    axis = stats::setNames(list(onoff(value)),
                           if (param == "axis") "column_axis" else param),
    rotate_titles = if (onoff(value)) list(look = "compact"),
    pct_of = list(percent_of = value),
    sort_by = list(sort_by = if (identical(value, "label")) "alpha" else value),
    top_n = tg_rw_top_n(value),
    ctrl_target = list(drill_target = trimws(as.character(value %||% ""))),
    ctrl_table = list(drill_table = trimws(as.character(value %||% ""))),
    title = , subtitle = , caption = stats::setNames(
      list(if (!is.null(value)) as.character(value)[[1L]]), param
    ),
    stats::setNames(list(value), param)
  )
}

# The gear sends a number of decimals as a number.
tg_rw_format <- function(value) {
  v <- as.character(unlist(value))[1L]
  n <- suppressWarnings(as.integer(v))
  if (!is.na(n)) n else v
}

# "0" is the reference's "all rows", which the new gear sends as null.
tg_rw_top_n <- function(value) {
  n <- suppressWarnings(as.integer(as.character(unlist(value))[1L]))
  if (is.na(n)) {
    return(list(top_n = as.character(unlist(value))[1L]))
  }
  list(top_n = if (n > 0L) n)
}

# The summary table widget's settings, as the variables gear sets them.
tg_rw_widget <- function(w, session, cx) {
  st <- session$returned$state
  vars <- as.character(unlist(w$vars))
  stats <- unique(as.character(unlist(w$stats)))
  stats <- stats[stats %in% names(SUMMARY_STATS_CATALOG)]
  id <- as.character(unlist(w$id_var))
  list(
    summaries = table_vars_to_summaries(shiny::isolate(st$summaries()), vars,
                                        cx$data()),
    by = as.character(unlist(w$by)),
    stats = if (length(stats)) stats else "mean_sd",
    sections = as.character(unlist(w$sections)),
    total = isTRUE(w$add_overall), total_label = w$overall_label %||% "Total",
    nest = isTRUE(w$nest_hierarchies),
    id = if (length(id) && nzchar(id)) id
  )
}

# A records click: a flat table's column and values, or a structured
# table's row keys and column keys. NULL clears.
tg_rw_records_click <- function(v) {
  rows <- v$filters
  cols <- v$col_filters
  filters <- if (length(rows)) {
    stats::setNames(lapply(rows, function(f) as.list(as.character(f$value))),
                    vapply(rows, function(f) as.character(f$column), ""))
  } else if (!is.null(v$column) && length(v$values)) {
    stats::setNames(list(as.list(as.character(unlist(v$values)))), v$column)
  }
  columns <- if (length(cols)) {
    stats::setNames(
      lapply(cols, function(f) as.list(as.character(unlist(f$values)))),
      vapply(cols, function(f) as.character(f$column), "")
    )
  }
  if (!length(filters) && !length(columns)) {
    return(NULL)
  }
  list(type = "categorical", filters = filters %||% list(),
       columns = columns %||% list())
}

tg_rw_groups_click <- function(v) {
  if (!identical(v$action, "filter")) {
    return(NULL)
  }
  f <- summarize_drill_filters(as.character(unlist(v$column)),
                               as.character(unlist(v$values)))
  if (length(f)) list(type = "categorical", filters = lapply(f, as.list))
}

# A saved block restored, then run for one flush: the warnings, the state it
# saves as, and the first step.
tg_rw_restore_run <- function(case) {
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
  d <- table_golden_data(case$data)
  step <- NULL
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      m <- table_golden_mock(session)
      expr <- session$makeScope("expr")
      session$flushReact()
      step <<- tg_rw_record(session, expr, m$sends, new.env())
    },
    args = list(x = blk, data = list(data = shiny::reactive(d)))
  )
  table_golden_scrub(list(
    warnings = warns,
    class = class(blk),
    saved = tg_rw_sort_dir(saved$payload),
    constructor = saved$constructor[c("constructor", "package")],
    steps = list(step)
  ))
}

# A recorded restore as the rewrite is compared with it.
tg_rw_restore_expected <- function(r, case) {
  r <- tg_rw_expected(list(initial = list(), saved = r$saved,
                           steps = r$steps, warnings = r$warnings,
                           class = r$class, constructor = r$constructor))
  r$initial <- NULL
  r
}

# Restored boards where the rewrite differs on purpose. `saved`: the state
# is the saved one for these arguments. `skip`: parts of the first step left
# uncompared.
tg_rw_restore_known <- list(
  old_summary_flat = list(why = "row 17", saved = "summaries"),
  old_summary_interim = list(why = "row 17", saved = "summaries"),
  old_table_group_agg = list(
    why = "row 20: the saved click is held, the reference dropped it",
    saved = "selection", skip = c("expr", "result", "browser")
  )
)

tg_rw_restore_apply <- function(want, got, nm) {
  k <- tg_rw_restore_known[[nm]]
  for (a in k$saved) want$steps[[1L]]$state[a] <- list(want$saved[[a]])
  for (p in k$skip) want$steps[[1L]][p] <- got$steps[[1L]][p]
  want
}

# --- the expected side ----------------------------------------------------------

# A recording as the rewrite is compared with it: the state under the public
# names, the browser messages and outputs as what a reader sees.
tg_rw_expected <- function(rec) {
  rec <- table_golden_translate(rec)
  state <- function(s) {
    s$max_height <- NULL
    core <- s[intersect(names(s), c("block_name", "block_metadata"))]
    out <- suppressWarnings(legacy_restore("table", s, VIZ_REWRITE_VERSION))
    tg_rw_sort_dir(c(out, core))
  }
  rec$initial <- state(rec$initial)
  rec$saved <- state(rec$saved)
  last <- list()
  for (i in seq_along(rec$steps)) {
    s <- rec$steps[[i]]
    last <- tg_ref_payloads(s$messages, last)
    s$state <- state(s$state)
    s$browser <- tg_ref_view(s, last)
    s$messages <- NULL
    s$outputs <- NULL
    s$expr <- tg_rw_in_c(s$expr)
    s$ctrl <- tg_ref_ctrl(s$ctrl)
    if (!is.null(s$download)) s$download <- tg_rw_download(s$download)
    rec$steps[[i]] <- s
  }
  rec
}

# The last table each engine sent, carried across steps.
tg_ref_payloads <- function(messages, last) {
  for (m in messages) {
    if (identical(m$type, "blockr-viz-table-data")) {
      last$records <- m$message$payload
    }
    if (identical(m$type, "blockr-viz-summarize-data")) {
      last$groups <- m$message$payload
    }
  }
  last
}

tg_ref_view <- function(s, last) {
  layout <- s$state$layout %||% "records"
  if (identical(layout, "groups")) {
    p <- last$groups
    ch <- p$chrome
    # The reference's groups footer was written in the browser, from the
    # held click.
    filter <- summarize_drill_phrase(s$state$selection$filters)
    return(tg_view(
      n = if (identical(p$kind, "flat")) as.integer(p$n),
      labels = if (identical(p$kind, "flat")) tg_text(p$label),
      titles = ch[c("title", "subtitle", "caption")],
      receipt = if (length(filter)) paste0("Filtered: ", filter)
    ))
  }
  p <- last$records
  status <- s$outputs[["records-dt_status"]]
  receipt <- if (is.character(status)) {
    m <- regmatches(status, regexpr("Filtered: [^<]*", status))
    if (length(m)) m
  }
  tg_view(
    n = if (identical(p$kind, "flat")) as.integer(p$n),
    labels = if (identical(p$kind, "flat")) tg_text(p$cols[[1L]]$disp),
    titles = p$titles[c("title", "subtitle", "caption")],
    receipt = receipt
  )
}

# The reference's sends under the block's own id (it had one module per
# engine), with the second clear of its second sender left out
# (behaviour-changes.md row 18).
tg_ref_ctrl <- function(log) {
  out <- list()
  for (x in log) {
    x$author <- sub("-(records|groups)-$", "-", x$author)
    dup <- length(out) && identical(x$op, "clear") &&
      identical(out[[length(out)]], x)
    if (!dup) out <- c(out, list(x))
  }
  out
}

# A downloaded web page without the old table script's settings attribute,
# which no reader of the file sees.
tg_rw_download <- function(dl) {
  if (is.list(dl) && is.character(dl$content)) {
    dl$content <- gsub(" data-summarize-cfg=\"[^\"]*\"", "", dl$content)
  }
  dl
}

# `sort_dir` as the direction the table sorts in. The reference wrote its
# default "desc" into the state; NULL means desc for sort_by = "value" and
# asc otherwise (behaviour-changes.md row 6), and picking "data" resets it.
tg_rw_sort_dir <- function(st) {
  if (is.list(st) && "sort_by" %in% names(st)) {
    st$sort_dir <- table_sort_dir(st)
  }
  st[order(names(st))]
}

# A drill on several values emits `%in% c(...)`; the reference wrote
# `%in% list(...)` (behaviour-changes.md row 13). The values are inlined.
tg_rw_in_c <- function(e) {
  if (!is.call(e)) {
    return(e)
  }
  if (identical(e[[1L]], as.name("%in%")) && length(e) == 3L) {
    rhs <- e[[3L]]
    if (is.call(rhs) && identical(rhs[[1L]], as.name("list"))) {
      rhs <- eval(rhs, baseenv())
    }
    if (is.list(rhs)) e[[3L]] <- as.character(unlist(rhs))
  }
  for (i in seq_along(e)[-1L]) {
    if (!is.null(e[[i]])) e[[i]] <- tg_rw_in_c(e[[i]])
  }
  e
}

# The parts of a step a recording compares: the reference's structured
# tables are HTML, so they pin no row count or labels here.
tg_rw_compare <- function(got, want) {
  got$browser <- got$browser[intersect(names(got$browser),
                                       c(names(want$browser), "titles",
                                         "receipt"))]
  got
}

# --- known differences ----------------------------------------------------------
#
# Steps where the rewrite differs from the reference on purpose, each with
# its row in behaviour-changes.md. `hold`: from step `from` on, `arg` keeps
# the value it had the step before, while the reference shows the healed
# default. `titles`: the bands the rewrite resolves at `step`. `state`: the
# value an argument has instead. `skip`: parts left uncompared from `from`
# on.
tg_rw_known <- list(
  rec_gear = list(why = "row 14", hold = list(
    list(arg = "format", from = 10L), list(arg = "look", from = 19L)
  )),
  grp_display = list(why = "row 14", hold = list(
    list(arg = "bar_width", from = 5L)
  )),
  rec_titles_slots = list(why = "row 15", titles = list(step = 1L, value = list(
    caption = "table look", subtitle = "Category", title = "By cat"
  ))),
  var_titles_slots = list(why = "row 15", titles = list(step = 1L, value = list(
    caption = "1 rows", title = "By grp"
  ))),
  var_options = list(why = "row 17", state = list(
    summaries = function(sc) {
      viz_check(viz_block_spec("table"),
                list(summaries = sc$args$summaries))$summaries
    }
  )),
  grp_facet = list(
    why = "row 6: the reference sorted a facet level descending by default",
    skip = list(from = 4L, parts = c("state", "browser"))
  )
)

tg_rw_apply_known <- function(want, got, nm, sc) {
  k <- tg_rw_known[[nm]]
  if (is.null(k)) {
    return(want)
  }
  steps <- want$steps
  for (h in k$hold) {
    healed <- steps[[h$from]]$state[[h$arg]]
    keep <- steps[[h$from - 1L]]$state[[h$arg]]
    for (i in seq(h$from, length(steps))) {
      if (!identical(steps[[i]]$state[[h$arg]], healed)) break
      steps[[i]]$state[h$arg] <- list(keep)
    }
  }
  if (!is.null(k$titles)) steps[[k$titles$step]]$browser$titles <- k$titles$value
  for (nm2 in names(k$state)) {
    v <- k$state[[nm2]](sc)
    for (i in seq_along(steps)) steps[[i]]$state[nm2] <- list(v)
    want$initial[nm2] <- list(v)
    want$saved[nm2] <- list(v)
  }
  if (!is.null(k$skip)) {
    for (i in seq(k$skip$from, length(steps))) {
      for (p in k$skip$parts) steps[[i]][p] <- got$steps[[i]][p]
    }
  }
  want$steps <- steps
  want
}
