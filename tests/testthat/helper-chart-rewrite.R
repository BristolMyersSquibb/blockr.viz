# How the chart recordings talk to the rewritten chart (chart-block.R on the
# kit). The recordings were made against the reference under the public
# names (helper-chart-reference.R); this target feeds the same steps through
# the kit's inputs and reads what the block holds, emits and draws.
#
# What is compared, per step:
#   state, starting state  the state, with numbers compared as numbers (the
#                          spec makes counts integers) and the reference's
#                          saved selection in the kit's shape
#   expr, result, ctrl, download  as recorded
#   browser                what R computes for the drawing: the rows, the
#                          column metadata, the resolved titles, the band,
#                          the smoother, the scales and the palette. The
#                          settings themselves travel as the state, which is
#                          compared above; healing a missing column and
#                          resolving a default are the browser's job now.

record_target_rewrite <- function() {
  list(
    name = "rewrite",
    ctor = function(args, sc = NULL) {
      if (isTRUE(sc$restore)) {
        return(blockr.core::blockr_deser(structure(list(), class = "chart_block"),
          list(payload = args, constructor = list(version = "0.3.0"))))
      }
      eval(as.call(c(list(quote(new_chart_block)), args)))
    },
    state = identity,
    inputs = record_rw_inputs,
    payload = record_rw_payload,
    capture = function(x) list(chart_capture = viz_read_capture(x)),
    wire = FALSE
  )
}

# A step to the kit's inputs, read by the same functions the input handlers
# call, as the browser's typed inputs would be.
record_rw_inputs <- function(step, cx) {
  rev <- cx$last_payload$state_rev %||% 0L
  sel <- function(s) {
    list(chart_select = viz_read_selection(list(selection = s,
                                                nonce = cx$nonce)))
  }
  switch(step$kind,
    set = list(chart_set = viz_read_set(
      viz_block_spec("chart"),
      list(rev = rev, values = record_rw_json(step$args))
    )),
    click = sel(if (length(step$args)) {
      list(type = "categorical", filters = record_ref_filters(step$args))
    }),
    point = sel(list(
      type = "range", x = step$x_col, y = step$y_col,
      x_range = rep(record_ref_value(step$x), 2L),
      y_range = rep(record_ref_value(step$y), 2L)
    )),
    brush = sel(if (!is.null(step$x_col)) {
      list(type = "range", x = step$x_col, y = step$y_col,
           x_range = as.list(record_ref_value(step$x_range)),
           y_range = if (!is.null(step$y_range)) {
             as.list(record_ref_value(step$y_range))
           },
           filters = if (!is.null(step$panel)) {
             record_ref_filters(step$panel)
           })
    }),
    stop("a reference-only step")
  )
}

# Settings as they arrive from the browser: through JSON.
record_rw_json <- function(x) {
  spec <- viz_block_spec("chart")
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

# The payload the output holds after a step, once per new payload.
record_rw_payload <- function(msgs, expr, cx) {
  p <- tryCatch(expr$output$chart, error = function(e) NULL)
  if (is.null(p) || identical(p, cx$last_payload)) {
    return(list())
  }
  cx$last_payload <- p
  d <- p$derived
  view <- record_computed(list(
    rows = if (!is.null(p[["data"]])) record_ref_rows(p[["data"]]),
    columns = record_ref_columns(p$columns),
    titles = lapply(d$titles[c("title", "subtitle", "caption")],
                    function(b) b$text),
    band = d$band,
    smoother = d$smoother,
    scales = d$scales,
    palette = d$palette
  ))
  # A payload that only carries the state anew (a click, a setting the
  # drawing reads in the browser) brings nothing computed: as the reference,
  # which sent nothing then.
  same <- identical(view[names(view) != "rows"], cx$last_view)
  cx$last_view <- view[names(view) != "rows"]
  if (same && is.null(view$rows)) list() else list(view)
}

# The payload's column description in the reference's shape: `type`
# numeric or categorical, `temporal` for a date or date-time, `label` only
# when set.
record_ref_columns <- function(cols) {
  lapply(cols, function(c) {
    temporal <- switch(c$class, date = "date", datetime = "datetime", NULL)
    out <- list(name = c$name,
                type = if (identical(c$class, "number")) "numeric" else "categorical",
                n_unique = c$n_unique)
    if (nzchar(c$label)) out$label <- c$label
    out$temporal <- temporal
    out$kind <- c$kind
    if (length(c$levels)) out$levels <- as.list(as.character(c$levels))
    out
  })
}

# The reference's browser view in the same terms.
record_ref_computed <- function(entry) {
  cfg <- entry$config
  json <- function(v) {
    if (inherits(v, "json")) jsonlite::fromJSON(v, simplifyVector = FALSE) else v
  }
  band <- if (!is.null(cfg$band_series) || !is.null(cfg$band_note) ||
                !is.null(cfg$band_refs)) {
    list(series = json(cfg$band_series), refs = cfg$band_refs,
         note = cfg$band_note)
  }
  record_computed(list(
    rows = entry$rows, columns = entry$columns,
    titles = lapply(list(title = cfg$title_resolved,
                         subtitle = cfg$subtitle_resolved,
                         caption = cfg$caption_resolved),
                    function(x) if (length(x) && nzchar(x)) x),
    band = band, smoother = cfg$smoother_series, scales = cfg$scales,
    palette = cfg$palette
  ))
}

# One JSON form for both: a pre-serialized piece and an R list compare
# alike.
record_computed <- function(x) {
  norm <- function(v) {
    if (is.null(v)) {
      return(NULL)
    }
    json <- if (inherits(v, "json")) {
      v
    } else {
      jsonlite::toJSON(v, auto_unbox = TRUE, null = "null", digits = NA)
    }
    jsonlite::fromJSON(json, simplifyVector = FALSE)
  }
  if (!is.null(x$rows)) x$rows <- x$rows[sort(names(x$rows))]
  x[c("band", "smoother", "scales", "palette", "columns", "titles")] <- lapply(
    x[c("band", "smoother", "scales", "palette", "columns", "titles")], norm
  )
  x
}

# A recording as the rewrite is compared with it: the reference's browser
# view in computed terms, its saved selections in the kit's shape.
record_rw_expected <- function(rec) {
  sel <- function(st) {
    if (!is.null(st$selection)) {
      st["selection"] <- list(legacy_chart_selection(st)$selection)
    }
    st <- legacy_values("chart", st, NULL)
    st[sort(names(st))]
  }
  rec$warnings <- record_rw_warnings(rec$warnings)
  rec$initial <- sel(rec$initial)
  msg <- function(dl) {
    if (inherits(dl, "record_error") && dl$message %in% names(record_rw_messages)) {
      dl$message <- record_rw_messages[[dl$message]]
    }
    dl
  }
  last <- NULL
  for (i in seq_along(rec$steps)) {
    s <- rec$steps[[i]]
    s$wire <- NULL
    s$state <- sel(s$state)
    s["download"] <- list(msg(s$download))
    views <- list()
    for (b in lapply(s$browser, record_ref_computed)) {
      core <- b[names(b) != "rows"]
      if (!(identical(core, last) && is.null(b$rows))) {
        views <- c(views, list(b))
      }
      last <- core
    }
    s$browser <- views
    rec$steps[[i]] <- s
  }
  rec
}

record_rw_got <- function(rec) {
  # A restored board heals with one warning (behaviour-changes.md row 7).
  rec$warnings <- record_rw_warnings(
    grep("^Restoring a chart: ", rec$warnings, value = TRUE, invert = TRUE)
  )
  rec$initial <- rec$initial[sort(names(rec$initial))]
  rec$steps <- lapply(rec$steps, function(s) {
    s$wire <- NULL
    s$state <- s$state[sort(names(s$state))]
    s
  })
  rec
}

# The old names a constructor warning lists, in sorted order: the rewrite
# lists them in call order (behaviour-changes.md).
record_rw_warnings <- function(w) {
  # The old-name warning says "an earlier blockr.viz" (behaviour-changes.md
  # row 26).
  w <- sub("from blockr.viz < 0.3.0.", "from an earlier blockr.viz.", w,
           fixed = TRUE)
  vapply(w, function(x) {
    m <- regmatches(x, regexpr("(`[^`]+`(, )?)+(?= (is|are) from)", x,
                               perl = TRUE))
    if (!length(m)) {
      return(x)
    }
    names <- sort(strsplit(m, ", ", fixed = TRUE)[[1L]])
    sub(m, paste(names, collapse = ", "), x, fixed = TRUE)
  }, character(1L), USE.NAMES = FALSE)
}

# Recordings that differ by a user-visible change still open in
# behaviour-changes.md. They skip until Christoph decides; an accepted change
# becomes a translation above, a rejected one a fix.
record_rw_open <- c(
  heal_missing_column = "row 1: clearing a column the data lacks clears it"
)

# Messages that changed with an open behaviour change (behaviour-changes.md),
# reference = rewrite.
record_rw_messages <- c(
  "the chart has not been drawn yet; try again in a moment" =
    "the picture is not ready yet; open the menu again"
)
