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
# view in computed terms, its saved selections in the kit's shape, and the
# accepted behaviour changes the scenario `sc` runs into.
record_rw_expected <- function(rec, sc) {
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
    if (is.data.frame(s$download) &&
          (rec$initial$type %||% "bar") %in% c(CHART_AGGREGATED_TYPES,
                                               "histogram")) {
      s$download <- record_rw_summary_sheet(s$download, record_df())
    }
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
  record_rw_empty_click_clear(rec, sc)
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

# An aggregated chart's Excel download starts with its Summary sheet
# (behaviour-changes.md, the chart's data view): the outer keys as row
# groups over the innermost, the columns by name, the statistic as "Mean of
# <column>". The reference's flat sheet in that layout; `data` holds the
# labels the reference's header used.
record_rw_summary_sheet <- function(dl, data) {
  lab <- vapply(names(data), function(n) attr(data[[n]], "label") %||% n, "")
  name <- function(x) {
    hit <- names(lab)[lab == x]
    if (length(hit)) hit[[1L]] else x
  }
  stat <- function(x) {
    if (grepl("^Count", x)) {
      return("Count of rows")
    }
    m <- regmatches(x, regexec("^(\\w+): (.+)$", x))[[1L]]
    if (length(m)) paste(m[[2L]], "of", name(m[[3L]])) else x
  }
  head_at <- which(!is.na(dl[[2L]]))[[1L]]
  top <- dl[seq_len(head_at - 1L), , drop = FALSE]
  body <- dl[-seq_len(head_at), , drop = FALSE]
  k <- ncol(dl) - 1L
  keys <- vapply(unlist(dl[head_at, seq_len(k)]), name, "")
  out <- list(c(keys[[k]], stat(dl[head_at, k + 1L])))
  outer <- NULL
  for (i in seq_len(nrow(body))) {
    o <- if (k > 1L) paste(unlist(body[i, seq_len(k - 1L)]), collapse = " ")
    if (k > 1L && !identical(o, outer)) {
      out <- c(out, list(c(o, NA)))
      outer <- o
    }
    out <- c(out, list(c(body[i, k], body[i, k + 1L])))
  }
  rows <- do.call(rbind, out)
  new <- data.frame(X1 = c(top$X1, rows[, 1L]), X2 = c(top$X2, rows[, 2L]))
  rownames(new) <- NULL
  new
}

# An empty click on a chart with a drill target sends a clear to the target
# (behaviour-changes.md row 33); the reference sent nothing. The clear goes
# into the control log after that click, and the log is cumulative, so every
# later step carries it too. Recorded step i + 1 is scenario step i.
record_rw_empty_click_clear <- function(rec, sc) {
  target <- sc$args$drill_target %||% ""
  for (j in seq_along(sc$steps)) {
    st <- sc$steps[[j]]
    if (identical(st$kind, "set") && "drill_target" %in% names(st$args)) {
      target <- trimws(st$args$drill_target %||% "")
    }
    if (!identical(st$kind, "click") || length(st$args) || !nzchar(target)) {
      next
    }
    at <- length(rec$steps[[j + 1L]]$ctrl)
    log <- rec$steps[[j + 1L]]$ctrl
    clear <- list(
      op = "clear", target = target,
      args = list(state = list(columns = list())),
      author = log[[at]]$author
    )
    for (i in seq(j + 1L, length(rec$steps))) {
      ctrl <- rec$steps[[i]]$ctrl
      rec$steps[[i]]$ctrl <- append(ctrl, list(clear), after = at)
    }
  }
  rec
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
