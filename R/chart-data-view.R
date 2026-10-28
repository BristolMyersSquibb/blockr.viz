# The chart's data view, after Power BI's "Show as a table": a table under
# the chart with two tabs.
#
#   Summary  one row per mark: the keys (facet, category or bin, colour) and
#            the numbers the chart draws ("Count of rows", "Mean of AVAL"; a
#            box's N, whiskers, Q1, median, Q3). chart_summary(). A chart
#            that draws rows (scatter, line, ...) has none.
#   Rows     the input rows behind the selection: the block's output.
#
# The view is the browser's: `<view>_data` says whether it is open and which
# tab shows (viz_read_data_view()). While it is open the kit adds
# `derived$data_view` to the payload (viz_with_shared()): the tab counts and
# the table view's payload for the tab that shows. The Summary frame is the
# one the Excel download writes (chart_exhibit()).
#
# The Summary table is an annotated frame, so the table view draws it and a
# row click claims the row's keys: the outer keys as row groups
# (`.group<k>`), the innermost as the row (`.variable`). A row click selects
# that mark on the chart; a selected mark lights its row.

#' The numbers a chart draws, one row per mark
#'
#' @param data The rows the chart draws (`src$plain()`).
#' @param v The chart's values (viz_values()).
#' @return An annotated data frame, or NULL when the chart draws rows.
#' @noRd
chart_summary <- function(data, v) {
  if (!is.data.frame(data) || !nrow(data)) {
    return(NULL)
  }
  type <- v$type %||% "bar"
  if (type %in% CHART_DISTRIBUTION_TYPES) {
    return(chart_summary_dist(data, v))
  }
  if (identical(type, "histogram")) {
    return(chart_summary_hist(data, v))
  }
  value <- chart_value_col(v)
  cat <- chart_category(v)
  d <- chart_xlsx_frame(
    data, chart_type = type, group = cat, color = v$color, facet = v$facet,
    func = v$func, value = value, na_group = v$missing
  )
  if (identical(d, data)) {
    return(NULL)
  }
  keys <- intersect(unique(c(v$facet, cat, v$color)), names(d))
  vals <- setdiff(names(d), keys)
  stats <- d[vals]
  for (j in seq_along(stats)) attr(stats[[j]], "label") <- NULL
  names(stats) <- vapply(vals, chart_summary_value_name, character(1L),
                         func = v$func %||% "count", value = value)
  chart_summary_annotate(d[keys], stats)
}

# "Count of rows", "Mean of AVAL": the aggregate named as the gear names it.
chart_summary_value_name <- function(col, func, value) {
  if (identical(func, "count") || !length(value)) {
    return("Count of rows")
  }
  word <- c(count_distinct = "Distinct count", pct_distinct = "% distinct",
            mean = "Mean", median = "Median", sum = "Sum", min = "Min",
            max = "Max", identity = "Value")[func]
  if (is.na(word)) word <- func
  paste(word, "of", value)
}

# One row per bin (and colour, and facet): the rows it counts.
chart_summary_hist <- function(data, v) {
  bin <- chart_bin_col(v$x)
  if (!length(bin) || !bin %in% names(data)) {
    return(NULL)
  }
  keys <- intersect(unique(c(v$facet, bin, v$color)), names(data))
  d <- table_aggregate(data, keys, list(), na_group = v$missing %||% "level")
  d <- d$data
  names(d)[names(d) == bin] <- v$x
  keys[keys == bin] <- v$x
  stats <- data.frame(`Count of rows` = as.integer(d[[ncol(d)]]),
                      check.names = FALSE)
  chart_summary_annotate(d[keys], stats)
}

# One row per box or point range: N and the numbers it draws.
chart_summary_dist <- function(data, v) {
  cat <- chart_category(v)
  val <- chart_value_col(v)
  if (!length(val) || !val %in% names(data)) {
    return(NULL)
  }
  keys <- unique(intersect(c(v$facet, cat, v$color), names(data)))
  box <- identical(v$type, "boxplot")
  inner <- v$summary %||% if (box) "median_q1_q3" else "mean_se"
  outer <- if (box) v$whiskers %||% "tukey"
  words <- CHART_SUMMARY_STAT_WORDS[[inner]] %||% c("Centre", "Low", "High")

  one <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]
    s <- summarize_stat(x, inner)
    w <- if (!is.null(outer)) summarize_stat(x, outer)
    out <- list(n = length(x))
    if (!is.null(outer)) out$wlo <- w$lo %||% NA_real_
    out$lo <- s$lo %||% NA_real_
    out$center <- s$center %||% NA_real_
    out$hi <- s$hi %||% NA_real_
    if (!is.null(outer)) out$whi <- w$hi %||% NA_real_
    out
  }

  groups <- chart_summary_groups(data, keys)
  rows <- lapply(groups, function(g) {
    k <- lapply(stats::setNames(keys, keys), function(k) {
      as.character(g[[k]][[1L]])
    })
    as.data.frame(c(k, one(g[[val]])), stringsAsFactors = FALSE,
                  check.names = FALSE)
  })
  wide <- do.call(rbind, rows)
  header <- c(n = "N", wlo = "Whisker low", lo = words[[2L]],
              center = words[[1L]], hi = words[[3L]], whi = "Whisker high")
  ids <- intersect(names(header), names(wide))
  stats <- wide[ids]
  stats$n <- as.integer(stats$n)
  names(stats) <- header[ids]
  chart_summary_annotate(wide[keys], stats)
}

CHART_SUMMARY_STAT_WORDS <- list(
  median_q1_q3 = c("Median", "Q1", "Q3"),
  mean_sd = c("Mean", "Mean - SD", "Mean + SD"),
  mean_2sd = c("Mean", "Mean - 2 SD", "Mean + 2 SD"),
  mean_se = c("Mean", "Mean - SE", "Mean + SE"),
  mean_ci95 = c("Mean", "95% CI low", "95% CI high"),
  p5_p95 = c("Median", "P5", "P95"),
  p10_p90 = c("Median", "P10", "P90"),
  min_max = c("Median", "Min", "Max")
)

# The groups of `data` by `keys`, in factor level order, else first
# appearance; the outer key first.
chart_summary_groups <- function(data, keys) {
  if (!length(keys)) {
    return(list(data))
  }
  codes <- lapply(keys, function(k) {
    x <- data[[k]]
    as.integer(if (is.factor(x)) addNA(x) else factor(x, levels = unique(x)))
  })
  id <- do.call(paste, c(codes, sep = "\r"))
  first <- !duplicated(id)
  ord <- do.call(order, lapply(codes, `[`, first))
  lapply(id[first][ord], function(g) data[id == g, , drop = FALSE])
}

# The table view's format: the outer keys as row groups, the innermost as
# the row, then the statistics. The key columns are annotation columns, so
# a row click claims them (chart_claim_to_selection()).
chart_summary_annotate <- function(keys, stats) {
  out <- data.frame(row.names = seq_len(nrow(stats)))
  k <- names(keys)
  if (length(k)) {
    last <- k[[length(k)]]
    out$.label <- as.character(keys[[last]])
    attr(out$.label, "label") <- last
    for (i in seq_along(k[-length(k)])) {
      out[[paste0(".group", i)]] <- k[[i]]
      out[[paste0(".group", i, "_level")]] <- as.character(keys[[k[[i]]]])
    }
    out$.variable <- last
    out$.variable_level <- as.character(keys[[last]])
  } else {
    out$.label <- "All"
  }
  rownames(stats) <- NULL
  cbind(out, stats)
}

# A row click on Summary as the chart's selection, the one a click on that
# mark makes: the claim's `.group<k>_level` and `.variable_level` back under
# their column names, only the drill column's when `drill` names one of
# them. A histogram's bin goes back to its bin column.
chart_claim_to_selection <- function(sel, summ, v) {
  f <- if (is.list(sel)) sel$filters
  if (!length(f) || !is.data.frame(summ)) {
    return(NULL)
  }
  out <- list()
  for (nm in names(f)) {
    col <- if (identical(nm, ".variable_level")) {
      summ$.variable[[1L]]
    } else if (grepl("^\\.group[0-9]+_level$", nm)) {
      summ[[sub("_level$", "", nm)]][[1L]]
    }
    if (!length(col) || is.na(col)) next
    if (identical(v$type, "histogram") && identical(col, v$x)) {
      col <- chart_bin_col(v$x)
    }
    out[[col]] <- as.character(unlist(f[[nm]]))
  }
  drill <- v$drill
  if (is.character(drill) && length(drill) == 1L && drill %in% names(out)) {
    out <- out[drill]
  }
  if (length(out)) list(type = "categorical", filters = out, columns = list())
}

# The chart's selection as a claim on Summary, to light the row; NULL
# unless it names exactly the table's keys.
chart_selection_to_claim <- function(sel, summ, v) {
  f <- chart_selection_filters(sel)
  if (!length(f) || !is.data.frame(summ) || !".variable" %in% names(summ)) {
    return(NULL)
  }
  if (identical(v$type, "histogram") && length(v$x)) {
    names(f)[names(f) == chart_bin_col(v$x)] <- v$x
  }
  gcols <- grep("^\\.group[0-9]+$", names(summ), value = TRUE)
  keys <- c(stats::setNames(vapply(gcols, function(g) summ[[g]][[1L]], ""),
                            paste0(gcols, "_level")),
            .variable_level = summ$.variable[[1L]])
  if (!setequal(unname(keys), names(f))) {
    return(NULL)
  }
  claim <- lapply(keys, function(col) as.list(f[[col]]))
  claim$.variable <- list(summ$.variable[[1L]])
  list(type = "categorical", filters = claim)
}

# The rows behind the selection: the block's expression evaluated on its
# input, as blockr.core evaluates it, with the `.(data)` slot bound.
chart_output_rows <- function(data, expr) {
  if (is.null(data) || is.null(expr)) {
    return(if (is.data.frame(data)) data)
  }
  out <- tryCatch(
    eval(dd_splice_slot(expr, quote(data)),
         blockr.core::eval_env(list(data = data))),
    error = function(e) NULL
  )
  if (is.data.frame(out)) out
}

#' The chart's data view
#'
#' Its frames are reactives, so nothing is computed until the view opens or
#' a download asks. Registers the observer that takes a row click on
#' Summary into the chart's selection.
#'
#' @param kit The chart's kit.
#' @param src The chart's sources (chart_sources()).
#' @param expr The block's expression, a reactive.
#' @return A list: `summary()` and `rows()`, reactives; `derived(msg)`, the
#'   payload's `data_view` entry for the reader's `<view>_data` message.
#' @noRd
chart_data_view <- function(kit, src, expr) {
  summary <- shiny::reactive({
    d <- tryCatch(src$plain(), error = function(e) NULL)
    chart_summary(d, viz_values(kit$state))
  })
  rows <- shiny::reactive({
    chart_output_rows(tryCatch(kit$data(), error = function(e) NULL),
                      tryCatch(expr(), error = function(e) NULL))
  })
  # Rows with nothing selected: the prepare script's output.
  total <- shiny::reactive({
    d <- tryCatch(src$prep$prepared(), error = function(e) NULL)
    if (is.data.frame(d)) nrow(d)
  })
  table <- viz_data_view_table()

  select <- paste0(kit$view, "-data_select")
  shiny::observeEvent(kit$input[[select]], {
    msg <- kit$input[[select]]
    v <- viz_values(kit$state)
    sel <- chart_claim_to_selection(
      msg$selection, shiny::isolate(summary()), v
    )
    viz_apply(kit$state, list(selection = sel), kit$revs)
  })

  derived <- function(msg) {
    s <- summary()
    r <- rows()
    tab <- if (is.null(s)) "rows" else msg$tab %||% "summary"
    x <- if (identical(tab, "rows")) r else s
    on_summary <- identical(tab, "summary")
    v <- viz_values(kit$state)
    # Rows click as the marks do: not at all without a drill.
    clicks <- on_summary && nzchar(v$drill %||% "")
    list(
      tab = tab,
      summary = if (is.data.frame(s)) nrow(s),
      rows = if (is.data.frame(r)) nrow(r),
      total = total(),
      table = table(x, drill = clicks,
                    selection = if (on_summary) {
                      chart_selection_to_claim(v$selection, s, v)
                    }, req = msg$n)
    )
  }
  list(summary = summary, rows = rows, derived = derived)
}

#' The table view's payload for a data view tab
#'
#' interactive_table_payload() of the frame, with revisions that move only
#' when the payload does: the data revision when the table differs, so the
#' view keeps its sort and scroll over a click; the state revision when
#' anything differs. The table's rows go out once per data revision, as the
#' chart's do (viz_payload_output()); the browser keeps them. A browser that
#' lacks them (a re-mounted view) sends a new request count `req`, and the
#' next payload carries them again.
#'
#' @return A function `(x, drill, selection, req)`.
#' @noRd
viz_data_view_table <- function() {
  last <- new.env(parent = emptyenv())
  last$state <- 0L
  last$data <- 0L
  function(x, drill, selection, req = 0) {
    p <- interactive_table_payload(
      x, drill = drill, drill_target = NULL, sortable = TRUE,
      collapsible = TRUE, search = FALSE, selection = selection
    )
    if (!identical(p$data, last$model)) {
      last$data <- last$data + 1L
      last$model <- p$data
    }
    if (!identical(p, last$p)) {
      last$state <- last$state + 1L
      last$p <- p
    }
    p$state_rev <- last$state
    p$data_rev <- last$data
    sent <- list(last$data, req)
    if (identical(sent, last$sent)) {
      p$data <- NULL
    } else {
      last$sent <- sent
    }
    p
  }
}
