# Summarize table: HTML -------------------------------------------------------
#
# Emits the table-block chrome (shared CSS, sticky-header scroll wrapper,
# search input, title / subtitle / caption bands) with the summarize
# table's <table> inside it, plus one small JS dependency for search / sort /
# expand / row-click. See R/summarize-table.R for the data half.

#' Summarize table, as static HTML
#'
#' Renders the table without a block. Its original form, a ranked
#' horizontal-bar table: one row per level of `group`,
#' ordered by the measure, with the bar drawn as a div in a cell. Search,
#' click-to-sort, exact values, a sticky header and an arbitrary row count
#' come from the table form; the bar carries the magnitude.
#'
#' The bar column takes one of three shapes, picked by the arguments:
#' plain (`group` only), split into segments (`color`), or one column per
#' level (`facet`).
#'
#' @param data A data frame.
#' @param group Column to rank by (one row per level).
#' @param summaries,by,facet_layout The summarize-table path: `summaries` is
#'   an ordered list of typed summary rows (see
#'   [new_summarize_table_block()]); non-empty, it takes over from the flat
#'   bar mappings. `by` is the grouping vector (outer -> inner, at most
#'   two).
#' @param value,func,id_var The measure. `func` is one of `"count"`,
#'   `"count_distinct"`, `"sum"`, `"mean"`, `"median"`, `"min"`, `"max"`;
#'   `value` names the column it reduces (ignored by `"count"`), `id_var` the
#'   subject identifier `"count_distinct"` counts.
#' @param parent Optional outer grouping column: parents become expandable
#'   rows with their children indented under them. A parent is aggregated in
#'   its own pass, never summed from its children.
#' @param color Optional column splitting each bar into segments. Composes
#'   with `facet`.
#' @param bar_mode `"stacked"`, `"grouped"` or `"percent"`. No-op without
#'   `color`.
#' @param facet Optional column giving one bar column per level, all on one
#'   shared scale.
#' @param cols Opt-in separate numeric columns beside the bar: any of `"n"`,
#'   `"pct"`. By default the bar cell carries its own value label.
#' @param fields Extra columns from the underlying row (identity measure
#'   only), shown as real columns beside the bar.
#' @param sort_by,sort_dir Server-side ordering. `sort_by` is `"value"`,
#'   `"data"` (the data's own order), `"label"`, a summary column name or a
#'   facet level name; `sort_dir` is `"desc"` or `"asc"`.
#' @param top_n Optional cap. Off by default -- the table scrolls instead.
#'   When set, the rows below the cut are reported in a visible fold row.
#' @param max_height `NULL` (default): the table runs its full length and
#'   scrolls with what is around it. A CSS length: a box of that height.
#' @param search Show the search input.
#' @param sortable Allow click-to-sort on the column headers.
#' @param axis Print each glyph column's domain as a tick strip under its
#'   header (default `TRUE`): the value domain for bars, boxes and dot
#'   ranges, the x domain (dates as dates) for swimlanes and sparklines.
#' @param bar_width `"fit"` (default), `"narrow"`, `"medium"` or `"wide"`:
#'   the length of the labelled marks. See [new_summarize_table_block()].
#' @param title,subtitle,caption Display text, already resolved (see
#'   `resolve_block_title()`).
#' @param drill Column a row click filters on, or `NULL` for a display-only
#'   table.
#' @param scale_map Board scale map, for palette agreement with the charts.
#' @param elem_id Shiny namespaced id used to build the `_action` input the
#'   JS emits. `NULL` outside a block (a static table).
#' @param active Active drill filter, `list(col =, vals =)`, for the row
#'   highlight and the status line.
#' @param expanded Open every nested row. `FALSE` (default) is the dashboard's
#'   collapsed-to-parents view; an EXPORT sets it, having nobody to click the
#'   chevron.
#'
#' @return An [htmltools::tagList()].
#' @examplesIf interactive()
#' summarize_table(mtcars, group = "cyl", func = "count")
#' @noRd
summarize_table <- function(data, group = NULL, value = ".count",
                            func = "count", id_var = NULL, summaries = list(),
                            by = NULL, facet_layout = "by_summary",
                            parent = NULL, color = NULL, bar_mode = "stacked",
                            facet = NULL, cols = NULL, fields = NULL,
                            sort_by = "value", sort_dir = "desc", top_n = NULL,
                            max_height = NULL, search = TRUE, sortable = TRUE,
                            axis = TRUE, value_labels = TRUE,
                            bar_width = "fit", title = NULL,
                            subtitle = NULL, caption = NULL, drill = NULL,
                            scale_map = NULL, elem_id = NULL, active = NULL,
                            expanded = FALSE) {
  prep <- summarize_prepare(
    data, group = group, value = value, func = func, id_var = id_var,
    summaries = summaries, by = by,
    facet_layout = facet_layout,
    parent = parent, color = color, bar_mode = bar_mode, facet = facet,
    cols = cols, fields = fields, sort_by = sort_by,
    sort_dir = sort_dir, top_n = top_n, scale_map = scale_map
  )

  # The three display slots follow the chart / table contract: NULL = auto
  # (the input's own label / subtitle / caption attribute), "" = none, else a
  # {...} template resolved against the current data. The block resolves them
  # before calling; resolving again here is a no-op for a plain string and
  # gives a standalone summarize_table() the same auto tier.
  title_raw <- title
  subtitle_raw <- subtitle
  caption_raw <- caption
  title <- resolve_block_title(title, data,
                               auto = summarize_attr(data, "label"))
  subtitle <- resolve_block_title(subtitle, data,
                                  auto = summarize_attr(data, "subtitle"))
  caption <- resolve_block_title(caption, data,
                                 auto = summarize_attr(data, "caption"))

  # The gear's working state: the block's config as given, plus the pickable
  # input columns. Read back by summarize-table.js off the rendered <table>.
  cfg <- list(
    group = group, parent = parent, color = color, facet = facet,
    func = func, value = value, id_var = id_var,
    summaries = summaries, by = by, facet_layout = facet_layout,
    bar_mode = bar_mode, cols = cols, fields = fields, sort_by = sort_by,
    sort_dir = sort_dir, top_n = top_n, search = search,
    sortable = sortable, axis = axis, value_labels = value_labels,
    bar_width = bar_width, drill = drill,
    titles = list(
      title = title, subtitle = subtitle, caption = caption,
      title_state = title_raw, subtitle_state = subtitle_raw,
      caption_state = caption_raw
    ),
    columns = summarize_gear_cols(data)
  )

  summarize_chrome(
    inner = if (!is.null(prep$err)) {
      summarize_message_table(prep$err)
    } else {
      htmltools::HTML(summarize_table_html(prep, drill = drill, active = active,
                                           cfg = cfg, expanded = expanded))
    },
    prep = prep, max_height = max_height, search = search,
    title = title, subtitle = subtitle, caption = caption,
    drill = drill, elem_id = elem_id, active = active
  )
}

#' @noRd
summarize_message_table <- function(msg = "No data") {
  htmltools::tags$table(
    class = "blockr-table blockr-summarize-table",
    htmltools::tags$tbody(
      htmltools::tags$tr(htmltools::tags$td(class = "blockr-data", msg))
    )
  )
}

# Chrome: title bands, toolbar (search + legend), scroll wrapper, caption,
# status line. Mirrors dt_chrome() -- same classes, so the shared table CSS
# and the design tokens apply unchanged -- plus the summarize table's CSS delta
# and JS.
#' @noRd
summarize_chrome <- function(inner, prep = NULL, max_height = NULL,
                             search = TRUE, title = NULL, subtitle = NULL,
                             caption = NULL, drill = NULL, elem_id = NULL,
                             active = NULL, shell = FALSE, download = NULL,
                             ctrl_target = "") {
  legend <- if (isTRUE(shell)) {
    summarize_legend_tag(NULL)
  } else {
    summarize_legend(prep)
  }

  # The control row holds the search only; summarize-table.js hoists it into the
  # gear row so search sits left of the gear (table-block parity), which keeps
  # that row at its 30px.
  header <- htmltools::tags$div(
    class = "blockr-html-table-header",
    if (isTRUE(search) || !is.null(download)) {
      htmltools::tags$div(
        class = "blockr-html-table-toolbar",
        # The shell always carries the box, hidden while off: the payload
        # turns it on and off without re-rendering the container.
        if (isTRUE(search) || isTRUE(shell)) {
          htmltools::tags$input(
            type = "search", class = "blockr-search",
            placeholder = "Search\u2026", `aria-label` = "Search table",
            style = if (!isTRUE(search)) "display:none"
          )
        },
        # The download control rides in the toolbar so the JS hoist carries it
        # into the gear row with the search box (one 30px control row), and so
        # it inherits the shared table CSS rather than growing its own.
        download
      )
    }
  )

  # Title band: the canonical .dd-table-titles the chart and table blocks use
  # (inst/css/table.css) -- title over subtitle, a hairline under the band
  # dividing it from the column headers. Hidden entirely when both are empty.
  titles <- if (isTRUE(shell)) {
    # Always present, hidden while empty: table.js's applyTitles() rule, so the
    # band can be filled from a payload without touching the container.
    htmltools::tags$div(
      class = "dd-table-titles", style = "display:none",
      htmltools::tags$div(class = "dd-table-title"),
      htmltools::tags$div(class = "dd-table-subtitle")
    )
  } else if (summarize_nz(title) || summarize_nz(subtitle)) {
    htmltools::tags$div(
      class = "dd-table-titles",
      if (summarize_nz(title)) {
        htmltools::tags$div(class = "dd-table-title", title)
      },
      if (summarize_nz(subtitle)) {
        htmltools::tags$div(class = "dd-table-subtitle", subtitle)
      }
    )
  }

  # No height: the table runs its full length and the panel or page around it
  # scrolls, the header following (table.js followHeader(), the composer
  # table's `scroll = "page"`). The box then scrolls sideways only.
  page <- is.null(max_height)
  scroll_style <- if (page) {
    NULL
  } else {
    paste0("max-height:", max_height, ";overflow:auto;")
  }

  htmltools::tagList(
    htmltools::tags$style(htmltools::HTML(html_table_shared_css_fallback())),
    htmltools::tags$style(htmltools::HTML(summarize_table_css())),
    summarize_table_dep(),
    htmltools::tags$div(
      class = "blockr-html-table-container blockr-summarize-container",
      `data-summarize-elem-id` = elem_id,
      `data-summarize-drill` = drill,
      # Transient drill: the JS reads this to decide whether a click is an
      # event (send and forget) or a selection it latches.
      `data-summarize-ctrl-target` = ctrl_target %||% "",
      htmltools::tags$div(class = "blockr-summarize-scope", header),
      titles,
      # The legend sits below the title band and above the table, never in the
      # control row -- a long legend must not push the search box around.
      legend,
      htmltools::tags$div(
        class = paste(c("blockr-table-wrapper", if (page) "dt-scroll-page"),
                      collapse = " "),
        style = scroll_style, inner
      ),
      if (isTRUE(shell)) {
        htmltools::tags$div(class = "dd-table-caption", style = "display:none")
      } else if (summarize_nz(caption)) {
        htmltools::tags$div(class = "dd-table-caption", caption)
      },
      if (isTRUE(shell)) {
        summarize_footer_tag(NULL)
      } else {
        summarize_footer(prep, drill = drill, active = active)
      }
    )
  )
}

# The footer's content as DATA: the note a reinterpreted config leaves, and
# the active drill filter. One definition, two consumers -- the chrome renders
# it server-side, and summarize-table.js refreshes it from the payload without
# re-rendering the container. There is no row count: a Top N cut already
# says what it left out in its fold row, and an uncut table has nothing to
# report.
#' @noRd
summarize_foot_spec <- function(prep, drill = NULL, active = NULL) {
  if (!is.null(prep$err)) {
    return(list(note = NULL, filter = NULL, reset = FALSE))
  }
  list(
    note = prep$note,
    # The footer prints "Filtered: " and this phrase.
    filter = summarize_drill_phrase(
      summarize_drill_filters(active$col, active$vals)
    ),
    reset = !is.null(drill)
  )
}

# The legend as DATA: a list of titled groups, always present for two or more
# series -- colour alone must not carry identity. One group is the ordinary
# case; the summarize table maps colour PER COLUMN, so it can carry several,
# and each is titled by the column it decodes.
#' @noRd
summarize_legend_spec <- function(prep) {
  if (!is.null(prep$err)) return(NULL)
  # Colour identity lives in the COLOUR mapping only: a plain facet's bars are
  # all the house blue and its column headers already name the levels, so it
  # carries no legend (chart parity -- a facet never recolours the marks).
  groups <- prep$color_groups
  if (is.null(groups)) {
    if (is.null(prep$color) || length(prep$series) < 2L) return(NULL)
    groups <- list(list(column = prep$color, title = prep$color_title,
                        levels = prep$series, palette = prep$palette))
  }
  if (!length(groups)) return(NULL)
  list(groups = lapply(groups, function(g) {
    # Positional, never by name: a colour column can carry a BLANK level (an
    # untreated subject's empty TRT01A), and `pal[[""]]` is a subscript error
    # even though the entry sits right there -- the empty string matches no
    # name in R. summarize_level_colors() returns the palette in `levels` order,
    # so the index IS the lookup. Blank reads as "(Missing)", as it does in
    # the table's group columns.
    pal <- g$palette
    list(
      # The column's variable label when it has one, as the hover card says.
      title = g$title %||% g$column,
      items = lapply(seq_along(g$levels), function(i) {
        l <- as.character(g$levels[[i]])
        list(
          label = if (nzchar(trimws(l))) l else "(Missing)",
          color = if (i <= length(pal)) unname(pal[[i]])
        )
      })
    )
  }))
}

#' @noRd
summarize_footer <- function(prep, drill = NULL, active = NULL) {
  summarize_footer_tag(
    summarize_foot_spec(prep, drill = drill, active = active)
  )
}

#' @noRd
summarize_footer_tag <- function(spec) {
  if (is.null(spec)) spec <- list(reset = FALSE)
  htmltools::tags$div(
    class = "blockr-summarize-footer",
    htmltools::tags$span(class = "blockr-summarize-note", spec$note %||% ""),
    # The drill's line, the chart's own markup and words (chart/chrome.js
    # _updateStatus): summarize-table.js fills it, because what it says depends
    # on clicks the server never hears about in transient mode.
    htmltools::tags$div(class = "dd-status-footer")
  )
}

#' @noRd
summarize_legend <- function(prep) {
  summarize_legend_tag(summarize_legend_spec(prep))
}

#' @noRd
summarize_legend_tag <- function(spec) {
  htmltools::tags$div(
    class = "blockr-summarize-legend",
    style = if (is.null(spec)) "display:none" else NULL,
    lapply(spec$groups, function(g) {
      htmltools::tags$span(
        class = "blockr-summarize-legend-group",
        htmltools::tags$span(class = "blockr-summarize-legend-title", g$title),
        lapply(g$items, function(it) {
          htmltools::tags$span(
            class = "blockr-summarize-legend-item",
            htmltools::tags$i(style = paste0("background:", it$color)),
            it$label
          )
        })
      )
    })
  )
}

#' @noRd
summarize_nz <- function(x) {
  !is.null(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

# Escape exactly as htmltools does for a text child -- & < > and the attribute
# quote -- because the JS row assembler applies the same rules and the two
# outputs must not drift.
#' @noRd
summarize_esc <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

# A data-frame-level display attribute, or NULL when absent / not a string.
#' @noRd
summarize_attr <- function(data, nm) {
  v <- attr(data, nm, exact = TRUE)
  if (is.character(v) && length(v) == 1L && nzchar(v)) v else NULL
}
#' The two parts of a numeric cell: the value string, and the percent string
#' when the column shows both. Data, never markup -- the consumers wrap it.
#' @noRd
summarize_num_parts <- function(v, denom = NULL, combined = FALSE,
                                signed = FALSE, pct_only = FALSE,
                                fmt = NULL) {
  if (isTRUE(signed)) {
    return(list(disp = ifelse(
      is.na(v), "",
      paste0(ifelse(v > 0, "+", ifelse(v < 0, "\u2212", "")),
             formatC(abs(v), format = "f", digits = 1L))
    )))
  }
  pct <- if (!is.null(denom) && is.finite(denom) && denom > 0) {
    v / denom * 100
  } else {
    NULL
  }
  if (isTRUE(pct_only)) {
    if (is.null(pct)) return(list(disp = rep("", length(v))))
    return(list(disp = ifelse(
      is.na(v), "", paste0(formatC(pct, format = "f", digits = 1L), "%")
    )))
  }
  # Four SIGNIFICANT digits, the same budget lane_fmt() prints a statistic at
  # (R/lane-stats.R), so a split bar's label and a box's centre read alike. "fg"
  # keeps whole numbers whole: a count of 12345 stays 12345, a mean of 155.625
  # reads 155.6 rather than spelling out a precision the estimate has not got.
  # `fmt`, when given, is the value label's own precision (one digit finer
  # than the column's axis, summarize_val_digits()).
  n <- ifelse(is.na(v), "", if (is.null(fmt)) {
    formatC(v, format = "fg", digits = 4L, big.mark = "")
  } else {
    fmt(v)
  })
  if (isTRUE(combined) && !is.null(pct)) {
    return(list(
      disp = n,
      pct = ifelse(is.na(v), "",
                   paste0("(", formatC(pct, format = "f", digits = 0L), "%)"))
    ))
  }
  list(disp = n)
}

# --- the table ---------------------------------------------------------------
#' @noRd
summarize_table_html <- function(prep, drill = NULL, active = NULL, cfg = NULL,
                                 expanded = FALSE) {
  summarize_cells_html(
    summarize_cells(prep, drill = drill, active = active, cfg = cfg),
    expanded = expanded
  )
}

#' @noRd
summarize_label_header <- function(prep) {
  if (is.null(prep$parent)) prep$group else paste0(prep$parent, " / ", prep$group)
}

# The same bundle the table block ships (blockr.ui's shared controls, the dd-*
# CSS and the gear engine), plus the summarize table JS LAST -- it reads
# Blockr.DrilldownConfig at bind time.
#' @noRd
summarize_table_dep <- memoise0(function() {
  htmltools::tagList(
    drilldown_table_dep(),
    htmltools::htmlDependency(
      name = "blockr-viz-summarize",
      version = paste0(utils::packageVersion("blockr.viz"), ".21"),
      src = system.file("js", package = "blockr.viz"),
      script = "summarize-table.js"
    )
  )
})

# The sort key for a cell: the raw number (or, for a text field column, the
# raw text -- escaped, it lives in an attribute), so the client never parses
# a formatted string (and a bar cell, which has no text at all, still sorts).
#' @noRd
summarize_data_v <- function(v) {
  # format() common-width-pads a CHARACTER vector even with trim (trim only
  # suppresses numeric left-padding) -- text sort keys pass through as-is.
  # Numbers are formatted PER ELEMENT: vectorized format() decimal-aligns
  # the column ("13.0" beside "11.5") where the JS assembler prints
  # String(13) = "13", and the two must agree byte for byte.
  s <- if (is.character(v)) {
    v
  } else {
    vapply(v, function(x) {
      format(x, scientific = FALSE, trim = TRUE, digits = 15L)
    }, character(1L))
  }
  paste0(" data-v=\"", summarize_esc(ifelse(is.na(v), "", s)), "\"")
}

# Pickable input columns for the gear's column pickers, in the shape the shared
# config engine expects ({name, type, label}). Always the block's RAW input
# columns -- not the displayed projection -- so the pickers stay correct while
# the table shows an aggregated frame (dt_gear_cols_json's rule).
#' @noRd
summarize_gear_cols <- function(data) {
  if (!is.data.frame(data)) return(list())
  lapply(names(data), function(nm) {
    num <- is.numeric(data[[nm]])
    out <- list(
      name = nm,
      type = if (num) "numeric" else "categorical"
    )
    # Level count for the categoricals: the gear seeds a colour / facet
    # mapping with a column that HAS few levels, so adding one lands on
    # AESEV rather than on the first character column, which is usually a
    # subject id (200 colours, or 200 column copies).
    if (!num) out$n_lev <- length(unique(data[[nm]]))
    lbl <- dt_col_label(data[[nm]], nm)
    if (!is.null(lbl)) out$label <- lbl
    out
  })
}

# Stamp the gear's state onto the rendered <table>. One JSON attribute rather
# than a dozen scalars: `titles` has to carry null-vs-"" (auto vs explicitly
# none), which an HTML attribute cannot say, and `cols` / `columns` are arrays.
#' @noRd
summarize_table_attrs <- function(prep, cfg) {
  if (is.null(cfg)) return("")
  # Facet levels travel too: the Compare picker offers levels, not columns.
  cfg$facet_levels <- as.list(prep$facet_levels %||% character())
  cfg$search <- if (isTRUE(cfg$search)) "on" else "off"
  cfg$sortable <- if (isTRUE(cfg$sortable %||% TRUE)) "on" else "off"
  cfg$axis <- if (isTRUE(cfg$axis %||% TRUE)) "on" else "off"
  cfg$value_labels <- if (isFALSE(cfg$value_labels)) "off" else "on"
  cfg$download <- if (isTRUE(cfg$download)) "on" else "off"
  cfg$bar_width <- summarize_bar_width(cfg$bar_width)
  json <- as.character(jsonlite::toJSON(cfg, auto_unbox = TRUE, null = "null"))
  # A preset is a CSS hook on the table; "fit" is the stylesheet's default
  # and needs none.
  width <- if (!identical(cfg$bar_width, "fit")) {
    paste0(" data-summarize-width=\"", cfg$bar_width, "\"")
  }
  paste0(" data-summarize-cfg=\"", summarize_esc(json), "\"", width)
}

#' The `bar_width` vocabulary: an unknown or empty value reads as "fit".
#' @noRd
summarize_bar_width <- function(x) {
  x <- summarize_chr1(x) %||% "fit"
  if (x %in% c("narrow", "medium", "wide", "fit")) x else "fit"
}

#' The chrome alone: container, control row, empty bands, empty scroll wrapper.
#'
#' Rendered ONCE by the block (a one-shot `renderUI`, like the table block's
#' chrome) so the gear, the search text and the scroll position outlive every
#' body update. summarize-table.js fills the body and the bands from each pushed
#' payload.
#'
#' @param max_height,search,drill,elem_id Same meaning as in
#' `summarize_table()`.
#' @return An [htmltools::tagList()].
#' @noRd
summarize_chrome_shell <- function(max_height = NULL, search = TRUE,
                                   drill = NULL, elem_id = NULL,
                                   download = NULL, ctrl_target = "") {
  summarize_chrome(
    inner = htmltools::HTML(""), prep = NULL, max_height = max_height,
    search = search, drill = drill, elem_id = elem_id, shell = TRUE,
    download = download, ctrl_target = ctrl_target
  )
}
