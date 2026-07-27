# Ranked bar table: HTML ------------------------------------------------------
#
# Emits the table-block chrome (shared CSS, sticky-header scroll wrapper,
# search input, title / subtitle / caption bands) with a rank-specific
# <table> inside it, plus one small JS dependency for search / sort /
# expand / row-click. See R/rank-table.R for the data half.

#' Ranked bar table
#'
#' Renders a ranked horizontal-bar table: one row per level of `group`,
#' ordered by the measure, with the bar drawn as a div in a cell. Search,
#' click-to-sort, exact values, a sticky header and an arbitrary row count
#' come from the table form; the bar carries the magnitude.
#'
#' The bar column takes one of four shapes, picked by the arguments:
#' plain (`group` only), split into segments (`color`), one column per level
#' (`facet`), or centred on zero (`facet` + `compare`).
#'
#' @param data A data frame.
#' @param group Column to rank by (one row per level).
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
#' @param compare With `facet`, the level to treat as the comparator: each
#'   other level gets a zero-centred difference bar in percentage points.
#' @param cols Opt-in separate numeric columns beside the bar: any of `"n"`,
#'   `"pct"`. By default the bar cell carries its own value label.
#' @param fields Extra columns from the underlying row (identity measure
#'   only), shown as real columns beside the bar.
#' @param sort_by,sort_dir Server-side ordering. `sort_by` is `"value"`,
#'   `"label"` or a facet level name; `sort_dir` is `"desc"` or `"asc"`.
#' @param top_n Optional cap. Off by default -- the table scrolls instead.
#'   When set, the rows below the cut are reported in a visible fold row.
#' @param max_height CSS max-height of the scroll container.
#' @param search Show the search input.
#' @param title,subtitle,caption Display text, already resolved (see
#'   `resolve_block_title()`).
#' @param drill Column a row click filters on, or `NULL` for a display-only
#'   table.
#' @param scale_map Board scale map, for palette agreement with the charts.
#' @param elem_id Shiny namespaced id used to build the `_action` input the
#'   JS emits. `NULL` outside a block (a static table).
#' @param active Active drill filter, `list(col =, vals =)`, for the row
#'   highlight and the status line.
#'
#' @return An [htmltools::tagList()].
#' @examplesIf interactive()
#' rank_table(mtcars, group = "cyl", func = "count")
#' @export
rank_table <- function(data, group = NULL, value = ".count", func = "count",
                       id_var = NULL, parent = NULL, color = NULL,
                       bar_mode = "stacked", facet = NULL, compare = NULL,
                       cols = NULL, fields = NULL, sort_by = "value",
                       sort_dir = "desc", top_n = NULL, max_height = "600px",
                       search = TRUE, title = NULL, subtitle = NULL,
                       caption = NULL, drill = NULL, scale_map = NULL,
                       elem_id = NULL, active = NULL) {
  prep <- rank_prepare(
    data, group = group, value = value, func = func, id_var = id_var,
    parent = parent, color = color, bar_mode = bar_mode, facet = facet,
    compare = compare, cols = cols, fields = fields, sort_by = sort_by,
    sort_dir = sort_dir, top_n = top_n, scale_map = scale_map
  )

  # The three display slots follow the chart / table contract: NULL = auto
  # (the input's own label / subtitle / caption attribute), "" = none, else a
  # {...} template resolved against the current data. The block resolves them
  # before calling; resolving again here is a no-op for a plain string and
  # gives a standalone rank_table() the same auto tier.
  title_raw <- title
  subtitle_raw <- subtitle
  caption_raw <- caption
  title <- resolve_block_title(title, data, auto = rank_attr(data, "label"))
  subtitle <- resolve_block_title(subtitle, data,
                                  auto = rank_attr(data, "subtitle"))
  caption <- resolve_block_title(caption, data, auto = rank_attr(data, "caption"))

  # The gear's working state: the block's config as given, plus the pickable
  # input columns. Read back by rank-table.js off the rendered <table>.
  cfg <- list(
    group = group, parent = parent, color = color, facet = facet,
    compare = compare, func = func, value = value, id_var = id_var,
    bar_mode = bar_mode, cols = cols, fields = fields, sort_by = sort_by,
    sort_dir = sort_dir, top_n = top_n, search = search, drill = drill,
    titles = list(
      title = title, subtitle = subtitle, caption = caption,
      title_state = title_raw, subtitle_state = subtitle_raw,
      caption_state = caption_raw
    ),
    columns = rank_gear_cols(data)
  )

  rank_chrome(
    inner = if (!is.null(prep$err)) {
      rank_message_table(prep$err)
    } else {
      htmltools::HTML(rank_table_html(prep, drill = drill, active = active,
                                      cfg = cfg))
    },
    prep = prep, max_height = max_height, search = search,
    title = title, subtitle = subtitle, caption = caption,
    drill = drill, elem_id = elem_id, active = active
  )
}

#' @noRd
rank_message_table <- function(msg = "No data") {
  htmltools::tags$table(
    class = "blockr-table blockr-rank-table",
    htmltools::tags$tbody(
      htmltools::tags$tr(htmltools::tags$td(class = "blockr-data", msg))
    )
  )
}

# Chrome: title bands, toolbar (search + legend), scroll wrapper, caption,
# status line. Mirrors dt_chrome() -- same classes, so the shared table CSS
# and the design tokens apply unchanged -- plus the rank delta CSS and JS.
#' @noRd
rank_chrome <- function(inner, prep = NULL, max_height = "600px", search = TRUE,
                        title = NULL, subtitle = NULL, caption = NULL,
                        drill = NULL, elem_id = NULL, active = NULL,
                        shell = FALSE) {
  legend <- if (isTRUE(shell)) rank_legend_tag(NULL) else rank_legend(prep)

  # The control row holds the search only; rank-table.js hoists it into the gear
  # row so search sits left of the gear (table-block parity), which keeps that
  # row at its 30px.
  header <- htmltools::tags$div(
    class = "blockr-html-table-header",
    if (isTRUE(search)) {
      htmltools::tags$div(
        class = "blockr-html-table-toolbar",
        htmltools::tags$input(
          type = "search", class = "blockr-search",
          placeholder = "Search\u2026", `aria-label` = "Search table"
        )
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
  } else if (rank_nz(title) || rank_nz(subtitle)) {
    htmltools::tags$div(
      class = "dd-table-titles",
      if (rank_nz(title)) {
        htmltools::tags$div(class = "dd-table-title", title)
      },
      if (rank_nz(subtitle)) {
        htmltools::tags$div(class = "dd-table-subtitle", subtitle)
      }
    )
  }

  scroll_style <- if (!is.null(max_height)) {
    paste0("max-height:", max_height, ";overflow:auto;")
  } else {
    "overflow:auto;"
  }

  htmltools::tagList(
    htmltools::tags$style(htmltools::HTML(html_table_shared_css_fallback())),
    htmltools::tags$style(htmltools::HTML(rank_table_css())),
    rank_table_dep(),
    htmltools::tags$div(
      class = "blockr-html-table-container blockr-rank-container",
      `data-rank-elem-id` = elem_id,
      `data-rank-drill` = drill,
      htmltools::tags$div(class = "blockr-rank-scope", header),
      titles,
      # The legend sits below the title band and above the table, never in the
      # control row -- a long legend must not push the search box around.
      legend,
      htmltools::tags$div(
        class = "blockr-table-wrapper", style = scroll_style, inner
      ),
      if (isTRUE(shell)) {
        htmltools::tags$div(class = "dd-table-caption", style = "display:none")
      } else if (rank_nz(caption)) {
        htmltools::tags$div(class = "dd-table-caption", caption)
      },
      if (isTRUE(shell)) {
        rank_footer_tag(NULL)
      } else {
        rank_footer(prep, drill = drill, active = active)
      }
    )
  )
}

# The footer's content as DATA: the count line, the note a reinterpreted config
# leaves, and the active drill filter. One definition, two consumers -- the
# chrome renders it server-side, and rank-table.js refreshes it from the
# payload without re-rendering the container.
#' @noRd
rank_foot_spec <- function(prep, drill = NULL, active = NULL) {
  if (!is.null(prep$err)) {
    return(list(count = "", note = NULL, filter = NULL, reset = FALSE))
  }
  n_shown <- if (is.null(prep$parent)) {
    sum(!prep$rows$.is_parent)
  } else {
    sum(prep$rows$.is_parent)
  }
  act <- as.character(unlist(active$vals %||% character()))
  list(
    count = paste0(
      n_shown, " of ", prep$n_total, " ",
      if (is.null(prep$parent)) "rows" else "groups",
      if (prep$folded > 0L) {
        paste0(", ", prep$folded, " folded")
      } else {
        ", all rendered"
      }
    ),
    note = prep$note,
    filter = if (length(act)) paste(act, collapse = ", ") else NULL,
    reset = !is.null(drill)
  )
}

# The legend as DATA. Always present for two or more series -- colour alone must
# not carry identity.
#' @noRd
rank_legend_spec <- function(prep) {
  if (!is.null(prep$err)) return(NULL)
  if (identical(prep$layout, "compare")) {
    return(list(
      title = "Direction",
      items = list(
        list(label = paste0("More than ", prep$compare),
             color = "var(--blockr-rank-pos, #d03b3b)"),
        list(label = paste0("Less than ", prep$compare),
             color = "var(--blockr-rank-neg, #2a78d6)")
      )
    ))
  }
  # Colour identity lives in the COLOUR mapping only: a plain facet's bars are
  # all the house blue and its column headers already name the levels, so it
  # carries no legend (chart parity -- a facet never recolours the marks).
  if (is.null(prep$color)) return(NULL)
  lv <- prep$series
  if (length(lv) < 2L) return(NULL)
  list(
    title = prep$color,
    items = lapply(lv, function(l) {
      list(label = l, color = unname(prep$palette[[l]]))
    })
  )
}

#' @noRd
rank_footer <- function(prep, drill = NULL, active = NULL) {
  rank_footer_tag(rank_foot_spec(prep, drill = drill, active = active))
}

#' @noRd
rank_footer_tag <- function(spec) {
  if (is.null(spec)) spec <- list(count = "", reset = FALSE)
  htmltools::tags$div(
    class = "blockr-rank-footer",
    htmltools::tags$span(class = "blockr-rank-count", spec$count %||% ""),
    htmltools::tags$span(class = "blockr-rank-note", spec$note %||% ""),
    htmltools::tags$span(
      class = "blockr-rank-status",
      style = if (is.null(spec$filter)) "display:none" else NULL,
      htmltools::tags$span(class = "blockr-rank-dot"),
      htmltools::tags$span(
        class = "blockr-rank-status-text",
        if (!is.null(spec$filter)) paste0("Filtering downstream: ", spec$filter)
      ),
      if (isTRUE(spec$reset)) {
        htmltools::tags$button(type = "button", class = "blockr-rank-reset",
                               "Reset")
      }
    )
  )
}

#' @noRd
rank_legend <- function(prep) {
  rank_legend_tag(rank_legend_spec(prep))
}

#' @noRd
rank_legend_tag <- function(spec) {
  htmltools::tags$div(
    class = "blockr-rank-legend",
    style = if (is.null(spec)) "display:none" else NULL,
    if (!is.null(spec)) {
      htmltools::tagList(
        htmltools::tags$span(class = "blockr-rank-legend-title", spec$title),
        lapply(spec$items, function(it) {
          htmltools::tags$span(
            class = "blockr-rank-legend-item",
            htmltools::tags$i(style = paste0("background:", it$color)),
            it$label
          )
        })
      )
    }
  )
}

#' @noRd
rank_nz <- function(x) {
  !is.null(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

# Escape exactly as htmltools does for a text child -- & < > and the attribute
# quote -- because the JS row assembler applies the same rules and the two
# outputs must not drift.
#' @noRd
rank_esc <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

# A data-frame-level display attribute, or NULL when absent / not a string.
#' @noRd
rank_attr <- function(data, nm) {
  v <- attr(data, nm, exact = TRUE)
  if (is.character(v) && length(v) == 1L && nzchar(v)) v else NULL
}
#' The two parts of a numeric cell: the value string, and the percent string
#' when the column shows both. Data, never markup -- the consumers wrap it.
#' @noRd
rank_num_parts <- function(v, denom = NULL, combined = FALSE, signed = FALSE,
                           pct_only = FALSE) {
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
  n <- ifelse(is.na(v), "",
              formatC(v, format = "fg", digits = 6L, big.mark = ""))
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
rank_table_html <- function(prep, drill = NULL, active = NULL, cfg = NULL) {
  rank_cells_html(rank_cells(prep, drill = drill, active = active, cfg = cfg))
}

#' @noRd
rank_label_header <- function(prep) {
  if (is.null(prep$parent)) prep$group else paste0(prep$parent, " / ", prep$group)
}

# The same bundle the table block ships (shared blockr.dplyr CSS/JS, Blockr.Select,
# the dd-* popover CSS, the settings band and the gear engine), plus the rank JS
# LAST -- it reads Blockr.DrilldownConfig at bind time.
#' @noRd
rank_table_dep <- memoise0(function() {
  htmltools::tagList(
    drilldown_table_dep(),
    htmltools::htmlDependency(
      name = "blockr-viz-rank",
      version = paste0(utils::packageVersion("blockr.viz"), ".6"),
      src = system.file("js", package = "blockr.viz"),
      script = "rank-table.js"
    )
  )
})

# The sort key for a cell: the raw number (or, for a text field column, the
# raw text -- escaped, it lives in an attribute), so the client never parses
# a formatted string (and a bar cell, which has no text at all, still sorts).
#' @noRd
rank_data_v <- function(v) {
  # format() common-width-pads a CHARACTER vector even with trim (trim only
  # suppresses numeric left-padding) -- text sort keys pass through as-is.
  s <- if (is.character(v)) v else format(v, scientific = FALSE, trim = TRUE)
  paste0(" data-v=\"", rank_esc(ifelse(is.na(v), "", s)), "\"")
}

# Pickable input columns for the gear's column pickers, in the shape the shared
# config engine expects ({name, type, label}). Always the block's RAW input
# columns -- not the displayed projection -- so the pickers stay correct while
# the table shows an aggregated frame (dt_gear_cols_json's rule).
#' @noRd
rank_gear_cols <- function(data) {
  if (!is.data.frame(data)) return(list())
  lapply(names(data), function(nm) {
    out <- list(
      name = nm,
      type = if (is.numeric(data[[nm]])) "numeric" else "categorical"
    )
    lbl <- dt_col_label(data[[nm]], nm)
    if (!is.null(lbl)) out$label <- lbl
    out
  })
}

# Stamp the gear's state onto the rendered <table>. One JSON attribute rather
# than a dozen scalars: `titles` has to carry null-vs-"" (auto vs explicitly
# none), which an HTML attribute cannot say, and `cols` / `columns` are arrays.
#' @noRd
rank_table_attrs <- function(prep, cfg) {
  if (is.null(cfg)) return("")
  # Facet levels travel too: the Compare picker offers levels, not columns.
  cfg$facet_levels <- as.list(prep$facet_levels %||% character())
  cfg$search <- if (isTRUE(cfg$search)) "on" else "off"
  json <- as.character(jsonlite::toJSON(cfg, auto_unbox = TRUE, null = "null"))
  paste0(" data-rank-cfg=\"", rank_esc(json), "\"")
}

#' The chrome alone: container, control row, empty bands, empty scroll wrapper.
#'
#' Rendered ONCE by the block (a one-shot `renderUI`, like the table block's
#' chrome) so the gear, the search text and the scroll position outlive every
#' body update. rank-table.js fills the body and the bands from each pushed
#' payload.
#'
#' @param max_height,search,drill,elem_id Same meaning as in [rank_table()].
#' @return An [htmltools::tagList()].
#' @noRd
rank_chrome_shell <- function(max_height = "600px", search = TRUE,
                              drill = NULL, elem_id = NULL) {
  rank_chrome(
    inner = htmltools::HTML(""), prep = NULL, max_height = max_height,
    search = search, drill = drill, elem_id = elem_id, shell = TRUE
  )
}
