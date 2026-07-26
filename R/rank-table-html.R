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
#' @param color Optional column splitting each bar into segments.
#' @param bar_mode `"stacked"`, `"grouped"` or `"percent"`. No-op without
#'   `color`.
#' @param facet Optional column giving one bar column per level, all on one
#'   shared scale. Takes precedence over `color`.
#' @param compare With `facet`, the level to treat as the comparator: each
#'   other level gets a zero-centred difference bar in percentage points.
#' @param cols Numeric columns beside the bar: any of `"n"`, `"pct"`.
#' @param sort_by,sort_dir Server-side ordering. `sort_by` is `"value"`,
#'   `"label"` or a facet level name; `sort_dir` is `"desc"` or `"asc"`.
#' @param top_n Optional cap. Off by default -- the table scrolls instead.
#'   When set, the rows below the cut are reported in a visible fold row.
#' @param max_height CSS max-height of the scroll container.
#' @param search Show the search input.
#' @param title,subtitle,caption Display text, already resolved (see
#'   [resolve_block_title()]).
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
                       cols = c("n", "pct"), sort_by = "value",
                       sort_dir = "desc", top_n = NULL, max_height = "600px",
                       search = TRUE, title = NULL, subtitle = NULL,
                       caption = NULL, drill = NULL, scale_map = NULL,
                       elem_id = NULL, active = NULL) {
  prep <- rank_prepare(
    data, group = group, value = value, func = func, id_var = id_var,
    parent = parent, color = color, bar_mode = bar_mode, facet = facet,
    compare = compare, cols = cols, sort_by = sort_by, sort_dir = sort_dir,
    top_n = top_n, scale_map = scale_map
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
    bar_mode = bar_mode, cols = cols, sort_by = sort_by, sort_dir = sort_dir,
    top_n = top_n, search = search, drill = drill,
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
rank_chrome <- function(inner, prep, max_height = "600px", search = TRUE,
                        title = NULL, subtitle = NULL, caption = NULL,
                        drill = NULL, elem_id = NULL, active = NULL) {
  legend <- rank_legend(prep)

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
          placeholder = "Search…", `aria-label` = "Search table"
        )
      )
    }
  )

  # Title band: the canonical .dd-table-titles the chart and table blocks use
  # (inst/css/table.css) -- title over subtitle, a hairline under the band
  # dividing it from the column headers. Hidden entirely when both are empty.
  titles <- if (rank_nz(title) || rank_nz(subtitle)) {
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
      if (rank_nz(caption)) {
        htmltools::tags$div(class = "dd-table-caption", caption)
      },
      rank_footer(prep, drill = drill, active = active)
    )
  )
}

# Footer: the row count (with what was folded, if anything), the note when a
# config had to be reinterpreted, and the active-filter status + Reset.
#' @noRd
rank_footer <- function(prep, drill = NULL, active = NULL) {
  if (!is.null(prep$err)) return(NULL)
  n_shown <- if (is.null(prep$parent)) {
    sum(!prep$rows$.is_parent)
  } else {
    sum(prep$rows$.is_parent)
  }
  bits <- paste0(
    n_shown, " of ", prep$n_total, " ",
    if (is.null(prep$parent)) "rows" else "groups",
    if (prep$folded > 0L) {
      paste0(", ", prep$folded, " folded")
    } else {
      ", all rendered"
    }
  )
  act_vals <- as.character(unlist(active$vals %||% character()))
  htmltools::tags$div(
    class = "blockr-rank-footer",
    htmltools::tags$span(class = "blockr-rank-count", bits),
    if (!is.null(prep$note)) {
      htmltools::tags$span(class = "blockr-rank-note", prep$note)
    },
    htmltools::tags$span(
      class = "blockr-rank-status",
      style = if (!length(act_vals)) "display:none" else NULL,
      htmltools::tags$span(class = "blockr-rank-dot"),
      htmltools::tags$span(
        class = "blockr-rank-status-text",
        if (length(act_vals)) {
          paste0("Filtering downstream: ", paste(act_vals, collapse = ", "))
        }
      ),
      if (!is.null(drill)) {
        htmltools::tags$button(
          type = "button", class = "blockr-rank-reset", "Reset"
        )
      }
    )
  )
}

# Legend: always present for two or more series, since colour alone must not
# carry identity.
#' @noRd
rank_legend <- function(prep) {
  if (!is.null(prep$err)) return(NULL)
  if (identical(prep$layout, "compare")) {
    items <- list(
      list(label = paste0("More than ", prep$compare), color = "var(--blockr-rank-pos, #d03b3b)"),
      list(label = paste0("Less than ", prep$compare), color = "var(--blockr-rank-neg, #2a78d6)")
    )
    ttl <- "Direction"
  } else {
    lv <- if (identical(prep$layout, "split")) prep$series else prep$facet_levels
    if (length(lv) < 2L) return(NULL)
    items <- lapply(lv, function(l) {
      list(label = l, color = prep$palette[[l]])
    })
    ttl <- if (identical(prep$layout, "split")) prep$color else prep$facet
  }
  htmltools::tags$div(
    class = "blockr-rank-legend",
    htmltools::tags$span(class = "blockr-rank-legend-title", ttl),
    lapply(items, function(it) {
      htmltools::tags$span(
        class = "blockr-rank-legend-item",
        htmltools::tags$i(style = paste0("background:", it$color)),
        it$label
      )
    })
  )
}

# A data-frame-level display attribute, or NULL when absent / not a string.
#' @noRd
rank_attr <- function(data, nm) {
  v <- attr(data, nm, exact = TRUE)
  if (is.character(v) && length(v) == 1L && nzchar(v)) v else NULL
}

#' @noRd
rank_nz <- function(x) {
  !is.null(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

#' @noRd
rank_esc <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

# --- marks -------------------------------------------------------------------
# One fill, rounded data-end, square at the baseline. Vectorised over the
# column so a whole bar column is one paste0 (the table block's fast path --
# see dt_bar_style).
#' @noRd
rank_bar_div <- function(v, mx, fill = NULL, sub = FALSE) {
  w <- if (is.finite(mx) && mx > 0) {
    pmax(0, pmin(100, abs(v) / mx * 100))
  } else {
    rep(0, length(v))
  }
  w[!is.finite(w)] <- 0
  # `sub` is vectorised over the column: in a nested table the child rows draw
  # a lighter step of the same hue, so depth reads without a second colour.
  sub <- rep_len(isTRUE(sub) | (is.logical(sub) & !is.na(sub) & sub), length(v))
  paste0(
    "<div class=\"blockr-rank-track", ifelse(sub, " is-sub", ""), "\">",
    "<div class=\"blockr-rank-fill\" style=\"width:",
    format(round(w, 2), trim = TRUE, scientific = FALSE), "%",
    if (!is.null(fill)) paste0(";background:", fill) else "", "\"></div></div>"
  )
}

# A split bar. stacked = segments to scale; percent = segments to 100% of the
# row; grouped = one thin track per series, shared scale. A 2px surface gap
# separates touching segments (the CSS supplies it), never a stroke.
#' @noRd
rank_bar_split <- function(mat, mx, series, pal, mode = "stacked",
                           labels = NULL) {
  n <- nrow(mat)
  k <- length(series)
  if (!n || !k) return(rep("<div class=\"blockr-rank-track\"></div>", n))
  mat[!is.finite(mat)] <- 0
  tot <- rowSums(mat)
  fmt <- function(x) format(round(x, 2), trim = TRUE, scientific = FALSE)
  # Vectorised over the WHOLE column, one paste0 per series (never per row):
  # the per-row loop this replaced cost 277ms on a 5k-row table, which is the
  # same fast-path rule dt_bar_style() follows for the table block's data bars.
  if (identical(mode, "grouped")) {
    w <- if (is.finite(mx) && mx > 0) pmin(mat / mx * 100, 100) else mat * 0
    seg <- vapply(seq_len(k), function(j) {
      paste0(
        "<div class=\"blockr-rank-row3\"><div class=\"blockr-rank-fill\"",
        " style=\"width:", fmt(w[, j]), "%;background:", pal[[series[[j]]]],
        "\" title=\"", rank_esc(series[[j]]), ": ", mat[, j],
        "\"></div></div>"
      )
    }, character(n))
    seg <- matrix(seg, nrow = n)
    return(paste0("<div class=\"blockr-rank-track is-tall\">",
                  apply(seg, 1L, paste0, collapse = ""), "</div>"))
  }
  # stacked = segments to scale; percent = each row normalised to 100%.
  scale <- if (identical(mode, "percent")) {
    ifelse(tot > 0, 100, 0)
  } else if (is.finite(mx) && mx > 0) {
    tot / mx * 100
  } else {
    tot * 0
  }
  share <- ifelse(tot > 0, 1, 0) * mat / ifelse(tot > 0, tot, 1)
  w <- share * scale
  seg <- vapply(seq_len(k), function(j) {
    # A zero segment emits nothing at all, so an absent level adds no markup.
    ifelse(
      mat[, j] > 0,
      paste0(
        "<div class=\"blockr-rank-fill\" style=\"width:", fmt(w[, j]),
        "%;background:", pal[[series[[j]]]], "\" title=\"",
        rank_esc(series[[j]]), ": ", mat[, j], "\"></div>"
      ),
      ""
    )
  }, character(n))
  seg <- matrix(seg, nrow = n)
  paste0("<div class=\"blockr-rank-track\">",
         apply(seg, 1L, paste0, collapse = ""), "</div>")
}

# Zero-centred bar: colour is polarity, not identity, so the pair is
# warm/cool with a neutral tick at zero.
#' @noRd
rank_bar_diverge <- function(v, mx) {
  w <- if (is.finite(mx) && mx > 0) pmin(50, abs(v) / mx * 50) else rep(0, length(v))
  w[!is.finite(w)] <- 0
  pos <- !is.na(v) & v >= 0
  paste0(
    "<div class=\"blockr-rank-dv\"><div class=\"blockr-rank-fill ",
    ifelse(pos, "is-pos", "is-neg"), "\" style=\"width:",
    format(round(w, 2), trim = TRUE), "%\"></div></div>"
  )
}

#' @noRd
rank_fmt_num <- function(v, denom = NULL, combined = FALSE, signed = FALSE,
                         pct_only = FALSE) {
  if (isTRUE(signed)) {
    out <- ifelse(
      is.na(v), "",
      paste0(ifelse(v > 0, "+", ifelse(v < 0, "−", "")),
             formatC(abs(v), format = "f", digits = 1L))
    )
    return(out)
  }
  pct <- if (!is.null(denom) && is.finite(denom) && denom > 0) {
    v / denom * 100
  } else {
    NULL
  }
  if (isTRUE(pct_only)) {
    if (is.null(pct)) return(rep("", length(v)))
    return(ifelse(is.na(v), "",
                  paste0(formatC(pct, format = "f", digits = 1L), "%")))
  }
  n <- ifelse(is.na(v), "", formatC(v, format = "fg", digits = 6L, big.mark = ""))
  if (isTRUE(combined) && !is.null(pct)) {
    return(paste0(
      n, " <span class=\"blockr-rank-pct\">(",
      formatC(pct, format = "f", digits = 0L), "%)</span>"
    ))
  }
  n
}

# --- the table ---------------------------------------------------------------
#' @noRd
rank_table_html <- function(prep, drill = NULL, active = NULL, cfg = NULL) {
  rows <- prep$rows
  plan <- prep$plan
  n <- nrow(rows)

  # Header cells come from the TABLE BLOCK's own builder (dt_th), so a rank
  # table's header is the same object as a table block's: the same classes, the
  # same name-over-label two-tier cell, the same sort-arrow slot. Every column
  # is a sort hook -- a bar cell carries `data-v` too, so sorting the bar
  # column means sorting its value.
  th <- c(
    as.character(dt_th(
      rank_label_header(prep), 0L, stub = TRUE,
      label = prep$group_label, sortable = TRUE
    )),
    vapply(seq_along(plan), function(i) {
      p <- plan[[i]]
      as.character(dt_th(
        p$label, i, label = p$sub_label,
        numeric = identical(p$kind, "num"), sortable = TRUE
      ))
    }, character(1L))
  )
  thead <- paste0("<thead><tr>", paste(th, collapse = ""), "</tr></thead>")

  # Body. Each plan entry contributes one column, built for all rows at once.
  cells <- lapply(seq_along(plan), function(i) {
    p <- plan[[i]]
    if (identical(p$kind, "bar")) {
      v <- rows[[p$key]]
      if (!is.null(p$denom) && is.finite(p$denom) && p$denom > 0) {
        v <- v / p$denom * 100
      }
      inner <- rank_bar_div(v, prep$bar_max, fill = p$fill,
                            sub = rows$.level > 0L)
      paste0("<td class=\"blockr-rank-bar-col\"", rank_data_v(v), ">",
             inner, "</td>")
    } else if (identical(p$kind, "barsplit")) {
      mat <- vapply(p$series, function(lv) {
        x <- rows[[paste0(".s_", lv)]]
        if (is.null(x)) rep(0, n) else as.numeric(x)
      }, numeric(n))
      mat <- matrix(mat, nrow = n, dimnames = list(NULL, p$series))
      mat[is.na(mat)] <- 0
      paste0("<td class=\"blockr-rank-bar-col\"",
             rank_data_v(rowSums(mat, na.rm = TRUE)), ">",
             rank_bar_split(mat, prep$bar_max, p$series, prep$palette, p$mode),
             "</td>")
    } else if (identical(p$kind, "bardiv")) {
      paste0("<td class=\"blockr-rank-bar-col\"", rank_data_v(rows[[p$key]]),
             ">", rank_bar_diverge(rows[[p$key]], prep$bar_max), "</td>")
    } else {
      v <- rows[[p$key]]
      txt <- rank_fmt_num(v, denom = p$denom, combined = isTRUE(p$combined),
                          signed = isTRUE(p$signed),
                          pct_only = isTRUE(p$pct_only))
      # data-v carries the number the client sorts on, so sorting never has
      # to parse a formatted string.
      paste0("<td class=\"blockr-rank-num dt-col-num\"", rank_data_v(v), ">",
             txt, "</td>")
    }
  })

  is_par <- rows$.is_parent
  chev <- paste0(
    "<button class=\"blockr-indent-btn\" type=\"button\" tabindex=\"-1\"",
    " aria-expanded=\"false\">", as.character(section_chevron_svg()), "</button>"
  )
  # Child rows are indented on the same 24 + 16px step the structured table
  # uses, and the chevron hangs into that gutter (margin-left:-18px).
  lbl_cell <- paste0(
    "<td class=\"blockr-rank-label-col blockr-stub",
    ifelse(is_par, " blockr-has-toggle", ""), "\"",
    ifelse(rows$.level > 0L, " style=\"padding-left:40px;\"", ""), ">",
    ifelse(is_par, chev, ""),
    "<span class=\"blockr-rank-label\">", rank_esc(rows$.label), "</span></td>"
  )

  act_col <- rank_chr1(active$col)
  act_vals <- as.character(unlist(active$vals %||% character()))
  on <- if (length(act_vals) && !is.null(act_col)) {
    rows$.label %in% act_vals
  } else {
    rep(FALSE, n)
  }

  tr_open <- paste0(
    "<tr class=\"blockr-rank-row",
    ifelse(is_par, " is-parent blockr-indent-toggle collapsed", ""),
    ifelse(rows$.level > 0L, " is-child collapsed-hidden", ""),
    ifelse(!is.null(drill), " is-pick", ""),
    ifelse(on, " is-on", ""),
    "\" data-rank-label=\"", rank_esc(rows$.label), "\"",
    ifelse(rows$.level > 0L,
           paste0(" data-rank-parent=\"", rank_esc(rows$.parent), "\""), ""),
    " data-rank-level=\"", rows$.level, "\">"
  )

  body <- paste0(
    tr_open, lbl_cell,
    do.call(paste0, cells),
    "</tr>"
  )

  fold <- if (prep$folded > 0L) {
    paste0(
      "<tr class=\"blockr-rank-fold\"><td colspan=\"", length(plan) + 1L,
      "\">Other — ", prep$folded, " ",
      if (is.null(prep$parent)) "rows" else "groups",
      " below the cut",
      if (is.finite(prep$fold_max)) {
        paste0(", each with a value ≤ ",
               format(prep$fold_max, scientific = FALSE, trim = TRUE))
      } else {
        ""
      },
      "</td></tr>"
    )
  } else {
    ""
  }

  paste0(
    "<table class=\"blockr-table blockr-rank-table\"",
    " data-rank-nested=\"", if (is.null(prep$parent)) "0" else "1", "\"",
    rank_table_attrs(prep, cfg), ">",
    thead, "<tbody>", paste(body, collapse = ""), fold, "</tbody></table>"
  )
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
      version = paste0(utils::packageVersion("blockr.viz"), ".3"),
      src = system.file("js", package = "blockr.viz"),
      script = "rank-table.js"
    )
  )
})

# The sort key for a cell: the raw number, so the client never parses a
# formatted string (and a bar cell, which has no text at all, still sorts).
#' @noRd
rank_data_v <- function(v) {
  paste0(" data-v=\"",
         ifelse(is.na(v), "", format(v, scientific = FALSE, trim = TRUE)),
         "\"")
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
