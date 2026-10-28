# The records layout's view: the cell model (R/table-model-records.R) as the
# table the browser draws, `derived$table` in the block's payload.

#' A flat records table as the browser draws it
#'
#' @param m dt_flat_build()'s cell model, `kind = "flat"`.
#' @return A TableModel list: `n`, `stub`, `cols`, `head`, `widths`, `rows`
#'   and `drill`.
#' @noRd
table_records_view <- function(m) {
  stopifnot(identical(m$kind, "flat"))
  cols <- c(list(table_view_col(m$stub)), lapply(m$cells, table_view_col))
  head <- c(
    list(table_view_head(m$stub, 0L, stub = TRUE)),
    lapply(seq_along(m$cells), function(j) {
      table_view_head(m$cells[[j]], j)
    })
  )
  out <- list(
    n = m$n, stub = 0L, cols = cols, head = list(head),
    widths = as.numeric(m$widths),
    rows = if (!is.null(m$stub$tint)) list(tint = m$stub$tint),
    drill = m$onclick_col
  )
  table_view_arrays(out[!vapply(out, is.null, logical(1L))])
}

#' Per-row vectors as JSON arrays
#'
#' Shiny writes an output's value with `auto_unbox`, so a one-row table's
#' columns would arrive as bare strings. Every vector the TableModel holds
#' per row or per column is marked as an array.
#' @param m A TableModel list.
#' @return The same list.
#' @noRd
table_view_arrays <- function(m) {
  arr <- function(x) if (is.null(x) || is.list(x)) x else I(unname(x))
  per_col <- c("disp", "raw", "key", "bg", "fg", "bar")
  m$cols <- lapply(m$cols, function(cc) {
    cc[intersect(per_col, names(cc))] <- lapply(cc[intersect(per_col,
                                                             names(cc))], arr)
    cc
  })
  if (!is.null(m$rows)) {
    keep <- setdiff(names(m$rows), "keys")
    m$rows[keep] <- lapply(m$rows[keep], arr)
  }
  if (!is.null(m$widths)) m$widths <- arr(m$widths)
  m
}

# One column of the cell model as a TableCol: name, numeric flag, display
# text and raw values, and the paint as values.
table_view_col <- function(cc) {
  out <- list(name = cc$name, num = isTRUE(cc$num), disp = cc$disp,
              raw = cc$raw, bg = cc$bg, fg = cc$fg, bar = cc$bar,
              bar_fill = cc$bar_fill)
  out[!vapply(out, is.null, logical(1L))]
}

# A leaf header cell: the column's name, its label on the second line.
table_view_head <- function(cc, col, stub = FALSE) {
  out <- list(text = cc$name, sub = cc$label, col = col,
              stub = if (stub) TRUE)
  out[!vapply(out, is.null, logical(1L))]
}

# --- the records layout's part of the block server (R/table-server.R) -----

#' The records layout on the kit
#'
#' The input rows as the table, or the rows nested under one header per
#' level of `by`, or a frame that carries its own structure (a Table 1 from
#' upstream) as it comes.
#' @param kit The kit (viz_block_server()).
#' @param src table_sources()'s list.
#' @return A part, as R/table-server.R describes it.
#' @noRd
table_records_part <- function(kit, src) {
  st <- kit$state
  shown <- shiny::reactive({
    d <- src$rows()
    if (length(st$by()) && is.data.frame(d)) {
      d <- table_nest(d, st$by(), st$rowname(), missing = st$missing(),
                      columns = st$columns())
    }
    d
  })
  table_frame_part(kit, src, shown, layout = "records")
}

# The part shared by the records and the variables layouts: both draw a
# frame, flat or structured, and filter it on a click.
table_frame_part <- function(kit, src, shown, layout, filter = identity) {
  st <- kit$state
  frame <- function() tryCatch(shown(), error = function(e) NULL)
  list(
    shown = shown,
    view = shiny::reactive(table_frame_view(src$values(), frame(), src,
                                            layout)),
    expr = function(sel) {
      d <- frame()
      s <- table_selection_records(sel, structured = dt_is_structured(d))
      ex <- if (identical(layout, "records") && length(st$by())) {
        table_nest_drill_expr(st$by(), s$group_cols, s$group_vals)
      } else {
        filter(table_records_expr(s, source = table_drill_is_source(
          st$drill(), d
        )))
      }
      src$prep$wrap_after(ex)
    },
    claims = function(sel) {
      table_frame_claims(sel, frame(), st$drill_table(), src$subject())
    },
    receipt = function(sel) table_frame_receipt(sel, frame()),
    exhibit = function(tt) {
      table_frame_exhibit(table_state_values(st), shown(), tt, src, layout)
    },
    title_args = function(v) viz_slot_values(kit$spec, v)
  )
}

# The table model of a frame: structured as it comes, else the flat
# records table with its tint and, in the cards look, the cards.
table_frame_view <- function(v, d, src, layout) {
  if (!is.data.frame(d)) {
    return(list(empty = list(kind = "no_data", message = "No data")))
  }
  if (dt_is_structured(d)) {
    m <- table_structured_view(d, drill = v$drill, digits = v$format,
                               unit = v$unit,
                               collapsible = isTRUE(v$collapsible))
    if (is.null(m)) {
      return(list(empty = list(kind = "no_rows",
                               message = "No rows to display")))
    }
    return(list(model = m))
  }
  v$by <- character()
  tint <- table_model_tint(v, d, src$scale_map())
  m <- table_model_records(v, d, tint = tint)
  if (identical(m$kind, "message")) {
    return(list(empty = list(kind = "message", message = m$message)))
  }
  model <- table_records_view(m)
  if (identical(v$look, "cards") && identical(layout, "records")) {
    model$cards <- cards_payload(table_cards_records(
      d, label_col = v$rowname, value_cols = v$columns, digits = v$format,
      unit = v$unit, row_hex = tint$hex
    ))
  }
  list(model = model)
}

# Is `drill` the "source" mode, or a column that happens to be called that?
table_drill_is_source <- function(drill, d) {
  identical(drill, "source") && is.data.frame(d) &&
    !"source" %in% names(d) && dt_is_structured(d)
}

# The claims of a click on a frame: the clicked rows' identity, or the
# clicked column's values, and the clicked cell's or header's column.
table_frame_claims <- function(sel, d, table, subject) {
  s <- table_selection_records(sel, structured = dt_is_structured(d))
  filters <- if (length(s$group_cols)) {
    stats::setNames(as.list(s$group_vals), s$group_cols)
  } else if (!is.null(s$column) && length(s$values)) {
    stats::setNames(list(s$values), s$column)
  } else {
    list()
  }
  rows <- dd_ctrl_claims(d, table, filters, subject = subject)
  if (is.null(rows)) {
    return(NULL)
  }
  c(dd_col_claims(s$col_keys, table), rows)
}

# The footer's "Filtered: ..." line for the held selection.
table_frame_receipt <- function(sel, d) {
  s <- table_selection_records(sel, structured = dt_is_structured(d))
  col_lab <- dd_col_keys_label(s$col_keys)
  if (length(s$group_cols) || length(col_lab)) {
    lab <- c(dt_claim_label(s$group_cols, s$group_vals, dt_group_names(d)),
             col_lab)
    return(if (length(lab)) paste0("Filtered: ", paste(lab, collapse = ", ")))
  }
  if (!is.null(s$column) && length(s$values)) {
    return(paste0("Filtered: ", s$column, " = ",
                  paste(s$values, collapse = ", ")))
  }
  NULL
}

# The downloads' exhibit: the frame as shown, with the shaded cells' paint
# as `.bg:` / `.fg:` columns and the unit on the numeric columns' headers;
# the cards look writes cards.
table_frame_exhibit <- function(v, d, tt, src, layout) {
  shiny::req(is.data.frame(d))
  structured <- dt_is_structured(d)
  frame <- dt_unit_labels(
    dt_exhibit_frame(d, v$rowname, v$columns,
                     shadings = dd_parse_shadings(v$shadings)),
    v$unit
  )
  cards <- if (identical(v$look, "cards") && identical(layout, "records") &&
                 !structured) {
    cards_exhibit(table_cards_records(
      d, label_col = v$rowname, value_cols = v$columns, digits = v$format,
      unit = v$unit, row_hex = table_model_tint(v, d, src$scale_map())$hex
    ))
  }
  new_viz_exhibit(
    frame, render = cards, title = tt$title, subtitle = tt$subtitle,
    caption = tt$caption,
    meta = list(dataset = dl_dataset(d), source = d,
                digits = if (!structured) v$format,
                collapsible = isTRUE(v$collapsible),
                sortable = isTRUE(v$sortable))
  )
}
