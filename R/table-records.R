# The records layout's TableModel: the cell model (R/table-model-records.R)
# as the table the browser draws, the payload's `data`. The TableModel's
# pieces every layout shares are in R/table-model-parts.R.

#' A flat records table as the browser draws it
#'
#' @param m dt_flat_build()'s cell model, `kind = "flat"`.
#' @return A TableModel list: `n`, `stub`, `cols`, `head`, `widths`, `rows`
#'   and `drill`.
#' @noRd
table_records_to_model <- function(m) {
  stopifnot(identical(m$kind, "flat"))
  cols <- c(list(table_model_col(m$stub)), lapply(m$cells, table_model_col))
  head <- c(
    list(table_model_head(m$stub, 0L, stub = TRUE)),
    lapply(seq_along(m$cells), function(j) {
      table_model_head(m$cells[[j]], j)
    })
  )
  out <- list(
    n = m$n, stub = 0L, cols = cols, head = list(head),
    widths = as.numeric(m$widths),
    rows = if (!is.null(m$stub$tint)) list(tint = m$stub$tint),
    drill = m$drill
  )
  table_model_arrays(out[!vapply(out, is.null, logical(1L))])
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
  frame <- shiny::reactive({
    d <- src$rows()
    if (length(st$by()) && is.data.frame(d)) {
      d <- table_nest(d, st$by(), st$rowname(), missing = st$missing(),
                      columns = st$columns())
    }
    d
  })
  table_frame_part(kit, src, frame, layout = "records")
}

# The part shared by the records and the variables layouts: both draw a
# frame, flat or structured, and filter it on a click.
table_frame_part <- function(kit, src, frame, layout, filter = identity) {
  st <- kit$state
  frame_or_null <- function() tryCatch(frame(), error = function(e) NULL)
  list(
    frame = frame,
    drawn = shiny::reactive(
      table_frame_drawn(src$values(), frame_or_null(), src, layout)
    ),
    expr = function(sel) {
      d <- frame_or_null()
      s <- table_click(sel, structured = table_is_structured(d))
      ex <- if (identical(layout, "records") && length(st$by())) {
        table_nest_drill_expr(st$by(), s$filters)
      } else {
        filter(table_records_expr(s, source = table_drill_is_source(
          st$drill(), d
        )))
      }
      src$prep$wrap_after(ex)
    },
    claims = function(sel) {
      table_frame_claims(sel, frame_or_null(), st$drill_table(), src$subject())
    },
    receipt = function(sel) table_frame_receipt(sel, frame_or_null()),
    exhibit = function(tt) {
      table_frame_exhibit(viz_values(st), frame(), tt, src, layout)
    },
    title_args = function(v) viz_slot_values(kit$spec, v)
  )
}

# The table model of a frame: structured as it comes, else the flat
# records table with its tint and, in the cards look, the cards.
table_frame_drawn <- function(v, d, src, layout) {
  if (!is.data.frame(d)) {
    return(list(empty = list(kind = "no_data", message = "No data")))
  }
  if (table_is_structured(d)) {
    m <- table_structured_to_model(d, drill = v$drill, digits = v$format,
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
  model <- table_records_to_model(m)
  if (identical(v$look, "cards") && identical(layout, "records")) {
    model$cards <- cards_model(table_cards_records(
      d, label_col = v$rowname, value_cols = v$columns, digits = v$format,
      unit = v$unit, row_hex = tint$hex
    ))
  }
  list(model = model)
}

# Is `drill` the "source" mode, or a column that happens to be called that?
# The mode exists only where there is no column to choose: a structured
# table without a column of that name.
table_drill_is_source <- function(drill, d) {
  identical(drill, "source") && is.data.frame(d) &&
    !"source" %in% names(d) && table_is_structured(d)
}

# The claims of a click on a frame: the clicked rows' identity, or the
# clicked column's values, and the clicked cell's or header's column.
table_frame_claims <- function(sel, d, table, subject) {
  s <- table_click(sel, structured = table_is_structured(d))
  rows <- ctrl_claims(d, table, s$filters, subject = subject)
  if (is.null(rows)) {
    return(NULL)
  }
  cols <- lapply(names(s$columns), function(cl) {
    claim_cond(cl, s$columns[[cl]], table)
  })
  c(cols, rows)
}

# The footer's "Filtered: ..." line for the held selection.
table_frame_receipt <- function(sel, d) {
  s <- table_click(sel, structured = table_is_structured(d))
  f <- s$filters
  rows <- if (s$structured) {
    table_claim_label(f, table_group_names(d))
  } else if (length(f)) {
    paste0(names(f)[[1L]], " = ", paste(f[[1L]], collapse = ", "))
  }
  lab <- c(rows, table_col_keys_label(s$columns))
  if (length(lab)) paste0("Filtered: ", paste(lab, collapse = ", "))
}

# The downloads' exhibit: the frame as shown, with the shaded cells' paint
# as `.bg:` / `.fg:` columns and the unit on the numeric columns' headers;
# the cards look writes cards.
table_frame_exhibit <- function(v, d, tt, src, layout) {
  shiny::req(is.data.frame(d))
  structured <- table_is_structured(d)
  frame <- dt_unit_labels(
    dt_exhibit_frame(d, v$rowname, v$columns, dd_parse_shadings(v$shadings)),
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
