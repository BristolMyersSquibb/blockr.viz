# Layout builders + top-level tile_html() for new_tile_block().
# Card grids cluster by group; the table renders measure-down (ungrouped) or
# as a group x measure matrix (grouped). Both render the identical cells from
# tile_long_frame() (see tile-block-render.R).

#' Per-measure format specs, resolved from ALL of a measure's values (so every
#' card / cell of one measure formats identically -- not per single value). All
#' measures share the flat render spec (style / good_when / format / unit).
#' @noRd
tile_fspecs <- function(cells, flat) {
  ms_names <- unique(cells$measure)
  out <- list()
  for (m in ms_names) {
    out[[m]] <- tk_resolve_format(flat$format, cells$value[cells$measure == m])
  }
  out
}

#' Unit shown in a matrix column header: the free-text unit, else "%" for a
#' percent format, else nothing.
#' @noRd
tk_header_unit <- function(unit, fspec) {
  if (!is.null(unit) && nzchar(unit)) return(unit)
  if (identical(fspec$kind, "percent")) "%" else ""
}

# ---------------------------------------------------------------------------
# CARD layout
# ---------------------------------------------------------------------------

#' One card for a (group, measure) cell.
#' @noRd
tk_card <- function(cell, flat, fspecs) {
  fspec <- fspecs[[cell$measure]] %||%
    tk_resolve_format(flat$format, cell$value)

  over <- cell$overline
  # The unit is a separate span next to the number ("847 apples", "1.2M USD").
  # Skipped for percent, where "%" is already part of the formatted value.
  unit_node <- if (nzchar(flat$unit %||% "") && !identical(fspec$kind, "percent")) {
    htmltools::tags$span(class = "tk-unit", flat$unit)
  }
  hex <- cell$.hex %||% NA_character_
  valrow <- htmltools::tags$div(
    class = "tk-valrow",
    tk_value_span(cell$value, fspec),
    unit_node,
    if (identical(flat$style, "delta")) {
      tk_secondary_node("delta", cell$secondary[[1]], flat$good_when, fspec)
    }
  )

  sec <- if (!identical(flat$style, "delta")) {
    node <- tk_secondary_node(flat$style, cell$secondary[[1]], flat$good_when,
                              fspec, hex = hex)
    if (!is.null(node)) htmltools::tags$div(class = "tk-secondaries", node)
  }
  cap <- if (!is.na(cell$caption) && nzchar(cell$caption)) {
    htmltools::tags$div(class = "tk-caption", cell$caption)
  }

  # No eyebrow when the overline is NA/empty (a single unlabelled value
  # column, or the Name mapping removed) -- same guard as the caption;
  # an unguarded NA would render as literal "NA" text. With an identity hex
  # ("Color by") the name wears it as a PILL -- a tinted chip with a derived
  # readable text tone (G + Reach 2, settled with Christoph): the identity
  # colors the NAME and the judgment-free fill bar, never the semantic
  # delta / status colors.
  over_node <- if (!is.na(over) && nzchar(over)) {
    # Pill only when the tint keys on the NAME -- then the overline IS the
    # identity. When it keys on the GROUP, the pill sits on the cluster
    # heading (tk_cards_layout) / row stub instead; card overlines stay plain.
    pill <- if (identical(cell$.hex_on %||% "", "name")) {
      tk_ident_pill_style(hex)
    }
    htmltools::tags$div(
      class = paste("tk-overline tk-clamp",
                    if (!is.null(pill)) "tk-overline--pill"),
      style = pill,
      over
    )
  }

  htmltools::tags$article(
    class = "tk-card",
    # Drill level: the group value when grouped, the measure (Name) value on
    # an ungrouped KPI list -- computed once in tile_html (cells$.dg).
    `data-group` = {
      dg <- cell$.dg %||% cell$group
      if (!is.null(dg) && nzchar(dg)) dg else NULL
    },
    over_node,
    valrow, sec, cap
  )
}

#' Inline style for the "Color by" identity pill (a tinted chip on the name /
#' row stub): background = a soft wash of the scale color over the card
#' surface, text = a readable tone of it (color-mix against the theme tokens,
#' so it adapts with them). NULL when the cell carries no hex.
#' @noRd
tk_ident_pill_style <- function(hex) {
  hex <- hex %||% NA_character_
  if (is.na(hex) || !nzchar(hex)) return(NULL)
  paste0(
    "background:color-mix(in srgb, ", hex, " 14%, var(--tk-surface-1));",
    "color:color-mix(in srgb, ", hex, " 68%, var(--tk-ink-1));"
  )
}

#' Cards layout: ungrouped -> a single auto-fit grid (card per measure);
#' grouped -> one labeled sub-grid per group level (card per measure).
#' @noRd
tk_cards_layout <- function(cells, flat, grouped, fspecs) {
  if (!grouped) {
    cards <- lapply(seq_len(nrow(cells)), function(i) {
      tk_card(cells[i, ], flat, fspecs)
    })
    return(htmltools::tags$div(class = "tk-grid", cards))
  }
  groups <- unique(cells$group)
  blocks <- lapply(groups, function(g) {
    sub <- cells[cells$group == g, , drop = FALSE]
    cards <- lapply(seq_len(nrow(sub)),
                    function(i) tk_card(sub[i, ], flat, fspecs))
    # "Color by" the group: the CLUSTER HEADING is the identity's name, so
    # it wears the pill (cards inside stay plain -- their overlines are
    # measure names, not the identity).
    pill <- if (identical(sub$.hex_on[1] %||% "", "group")) {
      tk_ident_pill_style(sub$.hex[1])
    }
    htmltools::tagList(
      htmltools::tags$p(
        class = paste("tk-overline", if (!is.null(pill)) "tk-overline--pill"),
        style = paste0("margin:18px 0 11px;", pill %||% ""),
        g
      ),
      htmltools::tags$div(class = "tk-grid", cards)
    )
  })
  htmltools::tags$div(class = "tk-stack", blocks)
}

# ---------------------------------------------------------------------------
# TABLE layout
# ---------------------------------------------------------------------------

#' Ungrouped table: one row per measure, columns Metric / Value / (Secondary).
#' @noRd
tk_table_flat <- function(cells, flat, fspecs = list()) {
  has_sec_col <- !identical(flat$style, "plain") &&
    any(vapply(cells$secondary, function(s) !all(is.na(s)), logical(1)))

  head_cells <- list(htmltools::tags$th("Metric"),
                     htmltools::tags$th(class = "r", "Value"))
  if (has_sec_col) {
    head_cells[[3]] <- htmltools::tags$th(class = "r", tk_sec_header(flat$style))
  }
  thead <- htmltools::tags$thead(htmltools::tags$tr(head_cells))

  rows <- lapply(seq_len(nrow(cells)), function(i) {
    cell <- cells[i, ]
    fspec <- fspecs[[cell$measure]] %||%
      tk_resolve_format(flat$format, cell$value)
    unit_sfx <- if (nzchar(flat$unit %||% "") && !identical(fspec$kind, "percent")) {
      htmltools::tags$span(class = "unit", flat$unit)
    }
    hex <- cell$.hex %||% NA_character_
    pill <- tk_ident_pill_style(hex)
    tds <- list(
      # Same NA/empty guard as the card eyebrow (number-only rows keep a
      # blank label cell rather than literal "NA"). With an identity hex the
      # label wears the "Color by" pill (G + Reach 2).
      htmltools::tags$td(class = "lbl",
        if (!is.na(cell$overline) && nzchar(cell$overline)) {
          if (is.null(pill)) cell$overline
          else htmltools::tags$span(class = "tk-stub-pill", style = pill,
                                    cell$overline)
        }),
      htmltools::tags$td(class = tk_val_td_class(cell$value),
                         tk_format(cell$value, fspec), unit_sfx)
    )
    if (has_sec_col) {
      node <- tk_secondary_node(flat$style, cell$secondary[[1]], flat$good_when,
                                fspec, context = "cell", hex = hex)
      tds[[3]] <- htmltools::tags$td(class = "r", node)
    }
    htmltools::tags$tr(class = "tk-data-row",
                       # Drill level (group value / Name value) -- see tk_card.
                       `data-group` = {
                         dg <- cell$.dg %||% cell$group
                         if (!is.null(dg) && nzchar(dg)) dg else NULL
                       },
                       tds)
  })
  tk_table_wrap(thead, htmltools::tags$tbody(rows))
}

#' Grouped matrix: rows = group levels, columns = measures (value + secondary).
#' @noRd
tk_table_matrix <- function(cells, flat, groups, meas) {
  # header: group stub + one column per measure (with unit)
  ths <- list(htmltools::tags$th("Group"))
  fspecs <- list()
  for (m in meas) {
    vals <- cells$value[cells$measure == m]
    fspec <- tk_resolve_format(flat$format, vals)
    fspecs[[m]] <- fspec
    unit <- tk_header_unit(flat$unit, fspec)
    ths[[length(ths) + 1L]] <- htmltools::tags$th(
      class = "r", tk_pretty(m),
      if (nzchar(unit)) htmltools::tags$span(class = "th-unit", unit)
    )
  }
  thead <- htmltools::tags$thead(htmltools::tags$tr(ths))

  rows <- lapply(groups, function(g) {
    # "Color by" identity pill on the row stub (G + Reach 2); any cell of
    # this group carries the group's hex.
    hex <- cells$.hex[match(g, cells$group)] %||% NA_character_
    pill <- tk_ident_pill_style(hex)
    tds <- list(htmltools::tags$td(class = "lbl",
      if (is.null(pill)) g
      else htmltools::tags$span(class = "tk-stub-pill", style = pill, g)))
    for (m in meas) {
      idx <- which(cells$group == g & cells$measure == m)
      if (length(idx) == 0L) {
        tds[[length(tds) + 1L]] <- htmltools::tags$td(class = "r", "\u2014")
        next
      }
      cell <- cells[idx[1], ]
      fspec <- fspecs[[m]]
      td <- if (identical(flat$style, "fill")) {
        htmltools::tags$td(class = "r",
          tk_secondary_node("fill", cell$secondary[[1]], flat$good_when, fspec,
                            context = "cell", hex = hex))
      } else if (identical(flat$style, "delta")) {
        htmltools::tags$td(class = "r", htmltools::tags$span(
          class = "tk-cell",
          htmltools::tags$span(class = "val num", tk_format(cell$value, fspec)),
          tk_secondary_node("delta", cell$secondary[[1]], flat$good_when, fspec)
        ))
      } else {
        htmltools::tags$td(class = tk_val_td_class(cell$value),
                           tk_format(cell$value, fspec))
      }
      tds[[length(tds) + 1L]] <- td
    }
    htmltools::tags$tr(class = "tk-data-row", `data-group` = g, tds)
  })
  tk_table_wrap(thead, htmltools::tags$tbody(rows))
}

#' Value cell class, with is-neg coloring for negatives.
#' @noRd
tk_val_td_class <- function(x) {
  paste(c("r val num", if (is.finite(x) && x < 0) "is-neg"), collapse = " ")
}

#' @noRd
tk_sec_header <- function(style) {
  switch(style, delta = "\u0394", fill = "Progress", pill = "Status", "Reference")
}

#' @noRd
tk_table_wrap <- function(thead, tbody) {
  htmltools::tags$div(
    class = "tk-tablewrap",
    htmltools::tags$div(
      class = "tk-scroll",
      htmltools::tags$table(class = "tk-table", thead, tbody)
    )
  )
}

# ---------------------------------------------------------------------------
# Top-level
# ---------------------------------------------------------------------------

#' Build the tile renderer HTML.
#'
#' @param data Input data frame (upstream, pre-filter).
#' @param value,group,measure,secondary,overline,caption Role mappings.
#' @param summaries Optional in-block aggregation list (`list(func, cols)`),
#'   shared with the table. When set, the raw input is reduced FOR DISPLAY --
#'   grand totals (no `group`) or one row per `group` level -- and each metric
#'   becomes a card / matrix column.
#' @param layout "cards" or "table".
#' @param style,good_when,format,unit Flat render spec defaults.
#' @param drill Logical; when TRUE cards / rows are clickable filters.
#' @param elem_id ns()-based id used to build the `_action` input name.
#' @param active_col,active_values The block's click-filter state at render
#'   time (the filter reactiveVals): drive the `.tk-active` card highlight
#'   (via the `data-tk-active` attribute the tile JS reads) and the
#'   active-filter status footer, so a restored board shows which card
#'   filters downstream.
#' @return An [htmltools::tagList()].
#' @noRd
tile_html <- function(data, value = character(), group = character(),
                      measure = "", layout = "cards", overline = "",
                      caption = "", secondary = "", style = "plain",
                      good_when = "up", format = "number", unit = "",
                      summaries = list(), drill = FALSE, elem_id = NULL,
                      color = "", scale_map = NULL,
                      active_col = NULL, active_values = NULL) {
  flat <- list(style = style %||% "plain", good_when = good_when %||% "up",
               format = format %||% "number", unit = unit %||% "")

  # In-block aggregation (the shared table aggregator): when `summaries` (and/or a
  # `group`) is set, reduce the raw input FOR DISPLAY -- one column per metric,
  # one row per group level (or a single grand-total row when `group` is
  # empty). The result feeds the wide-input renderer (each metric -> a card /
  # matrix column). No metric and no group -> the raw frame renders as before.
  group <- intersect(as.character(group), names(data))
  # Aggregate ONLY when summaries are set. The table treats "group, no metric"
  # as a grouped count (its gear seeds a count on checking Aggregation), but
  # the tile's `group` doubles as the CLUSTERING column for precomputed
  # input (cards per region, matrix rows) -- group-without-summaries must keep
  # rendering the precomputed values, not silently turn into a count.
  agg <- if (length(summaries)) {
    dd_table_aggregate(data, group, summaries)
  } else {
    list(aggregated = FALSE)
  }
  if (isTRUE(agg$aggregated)) {
    disp_data  <- agg$data
    disp_value <- agg$metric_cols
    disp_by    <- if (length(agg$group)) agg$group[1L] else ""
    disp_meas  <- ""
  } else {
    disp_data  <- data
    disp_value <- value
    disp_by    <- if (length(group)) group[1L] else ""
    disp_meas  <- measure
  }

  cells <- tile_long_frame(disp_data, value = disp_value, by = disp_by,
                           measure = disp_meas, secondary = secondary,
                           overline = overline, caption = caption)

  grouped <- nrow(cells) > 0L && any(nzchar(cells$group)) &&
    tk_is_col(disp_by, disp_data)

  # Drill target -- structurally determined, never user-picked. Grouped tile:
  # a click identifies a group level -> filter on the group column. Ungrouped
  # LONG tile (a KPI list): a click identifies a Name (measure) level ->
  # filter on the measure column. Anything else (bare single KPI, wide
  # columns-as-measures, aggregated grand totals whose "measures" are metric
  # names, not data values) has no meaningful click target -> no drill.
  drill_col <- if (grouped) {
    disp_by
  } else if (tk_is_col(disp_meas, disp_data)) {
    disp_meas
  } else {
    ""
  }
  # Per-cell drill level riding on the cells frame (`data-group` on each
  # card / row): the group value when grouped, the measure value otherwise.
  if (nrow(cells) > 0L) {
    cells$.dg <- if (!nzchar(drill_col)) {
      ""
    } else if (grouped) {
      cells$group
    } else {
      cells$measure
    }
  }

  # "Color by" card tint -- the chart's identity color applied to cards,
  # resolved through the board scale map (dd_row_hex), so a SEX-tinted tile
  # matches the SEX-colored chart. STRUCTURAL columns only (the group, or
  # the Name column) -- like the drill, the tint keys on what a card IS.
  # NA hex (no map / no binding / other column) = no tint.
  if (nrow(cells) > 0L) {
    cells$.hex <- NA_character_
    color <- as.character(color %||% "")[1L]
    hex_on <- ""
    key_vals <- if (!nzchar(color)) {
      NULL
    } else if (identical(color, disp_by) && grouped) {
      hex_on <- "group"
      cells$group
    } else if (identical(color, disp_meas) && tk_is_col(disp_meas, disp_data)) {
      hex_on <- "name"
      cells$measure
    } else {
      NULL
    }
    # Where the identity's NAME lives decides where the pill sits: on each
    # card's overline when the tint keys on the Name column, on the cluster
    # heading / row stub when it keys on the group.
    cells$.hex_on <- hex_on
    if (!is.null(key_vals)) {
      lk <- unique(key_vals)
      # dd_ident_hex: scale map first, deterministic palette fallback when
      # unbound (chart parity) -- the tile's color is always an explicit pick.
      lk_hex <- dd_ident_hex(
        scale_map, color,
        stats::setNames(
          data.frame(lk, stringsAsFactors = FALSE, check.names = FALSE),
          color
        )
      )
      if (!is.null(lk_hex)) cells$.hex <- lk_hex[match(key_vals, lk)]
    }
  }

  fspecs <- if (nrow(cells) > 0L) tile_fspecs(cells, flat) else list()

  body <- if (nrow(cells) == 0L) {
    tk_empty_card()
  } else if (identical(layout, "table")) {
    if (grouped) {
      tk_table_matrix(cells, flat,
                      groups = unique(cells$group),
                      meas = unique(cells$measure))
    } else {
      tk_table_flat(cells, flat, fspecs)
    }
  } else {
    tk_cards_layout(cells, flat, grouped, fspecs)
  }

  drill_on <- isTRUE(drill) && nzchar(drill_col) && !is.null(elem_id)

  # Active click-filter state (chart-footer parity). The highlight keys on
  # the drill values (`data-group`), so it only applies while the stored
  # filter column still IS the drill target; the status line always reports
  # the stored filter (even after a re-aim), so an active filter is never
  # invisible.
  act_vals <- as.character(unlist(active_values %||% character()))
  act_on   <- !is.null(active_col) && nzchar(active_col) && length(act_vals)
  status <- if (drill_on || act_on) {
    htmltools::tags$div(
      class = "dd-status-footer",
      htmltools::tags$span(
        class = "dd-status-text",
        if (act_on) {
          paste0("Filtered: ", active_col, " = ",
                 paste(act_vals, collapse = ", "))
        } else {
          "No filter active"
        }
      ),
      if (act_on) {
        htmltools::tags$button(
          type = "button", class = "dd-status-reset", "Reset"
        )
      }
    )
  }

  wrapper <- htmltools::tags$div(
    class = paste("tk-block", if (drill_on) "tk-clickable"),
    `data-tk-elem-id` = if (!is.null(elem_id)) elem_id else NULL,
    `data-tk-drill`   = if (drill_on) "1" else NULL,
    `data-tk-group`   = if (drill_on) drill_col else NULL,
    # Active drill value(s) as a JSON array; the tile JS re-applies the
    # .tk-active mark to matching cards / rows on every (re)render.
    # auto_unbox = FALSE on the character VECTOR keeps a single value a JSON
    # array (["South"], never a nested [["South"]]).
    `data-tk-active`  = if (act_on && identical(active_col, drill_col)) {
      as.character(jsonlite::toJSON(act_vals, auto_unbox = FALSE))
    } else {
      NULL
    },
    # The resolvable drill target, emitted even while drill is OFF: the gear
    # needs it to show (and word) the picker-less Drill-down section before
    # the capability is enabled. Empty/absent = no target -> section hidden.
    `data-tk-drill-col` = if (nzchar(drill_col)) drill_col else NULL,
    `data-tk-layout`  = layout,
    # The gear's column pickers offer the RAW columns (what you group /
    # aggregate over), not the aggregated frame.
    `data-tk-cols`    = tile_cols_json(data),
    `data-tk-config`  = tile_config_json(value, group, measure, secondary, style,
                                         good_when, format, unit, overline,
                                         caption, layout, drill, color = color),
    `data-tk-summaries` = dd_summaries_json(summaries),
    body,
    status
  )

  htmltools::tagList(tile_block_dep(), wrapper)
}

#' Column metadata JSON for the config engine's `columns()`:
#' `[{name, type, n_unique}]`, type in numeric / categorical.
#' @noRd
tile_cols_json <- function(data) {
  cols <- lapply(names(data), function(nm) {
    x <- data[[nm]]
    list(
      name = nm,
      type = if (is.numeric(x)) "numeric" else "categorical",
      n_unique = length(unique(x))
    )
  })
  as.character(jsonlite::toJSON(cols, auto_unbox = TRUE))
}

#' Serialize the current role config as JSON for the gear popover engine.
#' The popover's column pickers are single-select, so `value` is emitted as a
#' scalar (the first measure column); multi-column wide `value` is author / AI
#' set, not popover-editable in v1.
#' @noRd
tile_config_json <- function(value, group, name, secondary, style, good_when,
                             format, unit, overline, caption, layout, drill,
                             color = "") {
  value <- as.character(value)
  group <- as.character(group)
  cfg <- list(
    value     = if (length(value)) value[1] else "",
    group     = if (length(group)) group[1] else "",
    name      = name %||% "",
    color     = color %||% "",
    secondary = secondary %||% "",
    style     = style %||% "plain",
    good_when = good_when %||% "up",
    format    = format %||% "number",
    unit      = unit %||% "",
    overline  = overline %||% "",
    caption   = caption %||% "",
    layout    = layout %||% "cards",
    drill     = isTRUE(drill)
  )
  as.character(jsonlite::toJSON(cfg, auto_unbox = TRUE, null = "null"))
}
