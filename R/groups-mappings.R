# The summaries' colour and facet mappings, for groups_prepare()
# (R/groups-prepare.R).

#' Per-summary colour and facet, each optional. Levels come from the FULL data,
#' never a facet slice, so fills and formats agree across copies and rows.
#' @return the summaries or `list(err =)`.
#' @noRd
groups_prep_mappings <- function(summaries, data, drawn) {
  for (i in seq_along(summaries)) {
    s <- summaries[[i]]
    # A custom row runs its function once over the whole data to learn its
    # shape. A failing function leaves its column empty, with the error.
    if (identical(s$type, "custom")) {
      cs <- groups_custom_setup(s, data)
      if (!is.null(cs$err)) {
        summaries[[i]] <- groups_failed_summary(s, cs$err)
        next
      }
      s <- cs$s
    }
    r <- groups_map_color(s, data, drawn)
    if (!is.null(r$err)) return(r)
    r <- groups_map_facet(r$s, data, drawn)
    if (!is.null(r$err)) return(r)
    summaries[[i]] <- groups_map_levels(r$s, data)
  }
  summaries
}

# Colour mapping: levels over the drawn rows. Returns `list(s =)` or
# `list(err =)`.
groups_map_color <- function(s, data, drawn) {
  cc <- groups_present(s$color, data)
  if (!is.null(cc) && groups_color_capable(s)) {
    lv <- groups_color_levels(data[[cc]], drawn)
    if (length(lv) > GROUPS_MAX_LEVELS) {
      return(list(err = paste0(
        "Summary \"", s$name, "\": colour column \"", cc, "\" has ",
        length(lv), " levels. Pick one with at most ", GROUPS_MAX_LEVELS,
        " -- a palette has about seven readable colours."
      )))
    }
    s$color <- cc
    # A lane mark splits its cell per level; the swimlane colours segments
    # and reads `color` in the plan.
    s$.color <- if (groups_takes_color(s)) cc
    s$.levels <- lv
  }
  list(s = s)
}

# Facet mapping: levels, capped, in row-count order for a tile. Returns
# `list(s =)` or `list(err =)`.
groups_map_facet <- function(s, data, drawn) {
  fc <- groups_present(s$facet, data)
  if (!is.null(fc)) {
    # One level is a filter (one arm left), not a mistake: draw once, labelled.
    lv <- groups_levels(data[[fc]])
    tile <- identical(s$show, "tile")
    cap <- if (tile) GROUPS_MAX_TILE_LEVELS else GROUPS_MAX_LEVELS
    if (length(lv) > cap) {
      return(list(err = paste0(
        "Summary \"", s$name, "\": facet column \"", fc, "\" has ",
        length(lv), " levels, so the column would repeat ", length(lv),
        " times. Pick one with at most ", cap,
        if (tile) ", or keep the top ones in the prepare script", "."
      )))
    }
    # An ordered factor, number or date has an order of its own and keeps it:
    # a visit or sequence number reads 1, 2, 3 whatever its row counts.
    ordered <- is.ordered(data[[fc]]) || is.numeric(data[[fc]]) ||
      inherits(data[[fc]], c("Date", "POSIXct"))
    if (tile && !ordered && identical(s$facet_order, "count")) {
      # Most rows first; ties keep the level order.
      n <- tabulate(match(as.character(data[[fc]][drawn]), lv),
                    nbins = length(lv))
      lv <- lv[order(-n, seq_along(lv))]
    }
    s$.facet <- fc
    s$.flevels <- lv
  }
  list(s = s)
}

# Levels read off the full data besides colour and facet: tile paint, pair
# dash, date axis.
groups_map_levels <- function(s, data) {
  # One cell's worst level is read against the whole column's scale.
  if (identical(s$show, "tile") && !is.null(groups_present(s$fill, data))) {
    s$.fill <- s$fill
    s$.fill_levels <- groups_ord_levels(data[[s$fill]])
  }
  # Dash levels come from the full data, so a slice missing a level does not
  # renumber which rows draw dashed.
  if (identical(s$type, "pair") && !is.null(groups_present(s$linetype, data))) {
    s$.dlevels <- groups_levels(data[[s$linetype]])
  }
  if (s$type %in% c("sparkline", "spans")) {
    s$.date <- inherits(data[[chr_state(s$x)]], "Date")
  }
  if (identical(s$type, "spans")) s <- groups_spans_levels(s, data)
  s
}

# With ONE facet column the level alone labels a copy and by_level can span
# groups. With SEVERAL, each copy names its column ("SEX: F"), by_level has
# nothing to span and the layout stays by_summary; the header keeps the summary
# name beside its N so two "Placebo / N = 86" columns can be told apart.
groups_facet_info <- function(summaries, data) {
  facet_cols <- unique(unlist(lapply(summaries, function(s) s$.facet)))
  shared_facet <- if (length(facet_cols) == 1L) facet_cols else NULL
  list(
    cols = facet_cols, shared = shared_facet,
    qualify = length(facet_cols) > 1L,
    n_faceted = sum(vapply(summaries, function(s) !is.null(s$.facet),
                           logical(1L))),
    levels = if (is.null(shared_facet)) {
      character()
    } else {
      groups_levels(data[[shared_facet]])
    }
  )
}
