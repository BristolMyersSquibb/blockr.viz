# Table model, groups layout: tile column ---------------------------------------
#
# The tile is a matrix cell: the value in a square, painted by the worst level
# of a `fill` column or by the value on a ramp. Levels, colours, the worst
# level per row and the paint per row.

# Levels of a tile's paint column in their order of severity: a factor keeps
# its own level order (vocabularies live in the data), numeric grades order
# numerically, a bare character column falls back to sorted values.
lane_ord_levels <- function(x) {
  if (is.factor(x)) return(levels(x))
  if (is.numeric(x)) return(as.character(sort(unique(x[is.finite(x)]))))
  x <- as.character(x)
  sort(unique(x[!is.na(x) & nzchar(x)]))
}

# Colours for a tile's paint levels: the board's scale map when it covers
# every level, else a light-to-dark ramp in level order (worst darkest).
lane_tile_colors <- function(map, col, levels, column = NULL) {
  levels <- as.character(levels)
  if (!length(levels)) return(character())
  if (!is.null(map) && has_blockr_theme()) {
    res <- dd_resolve_scales(map, col, column %||% levels)
    pal <- res$color
    if (!is.null(pal) && all(levels %in% names(pal))) {
      return(stats::setNames(unname(pal[levels]), levels))
    }
  }
  stats::setNames(lane_tile_ramp(length(levels)), levels)
}

lane_tile_ramp <- function(n) {
  if (n <= 0L) return(character())
  if (n == 1L) return("#7fa2ee")
  grDevices::colorRampPalette(c("#cdd9f7", "#7fa2ee", "#2f5bd3"))(n)
}

# The worst `fill` level per target row: the largest level index among the
# slice's rows of that group, NA where the group has none.
lane_tile_worst <- function(target, tkeys, slice, fill, levels) {
  if (!nrow(slice)) return(rep(NA_integer_, nrow(target)))
  idx <- match(as.character(slice[[fill]]), levels)
  key_of <- function(d) {
    do.call(paste, c(lapply(tkeys, function(k) {
      as.character(d[[k]])
    }), sep = "\r"))
  }
  sk <- key_of(slice)
  ok <- !is.na(idx)
  w <- if (any(ok)) tapply(idx[ok], sk[ok], max) else integer()
  out <- unname(w[key_of(target)])
  as.integer(out)
}

# A tile column's paint per row: `bg` / `fg` hex (NA = empty cell) and `has`
# (the cell shows a value). By the worst `fill` level when the column has
# one, else by the value on a ramp from zero to the column's maximum. Shared
# by the screen (summarize_cells()) and the spreadsheet
# (summarize_export_df()).
lane_tile_paint <- function(p, rows) {
  v <- rows[[p$key]]
  n <- length(v)
  has <- is.finite(v) & !(isTRUE(p$zero_empty) & v %in% 0)
  # A group row (an arm over its subjects) is a header in a matrix: its
  # totals would dwarf every cell under it, so it shows no tiles.
  lvl <- rows$.level
  if (!is.null(lvl) && any(lvl > 0L)) has <- has & lvl > 0L
  bg <- rep(NA_character_, n)
  if (!is.null(p$wkey)) {
    w <- rows[[p$wkey]]
    fills <- as.character(p$fills)
    ok <- has & !is.na(w)
    bg[ok] <- fills[w[ok]]
    # A cell with rows but none carrying a level paints at the floor.
    bg[has & is.na(w)] <- fills[[1L]]
  } else {
    mxv <- p$dmax %||% 0
    ramp <- lane_tile_ramp(101L)
    k <- if (is.finite(mxv) && mxv > 0) {
      round(pmin(1, pmax(0, v / mxv)) * 100) + 1
    } else {
      rep(1, n)
    }
    bg[has] <- ramp[k[has]]
  }
  fg <- rep(NA_character_, n)
  if (any(has)) {
    fg[has] <- vapply(bg[has], dt_fg_for_hex, character(1L),
                      USE.NAMES = FALSE)
  }
  list(bg = bg, fg = fg, has = has)
}
