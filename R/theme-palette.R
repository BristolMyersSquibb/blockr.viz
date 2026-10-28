# Colour by role, resolved through the board theme. Series colours, header
# bands and heatmap ramps ask blockr.theme for the role they mean, so one theme
# entry recolours every renderer that shares it. With blockr.theme absent, too
# old to have theme_palette(), or no theme applied, every call returns the
# hard-coded fallback.

# Fallback series colours, mirroring BLOCKR_PALETTE in inst/js/chart/colors.js.
# An applied theme's `categorical` role wins, and the resolved vector is sent
# to JS with the chart config. The six colours are blockr.theme's "Blockr"
# palette (Okabe-Ito without black and yellow, since yellow is 1.3:1 on
# white).
DD_PALETTE_FALLBACK <- c(
  "#0072B2", "#E69F00", "#009E73", "#CC79A7", "#56B4E9", "#D55E00"
)

# Mirrors PALETTE_OVERFLOW and paletteAt() in inst/js/chart/colors.js, so
# static exports (PPTX, report code) agree with the screen.
DD_PALETTE_OVERFLOW <- "#9AA0A6"

# Colours for `n` levels from `pal` (the resolved theme palette by default):
# the palette while it lasts, then grey. Correct for any `n`, including 0.
#' @noRd
dd_level_palette <- function(n, pal = dd_palette()) {
  if (!length(pal)) {
    pal <- DD_PALETTE_FALLBACK
  }
  c(pal, rep(DD_PALETTE_OVERFLOW, n))[seq_len(n)]
}

# Interpolation endpoints for the numeric cell heatmap: two points low to high,
# three for diverging (pole, neutral midpoint, pole). dt_color_fun() lerps
# between them.
DT_SEQUENTIAL_FALLBACK <- c("#eef2ff", "#1d4ed8")
DT_DIVERGING_FALLBACK <- c("#99000d", "#ffffff", "#08306b")

# The presence guard for every blockr.theme entry point used (current_theme(),
# theme_palette(), resolve_scales_col()). The minimum version is compared by
# hand because requireNamespace(versionCheck =) enforces the bound only while
# loading the namespace. Keep it in step with DESCRIPTION's Suggests.
has_blockr_theme <- function() {
  requireNamespace("blockr.theme", quietly = TRUE) &&
    utils::packageVersion("blockr.theme") >= "0.0.0.9002"
}

#' Resolve a colour role through the board theme
#'
#' @param role One of `categorical`, `identity`, `sequential`, `diverging`,
#'   `bands`.
#' @param n Number of colours; `NULL` takes the palette's own length.
#' @param fallback Colours to use when no theme answers the role.
#' @return A character vector of hex colours, or `NULL` when the role is
#'   unanswered and `fallback` is `NULL`.
#'
#' For exporters in other blockr packages; not part of the user API.
#' @keywords internal
#' @export
viz_palette <- function(role, n = NULL, fallback = NULL) {
  # theme_palette() answers with the blockr defaults when no theme is current,
  # so a theme must be applied, not only installed. It answers NULL (with a
  # warning) for anything a theme cannot supply, and errors on an unknown role.
  if (has_blockr_theme() && !is.null(blockr.theme::current_theme())) {
    out <- blockr.theme::theme_palette(role, n)
    if (length(out)) {
      return(out)
    }
  }
  if (is.null(fallback) || is.null(n)) {
    return(fallback)
  }
  rep_len(fallback, n)
}

# The series pool. A function so that a theme applied after this package loads
# still wins.
dd_palette <- function(n = NULL) {
  viz_palette("categorical", n, DD_PALETTE_FALLBACK)
}
