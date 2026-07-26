# Colour by ROLE, resolved through the board theme.
#
# Before this, the three colour surfaces in this package were reachable in
# three different ways: series colours were a package constant (not reachable
# at all), header bands were a `blockr.viz.*` option and heatmap ramps were a
# function argument. A client theme could therefore restyle one of the three.
# Consumers now ask blockr.theme for the role they mean and one theme entry
# recolours every renderer that shares it.
#
# blockr.theme stays in Suggests: with the package absent, an older version
# without theme_palette(), or no theme applied, every call returns the
# hard-coded fallback and the board renders exactly as it did before.

# Mirrors the JS BLOCKR_PALETTE (inst/js/chart.js) -- the pool the chart
# cycles when no scale applies, so hash assignment draws from the same
# colours. Now only the FALLBACK: an applied theme's `categorical` role wins,
# and the resolved vector is sent to JS with the chart config so both sides
# stay in step without hand-syncing.
DD_PALETTE_FALLBACK <- c(
  "#0072B2", "#D55E00", "#F0E442", "#009E73", "#56B4E9", "#E69F00", "#CC79A7"
)

# Interpolation endpoints for the numeric cell heatmap: two points low-to-high,
# three for diverging (pole, neutral midpoint, pole). dt_color_fun() lerps
# between them, so these are endpoints and not discrete steps -- the ramp's
# step-separation ceiling does not apply.
DT_SEQUENTIAL_FALLBACK <- c("#eef2ff", "#1d4ed8")
DT_DIVERGING_FALLBACK <- c("#99000d", "#ffffff", "#08306b")

# Gated on a theme having been APPLIED, not merely on blockr.theme being
# installed. theme_palette() answers with the blockr defaults when no theme is
# current, which is right for its own callers but would mean that installing
# blockr.theme silently recoloured every existing board. Appearance changes
# only when an app calls use_theme().
has_theme_palette <- function() {
  requireNamespace("blockr.theme", quietly = TRUE) &&
    "theme_palette" %in% getNamespaceExports("blockr.theme") &&
    !is.null(blockr.theme::current_theme())
}

#' Resolve a colour role through the board theme
#'
#' @param role One of `categorical`, `identity`, `sequential`, `diverging`,
#'   `bands`.
#' @param n Number of colours; `NULL` takes the palette's own length.
#' @param fallback Colours to use when no theme answers the role.
#' @return A character vector of hex colours, or `NULL` when the role is
#'   unanswered and `fallback` is `NULL`.
#' @noRd
viz_palette <- function(role, n = NULL, fallback = NULL) {
  if (has_theme_palette()) {
    out <- tryCatch(
      blockr.theme::theme_palette(role, n),
      error = function(e) NULL
    )
    if (length(out)) {
      return(out)
    }
  }
  if (is.null(fallback) || is.null(n)) {
    return(fallback)
  }
  rep_len(fallback, n)
}

# The series pool. A function rather than a constant so a theme applied after
# this package loads still wins -- app.R calls use_theme() at startup, which is
# after the namespace is sealed.
dd_palette <- function(n = NULL) {
  viz_palette("categorical", n, DD_PALETTE_FALLBACK)
}
