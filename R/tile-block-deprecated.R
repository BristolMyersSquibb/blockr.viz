#' Deprecated: KPI block
#'
#' `new_kpi_block()` is superseded by [new_tile_block()]. The new
#' block separates aggregation (per-aesthetic stat) from rendering
#' and ships three visual modes (number / spark / progress).
#'
#' Calls to `new_kpi_block()` still work: their arguments are
#' converted to an equivalent `new_tile_block(showcase = "number")`
#' state. `titles`, `subtitles`, and `colors` are dropped — their
#' role is now data-driven via the `label` / `status` aesthetics.
#' Bake any per-measure label / color overrides into a `mutate()`
#' upstream and map them to `label` / `status`.
#'
#' @inheritParams new_tile_block
#' @param measures Character vector of columns to display as KPIs.
#' @param agg_fun Aggregation function: one of `"sum"`, `"mean"`,
#'   `"median"`, `"min"`, `"max"`, `"n"` (count). Mapped to the new
#'   block's `stats$value`.
#' @param prefix,suffix Glyphs for currency / percent. Converted to
#'   a format kind when they match known symbols (`"$"`, `"%"`,
#'   `"€"`, `"£"`).
#' @param digits Integer, decimals to display.
#' @param titles,subtitles,colors Silently dropped — see details.
#' @param ... Forwarded to [new_tile_block()].
#'
#' @return A tile block with `showcase = "number"`.
#' @export
#' @keywords internal
new_kpi_block <- function(
  measures = character(),
  agg_fun = "sum",
  prefix = "",
  suffix = "",
  digits = 0,
  titles = NULL,
  subtitles = NULL,
  colors = NULL,
  ...
) {
  if (requireNamespace("lifecycle", quietly = TRUE)) {
    lifecycle::deprecate_warn(
      "0.1.0",
      "new_kpi_block()",
      "new_tile_block()",
      details = paste(
        "Use `new_tile_block(showcase = \"number\")` plus an upstream",
        "`summarize_block` / `summary_table_block` for aggregation.",
        "`titles`, `subtitles`, and `colors` are dropped \u2014 set",
        "labels / colors via a `mutate()` upstream and the `label` /",
        "`status` aesthetics."
      )
    )
  }

  # Map old agg_fun names to tile stats. "n" → "count".
  stat <- switch(agg_fun,
    n = "count",
    agg_fun  # pass through sum/mean/median/min/max as-is
  )
  fmt_kind <- infer_from_glyphs(prefix, suffix)

  new_tile_block(
    showcase = "number",
    state = list(
      aesthetics = list(value = as.character(measures)),
      stats      = list(value = stat),
      formats    = list(value = list(
        kind   = fmt_kind,
        digits = suppressWarnings(as.integer(digits))
      ))
    ),
    ...
  )
}
