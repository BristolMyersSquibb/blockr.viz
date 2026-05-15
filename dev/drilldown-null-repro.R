# Repro for the drilldown-chart null-datum crash (clinical-explorer bug).
#
# Trigger conditions (all required):
#   1. aggregated bar chart
#   2. a color_by split (stacked, multi-series)
#   3. SPARSE data: some (group, color) combos have zero rows
#
# safetyData ADaM ADSL grouped by SITEID and colored by TRT01P is naturally
# sparse (most sites enrolled only a subset of arms), so the multi-series
# path emits `null` gap values. Pre-fix that crashed _updateHighlight
# (typeof null === 'object' -> null.value). Post-fix the chart renders.
#
# From /workspace:
#   Rscript blockr.bi/dev/drilldown-null-repro.R
# then open http://127.0.0.1:3838/

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.bi")

stopifnot(requireNamespace("safetyData", quietly = TRUE))
adsl <- safetyData::adam_adsl

board <- new_dock_board(
  blocks = c(
    data = new_static_block(data = adsl),
    chart = new_drilldown_chart_block(
      group_by   = "SITEID",
      color_by   = "TRT01P",
      chart_type = "bar",
      agg_fn     = "count"
    )
  ),
  links = links(from = "data", to = "chart"),
  extensions = list(blockr.dag::new_dag_extension())
)

shiny::runApp(serve(board))
