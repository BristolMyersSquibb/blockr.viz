# Distribution band (chart_type = "band") against real ADaM lab data.
#
# The band is a box plot dragged along a continuous x: at each grid point the
# same statistics a box plot computes, over a window of nearby observations.
# ALT from pharmaverseadam's ADLB, filtered to one PARAMCD upstream (the block
# assumes one analyte, like every other chart).
#
# Port pinned via BLOCKR_PORT (3838 is the container's forwarded port).
options(shiny.port = as.integer(Sys.getenv("BLOCKR_PORT", "3838")),
        shiny.host = "0.0.0.0")

pkgload::load_all("/workspace/blockr.core")
pkgload::load_all("/workspace/blockr.ui")
pkgload::load_all("/workspace/blockr.dplyr")
pkgload::load_all("/workspace/blockr.dock")
pkgload::load_all("/workspace/blockr.viz")

adlb <- pharmaverseadam::adlb
alt <- adlb[adlb$PARAMCD == "ALT" &
              !is.na(adlb$AVAL) & !is.na(adlb$ADY), , drop = FALSE]
alt <- alt[, c("USUBJID", "TRTA", "ADY", "AVAL", "PARAM",
               "ANRLO", "ANRHI"), drop = FALSE]

board <- new_dock_board(
  blocks = c(
    data = new_static_block(alt, block_name = "ADLB - ALT"),

    # 1. Faceted: one panel per arm, so every panel holds a single series and
    #    keeps its full band. The default view.
    band_facet = new_chart_block(
      chart_type = "band", x = "ADY", y = "AVAL",
      facet = "TRTA", band_id = "USUBJID",
      ref_hi = "ANRHI", ref_lo = "ANRLO",
      box_points = "outliers",
      title = "ALT over study day - by arm",
      block_name = "Band - faceted by arm"),

    # 2. Overlaid: colour maps three arms, so the ribbons hide and only the
    #    medians draw. Hover a line to bring its band back.
    band_overlay = new_chart_block(
      chart_type = "band", x = "ADY", y = "AVAL",
      color = "TRTA", band_id = "USUBJID",
      ref_hi = "ANRHI",
      title = "ALT over study day - arms overlaid",
      block_name = "Band - overlaid, band on hover"),

    # 3. Single series, fixed window: no colour mapping at all, so the band is
    #    simply on. Fixed window shows the failure mode the adaptive one
    #    avoids -- it tears wherever the visit schedule is sparse.
    band_single = new_chart_block(
      chart_type = "band", x = "ADY", y = "AVAL",
      band_id = "USUBJID", band_window = "fixed", band_size = 10,
      ref_hi = "ANRHI",
      title = "ALT - all patients, fixed +/-10 day window",
      block_name = "Band - single series, fixed window")
  ),
  links = c(
    new_link(from = "data", to = "band_facet", input = "data"),
    new_link(from = "data", to = "band_overlay", input = "data"),
    new_link(from = "data", to = "band_single", input = "data")
  )
)

cat("\n  http://127.0.0.1:",
    as.integer(Sys.getenv("BLOCKR_PORT", "3838")), "/\n\n", sep = "")

serve(board)
