# Distribution band (chart_type = "band") against real ADaM lab data.
#
# The band is a box plot dragged along a continuous x: at each grid point the
# same statistics a box plot computes, over a window of nearby observations.
# ALT from pharmaverseadam's ADLB, filtered to one PARAMCD upstream (the block
# assumes one analyte, like every other chart).
#
# Port pinned via BLOCKR_PORT (3838 is the container's forwarded port).
# Package roots resolve from THIS script's location, so the same command runs
# in the container (/workspace) and on a host clone. Port: command-line arg,
# then BLOCKR_PORT, then the container's forwarded 3838.
.self <- grep("^--file=", commandArgs(FALSE), value = TRUE)[1]
.ws <- normalizePath(if (is.na(.self)) "." else
  file.path(dirname(sub("^--file=", "", .self)), "..", ".."))
.port <- as.integer(c(commandArgs(TRUE), Sys.getenv("BLOCKR_PORT"), "3838")[1])

options(shiny.port = .port,
        shiny.host = "0.0.0.0")

pkgload::load_all(file.path(.ws, "blockr.core"))
pkgload::load_all(file.path(.ws, "blockr.ui"))
pkgload::load_all(file.path(.ws, "blockr.dplyr"))
pkgload::load_all(file.path(.ws, "blockr.dock"))
pkgload::load_all(file.path(.ws, "blockr.viz"))
pkgload::load_all(file.path(.ws, "blockr.theme"))

adlb <- pharmaverseadam::adlb
alt <- adlb[adlb$PARAMCD == "ALT" &
              !is.na(adlb$AVAL) & !is.na(adlb$ADY), , drop = FALSE]
alt <- alt[, c("USUBJID", "TRTA", "SEX", "ADY", "AVAL", "PARAM",
               "ANRLO", "ANRHI"), drop = FALSE]

# A limit the data never approaches, to exercise the off-scale path: the axis
# refuses to stretch this far, so the line pins to the frame edge and says so
# rather than being silently clipped out of view.
alt$FARHI <- 500

# Board scale map: the band must honour these instead of cycling the palette,
# whether the column is mapped to colour OR only to facet.
study_scale_map <- new_scale_map(
  scale_binding("SEX", color = c(F = "#0EA5E9", M = "#E69F00")),
  scale_binding("TRTA", color = c(
    "Placebo" = "#999999",
    "Xanomeline Low Dose" = "#56B4E9",
    "Xanomeline High Dose" = "#0072B2"
  ))
)

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
      block_name = "Band - single series, fixed window"),

    # 4. Facet AND colour on the SAME column: every panel holds exactly one
    #    series, so each keeps its ribbon -- "more than one series" is a
    #    per-panel fact, not a property of the mapping.
    band_same = new_chart_block(
      chart_type = "band", x = "ADY", y = "AVAL",
      facet = "SEX", color = "SEX", band_id = "USUBJID",
      ref_hi = "ANRHI",
      title = "ALT by sex - facet and colour on the same column",
      block_name = "Band - facet == colour"),

    # 5. Facet only, no colour mapping: the panels must still take their
    #    colours from the board scale map, not palette slot 1.
    band_facetonly = new_chart_block(
      chart_type = "band", x = "ADY", y = "AVAL",
      facet = "SEX", band_id = "USUBJID",
      ref_hi = "ANRHI",
      title = "ALT by sex - facet only, board colours",
      block_name = "Band - facet only, scale map")
  ),
  links = c(
    new_link(from = "data", to = "band_facet", input = "data"),
    new_link(from = "data", to = "band_overlay", input = "data"),
    new_link(from = "data", to = "band_single", input = "data"),
    new_link(from = "data", to = "band_same", input = "data"),
    new_link(from = "data", to = "band_facetonly", input = "data")
  ),
  options = new_board_options(new_scale_map_option(study_scale_map))
)

cat("\n  http://127.0.0.1:",
    .port, "/\n\n", sep = "")

serve(board)
