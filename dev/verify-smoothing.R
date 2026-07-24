# Verify monotone line smoothing across the two timeline surfaces.
#
#   Rscript blockr.viz/dev/verify-smoothing.R [port]   (default 3838)
#
# Chart block (`smooth` option, default "auto"):
#   few      — 10 subjects  -> monotone curves WITH point markers.
#   crowd    — ~250 subjects -> straight thin lines (dots and smoothing leave
#              the chart together past 50 series), opacity tier 0.35.
#   straight — smooth = "off" -> straight even at 10 subjects.
#   step     — step = "end"  -> step wins over smoothing (KM-style override).
# Patient profile ("Value lines" gear toggle, default Smooth):
#   profile  — lab lines monotone (no overshoot through the ref band) in the
#              shared palette's slot-1 blue; gear > Value lines flips straight.
# load_all EVERYTHING before touching any blockr namespace: an option set via
# `blockr.ui::` above these lines would resolve against the INSTALLED package,
# which may be older than source (e.g. missing html_table_display).
pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.viz")
pkgload::load_all("blockr.pharma")

options(blockr.tabular_display = blockr.ui::html_table_display)
options(blockr.dock_is_locked = FALSE)
options(
  shiny.port = {
    args <- commandArgs(trailingOnly = TRUE)
    p <- if (length(args)) args[[1]] else Sys.getenv("BLOCKR_PORT", "3838")
    as.integer(p)
  },
  shiny.host = "0.0.0.0"
)

# Clickable URL (shiny's own "Listening on http://0.0.0.0:..." is not).
cat(sprintf("\nOpen: http://127.0.0.1:%d/\n\n", getOption("shiny.port")))

# ALT trajectories from public CDISC ADaM data — sparse clinical visits, the
# case monotone-vs-spline is about. (pharmaverseadam ships one `adlb`; the dm
# below aliases it to `adlbc` for the profile's chemistry panel.)
adlb <- as.data.frame(pharmaverseadam::adlb)
alt <- adlb[adlb$PARAMCD == "ALT" & !is.na(adlb$ADY) & !is.na(adlb$AVAL), ]
subj <- unique(alt$USUBJID)
alt_few <- alt[alt$USUBJID %in% subj[1:10], c("USUBJID", "ADY", "AVAL", "TRT01A")]
alt_crowd <- alt[, c("USUBJID", "ADY", "AVAL", "TRT01A")]

# Single-subject dm for the patient profile (labs only keeps it light).
adsl <- pharmaverseadam::adsl
one <- subj[1]
chem <- adlb[adlb$PARAMCD %in% c("ALT", "AST", "BILI", "CREAT"), ]
pp_dm <- dm::dm(
  adsl = adsl[adsl$USUBJID == one, ],
  adlbc = chem[chem$USUBJID == one, ]
)

serve(
  new_dock_board(
    blocks = c(
      few_data = new_static_block(alt_few, block_name = "ALT, 10 subjects"),
      crowd_data = new_static_block(alt_crowd, block_name = "ALT, all subjects"),
      pp_data = new_static_block(pp_dm, block_name = "One-subject dm"),
      few = new_chart_block(
        chart_type = "line", x = "ADY", y = "AVAL",
        series = "USUBJID", color = "TRT01A",
        block_name = "Monotone (auto, 10 series)"),
      crowd = new_chart_block(
        chart_type = "line", x = "ADY", y = "AVAL",
        series = "USUBJID", color = "TRT01A",
        block_name = "Crowd (auto falls back straight)"),
      straight = new_chart_block(
        chart_type = "line", x = "ADY", y = "AVAL",
        series = "USUBJID", color = "TRT01A", smooth = "off",
        block_name = "Straight (smooth = \"off\")"),
      stepped = new_chart_block(
        chart_type = "line", x = "ADY", y = "AVAL",
        series = "USUBJID", color = "TRT01A", step = "end",
        block_name = "Step end (wins over smoothing)"),
      profile = new_patient_profile_block(block_name = "Patient profile")
    ),
    links = list(
      list(from = "few_data", to = "few", input = "data"),
      list(from = "crowd_data", to = "crowd", input = "data"),
      list(from = "few_data", to = "straight", input = "data"),
      list(from = "few_data", to = "stepped", input = "data"),
      list(from = "pp_data", to = "profile", input = "data")
    ),
    extensions = new_dag_extension(),
    grids = list(
      Chart = dock_grid("few", "straight"),
      Crowd = dock_grid("crowd", "stepped"),
      Profile = dock_grid("profile")
    ),
    active = "Chart"
  )
)
