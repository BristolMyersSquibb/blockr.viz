# Verify the lane chart's marks (the rank block renamed): box, point range,
# interval swimlane, sparkline with band -- plus the bar it always had.
#
#   Rscript blockr.viz/dev/verify-lane-chart.R [port]   (default 3838)
#
# Aggregated view
#   bar   — the original surface, unchanged. Sanity + type-switch start point.
#   box   — AE duration (days) by preferred term. Check: whiskers stay inside
#           the track (the domain runs to the widest whisker, not the widest
#           median); rows sort by the median; the tooltip carries the five
#           numbers + n.
#   pr    — same rows, mean · 95% CI (t-based, computed in R). Check: small-n
#           terms show visibly wide intervals; an n = 1 term draws the center
#           dot alone (NA bounds, no zero-width interval).
# Rows view
#   swim  — AE episodes per subject as x/xend spans on study day, colored by
#           severity. Check: hover a segment = exact span; hover empty track =
#           "~day N"; search matches subject ids; Events column sorts.
#   spark — ALT per subject over study day, one inline SVG per row, band =
#           the reference range. Check: hover snaps to the nearest point;
#           rows sort by last value; the y domain is shared across rows.
# Gear (any block): the mark type picker (tile grid, Aggregated /
# Individual) switches marks without wedging group / value / drill.
#
# load_all EVERYTHING before touching any blockr namespace: an option set via
# `blockr.ui::` above these lines would resolve against the INSTALLED package,
# which may be older than source.
pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.viz")

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

cat(sprintf("\nOpen: http://127.0.0.1:%d/\n\n", getOption("shiny.port")))

# --- data --------------------------------------------------------------------

adae <- as.data.frame(pharmaverseadam::adae)
adae <- adae[!is.na(adae$ASTDY) & !is.na(adae$AENDY), ]
adae$DUR <- adae$AENDY - adae$ASTDY + 1

# The distribution rows: duration by preferred term, terms with spread.
counts <- sort(table(adae$AEDECOD), decreasing = TRUE)
dur <- adae[adae$AEDECOD %in% names(counts)[1:15],
            c("USUBJID", "AEDECOD", "AESEV", "TRT01A", "DUR")]

# The swimlane rows: subjects with the most episodes, spans on study day.
per_subj <- sort(table(adae$USUBJID), decreasing = TRUE)
swim <- adae[adae$USUBJID %in% names(per_subj)[1:25],
             c("USUBJID", "AEDECOD", "AESEV", "TRT01A", "ASTDY", "AENDY")]

# The sparkline rows: ALT over study day, reference range as the band.
adlb <- as.data.frame(pharmaverseadam::adlb)
alt <- adlb[adlb$PARAMCD == "ALT" & !is.na(adlb$ADY) & !is.na(adlb$AVAL), ]
alt_subj <- names(sort(table(alt$USUBJID), decreasing = TRUE))[1:15]
spark <- alt[alt$USUBJID %in% alt_subj,
             c("USUBJID", "ADY", "AVAL", "ANRLO", "ANRHI", "TRT01A")]

serve(
  new_dock_board(
    blocks = c(
      dur_data = new_static_block(dur, block_name = "AE durations"),
      swim_data = new_static_block(swim, block_name = "AE episodes"),
      spark_data = new_static_block(spark, block_name = "ALT over time"),
      bar = new_lane_chart_block(
        group = "AEDECOD", func = "count_distinct", id_var = "USUBJID",
        drill = "AEDECOD", block_name = "Bar (the original surface)"
      ),
      box = new_lane_chart_block(
        mark = "box", group = "AEDECOD", value = "DUR", drill = "AEDECOD",
        title = "Duration of AE (days) by preferred term",
        block_name = "Box"
      ),
      pr = new_lane_chart_block(
        mark = "pointrange", group = "AEDECOD", value = "DUR",
        summary = "mean_ci95", drill = "AEDECOD",
        title = "Mean duration of AE (days), 95% CI",
        block_name = "Point range"
      ),
      swim = new_lane_chart_block(
        mark = "interval", group = "USUBJID", x = "ASTDY", xend = "AENDY",
        color = "AESEV", fields = "TRT01A", drill = "USUBJID",
        sort_dir = "asc",
        title = "AE episodes by subject, study day",
        block_name = "Swimlane"
      ),
      spark = new_lane_chart_block(
        mark = "sparkline", group = "USUBJID", x = "ADY", value = "AVAL",
        lo = "ANRLO", hi = "ANRHI", func = "mean", fields = "TRT01A",
        drill = "USUBJID",
        title = "ALT (U/L): highest mean first, plus the trajectory",
        block_name = "Sparkline"
      )
    ),
    links = list(
      list(from = "dur_data", to = "bar", input = "data"),
      list(from = "dur_data", to = "box", input = "data"),
      list(from = "dur_data", to = "pr", input = "data"),
      list(from = "swim_data", to = "swim", input = "data"),
      list(from = "spark_data", to = "spark", input = "data")
    ),
    extensions = new_dag_extension(),
    grids = list(
      Aggregated = dock_grid("bar", "box", "pr"),
      Rows = dock_grid("swim", "spark")
    ),
    active = Sys.getenv("BLOCKR_VIEW", "Aggregated")
  )
)
