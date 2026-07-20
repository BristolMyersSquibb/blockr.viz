# Legend title verification: the color legend must name its variable the way
# an axis names its column -- the attr `label` when set, else the column name.
# One block per legend-building family (bar, boxplot, radar, scatter, gantt).
options(shiny.port = as.integer(Sys.getenv("BLOCKR_PORT", "3838")),
        shiny.host = "0.0.0.0")
pkgload::load_all("/workspace/blockr.core")
pkgload::load_all("/workspace/blockr.ui")
pkgload::load_all("/workspace/blockr.dplyr")
pkgload::load_all("/workspace/blockr.dock")
pkgload::load_all("/workspace/blockr.dag")
pkgload::load_all("/workspace/blockr.theme")
pkgload::load_all("/workspace/blockr.viz")

adae <- safetyData::adam_adae
adsl <- safetyData::adam_adsl

# LABELLED color column -> legend must show the label, not AESEV.
attr(adae$AESEV, "label") <- "Severity of Adverse Event"
attr(adae$ASTDY, "label") <- "Study Day of Start of Adverse Event"
attr(adae$AENDY, "label") <- "Study Day of End of Adverse Event"
# ARM carries NO label -> legend must fall back to the bare column name.
attr(adsl$ARM, "label") <- NULL

ae <- adae[!is.na(adae$ASTDY) & !is.na(adae$AENDY), ]
ae <- ae[ae$USUBJID %in% head(unique(ae$USUBJID), 12), ]

board <- new_dock_board(
  blocks = c(
    ae_data = new_static_block(ae, block_name = "ADaM ADAE (AESEV labelled)"),
    sl_data = new_static_block(adsl, block_name = "ADaM ADSL (ARM unlabelled)"),

    # 1. Gantt -- the screenshot case: bare "1 2 3 4 5" chips.
    gantt = new_chart_block(
      chart_type = "gantt", x = "ASTDY", xend = "AENDY", y = "AEDECOD",
      color = "AESEV",
      block_name = "TIMELINE: legend says 'Severity of Adverse Event'"),

    # 2. Aggregated bar -- legend previously derived from series names.
    bar = new_chart_block(
      chart_type = "bar", group = "AEBODSYS", color = "AESEV", func = "count",
      block_name = "BAR: legend title + count by body system"),

    # 3. Boxplot -- legendOn = split.
    box = new_chart_block(
      chart_type = "boxplot", group = "AEBODSYS", value = "ASTDY",
      color = "AESEV",
      block_name = "BOXPLOT: legend title over split levels"),

    # 4. Radar -- separate _radarLayout reservation.
    radar = new_chart_block(
      chart_type = "radar", group = "AEBODSYS", color = "AESEV",
      func = "count",
      block_name = "RADAR: legend title, shape must not overlap"),

    # 5. Scatter with UNLABELLED color -> falls back to "ARM".
    scat = new_chart_block(
      chart_type = "scatter", x = "AGE", y = "BMIBL", color = "ARM",
      block_name = "SCATTER: no label -> legend says 'ARM'")
  ),
  links = c(
    new_link("ae_data", "gantt", "data"),
    new_link("ae_data", "bar", "data"),
    new_link("ae_data", "box", "data"),
    new_link("ae_data", "radar", "data"),
    new_link("sl_data", "scat", "data")
  )
)

serve(board)
