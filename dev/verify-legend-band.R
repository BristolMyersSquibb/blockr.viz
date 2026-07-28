# Legend-band verification: the HTML band (.dd-legend-band) is now the ONLY
# legend -- no family draws a native in-canvas ECharts legend any more.
# Each family appears TWICE, unfaceted and faceted, so the two modes that used
# to render different legends can be compared side by side. The legend title
# check from verify-legend-title.R rides along: AESEV is labelled (band must
# show "Severity of Adverse Event"), ARM is not (band must show "ARM").
#
# NOTE: blockr.viz loads from the WORKTREE, every other package from /workspace.
# All packages are load_all'd, so inst/js source is served (no reinstall).
VIZ <- "/workspace/blockr.viz/.claude/worktrees/legend-band"

options(shiny.port = as.integer(Sys.getenv("BLOCKR_PORT", "3838")),
        shiny.host = "0.0.0.0")
pkgload::load_all("/workspace/blockr.core")
pkgload::load_all("/workspace/blockr.ui")
pkgload::load_all("/workspace/blockr.dplyr")
pkgload::load_all("/workspace/blockr.dock")
pkgload::load_all("/workspace/blockr.dag")
pkgload::load_all("/workspace/blockr.theme")
pkgload::load_all(VIZ)

stopifnot(identical(system.file("js", package = "blockr.viz"),
                    file.path(VIZ, "inst", "js")))

adae <- safetyData::adam_adae
adsl <- safetyData::adam_adsl

attr(adae$AESEV, "label") <- "Severity of Adverse Event"
attr(adae$ASTDY, "label") <- "Study Day of Start of Adverse Event"
attr(adae$AENDY, "label") <- "Study Day of End of Adverse Event"
attr(adsl$ARM, "label") <- NULL

ae <- adae[!is.na(adae$ASTDY) & !is.na(adae$AENDY), ]
ae <- ae[ae$USUBJID %in% head(unique(ae$USUBJID), 12), ]

board <- new_dock_board(
  blocks = c(
    ae_data = new_static_block(ae, block_name = "ADaM ADAE (AESEV labelled)"),
    sl_data = new_static_block(adsl, block_name = "ADaM ADSL (ARM unlabelled)"),

    # -- unfaceted: the cases that used to draw a NATIVE legend --------------
    bar = new_chart_block(
      chart_type = "bar", group = "AEBODSYS", color = "AESEV", func = "count",
      block_name = "BAR (1 panel): band, plot keeps the reclaimed row"),
    box = new_chart_block(
      chart_type = "boxplot", group = "AEBODSYS", value = "ASTDY",
      color = "AESEV",
      block_name = "BOXPLOT (1 panel): band"),
    radar = new_chart_block(
      chart_type = "radar", group = "AEBODSYS", color = "AESEV", func = "count",
      block_name = "RADAR (1 panel): centre moved 46% -> 50%, check clipping"),
    scat = new_chart_block(
      chart_type = "scatter", x = "AGE", y = "BMIBL", color = "ARM",
      block_name = "SCATTER (1 panel): unlabelled -> band says 'ARM'"),
    gantt = new_chart_block(
      chart_type = "gantt", x = "ASTDY", xend = "AENDY", y = "AEDECOD",
      color = "AESEV",
      block_name = "TIMELINE (1 panel): band, no scroll legend"),

    # -- faceted: the cases that already used the band (regression guard) ----
    bar_f = new_chart_block(
      chart_type = "bar", group = "AEBODSYS", color = "AESEV", func = "count",
      facet = "SEX",
      block_name = "BAR (faceted): unchanged, chips toggle ALL panels"),
    box_f = new_chart_block(
      chart_type = "boxplot", group = "AEBODSYS", value = "ASTDY",
      color = "AESEV", facet = "SEX",
      block_name = "BOXPLOT (faceted): unchanged"),
    radar_f = new_chart_block(
      chart_type = "radar", group = "AEBODSYS", color = "AESEV", func = "count",
      facet = "SEX",
      block_name = "RADAR (faceted): unchanged"),
    scat_f = new_chart_block(
      chart_type = "scatter", x = "AGE", y = "BMIBL", color = "ARM",
      facet = "SEX",
      block_name = "SCATTER (faceted): unchanged"),
    gantt_f = new_chart_block(
      chart_type = "gantt", x = "ASTDY", xend = "AENDY", y = "AEDECOD",
      color = "AESEV", facet = "SEX",
      block_name = "TIMELINE (faceted): unchanged")
  ),
  links = c(
    new_link("ae_data", "bar", "data"),
    new_link("ae_data", "box", "data"),
    new_link("ae_data", "radar", "data"),
    new_link("sl_data", "scat", "data"),
    new_link("ae_data", "gantt", "data"),
    new_link("ae_data", "bar_f", "data"),
    new_link("ae_data", "box_f", "data"),
    new_link("ae_data", "radar_f", "data"),
    new_link("sl_data", "scat_f", "data"),
    new_link("ae_data", "gantt_f", "data")
  ),
  # TILED, not stacked: dock tabs are lazy, so a stacked panel never renders
  # its chart until fronted and the headless probe sees an empty band. One
  # view per mode; BLOCKR_FACET=1 fronts the faceted one.
  grids = list(
    Unfaceted = dock_grid("bar", "box", "radar", "scat", "gantt"),
    Faceted = dock_grid("bar_f", "box_f", "radar_f", "scat_f", "gantt_f"),
    Data = dock_grid(c("ae_data", "sl_data"))
  ),
  active = if (nzchar(Sys.getenv("BLOCKR_FACET"))) "Faceted" else "Unfaceted"
)

cat("\n  http://127.0.0.1:",
    getOption("shiny.port"), "/\n\n", sep = "")
serve(board)
