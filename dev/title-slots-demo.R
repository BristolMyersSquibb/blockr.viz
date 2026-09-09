# Title slots — the block's sentence, with its settings as the controls.
#
# The subtitle is already a template resolved against the current data
# ({ARM}, {label(col)}, {n}, {filters}). Two additions make it the block's
# control surface:
#
#   {@arg}      prints the current value of a block argument, and makes that
#               word a control: click it and the argument's own picker opens
#               under it. {label(@y)} prints the column's label and is still
#               the picker for `y`; {n_distinct(@y)} is a count, so it is
#               text, not a control.
#   [ ... ]     a clause that leaves with its token. "[, faceted by {@facet}]"
#               says nothing at all when facet is unset -- and the setting is
#               offered as a "+ Facet" chip at the end of the sentence, which
#               is the only way to reach a setting that is not set yet.
#
# Views
#   1. Sentence   one chart, four settable words, no band at all. Change the
#                 colour from the sentence and watch the clause for facet
#                 appear when you give it one.
#   2. Both       the same chart twice: with a sentence and without one. The
#                 second is what every chart looked like before this.
#   3. Optional   a chart whose facet is unset, so its sentence is shorter.
#
# Rules: blockr.docs design-system/pinned-controls.md.
#
# Run from the workspace root:
#   Rscript blockr.viz/dev/title-slots-demo.R [port]
# Port: argument, else BLOCKR_PORT, else 3838.

port <- suppressWarnings(as.integer(commandArgs(trailingOnly = TRUE)[1]))
if (is.na(port)) {
  port <- as.integer(Sys.getenv("BLOCKR_PORT", "3838"))
}
options(shiny.port = port, shiny.host = "0.0.0.0")
message("http://127.0.0.1:", port)

options(blockr.tabular_display = blockr.ui::html_table_display)
options(blockr.dock_is_locked = FALSE)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.viz")

set.seed(42)
subj <- sprintf("01-%03d", 1:24)
lab <- expand.grid(
  USUBJID = subj,
  AVISIT = factor(c("Baseline", "Week 4", "Week 8"),
                  levels = c("Baseline", "Week 4", "Week 8")),
  stringsAsFactors = FALSE
)
meta <- data.frame(
  USUBJID = subj,
  TRT = rep(c("Placebo", "Active arm"), each = 12),
  SEX = rep(c("F", "M"), 12),
  RACE = rep(c("WHITE", "ASIAN", "BLACK"), 8),
  stringsAsFactors = FALSE
)
lab <- merge(lab, meta, by = "USUBJID")
lab$AVISITN <- as.integer(lab$AVISIT)
lab$BASE <- round(runif(nrow(lab), 12, 30), 1)
lab$AVAL <- round(lab$BASE + rnorm(nrow(lab), 0, 4) +
                    ifelse(lab$TRT == "Active arm", lab$AVISITN * 2.2, 0), 1)
lab$CHG <- round(lab$AVAL - lab$BASE, 1)
lab$PCHG <- round(100 * lab$CHG / lab$BASE, 1)
attr(lab$AVAL, "label") <- "Analysis Value"
attr(lab$CHG, "label") <- "Change from Baseline"
attr(lab$PCHG, "label") <- "Percent Change"
attr(lab$TRT, "label") <- "Actual Treatment"

lab <- mark_column_kinds(
  lab,
  group = c("TRT", "SEX", "RACE"),
  time  = c("AVISIT", "AVISITN"),
  value = c("AVAL", "CHG", "PCHG", "BASE"),
  id    = "USUBJID"
)

sentence <- paste0(
  "{@func} of {label(@y)} by {@x}",
  "[, coloured by {@color}]",
  "[, split by {@facet}]"
)

# Facet is unset on this board, as it is on 31 of the 32 CEDX exhibits, so the
# clause is not there and neither is the word that would open it. The offer
# chip at the end of the sentence is what a reader clicks to add one.

board <- new_dock_board(
  blocks = c(
    raw = new_static_block(lab, block_name = "Lab"),

    # 1. Four settable words, no band.
    sent = new_chart_block(
      chart_type = "line", x = "AVISIT", y = "AVAL", func = "mean",
      series = "TRT", color = "TRT",
      title = "Laboratory trajectory",
      subtitle = sentence,
      block_name = "Trajectory"
    ),

    # 2a / 2b. The same settings, drawn twice. The band is the OLD channel and
    # is on its way out; this pair is what says why.
    both_sent = new_chart_block(
      chart_type = "bar", group = "USUBJID", value = "CHG", func = "max",
      color = "TRT", sort_by = "value", sort_dir = "desc",
      title = "Change from baseline",
      subtitle = "{@func} of {label(@value)} by subject[, coloured by {@color}][, split by {@facet}]",
      block_name = "Waterfall - sentence"
    ),
    both_band = new_chart_block(
      chart_type = "bar", group = "USUBJID", value = "CHG", func = "max",
      color = "TRT", sort_by = "value", sort_dir = "desc",
      title = "Change from baseline",
      # No sentence: everything is behind the gear, which is what a block
      # looks like when nothing is promoted.
      block_name = "Waterfall - gear only"
    ),

    # 3. Facet unset, so the clause is not there at all. Set it from the
    #    gear and the sentence grows one.
    optional = new_chart_block(
      chart_type = "boxplot", group = "TRT", value = "AVAL",
      title = "Analysis value by arm",
      subtitle = "{label(@value)} by {@group}[, split by {@facet}], {n} records",
      block_name = "Boxplot - optional clause"
    )
  ),
  links = links(
    from = c("raw", "raw", "raw", "raw"),
    to = c("sent", "both_sent", "both_band", "optional")
  ),
  grids = list(
    sentence = dock_grid("sent"),
    both = dock_grid("both_sent", "both_band", orientation = "vertical"),
    optional = dock_grid("optional")
  ),
  options = dock_board_options(),
  active = "sentence",
  extensions = list(blockr.dag::new_dag_extension())
)

serve(board)
