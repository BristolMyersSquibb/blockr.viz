# Exposed mapping band demo — the mapping a reader can steer, on the block.
#
# The problem it answers: a shared "Select Variables" panel in a view's rail
# offers roles that the exhibit on screen does not read, because the panel
# and the exhibit are joined only by a column name. Here every chart carries
# the roles IT reads, declared on the block (`expose =`), so a control exists
# exactly where it has an effect.
#
# Views
#   1. Band          waterfall with Value / Aggregate / Color / Facet on the
#                    face. Facet is optional -> its select leads with
#                    "(none)", so faceting can be switched off WITHOUT the
#                    control disappearing (which is what would happen if
#                    exposure were inferred from the current value).
#   2. Per exhibit   three blocks off one frame: a line chart exposing four
#                    roles, a scatter exposing two, and a chart that exposes
#                    none -- the band is not rendered at all.
#   3. Kinds         the same waterfall over a frame marked with
#                    mark_column_kinds(): Colour offers the four `group`
#                    columns, not USUBJID or the measures. Compare with view
#                    1, whose frame is unmarked, where every role offers
#                    every column.
#
# In the gear, the Mapping section header carries an "On block" checkbox and
# every mapping row an up-arrow pin: that is how a builder chooses what a
# reader gets. The pins write `expose`, which is block state, so the choice
# survives a save.
#
# Run from the workspace root:
#   Rscript blockr.viz/dev/expose-mapping-demo.R [port]
# Port: argument, else BLOCKR_PORT, else 3838.

port <- suppressWarnings(as.integer(commandArgs(trailingOnly = TRUE)[1]))
if (is.na(port)) {
  port <- as.integer(Sys.getenv("BLOCKR_PORT", "3838"))
}
options(shiny.port = port, shiny.host = "0.0.0.0")

options(blockr.tabular_display = blockr.ui::html_table_display)
options(blockr.dock_is_locked = FALSE)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.viz")

# A findings-shaped frame: subjects on arms, a few visits, three measures and
# a couple of stratifiers. Small enough to read, wide enough that "offer every
# column" is visibly the wrong answer.
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
  AGE = sample(34:78, 24, replace = TRUE),
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

# The same rows, with the marks a board's chain head would set. Value roles
# then offer the measures, colour and facet the stratifiers, and neither
# offers USUBJID (24 levels -- a palette per subject).
lab_marked <- mark_column_kinds(
  lab,
  group = c("TRT", "SEX", "RACE"),
  time  = c("AVISIT", "AVISITN"),
  value = c("AVAL", "CHG", "PCHG", "BASE", "AGE"),
  id    = "USUBJID"
)

board <- new_dock_board(
  blocks = c(
    raw = new_static_block(lab, block_name = "Lab (unmarked)"),
    marked = new_static_block(lab_marked, block_name = "Lab (kinds marked)"),

    # 1. The band itself. Facet starts empty on purpose: the view opens
    #    unfacetted and the "(none)" affordance is the visible one.
    wf = new_chart_block(
      chart_type = "bar", group = "USUBJID", value = "CHG", func = "max",
      color = "TRT", sort_by = "value", sort_dir = "desc",
      expose = c("value", "color", "facet"),
      block_name = "Waterfall — mapping on the block"
    ),

    # 2. Three exhibits over one frame, each with its own roles.
    traj = new_chart_block(
      chart_type = "line", x = "AVISIT", y = "AVAL", series = "USUBJID",
      color = "TRT",
      expose = c("y", "x", "color", "facet"),
      block_name = "Linechart — four roles"
    ),
    shift = new_chart_block(
      chart_type = "scatter", x = "BASE", y = "AVAL", color = "TRT",
      identity_line = TRUE,
      # Axes are fixed on purpose: a shift plot against a percent change
      # means nothing, so x/y stay in the gear and the face offers two roles.
      expose = c("color", "facet"),
      block_name = "Shift plot — two roles"
    ),
    anova = new_chart_block(
      chart_type = "boxplot", group = "TRT", value = "AVAL",
      block_name = "ANOVA — no band at all"
    ),

    # 3. Kinds. Same chart, marked frame: open Colour and compare.
    wf_kinds = new_chart_block(
      chart_type = "bar", group = "USUBJID", value = "CHG", func = "max",
      color = "TRT", sort_by = "value", sort_dir = "desc",
      expose = c("value", "color", "facet"),
      block_name = "Waterfall — offers narrowed by column kinds"
    )
  ),
  links = links(
    from = c("raw", "raw", "raw", "raw", "marked"),
    to = c("wf", "traj", "shift", "anova", "wf_kinds")
  ),
  grids = list(
    band = dock_grid("wf"),
    # Separate cells, not one tab strip: the point of the view is seeing the
    # three bands (four roles / two roles / none) at the same time.
    per_exhibit = dock_grid(
      "traj", "shift", "anova",
      orientation = "vertical"
    ),
    kinds = dock_grid("wf_kinds")
  ),
  options = dock_board_options(),
  active = "band",
  extensions = list(blockr.dag::new_dag_extension())
)

serve(board)
