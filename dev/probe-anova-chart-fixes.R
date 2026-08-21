# Repro for the three CDEx one-way-ANOVA chart complaints, one view each.
# See _team-ops/tasks/2026-08-christoph-cdex-anova-chart-fixes.
#
#   Rscript blockr.viz/dev/probe-anova-chart-fixes.R
#
# All three are boxplot chart-block behaviours, so none of them is
# ANOVA-specific -- the CDEx view just happens to be where the team hit them.
# The shipped `pop_anova` block sets neither `color` nor `facet`, which is why
# problems 2 and 3 are invisible there: this script switches them on.

.self <- grep("^--file=", commandArgs(FALSE), value = TRUE)[1]
.ws <- normalizePath(if (is.na(.self)) "." else
  file.path(dirname(sub("^--file=", "", .self)), "..", ".."))

port <- blockr_port()
options(shiny.port = port, shiny.host = "0.0.0.0")

for (p in c("blockr.core", "blockr.ui", "blockr.dock", "blockr.dag",
            "blockr.theme", "blockr.viz")) {
  pkgload::load_all(file.path(.ws, p), helpers = FALSE,
                    attach_testthat = FALSE, export_all = FALSE)
}

adsl <- pharmaverseadam::adsl

# A 10-level column for problem 3. ADSL has NOTHING in the 8..15 band that
# recycling needs: every candidate is under 8 (RACE 4, TRT01P 4, SEX 2) and
# SITEID has 17, which trips the MAX_COLOR_LEVELS = 15 hard stop and renders
# a "Too many color levels" message instead. So the recycling is unreachable
# on ADSL as shipped -- it has to be built.
top9 <- names(sort(table(adsl$SITEID), decreasing = TRUE))[1:9]
adsl$SITEGRP <- ifelse(adsl$SITEID %in% top9, paste0("Site ", adsl$SITEID),
                       "Other")

board <- new_dock_board(
  blocks = c(
    data = new_static_block(adsl, block_name = "ADSL (+ SITEGRP, 10 levels)"),

    # 1. Same config as cedx_board's `pop_anova`, minus the deprecated
    #    `metric` alias. Click a box: the footer reads "Filtered: TRT01P =
    #    ..." and the drill fires, but every box keeps full opacity.
    #    _updateHighlight() (inst/js/chart.js:5478) excludes 'boxplot' at
    #    :5510, in the same branch as line/scatter/custom -- families that
    #    are hover-driven and genuinely don't want the category mask.
    p1 = new_chart_block(
      chart_type = "boxplot", group = "TRT01P", value = "AGE",
      drill = "auto",
      block_name = "1. Click a box - nothing highlights"),

    # 2. RACE is 273 / 29 / 2 / 2 in ADSL, so the two tiny panels have no
    #    rows for most (SEX x arm) pairs -- and keep a slot for each anyway.
    #    inst/js/chart.js:3696: keepEmptySlots is TRUE whenever a facet is on
    #    and facet_scales is not "free". Switch facet_scales to "free" in the
    #    gear and the blanks go: that is the existing escape hatch.
    #    `drill` is on here so this view also exercises the colour-SPLIT click
    #    path: a split slot's category is `group + BOX_CAT_SEP + level`, and
    #    the selection is the group half only (chart.js:2431). Without a drill
    #    the click handler returns early (:2417) and nothing selects at all.
    p2 = new_chart_block(
      chart_type = "boxplot", group = "SEX", value = "AGE",
      color = "TRT01P", facet = "RACE", facet_scales = "fixed",
      drill = "auto",
      block_name = "2. Facet - empty slots kept in every panel"),

    # 3. 10 colour levels over a 7-colour pool (BLOCKR_PALETTE,
    #    inst/js/chart.js:270). hexFor() at :3651 is
    #    `palette[i % palette.length]`, so levels 8/9/10 repeat the hex of
    #    levels 1/2/3. Read the legend left to right: entry 8 is entry 1's
    #    blue again.
    #    A shorter palette is the other way to see it -- put a board
    #    `new_scale_map_option(new_scale_map(palette = c(...3 colours...)))`
    #    on this board and even the 4-arm charts above start repeating.
    p3 = new_chart_block(
      chart_type = "boxplot", group = "SITEGRP", value = "AGE",
      color = "SITEGRP", drill = "auto",
      block_name = "3. Colour - 10 levels over a 7-colour palette")
  ),
  links = links(from = rep("data", 3), to = c("p1", "p2", "p3")),
  views = list(
    p1 = dock_view("p1", name = "1. Highlight"),
    p2 = dock_view("p2", name = "2. Facet slots"),
    p3 = dock_view("p3", name = "3. Colour re-use")
  ),
  grids = list(
    p1 = dock_grid("p1"),
    p2 = dock_grid("p2"),
    p3 = dock_grid("p3")
  ),
  active = "p1"
)

message("Serving the ANOVA chart repro on http://127.0.0.1:", port, "/")
serve(board)
