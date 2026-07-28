# Verify facet panel scales (facet_scales).
#   Rscript blockr.viz/dev/verify-facet-scales.R
#
# The interesting pair is lab panels: faceting by PARAM puts columns with
# different units side by side, which is the one case where free scales are
# the honest reading. Everything else (arms, sex, age groups) shares a unit,
# and there the shared domain is what makes the panels comparable.
options(blockr.tabular_display = blockr.ui::html_table_display)
options(blockr.dock_is_locked = FALSE)
# Package roots resolve from THIS script's location, so the same command runs
# in the container (/workspace) and on a host clone. Port: command-line arg,
# then BLOCKR_PORT, then the container's forwarded 3838.
.self <- grep("^--file=", commandArgs(FALSE), value = TRUE)[1]
.ws <- normalizePath(if (is.na(.self)) "." else
  file.path(dirname(sub("^--file=", "", .self)), "..", ".."))
.port <- as.integer(c(commandArgs(TRUE), Sys.getenv("BLOCKR_PORT"), "3838")[1])
options(shiny.port = .port, shiny.host = "0.0.0.0")

pkgload::load_all(file.path(.ws, "blockr.core"))
pkgload::load_all(file.path(.ws, "blockr.ui"))
pkgload::load_all(file.path(.ws, "blockr.dock"))
pkgload::load_all(file.path(.ws, "blockr.dag"))
pkgload::load_all(file.path(.ws, "blockr.viz"))

stopifnot(requireNamespace("safetyData", quietly = TRUE))
adsl <- safetyData::adam_adsl
# Three labs with genuinely different ranges (the free-scales case).
adlb <- subset(
  safetyData::adam_adlbc,
  PARAM %in% unique(safetyData::adam_adlbc$PARAM)[1:3] & !is.na(AVAL)
)

board <- new_dock_board(
  blocks = c(
    data = new_static_block(adsl, block_name = "ADaM ADSL"),
    labs = new_static_block(adlb, block_name = "ADaM ADLBC (3 params)"),
    # Shared unit: every panel counts subjects, so one domain is right and
    # the arms line up slot for slot across the panels.
    fixed_bar = new_chart_block(
      chart_type = "bar", group = "ARM", facet = "SEX",
      value = ".count", func = "count",
      block_name = "Subjects by arm x sex — shared scale (default)"),
    # Same chart, per-panel axes: the F and M panels no longer compare.
    free_bar = new_chart_block(
      chart_type = "bar", group = "ARM", facet = "SEX",
      value = ".count", func = "count", facet_scales = "free",
      block_name = "Same chart, facet_scales = free"),
    # Different units per panel: this is what "free" is FOR.
    lab_free = new_chart_block(
      chart_type = "boxplot", group = "ARM", facet = "PARAM", value = "AVAL",
      facet_scales = "free",
      block_name = "Lab values by param — free (different units)"),
    # ...and the same thing on a shared scale, to see the squash the free
    # option exists to avoid.
    lab_fixed = new_chart_block(
      chart_type = "boxplot", group = "ARM", facet = "PARAM", value = "AVAL",
      block_name = "Lab values by param — shared (squashed on purpose)"),
    # A category missing from a panel: with the shared set it keeps its slot
    # in every panel instead of shifting the ones after it.
    ragged = new_chart_block(
      chart_type = "bar", group = "RACE", facet = "ARM",
      value = ".count", func = "count",
      block_name = "Race by arm — ragged categories, aligned")
  ),
  links = links(
    from = c("data", "data", "labs", "labs", "data"),
    to   = c("fixed_bar", "free_bar", "lab_free", "lab_fixed", "ragged")
  ),
  grids = list(
    Shared = dock_grid("fixed_bar"),
    Free   = dock_grid("free_bar"),
    Labs   = dock_grid("lab_free"),
    Squash = dock_grid("lab_fixed"),
    Ragged = dock_grid("ragged"),
    Data   = dock_grid("dag_extension")
  ),
  active = "Shared",
  extensions = list(dag_extension = blockr.dag::new_dag_extension())
)

cat("\n  http://127.0.0.1:", .port, "/\n\n", sep = "")
serve(board)
