# Live preview for the two Chart-block features on branch
# feat/bar-identity-and-tooltip-dims:
#
#   1. "None (as is)" bars  — a non-aggregated bar. Open the bar's gear: the
#      Aggregate control reads "None (as is)". The height is the `revenue`
#      column verbatim (one bar per region), not a Count/Sum. Flip Aggregate to
#      Sum/Mean to see the contrast.
#   2. Tooltip dimensions   — extra columns on hover. Scatter: each point shows
#      Petal.Length / Petal.Width below the mapped x/y/Species. Line: each
#      series row gets a compact "(age (yrs): …)" suffix. Add/remove columns
#      via the gear's "+ Add mapping" -> Tooltip fields.
#
# WHY THIS SCRIPT (vs dev/example-chart.R): it load_all()s blockr.viz from THIS
# worktree, so the browser is served the worktree's edited inst/js with no
# R CMD INSTALL — which is what lets you run it WITHOUT touching the shared
# installed package another container session may be using.
#
# Run (host OR inside the dev container), from anywhere:
#   Rscript <path-to-worktree>/dev/run-identity-tooltip-preview.R [port]
# Port resolution: arg -> $BLOCKR_PORT -> 3838. On the host pick any free port
# (e.g. `Rscript .../run-identity-tooltip-preview.R 8080`); in the container use
# 3838 (the only forwarded port). Open the URL serve() prints.

# --- locate the worktree + workspace root RELATIVE to this file, so the paths
# --- hold wherever the bind-mount lands on the host ------------------------
this_file <- sub(
  "^--file=", "",
  grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1]
)
wt   <- normalizePath(file.path(dirname(this_file), ".."))          # the worktree
root <- normalizePath(file.path(wt, "..", "..", ".."))              # workspace root
stopifnot(
  file.exists(file.path(wt, "DESCRIPTION")),
  dir.exists(file.path(root, "blockr.core"))
)

# --- port ------------------------------------------------------------------
arg  <- commandArgs(trailingOnly = TRUE)
port <- if (length(arg) && nzchar(arg[1])) {
  as.integer(arg[1])
} else if (nzchar(Sys.getenv("BLOCKR_PORT"))) {
  as.integer(Sys.getenv("BLOCKR_PORT"))
} else {
  3838L
}
options(shiny.port = port, shiny.host = "0.0.0.0")
options(blockr.dock_is_locked = FALSE)

# --- load every package (all load_all'd so pkgload serves SOURCE inst/js) ---
pkgload::load_all(file.path(root, "blockr.core"), quiet = TRUE)
pkgload::load_all(file.path(root, "blockr.ui"),   quiet = TRUE)
pkgload::load_all(file.path(root, "blockr.dplyr"), quiet = TRUE)
pkgload::load_all(file.path(root, "blockr.dock"), quiet = TRUE)
pkgload::load_all(file.path(root, "blockr.dag"),  quiet = TRUE)
pkgload::load_all(wt, quiet = TRUE)                                 # blockr.viz (worktree)

options(blockr.tabular_display = blockr.ui::html_table_display)

# Confirm the SOURCE engine is what will be served (the whole point). If this
# prints the worktree path, inst/js edits are live with no reinstall.
message("blockr.viz JS served from: ",
        system.file("js", package = "blockr.viz"))

# --- demo data (base only — no safetyData, so it runs on a bare host R) -----

# Feature 1: heights already computed upstream — one row per category. Extra
# columns (manager, target) exist to surface as bar tooltip dimensions.
revenue <- data.frame(
  region  = c("North", "South", "East", "West", "Central"),
  revenue = c(1180, 940, 1320, 760, 1050),
  manager = c("Ann", "Ben", "Cara", "Dan", "Eve"),
  target  = c(1100, 1000, 1250, 800, 1000),
  stringsAsFactors = FALSE
)

# Feature 2 (scatter): iris carries extra measurements to surface on hover.
iris_df <- datasets::iris

# Feature 2 (line): Orange + a derived column to show the tooltip suffix.
orange <- datasets::Orange
orange$age_years <- round(orange$age / 365, 2)

board <- new_dock_board(
  blocks = c(
    revenue_data = new_static_block(revenue, block_name = "Regional revenue (precomputed)"),
    # FEATURE 1 — non-aggregated bars: value plotted as-is (func = "identity").
    ident_bar = new_chart_block(
      chart_type = "bar", group = "region", value = "revenue", func = "identity",
      tt_fields = c("manager", "target"),
      block_name = "Revenue by region — None (as is)"),

    iris_data = new_static_block(iris_df, block_name = "iris"),
    # FEATURE 2 (scatter) — extra tooltip dimensions beyond x / y / colour.
    scat = new_chart_block(
      chart_type = "scatter", x = "Sepal.Length", y = "Sepal.Width",
      color = "Species", tt_fields = c("Petal.Length", "Petal.Width"),
      block_name = "Iris — scatter with tooltip dims"),

    orange_data = new_static_block(orange, block_name = "Orange trees"),
    # FEATURE 2 (line) — tooltip dims as a compact per-series suffix.
    lines = new_chart_block(
      chart_type = "line", x = "age", y = "circumference", series = "Tree",
      tt_fields = "age_years",
      block_name = "Orange growth — line with tooltip dims")
  ),
  # Current dock API: links carry an `input` slot; views come from a named
  # `grids` list (the old `links()`/`layouts=` forms are silently ignored).
  links = list(
    list(from = "revenue_data", to = "ident_bar", input = "data"),
    list(from = "iris_data",    to = "scat",      input = "data"),
    list(from = "orange_data",  to = "lines",     input = "data")
  ),
  # Each chart shares its grid with its source data block. Keeping the upstream
  # in the SAME visible view avoids the lazy-eval race where a rebuilt chart
  # panel evaluates before its (otherwise hidden) data block has re-produced
  # data — which surfaced as a transient "Input must be a data frame; got
  # <NULL>" that only cleared on a view switch.
  grids = list(
    Pipeline = dock_grid("dag_extension"),
    Identity = dock_grid(panels("revenue_data"), panels("ident_bar")),
    Scatter  = dock_grid(panels("iris_data"), panels("scat")),
    Line     = dock_grid(panels("orange_data"), panels("lines"))
  ),
  active = "Identity",
  extensions = blockr.dag::new_dag_extension()
)

serve(board)
