# Preview: Lab: Waterfall as a summarize table (rows = subject).
#
#   Rscript blockr.viz/dev/preview-lab-waterfall-table.R
#
# A waterfall is one signed number per subject, sorted. Row key = subject, one
# statistic, ranking is the point: structurally the summarize table's best
# case, and it fixes the chart's known weakness (250 anonymous bars, no way to
# tell which bar is which subject).
#
# This needed the sign fix to be honest at all: a `simple` bar column used to
# draw value as LENGTH FROM ZERO, so a fall drew as wide as a rise and an
# all-negative column collapsed to empty tracks. On this data 95 of 246
# subjects have a negative extreme, so 39% of the rows were wrong, and all of
# them sat below the fold of a descending sort. A bar column whose values go
# negative now becomes the zero-centred diverging bar instead
# (lane_summary_domains(), R/lane-summaries.R).
options(blockr.tabular_display = blockr.ui::html_table_display)
options(blockr.dock_is_locked = FALSE)
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

stopifnot(requireNamespace("pharmaverseadam", quietly = TRUE))
suppressMessages(library(dplyr))

PARAM_PIN <- "Alanine Aminotransferase (U/L)"
lb <- pharmaverseadam::adlb |>
  filter(PARAM == PARAM_PIN, grepl("^(Baseline|Week )", AVISIT))

# Subject-level facts, carried on the long frame so the sparkline column still
# has its per-visit rows. On the production board these are mutates upstream.
subj <- lb |>
  filter(grepl("^Week", AVISIT), !is.na(PCHG)) |>
  group_by(USUBJID) |>
  summarise(EXTREME = PCHG[which.max(abs(PCHG))], .groups = "drop")
base <- lb |>
  filter(AVISIT == "Baseline") |>
  group_by(USUBJID) |>
  summarise(BASELINE = median(AVAL, na.rm = TRUE), .groups = "drop")

lb <- lb |> inner_join(subj, by = "USUBJID") |> left_join(base, by = "USUBJID")
lb$ABNORMAL <- as.integer(lb$ANRIND %in% c("HIGH", "LOW"))

# Waterfall ordering: biggest rise at the top, biggest fall at the bottom.
# EXTREME is constant within a subject, so the group minimum IS the value.
SORT <- list(sort_by = "EXTREME", sort_dir = "desc")

board <- new_dock_board(
  blocks = c(
    labs = new_static_block(
      lb, block_name = paste0("ADaM ADLB pinned to ", PARAM_PIN)),

    # ONE signed column. A bar column whose values go negative now becomes the
    # zero-centred diverging bar, so this is a waterfall: sorted descending,
    # rises right of centre, falls left, each with its subject label.
    waterfall = new_summarize_table_block(
      by = "USUBJID",
      summaries = list(
        list(type = "field", col = "TRTA", name = "Arm"),
        list(type = "simple", func = "max", col = "BASELINE", show = "number",
             name = "Baseline (U/L)"),
        list(type = "simple", func = "max", col = "EXTREME", show = "bar",
             name = "Extreme % change"),
        # The locator the series note argued for: the bar says how much, the
        # sparkline says when, the row click says who.
        list(type = "series", x = "AVISITN", col = "AVAL",
             name = "Trajectory"),
        list(type = "simple", func = "mean", col = "ABNORMAL", show = "bar",
             name = "Abnormal rate")
      ),
      sort_by = SORT$sort_by, sort_dir = SORT$sort_dir,
      max_height = "700px", search = TRUE, sortable = TRUE,
      title = PARAM_PIN,
      subtitle = "extreme % change per subject, sorted: the waterfall",
      block_name = "1. Waterfall (one diverging bar)"),

    # The same table sorted ascending, so the fallers are at the top. Before
    # the fix this view was the one that exposed the bug: every fall drew as a
    # full-width rise. It is now the mirror image of grid 1, as it should be.
    falls = new_summarize_table_block(
      by = "USUBJID",
      summaries = list(
        list(type = "field", col = "TRTA", name = "Arm"),
        list(type = "simple", func = "max", col = "EXTREME", show = "bar",
             name = "Extreme % change")
      ),
      sort_by = SORT$sort_by, sort_dir = "asc",
      max_height = "700px", search = TRUE, sortable = TRUE,
      title = PARAM_PIN,
      subtitle = "sorted ascending: the fallers, where the bug used to hide",
      block_name = "2. Falls first (ascending)"),

    # The chart it would replace.
    wf = new_chart_block(
      chart_type = "bar", group = "USUBJID", value = "EXTREME",
      func = "max", block_name = "Lab: Waterfall (the chart this replaces)")
  ),
  links = links(
    from = c("labs", "labs", "labs"),
    to   = c("waterfall", "falls", "wf")
  ),
  grids = list(
    Waterfall = dock_grid("waterfall"),
    Falls     = dock_grid("falls"),
    Chart     = dock_grid("wf"),
    Data      = dock_grid("dag_extension")
  ),
  active = "Waterfall",
  extensions = list(dag_extension = blockr.dag::new_dag_extension())
)

cat("\n  http://127.0.0.1:", .port, "/\n\n", sep = "")
serve(board)
