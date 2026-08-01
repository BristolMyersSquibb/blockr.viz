# Preview: the lab parameter table with VISIT on the rows.
#
#   Rscript blockr.viz/dev/preview-lab-by-visit.R
#
# The counterpart to the range chart, not a replacement for it. The chart puts
# time on a continuous x axis and reads the trend; this table puts the same
# visits on rows and carries what the chart cannot print: n per visit, the
# numbers themselves, and both AVAL and % change at once.
#
# Supersedes preview-lab-parameters-overview.R, which put PARAM on the rows.
# That failed because 41 tests in different units have to share one column
# domain. Here the parameter is PINNED, exactly as CDEx 244-025's Lab local
# filter already pins it, so every row is the same measurement in the same
# unit and the shared domain is what makes the visits comparable.
#
# Consequence worth noting: this table sits DOWNSTREAM of the existing pin,
# like every other chart on the page. No second filter branch, no drill wiring.
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

# Stand in for the board's Lab: Local Filter: one parameter, scheduled visits.
# ALT is what the production board pins today.
PARAM_PIN <- "Alanine Aminotransferase (U/L)"
lb <- pharmaverseadam::adlb
lb <- lb[lb$PARAM == PARAM_PIN & grepl("^(Baseline|Week )", lb$AVISIT), ]
lb$ABNORMAL <- as.integer(lb$ANRIND %in% c("HIGH", "LOW"))

# Rows in visit order: `sort_by` takes a raw data column and orders each row by
# that column's minimum within the group. AVISIT is text, so "Week 12" sorts
# before "Week 2"; AVISITN is what the column is for. rank-table.R:617-620 calls
# out this exact case.
VISIT_ORDER <- list(sort_by = "AVISITN", sort_dir = "asc")

# Baseline STAYS as a row even though its % change is 0 by construction. That
# zero is not noise: it shows the reader what "change from baseline" is
# measured against, which is the concept the two columns exist to teach.
COMMON <- list(
  list(type = "simple", func = "count_distinct", col = "USUBJID",
       show = "number", name = "Subjects"),
  list(type = "dist", col = "AVAL", style = "box",
       inner = "median_q1_q3", outer = "tukey", name = "Analysis value (U/L)"),
  list(type = "dist", col = "PCHG", style = "box",
       inner = "median_q1_q3", outer = "tukey", name = "% change from baseline"),
  # Unit-free, so it is the one column that would survive any parameter. It is
  # also the table's stand-in for the chart's ANRHI / ANRLO reference lines:
  # a box at 22 U/L means nothing without the normal range, but an abnormal
  # rate encodes out-of-range directly.
  list(type = "simple", func = "mean", col = "ABNORMAL", show = "bar",
       name = "Abnormal rate")
)

# Same table, both distributions split by treatment. This is the clinician's
# second question ("does the development differ by arm") and the reason the
# VS page's one-way ANOVA chart can go.
BY_ARM <- COMMON
BY_ARM[[2]]$facet <- "TRTA"
BY_ARM[[3]]$facet <- "TRTA"

board <- new_dock_board(
  blocks = c(
    labs = new_static_block(
      lb, block_name = paste0("ADaM ADLB pinned to ", PARAM_PIN)),

    by_visit = new_summarize_table_block(
      by = "AVISIT", summaries = COMMON,
      sort_by = VISIT_ORDER$sort_by, sort_dir = VISIT_ORDER$sort_dir,
      max_height = "700px", search = FALSE, sortable = TRUE,
      title = PARAM_PIN,
      subtitle = "by visit: value and % change from baseline, all arms pooled",
      block_name = "1. By visit (pooled)"),

    by_arm = new_summarize_table_block(
      by = "AVISIT", summaries = BY_ARM,
      sort_by = VISIT_ORDER$sort_by, sort_dir = VISIT_ORDER$sort_dir,
      max_height = "700px", search = FALSE, sortable = TRUE,
      title = PARAM_PIN,
      subtitle = "by visit x treatment arm",
      block_name = "2. By visit x arm"),

    # What it replaces on the page today: a box plot grouped by visit, and an
    # error bar. Both are one column of the table above.
    box = new_chart_block(
      chart_type = "boxplot", group = "AVISIT", value = "AVAL",
      block_name = "Lab: Box Plot (the chart this replaces)"),
    err = new_chart_block(
      chart_type = "pointrange", group = "AVISIT", value = "AVAL",
      block_name = "Lab: Error Bar (the other chart this replaces)")
  ),
  links = links(
    from = c("labs", "labs", "labs", "labs"),
    to   = c("by_visit", "by_arm", "box", "err")
  ),
  grids = list(
    ByVisit = dock_grid("by_visit"),
    ByArm   = dock_grid("by_arm"),
    Replaces = dock_grid("box", "err"),
    Data    = dock_grid("dag_extension")
  ),
  active = "ByVisit",
  extensions = list(dag_extension = blockr.dag::new_dag_extension())
)

cat("\n  http://127.0.0.1:", .port, "/\n\n", sep = "")
serve(board)
