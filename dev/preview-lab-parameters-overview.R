# Preview: the UNFILTERED lab parameter overview. REJECTED -- kept as the
# record of why. Use dev/preview-lab-by-visit.R instead.
#
#   Rscript blockr.viz/dev/preview-lab-parameters-overview.R
#
# Why it was rejected: PARAM on the rows puts 41 tests in different units on
# one shared column domain, so nothing is comparable and the data has to be
# clamped to be legible at all. Clinicians want one parameter's development
# over time, split by treatment -- which is a chart's job, or a table with
# VISIT on the rows and the parameter pinned. See the by-visit preview.
#
# CDEx 244-025 pins PARAM to a single value in each of the Lab / VS / ECG
# local filters, so all six charts on a page show one test. This is the other
# half of that page: one row per PARAM, no pin, so every test is on screen at
# once. It answers "which tests are moving", which the pinned page cannot ask.
#
# The pin is NOT removed here — this table is what would sit BESIDE it. Row
# click -> set the pin is deliberately not wired yet.
#
# The one real constraint: a dist column shares ONE x domain across all rows
# (R/rank-push.R:601). Lab AVAL runs from fractions to 1860 U/L across the 47
# params in this study, so AVAL cannot be the glyph — grid "3. AVAL squash"
# shows exactly how badly. % change from baseline is unit-free and is what
# makes the overview readable.
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

adlb <- pharmaverseadam::adlb

# Stand in for the board's VISIT_TYPE = Scheduled: keep the scheduled weeks
# only. POST-BASELINE MAXIMUM / MINIMUM / LAST are derived qualifier records,
# not visits, and would each become a facet panel of their own.
adlb <- adlb[grepl("^(Baseline|Week )", adlb$AVISIT), ]
# AVISIT is TEXT, so the facet panels come out alphabetically: Week 12, 16, 2,
# 20, ... Order them by AVISITN, which is what the visit column is for.
.ord <- unique(adlb[order(adlb$AVISITN), c("AVISIT", "AVISITN")])
adlb$AVISIT <- factor(adlb$AVISIT, levels = .ord$AVISIT)
adlb$ABNORMAL <- as.integer(adlb$ANRIND %in% c("HIGH", "LOW"))
# "Color" and friends carry no numeric AVAL at all.
keep <- tapply(adlb$AVAL, adlb$PARAM, function(x) any(!is.na(x)))
adlb <- adlb[adlb$PARAM %in% names(keep)[which(keep)], ]

# Post-baseline only for the % change glyph: PCHG is 0 by construction at
# baseline, so a Baseline panel is a column of zeros.
adlb_pb <- adlb[adlb$AVISIT != "Baseline" & !is.na(adlb$PCHG), ]
adlb_pb$AVISIT <- droplevels(adlb_pb$AVISIT)
# Winsorized copy. The dist column's domain is shared by every row, and a few
# heavy-tailed tests (Eosinophils and friends run to several hundred %) drag it
# to 0..400, which flattens the other 40. Clamping to +/-50 is a stand-in for
# the domain override the block does NOT have — the same gap the AE swimlane
# hit with its -200 x domain. The median is untouched by the clamp; only the
# whiskers are pulled in.
adlb_pb$PCHG_C <- pmin(pmax(adlb_pb$PCHG, -50), 50)

board <- new_dock_board(
  blocks = c(
    labs = new_static_block(
      adlb, block_name = "ADaM ADLB — scheduled visits, all parameters"),
    labs_pb = new_static_block(
      adlb_pb, block_name = "ADaM ADLB — post-baseline, PCHG present"),

    # 1. THE OVERVIEW, honest and readable. A box per test per visit, on a
    #    domain clamped to +/-50% so the heavy-tailed tests stop owning the
    #    axis. Sign is real here: a box left of centre is a fall.
    overview = new_summarize_table_block(
      by = "PARAM",
      summaries = list(
        list(type = "simple", func = "count_distinct", col = "USUBJID",
             show = "number", name = "Subjects"),
        list(type = "dist", col = "PCHG_C", style = "box",
             inner = "median_q1_q3", outer = "p10_p90", facet = "AVISIT",
             name = "% change from baseline")
      ),
      sort_by = "label", sort_dir = "asc",
      max_height = "700px", search = TRUE, sortable = TRUE,
      title = "Lab: Parameters",
      subtitle = "% change from baseline by visit, clamped to +/-50%",
      block_name = "1. Lab: Parameters (clamped domain)"),

    # 2. The same thing on the raw PCHG: what the shared domain does when a
    #    handful of tests run to several hundred percent.
    dist = new_summarize_table_block(
      by = "PARAM",
      summaries = list(
        list(type = "simple", func = "count_distinct", col = "USUBJID",
             show = "number", name = "Subjects"),
        list(type = "dist", col = "PCHG", style = "box",
             inner = "median_q1_q3", outer = "p10_p90", facet = "AVISIT",
             name = "% change from baseline")
      ),
      sort_by = "label", sort_dir = "asc",
      max_height = "700px", search = TRUE, sortable = TRUE,
      title = "Lab: Parameters",
      subtitle = "unclamped: the shared domain runs to +400%",
      block_name = "2. Unclamped (why the block wants a domain override)"),

    # 3. Reducing each cell to its median makes the table far more compact —
    #    but a `simple` bar column CANNOT draw a signed value. All-negative
    #    columns render empty, and in a mixed column -30 draws the same full
    #    width as +10. Read the numbers, not the bars, in this grid.
    medbar = new_summarize_table_block(
      by = "PARAM",
      summaries = list(
        list(type = "simple", func = "count_distinct", col = "USUBJID",
             show = "number", name = "Subjects"),
        list(type = "simple", func = "median", col = "PCHG", show = "bar",
             facet = "AVISIT", name = "Median % change")
      ),
      sort_by = "label", sort_dir = "asc",
      max_height = "700px", search = TRUE, sortable = TRUE,
      title = "Lab: Parameters",
      subtitle = "median % change — BARS IGNORE SIGN, see dev note",
      block_name = "3. Median bar (sign bug: bars are not trustworthy)"),

    # 2. The same table without the visit facet — one glyph per test over the
    #    whole post-baseline period. Narrower; the question is whether losing
    #    the time axis costs too much.
    compact = new_summarize_table_block(
      by = "PARAM",
      summaries = list(
        list(type = "simple", func = "count_distinct", col = "USUBJID",
             show = "number", name = "Subjects"),
        list(type = "dist", col = "PCHG_C", style = "box",
             inner = "median_q1_q3", outer = "p10_p90",
             name = "% change from baseline"),
        list(type = "simple", func = "mean", col = "ABNORMAL", show = "bar",
             name = "Abnormal rate")
      ),
      sort_by = "label", sort_dir = "asc",
      max_height = "700px",
      title = "Lab: Parameters",
      subtitle = "pooled post-baseline % change, plus abnormal rate",
      block_name = "3. Compact (no visit facet, + abnormal rate)"),

    # 3. Why the glyph is PCHG and not AVAL: 47 tests in different units on
    #    one shared domain. Creatinine Kinase (to 1860) owns the axis and
    #    every other test is a dot on the left edge.
    squash = new_summarize_table_block(
      by = "PARAM",
      summaries = list(
        list(type = "dist", col = "AVAL", style = "box",
             inner = "median_q1_q3", outer = "tukey", name = "Value")
      ),
      sort_by = "label", sort_dir = "asc",
      max_height = "700px",
      title = "Lab: Parameters — raw AVAL",
      subtitle = "one shared x domain across mixed units: unreadable on purpose",
      block_name = "4. AVAL squash (the case against raw values)")
  ),
  links = links(
    from = c("labs_pb", "labs_pb", "labs_pb", "labs_pb", "labs"),
    to   = c("overview", "dist", "medbar", "compact", "squash")
  ),
  grids = list(
    Overview  = dock_grid("overview"),
    Unclamped = dock_grid("dist"),
    MedianBar = dock_grid("medbar"),
    Compact   = dock_grid("compact"),
    Squash    = dock_grid("squash"),
    Data      = dock_grid("dag_extension")
  ),
  active = "Overview",
  extensions = list(dag_extension = blockr.dag::new_dag_extension())
)

cat("\n  http://127.0.0.1:", .port, "/\n\n", sep = "")
serve(board)
