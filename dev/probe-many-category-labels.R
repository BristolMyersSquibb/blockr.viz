# Repro for CDEx round-2 item 50, "Not all labels are visible" on Most
# Frequent AE. See _team-ops/tasks/2026-08-david-cdex-ae-round2 (item 50 is
# Christoph's, listed in feedback-round2.md).
#
#   Rscript blockr.viz/dev/probe-many-category-labels.R
#
# The production block is a HORIZONTAL STACKED bar: group = AE Term,
# color = AETOXGR, value = USUBJID, func = count_distinct, sort by value desc.
# Horizontal bars size the panel at one ROW_BAND (28px) per category and clamp
# the total at PANEL_H_CAP = 4000 (inst/js/chart.js:3120-3140, :3327). Past
# 4000/28 = 143 categories the band shrinks, and the category axisLabel sets no
# `interval`, so ECharts falls back to 'auto' and silently drops labels that
# would collide. adae ships 242 distinct AEDECOD, i.e. well past the knee.
#
# Four panels walk across the knee so the drop-out point is visible rather
# than argued about.

.self <- grep("^--file=", commandArgs(FALSE), value = TRUE)[1]
.ws <- normalizePath(if (is.na(.self)) "." else
  file.path(dirname(sub("^--file=", "", .self)), "..", ".."))

# In the devcontainer blockr_port() picks a free forwarded port (3838-3847).
# On the host those ten are held by the container's forward, so fall back
# outside the range.
port <- if (exists("blockr_port")) blockr_port() else 8765L
options(shiny.port = port, shiny.host = "0.0.0.0")

for (p in c("blockr.core", "blockr.ui", "blockr.dock", "blockr.dag",
            "blockr.theme", "blockr.viz")) {
  pkgload::load_all(file.path(.ws, p), helpers = FALSE,
                    attach_testthat = FALSE, export_all = FALSE)
}

adae <- pharmaverseadam::adae

# Terms ordered by subject count, so "top n" is the same cut the chart's
# sort_by = "value" would show anyway.
by_subj <- sort(tapply(adae$USUBJID, adae$AEDECOD, function(x) length(unique(x))),
                decreasing = TRUE)
top <- function(n) adae[adae$AEDECOD %in% names(by_subj)[seq_len(n)], ]

# A real oncology study carries more MedDRA PTs than pharmaverseadam's 242.
# Synthesise the wider case by suffixing terms, so the category count is the
# only thing that changes.
widen <- function(n) {
  d <- adae[rep(seq_len(nrow(adae)), length.out = n * 6L), ]
  d$AEDECOD <- paste0(d$AEDECOD, " ", rep(seq_len(n), length.out = nrow(d)))
  d$USUBJID <- paste0(d$USUBJID, "-", rep(seq_len(n), length.out = nrow(d)))
  d[d$AEDECOD %in% unique(d$AEDECOD)[seq_len(n)], ]
}

cat(sprintf("adae: %d rows, %d subjects, %d distinct AEDECOD\n",
            nrow(adae), length(unique(adae$USUBJID)), length(by_subj)))
cat(sprintf("PANEL_H_CAP knee: 4000 / 28px = %.0f categories\n", 4000 / 28))

chart <- function(nm) {
  new_chart_block(
    chart_type = "bar", orientation = "horizontal", bar_mode = "stacked",
    group = "AEDECOD", color = "AETOXGR", value = "USUBJID",
    func = "count_distinct", sort_by = "value", sort_dir = "desc",
    drill = "auto", block_name = nm)
}

board <- new_dock_board(
  blocks = c(
    d_40  = new_static_block(top(40),  block_name = "adae, top 40 terms"),
    d_143 = new_static_block(top(143), block_name = "adae, top 143 terms"),
    d_500 = new_static_block(widen(500), block_name = "adae widened to 500 terms"),
    d_all = new_static_block(adae,     block_name = "adae, all 242 terms"),

    # 40 rows: 40 * 28 = 1120px, well under the cap. Every label should show.
    p40  = chart("1. 40 terms - band 28px"),
    # 143 rows: 143 * 28 = 4004px, the knee itself.
    p143 = chart("2. 143 terms - at the cap"),
    # 500 rows: capped, band = 4000/500 = 8px, under the ~13px label height.
    p500 = chart("3. 500 terms - band 8px, LABELS DROP"),
    # 242 rows: capped, band = 4000/242 = 16.5px. This is production.
    pall = chart("4. 242 terms - band 16.5px, PRODUCTION")
  ),
  links = links(from = c("d_40", "d_143", "d_500", "d_all"),
                to   = c("p40", "p143", "p500", "pall")),
  views = list(
    p40  = dock_view("p40",  name = "1. 40 terms"),
    p143 = dock_view("p143", name = "2. 143 terms"),
    p500 = dock_view("p500", name = "3. 500 terms"),
    pall = dock_view("pall", name = "4. 242 terms (prod)")
  ),
  grids = list(
    p40  = dock_grid("p40"),
    p143 = dock_grid("p143"),
    p500 = dock_grid("p500"),
    pall = dock_grid("pall")
  ),
  active = "p500"
)

message("Serving the AE label repro on http://127.0.0.1:", port, "/")
serve(board)
