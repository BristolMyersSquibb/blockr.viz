# Repro + acceptance for the AE percentage work (CDEx round-2 items 47/48, see
# _team-ops/tasks/2026-08-christoph-cdex-most-frequent-ae-percent).
#
#   Rscript blockr.viz/dev/probe-pct-distinct.R
#
# The point: the denominator is the PANEL's subjects, and it comes from rows
# rather than from a joined-on number. Every safety subject is in the frame;
# the ones with no AE carry an NA term, draw no bar (na_group = "drop") and
# still count (pct_distinct). Panel 4 is the whole thing together.

.self <- grep("^--file=", commandArgs(FALSE), value = TRUE)[1]
.ws <- normalizePath(if (is.na(.self)) "." else
  file.path(dirname(sub("^--file=", "", .self)), "..", ".."))

port <- if (exists("blockr_port")) blockr_port() else 8765L
options(shiny.port = port, shiny.host = "0.0.0.0")

for (p in c("blockr.core", "blockr.ui", "blockr.dock", "blockr.dag",
            "blockr.theme", "blockr.viz")) {
  pkgload::load_all(file.path(.ws, p), helpers = FALSE,
                    attach_testthat = FALSE, export_all = FALSE)
}

adsl <- pharmaverseadam::adsl
adae <- pharmaverseadam::adae
saf <- adsl[!is.na(adsl$SAFFL) & adsl$SAFFL == "Y",
            c("USUBJID", "TRT01A", "SAFFL")]

# What `dm_flatten_block(join_type = "right")` produces: every safety subject
# present, those with no AE carrying an NA term. 29 of 254 here.
pop <- merge(saf, adae[, c("USUBJID", "AEDECOD", "AESEV")],
             by = "USUBJID", all.x = TRUE)
# One row per subject per term, worst severity kept (composer::keep_worst does
# this on the real board; done inline so the probe needs no composer).
pop$rank <- match(pop$AESEV, c("MILD", "MODERATE", "SEVERE"))
pop <- pop[order(pop$USUBJID, pop$AEDECOD, -pop$rank, na.last = TRUE), ]
pop <- pop[!duplicated(paste(pop$USUBJID, pop$AEDECOD, sep = "\r")), ]
# Cut to the 12 most frequent terms -- and keep one row for every subject the
# cut would otherwise remove entirely. This is the invariant the design note
# names: a term filter must not drop POPULATION rows, or the denominator
# quietly falls back to subjects-with-a-kept-term. Doing it wrong here is what
# made an earlier run of this probe report N = 59 for placebo instead of 86.
top <- names(sort(tapply(pop$USUBJID, pop$AEDECOD,
                         function(x) length(unique(x))), decreasing = TRUE))[1:12]
kept <- pop[is.na(pop$AEDECOD) | pop$AEDECOD %in% top, ]
lost <- setdiff(saf$USUBJID, kept$USUBJID)
pop <- rbind(kept, transform(saf[saf$USUBJID %in% lost, ],
                             AEDECOD = NA_character_, AESEV = NA_character_,
                             rank = NA_integer_))

# The events-only frame the board feeds the chart today, for comparison.
ev <- pop[!is.na(pop$AEDECOD), ]

cat(sprintf("safety subjects %d | in the frame %d | with a kept term %d | NA-term rows %d\n",
            nrow(saf), length(unique(pop$USUBJID)),
            length(unique(pop$USUBJID[!is.na(pop$AEDECOD)])),
            sum(is.na(pop$AEDECOD))))
stopifnot(length(unique(pop$USUBJID)) == nrow(saf))  # the invariant, asserted

board <- new_dock_board(
  blocks = c(
    d_ev  = new_static_block(ev,  block_name = "events only (today)"),
    d_pop = new_static_block(pop, block_name = "population carried as rows"),

    # 1. What the board does now: no population, so a percentage would divide
    #    by subjects-with-an-AE.
    p1 = new_chart_block(
      chart_type = "bar", orientation = "horizontal", bar_mode = "stacked",
      group = "AEDECOD", color = "AESEV", value = "USUBJID",
      func = "count_distinct", sort_by = "value", sort_dir = "desc",
      block_name = "1. counts, events-only frame"),

    # 2. Population rows in, na_group left at its default: the NA term becomes
    #    a nameless category. This is the behaviour na_group = "drop" fixes.
    p2 = new_chart_block(
      chart_type = "bar", orientation = "horizontal", bar_mode = "stacked",
      group = "AEDECOD", color = "AESEV", value = "USUBJID",
      func = "count_distinct", sort_by = "value", sort_dir = "desc",
      block_name = "2. population rows, na_group = level (nameless bar)"),

    # 3. Same, dropped from the categories. Identical bars to panel 1.
    p3 = new_chart_block(
      chart_type = "bar", orientation = "horizontal", bar_mode = "stacked",
      group = "AEDECOD", color = "AESEV", value = "USUBJID",
      func = "count_distinct", na_group = "drop",
      sort_by = "value", sort_dir = "desc",
      block_name = "3. population rows, na_group = drop"),

    # 4. The real thing: percent of the panel's subjects, faceted by arm so
    #    each panel divides by its own N, with the toggle on the card.
    p4 = new_chart_block(
      chart_type = "bar", orientation = "horizontal", bar_mode = "stacked",
      group = "AEDECOD", color = "AESEV", facet = "TRT01A",
      value = "USUBJID", func = "pct_distinct", na_group = "drop",
      func_toggle = TRUE, sort_by = "value", sort_dir = "desc",
      count_on = "facet", count_col = "USUBJID",
      block_name = "4. % of panel, faceted by arm, with the toggle")
  ),
  links = links(from = c("d_ev", "d_pop", "d_pop", "d_pop"),
                to   = c("p1", "p2", "p3", "p4")),
  views = list(
    p1 = dock_view("p1", name = "1. today"),
    p2 = dock_view("p2", name = "2. na_group level"),
    p3 = dock_view("p3", name = "3. na_group drop"),
    p4 = dock_view("p4", name = "4. % of panel")
  ),
  grids = list(p1 = dock_grid("p1"), p2 = dock_grid("p2"),
               p3 = dock_grid("p3"), p4 = dock_grid("p4")),
  active = "p4"
)

message("Serving the pct_distinct probe on http://127.0.0.1:", port, "/")
serve(board)
