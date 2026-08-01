# Demo: what a summarize table looks like painted as ONE picture for pptx.
#
#   Rscript blockr.viz/dev/pptx-summarize/demo.R
#
# Writes, into _scratch/pptx-summarize/:
#   *.png   the painted exhibit, one per table shape
#   *.html  the SAME table as the block draws it, for the side-by-side
#   summarize-tables.pptx  the pictures placed on real slides
#
# The painter is package code (R/rank-paint.R, R/summarize-exhibit.R); this
# script is the eyeball harness for it: real ADaM shapes, the slide's own
# geometry, and the browser rendering of each table beside its painted twin.

.self <- grep("^--file=", commandArgs(FALSE), value = TRUE)[1]
.ws <- normalizePath(if (is.na(.self)) "." else
  file.path(dirname(sub("^--file=", "", .self)), "..", "..", ".."))

pkgload::load_all(file.path(.ws, "blockr.viz"), quiet = TRUE)

# The painter is package code now (R/rank-paint.R, R/summarize-exhibit.R);
# this script drives the SAME entry points a deck does. The two internals it
# reaches for are the page list and the png writer, which the pptx method
# calls itself -- the preview needs the pages as files.
paint_pages <- blockr.viz:::rank_paint_pages
write_png <- blockr.viz:::rp_write_png

out <- file.path(.ws, "_scratch", "pptx-summarize")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# --- data ----------------------------------------------------------------
adae <- pharmaverseadam::adae
adae <- adae[!is.na(adae$AEBODSYS) & !is.na(adae$AEDECOD), ]

# The swimlane's subjects: the ones carrying the most events, which is what
# "problematic" means on a safety review slide. Spans need a real start and
# end day.
ae_sp <- as.data.frame(adae[!is.na(adae$ASTDY) & !is.na(adae$AENDY), ])
ae_sp$DUR <- ae_sp$AENDY - ae_sp$ASTDY + 1
worst <- names(sort(table(ae_sp$USUBJID), decreasing = TRUE))[1:14]
ae_sp <- ae_sp[ae_sp$USUBJID %in% worst, ]
# Subject ids are study-site-number; the site prefix is the same for all and
# eats a third of the stub.
ae_sp$SUBJ <- sub("^.*-", "", ae_sp$USUBJID)

adlb <- pharmaverseadam::adlb
adlb <- adlb[grepl("^(Baseline|Week )", adlb$AVISIT), ]
.ord <- unique(adlb[order(adlb$AVISITN), c("AVISIT", "AVISITN")])
adlb$AVISIT <- factor(adlb$AVISIT, levels = .ord$AVISIT)
adlb_pb <- adlb[adlb$AVISIT != "Baseline" & !is.na(adlb$PCHG), ]
adlb_pb$AVISIT <- droplevels(adlb_pb$AVISIT)
adlb_pb$PCHG_C <- pmin(pmax(adlb_pb$PCHG, -50), 50)
# keep the demo to one slide's worth of rows
keep <- names(sort(table(adlb_pb$PARAM), decreasing = TRUE))[1:14]
adlb_pb <- adlb_pb[adlb_pb$PARAM %in% keep, ]
adlb_pb$AVISIT <- droplevels(adlb_pb$AVISIT)
adlb_pb <- adlb_pb[adlb_pb$AVISIT %in% levels(adlb_pb$AVISIT)[1:4], ]
adlb_pb$AVISIT <- droplevels(adlb_pb$AVISIT)

# --- the tables ----------------------------------------------------------
# Same argument lists a summarize table block would carry.
specs <- list(
  ae_arm = list(
    label = "AE incidence by system organ class, count bar per arm",
    args = list(
      data = adae, by = "AEBODSYS",
      summaries = list(
        list(type = "simple", func = "count_distinct", col = "USUBJID",
             show = "number", name = "Subjects"),
        list(type = "simple", func = "count_distinct", col = "USUBJID",
             show = "bar", facet = "TRT01A", name = "Subjects with an event")
      ),
      sort_by = "value", sort_dir = "desc", top_n = 12
    ),
    title = "Adverse events by system organ class",
    subtitle = "subjects with at least one event, by treatment arm"
  ),
  lab_box = list(
    label = "Lab parameters, box distribution faceted by visit",
    args = list(
      data = adlb_pb, by = "PARAM",
      summaries = list(
        list(type = "simple", func = "count_distinct", col = "USUBJID",
             show = "number", name = "Subjects"),
        list(type = "dist", col = "PCHG_C", style = "box",
             inner = "median_q1_q3", outer = "p10_p90", facet = "AVISIT",
             name = "% change from baseline")
      ),
      sort_by = "label", sort_dir = "asc"
    ),
    title = "Laboratory parameters",
    subtitle = "% change from baseline by visit, clamped to +/-50%"
  ),
  swimlane = list(
    label = "Problematic patients: AE episodes as a swimlane, by severity",
    args = list(
      data = ae_sp, by = "SUBJ",
      summaries = list(
        list(type = "simple", func = "count", show = "number",
             name = "Events"),
        list(type = "dist", col = "DUR", style = "pointrange",
             inner = "median_q1_q3", name = "Duration (days)"),
        list(type = "spans", name = "Episodes", x = "ASTDY", xend = "AENDY",
             color = "AESEV", label = "AEDECOD", size = "lg")
      ),
      sort_by = "value", sort_dir = "desc"
    ),
    title = "Subjects with the most adverse events",
    subtitle = "one lane per subject, each segment an episode, coloured by severity"
  ),
  trajectory = list(
    label = "Sparkline trajectory per subject",
    args = list(
      data = ae_sp, by = "SUBJ",
      summaries = list(
        list(type = "simple", func = "count", show = "bar", name = "Events"),
        list(type = "series", name = "Episode duration over time",
             x = "ASTDY", col = "DUR", ref = "mean_sd")
      ),
      sort_by = "value", sort_dir = "desc"
    ),
    title = "Episode duration over study day",
    subtitle = "sparkline per subject, dashed line and band = pooled mean +/- SD"
  ),
  ae_nested = list(
    label = "AE table nested SOC / preferred term: the long one that pages",
    args = list(
      data = adae, by = c("AEBODSYS", "AEDECOD"),
      summaries = list(
        list(type = "simple", func = "count_distinct", col = "USUBJID",
             show = "number", name = "Subjects"),
        list(type = "simple", func = "count_distinct", col = "USUBJID",
             show = "bar", facet = "TRT01A", name = "Subjects with an event")
      ),
      sort_by = "value", sort_dir = "desc"
    ),
    title = "Adverse events by system organ class and preferred term",
    subtitle = "subjects with at least one event, by treatment arm"
  ),
  lab_pr = list(
    label = "Lab parameters, dot range (pointrange), pooled",
    args = list(
      data = adlb_pb, by = "PARAM",
      summaries = list(
        list(type = "simple", func = "count_distinct", col = "USUBJID",
             show = "number", name = "Subjects"),
        list(type = "dist", col = "PCHG_C", style = "pointrange",
             inner = "median_q1_q3", outer = "p10_p90",
             name = "% change from baseline"),
        list(type = "simple", func = "mean", col = "PCHG_C", show = "number",
             name = "Mean")
      ),
      sort_by = "label", sort_dir = "asc"
    ),
    title = "Laboratory parameters",
    subtitle = "pooled post-baseline % change, median with Q1-Q3 and P10-P90"
  )
)

FIT_WIDTH <- 12.53   # the BMS template's usable slide width
BODY_H <- 5.4        # what is left under the title placeholder
pngs <- list()

for (nm in names(specs)) {
  s <- specs[[nm]]
  cat("\n== ", s$label, "\n", sep = "")

  ex <- do.call(static_summarize_table, c(
    list(s$args$data), s$args[setdiff(names(s$args), "data")],
    list(title = s$title, subtitle = s$subtitle,
         caption = "Source: pharmaverseadam, demo data")))
  m <- ex$cells
  cat("   rows: ", m$n, "  cols: ", length(m$cols),
      "  kinds: ", paste(unique(vapply(m$cols, function(c) c$kind, "")),
                         collapse = "/"), "\n", sep = "")

  # BODY_H is the slide's body box: what the picture has to fit into, and so
  # what decides where the rows are cut.
  pg <- paint_pages(m, ex$prep, width_in = FIT_WIDTH, max_height = BODY_H,
                    fs = 10, title = ex$title, subtitle = ex$subtitle,
                    caption = ex$caption)
  ps <- lapply(seq_along(pg), function(k) {
    f <- file.path(out, if (k == 1L) paste0(nm, ".png")
                   else paste0(nm, "-", k, ".png"))
    write_png(pg[[k]], f, res = 300)
  })
  cat("   painted: ", length(ps), if (length(ps) == 1) " page" else " pages",
      "  ", round(ps[[1]]$width, 2), " x ", round(ps[[1]]$height, 2), " in\n",
      sep = "")
  for (k in seq_along(ps)) {
    pngs[[paste0(nm, "-", k)]] <- c(ps[[k]], list(title = s$title))
  }

  # the on-screen twin, for the side-by-side: the exhibit's OWN html
  # renderer, which is what an HTML deck or a download would show.
  tbl <- html_exhibit(ex)
  htmltools::save_html(
    htmltools::tagList(
      htmltools::tags$style("body{font-family:system-ui,sans-serif;padding:16px;background:#fff}"),
      htmltools::div(class = "blockr-rank-container", tbl)
    ),
    file.path(out, paste0(nm, ".html")),
    libdir = "lib"
  )
}

# --- the deck ------------------------------------------------------------
tpl <- file.path(.ws, "blockr.topline", "inst", "templates", "bms-template.pptx")
doc <- if (file.exists(tpl)) {
  d <- officer::read_pptx(tpl)
  while (length(d) > 0) d <- officer::remove_slide(d, 1)
  d
} else {
  officer::read_pptx()
}
layout <- if (file.exists(tpl)) "Title and Content" else "Title and Content"
master <- officer::layout_summary(doc)$master[
  match(layout, officer::layout_summary(doc)$layout)]

for (nm in names(pngs)) {
  p <- pngs[[nm]]
  doc <- officer::add_slide(doc, layout = layout, master = master)
  doc <- tryCatch(
    officer::ph_with(doc, p$title,
                     location = officer::ph_location_type(type = "title")),
    error = function(e) doc)
  # scale to fit the body box, never up
  max_w <- FIT_WIDTH
  max_h <- BODY_H
  sc <- min(1, max_w / p$width, max_h / p$height)
  # Scaling down is a LEGIBILITY loss, not a layout detail: the painted type
  # was sized for the slide. Say it rather than silently shrinking -- this is
  # the row budget the paging step will have to respect.
  if (sc < 1) {
    cat("  ! ", nm, ": ", round(p$height, 2), "in tall, scaled to ",
        round(sc * 100), "% to fit the body box (", max_h,
        "in). It needs paging, not shrinking.\n", sep = "")
  }
  doc <- officer::ph_with(
    doc, officer::external_img(p$file, width = p$width * sc,
                               height = p$height * sc),
    location = officer::ph_location(
      left = (13.333 - p$width * sc) / 2, top = 1.45,
      width = p$width * sc, height = p$height * sc))
}
deck <- file.path(out, "summarize-tables.pptx")
print(doc, target = deck)
cat("\n  deck: ", deck, "\n", sep = "")

# --- the preview page ----------------------------------------------------
# Generated rather than hand-written, so a table that gained a page shows all
# of them.
esc <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  gsub("<", "&lt;", s, fixed = TRUE)
}
sect <- vapply(names(specs), function(nm) {
  s <- specs[[nm]]
  pg <- pngs[grepl(paste0("^", nm, "-[0-9]+$"), names(pngs))]
  imgs <- paste0(vapply(seq_along(pg), function(k) {
    p <- pg[[k]]
    paste0('<span class="tag">painted',
           if (length(pg) > 1) paste0(", page ", k, " of ", length(pg)) else "",
           " &middot; ", round(p$width, 2), " x ", round(p$height, 2),
           'in</span><img src="', basename(p$file), '">')
  }, character(1L)), collapse = "\n")
  paste0(
    '<section><h2>', esc(s$title), '</h2>\n<p class="note">', esc(s$label),
    '</p>\n', imgs,
    '\n<span class="tag">browser</span><img src="', nm, '-html.png">',
    '\n<p class="note"><a href="', nm, '.html">open the interactive table</a>',
    '</p></section>')
}, character(1L))

writeLines(c(
  '<!doctype html><meta charset="utf-8">',
  '<title>Summarize table, painted for pptx</title>',
  '<style>',
  ' body{font-family:system-ui,sans-serif;margin:0 auto;max-width:1500px;',
  '      padding:24px;color:#1c1b18;background:#fafaf8}',
  ' h1{font-size:20px;margin:0 0 4px} h2{font-size:15px;margin:0 0 2px}',
  ' p.lede{color:#6f6d66;margin:0 0 24px}',
  ' section{background:#fff;border:1px solid #e1e0d9;border-radius:6px;',
  '         padding:16px;margin-bottom:20px}',
  ' .note{color:#6f6d66;font-size:13px;margin:0 0 12px}',
  ' .tag{display:inline-block;font-size:11px;letter-spacing:.04em;',
  '      text-transform:uppercase;color:#6f6d66;margin:10px 0 4px}',
  ' img{width:100%;display:block;border:1px solid #eeeeea}',
  ' a{color:#2a78d6}',
  '</style>',
  '<h1>Summarize table painted as one picture for PowerPoint</h1>',
  paste0('<p class="lede">Painted exhibit first (what lands on the slide),',
         ' then the same table drawn by <code>rank_table()</code> in a',
         ' browser. Both read the same <code>rank_cells()</code> output.',
         ' Slide box: ', FIT_WIDTH, ' x ', BODY_H, 'in. Deck: ',
         '<a href="summarize-tables.pptx">summarize-tables.pptx</a>',
         ' (', length(pngs), ' slides).</p>'),
  sect
), file.path(out, "index.html"))
cat("  page: http://127.0.0.1:3838/\n")
