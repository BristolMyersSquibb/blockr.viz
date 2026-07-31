# Builds a PowerPoint deck against the BMS house template with the same table
# on two consecutive slides: the positional split the exporter used before
# (col_widths = "even") and the measured one it uses now. Real ADaM data from
# bmsExampleData, in the shapes CDEx actually produces.
#
#   Rscript dev/pptx-layout/bms-preview.R
#
# Output: _scratch/pptx-preview/table-widths-bms.pptx

suppressMessages({
  pkgload::load_all(".", quiet = TRUE)
  library(bmsExampleData)
})

TEMPLATE <- "/workspace/blockr.topline/inst/templates/bms-template.pptx"
OUT <- "/workspace/_scratch/pptx-preview/table-widths-bms.pptx"
dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)

# Same resolution the download does: the template's usable width sizes the
# table and the template's theme font sets it, so what comes out is in the
# deck's own face at the deck's own width.
fit <- pptx_content_width(TEMPLATE)
font <- pptx_body_font(TEMPLATE)
options(blockr.viz.ft_fit_width = fit, blockr.viz.ft_font = font)
message("template: ", basename(TEMPLATE), "  width ", round(fit, 2),
        "in  font ", font)

doc <- pptx_strip_slides(officer::read_pptx(TEMPLATE))
sizes <- officer::slide_size(doc)

add_table_slide <- function(doc, x, title, mode, note = NULL) {
  ft <- static_table(x, title = "", col_widths = mode)
  dim <- flextable::flextable_dim(ft)
  doc <- officer::add_slide(doc, layout = "Title and Content",
                            master = "Office Theme")
  doc <- officer::ph_with(
    doc, sprintf("%s  [%s]", title, mode),
    location = officer::ph_location_type(type = "title")
  )
  doc <- officer::ph_with(
    doc, ft,
    location = officer::ph_location(
      left = max(0.25, (sizes$width - dim$widths) / 2), top = 1.1,
      width = dim$widths, height = dim$heights
    )
  )
  cat(sprintf("  %-34s %-8s %5.2fin x %5.2fin  [%s]\n", title, mode,
              dim$widths, dim$heights,
              paste(sprintf("%.2f", ft$body$colwidths), collapse = " ")))
  doc
}

pair <- function(doc, x, title) {
  doc <- add_table_slide(doc, x, title, "even")
  add_table_slide(doc, x, title, "measured")
}

# ---- 1. adverse events by SOC and preferred term --------------------------
# The shape that started this: a long stub, and two statistics per arm, so
# each arm's name sits over a pair of narrow columns. Trimmed to the SOCs and
# terms a deck slide would actually carry.
sentence <- function(x) {
  paste0(toupper(substring(x, 1L, 1L)), tolower(substring(x, 2L)))
}

# Arms plus the Total column a topline AE table carries, which is what takes
# the data side from six columns to eight.
adae$ARM <- adae$TRTA
tot <- adae
tot$ARM <- "Total"
adae2 <- rbind(adae, tot)

n_arm <- table(unique(adae2[, c("USUBJID", "ARM")])$ARM)
arms <- c(names(sort(n_arm[names(n_arm) != "Total"], decreasing = TRUE)),
          "Total")

ae_counts <- function(d, key) {
  subj <- unique(d[, c("USUBJID", "ARM", key)])
  list(n = table(subj[[key]], subj$ARM), ev = table(d[[key]], d$ARM))
}

# A term with no events in one arm has no cell in the table at all, so the
# lookup has to answer zero rather than fall over.
pick <- function(tab, rows, col) {
  if (!col %in% colnames(tab)) {
    return(rep(0L, length(rows)))
  }
  hit <- match(rows, rownames(tab))
  out <- rep(0L, length(rows))
  out[!is.na(hit)] <- as.integer(tab[hit[!is.na(hit)], col])
  out
}

ae_block <- function(d, key, labels, indent, strong) {
  cnt <- ae_counts(d, key)
  out <- data.frame(.label = sentence(labels), .indent = indent,
                    .strong = strong, check.names = FALSE)
  for (a in arms) {
    n <- pick(cnt$n, labels, a)
    ev <- pick(cnt$ev, labels, a)
    top <- sprintf("%s (N=%d)", a, n_arm[[a]])
    out[[paste0(top, "||n (%)")]] <- structure(
      sprintf("%d (%.1f%%)", n, 100 * n / n_arm[[a]]), label = "n (%)"
    )
    out[[paste0(top, "||Events")]] <- structure(as.character(ev),
                                                label = "Events")
  }
  out
}

top_soc <- names(sort(table(unique(adae[, c("USUBJID", "AEBODSYS")])$AEBODSYS),
                      decreasing = TRUE))[1:4]
ae_tbl <- do.call(rbind, lapply(top_soc, function(s) {
  d <- adae2[adae2$AEBODSYS == s, ]
  pts <- names(sort(table(unique(d[, c("USUBJID", "AEDECOD")])$AEDECOD),
                    decreasing = TRUE))[1:3]
  rbind(ae_block(d, "AEBODSYS", s, 0L, TRUE),
        ae_block(d[d$AEDECOD %in% pts, ], "AEDECOD", pts, 1L, FALSE))
}))
attr(ae_tbl$.label, "label") <- "System organ class / Preferred term"
attr(ae_tbl, "label") <-
  "Adverse events by system organ class and preferred term"
attr(ae_tbl, "subtitle") <- "Safety analysis set"
attr(ae_tbl, "caption") <- paste(
  "A subject is counted once per preferred term. Percentages use the number",
  "of subjects in the treatment arm."
)

# ---- 2. demographics ------------------------------------------------------
# Wide labels, mixed statistics, three arms. adsl in this dataset carries one
# arm, so the actual treatment comes from adae.
arm <- unique(adae[, c("USUBJID", "TRTA")])
arm <- arm[!duplicated(arm$USUBJID), ]
dm <- merge(adsl, arm, by = "USUBJID")
dm$TRTA[is.na(dm$TRTA)] <- "Placebo"
dm_tbl <- summary_table(
  dm, vars = c("AGE", "AGEGR1", "SEX", "RACE", "ETHNIC", "BMIBL", "WEIGHTBL"),
  by = "TRTA"
)
attr(dm_tbl, "label") <- "Demographics and baseline characteristics"
attr(dm_tbl, "subtitle") <- "Safety analysis set"

# ---- 3. a wide table ------------------------------------------------------
# Two-level by: arm over age group, so nine data columns under spanners.
wide_tbl <- summary_table(dm, vars = c("AGE", "BMIBL", "WEIGHTBL"),
                          by = c("TRTA", "SEX"))
attr(wide_tbl, "label") <- "Baseline measures by treatment arm and sex"

# ---- 4. a small table -----------------------------------------------------
# Four rows, two columns. Nothing here needs a 12.5in slide, and the measured
# rule stops stretching it across one.
small_tbl <- summary_table(dm, vars = "AGEGR1", by = "SEX")
attr(small_tbl, "label") <- "Age group by sex"

# ---- 5. long free text ----------------------------------------------------
# A listing-style stub, where the label is a sentence rather than a term.
long_tbl <- data.frame(
  .label = c(
    "Subjects with at least one treatment emergent adverse event",
    "Subjects with at least one serious treatment emergent adverse event",
    "Subjects with an adverse event leading to study drug discontinuation",
    "Subjects with an adverse event leading to death"
  ),
  .indent = 0L,
  Placebo = c("221 (73.4%)", "31 (10.3%)", "18 (6.0%)", "2 (0.7%)"),
  `Xanomeline Low Dose` = c("264 (86.6%)", "44 (14.4%)", "37 (12.1%)",
                            "3 (1.0%)"),
  `Xanomeline High Dose` = c("281 (91.8%)", "52 (17.0%)", "61 (19.9%)",
                             "5 (1.6%)"),
  check.names = FALSE
)
attr(long_tbl, "label") <- "Overall summary of treatment emergent adverse events"
attr(long_tbl, "caption") <- "Treatment emergent is defined as on or after the first dose."

TABLES <- list(
  list(ae_tbl, "Adverse events by SOC and PT",
       "Long stub, two statistics per arm. The shape the complaint came from."),
  list(dm_tbl, "Demographics",
       "The stub is 3.9in of text handed 5.65in by the old rule."),
  list(wide_tbl, "Wide: arm over sex",
       "Two-level spanners over six data columns."),
  list(small_tbl, "Small: age group by sex",
       "Four rows. Nothing here needs a 12.5in slide."),
  list(long_tbl, "Long free-text stub",
       "A stub that needs MORE than the old half-slide cap.")
)

for (x in TABLES) doc <- pair(doc, x[[1L]], x[[2L]])

print(doc, target = OUT)
message("\nwrote ", OUT, " (", length(doc), " slides)")

# ---- the same slides in a browser -----------------------------------------
# The deck is the thing to check in PowerPoint; this is for looking at both
# versions side by side without opening it.
source("dev/pptx-layout/slide-html.R")

page <- tagList(
  tags$head(tags$meta(charset = "utf-8"),
            tags$title("BMS template: measured column widths"),
            tags$style(HTML(slide_css)), tags$script(HTML(slide_js))),
  tags$main(
    tags$h1("Measured column widths, BMS template"),
    tags$p(class = "lede", sprintf(paste(
      "Real ADaM tables from bmsExampleData, sized against %s (%.2fin of",
      "usable width, %s). Each pair is the same table under the old",
      "positional split and the new measured one. The deck itself is at",
      "_scratch/pptx-preview/table-widths-bms.pptx."
    ), basename(TEMPLATE), fit, font)),
    lapply(TABLES, function(x) {
      tags$section(
        tags$h2(x[[2L]]),
        tags$p(x[[3L]]),
        tags$h3("even (before)"),
        slide(static_table(x[[1L]], title = "", col_widths = "even"),
              title = attr(x[[1L]], "label") %||% x[[2L]]),
        tags$h3("measured (now)"),
        slide(static_table(x[[1L]], title = "", col_widths = "measured"),
              title = attr(x[[1L]], "label") %||% x[[2L]])
      )
    })
  )
)

# ---- too many columns -----------------------------------------------------
# The shape that broke in production: six arms times six toxicity grades. At
# 36 columns the slide runs out, and before the headers could turn on their
# side each column ended up narrower than a character, so PowerPoint stacked
# the letters one per line.
GRADE <- "/workspace/_scratch/pptx-preview/grade-table-bms.pptx"

all_soc <- names(sort(table(unique(adae2[, c("USUBJID", "AEBODSYS")])$AEBODSYS),
                      decreasing = TRUE))

grade_arms <- paste0(c("Placebo", "300mg", "600mg", "900mg", "1500mg",
                       "1200mg"), " (N=20)")
grade_stats <- c("Any Grade\nN=20", paste0("Grade ", 1:5, "\nN=20"))
n_grade <- 120
grade_tbl <- data.frame(
  .label = sentence(rep(unique(adae2$AEDECOD), length.out = n_grade)),
  .indent = 0L,
  .group1 = "System organ class",
  .group1_level = rep(sentence(all_soc[1:6]), each = n_grade / 6),
  check.names = FALSE
)
set.seed(7)
for (a in grade_arms) {
  for (s in grade_stats) {
    grade_tbl[[paste0(a, "||", s)]] <-
      structure(as.character(rpois(n_grade, 2)), label = s)
  }
}
attr(grade_tbl$.label, "label") <- "Dictionary derived term"
attr(grade_tbl, "subtitle") <- "Safety Population"
GRADE_TITLE <- paste(
  "Number of Subjects with Treatment-Emergent Adverse Events by highest",
  "Standard Toxicity Grade, System Organ Class, and Dictionary Derived Term"
)

write_exhibit_pptx(grade_tbl, GRADE, title = GRADE_TITLE, template = TEMPLATE)
message("wrote ", GRADE, " (", length(officer::read_pptx(GRADE)),
        " slides, ", n_grade, " rows x ", length(grade_arms) *
          length(grade_stats), " columns)")

page <- tagAppendChild(page, tags$main(
  tags$section(
    tags$h2("36 columns"),
    tags$p(paste(
      "Six arms times six toxicity grades. Flat, the word \"Grade\" alone",
      "needs 23.5in across 36 columns, so every column is squeezed below the",
      "width of a capital letter and the header comes out one character per",
      "line. Turned on its side the header asks for no width at all, and the",
      "columns are sized by their counts instead."
    )),
    tags$p(class = "q", paste(
      "Read the shape here, not the width badge. A browser refuses to draw a",
      "table narrower than its content, so both of these measure wider than",
      "they are; PowerPoint honours the stated widths exactly, and the",
      "written deck is 12.53in either way."
    )),
    tags$h3("header_rotate = \"none\" (what production did)"),
    slide(static_table(grade_tbl[1:12, ], title = "", fit_width = fit,
                       header_rotate = "none"), GRADE_TITLE),
    tags$h3("header_rotate = \"auto\" (now)"),
    slide(static_table(grade_tbl[1:12, ], title = "", fit_width = fit),
          GRADE_TITLE)
  )
))

# ---- the long one ---------------------------------------------------------
# Every SOC and every preferred term, which is what a real AE table looks like
# and what no slide can hold. Written through write_exhibit_pptx() itself, so
# this is the download path end to end: shrink first, split second, repeat the
# header band, the footnote and the section heading on every page.
PAGED <- "/workspace/_scratch/pptx-preview/table-widths-bms-paged.pptx"

# Sections, so the heading is the thing that repeats: the SOC drives the
# grouping instead of being a row of the stub.
long_ae <- do.call(rbind, lapply(all_soc, function(s) {
  d <- adae2[adae2$AEBODSYS == s, ]
  pts <- names(sort(table(unique(d[, c("USUBJID", "AEDECOD")])$AEDECOD),
                    decreasing = TRUE))
  b <- ae_block(d, "AEDECOD", pts, 0L, FALSE)
  b$.group1 <- "System organ class"
  b$.group1_level <- sentence(s)
  b
}))
attr(long_ae$.label, "label") <- "Preferred term"
attr(long_ae, "subtitle") <- "Safety analysis set"
attr(long_ae, "caption") <- attr(ae_tbl, "caption")

write_exhibit_pptx(
  long_ae, PAGED,
  title = "Adverse events by system organ class and preferred term",
  template = TEMPLATE
)
n_slides <- length(officer::read_pptx(PAGED))
message("wrote ", PAGED, " (", n_slides, " slides, ", nrow(long_ae), " rows)")

# The first pages of it in the browser, planned exactly the way the writer
# plans them, so the continuation headings can be read without PowerPoint.
lx <- fmt_to_wide(as_annotated_df(long_ae))
lft <- static_table(lx, title = "")
lbreaks <- pptx_hold_sections(pptx_page_breaks(lft, 7.5 - 1.1 - 0.4),
                              pptx_section_key(lx))
lfrom <- c(1L, utils::head(lbreaks, -1L) + 1L)
lkey <- pptx_section_key(lx)

paged_slides <- lapply(1:3, function(i) {
  rows <- lfrom[[i]]:lbreaks[[i]]
  cont <- lfrom[[i]] > 1L &&
    identical(lkey[[lfrom[[i]]]], lkey[[lfrom[[i]] - 1L]])
  ft <- static_table(pptx_slice_rows(lx, rows), title = "",
                     col_widths = lft$body$colwidths, continued = cont)
  slide(ft, sprintf("%s (%d of %d)", attr(long_ae, "label") %||%
                      "Adverse events by system organ class and preferred term",
                    i, length(lbreaks)))
})

page <- tagAppendChild(page, tags$main(
  tags$section(
    tags$h2("Pagination: the first three of ", length(lbreaks), " slides"),
    tags$p(sprintf(paste(
      "Every SOC and every preferred term, %d rows. No font step saves this,",
      "so it splits. Each slide repeats the title with its page number, the",
      "spanner band, the leaf header row and the footnote, and a section",
      "carried across a break repeats its heading marked (continued)."
    ), nrow(long_ae))),
    paged_slides
  )
))

save_html(page, "dev/pptx-layout/bms.html")
message("wrote dev/pptx-layout/bms.html")
