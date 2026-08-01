# How readily a wide table comes apart sideways.
#
# The exporter would deal a table's columns over two sets of slides as soon as
# any header broke inside a word, which put tables on a left half and a right
# half that a reader has to hold together in their head. It now wraps the row
# stub first and tolerates a header losing a syllable, so the same table is
# carried DOWN over slides instead -- which is the split a reader follows.
#
#   Rscript dev/pptx-layout/split-tolerance.R
#
# Prints the width verdicts behind the decision, and writes a deck with the
# same table dealt over two sets (what it used to do) and kept whole (what it
# does now), so the two can be looked at side by side.
#
# Output: _scratch/pptx-preview/table-split-tolerance.pptx

suppressMessages({
  pkgload::load_all(".", quiet = TRUE)
  library(bmsExampleData)
})

TEMPLATE <- "/workspace/blockr.topline/inst/templates/bms-template.pptx"
OUT <- "/workspace/_scratch/pptx-preview/table-split-tolerance.pptx"
dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)

options(blockr.viz.ft_fit_width = pptx_content_width(TEMPLATE),
        blockr.viz.ft_font = pptx_body_font(TEMPLATE))

sentence <- function(x) {
  paste0(toupper(substring(x, 1L, 1L)), tolower(substring(x, 2L)))
}

subj <- unique(adae[, c("USUBJID", "TRTA")])
n_arm <- table(subj$TRTA)
arm1 <- names(sort(n_arm, decreasing = TRUE))[[1L]]

# Real adverse events -- the four system organ classes with the most subjects,
# three preferred terms under each -- repeated into as many cohort columns as
# the shape asks for. The counts step down by cohort so the columns are
# telling apart at a glance.
ae_table <- function(n_col, stats) {

  socs <- names(sort(table(unique(adae[, c("USUBJID", "AEBODSYS")])$AEBODSYS),
                     decreasing = TRUE))[1:4]
  rows <- do.call(rbind, lapply(socs, function(s) {
    d <- adae[adae$AEBODSYS == s, ]
    pts <- names(sort(table(unique(d[, c("USUBJID", "AEDECOD")])$AEDECOD),
                      decreasing = TRUE))[1:3]
    data.frame(.label = sentence(c(s, pts)), .indent = c(0L, rep(1L, 3L)),
               .strong = c(TRUE, rep(FALSE, 3L)), key = c(s, pts),
               level = c("AEBODSYS", rep("AEDECOD", 3L)),
               stringsAsFactors = FALSE)
  }))

  out <- rows[c(".label", ".indent", ".strong")]
  n <- integer(nrow(rows))
  ev <- integer(nrow(rows))
  for (lvl in unique(rows$level)) {
    at <- rows$level == lvl
    s <- unique(adae[adae$TRTA == arm1, c("USUBJID", lvl)])
    n[at] <- as.integer(table(factor(s[[lvl]], levels = rows$key[at])))
    ev[at] <- as.integer(table(factor(adae[adae$TRTA == arm1, lvl],
                                      levels = rows$key[at])))
  }

  for (i in seq_len(n_col)) {
    top <- sprintf("Cohort %s (N=%d)", LETTERS[[i]], n_arm[[arm1]])
    ni <- pmax(0L, n - (i - 1L))
    for (st in stats) {
      v <- switch(
        st,
        "n (%)" = sprintf("%d (%.1f%%)", ni, 100 * ni / n_arm[[arm1]]),
        "Events" = as.character(ev),
        "Grade >= 3" = sprintf("%d (%.1f%%)", ni %/% 3L,
                               100 * (ni %/% 3L) / n_arm[[arm1]])
      )
      out[[paste0(top, "||", st)]] <- structure(v, label = st)
    }
  }

  attr(out$.label, "label") <- "System organ class / Preferred term"
  attr(out, "label") <- "Adverse events by system organ class and preferred term"
  out
}

shapes <- list(
  list(n = 8L, stats = c("n (%)", "Events"),
       title = "Eight cohorts, count and events"),
  list(n = 9L, stats = c("n (%)", "Events"),
       title = "Nine cohorts, count and events"),
  list(n = 6L, stats = c("n (%)", "Events", "Grade >= 3"),
       title = "Six cohorts, three statistics")
)

# What the width pass decided, at every font the exporter may use. `broken` is
# the one that deals the columns over slides: cells that do not fit, or a
# header word cut past blockr.viz.ft_header_break_tol.
for (sh in shapes) {
  x <- ae_table(sh$n, sh$stats)
  cat(sprintf("\n%s (%d data columns)\n", sh$title, sh$n * length(sh$stats)))
  for (sz in 13:11) {
    ft <- static_table(x, title = "Adverse events", font_size = sz)
    w <- attr(ft, "layout_plan")$col_widths
    cat(sprintf(
      "  %2dpt: stub %.2fin, narrowest column %.2fin, word_fit %.2f -> %s\n",
      sz, w[[1L]], min(w[-1L]), attr(ft, "word_fit"),
      if (pptx_width_broken(ft)) "deal the columns" else "keep it whole"
    ))
  }
}

doc <- pptx_strip_slides(officer::read_pptx(TEMPLATE))

for (sh in shapes) {
  x <- ae_table(sh$n, sh$stats)
  n_data <- sh$n * length(sh$stats)
  for (mode in c("dealt over two sets", "kept whole")) {
    was <- length(doc)
    doc <- suppressMessages(suppressWarnings(pptx_add_exhibit(
      doc, x, template = TEMPLATE,
      title = sprintf("%s [%s]", sh$title, mode),
      # Forcing half the columns onto a slide is what the exporter used to do
      # on its own; the default is what it does now.
      max_cols = if (identical(mode, "kept whole")) "auto" else n_data %/% 2L
    )))
    cat(sprintf("%-32s %-20s %2d columns -> %d slides\n", sh$title, mode,
                n_data, length(doc) - was))
  }
}

print(doc, target = OUT)
cat("wrote ", OUT, "\n", sep = "")
