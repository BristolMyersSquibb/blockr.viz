# Renders dev/pptx-layout/study.html: the same AE table under every layout
# idea on the table, each one a real flextable at real slide scale, so the
# widths and wrap points in the page are the ones PowerPoint would get.
#
#   Rscript dev/pptx-layout/study.R
#
# The flextables are built by the shipping static_table(); the variants are
# applied on top of it, which is deliberate. Everything a variant does here is
# something static_table() could do itself given one more argument.

suppressMessages(pkgload::load_all(".", quiet = TRUE, export_all = TRUE))
library(htmltools)
source("dev/pptx-layout/slide-html.R")
source("dev/pptx-layout/demo-table.R")
source("dev/pptx-layout/proto-widths.R")

TITLE <- "Adverse events by system organ class and preferred term"
W <- 12.53   # usable content width of blockr.outline's widescreen template
FONT <- "Arial"

# ---- pull the pieces static_table() measures ------------------------------
# The same derivation the renderer does internally, so the prototype measures
# exactly the cells that end up on the slide.
table_parts <- function(d, na_rep = "—") {
  d <- fmt_to_wide(as_annotated_df(d))
  view <- annotated_structure_view(d)
  df <- view$data
  stub <- if (".label" %in% names(df)) ".label" else names(df)[1]
  cols <- setdiff(names(df), stub)
  cols <- cols[!startsWith(cols, ".")]

  parts <- strsplit(cols, "||", fixed = TRUE)
  top <- vapply(parts, function(p) if (length(p) > 1L) p[[1L]] else "",
                character(1))
  leaf <- vapply(cols, function(cn) {
    lbl <- attr(df[[cn]], "label")
    if (is.null(lbl) || !nzchar(lbl)) utils::tail(parts[[match(cn, cols)]], 1L)
    else lbl
  }, character(1))

  body <- cbind(as.character(df[[stub]]),
                vapply(cols, function(cn) {
                  v <- as.character(df[[cn]])
                  v[is.na(v)] <- na_rep
                  v
                }, character(nrow(df))))

  stub_lbl <- attr(df[[stub]], "label") %||% ""
  list(body = body, stub_label = stub_lbl, leaf = leaf, top = top,
       n_row = nrow(df))
}

# ---- variant helpers ------------------------------------------------------
with_widths <- function(ft, w) {
  for (j in seq_along(w)) ft <- flextable::width(ft, j = j, width = w[[j]])
  ft
}

# static_table() states the row height through flextable::height_all(), which
# PowerPoint honours and HTML ignores -- so a browser mock of the deck shows
# half again as many rows as the slide would. Restating the same height as
# vertical padding makes the two media agree, and is the better instruction
# anyway: a fixed height clips a wrapped row, padding grows with it.
row_pad <- function(ft, h, size, part = "body") {
  line <- size * 1.2 / 72
  pad <- max(0, (h - line) / 2) * 72
  flextable::padding(ft, padding.top = pad, padding.bottom = pad, part = part)
}

row_h_for <- function(size, dense = FALSE) {
  (if (dense) 0.24 else 0.3) * (size / 14)
}

tighten <- function(ft, pad = 2) {
  n <- length(ft$body$col_keys)
  ft |>
    flextable::padding(j = 2:n, padding.left = pad, padding.right = pad,
                       part = "all") |>
    flextable::padding(j = 1, padding.right = pad, part = "all")
}

# Rules instead of a full grid: a rule under the header band, one above each
# section, a closing rule, and nothing between the columns.
rules_only <- function(ft, section_rows = integer(0)) {
  thin <- flextable::fp_border_default(color = "#AEB7C2", width = 0.5)
  thick <- flextable::fp_border_default(color = "#333333", width = 1)
  ft <- ft |>
    flextable::border_remove() |>
    flextable::hline_top(border = thick, part = "header") |>
    flextable::hline_bottom(border = thick, part = "header") |>
    flextable::hline_bottom(border = thick, part = "body")
  sec <- setdiff(section_rows, 1L) - 1L
  if (length(sec)) {
    ft <- flextable::hline(ft, i = sec, border = thin, part = "body")
  }
  ft
}

zebra <- function(ft, rows, bg = "#EBEFF4") {
  if (!length(rows)) return(ft)
  flextable::bg(ft, i = rows, bg = bg, part = "body")
}

# ---- the variants ---------------------------------------------------------
flat <- demo_ae_table(6)
secd <- demo_ae_table(6, sections = TRUE)

measured <- function(d, size = 14, pad = 0.07, dense = FALSE, ...) {
  p <- table_parts(d)
  m <- measure_cols(p$body, p$stub_label, p$leaf, p$top, FONT, size, pad)
  w <- allocate_widths(m, W)
  ft <- static_table(d, title = "", font_size = size, ...)
  if (pad < 0.07) ft <- tighten(ft, pad * 72)
  ft <- row_pad(with_widths(ft, w), row_h_for(size, dense), size, "all")
  list(parts = p, m = m, w = w, size = size, row_h = row_h_for(size, dense),
       ft = ft)
}

# The baseline this study argued against, pinned explicitly now that measured
# widths are what static_table() does by default.
v_current <- row_pad(
  static_table(flat, title = "", fit_width = W, font_size = 14,
               col_widths = "even"),
  0.3, 14, "all"
)
v_meas <- measured(flat)
v_meas12 <- measured(flat, size = 12)
v_tight <- measured(flat, size = 12, pad = 0.03, dense = TRUE)
v_rules <- local({
  soc <- which(flat$.strong)
  ft <- rules_only(zebra(v_tight$ft, soc), soc)
  flextable::padding(ft, j = 1, i = soc, padding.left = 3, part = "body")
})
v_sec <- measured(secd, size = 12, pad = 0.03, dense = TRUE)

# ---- pagination -----------------------------------------------------------
# The height budget: 7.5in slide, 1.4in for the title band, 0.45in bottom
# margin, and what the header / footer of the table itself take.
# A longer table than fits at any size, so the split is the only answer.
long <- demo_ae_table(6)
row_h <- v_tight$row_h
per <- rows_per_slide(top = 1.1, row_h = row_h,
                      head_h = 3 * row_h + 0.2, foot_h = 0.5)
starts <- long$.strong
cuts <- break_rows(nrow(long), per, starts)
pages <- Map(function(a, b) long[a:b, , drop = FALSE],
             c(1L, utils::head(cuts, -1L) + 1L), cuts)

page_ft <- function(i) {
  d <- pages[[i]]
  for (nm in setdiff(names(attributes(long)),
                     c("names", "row.names", "class"))) {
    attr(d, nm) <- attr(long, nm)
  }
  # Slide 2..n repeat the header band and the footnote and say which page they
  # are; each slide has to stand on its own when it is pulled out of the deck.
  sub <- if (i == 1L) "Safety analysis set" else
    sprintf("Safety analysis set (continued, %d of %d)", i, length(pages))
  ft <- tighten(static_table(d, title = "", subtitle = sub, font_size = 12),
                0.03 * 72)
  # Widths measured on the WHOLE table, not on the page, so the columns line
  # up when you flip between slides.
  row_pad(with_widths(ft, v_tight$w), row_h, 12, "all")
}
paged <- lapply(seq_along(pages), page_ft)

# ---- page -----------------------------------------------------------------
ft_html <- function(ft) flextable::htmltools_value(ft)

wtable <- function(labels, w, caption = NULL) {
  tags$table(class = "wtbl",
    tags$thead(tags$tr(lapply(c("column", "inches"), tags$th))),
    tags$tbody(Map(function(l, x) tags$tr(tags$td(l),
                                          tags$td(sprintf("%.2f", x))),
                   labels, w)),
    if (!is.null(caption)) tags$caption(caption)
  )
}

sec <- function(n, title, ...) {
  tags$section(tags$h2(tags$span(class = "num", n), title), ...)
}

meas_now <- measure_cols(table_parts(flat)$body, table_parts(flat)$stub_label,
                         table_parts(flat)$leaf, table_parts(flat)$top,
                         FONT, 14, 0.07)
cur_w <- c(min(5.65, W * .5), rep((W - min(5.65, W * .5)) / 8, 8))

page <- tagList(
  tags$head(tags$meta(charset = "utf-8"),
            tags$title("pptx table layout study"), tags$style(HTML(slide_css)),
    tags$script(HTML(slide_js))),
  tags$main(
    tags$h1("Making the PowerPoint table fit"),
    tags$p(class = "knob", tags$b("Status: "),
           "idea 2 (measured widths) and the font step from idea 3 have ",
           "landed in static_table(). Ideas 4 to 7 have not, and pagination ",
           "is out of scope for now. For what the shipping code does to real ",
           "ADaM tables in the BMS template, see ",
           tags$a(href = "bms.html", "bms.html"), "."),
    tags$p(class = "lede", paste(
      "Every slide below is a real flextable, built by the shipping",
      "static_table() and drawn at true slide scale (13.33 x 7.5in).",
      "The test table is a 32 row adverse event table with four arms and",
      "two columns per arm, which is where the download looks worst."
    )),

    sec(1, "What the download does today",
      tags$p(paste(
        "The widths are positional. The stub gets first_col_width (5.65in,",
        "capped at half the slide) and the eight data columns split the",
        "remaining 6.88in equally, 0.86in each. Nothing measures the text."
      )),
      tags$p(paste0(
        "Measured at 14pt Arial, the stub needs ",
        sprintf("%.2f", meas_now$body[1]),
        "in for its longest label and the n (%) columns need ",
        sprintf("%.2f", meas_now$body[2]),
        "in for a cell like 143 (41.2%). So the stub is handed 0.6in more",
        " than it can use while every count column is 0.3in short and wraps."
      )),
      tags$p(paste(
        "That is also why so little of the table lands on the slide. Every",
        "wrapped cell doubles the height of its row, and the table ends up",
        "16in tall against a 6in budget. The badge on each slide counts what",
        "fits."
      )),
      slide(v_current, TITLE),
      wtable(sprintf("%s (asks %.2f)", c("stub", "n (%)", "Events"),
                     meas_now$body[1:3]),
             cur_w[1:3], "What each column is given today")
    ),

    sec(2, "Measure the columns", tags$span(class = "tag win", "biggest win"),
      tags$p(paste(
        "Give every column what its content needs, in this order: data cells",
        "never wrap, header labels never break inside a word, and the stub",
        "takes what is left. The stub is the only column whose text is prose,",
        "so it is the one that wraps well."
      )),
      tags$p(paste(
        "At 14pt this alone fixes the header row. No count column wraps, the",
        "arm names sit on one line, and the stub still gets 4.3in."
      )),
      slide(v_meas$ft, TITLE),
      wtable(c("stub", "n (%)", "Events"),
             v_meas$w[1:3], "Measured allocation at 14pt"),
      div(class = "knob", tags$b("The knob: "),
          "static_table(col_widths = \"measured\") as the default for the ",
          "pptx and docx paths, with the current positional rule kept as ",
          "\"fixed\" for callers that already tuned their numbers.")
    ),

    sec(3, "Then take the font down a step",
      tags$p(paste(
        "14pt is a presenter font. A table this dense reads at 11 or 12pt,",
        "and the two points buy about 15% of the width back and 15% of the",
        "height, which is two more rows per slide."
      )),
      slide(v_meas12$ft, TITLE),
      tags$p(paste(
        "The size does not have to be picked by hand. Step down 14, 13, 12,",
        "11, 10 until the measured minimum fits the slide, which is what",
        "PowerPoint's own shrink-on-overflow does, and stop there."
      )),
      div(class = "knob", tags$b("The knob: "),
          "font_size = \"auto\" alongside the numeric value.")
    ),

    sec(4, "Tighten the cell padding",
      tags$p(paste(
        "flextable pads every cell 5pt left and right. Across nine columns",
        "that is 0.94in of the slide spent on padding. At 2pt it is 0.38in,",
        "and the table gains half an inch of text width for free."
      )),
      slide(v_tight$ft, TITLE),
      div(class = "knob", tags$b("The knob: "),
          "one density argument rather than three. density = ",
          "c(\"comfortable\", \"compact\", \"dense\") sets font size, cell ",
          "padding and row height together.")
    ),

    sec(5, "Drop the grid",
      tags$p(paste(
        "The full grid is the topline look. Rules only (a line under the",
        "header band, a hairline between rows, a frame) with the SOC rows",
        "tinted reads as less dense at the same size, and the eye follows",
        "the row instead of the cell."
      )),
      slide(v_rules, TITLE),
      div(class = "knob", tags$b("The knob: "),
          "borders = c(\"grid\", \"rules\", \"none\"), default grid so no ",
          "existing deck changes.")
    ),

    sec(6, "Let the long labels span the table",
      tags$p(paste(
        "The system organ class is what forces the stub wide. Lifted out of",
        "the stub into a merged full-width section row (the annotated df",
        "already supports this through .group1_level) the stub only has to",
        "hold preferred terms, which are much shorter, and the whole table",
        "narrows."
      )),
      slide(v_sec$ft, TITLE),
      wtable(c("stub", "n (%)", "Events"), v_sec$w[1:3],
             "Measured allocation with SOC as a section row, 12pt"),
      div(class = "knob", tags$b("Not a renderer knob: "),
          "this is a producer choice, so it belongs in the table block's ",
          "gear (group as section rows) rather than in the exporter.")
    ),

    sec(7, "Split a long table over several slides",
      tags$span(class = "tag hard", "the hard one"),
      tags$p(paste(
        "The table is 32 rows. At 12pt about", per, "body rows fit under the",
        "title, so it needs", length(pages), "slides. Each one repeats the",
        "header band and the footnote and says which page it is."
      )),
      tags$p(paste(
        "The break points are not every N rows. A break never lands right",
        "after an SOC row and never pushes the last row or two of a section",
        "onto the next slide alone; both cases move the whole section over.",
        "The widths are measured once on the whole table and reused on every",
        "page, so the columns line up when you flip."
      )),
      lapply(paged, slide, title = TITLE),
      div(class = "knob", tags$b("The knob: "),
          "write_exhibit_pptx(max_rows = NULL | \"auto\" | n). Two caveats: ",
          "the row count is only exact when no cell wraps, because a wrapped ",
          "row grows, which is another reason to measure the widths first; ",
          "and a section that spans a break should repeat its heading with ",
          "(continued), which needs the section-row shape from idea 6."),
      tags$p(class = "q", paste(
        "Open: does the caption belong on every slide or only the last, and",
        "should the slide title repeat verbatim or carry (2 of 3)?"
      ))
    ),

    sec(8, "What this adds up to",
      tags$p(paste(
        "Ideas 2 to 4 change how widths and sizes are chosen and need no",
        "decision from the user. Measured widths on their own take the test",
        "table from 11 rows on the slide to 19 and remove every wrapped count",
        "cell. The font step takes it to 22 and the tighter padding to 30, at",
        "which point a 32 row table is one slide short rather than three",
        "slides deep."
      )),
      tags$h3("Order I would build it in"),
      tags$ul(
        tags$li(tags$b("Measured widths"), " in static_table(), on by ",
                "default for the pptx and docx paths. Self-contained, no ",
                "new state on the block."),
        tags$li(tags$b("A density argument"), " (comfortable / compact / ",
                "dense) that sets font size, padding and row height ",
                "together, plus font_size = \"auto\"."),
        tags$li(tags$b("Border style"), " as a second argument, default ",
                "unchanged so existing decks do not move."),
        tags$li(tags$b("Pagination"), " last, because it needs the height ",
                "budget to be trustworthy, which the first three deliver.")
      ),
      tags$h3("Also on the list, not mocked here"),
      tags$ul(
        tags$li("A table narrower than the slide is left flush at 0.4in ",
                "today. Centring it is one line."),
        tags$li("Too many arms is the same problem sideways: split the ",
                "columns over two slides, repeating the stub. Same ",
                "machinery as the row split."),
        tags$li("The stub could drop the section prefix from repeated ",
                "labels, since the section row above already says it.")
      )
    )
  )
)

save_html(page, "dev/pptx-layout/study.html")
cat("rows/slide:", per, " pages:", length(pages), "\n")
cat("current widths:", sprintf("%.2f", cur_w[1:3]), "\n")
cat("measured 14pt:", sprintf("%.2f", v_meas$w[1:3]), "\n")
cat("measured 12pt tight:", sprintf("%.2f", v_tight$w[1:3]), "\n")
cat("wrote dev/pptx-layout/study.html\n")
