# The table block's downloads: the frame they write (dt_exhibit_frame) and
# what the three writers make of it.

xt_frame <- function() {
  xt <- as.data.frame.matrix(table(mtcars$cyl, mtcars$gear))
  data.frame(cyl = rownames(xt), xt, check.names = FALSE)
}

seq_rule <- function() drilldown_table_color(type = "sequential")

# The background / text colours the screen styles a column with, NA where the
# cell is unpainted -- read off the same cell model the browser renders.
screen_paint <- function(data, label_col, shadings, col) {
  b <- dt_flat_build(data, label_col, NULL, dd_parse_shadings(shadings),
                     toggles = list())
  j <- match(col, setdiff(names(data), label_col))
  st <- b$cells[[j]]$style
  bg <- ifelse(nzchar(st), sub(".*background:([^;]+);.*", "\\1", st), NA)
  fg <- ifelse(nzchar(st), sub(".*color:([^;]+);.*", "\\1", st), NA)
  list(bg = unname(bg), fg = unname(fg))
}

slide_xml <- function(f) {
  td <- withr::local_tempdir(.local_envir = parent.frame())
  utils::unzip(f, exdir = td)
  paste(unlist(lapply(
    list.files(file.path(td, "ppt/slides"), "xml$", full.names = TRUE),
    readLines, warn = FALSE
  )), collapse = "")
}

xlsx_styles <- function(f) {
  td <- withr::local_tempdir(.local_envir = parent.frame())
  utils::unzip(f, exdir = td)
  paste(readLines(file.path(td, "xl/styles.xml"), warn = FALSE),
        collapse = "")
}

test_that("the download frame carries the screen's cell paint (#43)", {
  d <- xt_frame()
  ex <- dt_exhibit_frame(d, "cyl", shadings = seq_rule())

  expect_identical(names(as_plain_df(ex)), c("cyl", "3", "4", "5"))
  for (cn in c("3", "4", "5")) {
    sp <- screen_paint(d, "cyl", seq_rule(), cn)
    expect_identical(ex[[paste0(".bg:", cn)]], sp$bg)
    expect_identical(ex[[paste0(".fg:", cn)]], sp$fg)
  }
  # Numbers are not rounded or formatted here; the writers do that.
  expect_identical(ex[["3"]], d[["3"]])
})

test_that("colour-source columns stay out of the download, as on screen", {
  d <- data.frame(
    USUBJID = c("s1", "s2", "s3"),
    RASH = c(2, NA, 1), `RASH (sev)` = c(3, NA, 1),
    NAUSEA = c(1, 4, NA), `NAUSEA (sev)` = c(NA, 2, NA),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  rule <- list(list(mode = "sequential", cols = list(),
                    source = "{col} (sev)"))
  ex <- dt_exhibit_frame(d, "USUBJID", shadings = rule)

  expect_identical(names(as_plain_df(ex)), c("USUBJID", "RASH", "NAUSEA"))
  # Painted by the source: s1 (sev 3) and s3 (sev 1) differ although both
  # show a count; the NA count and the missing source stay unpainted.
  sp <- screen_paint(d, "USUBJID", rule, "RASH")
  expect_identical(ex[[".bg:RASH"]], sp$bg)
  expect_false(identical(ex[[".bg:RASH"]][1], ex[[".bg:RASH"]][3]))
  expect_true(is.na(ex[[".bg:RASH"]][2]))
  expect_true(is.na(ex[[".bg:NAUSEA"]][1]))
})

test_that("bar shading exports plain columns", {
  d <- xt_frame()
  ex <- dt_exhibit_frame(d, "cyl",
                         shadings = drilldown_table_color(type = "bar"))
  expect_false(any(grepl("^\\.(bg|fg):", names(ex))))
})

test_that("the download frame follows the display projection", {
  d <- data.frame(arm = c("A", "A", "B"), val = c(1, 3, 10),
                  other = c("x", "y", "z"))
  # rowname moves first, an explicit value pick drops the rest
  ex <- dt_exhibit_frame(d, rowname = "other", value = "val")
  expect_identical(names(ex), c("other", "val"))

  # an aggregated table downloads its aggregate, painted
  ex <- dt_exhibit_frame(d, group = "arm",
                         summaries = list(list(func = "mean",
                                               cols = "val")),
                         shadings = list(list(mode = "sequential",
                                              cols = list())))
  expect_identical(ex$arm, c("A", "B"))
  expect_equal(as.numeric(ex$val), c(2, 10))
  expect_false(anyNA(ex[[".bg:val"]]))

  # a structured frame is the writers' business and passes through
  st <- data.frame(.label = c("a", "b"), n = c("1", "2"))
  expect_identical(dt_exhibit_frame(st, shadings = seq_rule()), st)
})

test_that("the three downloads of the issue's table are coloured (#43)", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")

  d <- xt_frame()
  blk <- new_table_block(rowname = "cyl", cell_color = seq_rule(),
                         download = TRUE,
                         title = "Cars by cylinders and gears")
  bg <- unique(unlist(lapply(c("3", "4", "5"), function(cn) {
    stats::na.omit(screen_paint(d, "cyl", seq_rule(), cn)$bg)
  })))
  hex <- toupper(sub("^#", "", bg))

  shiny::testServer(blk$expr_server,
                    args = list(data = shiny::reactive(d)), {
    session$flushReact()

    h <- paste(readLines(output$dl_html, warn = FALSE), collapse = "")
    for (b in bg) expect_match(h, paste0("background:", b), fixed = TRUE)
    expect_false(grepl(".bg:", h, fixed = TRUE))

    st <- xlsx_styles(output$dl_xlsx)
    for (x in hex) expect_match(st, x, fixed = TRUE)

    sl <- toupper(slide_xml(output$dl_pptx))
    for (x in hex) expect_match(sl, x, fixed = TRUE)
  })
})

# --- digits (#37) -------------------------------------------------------------

coef_frame <- function() {
  data.frame(term = "A", times_more = 3.81427057462906,
             ci_low = 2.70122773653209, n = 12L)
}

xlsx_num_formats <- function(f) {
  wb <- openxlsx::loadWorkbook(f)
  out <- list()
  for (s in wb$styleObjects) {
    code <- s$style$numFmt$formatCode
    if (is.null(code)) next
    for (cc in unique(s$cols)) out[[as.character(cc)]] <- code
  }
  out
}

test_that("exports format numbers like the screen (#37)", {
  d <- coef_frame()
  attr(d$times_more, "label") <- "Times more"
  out <- dt_format_digits(d, 2L)
  expect_identical(as.character(out$times_more), "3.81")
  expect_identical(out$ci_low, "2.7")
  expect_identical(out$n, "12")
  expect_identical(attr(out$times_more, "label"), "Times more")
  # the screen's own formatter, cell for cell
  b <- dt_flat_build(d, "term", NULL, toggles = list(), digits = 2L)
  expect_identical(b$cells[[1]]$disp, "3.81")
  expect_identical(b$cells[[2]]$disp, "2.7")

  # NA stays missing, a numeric stub is left alone, NULL is a no-op
  d2 <- data.frame(id = c(1.234, 2), v = c(NA, 1.005))
  out2 <- dt_format_digits(d2, 1L)
  expect_identical(out2$id, d2$id)
  expect_identical(out2$v, c(NA, "1"))
  expect_identical(dt_format_digits(d2, NULL), d2)
})

test_that("Excel keeps the full value under a rounding number format (#37)", {
  skip_if_not_installed("openxlsx")
  f <- withr::local_tempfile(fileext = ".xlsx")
  write_annotated_xlsx(coef_frame(), f, digits = 2L)

  back <- openxlsx::read.xlsx(f)
  expect_equal(back$times_more, 3.81427057462906)
  fmts <- xlsx_num_formats(f)
  expect_identical(fmts[["2"]], "0.00")
  expect_identical(fmts[["3"]], "0.00")
  # a count column keeps the general format (3, not 3.00)
  expect_null(fmts[["4"]])

  # the format survives the paint stacked over it
  ex <- dt_exhibit_frame(coef_frame(), shadings = list(list(
    mode = "sequential", cols = list("times_more", "ci_low")
  )))
  write_annotated_xlsx(ex, f, digits = 0L)
  expect_identical(xlsx_num_formats(f)[["2"]], "0")
})

test_that("the block's downloads round to its digits (#37)", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")

  blk <- new_table_block(digits = 2L, download = TRUE)
  shiny::testServer(blk$expr_server,
                    args = list(data = shiny::reactive(coef_frame())), {
    session$flushReact()

    h <- paste(readLines(output$dl_html, warn = FALSE), collapse = "")
    expect_match(h, ">3.81<", fixed = TRUE)
    expect_match(h, ">2.7<", fixed = TRUE)
    expect_false(grepl("3.8142", h, fixed = TRUE))

    sl <- slide_xml(output$dl_pptx)
    expect_match(sl, ">3.81<", fixed = TRUE)
    expect_match(sl, ">2.7<", fixed = TRUE)
    expect_false(grepl("3.8142", sl, fixed = TRUE))

    fx <- output$dl_xlsx
    expect_equal(openxlsx::read.xlsx(fx)$times_more, 3.81427057462906)
    expect_identical(xlsx_num_formats(fx)[["2"]], "0.00")
  })
})

test_that("table_exhibit() is the block as a printed table", {
  d <- coef_frame()
  attr(d, "label") <- "From upstream"
  ex <- table_exhibit(d, digits = 2L, subtitle = "GLMM", caption = "",
                      shadings = drilldown_table_color("sequential"))
  expect_identical(ex$times_more, "3.81")
  expect_false(anyNA(ex[[".bg:times_more"]]))
  # title tiers: NULL takes the input's label, "" none, else a template
  expect_identical(attr(ex, "label"), "From upstream")
  expect_identical(attr(ex, "subtitle"), "GLMM")
  expect_null(attr(ex, "caption"))

  skip_if_not_installed("flextable")
  ft <- static_exhibit(ex)
  expect_s3_class(ft, "flextable")
  body <- ft$body$dataset
  expect_true("3.81" %in% unlist(body))
  expect_false(any(grepl("^\\.(bg|fg):", names(body))))
})

test_that("the table block's report call carries digits, text and colours (#37)", {
  blk <- new_table_block(digits = 1L, title = "Area effect", subtitle = "",
                         cell_color = drilldown_table_color("sequential"))
  cl <- report_call(blk, "gtbl")
  expect_identical(cl[[1]], quote(blockr.viz::static_exhibit))
  inner <- cl[[2]]
  expect_identical(inner[[1]], quote(blockr.viz::table_exhibit))
  expect_identical(inner[[2]], as.name("gtbl"))
  expect_identical(inner$digits, 1L)
  expect_identical(inner$title, "Area effect")
  expect_identical(inner$subtitle, "")
  expect_identical(inner$shadings[[1]]$mode, "sequential")
  expect_null(inner$caption)

  # defaults stay out of the emitted call
  plain <- report_call(new_table_block(), "x")[[2]]
  expect_identical(length(plain), 2L)

  # and the emitted call runs
  gtbl <- coef_frame()
  skip_if_not_installed("flextable")
  ft <- eval(cl)
  expect_true("3.8" %in% unlist(ft$body$dataset))
})
