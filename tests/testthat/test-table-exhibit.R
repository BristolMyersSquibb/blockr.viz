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
