# Lanes on the spans row: records with an empty lane draw on the main lane,
# every other lane level draws a thin lane above it, with its own open end,
# segment styles and hover strip.

lanes_fixture <- function() {
  na <- NA_real_
  data.frame(
    id = c("a", "a", "a", "a", "a", "a", "a", "a", "a", "b", "b"),
    s = c(0, 40, 0, 50, 21, 42, 63, 70, 10, 0, 0),
    e = c(40, 100, 30, 90, 26, 49, na, na, na, 60, 20),
    resp = c("SD", "PR", "Drug A", "Drug B", "Delay", "Drug B", "Drug B",
             "Drug B", NA, "PD", "Drug A"),
    drug = factor(c(NA, NA, "Drug A", "Drug B", "Drug B", "Drug B", "Drug B",
                    "Drug B", "Drug B", NA, "Drug A"),
                  levels = c("Drug A", "Drug B")),
    style = c(NA, NA, NA, NA, "hollow", "dashed", "dot", "step", NA, NA, NA),
    strip = c(NA, NA, NA, NA, NA, NA, "C1\n75 mg", NA, "C0", NA, NA),
    ongoing = c(TRUE, NA, FALSE, TRUE, NA, NA, NA, NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )
}

lanes_spans <- function(...) {
  list(type = "spans", name = "Lane", x = "s", xend = "e", color = "resp",
       lane = "drug", ...)
}

test_that("a lane column splits a row into a main lane and thin lanes", {
  m <- sum_model(lanes_fixture(), by = "id", summaries = list(lanes_spans(
    open = "ongoing", seg_style = "style", strip = "strip"
  )))
  expect_null(m$prep$err)
  c1 <- m$cells$cols[[1]]
  i <- match(c("a", "b"), m$cells$label)
  # The main lane holds only the records without a lane.
  expect_identical(lengths(c1$segs[i]), c(2L, 1L))
  # Thin lanes in level order, each with its own segments.
  a <- c1$sl[[i[[1]]]]
  expect_identical(vapply(a, function(l) l$n, ""), c("Drug A", "Drug B"))
  expect_length(a[[1]]$s, 1L)
  expect_length(a[[2]]$s, 5L)
  expect_identical(vapply(c1$sl[[i[[2]]]], function(l) l$n, ""), "Drug A")
  # The open end is per lane: the main lane and Drug B are open, Drug A is
  # not, and the main lane's arrow reads only its own records.
  pos <- function(v) round((v - c1$d0) / (c1$d1 - c1$d0) * 100, 2L)
  expect_equal(c1$op[[i[[1]]]], pos(100))
  expect_null(a[[1]]$o)
  expect_equal(a[[2]]$o, pos(90))
  expect_true(is.na(c1$op[[i[[2]]]]))
})

test_that("segment styles ride as the fifth slot, points need no end", {
  m <- sum_model(lanes_fixture(), by = "id", summaries = list(lanes_spans(
    seg_style = "style"
  )))
  c1 <- m$cells$cols[[1]]
  b <- c1$sl[[match("a", m$cells$label)]][[2]]$s
  st <- vapply(b, function(g) if (length(g) >= 5L) g[[5L]] else 1L, 1L)
  # The period first, so the styled marks draw over it; then by start.
  expect_identical(st, c(1L, 3L, 2L, 4L, 5L))
  # A dot and a step have no end: a zero width at their start.
  expect_identical(b[[4]][[2]], 0)
  # A solid segment keeps the three-slot tuple.
  expect_length(b[[1]], 3L)
  expect_identical(groups_spans_styles(
    data.frame(x = c("Dashed", " dot ", "other", NA)), "x"
  ), c(2L, 4L, 1L, 1L))
})

test_that("strip boxes come per lane in time order, with their style", {
  d <- lanes_fixture()
  d$strip[[5]] <- "C2\nlate"
  m <- sum_model(d, by = "id", summaries = list(lanes_spans(
    seg_style = "style", strip = "strip"
  )))
  c1 <- m$cells$cols[[1]]
  a <- c1$sl[[match("a", m$cells$label)]]
  bx <- a[[2]]$b
  expect_identical(vapply(bx, function(x) x[[1L]], ""),
                   c("C0", "C2\nlate", "C1\n75 mg"))
  expect_identical(vapply(bx, function(x) x[[2L]], 1L), c(1L, 3L, 4L))
  # A strip record without an end still makes a box (C0 draws nothing).
  expect_length(a[[2]]$s, 5L)
  expect_length(a[[1]]$b, 0L)
  # No main-lane strip here.
  expect_null(c1$ms)
})

test_that("without a lane column the payload has no lanes", {
  d <- lanes_fixture()
  c1 <- sum_model(d, by = "id", summaries = list(list(
    type = "spans", x = "s", xend = "e", color = "resp"
  )))$cells$cols[[1]]
  expect_false(any(c("sl", "ms") %in% names(c1)))
  # Every finite record is on the one lane.
  expect_identical(lengths(c1$segs[match("a", sum_model(
    d, by = "id", summaries = list(list(type = "spans", x = "s", xend = "e"))
  )$cells$label)]), 6L)
})

test_that("a parent row draws no thin lanes", {
  d <- lanes_fixture()
  d$arm <- "A"
  m <- sum_model(d, by = c("arm", "id"), group_rows = "summary",
                 summaries = list(lanes_spans()))
  c1 <- m$cells$cols[[1]]
  expect_length(c1$sl[[which(m$cells$label == "A")]], 0L)
  expect_length(c1$sl[[which(m$cells$label == "a")]], 2L)
})

test_that("the lanes reach the browser under their names", {
  m <- sum_model(lanes_fixture(), by = "id", summaries = list(lanes_spans(
    seg_style = "style", strip = "strip", open = "ongoing"
  )))
  w <- table_groups_wire(
    table_groups_pack(m$cells$cols[[1]], "interval", TRUE), "interval"
  )
  expect_true("subLanes" %in% names(w))
  j <- jsonlite::fromJSON(
    jsonlite::toJSON(w$subLanes, auto_unbox = TRUE, null = "null",
                     na = "null"),
    simplifyVector = FALSE
  )
  a <- j[[match("a", m$cells$label)]]
  expect_identical(a[[1]]$n, "Drug A")
  expect_true(is.list(a[[1]]$t))
  expect_identical(a[[2]]$b[[1]][[1]], "C0")
})

test_that("a column's own colours win for the levels it names", {
  col <- structure(c("CR", "PD", "Drug B"),
                   blockr_colors = c(CR = "#1d7a55", PD = "#c8473d",
                                     "Drug B" = "#2a9686"))
  expect_identical(
    groups_level_colors(NULL, "resp", c("PD", "CR"), col),
    c(PD = "#c8473d", CR = "#1d7a55")
  )
  # A level it does not name falls back to the palette for all.
  expect_false(identical(
    unname(groups_level_colors(NULL, "resp", c("PD", "SD"), col)[["PD"]]),
    "#c8473d"
  ))
})

test_that("the static picture grows a row by its thin lanes", {
  skip_if_not_installed("systemfonts")
  m <- sum_model(lanes_fixture(), by = "id", summaries = list(lanes_spans(
    seg_style = "style", open = "ongoing"
  )))
  c1 <- m$cells$cols[[1]]
  px <- 1 / 96
  h <- paint_row_heights(m$cells, 26 * px, px, 9)
  i <- match(c("a", "b"), m$cells$label)
  expect_equal(h[[i[[1]]]], paint_iv_stack(2, px) + 8 * px)
  expect_equal(h[[i[[2]]]], max(26 * px, paint_iv_stack(1, px) + 8 * px))
  gl <- list(H = 2, W = 4, px = px, lane = 12 * px, hair = px,
             row_h = h[[i[[1]]]], fs = 10, family = "sans")
  g <- Filter(Negate(is.null), paint_interval(c1, i[[1]], 0, 3, 0.2, gl))
  # Six thin-lane segments, Drug B's arrow, two main segments, its arrow.
  expect_length(g, 10L)
  for (st in 1:5) {
    sg <- list(10, 20, 1L, "", st)
    expect_true(grid::is.grob(paint_iv_seg(sg, "#000", 0, 3, 0.2, 0.05, gl)) ||
                  inherits(paint_iv_seg(sg, "#000", 0, 3, 0.2, 0.05, gl),
                           "gList"))
  }
  png <- tempfile(fileext = ".png")
  on.exit(unlink(png))
  p <- summarize_paint_grob(m$cells, m$prep, width_in = 6)
  paint_write_png(p, png, res = 72)
  expect_true(file.exists(png))
})

test_that("the table block takes a dm for its prepare script", {
  skip_if_not_installed("dm")
  dmo <- dm::dm(a = data.frame(x = 1:2), b = data.frame(y = 3))
  blk <- new_table_block()
  expect_no_error(blockr.core::validate_data_inputs(blk, list(data = dmo)))
  expect_error(
    blockr.core::validate_data_inputs(blk, list(data = structure(
      list(), class = "no_such_table"
    ))),
    "as_annotated_df"
  )
})

test_that("a tooltip leaves out the fields a record has no value in", {
  d <- lanes_fixture()
  d$dose <- ifelse(is.na(d$drug), "", "75 mg")
  c1 <- sum_model(d, by = "id", summaries = list(lanes_spans(
    tooltip = "dose"
  )))$cells$cols[[1]]
  i <- match("a", sum_model(d, by = "id", summaries = list(lanes_spans()))$cells$label)
  expect_false(any(grepl("dose:", unlist(c1$tips[[i]]))))
  expect_match(c1$sl[[i]][[1]]$t[[1]], "dose: 75 mg")
})

# AE-style rows: every record on a lane, some ongoing without an end.
ae_fixture <- function() {
  na <- NA_real_
  data.frame(
    id = c("a", "a", "a", "a", "b"),
    s = c(0, 10, 30, na, 5),
    e = c(20, na, 50, na, 15),
    sev = c("MILD", "SEVERE", "MILD", NA, "MILD"),
    lane = c("1", "2", "1", "2", "1"),
    ongoing = c(NA, TRUE, NA, NA, NA),
    ev = c(na, na, na, 45, na),
    kind = c(NA, NA, NA, "Death", NA),
    style = c(NA, "outline", NA, NA, NA),
    stringsAsFactors = FALSE
  )
}

ae_spans <- function(...) {
  list(type = "spans", x = "s", xend = "e", color = "sev", lane = "lane",
       open = "ongoing", ...)
}

test_that("an open record without an end runs to the axis end, ongoing", {
  m <- sum_model(ae_fixture(), by = "id", summaries = list(ae_spans()))
  c1 <- m$cells$cols[[1]]
  # The axis reads the lanes' bars when no record is on the main lane.
  expect_equal(c(c1$d0, c1$d1), c(0, 50))
  expect_equal(c1$sl[[1]][[1]]$s[[2]][[1]], 60)
  a <- c1$sl[[match("a", m$cells$label)]]
  sg <- a[[2]]$s[[1]]
  expect_equal(sg[[1]] + sg[[2]], 100, tolerance = 0.02)
  expect_equal(a[[2]]$o, 100)
  expect_match(a[[2]]$t[[1]], "–ongoing")
  expect_null(a[[1]]$o)
})

test_that("a row-level open flag on a record with an end keeps its arrow", {
  d <- data.frame(id = "a", s = c(0, 40), e = c(40, 100),
                               ongoing = c(TRUE, NA))
  c1 <- sum_model(d, by = "id", summaries = list(list(
    type = "spans", x = "s", xend = "e", open = "ongoing"
  )))$cells$cols[[1]]
  expect_equal(c1$op[[1]], 100)
  expect_length(c1$segs[[1]], 2L)
})

test_that("without main-lane records the lanes are even; events go to their lane", {
  m <- sum_model(ae_fixture(), by = "id", summaries = list(ae_spans(
    event_x = "ev", event_shape = "kind", seg_style = "style"
  )))
  c1 <- m$cells$cols[[1]]
  expect_true(c1$eq)
  a <- c1$sl[[match("a", m$cells$label)]]
  expect_length(a[[2]]$e, 1L)
  expect_match(a[[2]]$et[[1]], "^Death")
  # The death sits on lane 2, not on the main lane.
  expect_length(c1$ev[[match("a", m$cells$label)]], 0L)
  # The serious AE keeps its colour and gets the outline style.
  expect_identical(a[[2]]$s[[1]][[5]], 6L)
  # With a main-lane record the lanes are thin again.
  d <- ae_fixture()
  d$lane[[1]] <- NA
  c2 <- sum_model(d, by = "id", summaries = list(ae_spans()))$cells$cols[[1]]
  expect_null(c2$eq)
})

test_that("the static picture draws even lanes, their events and outlines", {
  skip_if_not_installed("systemfonts")
  m <- sum_model(ae_fixture(), by = "id", summaries = list(ae_spans(
    event_x = "ev", event_shape = "kind", seg_style = "style"
  )))
  c1 <- m$cells$cols[[1]]
  px <- 1 / 96
  h <- paint_row_heights(m$cells, 26 * px, px, 9)
  i <- match("a", m$cells$label)
  expect_equal(h[[i]], max(26 * px, paint_iv_stack(2, px, TRUE) + 8 * px))
  gl <- list(H = 2, W = 4, px = px, lane = 12 * px, hair = px,
             row_h = h[[i]], fs = 10, family = "sans")
  g <- Filter(Negate(is.null), paint_interval(c1, i, 0, 3, 0.2, gl))
  # Lane 1: two bars; lane 2: the outlined bar, its arrow, the death.
  expect_length(g, 5L)
  png <- tempfile(fileext = ".png")
  on.exit(unlink(png))
  paint_write_png(summarize_paint_grob(m$cells, m$prep, width_in = 6), png,
                  res = 72)
  expect_true(file.exists(png))
})

test_that("bar labels ride per segment, empty where a record has none", {
  d <- ae_fixture()
  d$term <- c("Pain", "", "Rash", NA, "Cough")
  m <- sum_model(d, by = "id", summaries = list(ae_spans(bar_label = "term")))
  c1 <- m$cells$cols[[1]]
  a <- c1$sl[[match("a", m$cells$label)]]
  expect_identical(unlist(a[[1]]$l), c("Pain", "Rash"))
  # A lane whose records have no label sends none.
  expect_null(a[[2]]$l)
  # Without the field nothing is sent.
  c2 <- sum_model(d, by = "id", summaries = list(ae_spans()))$cells$cols[[1]]
  expect_null(c2$bl)
  expect_null(c2$sl[[1]][[1]]$l)
  # On the main lane too.
  d2 <- data.frame(id = "a", s = c(0, 50), e = c(40, 60), t = c("Long", "B"))
  c3 <- sum_model(d2, by = "id", summaries = list(list(
    type = "spans", x = "s", xend = "e", bar_label = "t"
  )))$cells$cols[[1]]
  expect_identical(unlist(c3$bl[[1]]), c("Long", "B"))
})

test_that("the static picture writes bar labels where they fit", {
  skip_if_not_installed("systemfonts")
  d <- data.frame(id = "a", s = c(0, 41, 43), e = c(40, 42, 100),
                  t = c("Headache", "Rash", ""))
  m <- sum_model(d, by = "id", summaries = list(list(
    type = "spans", x = "s", xend = "e", bar_label = "t"
  )))
  c1 <- m$cells$cols[[1]]
  px <- 1 / 96
  gl <- list(H = 2, W = 6, px = px, lane = 12 * px, hair = px, row_h = 0.3,
             fs = 9, family = "sans")
  txt <- Filter(function(g) inherits(g, "text"),
                paint_interval(c1, 1L, 0, 5, 0.2, gl))
  # Headache fits on its bar; Rash fits neither on its bar nor in the gap
  # before the next bar; the third bar has no text.
  expect_identical(vapply(txt, function(g) g$label, ""), "Headache")
})

test_that("full lanes draw as bars below the main one, each named", {
  d <- data.frame(
    id = c("a", "a", "a", "a", "a", "b", "b"),
    s = c(0, 40, 0, 30, 0, 0, 0),
    e = c(40, 90, 30, 90, 80, 50, 50),
    resp = c("SD", "PR", "SD", "PD", "Drug A", "PR", "SD"),
    lane = factor(c("INV", "INV", "BICR", "BICR", "Drug A", "INV", "BICR"),
                  levels = c("Drug A", "INV", "BICR")),
    full = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE),
    ev = c(NA, NA, NA, 30, NA, NA, NA), evk = c(NA, NA, NA, "PD", NA, NA, NA),
    stringsAsFactors = FALSE
  )
  m <- sum_model(d, by = "id", summaries = list(list(
    type = "spans", name = "Lane", x = "s", xend = "e", color = "resp",
    lane = "lane", lane_full = "full", event_x = "ev", event_shape = "evk"
  )))
  expect_null(m$prep$err)
  c1 <- m$cells$cols[[1]]
  i <- match("a", m$cells$label)
  # The first full lane (INV) is the main lane; BICR is a full bar below it,
  # with its own event; the drug stays a thin lane.
  expect_length(c1$segs[[i]], 2L)
  a <- c1$sl[[i]]
  expect_identical(vapply(a, function(l) l$n, ""), c("Drug A", "BICR"))
  expect_null(a[[1]]$f)
  expect_true(a[[2]]$f)
  expect_length(a[[2]]$e, 1L)
  expect_identical(c1$mn, "INV")
  expect_identical(c1$lw, 4L)
  # One full lane alone is just the main lane, unnamed.
  m1 <- sum_model(d[d$lane != "BICR", ], by = "id", summaries = list(list(
    type = "spans", name = "Lane", x = "s", xend = "e", color = "resp",
    lane = "lane", lane_full = "full"
  )))
  c2 <- m1$cells$cols[[1]]
  expect_null(c2$mn)
  expect_identical(vapply(c2$sl[[match("a", m1$cells$label)]], function(l) l$n, ""),
                   "Drug A")
  # The static picture draws the full bar, its event and the names, and the
  # row grows by it.
  gl <- list(px = 1 / 96, H = 3, lane = 12 / 96, row_h = 0.4, fs = 9,
             family = "sans")
  g <- paint_interval(c1, i, 0, 3, 0.2, gl)
  txt <- unlist(lapply(g, function(x) if (inherits(x, "text")) x$label))
  expect_true(all(c("INV", "BICR") %in% txt))
  # The names end left of the bars, which start after the gutter.
  nx <- unlist(lapply(g, function(x) {
    if (inherits(x, "text")) grid::convertX(x$x, "in", valueOnly = TRUE)
  }))
  expect_true(all(nx < paint_iv_gutter(c1$lw, gl)))
  expect_gt(paint_iv_stack(1, 1, FALSE, 1), paint_iv_stack(1, 1, FALSE, 0))
  png <- tempfile(fileext = ".png")
  on.exit(unlink(png))
  p <- summarize_paint_grob(m$cells, m$prep, width_in = 6)
  paint_write_png(p, png, res = 72)
  expect_true(file.exists(png))
})

test_that("a colour column with its own colours may have more than 15 levels", {
  lv <- paste0("L", 1:18)
  d <- data.frame(id = rep("a", 18), s = 0:17, e = 1:18,
                  col = factor(lv, levels = lv))
  m <- sum_model(d, by = "id", summaries = list(list(
    type = "spans", name = "Lane", x = "s", xend = "e", color = "col")))
  expect_match(m$prep$err %||% "", "18 levels")
  attr(d$col, "blockr_colors") <- stats::setNames(rep("#123456", 18), lv)
  m2 <- sum_model(d, by = "id", summaries = list(list(
    type = "spans", name = "Lane", x = "s", xend = "e", color = "col")))
  expect_null(m2$prep$err)
})
