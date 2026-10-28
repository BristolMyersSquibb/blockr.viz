# table_groups_to_model() (R/table-groups-view.R) against the groups recordings:
# the first render of each, `blockr-viz-summarize-data` with kind "flat". The
# browser gets the same rows, numbers and text the reference sent as JSON
# plus HTML, now as a TableModel.

# A recorded array as a vector, `NA` for null.
tg_vec <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  unlist(lapply(x, function(e) if (is.null(e)) NA else e), use.names = FALSE)
}

# The reference escaped its text for markup; the view sends it plain.
table_groups_plain <- function(x) {
  x <- as.character(x)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

# The recorded mark with its text plain.
tg_unescape <- function(w) {
  plain <- function(x) lapply(x, function(e) table_groups_plain(e))
  if (!is.null(w[["tip"]])) w$tip <- plain(w[["tip"]])
  w$lv <- lapply(w$lv, function(g) {
    g$tip <- plain(g[["tip"]])
    g
  })
  if (!is.null(w$tips)) w$tips <- lapply(w$tips, plain)
  if (!is.null(w$segs)) {
    w$segs <- lapply(w$segs, lapply, function(sg) {
      if (length(sg) >= 4L) sg[[4L]] <- table_groups_plain(sg[[4L]])
      sg
    })
  }
  if (is.null(w$lv) || !length(w$lv)) w$lv <- NULL
  w
}

# The mark has the fields of the recorded column, less what the TableCol
# holds, under the names the browser reads (table_groups_wire()).
tg_expect_mark <- function(mk, w, label) {
  moved <- c("fkey", "scol", "disp", "v", "pct", "bg", "fg")
  w <- table_groups_wire(tg_unescape(w[setdiff(names(w), moved)]), w$kind)
  expect_setequal(names(mk), names(w))
  for (f in names(w)) {
    expect_equal(mk[[f]], w[[f]], label = paste(label, f))
  }
}

# The claim a column carries: the facet level, the colour levels.
tg_col_keys <- function(w) {
  out <- list()
  if (!is.null(w$fkey)) out[[w$fkey$column]] <- list(w$fkey$value)
  if (!is.null(w$scol)) out[[w$scol]] <- w$names
  out <- lapply(out, function(v) as.character(unlist(v)))
  if (length(out)) out
}

# One column against the reference payload's column.
tg_expect_col <- function(g, w, n, pick, label) {
  expect_identical(g$num, identical(w$kind, "num") && !isTRUE(w$text),
                   label = paste(label, "num"))
  expect_length(g$disp, n)
  if (!is.null(w$disp)) {
    want <- tg_vec(w$disp)
    if (isTRUE(w$text)) want <- table_groups_plain(want)
    want[!nzchar(want)] <- NA
    expect_equal(tg_vec(g$disp), want, label = paste(label, "disp"))
  }
  expect_equal(tg_vec(g$key), tg_vec(w$v), label = paste(label, "key"))
  pct <- tg_vec(w$pct)
  if (!is.null(pct)) pct[!nzchar(pct)] <- NA
  expect_equal(tg_vec(g$pct), pct, label = paste(label, "pct"))
  expect_equal(tg_vec(g$bg), tg_vec(w$bg))
  expect_equal(tg_vec(g$fg), tg_vec(w$fg))
  keys <- if (pick) tg_col_keys(w)
  expect_identical(if (!is.null(g$keys)) lapply(g$keys, tg_vec), keys, label = paste(label, "keys"))
  expect_identical(!is.null(g$mark), w$kind != "num", label = label)
  if (!is.null(g$mark)) tg_expect_mark(g$mark, w, label)
}

# The recorded header: one list per <tr> of its cells, as the browser read
# them (name, sub line, spans, body column), and the axis strips.
tg_head <- function(html) {
  doc <- xml2::read_html(html)
  attr <- function(x, a) {
    v <- xml2::xml_attr(x, a)
    if (is.na(v)) NULL else as.integer(v)
  }
  span_text <- function(th, cls) {
    s <- xml2::xml_find_first(
      th, sprintf(".//span[contains(@class, '%s')]", cls)
    )
    if (inherits(s, "xml_missing")) NULL else xml2::xml_text(s)
  }
  lapply(xml2::xml_find_all(doc, "//thead/tr"), function(tr) {
    lapply(xml2::xml_find_all(tr, "./th"), function(th) {
      list(
        text = span_text(th, "blockr-col-name"),
        sub = span_text(th, "blockr-col-label"),
        span = attr(th, "colspan"), rows = attr(th, "rowspan"),
        col = attr(th, "data-col-index"),
        stub = grepl("blockr-stub-header", xml2::xml_attr(th, "class")),
        axis = tg_axis(xml2::xml_find_first(
          th, ".//div[contains(@class, 'blockr-summarize-axis')]"
        ))
      )
    })
  })
}

# An axis strip: the tick positions (percent) and labels, and whether it
# reserves the value slot.
tg_axis <- function(el) {
  if (inherits(el, "xml_missing")) {
    return(NULL)
  }
  ticks <- xml2::xml_find_all(el, ".//span[not(contains(@class, 'axis-pad'))][@style]")
  ticks <- ticks[grepl("left:", xml2::xml_attr(ticks, "style"), fixed = TRUE)]
  list(
    pos = as.numeric(sub("%.*", "", sub(".*left:", "",
                                        xml2::xml_attr(ticks, "style")))),
    labels = xml2::xml_text(ticks),
    has_val = grepl("has-val", xml2::xml_attr(el, "class"))
  )
}

tg_expect_axis <- function(want, col, label) {
  ax <- col$axis
  if (is.null(want)) {
    expect_null(ax, label = paste(label, "no strip"))
    return(invisible())
  }
  expect_equal(tg_vec(ax$at), want$pos, tolerance = 0.01,
               label = paste(label, "ticks"))
  expect_identical(tg_vec(ax$labels), want$labels, label = paste(label, "labels"))
  expect_identical(!is.null(col$mark$valueWidth), want$has_val, label = paste(label, "slot"))
}

tg_expect_head <- function(v, want, label) {
  recorded <- tg_head(want$head)
  expect_length(v$head, length(recorded))
  for (r in seq_along(recorded)) {
    expect_length(v$head[[r]], length(recorded[[r]]))
    for (k in seq_along(recorded[[r]])) {
      w <- recorded[[r]][[k]]
      h <- v$head[[r]][[k]]
      lab <- paste(label, "head", r, k)
      expect_identical(h$text, w$text %||% "", label = lab)
      expect_identical(h$sub, w$sub, label = paste(lab, "sub"))
      expect_null(h$axis)
      expect_identical(h$span, w$span, label = paste(lab, "span"))
      expect_identical(h$rows, w$rows, label = paste(lab, "rows"))
      if (!is.null(w$col)) expect_identical(h$col, w$col, label = paste(lab, "col"))
      expect_identical(isTRUE(h$stub), w$stub, label = paste(lab, "stub"))
      if (!is.null(w$col) && w$col > 0L) {
        tg_expect_axis(w$axis, v$cols[[w$col + 1L]], lab)
      }
    }
  }
  # The bottom axis of the large swimlanes.
  foot <- xml2::xml_find_all(xml2::read_html(want$head),
                             "//tfoot//div[contains(@class, 'blockr-summarize-axis')]")
  large <- Filter(function(cc) isTRUE(cc$axis$foot), v$cols)
  expect_length(large, length(foot))
}

# Rows: nesting, claims, the fold note.
tg_expect_rows <- function(v, w, label) {
  expect_identical(v$n, w$n)
  expect_identical(tg_vec(v$cols[[1L]]$disp), tg_vec(w$label))
  nested <- !is.null(w$parent)
  if (nested) {
    level <- if (is.null(w$level)) rep(0L, w$n) else tg_vec(w$level)
    expect_equal(tg_vec(v$rows$indent), level)
    parent <- if (is.null(w$parent_row)) rep(FALSE, w$n) else tg_vec(w$parent_row)
    expect_equal(tg_vec(v$rows$strong), parent)
  } else {
    expect_null(v$rows$indent)
    expect_null(v$rows$strong)
  }
  expect_identical(isTRUE(v$collapsed), nested && !isTRUE(w$expanded))
  expect_identical(!is.null(v$rows$keys), isTRUE(w$pick), label = label)
  expect_identical(v$note, w$fold)
  if (!isTRUE(w$pick)) {
    return(invisible())
  }
  path <- tg_vec(w$path)
  lab <- tg_vec(w$label)
  pk <- tg_vec(w$parent_key)
  parent <- if (is.null(w$parent_row)) rep(FALSE, w$n) else tg_vec(w$parent_row)
  for (i in seq_len(w$n)) {
    want <- if (length(path) < 2L) {
      stats::setNames(list(lab[[i]]), path)
    } else if (parent[[i]]) {
      stats::setNames(list(pk[[i]]), path[[1L]])
    } else {
      stats::setNames(list(pk[[i]], lab[[i]]), path)
    }
    got <- lapply(v$rows$keys[[i]], tg_vec)
    expect_identical(got, want, label = paste(label, "claim", i))
  }
}

tg_check <- function(v, want, label, pick = isTRUE(want$pick)) {
  tg_expect_rows(v, want, label)
  expect_length(v$cols, length(want$cols) + 1L)
  for (j in seq_along(want$cols)) {
    tg_expect_col(v$cols[[j + 1L]], want$cols[[j]], want$n, pick,
                  paste(label, j))
  }
  tg_expect_head(v, want, label)
}

test_that("table_groups_to_model() draws what the reference sent", {
  cases <- tm_cases("groups")
  seen <- character()
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    want <- tm_payload(cs$messages, "blockr-viz-summarize-data")
    if (!identical(want$kind, "flat")) next
    m <- table_model_groups(cs$state, cs$data)
    v <- tm_json(table_groups_to_model(
      m, axis = isTRUE(cs$state$column_axis %||% TRUE)
    ))
    tg_check(v, want, nm)
    seen <- c(seen, nm)
  }
  expect_gte(length(seen), 20L)
  expect_true(all(c("grp_all_types", "grp_facet", "grp_two_by",
                    "grp_color_split", "grp_compact", "grp_drill") %in% seen))
})

# A state that draws every colour-split glyph, the nested drill and the
# facet header, checked against the reference payload of the same cells
# (the recordings hold no colour-split box or pair).
tg_state <- function(cs, ...) {
  s <- cs$state
  for (nm in names(list(...))) s[[nm]] <- list(...)[[nm]]
  s
}

test_that("a one-row table's columns are arrays on the wire", {
  cs <- tm_cases("groups")$grp_no_by
  v <- table_groups_to_model(table_model_groups(cs$state, cs$data))
  back <- tm_json(v)
  expect_type(back$cols[[1L]]$disp, "list")
  expect_type(back$cols[[2L]]$mark$w, "list")
  expect_type(back$cols[[2L]]$key, "list")
})

test_that("a column's axis has no empty fields on the wire", {
  cs <- tm_cases("groups")$grp_all_types
  v <- tm_json(table_groups_to_model(table_model_groups(cs$state, cs$data)))
  expect_named(v$cols[[2L]]$axis, c("at", "labels"))
  expect_true(v$cols[[11L]]$axis$foot)
})

test_that("a failed preparation has no view", {
  expect_null(table_groups_to_model(list(prep = list(err = "x"), cells = NULL)))
})
