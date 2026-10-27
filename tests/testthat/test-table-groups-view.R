# table_groups_view() (R/table-groups-view.R) against the groups recordings:
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

# The recorded fields of each mark kind and where the view puts them.
tg_fields <- list(
  bar = c(w = "width", sub = "child"),
  bardiv = c(w = "width", pos = "positive"),
  box = c(wl = "whisker_low", w1 = "whisker_low_width", bl = "box_low",
          bw = "box_width", bc = "center", b2 = "box_high",
          w2 = "whisker_high_width", wh = "whisker_high", nn = "n"),
  pointrange = c(c = "center", l = "inner_low", rw = "inner_width", nn = "n",
                 ol = "outer_low", ow = "outer_width"),
  pair = c(a = "from", b = "to", l = "link_left", w = "link_width",
           bl = "band_left", bw = "band_width", dash = "dash", open = "open",
           fill = "fill"),
  sparkline = c(pl = "line", bd = "band", dx = "end_x", dy = "end_y",
                xs = "x_text", ys = "y_text", nn = "n")
)

tg_plain <- function(x) {
  table_groups_plain(vapply(x, function(e) e %||% NA_character_, ""))
}

# Geometry of one glyph (a plain column, or one level of a split one).
tg_expect_glyph <- function(mk, w, kind, label, level = FALSE) {
  fields <- tg_fields[[kind]]
  if (level) fields <- fields[names(fields) != "fill"]
  for (old in intersect(names(fields), names(w))) {
    new <- fields[[old]]
    expect_equal(tg_vec(mk[[new]]), tg_vec(w[[old]]), label = paste(label, old))
  }
  for (new in setdiff(fields, fields[names(w)])) {
    expect_null(mk[[new]], label = paste(label, new, "absent"))
  }
  if (!is.null(w[["tip"]])) {
    tip <- tg_plain(w[["tip"]])
    tip[!nzchar(tip)] <- NA
    expect_equal(tg_vec(mk$tip), tip, label = paste(label, "tip"))
  }
  if (!is.null(w[["r"]])) {
    stats <- c(bc = "center", bl = "low", bh = "high", wl = "whisker_low",
               wh = "whisker_high")
    expect_identical(names(mk$stats), unname(stats[names(w[["r"]])]))
    for (nm in names(w[["r"]])) {
      expect_equal(tg_vec(mk$stats[[stats[[nm]]]]), tg_vec(w[["r"]][[nm]]))
    }
  }
}

tg_expect_mark <- function(mk, w, label, pick) {
  kind <- w$kind
  expect_identical(mk$kind, kind, label = label)
  expect_identical(mk$dw, w$dw, label = paste(label, "dw"))
  if (kind %in% c("bar", "bardiv", "sparkline") ||
        (kind %in% c("box", "pointrange", "pair") && !isTRUE(w$multi))) {
    tg_expect_glyph(mk, w, kind, label)
  }
  if (identical(kind, "bar")) {
    expect_identical(mk$fill, w$fill)
  }
  if (identical(kind, "tile")) {
    expect_equal(tg_vec(mk$fill), tg_vec(w$bg))
    expect_equal(tg_vec(mk$ink), tg_vec(w$fg))
  }
  if (identical(kind, "barsplit")) {
    expect_identical(mk$mode, w$mode)
    expect_identical(mk$level_col, if (pick) w$scol)
    for (j in seq_along(w$names)) {
      lv <- mk$levels[[j]]
      expect_identical(lv$name, w$names[[j]])
      expect_identical(lv$color, w$fills[[j]])
      expect_equal(tg_vec(lv$width), tg_vec(w$seg[[j]]))
      expect_equal(tg_vec(lv$amount), tg_vec(w$segv[[j]]))
      expect_equal(tg_vec(lv$inside), tg_vec(w$slab[[j]]))
      expect_identical(lv$ink, w$sink[[j]])
      expect_equal(tg_vec(lv$label), tg_vec(w$ldisp[[j]]))
    }
  }
  if (isTRUE(w$multi)) {
    expect_length(mk$levels, length(w$lv))
    for (j in seq_along(w$lv)) {
      lv <- mk$levels[[j]]
      expect_identical(lv$name, w$levels[[j]])
      expect_identical(lv$color, w$fills[[j]])
      tg_expect_glyph(lv, w$lv[[j]], kind, paste(label, "level", j), TRUE)
      expect_equal(tg_vec(lv$label), tg_vec(w$lv[[j]]$disp))
    }
  }
  if (identical(kind, "pair")) {
    expect_identical(mk$ref, w$rf)
  }
  if (identical(kind, "sparkline")) {
    expect_identical(mk$ref_line, w$rc)
    expect_identical(mk$ref_band_top, w$rby)
    expect_identical(mk$ref_band_height, w$rbh)
  }
  if (identical(kind, "interval")) {
    expect_identical(isTRUE(mk$large), isTRUE(w$lg))
    expect_identical(isTRUE(mk$domain$date), isTRUE(w$dd))
    expect_equal(mk$domain$min, w$d0)
    expect_equal(mk$domain$max, w$d1)
    expect_equal(tg_vec(mk$fills), tg_vec(w$fills))
    for (i in seq_along(w$segs)) {
      expect_length(mk$spans[[i]], length(w$segs[[i]]))
      for (k in seq_along(w$segs[[i]])) {
        sg <- w$segs[[i]][[k]]
        sg[[3L]] <- sg[[3L]] - 1L
        if (length(sg) >= 4L) sg[[4L]] <- table_groups_plain(sg[[4L]])
        expect_equal(mk$spans[[i]][[k]], sg)
      }
      expect_identical(tg_vec(mk$tips[[i]]), table_groups_plain(tg_vec(w$tips[[i]])))
    }
  }
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
  if (isTRUE(w$text)) {
    expect_null(g$key)
  } else {
    expect_equal(tg_vec(g$key), tg_vec(w$v), label = paste(label, "key"))
  }
  expect_identical(!is.null(g$keys), pick && !is.null(w$fkey),
                   label = paste(label, "keys"))
  if (!is.null(g$keys)) {
    expect_identical(names(g$keys), w$fkey$column)
    expect_identical(tg_vec(g$keys[[1L]]), as.character(w$fkey$value))
  }
  mark <- w$kind != "num" || !is.null(w$pct) || isTRUE(w$failed)
  expect_identical(!is.null(g$mark), mark, label = paste(label, "mark"))
  if (mark) {
    tg_expect_mark(g$mark, w, label, pick)
    expect_equal(tg_vec(g$mark$pct), tg_vec(w$pct))
    tt <- w$tt
    h <- g$mark$hover
    expect_identical(h$head, tt$head)
    expect_identical(h$sub, tt$sub)
    expect_identical(h$measure, tt$meas)
    expect_identical(h$color_by, tt$cvar)
    expect_identical(h$base, tt$den)
    expect_identical(h$words, tt$words)
  }
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
        axis = tg_axis(xml2::xml_find_first(th, ".//div[contains(@class, 'blockr-summarize-axis')]"))
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

tg_expect_axis <- function(want, mark, label) {
  if (is.null(want)) {
    expect_null(mark$ticks, label = paste(label, "no strip"))
    return(invisible())
  }
  tk <- mark$ticks
  dom <- mark$domain
  pos <- round((tg_vec(tk$at) - dom$min) / (dom$max - dom$min) * 100, 2L)
  expect_equal(pos, want$pos, tolerance = 0.01, label = paste(label, "ticks"))
  expect_identical(tg_vec(tk$labels), want$labels, label = paste(label, "labels"))
  expect_identical(!is.null(mark$dw), want$has_val, label = paste(label, "slot"))
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
      expect_identical(h$span, w$span, label = paste(lab, "span"))
      expect_identical(h$rows, w$rows, label = paste(lab, "rows"))
      if (!is.null(w$col)) expect_identical(h$col, w$col, label = paste(lab, "col"))
      expect_identical(isTRUE(h$stub), w$stub, label = paste(lab, "stub"))
      if (!is.null(w$col) && w$col > 0L) {
        tg_expect_axis(w$axis, v$cols[[w$col + 1L]]$mark, lab)
      }
    }
  }
  # The bottom axis of the large swimlanes.
  foot <- xml2::xml_find_all(xml2::read_html(want$head),
                             "//tfoot//div[contains(@class, 'blockr-summarize-axis')]")
  large <- Filter(function(cc) isTRUE(cc$mark$large) && !is.null(cc$mark$ticks),
                  v$cols)
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
  expect_identical(v$fold, w$fold)
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

test_that("table_groups_view() draws what the reference sent", {
  cases <- tm_cases("groups")
  seen <- character()
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    want <- tm_payload(cs$messages, "blockr-viz-summarize-data")
    if (!identical(want$kind, "flat")) next
    m <- table_model_groups(cs$state, cs$data)
    v <- tm_json(table_groups_view(
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

test_that("colour-split lanes, hidden labels and a drill match the reference", {
  cs <- tm_cases("groups")$grp_all_types
  sm <- cs$state$summaries
  for (k in c(5L, 6L, 11L)) sm[[k]]$color <- "grp"
  sm[[2L]]$facet <- "grp"
  sm[[1L]]$facet <- "grp"
  sets <- list(
    list(summaries = sm, by = "cat", drill = "auto"),
    list(summaries = sm, by = c("grp", "cat"), drill = "auto",
         facet_layout = "by_level", column_axis = FALSE),
    list(summaries = sm, by = "cat", value_labels = FALSE)
  )
  for (k in seq_along(sets)) {
    st <- do.call(tg_state, c(list(cs), sets[[k]]))
    m <- table_model_groups(st, cs$data)
    expect_null(m$prep$err)
    ref <- tm_json(summarize_flat_payload(m$cells))
    v <- tm_json(table_groups_view(m, axis = !isFALSE(st$column_axis)))
    ref$head <- paste0("<table>", m$cells$thead, m$cells$tfoot, "</table>")
    ref$pick <- m$cells$pick
    tg_check(v, ref, paste("set", k))
    if (identical(st$facet_layout, "by_level")) {
      spans <- Filter(function(h) !is.null(h$span), v$head[[1L]])
      expect_length(spans, 2L)
      expect_identical(lapply(spans[[2L]]$keys, tg_vec), list(grp = "g2"))
    }
  }
})

test_that("a one-row table's columns are arrays on the wire", {
  cs <- tm_cases("groups")$grp_no_by
  v <- table_groups_view(table_model_groups(cs$state, cs$data))
  back <- tm_json(v)
  expect_type(back$cols[[1L]]$disp, "list")
  expect_type(back$cols[[2L]]$mark$width, "list")
  expect_type(back$cols[[2L]]$key, "list")
})

test_that("a failed preparation has no view", {
  expect_null(table_groups_view(list(prep = list(err = "x"), cells = NULL)))
})
