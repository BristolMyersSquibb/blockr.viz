# table_structured_view() (R/table-structured.R) against the first render of
# the structured table recordings. The reference sent the table as HTML; the
# model is compared with what that HTML says: cell texts in row order, header
# texts and spans, sections and where they start, indents, row styles,
# column widths and the drill claims.

ts_dash <- "—"

# A claim as the recorded JSON says it: `{column, value}` or `{column, values}`
# pairs, as `{column: [values]}`.
ts_claim_json <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(NULL)
  pairs <- jsonlite::fromJSON(x, simplifyVector = FALSE)
  out <- list()
  for (p in pairs) {
    v <- as.character(unlist(p$values %||% p$value))
    out[[p$column]] <- c(out[[p$column]], v)
  }
  out
}

ts_claim <- function(x) {
  if (!length(x)) return(NULL)
  lapply(x, function(v) as.character(unlist(v)))
}

# The recorded table, read for comparison.
ts_read <- function(html) {
  doc <- xml2::read_html(html)
  txt <- function(node) {
    for (b in xml2::xml_find_all(node, ".//br")) xml2::xml_text(b) <- "\n"
    gsub(" ", " ", xml2::xml_text(node), fixed = TRUE)
  }
  th <- xml2::xml_find_all(doc, "//thead/tr")
  head <- lapply(th, function(tr) {
    lapply(xml2::xml_find_all(tr, "./th"), function(h) {
      name <- xml2::xml_find_first(h, ".//span[contains(@class, 'arm__name')]")
      n <- xml2::xml_find_first(h, ".//span[contains(@class, 'arm__n ')]")
      text <- if (inherits(name, "xml_missing")) txt(h) else txt(name)
      sub <- if (inherits(n, "xml_missing")) NULL else txt(n)
      list(
        text = trimws(paste(c(text, sub), collapse = "\n")),
        span = as.integer(xml2::xml_attr(h, "colspan") %|na|% 1L),
        rows = as.integer(xml2::xml_attr(h, "rowspan") %|na|% 1L),
        col = xml2::xml_attr(h, "data-col-index"),
        keys = ts_claim_json(xml2::xml_attr(h, "data-dd-colkeys")),
        stub = grepl("blockr-stub-header", xml2::xml_attr(h, "class"))
      )
    })
  })
  body <- lapply(xml2::xml_find_all(doc, "//tbody/tr"), function(tr) {
    cls <- xml2::xml_attr(tr, "class")
    if (grepl("blockr-section-header", cls)) {
      lbl <- xml2::xml_find_first(tr, ".//span[@class='blockr-section-label']")
      return(list(
        section = TRUE, level = as.integer(xml2::xml_attr(tr, "data-level")),
        value = xml2::xml_text(
          xml2::xml_find_first(tr, ".//span[@class='blockr-section-value']")
        ),
        label = if (!inherits(lbl, "xml_missing")) xml2::xml_text(lbl),
        keys = ts_claim_json(xml2::xml_attr(tr, "data-dd-keys"))
      ))
    }
    list(
      section = FALSE,
      cells = vapply(xml2::xml_find_all(tr, "./td"), function(td) {
        if (grepl("blockr-dash", xml2::xml_attr(td, "class"))) ts_dash
        else xml2::xml_text(td)
      }, ""),
      indent = as.integer(xml2::xml_attr(tr, "data-indent")),
      strong = grepl("blockr-bold", cls), emph = grepl("blockr-italic", cls),
      keys = ts_claim_json(xml2::xml_attr(tr, "data-dd-keys"))
    )
  })
  widths <- as.numeric(sub("width: *([0-9.]+)px;?", "\\1",
                           xml2::xml_attr(xml2::xml_find_all(doc, "//col"),
                                          "style")))
  list(head = head, body = body, widths = widths)
}

`%|na|%` <- function(x, y) if (is.na(x)) y else x

# The body the model describes, in the order the browser draws it: section
# headers above the row they start at.
ts_body <- function(v) {
  out <- list()
  secs <- v$sections %||% list()
  at <- vapply(secs, function(s) s$at, 0L)
  for (i in seq_len(v$n)) {
    for (s in secs[at == i - 1L]) {
      out[[length(out) + 1L]] <- list(
        section = TRUE, level = s$level, value = s$value, label = s$label,
        keys = ts_claim(s$keys)
      )
    }
    cells <- vapply(v$cols, function(cc) {
      if (is.na(cc$disp[[i]])) ts_dash else cc$disp[[i]]
    }, "")
    out[[length(out) + 1L]] <- list(
      section = FALSE, cells = cells,
      indent = (v$rows$indent %||% integer(v$n))[[i]],
      strong = (v$rows$strong %||% logical(v$n))[[i]],
      emph = (v$rows$emph %||% logical(v$n))[[i]],
      keys = ts_claim(v$rows$keys[[i]])
    )
  }
  out
}

# The cell claim a click on column `j` of a keyed row adds, as the reference's
# colkeys map says it.
ts_expect_equal_body <- function(got, want, nm) {
  expect_length(got, length(want))
  for (k in seq_along(want)) {
    w <- want[[k]]
    g <- got[[k]]
    lab <- paste(nm, "body", k)
    expect_identical(g$section, w$section, label = lab)
    if (w$section) {
      expect_identical(g$level, w$level, label = paste(lab, "level"))
      expect_identical(g$value, w$value, label = paste(lab, "value"))
      expect_identical(g$label, w$label, label = paste(lab, "label"))
    } else {
      expect_identical(g$cells, w$cells, label = paste(lab, "cells"))
      expect_identical(g$indent, w$indent, label = paste(lab, "indent"))
      expect_identical(g$strong, w$strong, label = paste(lab, "strong"))
      expect_identical(g$emph, w$emph, label = paste(lab, "emph"))
    }
    expect_equal(g$keys, w$keys, label = paste(lab, "keys"))
  }
}

# The header the model describes, as the recorded rows read.
ts_head <- function(v) {
  lapply(v$head, function(row) {
    lapply(row, function(h) {
      list(
        text = paste(c(h$text, h$sub), collapse = "\n"),
        span = h$span %||% 1L, rows = h$rows %||% 1L,
        col = if (!is.null(h$col)) as.character(h$col),
        keys = ts_claim(h$keys), stub = isTRUE(h$stub)
      )
    })
  })
}

ts_expect_equal_head <- function(got, want, nm) {
  expect_length(got, length(want))
  for (r in seq_along(want)) {
    expect_length(got[[r]], length(want[[r]]))
    for (k in seq_along(want[[r]])) {
      w <- want[[r]][[k]]
      g <- got[[r]][[k]]
      lab <- paste(nm, "head", r, k)
      expect_identical(g$text, w$text, label = paste(lab, "text"))
      expect_identical(g$span, w$span, label = paste(lab, "span"))
      expect_identical(g$rows, w$rows, label = paste(lab, "rows"))
      expect_identical(g$stub, w$stub, label = paste(lab, "stub"))
      # The reference numbered the sortable cells only (the stub is not, and
      # a table with sorting off has none); the model names the column of
      # every leaf and leaves the flag to the browser.
      if (!is.na(w$col)) {
        expect_identical(g$col, w$col, label = paste(lab, "col"))
      }
      if (w$stub) expect_null(g$col, label = paste(lab, "stub col"))
      expect_equal(g$keys, w$keys, label = paste(lab, "keys"))
    }
  }
}

test_that("table_structured_view() draws what the reference sent", {
  n <- 0L
  skipped <- character()
  for (layout in c("records", "variables")) {
    cases <- tm_cases(layout)
    for (nm in names(cases)) {
      cs <- cases[[nm]]
      want <- tm_payload(cs$messages, "blockr-viz-table-data")
      if (!identical(want$kind, "html")) next
      frame <- if (layout == "records") {
        m <- table_model_records(cs$state, cs$data)
        if (!identical(m$kind, "structured")) next
        m$data
      } else {
        table_model_variables(cs$state, cs$data)
      }
      if (!grepl("<thead>", want$html, fixed = TRUE)) next
      v <- table_structured_view(
        frame, drill = cs$state$drill, digits = cs$state$format %||% "number",
        unit = cs$state$unit, collapsible = isTRUE(cs$state$collapsible)
      )
      ref <- ts_read(want$html)
      expect_equal(v$widths, ref$widths, label = paste(nm, "widths"))
      ts_expect_equal_head(ts_head(v), ref$head, nm)
      ts_expect_equal_body(ts_body(v), ref$body, nm)
      n <- n + 1L
    }
  }
  expect_gte(n, 8L)
})

test_that("a structured model survives the trip to the browser", {
  d <- table_golden_df()
  state <- list(by = "grp", rowname = "cat", format = "number", drill = "auto")
  m <- table_model_records(state, d)
  v <- table_structured_view(m$data, drill = "cat")
  j <- tm_json(v)
  expect_identical(j$n, nrow(m$data))
  expect_null(j$drill)
  expect_true(all(vapply(j$sections, function(s) is.character(s$value), NA)))
  k <- Filter(Negate(is.null), j$rows$keys)
  expect_gt(length(k), 0L)
  expect_true(all(vapply(k[[1L]], is.list, NA)))
})

test_that("a header label with HTML is plain text and a sub line", {
  d <- summary_table(table_golden_df(), vars = "cat", by = "grp")
  lab <- function(x) {
    table_structured_label(x, "fallback")
  }
  expect_identical(lab("<strong>Placebo</strong><br>N = 86"),
                   list(text = "Placebo", sub = "N = 86"))
  expect_identical(lab("Arm A\nN = 3"), list(text = "Arm A", sub = "N = 3"))
  expect_identical(lab("<65"), list(text = "<65"))
  expect_identical(lab("A &amp; B"), list(text = "A &amp; B"))
  expect_identical(lab("<b>A &amp; B</b>"), list(text = "A & B"))
  expect_identical(lab(NULL), list(text = "fallback"))
  expect_identical(lab(""), list(text = "fallback"))
  expect_true(table_is_structured(d))
})

test_that("spanners span and a leaf reaches the last header row", {
  d <- data.frame(.label = c("a", "b"), `Arm||F` = c("1", "2"),
                  `Arm||M` = c("3", "4"), All = c("5", "6"),
                  check.names = FALSE)
  names(d)[1L] <- ".label"
  v <- table_structured_view(d)
  expect_identical(v$stub, 0L)
  expect_length(v$head, 2L)
  top <- v$head[[1L]]
  expect_identical(top[[1L]], list(text = "", stub = TRUE, rows = 2L))
  expect_identical(top[[2L]], list(text = "Arm", span = 2L))
  expect_identical(top[[3L]], list(text = "All", rows = 2L, col = 3L))
  expect_identical(v$head[[2L]][[1L]], list(text = "F", col = 1L))
  expect_identical(v$head[[2L]][[2L]], list(text = "M", col = 2L))
})

test_that("a frame with nothing to draw has no model", {
  d <- data.frame(.label = character(), v = character())
  expect_null(table_structured_view(d))
  expect_null(table_structured_view(data.frame(.label = "a")))
})

# A frame no recording has: three header levels, spanner labels with a Big N,
# a pre-baked HTML leaf label, two section levels with a label and a missing
# value, a variable block, painted cells, column keys and per-row stub titles.
ts_frame <- function() {
  d <- data.frame(
    .label = c("Female", "Male", "Mean", "Total", "Other"),
    .indent = c(1L, 1L, 1L, 0L, 1L),
    .strong = c(FALSE, FALSE, FALSE, TRUE, FALSE),
    .emph = c(FALSE, FALSE, TRUE, FALSE, FALSE),
    .group1_level = c("SOC A", "SOC A", "SOC A", "SOC B", NA),
    .group2_level = c("PT 1", "PT 1", "PT 2", "PT 2", "PT 3"),
    .variable = c("SEX", "SEX", NA, "SEX", "SEX"),
    .variable_level = c("F", "M", NA, "T", NA),
    .variable_label = c("Sex", "Sex", "Age", "Age", "Age"),
    `Placebo||F||n` = c("1", "2", "3", NA, "5"),
    `Placebo||M||n` = c("<65", "2", "3", "4", "5"),
    `Drug X||n` = c("1", "2", "3", "4", "5"),
    Total = c("9", "9", "9", "9", "9"),
    `.bg:Total` = c("#FF0000", NA, NA, NA, "#00FF00"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(d$.label, "label") <- c("Characteristic", "n (%)")
  attr(d$.label, "label_bold") <- c(TRUE, FALSE)
  attr(d$.group1_level, "label") <- "System"
  attr(d$`Placebo||M||n`, "label") <- "<strong>Male</strong><br>N = 40"
  attr(d$Total, "label") <- "All\nN = 90"
  attr(d, "spanner_labels") <- c("Placebo" = "Placebo\nN = 80",
                                 "Placebo||F" = "<b>Female</b>")
  attr(d, "column_keys") <- list(
    "Placebo" = list(list(column = "ARM", values = "Placebo")),
    "Placebo||F" = list(list(column = "ARM", values = "Placebo"),
                        list(column = "SEX", values = "F")),
    "Placebo||F||n" = list(list(column = "ARM", values = "Placebo"),
                           list(column = "SEX", values = "F")),
    Total = list(list(column = "ARM", values = c("Placebo", "Drug X")))
  )
  d
}

test_that("the claims name their columns and keep single values as arrays", {
  v <- table_structured_view(ts_frame(), drill = "auto")
  expect_identical(v$rows$keys[[1L]],
                   list(.group1_level = I("SOC A"), .group2_level = I("PT 1"),
                        .variable_label = I("Sex"), .variable = I("SEX"),
                        .variable_level = I("F")))
  expect_null(v$rows$keys[[3L]])
  s <- Filter(function(s) s$level == 2L && s$at == 0L, v$sections)[[1L]]
  expect_identical(s$keys, list(.group1_level = I("SOC A"),
                                .group2_level = I("PT 1")))
  expect_identical(v$cols[[5L]]$keys,
                   list(ARM = I(c("Placebo", "Drug X"))))
  expect_identical(as.vector(v$cols[[5L]]$bg),
                   c("#FF0000", NA, NA, NA, "#00FF00"))
  expect_identical(v$cols[[2L]]$disp[[4L]], NA_character_)
  none <- table_structured_view(ts_frame(), drill = NULL)
  expect_null(none$rows$keys)
  expect_null(none$head[[1L]][[2L]]$keys)
})
