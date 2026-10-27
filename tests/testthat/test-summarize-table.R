# Summarize table: the data half (summarize_prepare) and the HTML contract.
#
# The JS half (search / sort / expand / row-click) is verified against the
# running board in dev/verify-summarize-block.R. The column list itself
# (normalization, marks, scope, by_level) is in test-lane-summaries.R.

ae_fixture <- function() {
  subj <- sprintf("S%03d", seq_len(60))
  arm <- stats::setNames(rep(c("Placebo", "Low", "High"), each = 20), subj)
  # Deterministic: term i is reported by subjects 1..(30 - 4i), so the ranking,
  # the parent totals and the arm split are all known up front.
  rows <- do.call(rbind, lapply(seq_len(5), function(i) {
    n <- 30L - 4L * i
    data.frame(
      USUBJID = subj[seq_len(n)],
      TERM = paste0("T", i),
      SOC = if (i <= 2L) "SOC A" else "SOC B",
      SEV = rep(c("MILD", "MODERATE"), length.out = n),
      AVAL = seq_len(n),
      stringsAsFactors = FALSE
    )
  }))
  rows$ARM <- factor(unname(arm[rows$USUBJID]),
                     levels = c("Placebo", "Low", "High"))
  rows$SEV <- factor(rows$SEV, levels = c("MILD", "MODERATE"))
  rows
}

# One summary column, as the gear writes it.
bar_col <- function(func = "count", col = NULL, ...) {
  c(list(type = "simple", func = func), if (!is.null(col)) list(col = col),
    list(show = "bar"), list(...))
}

# Subjects per term.
subj_bar <- function(...) {
  list(bar_col("count_distinct", "USUBJID", ...))
}

# The column of a facet copy, by its level.
facet_key <- function(p, level) {
  hit <- vapply(p$plan, function(x) identical(x$flevel, level), logical(1L))
  p$plan[[which(hit)[[1L]]]]$key
}

test_that("summarize_prepare ranks by the measure and counts subjects", {
  ae <- ae_fixture()
  p <- summarize_prepare(ae, by = "TERM", summaries = subj_bar())

  expect_null(p$err)
  expect_identical(p$rows$.label, paste0("T", 1:5))
  expect_identical(p$rows$.v, c(26, 22, 18, 14, 10))
  expect_identical(p$layout, "simple")
  # The bar is a share of the column's N, 26 subjects in all, so the widest
  # bar is the full track whatever rows are visible.
  e <- p$plan[[1L]]
  expect_identical(e$denom, 26L)
  expect_equal(e$dmax, 100)
})

test_that("rows can be ranked ascending", {
  ae <- ae_fixture()
  asc <- summarize_prepare(ae, by = "TERM", summaries = list(bar_col()),
                           sort_dir = "asc")
  expect_identical(asc$rows$.label, paste0("T", 5:1))
})

test_that("a parent is aggregated in its own pass, never summed", {
  ae <- ae_fixture()
  p <- summarize_prepare(ae, by = c("SOC", "TERM"), summaries = subj_bar())

  expect_null(p$err)
  expect_identical(sum(p$rows$.is_parent), 2L)
  soc_a <- p$rows$.v[p$rows$.is_parent & p$rows$.label == "SOC A"]
  kids_a <- sum(p$rows$.v[!p$rows$.is_parent & p$rows$.parent == "SOC A"])
  # SOC A = T1 (26 subjects) + T2 (22), all drawn from the same subject pool.
  expect_identical(soc_a, 26)
  expect_gt(kids_a, soc_a)
  # Children follow their own parent, in rank order.
  first_kids <- p$rows$.label[p$rows$.parent == p$rows$.label[1L]]
  expect_identical(first_kids[!is.na(first_kids)], c("T1", "T2"))
})

test_that("a colour split adds one segment per level, summing to the row", {
  ae <- ae_fixture()
  p <- summarize_prepare(ae, by = "TERM",
                         summaries = list(bar_col(color = "SEV")))

  e <- p$plan[[1L]]
  expect_identical(e$kind, "barsplit")
  expect_identical(p$series, c("MILD", "MODERATE"))
  segs <- rowSums(p$rows[, paste0(e$prefix, c("MILD", "MODERATE"))])
  expect_equal(unname(segs), p$rows$.v)
  expect_identical(unname(p$palette[["MILD"]]), dd_palette(1L))
})

test_that("a split of a non-additive measure groups instead of stacking", {
  ae <- ae_fixture()
  # Stacking mean(MILD) on mean(MODERATE) would draw a bar nothing computes,
  # and scale the axis to that non-number. Side by side, each segment is a
  # real mean.
  p <- summarize_prepare(ae, by = "TERM", summaries = list(
    bar_col("mean", "AVAL", color = "SEV")
  ))
  e <- p$plan[[1L]]
  expect_identical(e$mode, "grouped")
  # The axis reaches the widest SEGMENT, never the sum of them.
  cells <- unlist(p$rows[, paste0(e$prefix, c("MILD", "MODERATE"))])
  expect_equal(e$dmax, max(cells))
  # Side by side, each level prints its own mean; the cell has no one total.
  m <- summarize_cells(p)
  expect_null(m$cols[[1]]$disp)
  expect_length(m$cols[[1]]$ldisp, 2L)

  # The additive measures stack: their parts really do sum to the whole.
  cnt <- summarize_prepare(ae, by = "TERM",
                           summaries = list(bar_col(color = "SEV")))
  expect_identical(cnt$plan[[1]]$mode, "stacked")
  # The domain is in percent of N, the unit the bars are drawn in.
  expect_equal(cnt$plan[[1]]$dmax,
               max(cnt$rows$.v) / cnt$plan[[1]]$denom * 100)

  # A count can be asked to sit side by side, too.
  g <- summarize_prepare(ae, by = "TERM", summaries = list(
    bar_col(color = "SEV", position = "dodge")
  ))
  expect_identical(g$plan[[1]]$mode, "grouped")
})

test_that("faceting gives one bar column per level with its own denominator", {
  ae <- ae_fixture()
  p <- summarize_prepare(ae, by = "TERM",
                         summaries = subj_bar(facet = "ARM"))

  expect_identical(p$layout, "facet")
  # "High" is a level of the factor with no rows in this fixture: an empty
  # column, left out as the chart leaves out the panel.
  expect_identical(p$facet_levels, c("Placebo", "Low"))
  bars <- vapply(p$plan, function(x) identical(x$kind, "bar"), logical(1L))
  expect_identical(sum(bars), 2L)
  # Each arm's percentage is over that arm's own N, never the pooled total.
  expect_identical(p$plan[[1L]]$sub_label, "N = 20")
  expect_identical(p$plan[[1L]]$denom, 20L)
  expect_identical(p$plan[[2L]]$denom, 6L)
  # The copies share one percentage scale.
  expect_true(all(vapply(p$plan, function(x) x$dmax, 1) <= 100))
  # T1 is reported by every Placebo subject: a full bar in that arm.
  expect_identical(p$rows[[facet_key(p, "Placebo")]][[1L]], 20)
})

test_that("facet and colour compose: split bars inside each facet column", {
  ae <- ae_fixture()
  S <- list(bar_col(facet = "ARM", color = "SEV"))
  p <- summarize_prepare(ae, by = "TERM", summaries = S)
  expect_identical(p$layout, "facet")
  expect_null(p$note)
  splits <- Filter(function(x) identical(x$kind, "barsplit"), p$plan)
  # "High" has no rows (see above), so it has no column
  expect_length(splits, 2L)
  expect_identical(splits[[1]]$label, "Placebo")
  expect_identical(as.character(splits[[1]]$series), c("MILD", "MODERATE"))
  # Column keys are facet-INDEXED so level names can never collide.
  expect_identical(splits[[2]]$prefix, ".s1f2_S_")
  # Each facet cell's segments sum to that facet's own total.
  segs <- p$rows[[".s1f1_S_MILD"]] + p$rows[[".s1f1_S_MODERATE"]]
  expect_equal(unname(segs), p$rows[[facet_key(p, "Placebo")]])
  # The palette encodes the COLOUR levels, and the legend says so.
  expect_identical(names(p$palette), c("MILD", "MODERATE"))
  expect_identical(summarize_legend_spec(p)$groups[[1L]]$title, "SEV")
  # A labelled colour column titles the legend by its label.
  attr(ae$SEV, "label") <- "Severity"
  p <- summarize_prepare(ae, by = "TERM", summaries = S)
  expect_identical(summarize_legend_spec(p)$groups[[1L]]$title, "Severity")
})

test_that("a plain facet is colour-neutral: no per-level hues, no legend", {
  ae <- ae_fixture()
  p <- summarize_prepare(ae, by = "TERM",
                         summaries = subj_bar(facet = "ARM"))
  bars <- Filter(function(x) identical(x$kind, "bar"), p$plan)
  expect_true(all(vapply(bars, function(x) identical(x$fill, dd_palette(1L)),
                         logical(1L))))
  expect_null(summarize_legend_spec(p))
})

test_that("a BLANK colour level legends as (Missing) instead of throwing", {
  # Real data codes a missing arm / severity as "", and `pal[[""]]` is a
  # subscript error in R even when the entry is there -- the legend must
  # never look a level up by name.
  ae <- ae_fixture()
  ae$SEV <- as.character(ae$SEV)
  ae$SEV[seq(1L, nrow(ae), by = 7L)] <- ""
  p <- summarize_prepare(ae, by = "TERM",
                         summaries = list(bar_col(color = "SEV")))
  expect_identical(names(p$palette), c("", "MILD", "MODERATE"))
  spec <- summarize_legend_spec(p)
  expect_identical(
    vapply(spec$groups[[1L]]$items, `[[`, character(1L), "label"),
    c("(Missing)", "MILD", "MODERATE")
  )
  # Every swatch keeps a real colour -- no `background:NA`.
  expect_true(all(vapply(spec$groups[[1L]]$items,
                         function(it) nzchar(it$color %||% ""), logical(1L))))
})

test_that("a bar column carries its own value label; a number column is another", {
  ae <- ae_fixture()
  # The bar plan entry wants its label, with the share of N beside it.
  p <- summarize_prepare(ae, by = "TERM", summaries = list(bar_col()))
  expect_length(p$plan, 1L)
  expect_true(isTRUE(p$plan[[1]]$show_val))
  expect_identical(p$plan[[1]]$val_denom, nrow(ae))
  m <- summarize_cells(p)
  expect_identical(m$cols[[1]]$disp[[1]], "26")
  expect_match(m$cols[[1]]$pct[[1]], "^\\(\\d+%\\)$")
  expect_true(m$cols[[1]]$dw >= nchar("26 (43%)") - 1L)

  # A number column beside it is a column of its own; the bar keeps its label.
  pc <- summarize_prepare(ae, by = "TERM", summaries = list(
    bar_col(), list(type = "simple", func = "count", show = "number")
  ))
  expect_identical(
    vapply(pc$plan, function(x) x$kind, ""), c("bar", "num")
  )
  expect_true(isTRUE(pc$plan[[1]]$show_val))
  mc <- summarize_cells(pc)
  expect_identical(mc$cols[[1]]$disp, m$cols[[1]]$disp)
  expect_identical(trimws(mc$cols[[2]]$disp), m$cols[[1]]$disp)
})

test_that("field columns ride as raw text, sorting on the text", {
  subj <- data.frame(
    USUBJID = c("S1", "S2", "S3"),
    AVAL = c(4, 2, 8),
    AGE = c(61L, 70L, 55L),
    ARM = c("Placebo", "High", "High"),
    stringsAsFactors = FALSE
  )
  p <- summarize_prepare(subj, by = "USUBJID", summaries = list(
    bar_col("identity", "AVAL"),
    list(type = "field", col = "ARM"), list(type = "field", col = "AGE")
  ))
  expect_null(p$err)
  kinds <- vapply(p$plan, function(x) x$kind, "")
  expect_identical(kinds, c("bar", "num", "num"))
  expect_true(isTRUE(p$plan[[2]]$text))     # fields are text
  expect_true(isTRUE(p$plan[[3]]$text))
  # Rows arrive ranked by AVAL: S3, S1, S2.
  expect_identical(p$rows$.label, c("S3", "S1", "S2"))
  expect_identical(p$rows[[p$plan[[2]]$key]], c("High", "Placebo", "High"))
  m <- summarize_cells(p)
  expect_identical(as.character(m$cols[[2]]$disp), c("High", "Placebo", "High"))
  h <- summarize_cells_html(m)
  expect_match(h, "blockr-summarize-txt")
  expect_match(h, 'data-v="High"', fixed = TRUE)
})

test_that("top_n caps with a reported fold, and is off by default", {
  ae <- ae_fixture()
  capped <- summarize_prepare(ae, by = "TERM", summaries = list(bar_col()),
                              top_n = 2)
  expect_identical(nrow(capped$rows), 2L)
  expect_identical(capped$folded, 3L)
  expect_identical(capped$n_total, 5L)

  full <- summarize_prepare(ae, by = "TERM", summaries = list(bar_col()))
  expect_identical(nrow(full$rows), 5L)
  expect_identical(full$folded, 0L)
})

test_that("an optional dim whose column vanished reads as unmapped", {
  ae <- ae_fixture()
  # An upstream picker's "(none)" drops the column from the DATA; the saved
  # mapping must self-heal (chart parity), not error the whole table.
  p <- summarize_prepare(ae, by = "TERM", summaries = list(
    bar_col(facet = "GONE_FACET", color = "GONE_COLOR")
  ))
  expect_null(p$err)
  expect_identical(p$layout, "simple")
  expect_null(p$facet)
  expect_null(p$color)

  # One dim missing, the other present: only the missing one drops.
  ph <- summarize_prepare(ae, by = "TERM", summaries = list(
    bar_col(facet = "GONE", color = "SEV")
  ))
  expect_null(ph$err)
  expect_identical(ph$layout, "simple")
  expect_identical(ph$plan[[1L]]$kind, "barsplit")

  # REQUIRED columns still report by name.
  expect_match(summarize_prepare(ae, by = "GONE",
                                 summaries = list(bar_col()))$err, "GONE")
  expect_match(
    summarize_prepare(ae, by = "TERM",
                      summaries = list(bar_col("identity", "GONE")))$err,
    "GONE"
  )
})

test_that("a bad config is a message, never an error", {
  ae <- ae_fixture()
  expect_match(summarize_prepare(ae, by = "GONE")$err, "GONE")
  expect_match(
    summarize_prepare(ae, by = "TERM", summaries = list(bar_col("mean")))$err,
    "`col` is required"
  )
  expect_identical(summarize_prepare(ae[0, ], by = "TERM")$err,
                   "No rows to display")
  # A one-level facet (one arm left after a filter) draws, it is not an error.
  one_arm <- droplevels(ae[ae$ARM == "Placebo", ])
  expect_null(
    summarize_prepare(one_arm, by = "TERM",
                      summaries = list(bar_col(facet = "ARM")))$err
  )
})

test_that("a table without `by` is one Total row, not a message", {
  ae <- ae_fixture()
  p <- summarize_prepare(ae)
  expect_null(p$err)
  expect_identical(p$rows$.label, "Total")
  expect_true(p$all)
  # Empty `summaries` counts rows.
  expect_equal(p$rows$.v, nrow(ae))
})

test_that("identity ranks a per-group value as-is, like the chart's None (as is)", {
  # The headline use: one pre-computed value per subject, stratified the same
  # way the chart block does it (colour split, facet columns).
  subj <- data.frame(
    USUBJID = c("S1", "S2", "S3", "S4"),
    AVAL = c(4, NA, 2, 8),
    SEX = factor(c("F", "M", "F", "M")),
    ARM = factor(c("Placebo", "Placebo", "High", "High")),
    stringsAsFactors = FALSE
  )
  as_is <- function(...) list(bar_col("identity", "AVAL", ...))

  p <- summarize_prepare(subj, by = "USUBJID", summaries = as_is())
  expect_null(p$err)
  expect_identical(p$rows$.label, c("S4", "S1", "S3", "S2"))
  expect_identical(p$rows$.v, c(8, 4, 2, NA))
  # The value column names the bar itself; no percentage exists.
  expect_identical(p$plan[[1L]]$label, "AVAL")
  expect_null(p$plan[[1L]]$denom)

  # An all-NA group stays NA; duplicates collapse to the first non-missing.
  dup <- rbind(subj, data.frame(USUBJID = "S2", AVAL = 6, SEX = factor("M"),
                                ARM = factor("Placebo")))
  pd <- summarize_prepare(dup, by = "USUBJID", summaries = as_is())
  expect_identical(pd$rows$.v[pd$rows$.label == "S2"], 6)

  # Faceted identity: raw shared scale, no arm-N denominator.
  pf <- summarize_prepare(subj, by = "SEX", summaries = as_is(facet = "ARM"))
  expect_null(pf$err)
  bars <- Filter(function(x) identical(x$kind, "bar"), pf$plan)
  expect_length(bars, 2L)
  expect_true(all(vapply(bars, function(x) is.null(x$denom), logical(1L))))
  expect_true(all(vapply(bars, function(x) x$dmax, 1) == 8))

  # A subject has NO value in an arm they are not in: blank (NA), not the 0 a
  # counting measure fills (the chart's null gap).
  pp <- summarize_prepare(subj, by = "USUBJID", summaries = as_is(facet = "ARM"))
  s1 <- pp$rows$.label == "S1"
  expect_true(is.na(pp$rows[[facet_key(pp, "High")]][s1]))
  expect_identical(pp$rows[[facet_key(pp, "Placebo")]][s1], 4)

  # No value picked is a message, not an error.
  expect_match(
    summarize_prepare(subj, by = "USUBJID",
                      summaries = list(bar_col("identity")))$err,
    "`col` is required"
  )
})

markup <- function(ae, ...) {
  # Drop the inlined <style> blocks: their rule text mentions the same class
  # names as the markup, which would make every grepl trivially true.
  h <- as.character(htmltools::renderTags(summarize_table(ae, ...))$html)
  gsub("<style>.*?</style>", "", h)
}

test_that("the chrome uses the canonical title / caption bands", {
  ae <- ae_fixture()
  h <- markup(ae, by = "TERM", summaries = list(bar_col()), title = "T",
              subtitle = "S", caption = "C")
  # .dd-table-titles / .dd-table-caption are the chart and table blocks' bands
  # (inst/css/table.css), so the control row keeps its height and the title sits
  # between it and the column headers.
  expect_match(h, "dd-table-titles")
  expect_match(h, "dd-table-title")
  expect_match(h, "dd-table-subtitle")
  expect_match(h, "dd-table-caption")
  # The control row carries the search only; the JS hoists it next to the gear.
  expect_match(h, "blockr-html-table-toolbar")
})

test_that("header sub-lines carry the real data labels", {
  ae <- ae_fixture()
  attr(ae$TERM, "label") <- "Preferred term"
  h <- markup(ae, by = "TERM", summaries = subj_bar())
  expect_match(h, "Preferred term")          # the group column's own label
  expect_match(h, "Distinct USUBJID")        # what the column counts
  expect_match(h, "N = 26")                  # over how many
  expect_match(h, "blockr-col-label")        # the table block's label class
})

test_that("the HTML carries the chrome, the marks and the drill contract", {
  ae <- ae_fixture()
  html <- function(...) markup(ae, ...)

  h <- html(by = "TERM", summaries = subj_bar(),
            title = "Ranked", subtitle = "N = {n_distinct(USUBJID)}",
            caption = "Source: fixture", drill = "TERM", elem_id = "blk-1")
  expect_match(h, "dd-table-title")
  expect_match(h, "N = 26", fixed = TRUE)      # the {...} token resolved
  expect_match(h, "dd-table-caption")
  expect_match(h, 'data-summarize-drill="TERM"', fixed = TRUE)
  expect_match(h, 'data-summarize-elem-id="blk-1"', fixed = TRUE)
  expect_match(h, "is-pick")                    # rows are clickable
  expect_match(h, 'data-v="', fixed = TRUE)     # numbers the client sorts on
  expect_match(h, "blockr-summarize-fill")

  # No drill = no click affordance.
  expect_false(grepl("is-pick", html(by = "TERM", summaries = list(bar_col()))))
})

test_that("the HTML marks nested and split shapes distinctly", {
  ae <- ae_fixture()
  html <- function(...) markup(ae, ...)

  nested <- html(by = c("SOC", "TERM"), summaries = list(bar_col()))
  expect_match(nested, 'data-summarize-nested="1"', fixed = TRUE)
  # The collapse affordance is the table block's own chevron: same button, same
  # svg, same rotation contract (the ROW carries `collapsed`).
  expect_match(nested, "blockr-indent-btn")
  expect_match(nested, "blockr-chev")
  expect_match(nested, "blockr-indent-toggle collapsed")
  expect_match(nested, "is-sub")   # child bars are the subordinate step
  # Child labels start on their parent's name: no padding of their own.
  expect_match(nested, "is-child", fixed = TRUE)
  expect_no_match(nested, "padding-left:48px", fixed = TRUE)

  side <- list(bar_col(color = "SEV", position = "dodge"))
  grouped <- html(by = "TERM", summaries = side)
  # Each level its own thin track with its value beside it; with the
  # values off, the levels stack as thin rows in one tall track.
  expect_match(grouped, "blockr-summarize-track is-lv", fixed = TRUE)
  bare <- html(by = "TERM", summaries = side, value_labels = FALSE)
  expect_match(bare, "blockr-summarize-row3")
})

test_that("title tiers follow the chart and table contract", {
  ae <- ae_fixture()
  attr(ae, "label") <- "Events fixture"
  html <- function(...) markup(ae, ...)
  # NULL = auto (the input's own label), "" = explicitly none.
  S <- list(bar_col())
  expect_match(html(by = "TERM", summaries = S, title = NULL),
               "Events fixture")
  expect_false(grepl("dd-table-title",
                     html(by = "TERM", summaries = S, title = "")))
})

test_that("a config that cannot be honored renders a message table", {
  ae <- ae_fixture()
  h <- markup(ae, by = "GONE")
  expect_match(h, "Mapped column not in data")
  expect_false(grepl("blockr-summarize-fill", h))
})

test_that("the old constructor builds a table block in the groups layout", {
  blk <- new_summarize_table_block(group = "TERM", func = "count_distinct",
                                   id_var = "USUBJID", drill = "TERM")
  expect_s3_class(blk, "table_block")
  expect_s3_class(blk, "transform_block")

  st <- blockr.core::blockr_ser(blk)$payload
  expect_identical(st$layout, "groups")
  expect_identical(st$by, "TERM")
  expect_identical(st$id, "USUBJID")
  expect_identical(st$drill, "TERM")
  expect_identical(st$summaries[[1L]]$func, "count_distinct")

  # The click survives a save: one `selection` argument carries it.
  expect_true("selection" %in% names(formals(new_table_block)))

  # Only the table block is in the block-adder and the assistant universe.
  expect_true("table_block" %in% names(blockr.core::available_blocks()))
  expect_false("summarize_table_block" %in%
                 names(blockr.core::available_blocks()))
})

test_that("the single bar's old arguments still read as the column list", {
  ae <- ae_fixture()
  # Same table through the old names and through `by` + `summaries`: the
  # translation lands on one bar column with its colour split.
  old <- markup(ae, group = "TERM", parent = "SOC", func = "count_distinct",
                id_var = "USUBJID", color = "SEV", bar_mode = "grouped")
  new <- markup(ae, by = c("SOC", "TERM"), summaries = subj_bar(
    color = "SEV", position = "dodge"
  ))
  expect_identical(old, new)

  # The exhibit translates too, extra count column included.
  ex <- static_summarize_table(ae, group = "TERM", func = "mean",
                               value = "AVAL", cols = "n")
  expect_identical(vapply(ex$prep$plan, function(x) x$kind, ""),
                   c("bar", "num"))
  expect_identical(ex$prep$rows$.label, paste0("T", 1:5))
})

test_that("only a count carries an N and a share", {
  ae <- ae_fixture()
  p <- summarize_prepare(ae, by = "TERM", summaries = list(
    bar_col("mean", "AVAL"), bar_col()
  ))
  expect_null(p$plan[[1L]]$denom)
  expect_null(p$plan[[1L]]$val_denom)
  expect_identical(p$plan[[2L]]$val_denom, nrow(ae))
})

test_that("html escaping survives a label with markup in it", {
  ae <- ae_fixture()
  ae$TERM[ae$TERM == "T1"] <- "<b>T1</b>"
  h <- markup(ae, by = "TERM", summaries = list(bar_col()))
  expect_match(h, "&lt;b&gt;T1&lt;/b&gt;", fixed = TRUE)
  expect_false(grepl("<b>T1</b>", h, fixed = TRUE))
})

test_that("sorting can be turned off: no hook class, no arrow", {
  d <- data.frame(AVISIT = c("Baseline", "Week 2", "Week 2"),
                  AVAL = c(1, 2, 3), stringsAsFactors = FALSE)
  prep <- summarize_prepare(d, by = "AVISIT", summaries = list(bar_col()))

  on <- summarize_thead(prep)
  expect_match(on, "blockr-sortable")
  expect_match(on, "blockr-sort-icon")

  off <- summarize_thead(prep, sortable = FALSE)
  expect_false(grepl("blockr-sortable", off, fixed = TRUE))
  expect_false(grepl("blockr-sort-icon", off, fixed = TRUE))

  # The cell model carries the toggle through cfg, and so does the ctor.
  m <- summarize_cells(prep, cfg = list(sortable = FALSE))
  expect_false(grepl("blockr-sortable", m$thead, fixed = TRUE))
  expect_true("sortable" %in% names(formals(new_table_block)))
})

test_that("every row carries the order it arrived in", {
  d <- data.frame(AVISIT = c("Baseline", "Week 2", "Week 10"),
                  AVISITN = c(0, 2, 10), stringsAsFactors = FALSE)
  prep <- summarize_prepare(d, by = "AVISIT", summaries = list(bar_col()),
                            sort_by = "AVISITN", sort_dir = "asc")
  html <- summarize_cells_html(summarize_cells(prep))
  ord <- regmatches(html, gregexpr("data-summarize-ord=\"[0-9]+\"", html))[[1L]]
  expect_identical(ord, paste0("data-summarize-ord=\"", 0:2, "\""))
})

test_that("the glyph, not the cell, carries the column's minimum width", {
  # A cell minimum is spent on the value label first, so a facet with many
  # levels used to squeeze every lane down to the leftovers. The floor rides
  # the lane instead, and the table scrolls sideways when it no longer fits.
  css <- summarize_table_css()
  expect_match(css, "--blockr-summarize-lane-min:\\s*\\d+px")
  rule <- regmatches(
    css,
    regexpr("\\.blockr-summarize-barwrap \\.blockr-summarize-track,[^}]*}", css)
  )
  expect_length(rule, 1L)
  expect_match(rule, "min-width: var(--blockr-summarize-lane-min", fixed = TRUE)
})

test_that("bar_width: fit needs no hook, a preset stamps the table", {
  ae <- data.frame(SOC = c("A", "A", "B"), TRT = c("x", "y", "x"))
  html <- function(...) {
    as.character(summarize_table(ae, by = "SOC", ...))
  }
  # Fit is the stylesheet's default (the lanes fill, up to the ceiling).
  expect_false(grepl("data-summarize-width=\"", html(), fixed = TRUE))
  expect_match(html(bar_width = "medium"), "data-summarize-width=\"medium\"",
               fixed = TRUE)
  # The gear reads the value back off the cfg, normalised.
  expect_match(html(bar_width = "wide"), "&quot;bar_width&quot;:&quot;wide&quot;",
               fixed = TRUE)
  expect_identical(summarize_bar_width(NULL), "fit")
  expect_identical(summarize_bar_width("huge"), "fit")
  expect_identical(summarize_bar_width("narrow"), "narrow")

  css <- summarize_table_css()
  expect_match(css, "--blockr-summarize-lane-max:\\s*\\d+px")
  for (w in c("narrow", "medium", "wide")) {
    expect_match(css, paste0("data-summarize-width='", w, "'"), fixed = TRUE)
  }
})

test_that("only a labelled column's axis follows the bar width", {
  bar <- as.character(summarize_axis_strip(
    list(kind = "bar", dmin = 0, dmax = 10), list(dw = 6L)
  ))
  expect_match(bar, "blockr-summarize-axis has-val", fixed = TRUE)
  span <- as.character(
    summarize_axis_strip(list(kind = "interval", dom = c(0, 9)))
  )
  expect_false(grepl("has-val", span))
})

test_that("the chrome shell keeps the search box when search is off", {
  # The gear turns search on and off through the payload; re-rendering the
  # container would close the open gear.
  html <- as.character(summarize_chrome_shell(
    search = FALSE, elem_id = "x", download = htmltools::span()
  ))
  expect_match(html, "<input[^>]*class=\"blockr-search\"[^>]*display:none")
})

test_that("a colour level no row carries gets no slot, and no colour shifts", {
  ae <- ae_fixture()
  ae$ARM <- factor(as.character(ae$ARM), levels = c("Placebo", "High", "Low"))
  # "High" has no rows in the fixture; the factor still lists it.
  p <- summarize_prepare(ae, by = "TERM", summaries = list(
    bar_col(color = "ARM", position = "dodge")
  ))
  split <- Filter(function(x) identical(x$kind, "barsplit"), p$plan)[[1]]
  expect_identical(as.character(split$series), c("Placebo", "Low"))
  expect_identical(unname(p$palette), dd_palette()[c(1L, 3L)])
})

test_that("a colour level only ungrouped rows carry gets no slot", {
  ae <- ae_fixture()
  ae$ARM <- factor(as.character(ae$ARM), levels = c("Placebo", "Low", "High"))
  # A population join's subject with no record: no term, but an arm.
  pop <- ae[1L, ]
  pop$TERM <- NA_character_
  pop$ARM <- factor("High", levels = levels(ae$ARM))
  d <- rbind(ae, pop)
  p <- summarize_prepare(d, by = "TERM", summaries = list(
    bar_col(color = "ARM", position = "dodge")
  ))
  split <- Filter(function(x) identical(x$kind, "barsplit"), p$plan)[[1]]
  expect_identical(as.character(split$series), c("Placebo", "Low"))
  S <- list(list(type = "simple", name = "Subjects", func = "count_distinct",
                 col = "USUBJID", show = "bar"))
  l <- lane_prepare_summaries(d, by = "TERM", summaries = S, color = "ARM")
  expect_identical(as.character(l$plan[[1L]]$series), c("Placebo", "Low"))
})

test_that("table.css names only classes the summarize table draws", {
  # The rank -> summarize rename (0.2.194) left table.css on the old class
  # names, and nothing failed: the title band fell out of the gear row and the
  # shared table styles stopped applying, on every summarize table.
  sheet <- paste(readLines(system.file("css", "table.css", package = "blockr.viz")),
                 collapse = "\n")
  used <- unique(regmatches(
    sheet, gregexpr("blockr-(rank|summarize)-[a-z-]+", sheet)
  )[[1]])
  expect_gt(length(used), 0L)
  drawn <- paste(
    as.character(summarize_chrome_shell(
      elem_id = "x", download = htmltools::span()
    )),
    paste(readLines(system.file("js", "summarize-table.js",
                                package = "blockr.viz")), collapse = "\n")
  )
  for (cls in used) {
    expect_true(grepl(cls, drawn, fixed = TRUE), info = cls)
  }
})
