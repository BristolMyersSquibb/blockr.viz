# Ranked bar table: the data half (rank_prepare) and the HTML contract.
#
# The JS half (search / sort / expand / row-click) is verified against the
# running board in dev/verify-rank-block.R.

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

test_that("rank_prepare ranks by the measure and counts distinct subjects", {
  ae <- ae_fixture()
  p <- rank_prepare(ae, group = "TERM", func = "count_distinct",
                    id_var = "USUBJID")

  expect_null(p$err)
  expect_identical(p$rows$.label, paste0("T", 1:5))
  expect_identical(p$rows$.v, c(26, 22, 18, 14, 10))
  expect_identical(p$layout, "simple")
  # The bar scale is the whole column's max, not the visible rows' -- otherwise
  # scrolling or searching would rescale the bars.
  expect_identical(p$bar_max, 26)
})

test_that("rank_prepare sorts by label and ascending on request", {
  ae <- ae_fixture()
  asc <- rank_prepare(ae, group = "TERM", func = "count", sort_dir = "asc")
  expect_identical(asc$rows$.label, paste0("T", 5:1))

  lab <- rank_prepare(ae, group = "TERM", func = "count", sort_by = "label",
                      sort_dir = "asc")
  expect_identical(lab$rows$.label, paste0("T", 1:5))
})

test_that("a parent is aggregated in its own pass, never summed", {
  ae <- ae_fixture()
  p <- rank_prepare(ae, group = "TERM", parent = "SOC",
                    func = "count_distinct", id_var = "USUBJID")

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

test_that("a colour split adds one series column per level, summing to the row", {
  ae <- ae_fixture()
  p <- rank_prepare(ae, group = "TERM", color = "SEV", func = "count")

  expect_identical(p$layout, "split")
  expect_identical(p$series, c("MILD", "MODERATE"))
  segs <- rowSums(p$rows[, c(".s_MILD", ".s_MODERATE")])
  expect_equal(unname(segs), p$rows$.v)
  expect_identical(unname(p$palette[["MILD"]]), dd_palette(1L))
})

test_that("faceting gives one bar column per level with its own denominator", {
  ae <- ae_fixture()
  p <- rank_prepare(ae, group = "TERM", facet = "ARM",
                    func = "count_distinct", id_var = "USUBJID")

  expect_identical(p$layout, "facet")
  expect_identical(p$facet_levels, c("Placebo", "Low", "High"))
  bars <- vapply(p$plan, function(x) identical(x$kind, "bar"), logical(1L))
  expect_identical(sum(bars), 3L)
  # Each arm's percentage is over that arm's own N, never the pooled total.
  expect_identical(unname(p$denoms[["Placebo"]]), 20L)
  expect_true(p$bar_max <= 100)
})

test_that("facet takes precedence over colour, and says so", {
  ae <- ae_fixture()
  p <- rank_prepare(ae, group = "TERM", facet = "ARM", color = "SEV",
                    func = "count")
  expect_identical(p$layout, "facet")
  expect_match(p$note, "ignoring the color split")
})

test_that("compare gives a signed difference in points per non-comparator arm", {
  ae <- ae_fixture()
  p <- rank_prepare(ae, group = "TERM", facet = "ARM", compare = "Placebo",
                    func = "count_distinct", id_var = "USUBJID")

  expect_identical(p$layout, "compare")
  expect_named(p$rows[grep("^\\.d_", names(p$rows))], c(".d_Low", ".d_High"))
  manual <- p$rows$.f_High / p$denoms[["High"]] * 100 -
    p$rows$.f_Placebo / p$denoms[["Placebo"]] * 100
  expect_equal(p$rows$.d_High, manual)
})

test_that("compare rejects a bad comparator and a non-counting measure", {
  ae <- ae_fixture()
  expect_match(
    rank_prepare(ae, group = "TERM", facet = "ARM", compare = "Nope",
                 func = "count")$err,
    "not a level"
  )
  expect_match(
    rank_prepare(ae, group = "TERM", facet = "ARM", compare = "Placebo",
                 func = "mean", value = "AVAL")$err,
    "counting measure"
  )
})

test_that("top_n caps with a reported fold, and is off by default", {
  ae <- ae_fixture()
  capped <- rank_prepare(ae, group = "TERM", func = "count", top_n = 2)
  expect_identical(nrow(capped$rows), 2L)
  expect_identical(capped$folded, 3L)
  expect_identical(capped$n_total, 5L)

  full <- rank_prepare(ae, group = "TERM", func = "count")
  expect_identical(nrow(full$rows), 5L)
  expect_identical(full$folded, 0L)
})

test_that("a bad config is a message, never an error", {
  ae <- ae_fixture()
  expect_identical(rank_prepare(ae, group = NULL)$err,
                   "Pick a Rank by column in the gear")
  expect_match(rank_prepare(ae, group = "GONE")$err, "GONE")
  expect_match(rank_prepare(ae, group = "TERM", func = "count_distinct")$err,
               "Subject id")
  expect_match(rank_prepare(ae, group = "TERM", func = "mean")$err,
               "Value column")
  expect_identical(rank_prepare(ae[0, ], group = "TERM")$err,
                   "No rows to display")
  # A one-level facet has nothing to compare across columns.
  one_arm <- droplevels(ae[ae$ARM == "Placebo", ])
  expect_match(
    rank_prepare(one_arm, group = "TERM", facet = "ARM", func = "count")$err,
    "fewer than two levels"
  )
})

test_that("percentages are dropped for measures that have no denominator", {
  ae <- ae_fixture()
  p <- rank_prepare(ae, group = "TERM", func = "mean", value = "AVAL",
                    cols = c("n", "pct"))
  pct <- vapply(p$plan, function(x) isTRUE(x$pct_only), logical(1L))
  expect_false(any(pct))
})

markup <- function(ae, ...) {
  # Drop the inlined <style> blocks: their rule text mentions the same class
  # names as the markup, which would make every grepl trivially true.
  h <- as.character(htmltools::renderTags(rank_table(ae, ...))$html)
  gsub("<style>.*?</style>", "", h)
}

test_that("the chrome uses the canonical title / caption bands", {
  ae <- ae_fixture()
  h <- markup(ae, group = "TERM", func = "count", title = "T", subtitle = "S",
              caption = "C")
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
  h <- markup(ae, group = "TERM", func = "count_distinct", id_var = "USUBJID")
  expect_match(h, "Preferred term")          # the group column's own label
  expect_match(h, "distinct USUBJID")        # what the measure counts
  expect_match(h, "blockr-col-label")        # the table block's label class
})

test_that("the HTML carries the chrome, the marks and the drill contract", {
  ae <- ae_fixture()
  html <- function(...) markup(ae, ...)

  h <- html(group = "TERM", func = "count_distinct", id_var = "USUBJID",
            title = "Ranked", subtitle = "N = {n_distinct(USUBJID)}",
            caption = "Source: fixture", drill = "TERM", elem_id = "blk-1")
  expect_match(h, "dd-table-title")
  expect_match(h, "N = 26", fixed = TRUE)      # the {...} token resolved
  expect_match(h, "dd-table-caption")
  expect_match(h, 'data-rank-drill="TERM"', fixed = TRUE)
  expect_match(h, 'data-rank-elem-id="blk-1"', fixed = TRUE)
  expect_match(h, "is-pick")                    # rows are clickable
  expect_match(h, 'data-v="', fixed = TRUE)     # numbers the client sorts on
  expect_match(h, "blockr-rank-fill")

  # No drill = no click affordance.
  expect_false(grepl("is-pick", html(group = "TERM", func = "count")))
})

test_that("the HTML marks nested, split and compare shapes distinctly", {
  ae <- ae_fixture()
  html <- function(...) markup(ae, ...)

  nested <- html(group = "TERM", parent = "SOC", func = "count")
  expect_match(nested, 'data-rank-nested="1"', fixed = TRUE)
  # The collapse affordance is the table block's own chevron: same button, same
  # svg, same rotation contract (the ROW carries `collapsed`).
  expect_match(nested, "blockr-indent-btn")
  expect_match(nested, "blockr-chev")
  expect_match(nested, "blockr-indent-toggle collapsed")
  expect_match(nested, "is-sub")   # child bars are the subordinate step

  grouped <- html(group = "TERM", color = "SEV", func = "count",
                  bar_mode = "grouped")
  expect_match(grouped, "blockr-rank-row3")

  cmp <- html(group = "TERM", facet = "ARM", compare = "Placebo",
              func = "count_distinct", id_var = "USUBJID")
  expect_match(cmp, "is-pos")
  expect_match(cmp, "is-neg")
  expect_match(cmp, "blockr-rank-dv")
})

test_that("title tiers follow the chart and table contract", {
  ae <- ae_fixture()
  attr(ae, "label") <- "Events fixture"
  html <- function(...) markup(ae, ...)
  # NULL = auto (the input's own label), "" = explicitly none.
  expect_match(html(group = "TERM", func = "count", title = NULL),
               "Events fixture")
  expect_false(grepl("dd-table-title",
                     html(group = "TERM", func = "count", title = "")))
})

test_that("a config that cannot be honored renders a message table", {
  ae <- ae_fixture()
  h <- markup(ae)
  expect_match(h, "Pick a Rank by column")
  expect_false(grepl("blockr-rank-fill", h))
})

test_that("the block constructs, registers and round-trips its state", {
  blk <- new_rank_block(group = "TERM", func = "count_distinct",
                        id_var = "USUBJID", drill = "TERM")
  expect_s3_class(blk, "rank_block")
  expect_s3_class(blk, "transform_block")

  # blockr.core serializes a block from its constructor formals and restores by
  # re-calling the constructor, so the runtime filter transport has to stay in
  # the signature or a saved click cannot come back.
  fmls <- names(formals(new_rank_block))
  expect_true(all(c("group", "func", "id_var", "drill", "filter_type",
                    "filter_column", "filter_values") %in% fmls))

  # Registered, so it shows up in the block-adder and the assistant universe.
  expect_true("rank_block" %in% names(blockr.core::available_blocks()))
})

test_that("html escaping survives a label with markup in it", {
  ae <- ae_fixture()
  ae$TERM[ae$TERM == "T1"] <- "<b>T1</b>"
  h <- markup(ae, group = "TERM", func = "count")
  expect_match(h, "&lt;b&gt;T1&lt;/b&gt;", fixed = TRUE)
  expect_false(grepl("<b>T1</b>", h, fixed = TRUE))
})
