# The chart-only "pct_distinct" aggregation ("% of panel"): the share of the
# PANEL's distinct values, not of the bar (bar_mode "percent" already does
# that) and not of the grid. Like `identity` it lives only in the JS engine and
# is deliberately absent from the shared AGG_FNS, so it has no R twin and
# cannot join the golden cross-test; this JS-only test guards it instead,
# through the same shipped engine and node runner.
#
# What makes it worth having rather than a precomputed percentage column: the
# denominator is counted from the rows the panel actually holds, AFTER
# faceting. So it is right for whatever the user facets by, it follows an
# upstream filter, and it cannot fall out of sync the way a joined-on N does.
# Its partner is na_group = "drop" (test-agg-golden.R): rows that exist only to
# be counted -- a subject with no event -- draw no bar and still count.

skip_if_no_node <- function() {
  skip_on_cran()
  skip_if(Sys.which("node") == "", "node not available")
}

js_aggregate <- function(rows, config) {
  runner <- testthat::test_path("fixtures", "agg-golden-runner.js")
  engine <- system.file("js", "drilldown-agg.js", package = "blockr.viz")
  payload <- jsonlite::toJSON(
    list(rows = rows, config = config),
    dataframe = "rows", na = "null", auto_unbox = TRUE
  )
  out <- system2(
    Sys.which("node"), shQuote(c(runner, engine)),
    input = as.character(payload), stdout = TRUE, stderr = TRUE
  )
  jsonlite::fromJSON(paste(out, collapse = ""), simplifyVector = FALSE)
}

val_by_group <- function(js) {
  stats::setNames(lapply(js, function(r) r$value),
                  vapply(js, function(r) r$group, ""))
}

# The worked example from the design note. Four subjects in one arm; S4 had no
# adverse event and is carried as a row with no term, which is what a
# right-join flatten produces. Diarrhoea is 2 of the 4 TREATED subjects, not 2
# of the 3 who happen to appear in the event table.
ae <- data.frame(
  USUBJID = c("S1", "S1", "S2", "S3", "S4"),
  AEDECOD = c("Diarrhoea", "Diarrhoea", "Diarrhoea", "Nausea", NA),
  TRT     = "A",
  stringsAsFactors = FALSE
)

test_that("the denominator is the panel's subjects, not the bars'", {
  skip_if_no_node()
  js <- js_aggregate(ae, list(group = "AEDECOD", value = "USUBJID",
                              func = "pct_distinct", na_group = "drop"))
  v <- val_by_group(js)
  expect_setequal(names(v), c("Diarrhoea", "Nausea"))
  expect_equal(v[["Diarrhoea"]], 2 / 4)   # NOT 2/3
  expect_equal(v[["Nausea"]], 1 / 4)
})

test_that("the row with no category still counts, which is the whole point", {
  skip_if_no_node()
  # Same data minus S4: the identical bars, a different denominator. If a
  # transform upstream ever drops the population rows this is the shape of the
  # damage -- 67% instead of 50%, plausible and wrong.
  no_pop <- ae[!is.na(ae$AEDECOD), ]
  js <- js_aggregate(no_pop, list(group = "AEDECOD", value = "USUBJID",
                                  func = "pct_distinct", na_group = "drop"))
  expect_equal(val_by_group(js)[["Diarrhoea"]], 2 / 3)
})

test_that("each facet divides by its own population", {
  skip_if_no_node()
  d <- rbind(
    ae,
    data.frame(USUBJID = c("T1", "T2"), AEDECOD = c("Diarrhoea", NA),
               TRT = "B", stringsAsFactors = FALSE)
  )
  js <- js_aggregate(d, list(group = "AEDECOD", facet = "TRT",
                             value = "USUBJID", func = "pct_distinct",
                             na_group = "drop"))
  by_facet <- stats::setNames(
    lapply(js, function(r) r$value),
    vapply(js, function(r) paste(r$facet, r$group), "")
  )
  expect_equal(by_facet[["A Diarrhoea"]], 2 / 4)
  expect_equal(by_facet[["B Diarrhoea"]], 1 / 2)  # its own panel, not 3/6
})

test_that("a colour split shares the panel denominator, so segments sum to it", {
  skip_if_no_node()
  # One subject per (term, grade) after an upstream worst-case dedup, which is
  # what makes the stack meaningful: the segments of a term sum to that term's
  # share of the panel rather than over-totalling.
  d <- data.frame(
    USUBJID = c("S1", "S2", "S3", "S4"),
    AEDECOD = c("Diarrhoea", "Diarrhoea", "Nausea", NA),
    GRADE   = c("1", "2", "1", NA),
    stringsAsFactors = FALSE
  )
  js <- js_aggregate(d, list(group = "AEDECOD", color = "GRADE",
                             value = "USUBJID", func = "pct_distinct",
                             na_group = "drop"))
  diarrhoea <- Filter(function(r) r$group == "Diarrhoea", js)
  expect_equal(sum(vapply(diarrhoea, function(r) r$value, 0)), 2 / 4)
})

test_that("a panel with no usable value is a gap, not a fabricated zero", {
  skip_if_no_node()
  d <- data.frame(USUBJID = c(NA, NA), AEDECOD = c("Diarrhoea", "Nausea"),
                  stringsAsFactors = FALSE)
  js <- js_aggregate(d, list(group = "AEDECOD", value = "USUBJID",
                             func = "pct_distinct"))
  expect_true(all(vapply(js, function(r) is.null(r$value), TRUE)))
})

test_that("without na_group = 'drop' the population row becomes a category", {
  skip_if_no_node()
  # The default is unchanged behaviour, so an existing board keeps its
  # nameless bar rather than silently gaining a different denominator.
  js <- js_aggregate(ae, list(group = "AEDECOD", value = "USUBJID",
                              func = "pct_distinct"))
  expect_true("" %in% vapply(js, function(r) r$group, ""))
})

# --- pct_of: which role the denominator is taken within ---------------------
# The chart cannot infer this. It would have to know whether a column is a
# POPULATION split (arm, sex, country -- a per-level denominator is
# meaningful) or an EVENT attribute (grade -- dividing grade-2 subjects by
# subjects-with-grade-2 is circular). Nothing in the data says which.

# Two countries. 4 of 10 US subjects and 3 of 5 Japanese subjects took an
# action. The right answer is 40% and 60%: Japan is HIGHER.
by_country <- rbind(
  data.frame(USUBJID = sprintf("U%02d", 1:10), COUNTRY = "USA",
             ACTION = c(rep("DOSE NOT CHANGED", 4), rep(NA, 6)),
             stringsAsFactors = FALSE),
  data.frame(USUBJID = sprintf("J%02d", 1:5), COUNTRY = "JPN",
             ACTION = c(rep("DOSE NOT CHANGED", 3), rep(NA, 2)),
             stringsAsFactors = FALSE)
)

test_that("pct_of = 'group' divides within the group", {
  skip_if_no_node()
  js <- js_aggregate(by_country, list(
    group = "COUNTRY", facet = "ACTION", value = "USUBJID",
    func = "pct_distinct", na_group = "drop", pct_of = "group"))
  v <- stats::setNames(lapply(js, function(r) r$value),
                       vapply(js, function(r) r$group, ""))
  expect_equal(v[["USA"]], 4 / 10)
  expect_equal(v[["JPN"]], 3 / 5)
})

test_that("the default divides within the facet, which is wrong HERE", {
  skip_if_no_node()
  # Pinning the failure the option exists to fix. The panel is one ACTION, so
  # its population is the 7 subjects who took that action -- 4 US and 3 JPN.
  # Every bar then reads as a share of those 7, i.e. a COMPOSITION across
  # countries summing to 100%, not a rate within each country. And the RANKING
  # FLIPS: USA reads higher than Japan when it is in fact lower (40% v 60%).
  # A reader draws the opposite conclusion with nothing on screen to say so.
  js <- js_aggregate(by_country, list(
    group = "COUNTRY", facet = "ACTION", value = "USUBJID",
    func = "pct_distinct", na_group = "drop"))
  v <- stats::setNames(lapply(js, function(r) r$value),
                       vapply(js, function(r) r$group, ""))
  expect_equal(v[["USA"]], 4 / 7)
  expect_equal(v[["JPN"]], 3 / 7)
  expect_equal(v[["USA"]] + v[["JPN"]], 1)     # a composition, not a rate
  expect_gt(v[["USA"]], v[["JPN"]])            # backwards, by construction
})

test_that("pct_of = 'color' divides within the colour level", {
  skip_if_no_node()
  d <- data.frame(
    USUBJID = c("S1", "S2", "S3", "S4"),
    TERM    = c("Diarrhoea", "Diarrhoea", "Diarrhoea", NA),
    SEX     = c("F", "F", "M", "M"),
    stringsAsFactors = FALSE
  )
  js <- js_aggregate(d, list(group = "TERM", color = "SEX", value = "USUBJID",
                             func = "pct_distinct", na_group = "drop",
                             pct_of = "color"))
  v <- stats::setNames(lapply(js, function(r) r$value),
                       vapply(js, function(r) r$color, ""))
  expect_equal(v[["F"]], 2 / 2)   # both F subjects had it
  expect_equal(v[["M"]], 1 / 2)   # one of two M subjects
})

test_that("an unmapped role collapses to the whole frame, not to nothing", {
  skip_if_no_node()
  # pct_of names COLOUR, which is not mapped: every row shares the
  # placeholder, so the denominator is all 15. A null here would mean silent
  # blank bars rather than a visible over- or under-count.
  js <- js_aggregate(by_country, list(
    group = "COUNTRY", facet = "ACTION", value = "USUBJID",
    func = "pct_distinct", na_group = "drop", pct_of = "color"))
  v <- stats::setNames(lapply(js, function(r) r$value),
                       vapply(js, function(r) r$group, ""))
  expect_equal(v[["USA"]], 4 / 15)
  expect_equal(v[["JPN"]], 3 / 15)
})

test_that("several roles at once take the denominator within their crossing", {
  skip_if_no_node()
  # The gear offers one role; the engine takes a vector, so the capability is
  # there before a control for it is.
  d <- data.frame(
    USUBJID = c("S1", "S2", "S3", "S4"),
    TERM    = c("D", "D", NA, NA),
    ARM     = c("A", "B", "A", "B"),
    SEX     = c("F", "F", "F", "F"),
    stringsAsFactors = FALSE
  )
  js <- js_aggregate(d, list(group = "TERM", facet = "ARM", color = "SEX",
                             value = "USUBJID", func = "pct_distinct",
                             na_group = "drop", pct_of = list("facet", "color")))
  # Arm A has 2 subjects, both F; one had the term.
  a <- Filter(function(r) r$facet == "A", js)
  expect_equal(a[[1]]$value, 1 / 2)
})

test_that("a row dropped from its PANEL still counts in the denominator", {
  skip_if_no_node()
  # The Actions-by-Country shape, and the reason the two rules have to be
  # ordered the way they are. The population rows have no ACTION, so
  # na_group = "drop" keeps them out of the facet -- no empty panel -- while
  # pct_of = "group" still divides by every subject in the country, because
  # the denominator is counted before anything is dropped.
  js <- js_aggregate(by_country, list(
    group = "COUNTRY", facet = "ACTION", value = "USUBJID",
    func = "pct_distinct", na_group = "drop", pct_of = "group"))
  expect_equal(length(js), 2L)                       # no NA-action panel
  expect_true(all(vapply(js, function(r) nzchar(r$facet), TRUE)))
  v <- stats::setNames(lapply(js, function(r) r$value),
                       vapply(js, function(r) r$group, ""))
  expect_equal(v[["USA"]], 4 / 10)                   # not 4/4
  expect_equal(v[["JPN"]], 3 / 5)                    # not 3/3
})
