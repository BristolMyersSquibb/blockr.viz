# Dynamic block titles (R/title-template.R): the {...} token resolver and
# the three-tier NULL / "" / template contract shared by the viz blocks.

test_that("resolve_title_template substitutes column-value tokens", {
  d <- data.frame(ARM = c("Placebo", "Drug A", "Placebo"))
  expect_equal(
    resolve_title_template("AEs by {ARM}", d),
    "AEs by Placebo, Drug A"
  )
})

test_that("factor columns collapse in level order, not appearance order", {
  d <- data.frame(
    ARM = factor(c("Drug A", "Placebo"), levels = c("Placebo", "Drug A"))
  )
  expect_equal(resolve_title_template("{ARM}", d), "Placebo, Drug A")
})

test_that("label(col) resolves the label attribute with name fallback", {
  d <- data.frame(value = 1:3, other = 4:6)
  attr(d$value, "label") <- "Systolic Blood Pressure"
  expect_equal(
    resolve_title_template("Mean {label(value)} by visit", d),
    "Mean Systolic Blood Pressure by visit"
  )
  expect_equal(resolve_title_template("{label(other)}", d), "other")
})

test_that("n and n_distinct tokens count rows and values", {
  d <- data.frame(id = c("a", "a", "b", NA))
  expect_equal(resolve_title_template("{n} rows", d), "4 rows")
  expect_equal(resolve_title_template("{n_distinct(id)} ids", d), "2 ids")
})

test_that("missing columns resolve to empty, never error", {
  d <- data.frame(x = 1)
  expect_equal(resolve_title_template("a {gone} b", d), "a  b")
  expect_equal(resolve_title_template("{label(gone)}", d), "")
  expect_equal(resolve_title_template("{n_distinct(gone)}", d), "")
})

test_that("long value lists elide past TITLE_MAX_VALUES", {
  d <- data.frame(id = sprintf("S%02d", 1:20))
  out <- resolve_title_template("{id}", d)
  expect_true(endsWith(out, "…"))
  expect_equal(length(strsplit(out, ", ")[[1]]), TITLE_MAX_VALUES + 1L)
})

test_that("plain text and empty templates pass through", {
  d <- data.frame(x = 1)
  expect_equal(resolve_title_template("No tokens here", d), "No tokens here")
  expect_equal(resolve_title_template("", d), "")
})

test_that("resolve_block_title implements the three tiers", {
  d <- data.frame(ARM = "Placebo")

  # NULL = auto: falls back to the supplied label, verbatim (not a template).
  expect_equal(resolve_block_title(NULL, d, auto = "Table 14.3.1"), "Table 14.3.1")
  expect_null(resolve_block_title(NULL, d, auto = NULL))

  # "" (and whitespace) = explicitly none, even when an auto label exists.
  expect_null(resolve_block_title("", d, auto = "Table 14.3.1"))
  expect_null(resolve_block_title("   ", d, auto = "Table 14.3.1"))

  # Text = template, resolved; the auto label is ignored.
  expect_equal(
    resolve_block_title("AEs: {ARM}", d, auto = "Table 14.3.1"),
    "AEs: Placebo"
  )
})

test_that("title_state keeps '' (off) and heals list() to NULL", {
  expect_null(title_state(NULL))
  expect_null(title_state(list()))     # pre-#144 DAG paste corruption
  expect_equal(title_state(""), "")    # explicitly none must survive
  expect_equal(title_state("x {y}"), "x {y}")
})

test_that("input_display_attrs reads label, subtitle and caption from raw input", {
  d <- data.frame(x = 1)
  expect_equal(
    input_display_attrs(d),
    list(label = NULL, subtitle = NULL, caption = NULL)
  )
  attr(d, "label") <- "Demographics"
  attr(d, "subtitle") <- "Safety population"
  attr(d, "caption") <- "Source: ADSL"
  expect_equal(
    input_display_attrs(d),
    list(label = "Demographics", subtitle = "Safety population",
         caption = "Source: ADSL")
  )
  expect_equal(
    input_display_attrs(NULL),
    list(label = NULL, subtitle = NULL, caption = NULL)
  )
  # Empty strings do not count as present.
  attr(d, "subtitle") <- ""
  expect_null(input_display_attrs(d)$subtitle)
})

test_that("chart block round-trips the three title tiers through state", {
  df <- data.frame(ARM = c("Placebo", "Drug A"), AVAL = c(1, 2))
  blk <- new_chart_block(
    chart_type = "bar", group = "ARM",
    title = "AEs by {ARM}", subtitle = "", caption = NULL
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_equal(session$returned$state$title(), "AEs by {ARM}")
      expect_equal(session$returned$state$subtitle(), "")   # explicitly none
      expect_null(session$returned$state$caption())          # auto
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("{filters} renders the filter trail the data carries", {

  d <- data.frame(x = 1:3)
  attr(d, "blockr_filters") <- c(
    global_filter = "SEX = F", ae_flags = "TRTEMFL"
  )

  expect_equal(resolve_title_template("{filters}", d), "SEX = F; TRTEMFL")
  expect_equal(
    resolve_title_template("Filtered: {filters} (n = {n})", d),
    "Filtered: SEX = F; TRTEMFL (n = 3)"
  )

  # Works in any slot, because all three go through the same resolver.
  expect_equal(resolve_block_title("{filters}", d), "SEX = F; TRTEMFL")
})

test_that("{filters} disappears rather than erroring when nothing is filtered", {

  d <- data.frame(x = 1:3)

  # A caption resolving to "" is how the band gets hidden, so `{filters}` on
  # its own leaves no trace on an unfiltered board. A prefix does survive,
  # which is why the token is documented as best used alone.
  expect_equal(resolve_title_template("{filters}", d), "")
  expect_equal(resolve_title_template("Filtered: {filters}", d), "Filtered: ")

  # An empty trail is the same as no trail.
  attr(d, "blockr_filters") <- character()
  expect_equal(resolve_title_template("{filters}", d), "")
})

# -- Argument tokens and optional segments -----------------------------------
#
# The sentence a block writes about itself: `{@arg}` prints a setting and
# marks the word as that setting's control, `[ ... ]` drops a clause whose
# token is empty. blockr.docs design-system/pinned-controls.md.

test_that("@arg tokens print the argument value, not the column's values", {
  d <- data.frame(TRT = c("Placebo", "Drug A"), stringsAsFactors = FALSE)
  expect_equal(
    resolve_title_template("Coloured by {@color}", d, list(color = "TRT")),
    "Coloured by TRT"
  )
  # The bare token is unchanged: it still spells out the column's values.
  expect_equal(
    resolve_title_template("Coloured by {TRT}", d, list(color = "TRT")),
    "Coloured by Placebo, Drug A"
  )
})

test_that("label() and n_distinct() compose with an @arg", {
  d <- data.frame(AVAL = c(1, 2, 2))
  attr(d$AVAL, "label") <- "Analysis Value"
  a <- list(y = "AVAL")
  expect_equal(resolve_title_template("{label(@y)}", d, a), "Analysis Value")
  expect_equal(resolve_title_template("{n_distinct(@y)}", d, a), "2")
  # Falls back to the column name, like {label(col)} does.
  expect_equal(
    resolve_title_template("{label(@y)}", data.frame(AVAL = 1), a), "AVAL"
  )
})

test_that("an empty argument empties its token", {
  d <- data.frame(x = 1)
  expect_equal(resolve_title_template("[{@facet}]", d, list(facet = NULL)), "")
  expect_equal(resolve_title_template("[{@facet}]", d, list(facet = "")), "")
  expect_equal(
    resolve_title_template("[{@facet}]", d, list(facet = "(none)")), ""
  )
  # No args at all: a board written before this reads the token as empty
  # rather than erroring.
  expect_equal(resolve_title_template("a{@facet}b", d), "ab")
})

test_that("an optional segment leaves with its token", {
  d <- data.frame(x = 1)
  tpl <- "Most frequent {@group}[, faceted by {@facet}][, {@min_n}+ patients]"
  expect_equal(
    resolve_title_template(tpl, d, list(group = "AEDECOD", facet = "SEX",
                                        min_n = 5)),
    "Most frequent AEDECOD, faceted by SEX, 5+ patients"
  )
  expect_equal(
    resolve_title_template(tpl, d, list(group = "AEDECOD", facet = "(none)",
                                        min_n = 5)),
    "Most frequent AEDECOD, 5+ patients"
  )
  # An unmatched bracket is literal: display copy must not error.
  expect_equal(resolve_title_template("a [b {@x}", d, list(x = "X")), "a [b X")
})

test_that("parts carry the argument each settable word belongs to", {
  d <- data.frame(AVAL = 1)
  attr(d$AVAL, "label") <- "Analysis Value"
  parts <- title_template_parts(
    "Top {label(@y)} by {@color}, {n_distinct(@y)} values",
    d, list(y = "AVAL", color = "TRT")
  )
  # A count is not something a reader can set, so it is text, not a slot --
  # which is why it merges into the run of plain text around it.
  expect_equal(vapply(parts, function(p) p$text, character(1L)),
               c("Top ", "Analysis Value", " by ", "TRT", ", 1 values"))
  expect_equal(parts[[2L]]$arg, "y")
  expect_equal(parts[[4L]]$arg, "color")
  expect_null(parts[[5L]]$arg)
  expect_null(parts[[1L]]$arg)
})

test_that("block_title_parts follows the three tiers", {
  d <- data.frame(x = 1)
  expect_null(block_title_parts(NULL, d, auto = NULL))
  expect_equal(block_title_parts(NULL, d, auto = "From upstream"),
               list(list(text = "From upstream")))
  expect_null(block_title_parts("", d, auto = "From upstream"))
  # `by` travels with a slot so the menu can lead with the half the sentence
  # printed: the name here, the label for a {label(@x)} token.
  expect_equal(
    block_title_parts("by {@color}", d, args = list(color = "TRT")),
    list(list(text = "by "), list(text = "TRT", arg = "color", by = "name"))
  )
  d2 <- data.frame(TRT = "A")
  attr(d2$TRT, "label") <- "Actual Treatment"
  expect_equal(
    block_title_parts("by {label(@color)}", d2, args = list(color = "TRT")),
    list(list(text = "by "),
         list(text = "Actual Treatment", arg = "color", by = "label"))
  )
})
