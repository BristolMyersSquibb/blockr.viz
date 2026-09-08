# The chart block's `script`: an ordinary R script run on the incoming data
# before the chart sees it, whose plain-value declarations become controls on
# the block's face. The parse layer itself (cb_parse / cb_specs / cb_expr) is a
# copy of blockr.extra's and is covered there; what is tested here is the
# chart block's side of it.

script_of <- function(...) paste(c(...), collapse = "\n")

parsed_of <- function(txt, data = datasets::iris) {
  p <- cb_parse(txt)
  list(parsed = p, specs = cb_specs(p, data))
}


test_that("no script leaves the data exactly as it arrived", {
  # The case nineteen chart blocks in twenty are in: the prepare step must be
  # invisible, not merely harmless.
  for (txt in list(NULL, "", "   ")) {
    p <- cb_parse(txt)
    out <- dd_prepare_run(datasets::iris, p, list(), list())
    expect_identical(out$data, datasets::iris)
    expect_null(out$error)
  }
})


test_that("a script transforms the frame and its declarations drive it", {
  txt <- script_of(
    "n <- 6",
    "sp <- factor(\"setosa\", levels = unique(data$Species))",
    "utils::head(dplyr::filter(data, Species %in% sp), n)"
  )
  ps <- parsed_of(txt)

  # Untouched controls run at the values the script itself declares.
  out <- dd_prepare_run(datasets::iris, ps$parsed, ps$specs, list())
  expect_null(out$error)
  expect_equal(nrow(out$data), 6L)
  expect_equal(as.character(unique(out$data$Species)), "setosa")

  # And a knob turned on the band reaches the same code path.
  out <- dd_prepare_run(datasets::iris, ps$parsed, ps$specs,
                        list(n = 3, sp = "versicolor"))
  expect_equal(nrow(out$data), 3L)
  expect_equal(as.character(unique(out$data$Species)), "versicolor")
})


test_that("every way a script can fail is reported rather than swallowed", {
  # A SYNTAX error is the one that got away first: cb_parse() reports a failure
  # with an empty statement list, so a "nothing to do here" check placed ahead
  # of the ok flag turns every unfinished script into a silent pass-through.
  out <- dd_prepare_run(datasets::iris, cb_parse("data |> "), list(), list())
  expect_null(out$data)
  expect_match(out$error, "unexpected end of input")

  # A runtime error.
  ps <- parsed_of("dplyr::filter(data, NOPE == 1)")
  out <- dd_prepare_run(datasets::iris, ps$parsed, ps$specs, list())
  expect_null(out$data)
  expect_match(out$error, "NOPE")

  # A script that returns something that is not a frame. The chart cannot draw
  # it and nor can the block's downstream, so it is an error and not a coercion.
  ps <- parsed_of("nrow(data)")
  out <- dd_prepare_run(datasets::iris, ps$parsed, ps$specs, list())
  expect_null(out$data)
  expect_match(out$error, "not a data frame")

  # Declarations and no body yet is a half-written script, not a failure: the
  # knobs are already on the band and the pipeline comes next.
  ps <- parsed_of("n <- 5")
  out <- dd_prepare_run(datasets::iris, ps$parsed, ps$specs, list())
  expect_identical(out$data, datasets::iris)
  expect_null(out$error)
})


test_that("the script wraps the click filter, so drilling stays on source rows", {
  # Order is the whole contract: a click identifies a mark by an UPSTREAM
  # column, which the script is free not to keep. The filter therefore has to
  # be innermost, with the script around it.
  ps <- parsed_of("dplyr::mutate(data, ratio = Sepal.Length / Sepal.Width)")
  sx <- cb_expr(ps$parsed, ps$specs, list(), slot = TRUE)
  filt <- blockr.core::bbquote(
    dplyr::filter(.(data), .data[["Species"]] == "setosa")
  )
  out <- dd_splice_slot(sx, filt)

  txt <- paste(deparse(out), collapse = " ")
  expect_match(txt, "dplyr::mutate\\(dplyr::filter\\(")
  # Exactly one `.(data)` marker survives, in the innermost position.
  expect_equal(lengths(regmatches(txt, gregexpr(".(data)", txt, fixed = TRUE))),
               1L)
})


test_that("declared kinds reach the browser in the engine's vocabulary", {
  txt <- script_of(
    "num <- 10",
    "flag <- TRUE",
    "txt <- \"hello\"",
    "one <- factor(\"a\", levels = c(\"a\", \"b\"))",
    "many <- factor(c(\"a\", \"b\"), levels = c(\"a\", \"b\", \"c\"))",
    "when <- as.Date(\"2026-01-01\")",
    "data"
  )
  ps <- parsed_of(txt)
  roles <- dd_script_roles(ps$specs)
  names(roles) <- vapply(roles, `[[`, character(1L), "name")

  expect_equal(roles$num$kind, "number")
  # A flag becomes the engine's boolean segmented, which it already draws as a
  # checkbox -- no new control kind for something the gear can do.
  expect_equal(roles$flag$kind, "segmented")
  expect_equal(roles$txt$kind, "text")
  expect_equal(roles$one$kind, "select")
  expect_equal(roles$many$kind, "multi")
  expect_equal(roles$when$kind, "date")

  # Keys are prefixed, so a script variable called `color` cannot write the
  # chart's colour mapping.
  expect_equal(roles$num$key, "sv_num")
  # A factor's levels are the choice list, in declaration order. Bare strings,
  # since a level is its own label and a labelled option renders as
  # "value (label)".
  expect_equal(unlist(roles$many$options), c("a", "b", "c"))
})


test_that("a declaration that cannot become a control says why", {
  # The usual cause is a mistyped column name, which leaves the factor with no
  # levels. Carried through with its message rather than dropped: a knob that
  # quietly fails to appear is the hardest kind to debug.
  ps <- parsed_of("site <- factor(character(), levels = unique(data$NOPE))\ndata")
  roles <- dd_script_roles(ps$specs)
  expect_length(roles, 1L)
  expect_equal(roles[[1L]]$kind, "error")
  expect_true(nzchar(roles[[1L]]$error))
})


test_that("values coming back from the band are coerced by the declaration", {
  txt <- script_of(
    "num <- 10",
    "flag <- TRUE",
    # Two picks, so the declaration says multi-select: exactly one pick is how
    # a script declares a SINGLE select.
    "many <- factor(c(\"a\", \"b\"), levels = c(\"a\", \"b\", \"c\"))",
    "data"
  )
  ps <- parsed_of(txt)

  # Numbers arrive as strings from the DOM; the declaration knows better.
  vals <- dd_script_values(list(sv_num = "42"), ps$specs, list())
  expect_identical(vals$num, 42)

  # The engine speaks on/off for a boolean; the script declared a logical.
  expect_false(dd_script_values(list(sv_flag = "off"), ps$specs, list())$flag)
  expect_true(dd_script_values(list(sv_flag = "on"), ps$specs, list())$flag)

  # A JS array arrives as an R list.
  vals <- dd_script_values(list(sv_many = list("a", "c")), ps$specs, list())
  expect_identical(vals$many, c("a", "c"))

  # Emptying a multi-select is a REAL value, and an empty JS array arrives here
  # as NULL -- indistinguishable from an absent key. The band sends "" for it,
  # exactly as the expose handler does.
  vals <- dd_script_values(list(sv_many = ""), ps$specs, list(many = c("a")))
  expect_identical(vals$many, character())

  # A key the message does not carry leaves the current value alone.
  vals <- dd_script_values(list(sv_num = 1), ps$specs, list(flag = FALSE))
  expect_false(vals$flag)
})


test_that("a control the user has not touched shows what the script says", {
  ps <- parsed_of(script_of("n <- 7", "flag <- FALSE", "data"))
  cfg <- dd_script_cfg(ps$specs, list())
  expect_equal(cfg$sv_n, 7)
  expect_equal(cfg$sv_flag, "off")

  cfg <- dd_script_cfg(ps$specs, list(n = 2, flag = TRUE))
  expect_equal(cfg$sv_n, 2)
  expect_equal(cfg$sv_flag, "on")
})


test_that("script and values round-trip through block state", {
  blk <- new_chart_block(
    group = "Species", value = ".count", func = "count",
    script = "n <- 5\nutils::head(data, n)",
    values = list(n = 3)
  )
  payload <- blockr.core::blockr_ser(blk)[["payload"]]
  expect_equal(payload$script, "n <- 5\nutils::head(data, n)")
  expect_equal(payload$values$n, 3)

  # A script handed over as lines (a hand-edited board, or an MCP write)
  # collapses on the way in rather than at every read site.
  blk <- new_chart_block(script = c("n <- 5", "utils::head(data, n)"))
  expect_equal(blockr.core::blockr_ser(blk)[["payload"]]$script,
               "n <- 5\nutils::head(data, n)")
})


test_that("script is externally controllable but may be empty", {
  blk <- new_chart_block(group = "Species", value = ".count", func = "count")
  for (nm in c("script", "values")) {
    expect_true(nm %in% blockr.core::external_ctrl_vars(blk))
    # No script is the default, and a state field that is empty and NOT in
    # allow_empty_state wedges the block for good
    # (reference_blockr_allow_empty_state_wedge).
    expect_true(nm %in% attr(blk, "allow_empty_state"))
  }
})


test_that("`script` is deliberately absent from the AI surface", {
  # Signed off as a non-AI-exposed argument: a general escape hatch on a block
  # that appears twenty times in a board invites the assistant to write a
  # script where a mapping would have done the job.
  expect_false("script" %in% names(chart_arguments()))
  expect_false("values" %in% names(chart_arguments()))
})


test_that("the gear's textarea writes the script, and the band writes its knobs", {
  blk <- new_chart_block(
    group = "Species", value = ".count", func = "count",
    script = "n <- 6\nutils::head(data, n)"
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      expr_scope <- session$makeScope("expr")
      session$flushReact()
      expect_equal(session$returned$state$script(), "n <- 6\nutils::head(data, n)")

      # A knob turned on the band.
      expr_scope$setInputs(drilldown_block_action = list(
        action = "config", sv_n = 2
      ))
      session$flushReact()
      expect_equal(session$returned$state$values()$n, 2)

      # The script rewritten in the gear. The old value is dropped, because the
      # script no longer declares that name.
      expr_scope$setInputs(drilldown_block_action = list(
        action = "config", script = "keep <- 4\nutils::head(data, keep)"
      ))
      session$flushReact()
      expect_equal(session$returned$state$script(),
                   "keep <- 4\nutils::head(data, keep)")

      # Clearing it (the gear's section checkbox) is a real write, not an
      # absent key.
      expr_scope$setInputs(drilldown_block_action = list(
        action = "config", script = ""
      ))
      session$flushReact()
      expect_equal(session$returned$state$script(), "")
    },
    args = list(x = blk, data = list(data = function() datasets::iris))
  )
})


test_that("the block's expression carries the script and the filter together", {
  blk <- new_chart_block(
    group = "Species", value = ".count", func = "count",
    drill = "Species",
    script = "dplyr::mutate(data, ratio = Sepal.Length / Sepal.Width)"
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      expr_scope <- session$makeScope("expr")
      expr_scope$setInputs(drilldown_block_action = list(
        action = "filter", filter_type = "categorical",
        column = "Species", values = list("setosa")
      ))
      session$flushReact()
      txt <- paste(deparse(session$returned$expr()), collapse = " ")
      expect_match(txt, "dplyr::mutate")
      expect_match(txt, "Species")
    },
    args = list(x = blk, data = list(data = function() datasets::iris))
  )
})
