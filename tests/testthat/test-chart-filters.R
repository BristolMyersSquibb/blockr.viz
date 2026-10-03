# The chart block's click filter: v2's `filters` message (several columns,
# missing values, a range in a facet panel), v1's `column` / `values`, the
# `filters` state and its restore from a board saved by v1.

filters_df <- function() {
  data.frame(
    AETERM = c("Rash", "Rash", "Rash", "Nausea", "Nausea"),
    AESEV = c("MILD", "MODERATE", NA, "MILD", ""),
    USUBJID = c("S1", "S2", "S3", "S1", "S4"),
    ADY = c(1, 2, 3, 4, 5),
    AVAL = c(10, 20, 30, 40, 50),
    stringsAsFactors = FALSE
  )
}

# Run `fn(t)` against a chart block's server. `t$send(msg)` posts a chart
# action and flushes; `t$state` is the block state, `t$result()` its output.
with_chart <- function(blk, fn, data = filters_df()) {
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expr_scope <- session$makeScope("expr")
      fn(list(
        session = session,
        send = function(msg) {
          expr_scope$setInputs(drilldown_block_action = msg)
          session$flushReact()
        },
        state = session$returned$state,
        result = function() session$returned$result(),
        expr = function() session$returned$expr()
      ))
    },
    args = list(x = blk, data = list(data = function() data))
  )
}

# Write a serialized block the way blockr.core saves a board, and read it
# back.
board_json_trip <- function(ser) {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  writeLines(blockr.core:::write_json(ser), path)
  blockr.core:::read_json(path)
}

v2_click <- function(filters, nonce = 1) {
  list(action = "filter", filter_type = "categorical", filters = filters,
       nonce = nonce)
}

test_that("a v2 click on one column filters the output, with v1's code", {
  blk <- new_chart_block(chart_type = "bar", group = "AETERM")
  with_chart(blk, function(t) {
    t$send(v2_click(list(AETERM = list("Rash"))))
    expect_equal(t$state$filters(), list(AETERM = "Rash"))
    expect_equal(t$state$filter_type(), "categorical")
    expect_equal(t$result()$USUBJID, c("S1", "S2", "S3"))
    # The code a one-column filter emits is the code v1's click emitted.
    expect_identical(
      t$expr(),
      quote(dplyr::filter(.(data), .data[["AETERM"]] == "Rash"))
    )
  })
})

test_that("a v2 click on several columns ANDs them", {
  with_chart(new_chart_block(chart_type = "bar", group = "AETERM",
                             color = "AESEV"), function(t) {
    t$send(v2_click(list(AETERM = list("Rash"), AESEV = list("MODERATE"))))
    expect_equal(t$state$filters(), list(AETERM = "Rash", AESEV = "MODERATE"))
    expect_equal(t$result()$USUBJID, "S2")
    expect_identical(
      t$expr(),
      quote(dplyr::filter(.(data), .data[["AETERM"]] == "Rash",
                          .data[["AESEV"]] == "MODERATE"))
    )
  })
})

test_that("a null value is a missing one: NA and the empty string", {
  with_chart(new_chart_block(chart_type = "bar", group = "AETERM",
                             color = "AESEV"), function(t) {
    # Shiny hands a JSON null inside an array over as NULL in a list...
    t$send(v2_click(list(AETERM = list("Rash"), AESEV = list(NULL))))
    expect_equal(t$state$filters(),
                 list(AETERM = "Rash", AESEV = NA_character_))
    expect_equal(t$result()$USUBJID, "S3")

    # ...or as NA; the empty string counts as missing too.
    t$send(v2_click(list(AETERM = list("Nausea"), AESEV = NA), nonce = 2))
    expect_equal(t$result()$USUBJID, "S4")

    # A missing value beside a real one.
    t$send(v2_click(list(AESEV = list("MILD", NULL)), nonce = 3))
    expect_equal(t$state$filters(), list(AESEV = c("MILD", NA)))
    expect_equal(t$result()$USUBJID, c("S1", "S3", "S1", "S4"))
  })
})

test_that("a v1 click (column + values) still filters, and clears", {
  blk <- new_chart_block(chart_type = "bar", group = "AETERM")
  with_chart(blk, function(t) {
    t$send(list(action = "filter", filter_type = "categorical",
              column = "AETERM", values = list("Nausea", "Rash")))
    expect_equal(t$state$filters(), list(AETERM = c("Nausea", "Rash")))
    expect_equal(nrow(t$result()), 5L)

    t$send(list(action = "filter", filter_type = "categorical",
              column = "AETERM", values = list("Nausea")))
    expect_equal(t$result()$USUBJID, c("S1", "S4"))

    # The clear both engines send.
    t$send(list(action = "filter", filter_type = "categorical", column = NULL,
              values = NULL, x_col = NULL, y_col = NULL, x_range = NULL,
              y_range = NULL))
    expect_null(t$state$filters())
    expect_equal(nrow(t$result()), 5L)
  })
})

test_that("D10: a range taken in a facet panel ANDs the panel's key", {
  with_chart(new_chart_block(chart_type = "scatter", x = "ADY", y = "AVAL",
                             facet = "AETERM"), function(t) {
    t$send(list(action = "filter", filter_type = "range", x_col = "ADY",
              y_col = "AVAL", x_range = list(1, 5), y_range = list(0, 45),
              filters = list(AETERM = list("Nausea"))))
    expect_equal(t$state$filter_type(), "range")
    expect_equal(t$state$filters(), list(AETERM = "Nausea"))
    expect_equal(t$state$filter_range()$x_range, c(1, 5))
    expect_equal(t$result()$USUBJID, "S1")

    # A range without a panel, as v1 sends it.
    t$send(list(action = "filter", filter_type = "range", x_col = "ADY",
              y_col = "AVAL", x_range = list(1, 5), y_range = list(0, 45)))
    expect_null(t$state$filters())
    expect_equal(t$result()$USUBJID, c("S1", "S2", "S3", "S1"))

    # The clear of a range.
    t$send(list(action = "filter", filter_type = "range", column = NULL,
              values = NULL, x_col = NULL, y_col = NULL, x_range = NULL,
              y_range = NULL))
    expect_null(t$state$filter_range())
    expect_equal(nrow(t$result()), 5L)
  })
})

test_that("a drill target receives the AND of the columns", {
  blk <- new_chart_block(chart_type = "bar", group = "AETERM",
                         color = "AESEV", ctrl_target = "vf")
  sent <- list()
  with_chart(blk, function(t) {
    t$session$userData$blockr_ctrl_send <- function(target, args,
                                                  author = NULL) {
      sent[[length(sent) + 1L]] <<- args
    }
    claims <- function(i) {
      cols <- sent[[i]]$state$columns
      stats::setNames(lapply(cols, function(c) unlist(c$values)),
                      vapply(cols, `[[`, "", "name"))
    }

    # Two terms alone resolve to no single term; with MODERATE they do.
    t$send(v2_click(list(AETERM = list("Rash", "Nausea"),
                       AESEV = list("MODERATE"))))
    expect_length(sent, 1L)
    expect_equal(claims(1), list(AETERM = "Rash", AESEV = "MODERATE"))

    # A missing value narrows the rows the other column is read from, and is
    # not claimed itself.
    t$send(v2_click(list(USUBJID = list("S3"), AESEV = list(NULL)), nonce = 2))
    expect_length(sent, 2L)
    expect_equal(claims(2), list(USUBJID = "S3"))

    # A v1 click still claims.
    t$send(list(action = "filter", filter_type = "categorical",
              column = "USUBJID", values = list("S4"), nonce = 3))
    expect_length(sent, 3L)
    expect_equal(claims(3), list(USUBJID = "S4"))

    # Nothing latched in this mode.
    expect_null(t$state$filters())
  })
})

test_that("dd_ctrl_claims() matches a missing value on NA and on \"\"", {
  d <- filters_df()
  expect_equal(
    dd_ctrl_claims(d, "", list(AETERM = "Nausea", AESEV = NA_character_)),
    list(list(name = "AETERM", mode = "multi", values = "Nausea"))
  )
  # Only the missing column: nothing to claim, so hold.
  expect_null(dd_ctrl_claims(d, "", list(AESEV = NA_character_)))
})

test_that("a board saved by v1 restores its filter through the constructor", {
  blk <- new_chart_block(chart_type = "bar", group = "AETERM",
                         filter_column = "AETERM",
                         filter_values = list("Nausea"))
  ser <- blockr.core::blockr_ser(blk)
  expect_equal(ser$payload$filters, list(AETERM = "Nausea"))
  expect_null(ser$payload$filter_column)
  expect_null(ser$payload$filter_values)

  with_chart(blk, function(t) {
    expect_equal(t$state$filters(), list(AETERM = "Nausea"))
    expect_null(t$state$filter_column())
    expect_equal(t$result()$USUBJID, c("S1", "S4"))
  })

  # The payload as v1 wrote it: no `filters`, the old pair set.
  v1 <- ser
  v1$payload$filters <- NULL
  v1$payload$filter_column <- "AETERM"
  v1$payload$filter_values <- list("Rash")
  restored <- blockr.core::blockr_deser(v1)
  expect_equal(blockr.core::blockr_ser(restored)$payload$filters,
               list(AETERM = "Rash"))
})

test_that("a multi-column filter with a missing value round-trips", {
  f <- list(AETERM = "Rash", AESEV = NA_character_)
  blk <- new_chart_block(chart_type = "bar", group = "AETERM",
                         color = "AESEV", filters = f)
  ser <- blockr.core::blockr_ser(blk)
  expect_identical(ser$payload$filters, f)

  # Through the JSON file a board is saved as.
  back <- blockr.core::blockr_deser(board_json_trip(ser))
  expect_identical(blockr.core::blockr_ser(back)$payload$filters, f)

  with_chart(back, function(t) {
    expect_identical(t$state$filters(), f)
    expect_equal(t$result()$USUBJID, "S3")

    # The state a live click leaves serializes the same way.
    t$send(v2_click(list(AETERM = list("Nausea"), AESEV = list(NULL))))
    live <- lapply(t$state, function(s) s())
    again <- blockr.core::blockr_ser(blk, state = live)
    expect_identical(
      blockr.core::blockr_ser(
        blockr.core::blockr_deser(board_json_trip(again))
      )$payload$filters,
      list(AETERM = "Nausea", AESEV = NA_character_)
    )
  })
})

test_that("the chart is told the filter in both engines' words", {
  spy <- function(session) {
    sent <- new.env(parent = emptyenv())
    sent$msgs <- list()
    root <- session$rootScope()
    root$sendCustomMessage <- function(type, message) {
      if (identical(type, "drilldown-data")) {
        sent$msgs <- c(sent$msgs, list(message))
      }
      invisible(NULL)
    }
    function() sent$msgs[[length(sent$msgs)]]$config
  }
  blk <- new_chart_block(chart_type = "bar", group = "AETERM",
                         filters = list(AETERM = "Rash"))
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      cfg <- spy(session)
      session$flushReact()
      # v2 reads `filters`, v1 one column and its values.
      expect_equal(cfg()$filters, list(AETERM = list("Rash")))
      expect_equal(cfg()$filter_column, "AETERM")
      expect_equal(cfg()$filter_values, list("Rash"))
      expect_equal(
        shiny:::toJSON(cfg()$filters),
        structure('{"AETERM":["Rash"]}', class = "json")
      )
    },
    args = list(x = blk, data = list(data = filters_df))
  )

  # Two columns and a missing value: v1 is told none, v2 all of it.
  f <- list(AETERM = "Rash", AESEV = NA_character_)
  expect_equal(chart_filters_v1(f, "categorical"),
               list(column = NULL, values = NULL))
  expect_equal(
    as.character(shiny:::toJSON(chart_filters_json(f))),
    '{"AETERM":["Rash"],"AESEV":[null]}'
  )
  expect_equal(chart_filters_v1(list(AETERM = "Rash"), "range"),
               list(column = NULL, values = NULL))
})
