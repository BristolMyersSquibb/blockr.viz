# A table block's own gear (table_gear()): what the constructor takes and
# what the browser gets.

tgear <- function() {
  table_gear(
    tabs = list(
      Data = list(Time = c("unit", "cutoff")),
      Rows = c("by", "sort", "columns"),
      Titles = "titles",
      Interaction = c("drill", "search")
    ),
    columns = list(
      Weeks = list(type = "simple", func = "max", col = "AGE"),
      Sex = list(type = "field", col = "SEX", on = FALSE)
    ),
    groups = c(Arm = "ARM", "SEX"),
    clauses = c(unit = "by study {@unit}", cutoff = "cutoff {@cutoff}")
  )
}

tgear_run <- function(blk, data, code) {
  dat <- shiny::reactive(data)
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      expr <- session$makeScope("expr")
      session$flushReact()
      out <- expr$output$table
      if (!is.character(out)) out <- jsonlite::toJSON(out, auto_unbox = TRUE)
      code(jsonlite::fromJSON(out, simplifyVector = FALSE))
    },
    args = list(x = blk, data = list(data = dat))
  )
}

# A class with no registry entry takes default metadata, and says so.
tgear_block <- function(...) {
  suppressWarnings(new_table_block(...), classes = "missing_block_metadata")
}

test_that("table_gear() spells out tabs, sections and shorthands", {
  g <- tgear()
  expect_s3_class(g, "viz_table_gear")
  expect_identical(vapply(g$tabs, `[[`, "", "title"),
                   c("Data", "Rows", "Titles", "Interaction"))
  # A character vector is one section titled as its tab.
  expect_identical(g$tabs[[2L]]$sections[[1L]]$title, "Rows")
  expect_identical(g$tabs[[2L]]$sections[[1L]]$keys,
                   c("by", "sort_by", "sort_dir", "columns"))
  expect_identical(g$tabs[[1L]]$sections[[1L]]$title, "Time")
  # The ticks come with the titles when there are clauses.
  expect_identical(g$tabs[[3L]]$sections[[1L]]$keys,
                   c("title", "subtitle", "words", "caption"))
  expect_identical(g$tabs[[4L]]$sections[[1L]]$keys,
                   c("drill", "drill_target", "drill_table", "search"))
  expect_identical(
    table_gear(list(T = "titles"))$tabs[[1L]]$sections[[1L]]$keys,
    c("title", "subtitle", "caption")
  )
  expect_output(print(g), "Sex \\(off\\)")
  expect_identical(g$groups, list(values = c("ARM", "SEX"), labels = c("Arm", "SEX")))
  expect_error(table_gear(list("by")))
  expect_error(table_gear(list(A = "by"), columns = list(list(type = "field"))))
})

test_that("the ticked columns start a new block's summaries", {
  g <- tgear()
  s <- table_gear_summaries(g, list(list(type = "field", name = "Arm",
                                         col = "ARM")))
  expect_identical(vapply(s, `[[`, "", "name"), c("Weeks", "Arm"))
  expect_null(s[[1L]]$on)
})

test_that("the gear and class are not settings, and the gear reaches the payload", {
  g <- tgear()
  blk <- tgear_block(layout = "groups", by = "ARM", gear = g,
                     class = "my_table_block")
  expect_identical(class(blk)[1:2], c("my_table_block", "table_block"))
  expect_null(attr(blk, "gear"))
  expect_false("gear" %in% names(blockr.core::blockr_ser(blk)$payload))
  expect_identical(attr(attr(blk, "ctor"), "fun"), "new_table_block")
  d <- data.frame(ARM = c("A", "B"), AGE = c(30, 40), SEX = c("F", "M"))
  tgear_run(blk, d, function(p) {
    wire <- p$derived$gear
    expect_identical(vapply(wire$tabs, `[[`, "", "title"),
                     c("Data", "Rows", "Titles", "Interaction"))
    expect_identical(vapply(wire$columns, `[[`, "", "name"), c("Weeks", "Sex"))
    expect_identical(wire$columns[[2L]]$summary$col, "SEX")
    expect_identical(wire$clauses$cutoff, "cutoff {@cutoff}")
    expect_identical(unlist(wire$groups$labels), c("Arm", "SEX"))
    # A section with one key is still a list of keys.
    expect_identical(wire$tabs[[3L]]$sections[[1L]]$keys[[1L]], "title")
    one <- table_gear_wire(table_gear(list(A = "by")))
    expect_true(grepl('"keys":["by"]', jsonlite::toJSON(one, auto_unbox = TRUE),
                      fixed = TRUE))
  })
  # A plain table sends no gear.
  tgear_run(new_table_block(layout = "groups", by = "ARM"), d, function(p) {
    expect_null(p$derived$gear)
  })
  expect_error(new_table_block(gear = list(tabs = list())))
})

test_that("a subclass saved with its own constructor is rebuilt by it", {
  saved <- function(object, ctor, payload) {
    list(object = object, payload = payload,
         constructor = list(object = "blockr_ctor", constructor = ctor,
                            package = "blockr.viz"))
  }
  cls <- c("my_table_block", "table_block", "transform_block", "block")
  # blockr.core's default calls the saved constructor with the saved state.
  back <- blockr.core::blockr_deser(saved(cls, "new_chart_block",
                                          list(type = "bar")))
  expect_s3_class(back, "chart_block")
  # A plain table saved under another name keeps the legacy path.
  plain <- blockr.core::blockr_ser(new_table_block(layout = "groups", by = "ARM"))
  plain$constructor$constructor <- "new_some_table_block"
  back <- blockr.core::blockr_deser(plain)
  expect_s3_class(back, "table_block")
  expect_identical(attr(attr(back, "ctor"), "fun"), "new_table_block")
})

test_that("a block with its own constructor lets others set only its arguments", {
  spec <- viz_block_spec("table")
  expect_identical(table_ctrl_args(spec, NULL, NULL), viz_ctrl_args(spec))
  # new_chart_block() shares title, subtitle, caption, ... with the table.
  own <- table_ctrl_args(spec, "new_chart_block", "blockr.viz")
  expect_true(all(own %in% names(formals(new_chart_block))))
  expect_true("title" %in% own)
  expect_false("layout" %in% own)
  expect_identical(table_ctrl_args(spec, "nope", "blockr.viz"), viz_ctrl_args(spec))
})

test_that("a gear's info reads the input, and its deps and open tab reach the page", {
  dep <- htmltools::htmlDependency("my-rows", "1.0", src = c(href = "x"),
                                   script = "rows.js")
  g <- table_gear(list(Data = "mapping"), open = "Data", deps = list(dep),
                  info = function(data) list(n = nrow(data), cols = names(data)))
  blk <- tgear_block(layout = "groups", by = "ARM", gear = g, class = "my_table_block")
  d <- data.frame(ARM = c("A", "B"), AGE = c(30, 40))
  tgear_run(blk, d, function(p) {
    expect_identical(p$derived$gear$info$n, 2L)
    expect_identical(unlist(p$derived$gear$info$cols), c("ARM", "AGE"))
    expect_identical(p$derived$gear$open, "Data")
  })
  ui <- htmltools::renderTags(blk$expr_ui("x"))
  expect_true(any(vapply(htmltools::findDependencies(blk$expr_ui("x")),
                         function(x) x$name == "my-rows", logical(1L))))
  # An info that fails sends nothing, and the block still draws.
  g2 <- table_gear(list(Data = "mapping"), info = function(data) stop("no"))
  tgear_run(tgear_block(layout = "groups", by = "ARM", gear = g2), d, function(p) {
    expect_null(p$derived$gear$info)
    expect_identical(p$derived$gear$tabs[[1L]]$title, "Data")
  })
})
