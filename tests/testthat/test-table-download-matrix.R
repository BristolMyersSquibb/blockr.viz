# Every download the table block offers, for every layout and look, through
# the block's own server. The groups layout hands its whole argument list to
# the exhibit and its writers, so an argument the screen takes and a writer
# does not breaks that download (`pct_of` and `missing` broke the web page).
# This clicks each format the menu offers and checks a file comes back.

dl_matrix_data <- function() {
  data.frame(
    arm = rep(c("A", "B", NA), 10),
    sex = rep(c("F", "M"), 15),
    age = seq_len(30),
    stringsAsFactors = FALSE
  )
}

# The output ids the download control offers, read off its rendered menu.
dl_offered <- function(ui) {
  html <- paste(as.character(ui$html), collapse = "")
  ids <- regmatches(html, gregexpr("id=\"[^\"]*-dl_[a-z]+\"", html))[[1L]]
  unique(sub("^.*-(dl_[a-z]+)\"$", "\\1", ids))
}

for (layout in c("records", "groups", "variables")) {
  for (look in c("table", "compact", "cards")) {
    test_that(paste("every download writes:", layout, "layout,", look,
                    "look"), {
      skip_if_not(summarize_paint_ready())
      blk <- new_table_block(
        layout = layout, look = look, download = TRUE, by = "arm",
        summaries = list(
          list(type = "simple", func = "count", show = "bar", color = "sex",
               name = "sex"),
          list(type = "dist", col = "age", show = "box")
        ),
        # Not the defaults, so a writer that drops them is exercised too.
        pct_of = "total", missing = "level",
        title = "Downloads"
      )
      d <- dl_matrix_data()
      engine <- if (identical(layout, "groups")) "groups" else "records"
      slot <- if (identical(layout, "groups")) {
        "summarize_download"
      } else {
        "dt_download"
      }
      shiny::testServer(
        blockr.core:::get_s3_method("block_server", blk),
        {
          session$flushReact()
          out <- function(id) output[[paste0("expr-", engine, "-", id)]]
          offered <- dl_offered(out(slot))
          expect_true(all(c("dl_xlsx", "dl_html") %in% offered))
          if (requireNamespace("officer", quietly = TRUE)) {
            expect_true("dl_pptx" %in% offered)
          }
          for (id in offered) {
            # The deck writer says when a table carries over slides.
            f <- suppressMessages(out(id))
            expect_true(file.exists(f), info = id)
            expect_gt(file.size(f), 0, label = id)
          }
        },
        args = list(x = blk, data = list(data = function() d))
      )
    })
  }
}

test_that("the groups web page honours `missing`", {
  d <- dl_matrix_data()
  s <- list(list(type = "simple", func = "count", show = "bar", name = "N"))
  page <- function(missing) {
    e <- static_summarize_table(d, by = "arm", summaries = s,
                                missing = missing)
    f <- tempfile(fileext = ".html")
    on.exit(unlink(f))
    write_exhibit_html(e, f)
    paste(readLines(f, warn = FALSE), collapse = "\n")
  }
  # Missing as a level: the page has a "(Missing)" row.
  expect_match(page("level"), "(Missing)", fixed = TRUE)
  expect_false(grepl("(Missing)", page("drop"), fixed = TRUE))
})
