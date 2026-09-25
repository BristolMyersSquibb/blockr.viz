# expand_groups() and the consumers that split by a group column carrying a
# `blockr_groups` definition (spec: _blockr.design/open/global-groups).

arms <- c("Pbo", "Low", "High")

grouped_df <- function(overlap = TRUE) {
  d <- data.frame(
    USUBJID = sprintf("s%02d", 1:6),
    TRT = c("Pbo", "Pbo", "Low", "Low", "High", "High"),
    AGE = c(60, 61, 70, 71, 80, 81),
    stringsAsFactors = FALSE
  )
  attr(d$AGE, "label") <- "Age"
  if (overlap) {
    d$Group <- d$TRT
    def <- list(
      groups = list(All = c("Low", "High"), Pbo = "Pbo", High = "High"),
      overlap = TRUE
    )
  } else {
    d$Group <- c("Pbo", "Pbo", "All", "All", "All", "All")
    def <- list(
      groups = list(Pbo = "Pbo", All = c("Low", "High")),
      overlap = FALSE
    )
  }
  attr(d$Group, "blockr_source") <- "TRT"
  attr(d$Group, "label") <- "Treatment group"
  attr(d$Group, "blockr_groups") <- def
  attr(d, "blockr_filters") <- list("trail")
  d
}

test_that("no definition: data comes back unchanged", {
  d <- grouped_df()
  attr(d$Group, "blockr_groups") <- NULL
  expect_identical(expand_groups(d), d)
  expect_identical(expand_groups(d, "nope"), d)
})

test_that("overlap: one copy of each group's member rows, in group order", {
  d <- grouped_df()
  e <- expand_groups(d)

  expect_equal(nrow(e), 4 + 2 + 2)
  expect_true(is.factor(e$Group))
  expect_equal(levels(e$Group), c("All", "Pbo", "High"))
  expect_equal(as.character(e$Group), rep(c("All", "Pbo", "High"), c(4, 2, 2)))
  expect_equal(e$USUBJID[e$Group == "All"], c("s03", "s04", "s05", "s06"))
  expect_equal(e$TRT[e$Group == "High"], c("High", "High"))

  # Attributes: data frame, other columns, the group column.
  expect_equal(attr(e, "blockr_filters"), list("trail"))
  expect_equal(attr(e$AGE, "label"), "Age")
  expect_equal(attr(e$Group, "blockr_source"), "TRT")
  expect_equal(attr(e$Group, "label"), "Treatment group")
  expect_equal(
    attr(e$Group, "blockr_groups"),
    list(groups = list(All = "All", Pbo = "Pbo", High = "High"),
         overlap = FALSE)
  )
  expect_equal(rownames(e), as.character(seq_len(nrow(e))))
})

test_that("a second call is a no-op", {
  e <- expand_groups(grouped_df())
  expect_identical(expand_groups(e), e)
  p <- expand_groups(grouped_df(overlap = FALSE))
  expect_identical(expand_groups(p), p)
})

test_that("partition: keeps rows naming a group, factor in group order", {
  d <- grouped_df(overlap = FALSE)
  d$Group[1] <- NA
  e <- expand_groups(d)
  expect_equal(nrow(e), 5)
  expect_equal(levels(e$Group), c("Pbo", "All"))
  expect_equal(attr(e$Group, "blockr_source"), "TRT")
})

test_that("tibbles keep their class and attributes", {
  d <- tibble::as_tibble(grouped_df())
  attr(d, "blockr_filters") <- list("trail")
  e <- expand_groups(d)
  expect_s3_class(e, "tbl_df")
  expect_equal(nrow(e), 8)
  expect_equal(attr(e, "blockr_filters"), list("trail"))
  expect_equal(attr(e$Group, "blockr_source"), "TRT")
})

test_that("a group without members is every subject", {
  d <- grouped_df()
  attr(d$Group, "blockr_groups") <- list(
    groups = list(All = NULL, Pbo = "Pbo"), overlap = TRUE
  )
  e <- expand_groups(d)
  expect_equal(as.vector(table(e$Group)), c(6, 2))
})

test_that("group_column_values maps a group name to what the column holds", {
  d <- grouped_df()
  expect_equal(group_column_values(d$Group, "All"), c("Low", "High"))
  expect_equal(group_column_values(d$Group, c("Pbo", "x")), c("Pbo", "x"))
  e <- expand_groups(d)
  # The expanded column holds names; upstream holds the members.
  expect_equal(group_column_values(e$Group, "All"), "All")
  expect_equal(group_column_values(e$Group, "All", origin = TRUE),
               c("Low", "High"))
  p <- grouped_df(overlap = FALSE)
  expect_equal(group_column_values(p$Group, "All"), "All")
})

test_that("chart roles decide whether the chart expands", {
  d <- grouped_df()
  expect_equal(nrow(expand_role_groups(d, c("TRT", "AGE"))), 6)
  expect_equal(nrow(expand_role_groups(d, chart_split_roles(color = "Group"))),
               8)
})

# --- drill claims ----------------------------------------------------------

test_that("a pooled click claims the source column as a set (overlap)", {
  d <- grouped_df()
  out <- dd_ctrl_claims(d, "adsl", list(Group = "All"))
  expect_length(out, 1L)
  expect_equal(out[[1]]$name, "TRT")
  expect_equal(out[[1]]$mode, "multi")
  expect_equal(out[[1]]$values, c("Low", "High"))

  # A single-level group still claims one value.
  out <- dd_ctrl_claims(d, "adsl", list(Group = "High"))
  expect_equal(out[[1]]$values, "High")
})

test_that("a pooled click claims the source as a set (partition)", {
  d <- grouped_df(overlap = FALSE)
  out <- dd_ctrl_claims(d, "", list(Group = "All"))
  expect_equal(out[[1]]$name, "TRT")
  expect_equal(out[[1]]$values, c("Low", "High"))
  expect_null(out[[1]]$table)
})

test_that("without a source column the group column claims its members", {
  d <- grouped_df()
  d$TRT <- NULL
  out <- dd_ctrl_claims(d, "", list(Group = "All"))
  expect_equal(out[[1]]$name, "Group")
  expect_equal(out[[1]]$values, c("Low", "High"))

  # Same answer from the expanded frame a chart draws.
  e <- expand_groups(d)
  out <- dd_ctrl_claims(e, "", list(Group = "All"))
  expect_equal(out[[1]]$values, c("Low", "High"))
})

test_that("group claims sit beside ordinary ones", {
  d <- grouped_df()
  d$SEX <- c("F", "M", "F", "F", "F", "M")
  out <- dd_ctrl_claims(d, "", list(Group = "All", SEX = "F"))
  expect_equal(vapply(out, `[[`, "", "name"), c("TRT", "SEX"))
  expect_equal(out[[1]]$values, c("Low", "High"))
  expect_equal(out[[2]]$values, "F")
})

# --- export path -------------------------------------------------------------

test_that("chart_expr expands when a role binds a defined column", {
  d <- grouped_df()
  ex <- chart_expr("data", chart_type = "bar", group = "Group", data = d,
                   qualify = TRUE)
  code <- chart_code(ex)
  expect_match(code, "blockr.viz::expand_groups(\"Group\")", fixed = TRUE)
  p <- eval(ex, list(data = d))
  expect_equal(nrow(p$data), 3)
  expect_equal(sort(p$data$n), c(2, 2, 4))

  # Not bound to a splitting role: no expansion.
  ex <- chart_expr("data", chart_type = "bar", group = "TRT", data = d)
  expect_no_match(chart_code(ex), "expand_groups")

  # No snapshot: the board's group column by name.
  ex <- chart_expr("data", chart_type = "bar", group = "Group")
  expect_match(chart_code(ex), "expand_groups", fixed = TRUE)
  ex <- chart_expr("data", chart_type = "bar", group = "Group",
                   facet = "Subgroup")
  expect_match(chart_code(ex), 'expand_groups\\(.*"Subgroup"\\)')
  ex <- chart_expr("data", chart_type = "bar", group = "SEX")
  expect_no_match(chart_code(ex), "expand_groups")
})

test_that("static_chart draws one bar per group", {
  skip_if_not_installed("ggplot2")
  d <- grouped_df()
  p <- static_chart(d, chart_type = "bar", group = "Group")
  expect_setequal(as.character(p$data$Group), c("All", "Pbo", "High"))
  n <- p$data[[setdiff(names(p$data), "Group")[1]]]
  expect_equal(sort(n), c(2, 2, 4))
})

# --- other blocks ------------------------------------------------------------

test_that("heatmap rows repeat per group under an overlap definition", {
  d <- grouped_df()
  d$AEDECOD <- "Headache"
  p <- heatmap_prep(d, "USUBJID", "AEDECOD", group = "Group")
  expect_equal(length(p$rows), 8)
  expect_equal(unname(vapply(p$groups, `[[`, 1L, "n")), c(4, 2, 2))
  expect_equal(p$rows[p$group_of == "High"], c("s05", "s06"))
})

test_that("summarize table counts per group, percent over subjects", {
  d <- grouped_df()
  p <- rank_prepare(d, group = "Group", func = "count", cols = c("n", "pct"))
  expect_null(p$err)
  n <- stats::setNames(p$rows$.v, p$rows$.label)
  expect_equal(unname(n[c("All", "Pbo", "High")]), c(4, 2, 2))
  # The percentage base is the six subjects, not the eight expanded rows.
  expect_equal(p$denoms[["all"]], 6)
})

# --- colours -----------------------------------------------------------------

test_that("a pool takes a member's pinned colour", {
  skip_if_not_installed("blockr.theme")
  map <- list(TRT = list(color = c(Pbo = "#111111", Low = "#222222",
                                   High = "#333333")))
  e <- expand_groups(grouped_df())
  res <- dd_resolve_scales(map, "Group", e$Group)
  expect_equal(res$color[["All"]], "#222222")
  expect_equal(res$color[["High"]], "#333333")
  expect_equal(res$color[["Pbo"]], "#111111")
  expect_equal(res$order, c("All", "Pbo", "High"))
  expect_false("Low" %in% names(res$color))

  # A pool name the map pins keeps its own colour.
  map$TRT$color <- c(map$TRT$color, All = "#444444")
  res <- dd_resolve_scales(map, "Group", e$Group)
  expect_equal(res$color[["All"]], "#444444")
})

# --- chart block, own filter -------------------------------------------------

test_that("a click on a pooled group filters the input on its members", {
  d <- grouped_df()
  blk <- new_chart_block(chart_type = "bar", group = "Group")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      expr_scope <- session$makeScope("expr")
      expr_scope$setInputs(drilldown_block_action = list(
        action = "filter",
        filter_type = "categorical",
        column = "Group",
        values = list("All")
      ))
      session$flushReact()
      result <- session$returned$result()
      expect_equal(sort(result$USUBJID), c("s03", "s04", "s05", "s06"))
      code <- paste(deparse(session$returned$expr()), collapse = " ")
      expect_match(code, 'c("Low", "High")', fixed = TRUE)
    },
    args = list(x = blk, data = list(data = function() d))
  )
})
