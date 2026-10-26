# The groups layout has one data model, `by` + `summaries`. Boards saved with
# the old summarize table's single ranked bar (`group`, `parent`, `func`,
# `value`, `id_var`, `color`, `facet`, `bar_mode`, `cols`, `fields`) restore
# into it through table_legacy_args(), and draw the numbers they drew.

# Events with repeats: a subject has several events of one term, so a count
# of rows and a count of subjects differ.
one_list_ae <- function() {
  d <- data.frame(
    USUBJID = c("S1", "S1", "S2", "S3", "S3", "S3", "S4", "S5", "S5", "S6"),
    TERM = c("Rash", "Rash", "Rash", "Itch", "Itch", "Rash", "Cough",
             "Cough", "Itch", "Cough"),
    SOC = c("Skin", "Skin", "Skin", "Skin", "Skin", "Skin", "Lung", "Lung",
            "Skin", "Lung"),
    SEV = c("MILD", "SEVERE", "MILD", "MILD", "MILD", "SEVERE", "MILD",
            "SEVERE", "MILD", "MILD"),
    ARM = c("A", "A", "B", "A", "A", "A", "B", "B", "B", "A"),
    AVAL = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    stringsAsFactors = FALSE
  )
  d
}

# A board saved before the rewrite: the summarize table's payload, restored
# by blockr.core through the class method.
restore_ranked <- function(...) {
  blockr_deser.summarize_table_block(
    structure(list(), class = "summarize_table_block"),
    list(payload = c(list(...), list(summaries = list(), by = NULL)))
  )
}

# What the restored block draws: its saved state, through the same prep the
# block's server runs.
drawn <- function(b, data) {
  p <- blockr.core::blockr_ser(b)$payload
  summarize_prepare(data, summaries = p$summaries, by = p$by,
                    sort_by = p$sort_by, sort_dir = p$sort_dir,
                    top_n = p$top_n)
}

values_of <- function(prep, key = ".v") {
  r <- prep$rows
  stats::setNames(r[[key]], r$.label)
}

test_that("a ranked distinct count restores as one bar column of subjects", {
  d <- one_list_ae()
  b <- restore_ranked(group = "TERM", value = ".count",
                      func = "count_distinct", id_var = "USUBJID",
                      compare = NULL, block_name = "AEs")
  p <- blockr.core::blockr_ser(b)$payload
  expect_identical(p$layout, "groups")
  expect_identical(p$by, "TERM")
  expect_length(p$summaries, 1L)
  expect_identical(p$summaries[[1L]]$type, "simple")
  expect_identical(p$summaries[[1L]]$func, "count_distinct")
  expect_identical(p$summaries[[1L]]$col, "USUBJID")
  expect_identical(p$summaries[[1L]]$show, "bar")
  expect_false(any(c("group", "func", "value", "id_var", "compare") %in%
                     names(p)))

  # The ranked bar counted distinct subjects per term, ordered by the count.
  want <- tapply(d$USUBJID, d$TERM, function(x) length(unique(x)))
  got <- values_of(drawn(b, d))
  expect_equal(got[names(want)], c(want), ignore_attr = TRUE)
  expect_true(all(diff(got) <= 0))
  expect_identical(names(got)[[3L]], "Itch")
})

test_that("a parent restores as the outer grouping, aggregated on its own", {
  d <- one_list_ae()
  b <- restore_ranked(group = "TERM", parent = "SOC",
                      func = "count_distinct", id_var = "USUBJID")
  expect_identical(blockr.core::blockr_ser(b)$payload$by, c("SOC", "TERM"))
  prep <- drawn(b, d)
  r <- prep$rows
  par <- r[is.na(r$.parent), , drop = FALSE]
  # Skin has four subjects, not the sum over its terms: Rash 3 + Itch 2
  # counts S3 twice.
  expect_equal(stats::setNames(par$.v, par$.label),
               c(Skin = 4, Lung = 3)[par$.label])
  kids <- r[!is.na(r$.parent) & r$.parent == "Skin", , drop = FALSE]
  expect_setequal(kids$.label, c("Rash", "Itch"))
})

test_that("a colour split and its bar mode restore onto the column", {
  d <- one_list_ae()
  b <- restore_ranked(group = "TERM", func = "count", color = "SEV",
                      bar_mode = "grouped")
  s <- blockr.core::blockr_ser(b)$payload$summaries[[1L]]
  expect_identical(s$color, "SEV")
  expect_identical(s$position, "dodge")
  prep <- drawn(b, d)
  expect_identical(prep$color, "SEV")
  # Event counts per term and severity, as the stacked segments were. A
  # level a term has no rows of is no segment: the list leaves it NA where
  # the ranked bar wrote 0, and both draw nothing.
  want <- table(d$TERM, d$SEV)
  r <- prep$rows
  cnt <- prep$plan[[1L]]
  for (lv in colnames(want)) {
    col <- grep(paste0("_", lv, "$"), names(r), value = TRUE)[[1L]]
    got <- stats::setNames(r[[col]], r$.label)[rownames(want)]
    got[is.na(got)] <- 0
    expect_equal(got, c(want[, lv]), ignore_attr = TRUE)
  }
  expect_identical(cnt$kind, "barsplit")
})

test_that("a facet restores onto the column, one column per level", {
  d <- one_list_ae()
  b <- restore_ranked(group = "TERM", func = "count_distinct",
                      id_var = "USUBJID", facet = "ARM")
  prep <- drawn(b, d)
  expect_setequal(prep$facet_levels, c("A", "B"))
  want <- tapply(d$USUBJID, list(d$TERM, d$ARM),
                 function(x) length(unique(x)))
  want[is.na(want)] <- 0
  for (lv in c("A", "B")) {
    pl <- Filter(function(p) identical(p$flevel, lv), prep$plan)[[1L]]
    got <- stats::setNames(prep$rows[[pl$key]], prep$rows$.label)
    expect_equal(got[rownames(want)], want[, lv], ignore_attr = TRUE)
  }
})

test_that("a mean and the as-is measure with fields restore as columns", {
  d <- one_list_ae()
  b <- restore_ranked(group = "TERM", func = "mean", value = "AVAL",
                      cols = c("n", "pct"))
  s <- blockr.core::blockr_ser(b)$payload$summaries
  expect_identical(vapply(s, `[[`, "", "func"), c("mean", "count"))
  expect_identical(s[[1L]]$col, "AVAL")
  got <- values_of(drawn(b, d))
  want <- tapply(d$AVAL, d$TERM, mean)
  expect_equal(got[names(want)], c(want), ignore_attr = TRUE)

  per <- data.frame(USUBJID = c("S1", "S2", "S3"), BMI = c(21, 25, 30),
                    ARM = c("A", "B", "A"))
  bi <- restore_ranked(group = "USUBJID", func = "identity", value = "BMI",
                       fields = "ARM")
  si <- blockr.core::blockr_ser(bi)$payload$summaries
  expect_identical(vapply(si, `[[`, "", "type"), c("simple", "field"))
  prep <- drawn(bi, per)
  expect_equal(values_of(prep)[per$USUBJID], c(S1 = 21, S2 = 25, S3 = 30))
  expect_identical(vapply(prep$plan, function(p) p$label, ""),
                   c("BMI", "ARM"))
  # Fields beside any other measure were ignored by the bar, so they map to
  # nothing.
  bf <- restore_ranked(group = "TERM", func = "count", fields = "ARM")
  expect_length(blockr.core::blockr_ser(bf)$payload$summaries, 1L)
})

test_that("sort, top_n and the fold carry over to the list", {
  d <- one_list_ae()
  b <- restore_ranked(group = "TERM", func = "count", sort_by = "value",
                      sort_dir = "asc", top_n = 2L)
  prep <- drawn(b, d)
  got <- values_of(prep)
  expect_length(got, 2L)
  expect_true(all(diff(got) >= 0))
  expect_identical(names(got), c("Cough", "Itch"))
  expect_identical(prep$folded, 1L)
  # By label, descending: the name order backwards.
  bl <- restore_ranked(group = "TERM", func = "count", sort_by = "label",
                       sort_dir = "desc")
  expect_identical(names(values_of(drawn(bl, d))), c("Rash", "Itch", "Cough"))
})

test_that("the old ranked arguments still draw through static_summarize_table()", {
  d <- one_list_ae()
  e <- static_summarize_table(d, group = "TERM", func = "count_distinct",
                              id_var = "USUBJID")
  want <- tapply(d$USUBJID, d$TERM, function(x) length(unique(x)))
  expect_equal(values_of(e$prep)[names(want)], c(want), ignore_attr = TRUE)
})

test_that("a new groups table starts with one count column, drawn as a bar", {
  b <- new_table_block(layout = "groups", by = "TERM")
  p <- blockr.core::blockr_ser(b)$payload
  expect_identical(p$summaries, table_groups_seed())
  expect_identical(p$summaries[[1L]][c("type", "func", "show", "name")],
                   list(type = "simple", func = "count", show = "bar",
                        name = "Count"))
  # The old constructor with nothing set gives a groups table with the same
  # column.
  p0 <- blockr.core::blockr_ser(new_summarize_table_block())$payload
  expect_identical(p0$layout, "groups")
  expect_identical(p0$summaries[[1L]][c("type", "func", "show")],
                   list(type = "simple", func = "count", show = "bar"))
})

test_that("switching to groups seeds the count column, and the list never empties", {
  d <- one_list_ae()
  b <- new_table_block()
  shiny::testServer(b$expr_server, args = list(data = shiny::reactive(d)), {
    session$setInputs(`records-drilldown_table_block_action` = list(
      action = "config", param = "layout", value = "groups"
    ))
    expect_identical(session$returned$state$summaries(), table_groups_seed())
    # A list sent empty (the gear never does) gets the count column back.
    session$setInputs(`groups-summarize_table_block_action` = list(
      action = "config", param = "summaries", value = list()
    ))
    expect_identical(session$returned$state$summaries(), table_groups_seed())
  })
})

test_that("an empty column list counts rows", {
  d <- one_list_ae()
  p <- summarize_prepare(d, by = "TERM")
  expect_equal(values_of(p)[c("Rash", "Itch", "Cough")],
               c(Rash = 4, Itch = 3, Cough = 3))
})

test_that("the ranked bar's arguments are gone from the specs and the server", {
  expect_false(any(c("group", "func", "value", "id_var", "parent", "color",
                     "bar_mode", "facet", "cols", "fields") %in%
                     names(summarize_arguments())))
  expect_false(any(c("group", "func", "value", "parent", "bar_mode",
                     "cols", "fields") %in%
                     names(formals(table_groups_server))))
  expect_false(any(c("group", "func", "value", "parent") %in%
                     names(formals(summarize_prepare))))
})

test_that("a restored faceted bar ranks rows by the value over all levels", {
  # S1 has four events, all on arm P; S3 has two, all on arm H. Ranked by its
  # first level (H) S3 would lead; the bar ranked by the pooled count.
  d <- data.frame(
    USUBJID = c("S1", "S1", "S1", "S1", "S3", "S3", "S2"),
    ARM = c("P", "P", "P", "P", "H", "H", "H"),
    stringsAsFactors = FALSE
  )
  b <- restore_ranked(group = "USUBJID", func = "count", facet = "ARM")
  prep <- drawn(b, d)
  expect_identical(prep$rows$.label, c("S1", "S3", "S2"))
  expect_equal(values_of(prep), c(S1 = 4, S3 = 2, S2 = 1))
  # Ascending reverses the same pooled order.
  ba <- restore_ranked(group = "USUBJID", func = "count", facet = "ARM",
                       sort_dir = "asc")
  expect_identical(drawn(ba, d)$rows$.label, c("S2", "S3", "S1"))
})

test_that("a restored count over pooled groups takes its percent over input rows", {
  d <- data.frame(
    USUBJID = sprintf("s%02d", 1:6),
    TRT = c("Pbo", "Pbo", "Low", "Low", "High", "High"),
    stringsAsFactors = FALSE
  )
  d$Group <- d$TRT
  attr(d$Group, "blockr_source") <- "TRT"
  attr(d$Group, "blockr_groups") <- list(
    groups = list(All = c("Low", "High"), Pbo = "Pbo", High = "High"),
    overlap = TRUE
  )
  b <- restore_ranked(group = "Group", func = "count")
  prep <- drawn(b, d)
  # Six input rows, copied into eight by the pooled All group: the base is
  # six, as the bar had it, so All reads 4 of 6.
  expect_equal(values_of(prep)[c("All", "Pbo", "High")],
               c(All = 4, Pbo = 2, High = 2))
  expect_equal(prep$plan[[1L]]$denom, 6)
  # A distinct count counts subjects either way.
  bd <- restore_ranked(group = "Group", func = "count_distinct",
                       id_var = "USUBJID")
  expect_equal(drawn(bd, d)$plan[[1L]]$denom, 6)
  # The row tag never reaches the drawn rows.
  expect_false(SUMMARIZE_ROW_ID %in% names(prep$rows))
})

test_that("a restored bar's header reads the column's label", {
  d <- one_list_ae()
  attr(d$AVAL, "label") <- "Analysis Value"
  b <- restore_ranked(group = "TERM", func = "mean", value = "AVAL")
  expect_identical(drawn(b, d)$plan[[1L]]$label, "Mean Analysis Value")
  per <- data.frame(USUBJID = c("S1", "S2"), BMI = c(21, 25))
  attr(per$BMI, "label") <- "Body mass index"
  bi <- restore_ranked(group = "USUBJID", func = "identity", value = "BMI")
  expect_identical(drawn(bi, per)$plan[[1L]]$label, "Body mass index")
  # A column without a label keeps its name.
  bn <- restore_ranked(group = "TERM", func = "max", value = "AVAL")
  expect_identical(drawn(bn, one_list_ae())$plan[[1L]]$label, "Max AVAL")
})
