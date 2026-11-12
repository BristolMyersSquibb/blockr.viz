# A swimlane column's segments, overlays and lanes as JSON written by
# R/table-lane-json.R: the text has to be what Shiny writes for the same
# model.

lj_data <- function() {
  na <- NA_real_
  data.frame(
    id = c("a", "a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "c"),
    s = c(0, 40, 0, 50, 21, 42, 63, 70, 10, 0, 0, 5),
    e = c(40, 100, 30, 90, 26, 49, na, na, na, 60, 20, 12.345),
    resp = c("SD", "PR", "Drug A", "Drug B", "Delay", "Drug B", "Drug B",
             "Drug B", NA, "PD", "Drug A", "SD"),
    drug = factor(c(NA, NA, "Drug A", "Drug B", "Drug B", "Drug B", "Drug B",
                    "Drug B", "Drug B", NA, "Drug A", NA),
                  levels = c("Drug A", "Drug B")),
    style = c(NA, NA, NA, NA, "hollow", "dashed", "dot", "step", NA, NA, NA,
              "outline"),
    strip = c(NA, NA, NA, NA, NA, NA, "C1\n75 mg", NA, "C0", NA, NA, NA),
    ongoing = c(TRUE, NA, FALSE, TRUE, NA, NA, NA, NA, NA, NA, NA, NA),
    visit = c(10, 20, na, na, na, na, na, na, na, 30, na, na),
    from = c(5, na, na, na, na, na, na, na, na, 0, na, na),
    to = c(60, na, na, na, na, na, na, na, na, 50, na, na),
    day = c(45, na, 12, na, na, na, na, na, na, 70, na, na),
    what = factor(c("PD", NA, "SAE", NA, NA, NA, NA, NA, NA, "Death", NA, NA),
                  levels = c("PD", "SAE", "Death")),
    term = c("first", "sec\"ond", NA, "a, b", "</script>", "café", NA,
             NA, NA, "back\\slash", "tab\there", "{\"x\":[1]}"),
    stringsAsFactors = FALSE
  )
}

lj_model <- function(d, ..., by = "id") {
  st <- list(
    layout = "groups", by = by, drill = "auto",
    summaries = list(list(type = "spans", name = "Lane", x = "s", xend = "e",
                          color = "resp", ...))
  )
  st <- table_normalize(viz_check(viz_block_spec("table"), st))
  table_groups_to_model(table_model_groups(st, d))
}

expect_lane_json <- function(model, info = NULL) {
  out <- table_lane_json(model)
  expect_s3_class(out$cols[[2L]]$mark$segs, "json")
  expect_identical(as.character(shiny_json(out)),
                   as.character(shiny_json(model)), info = info)
}

test_that("swimlane columns are written as Shiny writes them", {
  d <- lj_data()
  models <- list(
    plain = lj_model(d),
    labelled = lj_model(d, label = "term", tooltip = c("resp", "term"),
                        bar_label = "term"),
    overlays = lj_model(d, ticks = "visit", band_x = "from", band_xend = "to",
                        event_x = "day", event_shape = "what",
                        open = "ongoing"),
    lanes = lj_model(d, lane = "drug", open = "ongoing", seg_style = "style",
                     strip = "strip", label = "term", bar_label = "term",
                     event_x = "day", event_shape = "what"),
    # No record on the main lane: even lanes, the main lane's arrays empty.
    even = lj_model(d[!is.na(d$drug), ], lane = "drug", label = "term",
                    bar_label = "term", open = "ongoing"),
    nested = lj_model(transform(d, arm = ifelse(id == "b", "B", "A")),
                      by = c("arm", "id"), lane = "drug", strip = "strip"),
    one_row = lj_model(d[1L, ], label = "term")
  )
  for (nm in names(models)) {
    expect_lane_json(models[[nm]], info = nm)
  }
  expect_s3_class(table_lane_json(models$lanes)$cols[[2L]]$mark$subLanes,
                  "json")
})

test_that("a model without a swimlane column comes back as it is", {
  st <- table_normalize(viz_check(viz_block_spec("table"), list(
    layout = "groups", by = "id",
    summaries = list(list(type = "simple", func = "count", name = "n"))
  )))
  m <- table_groups_to_model(table_model_groups(st, lj_data()))
  expect_identical(table_lane_json(m), m)
})

test_that("a lane with a field the writer does not know stays a list", {
  m <- lj_model(lj_data(), lane = "drug", strip = "strip")
  at <- which(lengths(m$cols[[2L]]$mark$subLanes) > 0L)[[1L]]
  m$cols[[2L]]$mark$subLanes[[at]][[1L]]$extra <- TRUE
  out <- table_lane_json(m)
  expect_type(out$cols[[2L]]$mark$subLanes, "list")
  expect_identical(as.character(shiny_json(out)), as.character(shiny_json(m)))
})
