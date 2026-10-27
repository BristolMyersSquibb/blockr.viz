# The table model (R/table-model.R) against the reference's recorded
# behaviour (fixtures/table-golden): from a recorded state under the spec's
# names and the scenario's data, each layout's model gives what the block
# sent the browser on its first render.

tm_vocab <- list(args = TRUE, summary_table = TRUE)

# Scenarios whose first render drew the scenario's own data: no prepare
# script, no options, a data frame as input.
tm_cases <- function(layout) {
  out <- list()
  for (nm in names(table_golden_scenarios())) {
    f <- test_path("fixtures", "table-golden", paste0(nm, ".rds"))
    if (!file.exists(f)) next
    r <- readRDS(f)
    if (!all(c("initial", "steps") %in% names(r))) next
    sc <- table_golden_scenarios()[[nm]]
    if (!is.null(sc$args$script) || length(sc$options)) next
    data <- tryCatch(table_golden_data(sc$data %||% "df"),
                     error = function(e) NULL)
    if (!is.data.frame(data)) next
    r <- table_golden_translate(r, tm_vocab)
    st <- r$steps[[1L]]
    state <- suppressWarnings(legacy_restore("table", st$state,
                                             VIZ_REWRITE_VERSION))
    if (!identical(state$layout, layout)) next
    out[[nm]] <- list(state = state, data = data, messages = st$messages)
  }
  out
}

tm_payload <- function(messages, type) {
  hit <- Filter(function(m) identical(m$type, type), messages)
  if (length(hit)) hit[[1L]]$message$payload
}

# A payload as the browser read it: JSON, parsed without simplifying.
tm_json <- function(x) {
  jsonlite::fromJSON(dt_payload_json(x), simplifyVector = FALSE)
}

test_that("table_model_records() gives the recorded records cells", {
  cases <- tm_cases("records")
  n <- 0L
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    want <- tm_payload(cs$messages, "blockr-viz-table-data")
    if (!identical(want$kind, "flat")) next
    tint <- table_model_tint(cs$state, cs$data)
    m <- table_model_records(cs$state, cs$data, tint = tint)
    got <- tm_json(dt_flat_payload(m))
    for (k in c("n", "cols", "nodrill")) {
      expect_identical(got[[k]], want[[k]], label = paste(nm, k))
    }
    n <- n + 1L
  }
  expect_gt(n, 10L)
})

test_that("table_model_records() hands a structured frame on", {
  st <- list(by = "grp", rowname = "id", missing = "level")
  d <- data.frame(grp = c("a", "b"), id = c("x", "y"), v = 1:2)
  m <- table_model_records(st, d)
  expect_identical(m$kind, "structured")
  expect_true(dt_is_structured(m$data))
})

test_that("table_model_groups() gives the recorded groups cells", {
  cases <- tm_cases("groups")
  n <- 0L
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    want <- tm_payload(cs$messages, "blockr-viz-summarize-data")
    if (!identical(want$kind, "flat")) next
    m <- table_model_groups(cs$state, cs$data)
    got <- tm_json(summarize_flat_payload(m$cells))
    for (k in c("n", "ncol", "label", "expanded", "pick", "cols")) {
      expect_identical(got[[k]], want[[k]], label = paste(nm, k))
    }
    n <- n + 1L
  }
  expect_gt(n, 10L)
})

test_that("table_model_groups() passes a preparation error on", {
  m <- table_model_groups(list(by = "nope", summaries = list()),
                          data.frame(a = 1))
  expect_false(is.null(m$prep$err))
  expect_null(m$cells)
})

test_that("table_model_variables() builds the recorded Table 1", {
  cases <- tm_cases("variables")
  n <- 0L
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    want <- tm_payload(cs$messages, "blockr-viz-table-data")
    if (!identical(want$kind, "html")) next
    tab <- table_model_variables(cs$state, cs$data)
    expect_true(dt_is_structured(tab), label = nm)
    # The rows the reference drew: every variable label is in its table.
    labels <- unique(as.character(tab$.label))
    for (lb in labels[nzchar(labels)]) {
      expect_true(grepl(htmltools::htmlEscape(lb), want$html, fixed = TRUE),
                  label = paste(nm, lb))
    }
    n <- n + 1L
  }
  expect_gt(n, 3L)
})

test_that("table_model_variables() returns the data without variables", {
  d <- data.frame(a = 1:2)
  expect_identical(table_model_variables(list(summaries = list()), d), d)
})

test_that("the row tint follows the color setting", {
  d <- data.frame(name = c("a", "b"), grp = c("x", "y"))
  expect_identical(table_model_tint(list(color = NULL), d),
                   list(col = NULL, hex = NULL))
  auto <- table_model_tint(list(color = "auto", rowname = "name"), d)
  expect_identical(auto$col, "name")
  col <- table_model_tint(list(color = "grp"), d)
  expect_identical(col$col, "grp")
  expect_length(col$hex, 2L)
})
