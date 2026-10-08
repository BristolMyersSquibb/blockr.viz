# Custom summary rows: a function of the rows grouped by table row, run once
# per level of `by` and per facet level.

custom_fixture <- function() {
  data.frame(
    SOC = c("Skin", "Skin", "Skin", "Skin", "Skin", "GI"),
    TERM = c("Pruritus", "Erythema", "Pruritus", "Erythema", "Erythema",
             "Nausea"),
    USUBJID = c("P1", "P1", "P2", "P3", "P3", "P1"),
    ARM = c("A", "A", "B", "B", "B", "A"),
    GR = c("1", "3", "2", "1", "2", "2"),
    S = c(1, 5, 3, 10, 12, 4),
    E = c(4, 9, 8, 11, 20, 6),
    stringsAsFactors = FALSE
  )
}

worst_fn <- paste(
  "\\(d) d |>",
  "  dplyr::arrange(dplyr::desc(as.numeric(GR))) |>",
  "  dplyr::distinct(USUBJID, .keep_all = TRUE) |>",
  "  dplyr::count(GR, name = \"value\")",
  sep = "\n"
)

n_fn <- "dplyr::summarise(d, value = dplyr::n())"

row_of <- function(rows, label) rows[rows$.label == label, , drop = FALSE]

test_that("a worst-grade count nests: the SOC row reduces its own rows", {
  prep <- summarize_prepare(custom_fixture(), by = c("SOC", "TERM"),
                            summaries = list(list(type = "custom",
                                                  name = "Patients",
                                                  fn = worst_fn)))
  expect_null(prep$err)
  expect_identical(prep$plan[[1]]$kind, "barsplit")
  # The block cannot tell a count from a mean: custom levels sit side by side.
  expect_identical(prep$plan[[1]]$mode, "grouped")
  r <- prep$rows
  skin <- row_of(r, "Skin")
  # P1 is grade 1 for Pruritus and grade 3 for Erythema: once, at 3.
  expect_equal(skin$.s1_v, 3)
  expect_equal(c(skin$.s1_S_1, skin$.s1_S_2, skin$.s1_S_3), c(NA, 2, 1))
  pru <- row_of(r, "Pruritus")
  expect_equal(c(pru$.s1_S_1, pru$.s1_S_2, pru$.s1_S_3), c(1, 1, NA))
})

test_that("a split dumbbell draws one pair per level, the pooled pair sorts", {
  prep <- summarize_prepare(custom_fixture(), by = "TERM", summaries = list(
    list(type = "custom", name = "Onset to end",
         fn = paste("d |> dplyr::group_by(GR, .add = TRUE) |>",
                    "dplyr::summarise(from = mean(S), to = mean(E),",
                    ".groups = \"drop\")"))
  ))
  expect_null(prep$err)
  p <- prep$plan[[1]]
  expect_identical(p$kind, "pair")
  expect_length(p$lcols, 3L)
  ery <- row_of(prep$rows, "Erythema")
  # Erythema: grade 3 (5 -> 9), grade 1 (10 -> 11), grade 2 (12 -> 20).
  expect_equal(ery[[p$lcols[[3]][["a"]]]], 5)
  expect_equal(ery[[p$lcols[[2]][["b"]]]], 20)
  c1 <- summarize_cells(prep)$cols[[1]]
  expect_true(isTRUE(c1$multi))
})

test_that("the mark is read off the returned columns, or named", {
  d <- custom_fixture()
  kinds <- function(...) {
    prep <- summarize_prepare(d, by = "TERM", summaries = list(...))
    expect_null(prep$err)
    vapply(prep$plan, function(p) p$kind, character(1))
  }
  expect_identical(
    kinds(list(type = "custom", fn = n_fn),
          list(type = "custom", fn = n_fn, show = "number"),
          list(type = "custom", fn = "dplyr::distinct(d, text = ARM)"),
          list(type = "custom",
               fn = paste("dplyr::summarise(d, lo = min(S), mid = median(S),",
                          "hi = max(S))"),
               show = "pointrange")),
    c("bar", "num", "num", "pointrange")
  )
})

test_that("a bare body using `d` works like a function", {
  prep <- summarize_prepare(custom_fixture(), by = "TERM", summaries = list(
    list(type = "custom", name = "Rows", fn = n_fn)
  ))
  expect_equal(row_of(prep$rows, "Erythema")$.s1_v, 3)
})

test_that("a function that drops the grouping says so, without an error", {
  prep <- summarize_prepare(custom_fixture(), by = "TERM", summaries = list(
    list(type = "custom", name = "X", fn = "c(value = nrow(d))")
  ))
  expect_match(prep$err, "Summary \"X\": the function dropped the grouping by TERM")
})

test_that("a facet runs the function per facet level", {
  prep <- summarize_prepare(custom_fixture(), by = "TERM", summaries = list(
    list(type = "custom", name = "Patients", fn = worst_fn, facet = "ARM")
  ))
  expect_null(prep$err)
  r <- prep$rows
  ery <- row_of(r, "Erythema")
  # ARM A: P1 at grade 3. ARM B: P3 at grade 2.
  expect_equal(ery$.s1f1_S_3, 1)
  expect_equal(ery$.s1f2_S_2, 1)
})

test_that("a broken function says which column and why", {
  d <- custom_fixture()
  bad <- function(fn, show = NULL) {
    summarize_prepare(d, by = "TERM", summaries = list(
      c(list(type = "custom", name = "X", fn = fn), if (!is.null(show))
        list(show = show))
    ))$err
  }
  expect_match(bad("\\(d) d |> "), "Summary \"X\": the function does not parse")
  expect_match(bad("stop(\"nope\")"), "failed on the whole data: nope")
  expect_match(bad("data.frame(a = 1, b = 2)"), "not values \\(a, b\\)")
  expect_match(bad("data.frame(n = 1)"), "no mark reads that")
  expect_match(bad("c(value = 1)", show = "dumbbell"),
               "the mark \"dumbbell\" reads from, to")
})

test_that("a custom count with `denom` prints n (%) against the slice's N", {
  d <- custom_fixture()
  d <- rbind(d, data.frame(SOC = NA, TERM = NA, USUBJID = "P4", ARM = "B",
                           GR = NA, S = NA, E = NA))
  prep <- summarize_prepare(d, by = "TERM", summaries = list(
    list(type = "custom", name = "Patients", denom = "USUBJID",
         fn = "\\(d) d |> dplyr::filter(!is.na(GR)) |> dplyr::summarise(value = dplyr::n_distinct(USUBJID))")
  ))
  expect_null(prep$err)
  p <- prep$plan[[1]]
  # N = 4 patients, the one without an AE included.
  expect_equal(p$denom, 4)
  c1 <- summarize_cells(prep)$cols[[1]]
  ery <- which(prep$rows$.label == "Erythema")
  expect_match(c1$pct[[ery]], "50%")
})

test_that("a custom count with `denom` keeps N on its header", {
  prep <- summarize_prepare(custom_fixture(), by = "TERM", summaries = list(
    list(type = "custom", name = "Patients", denom = "USUBJID",
         fn = "dplyr::summarise(d, value = dplyr::n_distinct(USUBJID))",
         facet = "ARM")
  ))
  expect_null(prep$err)
  subs <- vapply(prep$plan, function(p) p$sub_label %||% "", character(1))
  expect_true(all(grepl("N = ", subs)))
})
