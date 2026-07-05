# fmt_cells(): golden equivalence of the grouped/vectorized interpolation
# against the original per-row regmatches()+gsub() loop. The vectorized
# rewrite (group rows by distinct template, parse once, paste0 column-wise)
# must be output-IDENTICAL; this reference copy of the old loop is the oracle.

ref_fmt_cells <- function(df, fmt_col = ".fmt", digits = 2L, na = "NA") {
  stopifnot(is.data.frame(df))
  if (!fmt_col %in% names(df)) {
    return(rep(NA_character_, nrow(df)))
  }
  tmpl <- as.character(df[[fmt_col]])
  row_digits <- if (".digits" %in% names(df)) {
    suppressWarnings(as.integer(df[[".digits"]]))
  } else {
    rep(NA_integer_, nrow(df))
  }
  out <- rep(NA_character_, nrow(df))
  for (i in seq_len(nrow(df))) {
    t <- tmpl[i]
    if (is.na(t) || !nzchar(t)) next
    d <- if (is.na(row_digits[i])) digits else row_digits[i]
    toks <- regmatches(t, gregexpr("\\{[^{}]+\\}", t))[[1]]
    cell <- t
    for (tok in unique(toks)) {
      spec <- sub("^\\{(.*)\\}$", "\\1", tok)
      tok_digits <- d
      if (grepl(":", spec, fixed = TRUE)) {
        parts <- strsplit(spec, ":", fixed = TRUE)[[1]]
        spec <- parts[1]
        pd <- suppressWarnings(as.integer(parts[2]))
        if (!is.na(pd)) tok_digits <- pd
      }
      col <- spec
      val <- if (col %in% names(df)) df[[col]][i] else NA
      sval <- if (length(val) != 1L || is.na(val)) na else fmt_num(val, tok_digits)
      cell <- gsub(tok, sval, cell, fixed = TRUE)
    }
    out[i] <- cell
  }
  out
}

expect_golden <- function(df, ...) {
  expect_identical(fmt_cells(df, ...), ref_fmt_cells(df, ...))
}

test_that("fmt_cells() matches the reference loop on mixed templates", {
  set.seed(42)
  n <- 400
  df <- data.frame(
    n    = sample(c(1:100, NA), n, replace = TRUE),
    pct  = c(runif(n - 1) * 100, NA),
    mean = rnorm(n),
    sd   = abs(rnorm(n)),
    .fmt = sample(
      c("{n} ({pct}%)",              # multi-token
        "{mean:1} ({sd:2})",         # per-token precision
        "{n}",                       # single token
        "{n} of {n}",                # repeated token
        "{missing} x",               # token naming a missing column
        "plain text",                # token-free template
        "",                          # empty template -> NA
        NA),                         # NA template -> NA
      n, replace = TRUE
    ),
    stringsAsFactors = FALSE
  )
  expect_golden(df)
  expect_golden(df, digits = 0L)
  expect_golden(df, digits = 4L)
  expect_golden(df, na = "n/a")
})

test_that("fmt_cells() matches the reference loop with a .digits column", {
  set.seed(1)
  n <- 300
  df <- data.frame(
    mean = rnorm(n),
    sd   = abs(rnorm(n)),
    .digits = sample(c(NA, 0L, 1L, 3L), n, replace = TRUE),
    .fmt = sample(c("{mean} ({sd})", "{mean:1} ({sd:2})", "{sd}"),
                  n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  expect_golden(df)
})

test_that("fmt_cells() matches the reference loop on character/factor columns", {
  df <- data.frame(
    lab = factor(c("a", "b", NA, "a")),
    txt = c("x", NA, "y", "z"),
    v   = c(1.234, NA, 5.678, 9),
    .fmt = c("{lab}: {v}", "{txt} {v}", "{lab}/{txt}", "{v} ({v:0})"),
    stringsAsFactors = FALSE
  )
  expect_golden(df)
})

test_that("fmt_cells() matches the reference loop on summary_table long frames", {
  # The collision fixtures from test-summary-table-collision.R: shared level
  # labels across vars, a grouped spread, and the multi-stat mtcars example.
  d <- data.frame(
    sex  = c("F", "M", "OTHER", "OTHER"),
    race = c("WHITE", "ASIAN", "OTHER", "OTHER"),
    grp  = c("A", "B", "A", "B"),
    stringsAsFactors = FALSE
  )
  expect_golden(summary_table_long(d, vars = c("sex", "race")))
  expect_golden(summary_table_long(d, vars = c("sex", "race"), by = "grp"))
  expect_golden(summary_table_long(
    mtcars, vars = c("mpg", "hp"), by = "cyl",
    stats = c("n_pct", "median_q1_q3", "min_max"), add_overall = TRUE
  ))
})

test_that("fmt_cells() edge shapes: no .fmt column, zero rows", {
  df0 <- data.frame(x = numeric(0), .fmt = character(0))
  expect_identical(fmt_cells(df0), character(0))
  df_no <- data.frame(x = 1:3)
  expect_identical(fmt_cells(df_no), rep(NA_character_, 3))
})
