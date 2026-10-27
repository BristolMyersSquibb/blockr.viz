# Every feature in the catalogues is linked to a test that passed.
#
#   Rscript tools/check-features.R --results r.xml js.xml -- features/*.md
#
# A feature is a body row of a markdown table in a catalogue
# (tools/features/features-chart.md, features-table.md). Each row carries
# one or more links:
#
#   [test: test-chart-record.R :: chart recording: bar_count]
#   [test: tests/js/legend.test.js :: legend = "none" draws no band]
#
# The results are JUnit files: testthat's JunitReporter (one suite per test
# file, test names with every run of non-alphanumerics turned into "_") and
# node's `--test-reporter=junit` (a `file` attribute per test). A link
# passes when its test is in the results and none of its runs failed,
# errored or skipped.
#
# Fails on a row without a link, a link to a test that is not in the
# results, and a link to a test that did not pass.

args <- commandArgs(TRUE)
sep <- match("--", args)
stopifnot(identical(args[1L], "--results"), !is.na(sep))
result_files <- args[seq(2L, sep - 1L)]
feature_files <- args[-seq_len(sep)]

mangle <- function(x) gsub("[^A-Za-z0-9]+", "_", x)

# node escapes the test name twice (`&amp;quot;`); undo the second time.
unescape <- function(x) {
  for (e in list(c("&quot;", "\""), c("&apos;", "'"), c("&lt;", "<"),
                 c("&gt;", ">"), c("&amp;", "&"))) {
    x <- gsub(e[[1L]], e[[2L]], x, fixed = TRUE)
  }
  x
}

read_results <- function(path) {
  doc <- xml2::read_xml(path)
  cases <- xml2::xml_find_all(doc, "//testcase")
  suite <- vapply(cases, function(x) {
    s <- xml2::xml_find_first(x, "ancestor::testsuite")
    if (inherits(s, "xml_missing")) "" else xml2::xml_attr(s, "name")
  }, character(1L))
  data.frame(
    suite = suite,
    file = xml2::xml_attr(cases, "file") %|NA|% "",
    name = unescape(xml2::xml_attr(cases, "name")),
    ok = vapply(cases, function(x) {
      length(xml2::xml_find_all(x, "failure|error|skipped")) == 0L
    }, logical(1L))
  )
}
`%|NA|%` <- function(x, y) ifelse(is.na(x), y, x)
res <- do.call(rbind, lapply(result_files, read_results))

# The runs of one linked test: by suite and mangled name for testthat, by
# file and exact name for node.
runs_of <- function(file, name) {
  if (grepl("\\.[Rr]$", file)) {
    suite <- sub("^test-", "", sub("\\.[Rr]$", "", basename(file)))
    res$ok[res$suite == suite & res$name == mangle(name)]
  } else {
    hit <- endsWith(res$file, file) | endsWith(res$file, paste0("/", file))
    res$ok[hit & res$name == name]
  }
}

link_rx <- "\\[test:\\s*([^]]+?)\\s*::\\s*([^]]+?)\\s*\\]"
problems <- 0L
for (f in feature_files) {
  lines <- readLines(f, warn = FALSE)
  is_row <- grepl("^\\s*\\|", lines)
  is_rule <- grepl("^\\s*\\|[-:| ]+\\|\\s*$", lines)
  is_head <- is_row & c(is_rule[-1L], FALSE)
  rows <- which(is_row & !is_rule & !is_head)
  unlinked <- broken <- failing <- character()
  linked <- 0L
  for (i in rows) {
    m <- regmatches(lines[i], gregexpr(link_rx, lines[i], perl = TRUE))[[1L]]
    where <- paste0(basename(f), ":", i)
    if (!length(m)) {
      unlinked <- c(unlinked, where)
      next
    }
    linked <- linked + 1L
    for (l in m) {
      p <- regmatches(l, regexec(link_rx, l, perl = TRUE))[[1L]]
      ok <- runs_of(p[2L], p[3L])
      if (!length(ok)) {
        broken <- c(broken, paste(where, l))
      } else if (!all(ok)) {
        failing <- c(failing, paste(where, l))
      }
    }
  }
  cat(sprintf(
    "features %s: %d rows, %d linked, %d without a link, %d links to no test, %d links to a failing or skipped test\n",
    basename(f), length(rows), linked, length(unlinked), length(broken),
    length(failing)
  ))
  show <- function(x, what) {
    if (!length(x)) return(invisible())
    cat("  ", what, ":\n", sep = "")
    writeLines(paste0("    ", utils::head(x, 20L)))
    if (length(x) > 20L) cat(sprintf("    ... and %d more\n", length(x) - 20L))
  }
  show(unlinked, "rows without a link")
  show(broken, "links to no test")
  show(failing, "links to a failing or skipped test")
  problems <- problems + length(unlinked) + length(broken) + length(failing)
}
quit(status = if (problems) 1L else 0L)
