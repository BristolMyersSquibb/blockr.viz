# Size budget (design.md section 10): lines per part of the package, as
# `wc -l` counts them.
#
#   Rscript tools/check-budget.R [pkg-dir]
#
# tools/budget.csv maps globs to parts (`part, glob, max`); a part may have
# several globs, all with the same max. Globs are relative to the package
# root; `*` stays within a directory, `**` crosses directories. The part
# "not counted" holds vendored and generated files.
#
# Fails when a part is over its budget, when a file under R/, inst/js/ or
# inst/css/ matches no part, or when it matches more than one. Changing a
# budget is a reviewed edit of the csv.

pkg_dir <- commandArgs(TRUE)[1L] %||% "."
budget <- utils::read.csv(file.path(pkg_dir, "tools", "budget.csv"),
                          stringsAsFactors = FALSE, strip.white = TRUE)

glob_rx <- function(g) {
  g <- gsub("([.+^$(){}|\\[\\]\\\\])", "\\\\\\1", g)
  g <- gsub("**", "\u0001", g, fixed = TRUE)
  g <- gsub("*", "[^/]*", g, fixed = TRUE)
  g <- gsub("?", "[^/]", g, fixed = TRUE)
  paste0("^", gsub("\u0001", ".*", g, fixed = TRUE), "$")
}

files <- c(
  file.path("R", list.files(file.path(pkg_dir, "R"), "\\.[Rr]$")),
  file.path("inst/js", list.files(file.path(pkg_dir, "inst/js"),
                                  "\\.(js|d\\.ts)$", recursive = TRUE)),
  file.path("inst/css", list.files(file.path(pkg_dir, "inst/css"), "\\.css$",
                                   recursive = TRUE))
)
lines <- vapply(file.path(pkg_dir, files),
                function(f) length(readLines(f, warn = FALSE)), integer(1L))

hits <- lapply(files, function(f) {
  unique(budget$part[vapply(budget$glob,
                            function(g) grepl(glob_rx(g), f), logical(1L))])
})
part_of <- vapply(hits, function(h) if (length(h) == 1L) h else NA_character_,
                  character(1L))
unmatched <- files[lengths(hits) == 0L]
twice <- files[lengths(hits) > 1L]

limits <- tapply(budget$max, budget$part, function(m) {
  if (length(unique(m)) != 1L) stop("one part, several budgets: ", m)
  m[[1L]]
})
counted <- names(limits) != "not counted"
used <- tapply(lines, factor(part_of, levels = names(limits)), sum)
used[is.na(used)] <- 0L
tab <- data.frame(part = names(limits), lines = as.integer(used),
                  budget = as.integer(limits))[counted, ]
tab$over <- pmax(tab$lines - tab$budget, 0L)
lang <- sub(":.*", "", tab$part)
totals <- data.frame(
  part = paste(unique(lang), "total"),
  lines = as.integer(tapply(tab$lines, lang, sum)[unique(lang)]),
  budget = as.integer(tapply(tab$budget, lang, sum)[unique(lang)])
)
totals$over <- pmax(totals$lines - totals$budget, 0L)

fmt <- function(d) {
  sprintf("  %-40s %7d %7d %s", d$part, d$lines, d$budget,
          ifelse(d$over > 0L, paste0("OVER by ", d$over), "ok"))
}
cat(sprintf("size budget: %d of %d parts over\n", sum(tab$over > 0L),
            nrow(tab)))
cat(sprintf("  %-40s %7s %7s\n", "part", "lines", "budget"))
writeLines(fmt(tab))
writeLines(fmt(totals))
if (length(unmatched)) {
  cat("files in no part:\n")
  writeLines(paste0("  ", unmatched))
}
if (length(twice)) {
  cat("files in more than one part:\n")
  writeLines(paste0("  ", twice))
}
quit(status = if (any(tab$over > 0L) || length(unmatched) ||
                    length(twice)) 1L else 0L)
