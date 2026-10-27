# R CMD check --as-cran: no ERROR, no WARNING, and no NOTE that
# tools/allowed-notes.txt does not list.
#
#   Rscript tools/check-rcmdcheck.R [pkg-dir]

pkg_dir <- commandArgs(TRUE)[1L] %||% "."
allowed <- readLines(file.path(pkg_dir, "tools", "allowed-notes.txt"))
allowed <- allowed[nzchar(allowed) & !startsWith(allowed, "#")]

res <- rcmdcheck::rcmdcheck(
  pkg_dir, args = c("--no-manual", "--as-cran"), error_on = "never",
  quiet = TRUE
)
# Every line of the NOTE, after its "checking ..." header, must be allowed;
# one allowed line does not excuse the rest of the NOTE.
ok_note <- vapply(res$notes, function(n) {
  body <- strsplit(n, "\n", fixed = TRUE)[[1L]][-1L]
  body <- body[nzchar(trimws(body))]
  all(vapply(body, function(l) {
    any(vapply(allowed, grepl, logical(1L), x = l, perl = TRUE))
  }, logical(1L)))
}, logical(1L))
notes <- res$notes[!ok_note]

cat(sprintf(
  "R CMD check --as-cran: %d errors, %d warnings, %d notes (%d allowed)\n",
  length(res$errors), length(res$warnings), length(res$notes), sum(ok_note)
))
for (x in c(res$errors, res$warnings, notes)) {
  writeLines(paste0("  ", strsplit(x, "\n")[[1L]]))
}
quit(status = if (length(res$errors) || length(res$warnings) ||
                    length(notes)) 1L else 0L)
