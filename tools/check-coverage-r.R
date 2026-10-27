# R line coverage of the package tests, at least 90%.
#
#   Rscript tools/check-coverage-r.R [pkg-dir] [min]
#
# Prints the total and the ten least covered files. Code that runs at load
# (.onLoad, memoised builders) shows as uncovered: covr cannot see it.

args <- commandArgs(TRUE)
pkg_dir <- args[1L] %||% "."
min_pct <- as.numeric(args[2L] %||% 90)

cov <- covr::package_coverage(pkg_dir, type = "tests", quiet = TRUE)
pct <- covr::percent_coverage(cov)
df <- covr::tally_coverage(cov, by = "line")
by_file <- vapply(split(df$value > 0, df$filename), mean, numeric(1L))
n_file <- vapply(split(df$value, df$filename), length, integer(1L))
low <- order(by_file)[seq_len(min(10L, length(by_file)))]

cat(sprintf("R coverage: %.1f%% of %d lines (minimum %g%%)\n", pct,
            nrow(df), min_pct))
cat("  least covered:\n")
writeLines(sprintf("    %5.1f%% of %5d  %s", 100 * by_file[low], n_file[low],
                   names(by_file)[low]))
quit(status = if (pct < min_pct) 1L else 0L)
