# Run the affordance sweep (tests/testthat/apps/table-sweep/sweep.js) against
# a running board and print what it found.
#
#   Rscript dev/sweep-url.R http://localhost:4042 [out.csv]
#
# Every view, every dock tab, every title word, offer and gear control. A row
# with effect DEAD looks clickable and does nothing; GEARS is a block with
# more than one gear.

args <- commandArgs(trailingOnly = TRUE)
url <- if (length(args)) args[[1]] else "http://localhost:4042"
out <- if (length(args) > 1 && nzchar(args[[2]])) args[[2]] else NULL
only <- if (length(args) > 2) jsonlite::toJSON(strsplit(args[[3]], ",")[[1]]) else "null"

here <- normalizePath(file.path(dirname(sub("^--file=", "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), ".."))
js <- paste(readLines(file.path(here, "tests/testthat/apps/table-sweep/sweep.js")),
            collapse = "\n")

b <- chromote::ChromoteSession$new(width = 1400, height = 1000)
on.exit(b$close(), add = TRUE)
b$Page$navigate(url)
Sys.sleep(10)
b$Runtime$evaluate(js)

res <- b$Runtime$evaluate(
  sprintf("blockrSweep.all(%s).then(r => JSON.stringify({results: r, errors: blockrSweep.state.errors}))", only),
  awaitPromise = TRUE, returnByValue = TRUE, timeout_ = 7200
)
val <- jsonlite::fromJSON(res$result$value)
df <- val$results
cat(sprintf("%d controls swept, %d DEAD, %d LIES, %d GEARS, %d skipped, %d disabled\n",
            nrow(df), sum(df$effect == "DEAD"), sum(df$effect == "LIES"), sum(df$effect == "GEARS"),
            sum(df$effect == "skipped"), sum(df$effect == "disabled")))
bad <- df[df$effect %in% c("DEAD", "GEARS", "LIES"), intersect(c("view", "tab", "block", "where", "what", "label", "effect", "note"), names(df))]
print(bad, row.names = FALSE, right = FALSE)
if (length(val$errors)) {
  cat("\nConsole errors:\n")
  cat(paste0("- ", unique(val$errors)), sep = "\n")
}
if (!is.null(out)) utils::write.csv(df, out, row.names = FALSE)
