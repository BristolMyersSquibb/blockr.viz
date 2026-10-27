# Units over the limit in R/: functions over 80 lines (top level, nested or
# anonymous) and files over 600 lines, counted as `wc -l` counts them.
#
#   Rscript tools/lengths.R [pkg-dir]
#
# A function with `# viz: long <reason>` on the line before is listed apart
# and does not fail. The JS side is checked by eslint (eslint.config.mjs:
# max-lines-per-function, max-lines). Exits 1 if anything is over.

pkg_dir <- commandArgs(TRUE)[1L] %||% "."
fn_max <- 80L
file_max <- 600L

fn_name <- function(pd, expr) {
  asg <- pd[pd$id == expr$parent, ]
  kids <- pd[pd$parent == asg$id, ]
  if (any(kids$token == "EQ_SUB")) {
    return(kids$text[kids$token == "SYMBOL_SUB"][1L])
  }
  if (!any(kids$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN"))) {
    return("<anonymous>")
  }
  lhs <- kids$id[kids$token == "expr"][1L]
  sym <- pd$text[pd$parent == lhs & pd$token %in% c("SYMBOL", "STR_CONST")]
  if (length(sym)) sym[[1L]] else "<anonymous>"
}

fns <- list()
files <- data.frame(file = character(), lines = integer())
for (f in list.files(file.path(pkg_dir, "R"), "\\.[Rr]$", full.names = TRUE)) {
  lines <- readLines(f, warn = FALSE)
  files[nrow(files) + 1L, ] <- list(paste0("R/", basename(f)), length(lines))
  pd <- utils::getParseData(parse(f, keep.source = TRUE))
  for (id in pd$parent[pd$token == "FUNCTION"]) {
    expr <- pd[pd$id == id, ]
    n <- expr$line2 - expr$line1 + 1L
    if (n <= fn_max) next
    marked <- expr$line1 > 1L &&
      grepl("^\\s*# viz: long", lines[expr$line1 - 1L])
    fns[[length(fns) + 1L]] <- data.frame(
      lines = n, fn = fn_name(pd, expr),
      at = paste0("R/", basename(f), ":", expr$line1), marked = marked
    )
  }
}
fns <- do.call(rbind, fns) %||%
  data.frame(lines = integer(), fn = character(), at = character(),
             marked = logical())
fns <- fns[order(-fns$lines), ]
over <- fns[!fns$marked, ]
big <- files[files$lines > file_max, ]
big <- big[order(-big$lines), ]

cat(sprintf("R functions over %d lines: %d\n", fn_max, nrow(over)))
if (nrow(over)) {
  writeLines(sprintf("  %5d  %-32s %s", over$lines, over$fn, over$at))
}
if (any(fns$marked)) {
  cat("marked `# viz: long`, not counted:\n")
  m <- fns[fns$marked, ]
  writeLines(sprintf("  %5d  %-32s %s", m$lines, m$fn, m$at))
}
cat(sprintf("R files over %d lines: %d\n", file_max, nrow(big)))
if (nrow(big)) writeLines(sprintf("  %5d  %s", big$lines, big$file))
quit(status = if (nrow(over) || nrow(big)) 1L else 0L)
