# Comments that tell history instead of what the code does: "used to",
# "no longer", "was", "before", "fixed", an issue number, a date. A report
# for the final audit; it never fails.
#
#   Rscript tools/check-comments.R [pkg-dir] [--all]
#
# R comments come from the parser, JS and CSS comments from a scan that
# skips strings. Prints counts per word and per file, and the first lines
# (all of them with --all).

args <- commandArgs(TRUE)
pkg_dir <- setdiff(args, "--all")[1L] %||% "."
show_all <- "--all" %in% args

words <- c(
  "used to" = "\\bused to\\b", "no longer" = "\\bno longer\\b",
  "was" = "\\bwas\\b", "before" = "\\bbefore\\b", "fixed" = "\\bfixed\\b",
  "#issue" = "(^|[^&\\w])#[0-9]+\\b",
  "date" = "\\b20[0-9]{2}-[0-9]{2}(-[0-9]{2})?\\b"
)

r_comments <- function(f) {
  pd <- utils::getParseData(parse(f, keep.source = TRUE))
  pd <- pd[pd$token == "COMMENT", ]
  data.frame(line = pd$line1, text = pd$text)
}

# Line comments and block comments, skipping string and template literals.
c_comments <- function(f, line_comments = TRUE) {
  src <- paste(readLines(f, warn = FALSE), collapse = "\n")
  ch <- strsplit(src, "")[[1L]]
  n <- length(ch)
  out <- list()
  i <- 1L
  line <- 1L
  while (i <= n) {
    c1 <- ch[i]
    c2 <- if (i < n) ch[i + 1L] else ""
    if (c1 %in% c("\"", "'", "`")) {
      j <- i + 1L
      while (j <= n && ch[j] != c1) {
        if (ch[j] == "\\") j <- j + 1L
        if (j <= n && ch[j] == "\n") line <- line + 1L
        j <- j + 1L
      }
      i <- j + 1L
      next
    }
    if (c1 == "/" && c2 == "*") {
      j <- i + 2L
      start <- line
      while (j < n && !(ch[j] == "*" && ch[j + 1L] == "/")) {
        if (ch[j] == "\n") line <- line + 1L
        j <- j + 1L
      }
      out[[length(out) + 1L]] <- data.frame(
        line = start, text = paste(ch[i:min(j + 1L, n)], collapse = "")
      )
      i <- j + 2L
      next
    }
    if (line_comments && c1 == "/" && c2 == "/") {
      j <- i
      while (j <= n && ch[j] != "\n") j <- j + 1L
      out[[length(out) + 1L]] <- data.frame(
        line = line, text = paste(ch[i:(j - 1L)], collapse = "")
      )
      i <- j
      next
    }
    if (c1 == "\n") line <- line + 1L
    i <- i + 1L
  }
  do.call(rbind, out) %||% data.frame(line = integer(), text = character())
}

files <- c(
  list.files(file.path(pkg_dir, "R"), "\\.[Rr]$", full.names = TRUE),
  list.files(file.path(pkg_dir, "inst/js"), "\\.js$", recursive = TRUE,
             full.names = TRUE),
  list.files(file.path(pkg_dir, "inst/css"), "\\.css$", recursive = TRUE,
             full.names = TRUE)
)
files <- files[!grepl("snapdom\\.js$|/spec/", files)]

hits <- do.call(rbind, lapply(files, function(f) {
  cm <- if (grepl("\\.[Rr]$", f)) r_comments(f) else
    c_comments(f, line_comments = !grepl("\\.css$", f))
  if (!nrow(cm)) return(NULL)
  do.call(rbind, lapply(names(words), function(w) {
    m <- grepl(words[[w]], cm$text, ignore.case = TRUE, perl = TRUE)
    if (!any(m)) return(NULL)
    data.frame(word = w, file = sub(paste0("^", pkg_dir, "/?"), "", f),
               line = cm$line[m], text = trimws(cm$text[m]))
  }))
}))

if (is.null(hits) || !nrow(hits)) {
  cat("history comments: none\n")
  quit(status = 0L)
}
lines <- unique(hits[c("file", "line")])
cat(sprintf("history comments: %d comment lines in %d files (report only)\n",
            nrow(lines), length(unique(hits$file))))
cat("  by word:\n")
by_word <- sort(table(hits$word), decreasing = TRUE)
writeLines(sprintf("    %-10s %5d", names(by_word), as.integer(by_word)))
cat("  by file (top 15):\n")
by_file <- sort(table(lines$file), decreasing = TRUE)
writeLines(utils::head(sprintf("    %5d  %s", as.integer(by_file),
                               names(by_file)), 15L))
show <- hits[!duplicated(hits[c("file", "line")]), ]
show <- if (show_all) show else utils::head(show, 25L)
cat(if (show_all) "  all:\n" else "  first 25:\n")
writeLines(sprintf("    %s:%d  %s", show$file, show$line,
                   substr(show$text, 1L, 100L)))
quit(status = 0L)
