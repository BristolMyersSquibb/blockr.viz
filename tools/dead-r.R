# Dead R code: namespace objects that no root reaches.
#
#   Rscript tools/dead-r.R [pkg-dir]
#
# Roots: the exports, the registered S3 methods (also for other packages'
# generics), .onLoad / .onAttach / .onUnload, every string in R/ that names
# an object (register_blocks("new_chart_block"), do.call("f")), and every
# name used in a top-level expression that is not `name <- function`
# (constants, memoise0(function() ...)). Edges are all.names() over each
# function's body and formals, plus the functions held in a wrapper's
# closure. A local variable named like an object counts as a use, so the
# list can miss dead code but does not report live code.
#
# Prints the unreachable objects with file:line, then those of them that
# tests/ still names. Exits 1 if there are any.

pkg_dir <- normalizePath(commandArgs(TRUE)[1L] %||% ".")
suppressMessages(pkgload::load_all(pkg_dir, quiet = TRUE, export_all = FALSE))
ns <- asNamespace(pkgload::pkg_name(pkg_dir))

internal <- c(".__NAMESPACE__.", ".__S3MethodsTable__.", ".packageName",
              ".__DEVTOOLS__", ".__global__")
nodes <- setdiff(ls(ns, all.names = TRUE), internal)

r_files <- list.files(file.path(pkg_dir, "R"), "\\.[Rr]$", full.names = TRUE)

strings <- unlist(lapply(r_files, function(f) {
  pd <- utils::getParseData(parse(f, keep.source = TRUE))
  gsub("^[\"']|[\"']$", "", pd$text[pd$token == "STR_CONST"])
}))

is_assign <- function(e) {
  is.call(e) && (identical(e[[1L]], as.name("<-")) ||
                   identical(e[[1L]], as.name("=")))
}
top_level <- unlist(lapply(r_files, function(f) {
  unlist(lapply(as.list(parse(f, keep.source = FALSE)), function(e) {
    rhs <- if (is_assign(e)) e[[3L]] else e
    if (is.call(rhs) && identical(rhs[[1L]], as.name("function"))) {
      return(NULL)
    }
    all.names(rhs)
  }))
}))

s3 <- parseNamespaceFile(basename(pkg_dir), dirname(pkg_dir))$S3methods
s3_fun <- ifelse(is.na(s3[, 3L]), paste(s3[, 1L], s3[, 2L], sep = "."),
                 s3[, 3L])

roots <- intersect(
  unique(c(top_level, getNamespaceExports(ns), s3_fun,
           ".onLoad", ".onAttach", ".onUnload", strings)),
  nodes
)

strings_in <- function(e) {
  if (is.character(e)) e else if (is.recursive(e)) {
    unlist(lapply(as.list(e), strings_in))
  }
}
refs <- function(obj) {
  if (is.list(obj) && !is.function(obj)) {
    return(unlist(lapply(Filter(is.function, obj), refs)))
  }
  if (!is.function(obj)) {
    return(character())
  }
  fmls <- Filter(is.language, formals(obj))
  out <- c(all.names(body(obj)), unlist(lapply(fmls, all.names)),
           strings_in(body(obj)), strings_in(fmls))
  env <- environment(obj)
  if (!is.null(env) && !isNamespace(env) && !identical(env, globalenv())) {
    inner <- Filter(is.function, mget(ls(env, all.names = TRUE), envir = env))
    out <- c(out, unlist(lapply(inner, function(f) all.names(body(f)))))
  }
  out
}
edges <- lapply(stats::setNames(nm = nodes),
                function(n) intersect(unique(refs(get(n, ns))), nodes))

seen <- character()
todo <- roots
while (length(todo)) {
  seen <- union(seen, todo)
  todo <- setdiff(unique(unlist(edges[todo])), seen)
}
dead <- sort(setdiff(nodes, seen))

defined_at <- function(n) {
  pat <- paste0("^`?", gsub(".", "\\.", n, fixed = TRUE), "`?\\s*(<-|=)")
  for (f in r_files) {
    hit <- grep(pat, readLines(f, warn = FALSE))
    if (length(hit)) return(paste0("R/", basename(f), ":", hit[1L]))
  }
  "?"
}
where <- vapply(dead, function(n) {
  sr <- attr(get(n, ns), "srcref")
  if (is.null(sr)) {
    defined_at(n)
  } else {
    paste0("R/", basename(attr(sr, "srcfile")$filename), ":", sr[1L])
  }
}, character(1L))

test_files <- list.files(file.path(pkg_dir, "tests"), "\\.[Rr]$",
                         recursive = TRUE, full.names = TRUE)
test_words <- unique(unlist(lapply(test_files, function(f) {
  regmatches(readLines(f, warn = FALSE),
             gregexpr("[A-Za-z.][A-Za-z0-9._]*", readLines(f, warn = FALSE)))
})))
in_tests <- dead[dead %in% test_words]

cat(sprintf("dead R: %d of %d namespace objects unreachable\n",
            length(dead), length(nodes)))
if (length(dead)) writeLines(sprintf("  %-40s %s", dead, where))
cat(sprintf("dead R still used by tests: %d\n", length(in_tests)))
if (length(in_tests)) writeLines(sprintf("  %s", in_tests))
quit(status = if (length(dead)) 1L else 0L)
