# aaa- so it collates first: the *_dep() builders call memoise0() when the
# package loads.

#' Run a nullary builder once per process and keep its result
#'
#' For the htmlDependency builders, which would otherwise read the package
#' version and files from disk on every render. A dependency is an immutable
#' list, so one copy serves every session. `exists()` rather than
#' `is.null()`, so a builder that returns NULL is not run again.
#' @noRd
memoise0 <- function(build) {
  force(build)
  cache <- new.env(parent = emptyenv())
  function() {
    if (!exists("v", cache, inherits = FALSE)) cache$v <- build()
    cache$v
  }
}
