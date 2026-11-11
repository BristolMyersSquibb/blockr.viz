# aaa- prefix so it collates first: the *_dep() builders call memoise0() at
# package-load time, so it must already be defined.

# Runs a nullary builder at most once per process and caches the result in a
# private environment. `exists()` rather than `is.null()` caches a `NULL`
# result too. Used for the htmlDependency builders: they take no
# data-dependent arguments, return the same immutable object for the life of
# the process, and would otherwise repeat their disk I/O on every render. One
# copy is shared across Shiny sessions.
#' @noRd
memoise0 <- function(build) {
  force(build)
  cache <- new.env(parent = emptyenv())
  function() {
    if (!exists("v", cache, inherits = FALSE)) cache$v <- build()
    cache$v
  }
}

# TRUE when the dependencies list the single source files instead of the
# bundles: `options(blockr.viz.dev_js = TRUE)`, for working on the JS without
# rebuilding.
viz_dev_js <- function() isTRUE(getOption("blockr.viz.dev_js"))

# memoise0() for a dependency builder `build(dev)` with a bundled and a
# source variant, each built once; the option picks one on every call.
memoise_js <- function(build) {
  bundled <- memoise0(function() build(FALSE))
  sources <- memoise0(function() build(TRUE))
  function() if (viz_dev_js()) sources() else bundled()
}
