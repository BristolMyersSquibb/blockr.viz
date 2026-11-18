# Tests that build a deprecated block on purpose: drop its deprecation
# warning, keep any other.
quiet_deprecated <- function(expr) {
  withCallingHandlers(
    expr,
    deprecatedWarning = function(w) invokeRestart("muffleWarning")
  )
}
