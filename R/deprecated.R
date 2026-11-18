# Deprecated blocks (blockr.pharma/dev/deprecation.md). The constructor keeps
# working and returns the replacement. Code that calls it gets a warning of
# class `deprecatedWarning`; a board that restores it shows a toast that asks
# the user to save the board. A restore does not warn, and code does not
# toast.

viz_deprecated_warning <- function(old, new) {
  warning(warningCondition(
    paste0(old, "() is deprecated. Use ", new, "."),
    class = c("deprecatedWarning", "lifecycle_warning_deprecated"),
    call = NULL
  ))
}

# One toast per board: every deprecated block of a restore shares the id, so
# a board with five of them shows it once.
viz_deprecated_toast <- function() {
  if (is.null(blockr.core::get_session())) {
    return(invisible())
  }
  blockr.core::notify(
    "This board has a block from an older version. Save the board to keep it.",
    type = "warning", duration = NULL, id = "blockr-viz-deprecated-block",
    glue = FALSE
  )
}
