#' Shared UI dependencies from blockr.dplyr
#'
#' Internal helpers that load the `Blockr` JS namespace, select widget,
#' and block stylesheet directly from the `blockr.dplyr` package install.
#' This mirrors the precedent in `blockr.sandbox::tidy_summary_deps()` and
#' avoids copying assets into this package.
#'
#' @return `htmltools::htmlDependency` (singular) / `htmltools::tagList`.
#' @noRd
blockr_core_js_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-core-js",
    version = utils::packageVersion("blockr.dplyr"),
    src = system.file("js", package = "blockr.dplyr"),
    script = "blockr-core.js"
  )
}

blockr_blocks_css_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-blocks-css",
    version = utils::packageVersion("blockr.dplyr"),
    src = system.file("css", package = "blockr.dplyr"),
    stylesheet = "blockr-blocks.css"
  )
}

blockr_select_dep <- function() {
  htmltools::tagList(
    blockr_core_js_dep(),
    htmltools::htmlDependency(
      name = "blockr-select-js",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("js", package = "blockr.dplyr"),
      script = "blockr-select.js"
    ),
    htmltools::htmlDependency(
      name = "blockr-select-css",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("css", package = "blockr.dplyr"),
      stylesheet = "blockr-select.css"
    )
  )
}
