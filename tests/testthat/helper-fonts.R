# The exhibit table measures every column through systemfonts at the deck's
# own typeface (blockr.viz.ft_font, "Inter" unset). Where that face is not
# installed systemfonts substitutes a different one, and the numbers become
# approximate -- which ft_measured_widths() says of itself, and which
# blockr.viz.ft_width_slack exists to absorb.
#
# A test asserting a LAYOUT OUTCOME -- this stub wraps, this table fits on one
# slide -- is therefore asserting the metrics of one particular face. It is a
# real assertion where that face is present and a coin flip where it is not:
# the GitHub runners ship no Inter, and the same table there is dealt over two
# sets of slides. Those tests skip rather than encode whichever font the
# machine happened to substitute.
#
# Tests that assert the MECHANISM instead (a smaller share yields a narrower
# stub, a cap re-deals width to the data columns) hold under any face and need
# no skip.
skip_if_font_substituted <- function(family = getOption("blockr.viz.ft_font",
                                                        "Inter")) {
  testthat::skip_if_not_installed("systemfonts")
  m <- systemfonts::match_fonts(family)
  got <- systemfonts::font_info(path = m$path, index = m$index)$family
  if (!identical(tolower(got[[1L]]), tolower(family))) {
    testthat::skip(paste0("font '", family, "' is not installed (systemfonts ",
                          "substitutes '", got[[1L]], "')"))
  }
  invisible(TRUE)
}
