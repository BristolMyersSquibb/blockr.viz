# A groups table's row-label header shows the `by` name and, under it, the
# column's variable label in the quiet sub-line, as 0.2.200 did. With a
# "(Missing)" group the label was lost (test-table-missing-label.R); in a
# nested table it came back in the bold of a Table 1 arm's Big N.
#
# Skipped without a headless browser and under R CMD check. On the Mac,
# Chrome cannot start in Claude's sandbox: start
# blockr.dev/scripts/chrome-remote.sh outside it and source
# blockr.dev/scripts/use-remote-chrome.R first.

test_that("a groups stub header shows the variable label under the name", {
  skip_without_browser()
  skip_if_not_installed("pkgload")
  skip_if_not_installed("blockr.dock")
  Sys.setenv(NOT_CRAN = "true")
  withr::local_envvar(BLOCKR_VIZ_SRC = normalizePath(test_path("..", "..")))
  configure_chromote()
  app <- shinytest2::AppDriver$new(
    test_path("apps", "table-groups-stub-label"),
    name = "table-groups-stub-label",
    width = 1000, height = 800,
    load_timeout = 120 * 1000, timeout = 30 * 1000
  )
  withr::defer(app$stop())

  # The stub header named `name` once its panel shows: its two lines and how
  # they are drawn.
  stub <- function(panel, name) {
    app$run_js(sprintf(
      "const tab = [...document.querySelectorAll('.dv-tab')].find(
         (t) => t.innerText.trim() === '%s');
       tab.dispatchEvent(new PointerEvent('pointerdown', { bubbles: true }));
       tab.click();", panel
    ))
    find <- sprintf(
      "[...document.querySelectorAll('th.vz-table-stub-header')].find((th) =>
         th.offsetParent && th.querySelector('.blockr-col-label') &&
         th.querySelector('.blockr-col-name').innerText === '%s')", name
    )
    app$wait_for_js(paste0("!!", find))
    app$get_js(paste0("(() => {
      const th = ", find, ";
      const css = (q) => {
        const e = th.querySelector(q);
        const s = getComputedStyle(e);
        return { text: e.innerText, weight: Number(s.fontWeight),
                 size: parseFloat(s.fontSize) };
      };
      return { name: css('.blockr-col-name'), sub: css('.blockr-col-label') };
    })()"))
  }

  want <- list(
    Flat = c("CMDECOD", "Standardized Medication Name"),
    Nested = c("CMCLAS / CMDECOD",
               "Medication Class / Standardized Medication Name")
  )
  for (panel in names(want)) {
    h <- stub(panel, want[[panel]][[1L]])
    expect_identical(h$sub$text, want[[panel]][[2L]])
    # The label line is the quieter one: no heavier and no larger.
    expect_lte(h$sub$weight, h$name$weight)
    expect_lt(h$sub$size, h$name$size)
  }
})
