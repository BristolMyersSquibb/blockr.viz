# A chart configuration the data cannot honour is reported in the GEAR, and the
# chart area keeps a quiet empty state (issue #24). Three things have to hold
# together for that to be an improvement rather than a hidden failure:
#
#   * the chart area says only that there is nothing to draw, and offers the
#     way in -- no diagnosis, no column list, no wall of text;
#   * the settings panel carries the whole diagnosis, above the pickers that
#     fix it, and marks the offending row;
#   * the gear grows an alert badge, because with the panel shut that badge is
#     the ONLY thing saying there is something to fix.
#
# The widget is driven the way Shiny drives it -- binding.initialize() then
# setData() -- inside a headless chromium, against one self-contained page that
# inlines the SHIPPED js/css and stubs only the browser globals chart.js loads
# into (Shiny, jQuery, echarts, the Blockr icon set). Nothing here
# reimplements the widget.
#
# Skipped where no headless browser exists, and under R CMD check (launching
# chromium there is flaky and leaves temp detritus -> a check NOTE). Runs under
# devtools::test() / CI, which is where the JS is meant to be exercised.

viz_asset <- function(dir, files) {
  vapply(files, function(f) system.file(dir, f, package = "blockr.viz"),
         character(1), USE.NAMES = FALSE)
}

PROBE_JS <- c("settings-band.js", "drilldown-agg.js", "drilldown-config.js",
              "drilldown-theme-register.js", "chart.js")
PROBE_CSS <- c("settings-band.css", "viz-block.css", "chart.css")

# The globals chart.js loads into. Deliberately thin: enough for the binding to
# register and for a render to be attempted, nothing that could stand in for
# the code under test.
PROBE_STUBS <- "
window.Blockr = { icons: { gear: '<svg width=\"14\" height=\"14\"></svg>' } };
window.$ = window.jQuery = function () {
  return { find: function () { return []; } };
};
var __ec = function () {
  return new Proxy({}, { get: function (t, k) {
    return function () {
      if (k === 'getZr') return __ec();
      return (k === 'getWidth' || k === 'getHeight') ? 400 : undefined;
    };
  } });
};
window.echarts = {
  init: __ec, getInstanceByDom: function () { return undefined; },
  registerTheme: function () {}, connect: function () {}
};
window.Shiny = {
  InputBinding: function () {},
  inputBindings: { register: function (b) { window.__binding = b; } },
  addCustomMessageHandler: function () {},
  setInputValue: function () {}
};
"

# One page holding the real assets. NA when the package's inst/ is not readable
# (an install without the JS), which the tests skip on.
chart_probe_page <- function() {
  js <- viz_asset("js", PROBE_JS)
  css <- viz_asset("css", PROBE_CSS)
  if (any(!nzchar(c(js, css))) || any(!file.exists(c(js, css)))) {
    return(NA_character_)
  }
  slurp <- function(p) paste(readLines(p, warn = FALSE), collapse = "\n")
  f <- tempfile(fileext = ".html")
  writeLines(c(
    "<!doctype html><meta charset='utf-8'>",
    paste0("<style>", vapply(css, slurp, character(1)), "</style>"),
    "<body><div id='chart' class='drilldown-chart-container' style='width:600px;height:320px'></div>",
    paste0("<script>", PROBE_STUBS, "</script>"),
    paste0("<script>", vapply(js, slurp, character(1)), "</script>")
  ), f)
  f
}

probe_cols <- list(
  list(name = "species", type = "categorical", n_unique = 3L),
  list(name = "island", type = "categorical", n_unique = 3L),
  list(name = "body_mass_g", type = "numeric")
)

probe_rows <- list(
  list(species = "Adelie", island = "Torgersen", body_mass_g = 3750),
  list(species = "Gentoo", island = "Biscoe", body_mass_g = 5000),
  list(species = "Chinstrap", island = "Dream", body_mass_g = 3800)
)

# Mount the widget on `config`, optionally re-mount on `then` (a config the
# user fixed in the gear), and read the DOM back. `click_action` presses the
# chart area's own way in first.
chart_probe <- function(config, then = NULL, columns = probe_cols,
                        rows = probe_rows, click_action = FALSE) {
  page <- chart_probe_page()
  testthat::skip_if(is.na(page), "shipped chart js/css not found")

  sess <- chromote::ChromoteSession$new()
  on.exit(try(sess$close(), silent = TRUE), add = TRUE)
  sess$Page$navigate(paste0("file://", page), wait_ = TRUE)

  ev <- function(js) {
    out <- sess$Runtime$evaluate(js, returnByValue = TRUE, awaitPromise = TRUE)
    if (!is.null(out$exceptionDetails)) {
      why <- out$exceptionDetails$exception$description
      stop("probe threw: ", if (is.null(why)) out$exceptionDetails$text else why)
    }
    out$result$value
  }

  # The inline scripts run before the load event, but poll rather than assume.
  for (i in seq_len(100)) {
    if (identical(ev("typeof window.__binding"), "object")) break
    Sys.sleep(0.05)
  }
  testthat::skip_if(!identical(ev("typeof window.__binding"), "object"),
                    "chart.js did not register its Shiny binding")

  json <- function(x) {
    as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
  }
  mount <- function(cfg) {
    ev(sprintf("(function () {
      var el = document.getElementById('chart');
      if (!el._block) window.__binding.initialize(el);
      el._block.setData(%s, %s, %s);
      return 'ok';
    })()", json(columns), json(rows), json(cfg)))
  }

  mount(config)
  if (isTRUE(click_action)) {
    ev("document.querySelector('.vd-empty-action').click(); 'ok'")
  }
  if (!is.null(then)) mount(then)

  jsonlite::fromJSON(ev("(function () {
    var q = function (s) { return document.querySelector(s); };
    var txt = function (s) { return q(s) ? q(s).textContent : null; };
    var gear = q('.blockr-gear-btn');
    var notices = q('.dd-notices');
    var stale = q('.dd-role-stale-col');
    return JSON.stringify({
      area: (txt('.dd-chart-grid') || '').replace(/\\s+/g, ' ').trim(),
      action: txt('.vd-empty-action'),
      gear_alert: !!(gear && gear.classList.contains('blockr-gear-btn--alert')),
      gear_label: gear ? gear.getAttribute('aria-label') : null,
      gear_title: gear ? gear.title : null,
      badge_shown: !!q('.blockr-gear-badge') &&
        getComputedStyle(q('.blockr-gear-badge')).display !== 'none',
      notices_shown: !!notices && !notices.hidden &&
        getComputedStyle(notices).display !== 'none',
      n_notices: document.querySelectorAll('.dd-notice').length,
      tone: q('.dd-notice') ? q('.dd-notice').className : null,
      live: q('.dd-notice') ? q('.dd-notice').getAttribute('role') : null,
      text: txt('.dd-notice-text'),
      detail: txt('.dd-notice-detail'),
      stale_role: stale ? stale.className : null,
      stale_options: stale
        ? Array.prototype.map.call(stale.querySelectorAll('option'),
                                   function (o) { return o.textContent; })
        : null,
      panel_open: !!(q('.blockr-settings') &&
        q('.blockr-settings').classList.contains('blockr-settings--open'))
    });
  })()"), simplifyVector = TRUE)
}

skip_if_no_chrome <- function() {
  testthat::skip_on_cran()
  testthat::skip_if(nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")),
                    "browser probe not run under R CMD check")
  testthat::skip_if(!requireNamespace("chromote", quietly = TRUE),
                    "chromote not installed")
  testthat::skip_if(!chromote_works(), "no headless chromium")
}

# A bar chart whose Group column was renamed away upstream: the everyday case
# from issue #24 (a flatten or pivot changed the name).
broken_bar <- list(chart_type = "bar", group = "not_a_column",
                   value = ".count", func = "count")

test_that("a missing mapped column leaves the chart area quiet", {
  skip_if_no_chrome()
  p <- chart_probe(broken_bar)

  # The whole point of the change: none of the diagnosis is painted here.
  expect_false(grepl("Mapped column not in data", p$area, fixed = TRUE))
  expect_false(grepl("not_a_column", p$area, fixed = TRUE))
  expect_false(grepl("re-pick", p$area, fixed = TRUE))
  expect_false(grepl("Columns available here", p$area, fixed = TRUE))
  # What is left is a statement and one way in.
  expect_match(p$area, "Nothing to plot")
  expect_equal(p$action, "Open chart settings")
})

test_that("the gear carries the diagnosis, above the picker that fixes it", {
  skip_if_no_chrome()
  p <- chart_probe(broken_bar)

  expect_true(p$notices_shown)
  expect_equal(p$n_notices, 1L)
  expect_equal(p$text, 'Mapped column not in data: Group = "not_a_column".')
  # Names the cause, says where the fix is, and lists what there is instead --
  # the three things the message the issue called "the good one" carried.
  expect_match(p$detail, "rename, flatten or pivot upstream")
  expect_match(p$detail, "re-pick it below", fixed = TRUE)
  expect_match(p$detail, "species, island, body_mass_g", fixed = TRUE)
  expect_match(p$tone, "dd-notice--error", fixed = TRUE)
  # Polite: the panel is opened BY the user, so a re-stated problem must not
  # interrupt a screen reader mid-sentence.
  expect_equal(p$live, "status")
  # The row the notice is about, marked and still showing what it points at.
  expect_match(p$stale_role, "dd-role-group", fixed = TRUE)
  expect_true(any(grepl("not_a_column (not in data)", p$stale_options,
                        fixed = TRUE)))
})

test_that("the gear badges the problem, in more than colour", {
  skip_if_no_chrome()
  p <- chart_probe(broken_bar)

  expect_true(p$gear_alert)
  expect_true(p$badge_shown)
  # With the panel shut the badge is the only signal, so it has to be readable
  # by something other than the eye: the count in the accessible name, the
  # diagnosis in the tooltip.
  expect_equal(p$gear_label, "Chart settings \u2014 1 problem")
  expect_match(p$gear_title, "Mapped column not in data", fixed = TRUE)
})

test_that("the chart area's own way in opens the settings", {
  skip_if_no_chrome()
  p <- chart_probe(broken_bar, click_action = TRUE)
  expect_true(p$panel_open)
})

test_that("re-picking a real column clears the notice and the badge", {
  skip_if_no_chrome()
  p <- chart_probe(broken_bar, then = list(
    chart_type = "bar", group = "species", value = ".count", func = "count"
  ))

  expect_false(p$gear_alert)
  expect_false(p$badge_shown)
  expect_false(p$notices_shown)
  expect_equal(p$n_notices, 0L)
  expect_equal(p$gear_label, "Chart settings")
  expect_null(p$stale_role)
  # Nothing left of the empty state either: the chart drew.
  expect_false(grepl("Nothing to plot", p$area, fixed = TRUE))
})

test_that("too many colour levels is reported the same way", {
  skip_if_no_chrome()
  # 20 distinct levels, past the 15 the legend can carry.
  many <- lapply(seq_len(20), function(i) {
    list(species = paste0("S", i), island = paste0("I", i),
         body_mass_g = 3000 + i)
  })
  p <- chart_probe(
    list(chart_type = "scatter", x = "body_mass_g", y = "body_mass_g",
         color = "species"),
    rows = many
  )

  expect_false(grepl("Too many color levels", p$area, fixed = TRUE))
  expect_match(p$area, "Nothing to plot")
  expect_equal(p$text, "Too many color levels (20).")
  expect_match(p$detail, "series", fixed = TRUE)
  expect_true(p$gear_alert)
})
