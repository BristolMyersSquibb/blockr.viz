# The header row of a table stays at the top while its rows scroll, in every
# layout, and a spanner row stays above the row it spans. The table runs its
# full length in its dock panel and the panel scrolls; the table's own box
# scrolls only sideways, so a sticky header would stick to the box and leave
# with the rows. table/chrome.js followHeader() moves the header instead. A
# box with a height limit (interactive_table()'s `max_height`) scrolls
# itself, and the header is sticky in it.
#
# Skipped without a headless browser and under R CMD check. On the Mac,
# Chrome cannot start in Claude's sandbox: start
# blockr.dev/scripts/chrome-remote.sh outside it and source
# blockr.dev/scripts/use-remote-chrome.R first.

# Installs window.stickyProbe(view, box): opens the view, scrolls its table
# 200px past the top of the panel (or, with `box`, of a 300px box around the
# table) and returns where each header row sits, from the scroller's top.
sticky_probe_js <- "
window.stickyProbe = async (view, box) => {
  const wait = (ms) => new Promise((r) => setTimeout(r, ms));
  const item = [...document.querySelectorAll('.blockr-view-item')].find((e) =>
    e.querySelector('.blockr-view-item-name').textContent.trim() === view);
  item.click();
  let t = null;
  for (let i = 0; i < 100 && !t; i++) {
    await wait(100);
    t = [...document.querySelectorAll('.dockview-panel table.blockr-table')]
      .find((x) => x.offsetParent && x.tBodies[0].rows.length > 20);
  }
  if (!t) return null;
  await wait(500);
  const panel = t.closest('.dockview-panel');
  const wrap = t.closest('.blockr-table-wrapper');
  panel.scrollTop = 0;
  let sc = panel;
  if (box) {
    wrap.style.maxHeight = '300px';
    sc = wrap;
  }
  await wait(300);
  const measure = () => {
    const s = sc.getBoundingClientRect().top;
    return {
      tableTop: t.getBoundingClientRect().top - s,
      rows: [...t.tHead.rows].map((r) => {
        const b = r.getBoundingClientRect();
        return { top: b.top - s, bottom: b.bottom - s };
      }),
      moved: t.tHead.style.transform !== ''
    };
  };
  // Scroll events can come a few frames late: wait for the header to
  // settle, up to 3 s.
  let out = measure();
  for (let i = 0; i < 30; i++) {
    if (out.tableTop > -150) {
      sc.scrollTop = sc.scrollTop + out.tableTop + 200;
    } else if (Math.abs(out.rows[0].top) < 1) {
      break;
    }
    await wait(100);
    out = measure();
  }
  wrap.style.maxHeight = '';
  return out;
};
"

test_that("a table's header stays at the top while its rows scroll", {
  skip_without_browser()
  skip_if_not_installed("pkgload")
  skip_if_not_installed("blockr.dock")
  Sys.setenv(NOT_CRAN = "true")
  withr::local_envvar(BLOCKR_VIZ_SRC = normalizePath(test_path("..", "..")))
  configure_chromote()
  app <- shinytest2::AppDriver$new(
    test_path("apps", "table-sticky-header"),
    name = "table-sticky-header",
    width = 1000, height = 700,
    load_timeout = 120 * 1000, timeout = 30 * 1000
  )
  withr::defer(app$stop())
  app$wait_for_js("document.querySelectorAll('.blockr-view-item').length === 3")
  app$run_js(sticky_probe_js)

  probe <- function(view, box = FALSE) {
    app$get_js(sprintf(
      "window.stickyProbe('%s', %s)", view, tolower(as.character(box))
    ))
  }
  expect_at_top <- function(p, n_rows, label) {
    expect_false(is.null(p), label = label)
    # The table has scrolled past the top...
    expect_lt(p$tableTop, -150, label = label)
    # ...and its header rows have not: the first at the top, each next one
    # right under the one before.
    expect_length(p$rows, n_rows)
    expect_lt(abs(p$rows[[1]]$top), 1, label = label)
    for (i in seq_along(p$rows)[-1]) {
      expect_lt(abs(p$rows[[i]]$top - p$rows[[i - 1]]$bottom), 1,
                label = label)
    }
  }

  flat <- probe("flat")
  expect_at_top(flat, 1L, "records")
  expect_true(flat$moved)

  span <- probe("span")
  expect_at_top(span, 2L, "structured, spanner row")
  expect_true(span$moved)

  grp <- probe("grp")
  expect_at_top(grp, 1L, "groups")
  expect_true(grp$moved)

  # A box with a height limit scrolls itself and its header is sticky.
  boxed <- probe("span", box = TRUE)
  expect_at_top(boxed, 2L, "structured in a 300px box")
  expect_false(boxed$moved)
})
