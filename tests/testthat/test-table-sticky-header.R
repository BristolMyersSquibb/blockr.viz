# The header row of a table stays at the top while its rows scroll, in every
# layout, and a spanner row stays above the row it spans. The table runs its
# full length in its dock panel and the panel scrolls; the table's own box
# scrolls only sideways, so a sticky header would stick to the box and leave
# with the rows. A scroll-driven animation (table.css, measures from
# table/chrome.js followHeader()) moves the header instead. A
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
  // The view's own table: the one inside its block (ids `...block_<id>-...`),
  // not the previous view's, which stays visible while the dock switches.
  const mine = (x) => {
    for (let e = x; e; e = e.parentElement) {
      if (e.id && e.id.includes('block_' + view + '-')) return true;
    }
    return false;
  };
  item.click();
  let t = null;
  for (let i = 0; i < 100 && !t; i++) {
    await wait(100);
    t = [...document.querySelectorAll('.dockview-panel table.blockr-table')]
      .find((x) => x.offsetParent && x.tBodies[0].rows.length > 20 && mine(x));
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
      // The move is an animation (or, without scroll timelines, an inline
      // transform): read the computed transform.
      moved: getComputedStyle(t.tHead).transform !== 'none'
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

# Above 400 items (dom.js VIRT_MIN) the table draws a window of rows around
# what is in view and a spacer for the rest. With the panel scrolling the
# table rather than the table's own box, the window followed the box's
# scroll, which stays at 0: the rows stopped after the first window and the
# rest of the table was blank. Here the table is scrolled to its end, on
# first draw, after a recompose from a short table, and after folding and
# unfolding every group: rows fill the view, nothing blank follows them and
# the box is as tall as the table.
window_probe_js <- "
window.windowProbe = async () => {
  const wait = (ms) => new Promise((r) => setTimeout(r, ms));
  const panel = document.querySelector('.panel-box');
  const t = panel.querySelector('table.blockr-table');
  const box = t.closest('.blockr-table-wrapper');
  const measure = () => {
    const pr = panel.getBoundingClientRect();
    const body = [...t.tBodies[0].rows];
    const inView = body.filter((r) => {
      if (r.classList.contains('vz-table-vspacer')) return false;
      const b = r.getBoundingClientRect();
      return b.bottom > pr.top && b.top < pr.bottom;
    }).length;
    const last = body[body.length - 1];
    return {
      inView: inView,
      blank: last.classList.contains('vz-table-vspacer') ? last.offsetHeight : 0,
      extra: box.scrollHeight - t.offsetHeight
    };
  };
  // Scroll events can come a few frames late: wait for the rows, up to 3 s.
  const toEnd = () => { panel.scrollTop = panel.scrollHeight; };
  toEnd();
  let m = measure();
  for (let i = 0; i < 30 && (m.blank > 0 || m.inView < 10); i++) {
    await wait(100);
    toEnd();
    m = measure();
  }
  return m;
};
"

test_that("a long table has rows to its end in a scrolling panel", {
  skip_without_browser()
  skip_if_not_installed("pkgload")
  Sys.setenv(NOT_CRAN = "true")
  withr::local_envvar(BLOCKR_VIZ_SRC = normalizePath(test_path("..", "..")))
  configure_chromote()
  app <- shinytest2::AppDriver$new(
    test_path("apps", "table-row-window"),
    name = "table-row-window",
    width = 1000, height = 800,
    load_timeout = 120 * 1000, timeout = 30 * 1000
  )
  withr::defer(app$stop())
  # The window draws part of the rows, so these count what it drew.
  rows <- "document.querySelectorAll('.panel-box tr.vz-table-row').length"
  sections <- "document.querySelectorAll('.panel-box tr.vz-table-section-header').length"
  app$wait_for_js(paste(rows, "> 40"))
  app$run_js(window_probe_js)
  expect_full <- function(label) {
    p <- app$get_js("window.windowProbe()")
    expect_gte(p$inView, 10, label = label)
    expect_equal(p$blank, 0, label = label)
    expect_equal(p$extra, 0, label = label)
  }

  expect_full("first draw")

  app$click("small")
  app$wait_for_js(paste(rows, "=== 40"))
  app$click("big")
  app$wait_for_js(paste(rows, "> 40"))
  expect_full("recompose")

  app$run_js("document.querySelector('.panel-box .vz-table-foldall').click()")
  app$wait_for_js(paste(sections, "=== 50 &&", rows, "=== 0"))
  app$run_js("document.querySelector('.panel-box .vz-table-foldall').click()")
  expect_full("folded and unfolded")
})
