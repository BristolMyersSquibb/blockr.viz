# Heading rows follow one rule in every layout (apps/table-heading-levels):
# a row that heads other rows is 600 in the body ink and runs across the row
# on one line; the rows under it are 400 in the body ink. A heading with no
# values of its own is one cell (a groups parent by default, the compact
# look's parents, a finished table's "AGE (years)"); a groups parent with
# its summaries (`group_rows = "summary"`) is one plain row, its name in the
# label column. Before, the compact look's parents widened the label column,
# and a finished table's empty headings wrapped beside empty cells.
#
# Skipped without a headless browser and under R CMD check. On the Mac,
# Chrome cannot start in Claude's sandbox: start
# blockr.dev/scripts/chrome-remote.sh outside it and source
# blockr.dev/scripts/use-remote-chrome.R first.

# Installs window.headProbe(view): opens the view, unfolds every group and
# reads each body row: its lines, cells, colspan, and the label's weight,
# colour, size and line count.
head_probe_js <- "
window.headProbe = async (view) => {
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
  for (let i = 0; i < 150 && !t; i++) {
    await wait(100);
    t = [...document.querySelectorAll('.dockview-panel table.blockr-table')]
      .find((x) => x.offsetParent && x.tBodies[0].rows.length > 0 && mine(x));
  }
  if (!t) return null;
  await wait(300);
  const fa = t.querySelector('.vz-table-foldall');
  if (fa && fa.getAttribute('aria-expanded') === 'false') {
    fa.click();
    await wait(500);
  }
  // Lines of the label's text, not of the fold chevron beside it.
  const lines = (el) => {
    const w = document.createTreeWalker(el, NodeFilter.SHOW_TEXT);
    const tops = new Set();
    let n;
    while ((n = w.nextNode())) {
      if (!n.textContent.trim()) continue;
      const r = document.createRange();
      r.selectNodeContents(n);
      for (const q of r.getClientRects()) tops.add(Math.round(q.top));
    }
    return tops.size;
  };
  const ncol = Math.max(...[...t.tBodies[0].rows].map((r) =>
    [...r.cells].reduce((k, c) => k + c.colSpan, 0)));
  const rows = [...t.tBodies[0].rows].filter((r) => r.classList.contains('vz-table-row'))
    .slice(0, 12).map((r) => {
      const c = r.cells[0];
      const cs = getComputedStyle(c);
      return {
        i: r.getAttribute('data-i'),
        kind: r.classList.contains('vz-table-heading') ? 'heading'
          : r.classList.contains('vz-table-heads') ? 'heads' : 'leaf',
        cells: r.cells.length,
        colspan: c.colSpan,
        text: c.textContent.trim(),
        weight: cs.fontWeight,
        color: cs.color,
        size: cs.fontSize,
        lines: c.textContent.trim() ? lines(c) : 0
      };
    });
  const leaf = t.querySelector('tbody tr.vz-table-row:not(.vz-table-heads) td');
  const head = t.querySelector('tbody tr.vz-table-heading .vz-table-heading-text');
  return {
    ncol: ncol,
    rows: rows,
    labelCol: leaf ? leaf.getBoundingClientRect().width : null,
    headText: head ? head.scrollWidth : null
  };
};
"

DARK <- "rgb(17, 24, 39)"

start_heading_app <- function() {
  skip_without_browser()
  skip_if_not_installed("pkgload")
  skip_if_not_installed("blockr.dock")
  Sys.setenv(NOT_CRAN = "true")
  withr::local_envvar(BLOCKR_VIZ_SRC = normalizePath(test_path("..", "..")),
                      .local_envir = parent.frame())
  configure_chromote()
  app <- shinytest2::AppDriver$new(
    test_path("apps", "table-heading-levels"),
    name = "table-heading-levels",
    width = 1000, height = 800,
    load_timeout = 120 * 1000, timeout = 30 * 1000
  )
  withr::defer(app$stop(), envir = parent.frame())
  app$wait_for_js("document.querySelectorAll('.blockr-view-item').length === 5")
  app$run_js(head_probe_js)
  app
}

expect_level <- function(r, weight, label) {
  expect_identical(r$weight, weight, label = label)
  expect_identical(r$color, DARK, label = label)
}

test_that("heading rows are 600, across the row on one line, in every layout", {
  app <- start_heading_app()
  probe <- function(view) {
    app$get_js(sprintf("window.headProbe('%s')", view), timeout = 60 * 1000)
  }

  # Groups by default: the parent's bare name across the row, one cell; the
  # row count shows only while a group is folded.
  g <- probe("grh")
  kinds <- vapply(g$rows, `[[`, "", "kind")
  expect_identical(kinds[1:3], c("heading", "leaf", "leaf"))
  h <- g$rows[[1]]
  expect_identical(h$colspan, g$ncol)
  expect_identical(h$cells, 1L)
  expect_match(h$text, "[a-z]$")
  expect_level(h, "600", "groups parent")
  expect_identical(h$size, "14px")
  expect_identical(h$lines, 1L)
  expect_level(g$rows[[2]], "400", "groups term")
  expect_gt(g$headText, g$labelCol)

  # With its summaries: one row, the name in the label column, its bar
  # beside it.
  g <- probe("grp")
  kinds <- vapply(g$rows, `[[`, "", "kind")
  expect_identical(kinds[1:3], c("heads", "leaf", "leaf"))
  h <- g$rows[[1]]
  expect_identical(h$colspan, 1L)
  expect_identical(h$cells, g$ncol)
  expect_level(h, "600", "groups parent with summaries")
  expect_false(grepl("(N =", h$text, fixed = TRUE))
  expect_identical(g$rows[[2]]$i, as.character(as.integer(h$i) + 1L))
  expect_level(g$rows[[2]], "400", "groups term")

  # Compact look: one cell across the row; the label column holds the ids.
  k <- probe("cmp")
  h <- k$rows[[1]]
  expect_identical(h$kind, "heading")
  expect_identical(h$cells, 1L)
  expect_identical(h$colspan, k$ncol)
  expect_level(h, "600", "compact parent")
  expect_identical(h$lines, 1L)
  expect_identical(k$rows[[2]]$kind, "leaf")
  expect_level(k$rows[[2]], "400", "compact subject")
  # The short ids keep the label column at the first column's 200px floor.
  expect_lte(k$labelCol, 201)
  expect_gt(k$headText, k$labelCol)

  # A finished table: a heading with empty values spans the row.
  f <- probe("fin")
  texts <- vapply(f$rows, `[[`, "", "text")
  for (lab in c("AGE (years)", "Body mass index at screening (kg/m2)",
                "SEX")) {
    r <- f$rows[[match(lab, texts)]]
    expect_identical(r$kind, "heading", label = lab)
    expect_identical(r$colspan, f$ncol, label = lab)
    expect_level(r, "600", lab)
    expect_identical(r$lines, 1L, label = lab)
  }
  expect_level(f$rows[[match("F", texts)]], "400", "finished leaf")
  expect_identical(f$rows[[match("F", texts)]]$cells, f$ncol)
})

test_that("a parent row with summaries folds, unfolds and drills", {
  app <- start_heading_app()
  app$get_js("window.headProbe('grp')", timeout = 60 * 1000)
  n_rows <- "document.querySelectorAll('.dockview-panel .vz-table-groups-table tbody tr.vz-table-row').length"
  all <- app$get_js(n_rows)
  par <- "document.querySelectorAll('.dockview-panel .vz-table-groups-table tbody tr.is-parent')"

  # Fold the first parent: its terms go, the next parent follows it.
  app$run_js(paste0(par, "[0].querySelector('.vz-table-indent-btn').click()"))
  app$wait_for_js(paste(n_rows, "<", all))
  first <- app$get_js("[...document.querySelectorAll('.dockview-panel .vz-table-groups-table tbody tr.vz-table-row')].slice(0, 2).map((r) => r.classList.contains('is-parent'))")
  expect_identical(unlist(first), c(TRUE, TRUE))
  app$run_js(paste0(par, "[0].querySelector('.vz-table-indent-btn').click()"))
  app$wait_for_js(paste(n_rows, "===", all))

  # A click on a parent's name claims it; R's echo lights its row. A click
  # on the next parent's bar moves the claim there.
  lit <- function(k) {
    sprintf("%s[%d].classList.contains('vz-table-row-active')", par, k)
  }
  app$run_js(paste0(par, "[0].cells[0].click()"))
  app$wait_for_js(lit(0L))
  expect_false(app$get_js(lit(1L)))
  app$run_js(paste0(par, "[1].cells[1].click()"))
  app$wait_for_js(lit(1L))
  expect_false(app$get_js(lit(0L)))
})

# Above 400 items the grid draws a window of rows and a spacer for the rest.
# At the end of a 640-item groups table (40 parents, headings by default),
# rows fill the view and no spacer is left over.
test_that("a long groups table with heading rows has rows to its end", {
  app <- start_heading_app()
  app$get_js("window.headProbe('lng')", timeout = 60 * 1000)
  p <- app$get_js("(async () => {
    const wait = (ms) => new Promise((r) => setTimeout(r, ms));
    const t = [...document.querySelectorAll('.dockview-panel table.blockr-table')]
      .find((x) => x.offsetParent);
    let sc = t.parentElement;
    while (sc && !(sc.scrollHeight > sc.clientHeight + 1 &&
                   /auto|scroll/.test(getComputedStyle(sc).overflowY))) sc = sc.parentElement;
    const measure = () => {
      const pr = sc.getBoundingClientRect();
      const body = [...t.tBodies[0].rows];
      const last = body[body.length - 1];
      return {
        spacers: t.querySelectorAll('tr.vz-table-vspacer').length,
        inView: body.filter((r) => {
          if (r.classList.contains('vz-table-vspacer')) return false;
          const b = r.getBoundingClientRect();
          return b.bottom > pr.top && b.top < pr.bottom;
        }).length,
        blank: last.classList.contains('vz-table-vspacer') ? last.offsetHeight : 0,
        lastText: last.cells[0].textContent.trim()
      };
    };
    const toEnd = () => { sc.scrollTop = sc.scrollHeight; };
    toEnd();
    let m = measure();
    for (let i = 0; i < 30 && (m.blank > 0 || m.inView < 10); i++) {
      await wait(100);
      toEnd();
      m = measure();
    }
    return m;
  })()", timeout = 60 * 1000)
  expect_gte(p$spacers, 1L)
  expect_gte(p$inView, 10L)
  expect_equal(p$blank, 0)
  expect_match(p$lastText, "^Term 40\\.")
})
