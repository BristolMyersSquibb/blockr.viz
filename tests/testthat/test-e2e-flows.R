# The five flows a reader of a clinical board goes through most, clicked in a
# browser on one dock board (apps/viz-flows):
#
# 1. the arms a study pins keep their colours, and a level it does not pin
#    keeps one colour across charts,
# 2. a click on a chart sends the subject to a drill filter block, says so
#    in the footer and latches nothing at the sender,
# 3. a title and the download switch set in the gear show on the face and
#    survive a save and restore,
# 4. every download format arrives, named after the title, with its content,
# 5. a word of the subtitle opens a menu that changes the mapping,
# 6. a prepare script's value named in a subtitle is a word that sets it, with
#    no second control in the script band, on a table, a chart and a table
#    saved by blockr.viz 0.2.x, and a restored board's Titles row loads
#    unedited ("not applied" hidden),
# 7. a groups table's row-column and summary words are links from the first
#    payload, also on a summarize table saved by 0.2.x (`By {@group}, top
#    {@top_n}`), and a pick in their menus changes that part.
#
# Clicks and key presses are real input events (DevTools protocol). The app
# loads blockr.viz from this source tree (BLOCKR_VIZ_SRC). Skipped without a
# headless browser and under R CMD check. On the Mac, Chrome cannot start in
# Claude's sandbox: start blockr.dev/scripts/chrome-remote.sh outside it and
# source blockr.dev/scripts/use-remote-chrome.R first.

run_flows <-
  run_browser_tests() &&
  requireNamespace("blockr.dock", quietly = TRUE) &&
  requireNamespace("blockr.dm", quietly = TRUE) &&
  requireNamespace("pkgload", quietly = TRUE)

flows_dir <- test_path("apps", "viz-flows")

flows_app <- function(name, restore = NULL) {
  Sys.setenv(NOT_CRAN = "true")
  withr::local_envvar(
    BLOCKR_VIZ_SRC = normalizePath(test_path("..", "..")),
    BLOCKR_FLOWS_RESTORE = restore %||% ""
  )
  configure_chromote()
  app <- shinytest2::AppDriver$new(
    flows_dir, name = name, width = 1500, height = 1000,
    load_timeout = 180 * 1000, timeout = 60 * 1000
  )
  # A background tab of a shared Chrome stops painting.
  try({
    cdp <- app$get_chromote_session()
    cdp$Page$bringToFront()
    cdp$Emulation$setFocusEmulationEnabled(enabled = TRUE)
  }, silent = TRUE)
  app$wait_for_idle()
  app$run_js(FLOWS_JS)
  app
}

# Page helpers: switch views, find a block's frame, read its face and its
# chart, and give the point a mouse event should go to.
FLOWS_JS <- "
(function () {
  if (window.flows) return;
  const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
  const txt = (e) => ((e && e.textContent) || '').replace(/\\s+/g, ' ').trim();
  const vis = (e) => !!e && e.isConnected && !!e.offsetParent;
  const frame = (id) => [...document.querySelectorAll(
    '[id$=\"-block_' + id + '-expr-chart\"] .vz-frame, ' +
    '[id$=\"-block_' + id + '-expr-table\"] .vz-frame')].find(vis) || null;
  const chart = (id) => {
    const f = frame(id);
    const el = f && f.querySelector('[_echarts_instance_]');
    return el ? { el, inst: window.echarts.getInstanceByDom(el) } : null;
  };
  async function view(name) {
    const it = [...document.querySelectorAll('.blockr-view-item')].find(
      (x) => txt(x.querySelector('.blockr-view-item-name') || x) === name);
    if (!it) return false;
    (it.querySelector('.blockr-view-item-name') || it).click();
    return true;
  }
  function face(id) {
    const f = frame(id);
    if (!f) return null;
    const q = (s) => { const e = f.querySelector(s); return e ? txt(e) : ''; };
    const c = chart(id);
    let colors = {}, cats = [];
    if (c) {
      c.inst.getModel().eachSeries((s) => {
        const st = s.getData().getVisual('style');
        colors[s.name] = String((st && (st.fill || st.stroke)) || '').toUpperCase();
      });
      const o = c.inst.getOption();
      const ax = [].concat(o.xAxis || [], o.yAxis || []).find((a) => a && a.type === 'category');
      cats = ax ? ax.data.map(String) : [];
    }
    return { title: q('.vz-title'), sub: q('.vz-subtitle'), status: q('.vz-status'),
             download: !!f.querySelector('.vz-download a, .vz-download button'),
             colors, cats, drawn: !!c };
  }
  // Viewport point of data item i of series s, scrolled into view.
  function item(id, s, i) {
    const c = chart(id);
    if (!c) return null;
    c.el.scrollIntoView({ block: 'center' });
    const L = c.inst.getModel().getSeriesByIndex(s).getData().getItemLayout(i);
    const r = c.el.getBoundingClientRect();
    const p = Array.isArray(L) ? L : [L.x + L.width / 2, L.y + L.height / 2];
    return { x: r.left + p[0], y: r.top + p[1],
             name: c.inst.getModel().getSeriesByIndex(s).getData().getName(i) };
  }
  // Viewport point at the middle of the first visible element matching
  // `sel` inside the block's frame (or the page), whose text matches `has`.
  function at(id, sel, has) {
    const root = id ? frame(id) : document;
    if (!root) return null;
    const e = [...root.querySelectorAll(sel)].filter(vis)
      .find((x) => !has || txt(x).includes(has));
    if (!e) return null;
    e.scrollIntoView({ block: 'center' });
    const r = e.getBoundingClientRect();
    return { x: r.left + r.width / 2, y: r.top + r.height / 2, text: txt(e) };
  }
  window.flows = { view, face, item, at, frame };
})();
"

flows_face <- function(app, id) {
  f <- app$get_js(sprintf("flows.face('%s')", id))
  f$colors <- unlist(f$colors)
  f$cats <- as.character(unlist(f$cats))
  f
}

flows_result <- function(app, id) {
  app$get_values(export = "result")$export$result[[id]]
}

flows_saved_block <- function(app, id) {
  json <- app$get_values(export = "saved")$export$saved
  jsonlite::fromJSON(json, simplifyVector = FALSE)$payload$blocks$payload[[id]]
}

# Show a view and wait until the block draws.
flows_view <- function(app, view, id) {
  app$get_js(sprintf("flows.view('%s')", view))
  t0 <- Sys.time()
  repeat {
    app$wait_for_idle()
    f <- flows_face(app, id)
    if (!is.null(f) && (isTRUE(f$drawn) || nzchar(f$title))) break
    if (difftime(Sys.time(), t0, units = "secs") > 60) {
      stop("view ", view, " did not draw ", id)
    }
    Sys.sleep(1)
  }
  Sys.sleep(0.8) # past the chart's deferred resize()
}

# Real mouse input at a viewport point.
flows_mouse <- function(app, p, type = c("click", "move")) {
  type <- match.arg(type)
  cdp <- app$get_chromote_session()
  cdp$Input$dispatchMouseEvent(type = "mouseMoved", x = p$x, y = p$y)
  if (type == "click") {
    for (t in c("mousePressed", "mouseReleased")) {
      cdp$Input$dispatchMouseEvent(type = t, x = p$x, y = p$y,
                                   button = "left", buttons = 1,
                                   clickCount = 1)
    }
  }
  Sys.sleep(0.4)
  app$wait_for_idle()
}

flows_click <- function(app, id, sel, has = NULL) {
  p <- app$get_js(sprintf("flows.at(%s, %s, %s)",
                          if (is.null(id)) "null" else sprintf("'%s'", id),
                          jsonlite::toJSON(sel, auto_unbox = TRUE),
                          if (is.null(has)) "null" else
                            jsonlite::toJSON(has, auto_unbox = TRUE)))
  if (is.null(p)) stop("nothing to click: ", sel, " ", has %||% "")
  flows_mouse(app, p)
  invisible(p)
}

flows_type <- function(app, text) {
  cdp <- app$get_chromote_session()
  cdp$Input$insertText(text = text)
  for (t in c("keyDown", "keyUp")) {
    cdp$Input$dispatchKeyEvent(type = t, key = "Enter", code = "Enter",
                               windowsVirtualKeyCode = 13)
  }
  Sys.sleep(0.6)
  app$wait_for_idle()
}

# The gear's groups: an index on a wide tray, tabs on a narrow one.
NAV <- ".blockr-tray-nav__group, .blockr-tray-nav__tab"

if (run_flows) {
  app <- flows_app("viz-flows")
  withr::defer(app$stop(), testthat::teardown_env())
}

test_that("flows: pinned arms keep their colours, an unpinned level its own", {
  skip_if_not(run_flows, "no headless browser, or under R CMD check")
  flows_view(app, "drill", "arms")
  pinned <- c("Placebo" = "#999999", "Xanomeline Low Dose" = "#56B4E9",
              "Xanomeline High Dose" = "#0072B2")
  scatter <- flows_face(app, "arms")$colors
  expect_identical(scatter[names(pinned)], pinned)
  expect_length(unique(scatter[names(pinned)]), 3L)
  # Screen Failure is not pinned: it takes some colour, and the same one in
  # a second chart of a different type.
  expect_true(nzchar(scatter[["Screen Failure"]]))
  flows_view(app, "drill", "arms_bar")
  bar <- flows_face(app, "arms_bar")$colors
  expect_identical(bar[names(pinned)], pinned)
  expect_identical(bar[["Screen Failure"]], scatter[["Screen Failure"]])
})

test_that("flows: a chart click drills to the target and latches nothing", {
  skip_if_not(run_flows, "no headless browser, or under R CMD check")
  flows_view(app, "drill", "arms")
  expect_identical(nrow(flows_result(app, "ctrl_out")), 32L)

  p <- app$get_js("flows.item('arms', 0, 0)")
  flows_mouse(app, p)
  receipt <- flows_face(app, "arms")$status
  subj <- flows_result(app, "ctrl_out")$USUBJID
  expect_length(subj, 1L)
  expect_match(receipt, "Drilled down to")
  expect_match(receipt, subj, fixed = TRUE)

  # The receipt goes and nothing stays selected at the sender.
  Sys.sleep(3.5)
  expect_identical(flows_face(app, "arms")$status, "")

  # A click on another subject replaces the claim.
  p2 <- app$get_js("flows.item('arms', 1, 0)")
  flows_mouse(app, p2)
  subj2 <- flows_result(app, "ctrl_out")$USUBJID
  expect_length(subj2, 1L)
  expect_false(identical(subj2, subj))
})

test_that("flows: a title and the download switch set in the gear survive a restore", {
  skip_if_not(run_flows, "no headless browser, or under R CMD check")
  flows_view(app, "tools", "gear")
  expect_identical(flows_face(app, "gear")$title, "Gear chart")
  expect_true(flows_face(app, "gear")$download)

  flows_click(app, "gear", ".blockr-gear-btn")
  flows_click(app, "gear", NAV, "Titles")
  p <- app$get_js("(function(){
    const f = flows.frame('gear');
    const row = [...f.querySelectorAll('.vz-tray .vz-row')].find((r) =>
      (r.querySelector('.blockr-label') || {}).textContent === 'Title');
    const i = row.querySelector('input');
    i.scrollIntoView({block: 'center'}); i.select();
    const b = i.getBoundingClientRect();
    return {x: b.left + b.width / 2, y: b.top + b.height / 2};
  })()")
  flows_mouse(app, p)
  app$run_js("document.activeElement.select();")
  flows_type(app, "Mean age, restored")
  expect_identical(flows_face(app, "gear")$title, "Mean age, restored")

  flows_click(app, "gear", NAV, "Interaction")
  p <- app$get_js("(function(){
    const f = flows.frame('gear');
    const row = [...f.querySelectorAll('.vz-tray .vz-row')].find((r) =>
      (r.querySelector('.blockr-label') || {}).textContent === 'Download');
    const c = row.querySelector('label.blockr-checkbox');
    c.scrollIntoView({block: 'center'});
    const b = c.getBoundingClientRect();
    return {x: b.left + 8, y: b.top + b.height / 2};
  })()")
  flows_mouse(app, p)
  expect_false(flows_face(app, "gear")$download)

  saved <- withr::local_tempfile(fileext = ".json")
  writeLines(app$get_values(export = "saved")$export$saved, saved)
  app2 <- flows_app("viz-flows-restore", restore = saved)
  withr::defer(app2$stop())
  flows_view(app2, "tools", "gear")
  f <- flows_face(app2, "gear")
  expect_identical(f$title, "Mean age, restored")
  expect_false(f$download)
  expect_true(f$drawn)
})

test_that("flows: every download format arrives named after the title", {
  skip_if_not(run_flows, "no headless browser, or under R CMD check")
  flows_view(app, "tools", "dl")
  # The menu's links, read while it is closed (an open menu leaves the frame).
  ids <- unlist(app$get_js(
    "[...flows.frame('dl').querySelectorAll('.vz-download a.shiny-download-link')].map((a) => a.id)"
  ))
  exts <- unlist(app$get_js(
    "[...flows.frame('dl').querySelectorAll('.vz-download .blockr-menu__meta')].map((a) => a.textContent.trim())"
  ))
  expect_setequal(exts, c(".xlsx", ".html", ".pptx", ".png"))
  # Reaching for the menu takes the picture the downloads write.
  p <- app$get_js("flows.at('dl', '.vz-download .blockr-action-menu__trigger')")
  flows_mouse(app, p, "move")
  flows_mouse(app, p)

  magic <- c(xlsx = "504b0304", pptx = "504b0304", png = "89504e47")
  for (k in seq_along(ids)) {
    f <- app$get_download(ids[[k]])
    ext <- sub("^\\.", "", exts[[k]])
    expect_match(basename(f), paste0("^Mean_age_by_arm_.*\\.", ext, "$"),
                 label = paste("file name of the", ext))
    expect_gt(file.size(f), 1000)
    if (ext %in% names(magic)) {
      head <- paste(as.character(readBin(f, "raw", 4L)), collapse = "")
      expect_identical(head, magic[[ext]], label = paste("signature of the", ext))
    }
    if (ext == "html") {
      expect_match(paste(readLines(f, warn = FALSE), collapse = "\n"),
                   "Mean age by arm", fixed = TRUE)
    }
    if (ext == "xlsx") {
      ss <- utils::unzip(f, "xl/sharedStrings.xml", exdir = tempfile())
      strs <- paste(readLines(ss, warn = FALSE), collapse = "")
      expect_match(strs, "Mean age by arm", fixed = TRUE)
      for (a in c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose",
                  "Screen Failure")) {
        expect_match(strs, a, fixed = TRUE)
      }
    }
  }
})

test_that("flows: a subtitle word opens a menu that changes the mapping", {
  skip_if_not(run_flows, "no headless browser, or under R CMD check")
  flows_view(app, "tools", "words")
  f <- flows_face(app, "words")
  expect_setequal(f$cats, c("Placebo", "Xanomeline High Dose",
                            "Xanomeline Low Dose", "Screen Failure"))

  flows_click(app, "words", ".vz-subtitle .blockr-slot", "ARM")
  flows_click(app, NULL, ".blockr-select__option", "SEX")
  app$run_js("document.dispatchEvent(new KeyboardEvent('keydown', {key: 'Escape'}));")
  app$wait_for_idle()
  Sys.sleep(1)

  f <- flows_face(app, "words")
  expect_match(f$sub, "SEX", fixed = TRUE)
  expect_setequal(f$cats, c("F", "M"))
  st <- flows_saved_block(app, "words")$payload$category
  expect_identical(unlist(st), "SEX")
})

# The words of a block's subtitle, and the script band's rows (by config key)
# with whether the band shows.
flows_script_face <- function(app, id) {
  f <- app$get_js(sprintf("(function () {
    const f = flows.frame('%s');
    if (!f) return null;
    const band = f.querySelector('.vz-script-band');
    return {
      sub: (f.querySelector('.vz-subtitle') || {}).textContent || '',
      words: [...f.querySelectorAll('.vz-subtitle .blockr-slot')].map((w) => w.textContent),
      band: [...f.querySelectorAll('.vz-script-band-row')].map((r) => r.getAttribute('data-role')),
      shown: !!band && !band.hidden
    };
  })()", id))
  f$words <- as.character(unlist(f$words))
  f$band <- as.character(unlist(f$band))
  f
}

# Click the subtitle word `word`, type `value` over it and press Enter.
flows_set_word <- function(app, id, word, value) {
  flows_click(app, id, ".vz-subtitle .blockr-slot", word)
  expect_true(app$get_js(
    "!!document.activeElement && document.activeElement.classList.contains('blockr-slot-input')"
  ), label = paste("the word", word, "turned into a field"))
  app$run_js("document.activeElement.select();")
  flows_type(app, value)
}

# The Titles group of a block's gear: is the subtitle row marked edited, and
# does its "not applied" label show?
flows_titles_row <- function(app, id) {
  flows_click(app, id, ".blockr-gear-btn")
  flows_click(app, id, NAV, "Titles")
  r <- app$get_js(sprintf("(function () {
    const f = flows.frame('%s');
    const inp = f.querySelector('.vz-tray [data-gear-input=\"subtitle\"]');
    if (!inp) return null;
    const row = inp.closest('.vz-row');
    const stale = row.querySelector('.vz-template-stale');
    return { value: inp.value, dirty: row.classList.contains('vz-row--dirty'),
             stale: getComputedStyle(stale).display };
  })()", id))
  flows_click(app, id, ".blockr-gear-btn")
  r
}

test_that("flows: script values in a subtitle are its words, not band rows", {
  skip_if_not(run_flows, "no headless browser, or under R CMD check")
  flows_view(app, "script", "script_tbl")
  tbl <- flows_script_face(app, "script_tbl")
  expect_identical(tbl$words, c("Arm", "1", "Patients"))
  expect_identical(tbl$band, character())
  expect_false(tbl$shown)

  flows_view(app, "script", "script_chart")
  ch <- flows_script_face(app, "script_chart")
  expect_identical(ch$words, "50")
  expect_identical(ch$band, "sv_sex", label = "the value no word names keeps its row")
  expect_true(ch$shown)

  flows_view(app, "script", "legacy")
  old <- flows_script_face(app, "legacy")
  expect_identical(old$words, c("Arm", "2", "Patients"),
                   label = "the 0.2.x table's saved value")
  expect_identical(old$band, character())
})

test_that("flows: a script word sets its value, the subtitle and the result follow", {
  skip_if_not(run_flows, "no headless browser, or under R CMD check")
  flows_view(app, "script", "script_chart")
  n0 <- nrow(flows_result(app, "script_chart"))
  flows_set_word(app, "script_chart", "50", "60")
  ch <- flows_script_face(app, "script_chart")
  expect_match(ch$sub, "aged 60 or older", fixed = TRUE)
  expect_identical(ch$words, "60")
  expect_identical(ch$band, "sv_sex")
  res <- flows_result(app, "script_chart")
  expect_true(all(res$AGE >= 60))
  expect_lt(nrow(res), n0)
  expect_identical(unlist(flows_saved_block(app, "script_chart")$payload$script_values),
                   c(min_age = 60))

  flows_view(app, "script", "script_tbl")
  flows_set_word(app, "script_tbl", "1", "3")
  tbl <- flows_script_face(app, "script_tbl")
  expect_match(tbl$sub, "with at least 3 Patients", fixed = TRUE)
  expect_identical(tbl$band, character())
  expect_identical(unlist(flows_saved_block(app, "script_tbl")$payload$script_values),
                   c(min_share = 3))
})

test_that("flows: restored script subtitles keep their words; Titles loads unedited", {
  skip_if_not(run_flows, "no headless browser, or under R CMD check")
  flows_view(app, "script", "legacy")
  r <- flows_titles_row(app, "legacy")
  # The saved template's line break, as the `\n` R reads as one.
  expect_identical(r$value, "By: {@rows}\\n[with at least {@min_share} {@share_unit}]")
  expect_false(r$dirty)
  expect_identical(r$stale, "none")

  # The board as saved after the edits above, restored.
  saved <- withr::local_tempfile(fileext = ".json")
  writeLines(app$get_values(export = "saved")$export$saved, saved)
  app3 <- flows_app("viz-flows-script-restore", restore = saved)
  withr::defer(app3$stop())
  flows_view(app3, "script", "script_chart")
  ch <- flows_script_face(app3, "script_chart")
  expect_identical(ch$words, "60")
  expect_identical(ch$band, "sv_sex")
  for (id in c("script_chart", "script_tbl", "legacy")) {
    flows_view(app3, "script", id)
    r <- flows_titles_row(app3, id)
    expect_false(r$dirty, label = paste(id, "subtitle row marked edited"))
    expect_identical(r$stale, "none", label = paste(id, "'not applied'"))
  }
})

# Pick `option` in the menu a subtitle word opened.
flows_pick <- function(app, option) {
  flows_click(app, NULL, ".blockr-select__option", option)
  app$run_js("document.dispatchEvent(new KeyboardEvent('keydown', {key: 'Escape'}));")
  app$wait_for_idle()
  Sys.sleep(1)
}

test_that("flows: a groups table's row and summary words are links from the start", {
  skip_if_not(run_flows, "no headless browser, or under R CMD check")
  flows_view(app, "groups", "groups_words")
  f <- flows_script_face(app, "groups_words")
  expect_identical(f$words, c("ARM", "mean", "AGE"))

  flows_view(app, "groups", "legacy_words")
  old <- flows_script_face(app, "legacy_words")
  expect_identical(old$words, c("SEX", "2"),
                   label = "the 0.2.x subtitle `By {@group}, top {@top_n}`")
})

test_that("flows: a groups word's menu changes the row column or the summary", {
  skip_if_not(run_flows, "no headless browser, or under R CMD check")
  flows_view(app, "groups", "groups_words")
  flows_click(app, "groups_words", ".vz-subtitle .blockr-slot", "ARM")
  flows_pick(app, "SEX")
  f <- flows_script_face(app, "groups_words")
  expect_match(f$sub, "By SEX", fixed = TRUE)
  expect_identical(unlist(flows_saved_block(app, "groups_words")$payload$by), "SEX")

  flows_click(app, "groups_words", ".vz-subtitle .blockr-slot", "mean")
  flows_pick(app, "Median")
  f <- flows_script_face(app, "groups_words")
  expect_match(f$sub, "median of AGE", fixed = TRUE)
  s1 <- flows_saved_block(app, "groups_words")$payload$summaries[[1L]]
  expect_identical(s1$func, "median")
  expect_identical(s1$col, "AGE")

  flows_view(app, "groups", "legacy_words")
  flows_click(app, "legacy_words", ".vz-subtitle .blockr-slot", "SEX")
  flows_pick(app, "ARM")
  expect_identical(unlist(flows_saved_block(app, "legacy_words")$payload$by), "ARM")
  expect_identical(flows_script_face(app, "legacy_words")$words, c("ARM", "2"))
})
