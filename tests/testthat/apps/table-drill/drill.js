// Helpers for test-table-drill.R: switch dock views, find the row, card or
// cell a user would click in a block, and read what the block shows as
// selected and what its footer says. The click itself is a real mouse event
// sent through the DevTools protocol at the point drillProbe.at() returns.
(function () {
  if (window.drillProbe) return;

  const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
  const vis = (e) => {
    if (!e || !e.isConnected || !e.offsetParent) return false;
    const r = e.getBoundingClientRect();
    return r.width > 0 && r.height > 0;
  };
  const txt = (e) => ((e && e.textContent) || "").replace(/\s+/g, " ").trim();

  const root = (id) =>
    [...document.querySelectorAll('.card[id$="-block_handle-' + id + '"]')]
      .find(vis) || null;

  const busy = () => [...document.querySelectorAll(".recalculating")].some(vis);
  async function idle(max = 30000) {
    const t0 = Date.now();
    await sleep(500);
    while (Date.now() - t0 < max && busy()) await sleep(200);
    await sleep(300);
  }

  async function view(name) {
    const it = [...document.querySelectorAll(".blockr-view-item")].find(
      (x) => txt(x.querySelector(".blockr-view-item-name") || x) === name);
    if (!it) return false;
    (it.querySelector(".blockr-view-item-name") || it).click();
    const t0 = Date.now();
    while (Date.now() - t0 < 30000) {
      await sleep(300);
      const r = document.querySelector('.card[id$="-block_handle-' + name + '"]');
      if (r && vis(r) && rows(r).length && !busy()) break;
    }
    await idle();
    return true;
  }

  // What a click can land on: a records row or card, a groups row or card.
  const ROW_SEL = [
    "tr[data-i]", ".tc-row[data-i]", "tr.blockr-data-row",
    ".blockr-summarize-row", "tr.blockr-section-header",
  ].join(",");
  // Card rows are `display: contents`: visible through their children.
  const shown = (x) => vis(x) || [...x.children].some(vis);
  const rows = (r) => [...r.querySelectorAll(ROW_SEL)].filter(shown);
  const LABEL_SEL = ".tc-rowlab, .blockr-summarize-label, th, td";
  const lab = (x) => x.getAttribute("data-summarize-label") ||
    txt(x.querySelector(LABEL_SEL) || x);

  // The first row whose own label (stub or first cell) is `label`, else
  // whose text starts with it.
  function findRow(id, label) {
    const r = root(id);
    if (!r) return null;
    const all = rows(r);
    return all.find((x) => lab(x) === label) ||
      all.find((x) => txt(x).startsWith(label)) || null;
  }

  // A cell of that row: the n-th td (0-based), or a selector inside it.
  function target(id, label, cell) {
    const row = findRow(id, label);
    if (!row) return null;
    if (cell == null) {
      for (const sel of [".tc", ".blockr-summarize-label", ".blockr-section-value",
                         "th", "td.blockr-stub", "td"]) {
        const el = row.querySelector(sel);
        if (el && vis(el)) return el;
      }
      return row;
    }
    if (typeof cell === "number") return row.querySelectorAll("td")[cell] || null;
    return row.querySelector(cell);
  }

  // Viewport point at the middle of the target, scrolled into view.
  function at(id, label, cell) {
    const el = target(id, label, cell);
    if (!el) return null;
    el.scrollIntoView({ block: "center", inline: "center" });
    const b = el.getBoundingClientRect();
    return { x: b.left + Math.min(b.width / 2, 40), y: b.top + b.height / 2 };
  }

  const ACTIVE_SEL = ".dt-row-active, .is-on, .dt-col-active";
  function state(id) {
    const r = root(id);
    if (!r) return { found: false };
    const active = [...r.querySelectorAll(ACTIVE_SEL)].filter(
      (x) => x.matches(ROW_SEL)).map(lab);
    const foot = [...r.querySelectorAll("*")].filter(
      (x) => x.children.length === 0 && /^Filtered:/.test(txt(x)) && vis(x));
    return {
      found: true,
      active: active,
      footer: foot.map(txt),
      rows: rows(r).length,
      count: txt(r.querySelector(".dt-row-count"))
    };
  }

  window.drillProbe = { view, idle, at, state, findRow, rows, root };
})();
