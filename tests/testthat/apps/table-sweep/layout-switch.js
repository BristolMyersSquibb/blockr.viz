// Layout switch probe for test-table-layout-switch.R. Needs sweep.js
// (blockrSweep.view / tab) loaded first.
//
//   blockrLayout.read(id)        what block `id` shows and its gears say
//   blockrLayout.pick(id, name)  one click on segment `name` in the open gear
//   blockrLayout.reopen(id)      close the gear, open it again, read
//
// A read is {shown, gears, open, selected, table1, badge}: the layout on
// screen, the number of visible gear buttons and open gear trays in the
// block, the chosen layout tile in the open tray (null without
// one), and whether that tray holds the Table 1 settings (the Variables
// section) and the input badge.
// A pick adds {gaps, blanks, samples}: from the click until the block has
// settled, sampled every 8 ms, how often the block showed no open tray, and
// how often it showed neither a table nor the layout's own empty message
// (Per variable without variables asks for them); and `scrollLost`, whether a scrolled panel
// lost its scroll although the new layout is tall enough to keep it.
//   blockrLayout.scroll(id, px)  scroll the block's dock panel
(function () {
  if (window.blockrLayout) return;

  const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
  const vis = (e) => {
    if (!e || !e.isConnected || !e.offsetParent) return false;
    if (e.checkVisibility && !e.checkVisibility({ checkVisibilityCSS: true })) return false;
    const r = e.getBoundingClientRect();
    return r.width > 0 && r.height > 0;
  };
  const txt = (e) => ((e && e.textContent) || "").replace(/\s+/g, " ").trim();
  // Tiles by value; a name in pick() is the value in any case.
  const LAYOUTS = ["records", "groups", "variables"];
  const val = (e) => (e && e.getAttribute("data-value")) || "";

  const root = (id) => document.querySelector(
    ['.card', '.blockr-block-card'].map((c) => c + '[id$="-block_handle-' + id + '"]').join(", "));
  // The table view writes the layout it draws on its frame (data-layout).
  function shown(id) {
    const r = root(id);
    const f = r && [...r.querySelectorAll(".vz-table[data-layout]")].find(vis);
    return f ? f.getAttribute("data-layout") : null;
  }

  const gears = (r) => [...r.querySelectorAll(".blockr-gear-btn")].filter(vis);
  const tables = (r) => [...r.querySelectorAll("table, .vz-empty")].filter(vis);
  const trays = (r) => [...r.querySelectorAll(".blockr-settings--open")].filter(vis);
  const segs = (tray) => [...tray.querySelectorAll("[data-role=layout] [role=radio]")]
    .filter((s) => LAYOUTS.includes(val(s)));

  function read(id) {
    const r = root(id);
    const t = trays(r);
    let selected = null, table1 = false, badge = false;
    if (t.length === 1) {
      const on = segs(t[0]).filter((s) => s.getAttribute("aria-checked") === "true");
      selected = on.length === 1 ? val(on[0]) : on.map(val).join("+") || null;
      table1 = !!t[0].querySelector('.vz-section[data-section="Variables"]');
      badge = !!t[0].querySelector(".dd-input-badge");
    }
    return { shown: shown(id), gears: gears(r).length, open: t.length, selected, table1, badge };
  }

  const busy = () => [...document.querySelectorAll(".recalculating")].some(vis);
  async function settle(max = 15000) {
    const t0 = Date.now();
    await sleep(300);
    while (Date.now() - t0 < max && busy()) await sleep(150);
    await sleep(500);
  }

  async function openGear(id) {
    const r = root(id);
    if (trays(r).length) return trays(r)[0];
    const g = gears(r)[0];
    if (!g) return null;
    g.click();
    await sleep(400);
    return trays(r)[0] || null;
  }

  async function closeGear(id) {
    const r = root(id);
    if (!trays(r).length) return;
    const g = gears(r)[0];
    if (g) g.click();
    await sleep(300);
  }

  // Show the block: its view, then its tab.
  async function show(view, tab) {
    await window.blockrSweep.view(view);
    if (tab) await window.blockrSweep.tab(tab);
    await settle();
  }

  // One click on the segment, then wait until the block shows that layout
  // (or `max` ms pass) and the server is idle.
  async function pick(id, name, max = 8000) {
    const tray = await openGear(id);
    if (!tray) return Object.assign(read(id), { clicked: false });
    const s = segs(tray).find((x) => val(x) === name.toLowerCase());
    if (!s) return Object.assign(read(id), { clicked: false });
    const r = root(id);
    const sc = scroller(r);
    const scroll0 = sc ? sc.scrollTop : 0;
    let gaps = 0, blanks = 0, samples = 0, sampling = true;
    const sample = () => {
      if (!sampling) return;
      samples++;
      if (trays(r).length !== 1) gaps++;
      if (!tables(r).length) blanks++;
      setTimeout(sample, 8);
    };
    s.click();
    sample();
    const want = name.toLowerCase();
    const t0 = Date.now();
    while (Date.now() - t0 < max && shown(id) !== want) await sleep(150);
    await settle();
    sampling = false;
    const room = sc ? sc.scrollHeight - sc.clientHeight : 0;
    const scrollLost = !!sc && scroll0 > 0 && room >= scroll0 && sc.scrollTop < scroll0 - 1;
    return Object.assign(read(id), { clicked: true, gaps, blanks, samples, scrollLost });
  }

  // The dock panel that scrolls the block.
  function scroller(r) {
    for (let e = r; e; e = e.parentElement) {
      if (e.scrollHeight > e.clientHeight && /auto|scroll/.test(getComputedStyle(e).overflowY)) return e;
    }
    return null;
  }
  async function scroll(id, px) {
    const sc = scroller(root(id));
    if (sc) sc.scrollTop = px;
    await sleep(200);
    return sc ? sc.scrollTop : null;
  }

  async function reopen(id) {
    await closeGear(id);
    const closed = read(id);
    await openGear(id);
    await sleep(300);
    return Object.assign(read(id), { closedOpen: closed.open });
  }

  window.blockrLayout = { read, pick, reopen, show, openGear, closeGear, scroll };
})();
