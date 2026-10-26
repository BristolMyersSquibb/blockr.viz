// "Rows are" switch probe for test-table-layout-switch.R. Needs sweep.js
// (blockrSweep.view / tab) loaded first.
//
//   blockrLayout.read(id)        what block `id` shows and its gears say
//   blockrLayout.pick(id, name)  one click on segment `name` in the open gear
//   blockrLayout.reopen(id)      close the gear, open it again, read
//
// A read is {shown, gears, open, selected, table1, badge}: the layout on
// screen, the number of visible gear buttons and open gear trays in the
// block, the highlighted "Rows are" segment in the open tray (null without
// one), and whether that tray holds the Table 1 settings and the input badge.
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
  const LAYOUTS = ["Records", "Groups", "Variables"];

  const root = (id) => document.querySelector(
    ['.card', '.blockr-block-card'].map((c) => c + '[id$="-block_handle-' + id + '"]').join(", "));
  const wrap = (r, w) => r && r.querySelector('[id$="-' + w + '_wrap"]');
  const shownOn = (e) => !!e && getComputedStyle(e).display !== "none";

  // The variables layout shows the records wrap too (the records engine
  // draws it), so the variables wrap decides first.
  function shown(id) {
    const r = root(id);
    if (shownOn(wrap(r, "groups"))) return "groups";
    if (shownOn(wrap(r, "variables"))) return "variables";
    if (shownOn(wrap(r, "records"))) return "records";
    return null;
  }

  const gears = (r) => [...r.querySelectorAll(".blockr-gear-btn")].filter(vis);
  const trays = (r) => [...r.querySelectorAll(".blockr-settings--open")].filter(vis);
  const segs = (tray) => [...tray.querySelectorAll(".dd-section--layout [role=radio]")]
    .filter((s) => LAYOUTS.includes(txt(s)));

  function read(id) {
    const r = root(id);
    const t = trays(r);
    let selected = null, table1 = false, badge = false;
    if (t.length === 1) {
      const on = segs(t[0]).filter((s) => s.getAttribute("aria-checked") === "true");
      selected = on.length === 1 ? txt(on[0]).toLowerCase() : on.map(txt).join("+") || null;
      table1 = !!t[0].querySelector(".stb-in-gear .stb-grid, .stb-in-gear > *");
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
    const s = segs(tray).find((x) => txt(x) === name);
    if (!s) return Object.assign(read(id), { clicked: false });
    s.click();
    const want = name.toLowerCase();
    const t0 = Date.now();
    while (Date.now() - t0 < max && shown(id) !== want) await sleep(150);
    await settle();
    return Object.assign(read(id), { clicked: true });
  }

  async function reopen(id) {
    await closeGear(id);
    const closed = read(id);
    await openGear(id);
    await sleep(300);
    return Object.assign(read(id), { closedOpen: closed.open });
  }

  window.blockrLayout = { read, pick, reopen, show, openGear, closeGear };
})();
