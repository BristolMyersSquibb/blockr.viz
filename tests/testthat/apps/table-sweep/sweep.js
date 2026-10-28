// Affordance sweep: clicks everything a user can see as clickable in the
// blocks on screen and records whether the click did anything. Injected by
// tests/testthat/test-table-sweep.R (and usable by hand in a browser console).
//
//   blockrSweep.run()            sweep the blocks visible now (async)
//   blockrSweep.state            {running, results, errors}
//
// A result is {block, where, what, label, effect}. effect is "ok", "DEAD"
// (looks clickable, nothing happened), "skipped" (destructive: remove / add)
// or "disabled". A block with more than one visible gear gets a "GEARS" row.
(function () {
  if (window.blockrSweep) return;

  const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
  // On screen: rendered, not hidden by CSS on any ancestor, and not pushed
  // off to the side (a closed offcanvas panel keeps its blocks rendered).
  const vis = (e) => {
    if (!e || !e.isConnected || !e.offsetParent) return false;
    if (e.checkVisibility && !e.checkVisibility({ checkVisibilityCSS: true, checkOpacity: true })) return false;
    const r = e.getBoundingClientRect();
    return r.width > 0 && r.height > 0 && r.right > 0 && r.left < window.innerWidth;
  };
  const txt = (e) =>
    ((e && (e.getAttribute("aria-label") || e.textContent || e.value)) || "")
      .replace(/\s+/g, " ").trim().slice(0, 40);

  const state = { running: false, results: [], errors: [], log: [] };

  window.addEventListener("error", (e) => state.errors.push(String(e.message)));
  const origErr = console.error;
  console.error = function (...a) {
    state.errors.push(a.map(String).join(" ").slice(0, 300));
    return origErr.apply(this, a);
  };

  // A block's card: #<board>-block_handle-<id>.
  // A block card is a .card in older blockr.dock, a .blockr-block-card on main.
  const blockRoot = (el) => el.closest(
    '.card[id*="-block_handle-"], .blockr-block-card[id*="-block_handle-"]');
  const blockId = (root) => {
    const m = root && root.id.match(/-block_handle-(.+)$/);
    return m ? m[1] : "?";
  };

  const OPEN_SEL = [
    ".blockr-select__listbox", "[role=listbox]", "[role=menu]",
    ".blockr-slot-input", ".blockr-settings--open", ".dropdown-menu.show",
    ".blockr-slot--open",
  ].join(",");
  const openCount = () => [...document.querySelectorAll(OPEN_SEL)].filter(vis).length;

  const sig = (el, scopes) =>
    [
      openCount(),
      el.isConnected,
      el.getAttribute("aria-checked"),
      el.getAttribute("aria-expanded"),
      el.checked,
      el.className && el.className.toString(),
      document.activeElement === el,
      ...scopes.filter(Boolean).map((s) => s.isConnected + ":" + s.textContent.length + ":" + s.querySelectorAll("*").length),
    ].join("|");

  const escape = async () => {
    const ae = document.activeElement;
    (ae || document).dispatchEvent(new KeyboardEvent("keydown", { key: "Escape", bubbles: true }));
    if (ae && ae.blur && ae.tagName === "INPUT") ae.blur();
    await sleep(150);
  };

  // Click and report whether the page changed around the element.
  const probe = async (el, scopes, wait = 700) => {
    const before = sig(el, scopes);
    el.dispatchEvent(new MouseEvent("mousedown", { bubbles: true }));
    el.click();
    if (el.tagName === "INPUT" && el.type === "text") el.focus();
    await sleep(wait);
    return sig(el, scopes) !== before;
  };

  const push = (r) => state.results.push(r);

  // What the block shows below its title and gear: the table or cards, the
  // legend, the cell paint. A pick in the sentence must change this, not
  // only the sentence.
  const BODY_SKIP = ".vz-head__titles, .blockr-gear-btn, .blockr-settings, .blockr-select-menu-host";
  const bodySig = (root) => {
    let out = "";
    const walk = (n) => {
      if (out.length > 60000) return;
      if (n.nodeType === 3) { out += n.nodeValue; return; }
      if (n.nodeType !== 1 || n.matches(BODY_SKIP)) return;
      const st = n.getAttribute("style");
      if (st) out += "{" + st + "}";
      // a look (compact, bar width) is often a class, not text
      const cl = n.getAttribute("class");
      if (cl) out += "<" + cl.replace(/\b(recalculating|is-hover|blockr-slot--open)\b/g, "") + ">";
      for (const c of n.childNodes) walk(c);
    };
    walk(root);
    return out;
  };

  // Until the server has answered: no recalculating output on screen. Outputs
  // of views that are not shown stay "recalculating" until they are, so only
  // visible ones count.
  const busy = () => [...document.querySelectorAll(".recalculating")].some(vis);
  const waitIdle = async (max = 30000) => {
    const t0 = Date.now();
    await sleep(800);
    while (Date.now() - t0 < max && busy()) await sleep(300);
    await sleep(400);
  };

  const openList = () => [...document.querySelectorAll(".blockr-select__listbox")].filter(vis).pop() || null;
  const options = (lb) => [...lb.querySelectorAll("[role=option]")];
  const choose = async (lb, text) => {
    const o = options(lb).find((x) => txt(x) === text);
    if (!o) return false;
    o.dispatchEvent(new MouseEvent("mousedown", { bubbles: true }));
    o.click();
    return true;
  };
  const slotsOf = (root) => [...root.querySelectorAll(".blockr-slot")].filter(vis);

  async function sweepTitles(root, id) {
    const n = slotsOf(root).length;
    for (let i = 0; i < n; i++) {
      const w = slotsOf(root)[i];
      if (!w) break;
      const label = txt(w);
      const band = w.parentElement;
      const body0 = bodySig(root);
      const opened = await probe(w, [band, root]);
      const lb = openList();
      if (!opened) {
        push({ block: id, where: "title", what: "word", label, effect: "DEAD" });
        await escape();
        continue;
      }
      if (!lb) {
        // an editor (a number typed over) or a flag that toggled
        push({ block: id, where: "title", what: "word", label, effect: "ok" });
        await escape();
        continue;
      }
      // A list: pick another value, the table must change; then put it back.
      const alt = options(lb).map(txt).find((t) => t && t !== label && t !== "(none)");
      if (!alt) {
        push({ block: id, where: "title", what: "word", label, effect: "ok" });
        await escape();
        continue;
      }
      await choose(lb, alt);
      await waitIdle();
      const body1 = bodySig(root);
      push({
        block: id, where: "title", what: "word", label,
        effect: body1 !== body0 ? "ok" : "LIES",
        note: "picked " + alt,
      });
      const back = slotsOf(root)[i];
      if (back) {
        back.click();
        await sleep(700);
        const lb2 = openList();
        if (lb2 && (await choose(lb2, label))) await waitIdle(); else await escape();
      }
    }
    // Offers ("+ Facet"): a click must open the setting.
    for (const o of [...root.querySelectorAll(".blockr-slot-offer")].filter(vis)) {
      const changed = await probe(o, [o.parentElement, root]);
      push({ block: id, where: "title", what: "offer", label: txt(o), effect: changed ? "ok" : "DEAD" });
      await escape();
    }
  }

  const gearPopover = () => {
    const p = [...document.querySelectorAll(".blockr-settings--open")].filter(vis);
    return p[p.length - 1] || null;
  };

  const ctrlKey = (c, i) => `${c.tagName}|${c.getAttribute("role")}|${c.type || ""}|${txt(c)}|${i}`;
  const CTRL_SEL = [
    "button", "[role=button]", "[role=radio]", "[role=switch]",
    "[role=checkbox]", "input", "select", "textarea",
  ].join(",");

  // The block's gear button, looked up fresh: a layout switch or a re-render
  // replaces it.
  const gearOf = (root) => [...root.querySelectorAll(".blockr-gear-btn")].filter(vis)[0] || null;

  async function openGear(root) {
    if (gearPopover()) return gearPopover();
    const g = gearOf(root);
    if (!g) return null;
    g.click();
    await sleep(700);
    return gearPopover();
  }

  // Fold sections close again when the gear re-renders: open them all.
  async function unfold(pop, id, record) {
    for (const f of [...pop.querySelectorAll(".dd-section-title--fold")].filter(vis)) {
      if (f.getAttribute("aria-expanded") === "true") continue;
      const changed = await probe(f, [pop], 300);
      if (record) push({ block: id, where: "gear", what: "section", label: txt(f), effect: changed ? "ok" : "DEAD" });
    }
  }

  // Custom checkboxes hide the native input; its label is what one clicks.
  const shown = (c) => vis(c) || (c.type === "checkbox" && vis(c.closest("label, .blockr-checkbox")));
  // The layout tiles (List, Per group, Per variable).
  const isLayoutSeg = (c) => c.getAttribute("role") === "radio" && !!c.closest(".dd-layout-tiles");
  const keyOf = (c, i) => (c.type === "checkbox" ? "checkbox|" + txt(c.closest("label, .blockr-checkbox") || c) : ctrlKey(c, i));

  // The layout tiles: each other layout must draw something else, and its gear
  // must be able to switch back.
  async function sweepLayouts(root, id) {
    let pop = await openGear(root);
    const segs = pop ? [...pop.querySelectorAll("[role=radio]")].filter((c) => vis(c) && isLayoutSeg(c)) : [];
    const cur = segs.map((s) => (s.getAttribute("aria-checked") === "true" ? txt(s) : null)).find(Boolean);
    for (const name of segs.map(txt).filter((t) => t !== cur)) {
      pop = await openGear(root);
      const s = pop && [...pop.querySelectorAll("[role=radio]")].filter(vis).find((x) => txt(x) === name && isLayoutSeg(x));
      if (!s) continue;
      if (s.disabled || s.getAttribute("aria-disabled") === "true") {
        push({ block: id, where: "gear", what: "layout", label: name, effect: "disabled" });
        continue;
      }
      const b0 = bodySig(root);
      s.click();
      await waitIdle();
      await escape();
      push({ block: id, where: "gear", what: "layout", label: name, effect: bodySig(root) !== b0 ? "ok" : "NO-EFFECT" });
      pop = await openGear(root);
      const back = pop && [...pop.querySelectorAll("[role=radio]")].filter(vis).find((x) => txt(x) === cur && isLayoutSeg(x));
      if (back) { back.click(); await waitIdle(); }
      else push({ block: id, where: "gear", what: "layout", label: name + " -> " + cur, effect: "DEAD", note: "no way back" });
      await escape();
    }
  }

  async function sweepGear(root, id) {
    let pop = await openGear(root);
    const gear = gearOf(root);
    if (!pop) {
      push({ block: id, where: "gear", what: "gear", label: txt(gear), effect: "DEAD" });
      return;
    }
    push({ block: id, where: "gear", what: "gear", label: txt(gear), effect: "ok" });
    await unfold(pop, id, true);
    const seen = new Set();
    // The grouped tray (blockr.ui's Blockr.trayNav) shows one group at a
    // time. Its index and tabs are swept once each: a click must show
    // another group. Controls are then taken group by group.
    const navSel = ".blockr-tray-nav__group, .blockr-tray-nav__tab";
    const groupNames = () => [...new Set([...(gearPopover() || document).querySelectorAll(navSel)]
      .filter(vis).map(txt))];
    const showGroup = async (name) => {
      const b = [...(gearPopover() || document).querySelectorAll(navSel)].filter(vis).find((x) => txt(x) === name);
      if (!b || b.classList.contains("is-current")) return false;
      b.click();
      await sleep(150);
      return true;
    };
    for (const name of groupNames()) {
      const p0 = gearPopover();
      const pane = p0 && p0.querySelector(".blockr-tray-nav__pane");
      const sig0 = pane ? [...pane.children].filter(vis).map(txt).join("|") : "";
      const moved = await showGroup(name);
      const sig1 = pane ? [...pane.children].filter(vis).map(txt).join("|") : "";
      if (moved) push({ block: id, where: "gear", what: "group", label: name, effect: sig1 !== sig0 ? "ok" : "DEAD" });
    }
    for (let guard = 0; guard < 200; guard++) {
      pop = await openGear(root);
      if (!pop) break;
      await unfold(pop, id, false);
      const unseen = () => {
        const ctrls = [...pop.querySelectorAll(CTRL_SEL)]
          .filter((c) => shown(c) && !isLayoutSeg(c) && !c.closest(".blockr-tray-nav__index, .blockr-tray-nav__tabs"));
        return { ctrls, keyed: ctrls.map((c, i) => [keyOf(c, i), c]) };
      };
      let { ctrls, keyed } = unseen();
      let next = keyed.find(([k]) => !seen.has(k));
      // Nothing new in the group on show: try the others, in order. A gear
      // rebuilt from scratch opens on its first group again, so every pass
      // looks through all of them.
      const groupsLeft = groupNames();
      while (!next && groupsLeft.length) {
        await showGroup(groupsLeft.shift());
        pop = gearPopover();
        if (!pop) break;
        await unfold(pop, id, false);
        ({ ctrls, keyed } = unseen());
        next = keyed.find(([k]) => !seen.has(k));
      }
      state.log.push(`${id} gear: ${ctrls.length} visible of ${pop.querySelectorAll(CTRL_SEL).length}, next ${next ? next[0] : "none"}`);
      if (!next) break;
      const [key, c] = next;
      seen.add(key);
      const label = c.type === "checkbox" ? txt(c.closest("label, .blockr-checkbox") || c) : txt(c);
      const role = c.getAttribute("role");
      const what = c.type === "checkbox" ? "checkbox" : role === "radio" ? "segment" :
        role === "combobox" || c.classList.contains("blockr-select__search") ? "select" :
        c.tagName === "INPUT" || c.tagName === "TEXTAREA" ? "input" :
        c.classList.contains("dd-section-title--fold") ? "section" : "button";
      if (c.disabled || c.getAttribute("aria-disabled") === "true") {
        push({ block: id, where: "gear", what, label, effect: "disabled" });
        continue;
      }
      if (what === "section") continue;
      if (/remove|✕|delete/i.test(label + " " + c.className) || /add-link|dd-add-trigger/.test(c.className)) {
        push({ block: id, where: "gear", what, label, effect: "skipped" });
        continue;
      }
      if (what === "segment" && c.classList.contains("is-selected")) continue;
      // Free text (a title, a prepare script) has no pick to probe.
      if ((c.tagName === "TEXTAREA" || (c.tagName === "INPUT" && (c.type === "number" || c.type === "text"))) &&
          what !== "select") {
        push({ block: id, where: "gear", what, label: label || c.placeholder || "", effect: "skipped" });
        continue;
      }
      // A native <select> does not open on a scripted click: set another
      // value, the table must follow, then set it back.
      if (c.tagName === "SELECT") {
        const opts = [...c.options].map((o) => o.value);
        const prev = c.value;
        const alt = opts.find((v) => v !== prev);
        if (!alt) { push({ block: id, where: "gear", what: "select", label, effect: "disabled" }); continue; }
        const b0 = bodySig(root);
        c.value = alt;
        c.dispatchEvent(new Event("change", { bubbles: true }));
        await waitIdle();
        push({ block: id, where: "gear", what: "select", label: label.slice(0, 30), effect: bodySig(root) !== b0 ? "ok" : "NO-EFFECT", note: "picked " + alt });
        const again = [...(gearPopover() || document).querySelectorAll("select")].find((x) => [...x.options].map((o) => o.value).join() === opts.join());
        if (again) { again.value = prev; again.dispatchEvent(new Event("change", { bubbles: true })); await waitIdle(); }
        continue;
      }
      const prevSel = what === "segment"
        ? [...c.parentElement.querySelectorAll(".is-selected")].map(txt)[0] : null;
      const b0 = bodySig(root);
      const target = c.type === "checkbox" && !vis(c) ? (c.closest("label, .blockr-checkbox") || c) : c;
      const was = c.checked;
      let changed = await probe(target, [pop, root]);
      // a checkbox that toggles in place changes no text: read the box
      if (!changed && c.type === "checkbox" && c.isConnected && c.checked !== was) changed = true;
      let effect = changed ? "ok" : "DEAD";
      // A setting has to reach the table, not only the gear.
      if (changed && (what === "segment" || what === "checkbox")) {
        await waitIdle();
        if (bodySig(root) === b0) effect = "NO-EFFECT";
      }
      push({ block: id, where: "gear", what, label, effect });
      if (what !== "segment" && what !== "checkbox") await escape();
      // put state back
      if (changed && what === "checkbox") {
        pop = await openGear(root);
        const again = pop && [...pop.querySelectorAll("input[type=checkbox]")]
          .find((x) => keyOf(x) === key);
        if (again) { (vis(again) ? again : (again.closest("label, .blockr-checkbox") || again)).click(); await waitIdle(); }
      }
      if (changed && what === "segment" && prevSel) {
        pop = await openGear(root);
        const back = pop && [...c.parentElement.isConnected ? c.parentElement.querySelectorAll("[role=radio]") : pop.querySelectorAll("[role=radio]")]
          .filter(vis).find((x) => txt(x) === prevSel);
        if (back) { back.click(); await waitIdle(); }
      }
    }
    await escape();
    if (gearPopover()) { const g = gearOf(root); if (g) { g.click(); await sleep(300); } }
    await sweepLayouts(root, id);
    await escape();
    if (gearPopover()) { const g = gearOf(root); if (g) { g.click(); await sleep(300); } }
  }

  async function run() {
    state.running = true;
    try {
      const roots = [...new Set([...document.querySelectorAll('[id*="-block_"]')].filter(vis).map(blockRoot))]
        .filter((r) => r && vis(r));
      for (const root of roots) {
        const id = blockId(root);
        const gears = [...root.querySelectorAll(".blockr-gear-btn")].filter(vis);
        if (gears.length > 1) push({ block: id, where: "block", what: "gears", label: String(gears.length), effect: "GEARS" });
        await sweepTitles(root, id);
        if (gears.length) await sweepGear(root, id);
      }
    } catch (e) {
      state.errors.push("sweep: " + (e && e.stack || e));
    }
    state.running = false;
    return state.results;
  }

  // Dock helpers: switch view / tab by its label.
  // The view menu's items switch the view (the hidden .blockr-view-tab
  // buttons only mark it). A view's blocks render on first show: wait until
  // no block in it is still empty, up to `wait` ms.
  async function view(name, wait = 30000) {
    const it = [...document.querySelectorAll(".blockr-view-item")].find((x) => txt(x.querySelector(".blockr-view-item-name") || x) === name);
    if (!it) return false;
    (it.querySelector(".blockr-view-item-name") || it).click();
    await waitIdle(wait);
    return true;
  }
  const tabs = () => [...document.querySelectorAll(".dv-tab")].filter(vis).map(txt);
  async function tab(name) {
    const t = [...document.querySelectorAll(".dv-tab")].filter(vis).find((x) => txt(x) === name);
    if (t) { t.dispatchEvent(new MouseEvent("pointerdown", { bubbles: true })); t.click(); await sleep(2000); }
    return !!t;
  }
  const views = () => [...document.querySelectorAll(".blockr-view-item .blockr-view-item-name")].map(txt);

  // Whole board: every view, every tab, then the sweep. Results carry the view.
  async function all(only) {
    state.running = true;
    state.results = [];
    for (const v of views().filter((x) => !only || only.includes(x))) {
      await view(v);
      for (const t of tabs()) {
        await tab(t);
        const n0 = state.results.length;
        await run();
        state.running = true;
        for (const r of state.results.slice(n0)) { r.view = v; r.tab = t; }
      }
    }
    // a block shown in two tabs is swept twice: keep the first row per key
    const k = (r) => [r.block, r.where, r.what, r.label].join("|");
    const seen = new Set();
    state.results = state.results.filter((r) => (seen.has(k(r)) ? false : (seen.add(k(r)), true)));
    state.running = false;
    return state.results;
  }

  window.blockrSweep = { run, all, view, tab, tabs, views, state };
})();
