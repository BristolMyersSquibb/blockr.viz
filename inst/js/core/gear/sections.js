// @ts-check
/**
 * The gear's sections: which spec arguments go into which section, the
 * order of the sections, and how they are laid out in the tray.
 *
 * A section is a spec argument's `gear.group`. Sections fall into the tray's
 * groups (Data, Sort, Format, Titles, Interaction), so a user meets the same
 * groups in a chart and in a table. blockr.ui's `Blockr.trayNav` shows one
 * group at a time.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  V.gear = V.gear || /** @type {VizGearNamespace} */ ({});

  /** @type {[string, string][]} */
  const GROUPS = [
    ['data', 'Data'], ['sort', 'Sort'], ['format', 'Format'],
    ['titles', 'Titles'], ['interact', 'Interaction']
  ];

  /** Each section's tray group and its place there. A section not listed
   * goes to Format, after the listed ones, in spec order. */
  /** @type {Record<string, [string, number]>} */
  const SECTIONS = {
    'Chart type': ['data', 0], 'Table': ['data', 0], 'Columns': ['data', 10],
    'Variables': ['data', 15], 'Mapping': ['data', 20], 'Prepare script': ['data', 90],
    'Order': ['sort', 0], 'Rows shown': ['sort', 10],
    'Look': ['format', 0], 'Numbers': ['format', 10], 'Bars': ['format', 20],
    'Marks': ['format', 30], 'Reference lines': ['format', 40],
    'Titles': ['titles', 0],
    'Reader tools': ['interact', 0], 'Drill-down': ['interact', 10]
  };

  /** Optional mappings: shown in Mapping once set, else offered in its Add
   * menu. */
  const ADD = 'Add mapping';

  /**
   * Does the argument mean something for this chart type or table layout?
   * @param {VizArg} arg @param {unknown} type
   */
  const applies = (arg, type) => {
    const a = arg.applies;
    if (!a || type == null) return true;
    const t = String(type);
    if (a.only) return a.only.includes(t);
    if (a.except) return !a.except.includes(t);
    return true;
  };

  /**
   * The sections to draw, in tray order.
   * @param {VizSpec} spec
   * @param {(key: string) => boolean} shown Whether a row is visible now.
   * @param {(key: string) => boolean} isSet Whether an optional mapping is
   *   shown as a row (set, or added in this tray) rather than in the menu.
   * @returns {VizSectionPlan[]}
   */
  function plan(spec, shown, isSet) {
    /** @type {Map<string, VizSectionPlan>} */
    const out = new Map();
    let extra = 100;
    const section = (/** @type {string} */ title) => {
      let s = out.get(title);
      if (!s) {
        const [group, ord] = SECTIONS[title] || ['format', extra++];
        s = { title, group, ord, keys: [], adds: [] };
        out.set(title, s);
      }
      return s;
    };
    for (const [key, arg] of Object.entries(spec)) {
      const g = arg.gear;
      if (!g || !shown(key)) continue;
      if (g.group === ADD) {
        const s = section('Mapping');
        if (isSet(key)) s.keys.push(key);
        else s.adds.push(key);
      } else {
        section(g.group).keys.push(key);
      }
    }
    // A two-cell text field goes last in its section, so it leaves no hole
    // in the row before it.
    const late = (/** @type {string} */ k) => spec[k].type.name === 'named';
    for (const s of out.values()) {
      s.keys = s.keys.filter((k) => !late(k)).concat(s.keys.filter(late));
    }
    const order = GROUPS.map((x) => x[0]);
    return Array.from(out.values())
      .filter((s) => s.keys.length || s.adds.length)
      .sort((a, b) => order.indexOf(a.group) - order.indexOf(b.group) || a.ord - b.ord);
  }

  /**
   * An empty section: its title, then its rows as the section's own
   * children, which the stylesheet lays out in a grid (gear.css,
   * `.vz-section`). `body` is the section itself.
   * @param {string} title
   */
  function sectionEl(title) {
    const el = document.createElement('div');
    el.className = 'vz-section' + (title === 'Titles' ? ' vz-section--titles' : '');
    el.setAttribute('data-section', title);
    const titleEl = document.createElement('div');
    titleEl.className = 'vz-section-title';
    titleEl.textContent = title;
    el.appendChild(titleEl);
    return { el, titleEl, body: el };
  }

  /**
   * Put the built sections into the tray, grouped through blockr.ui's
   * Blockr.trayNav. The index lists the groups only: a section goes in
   * without a title, which trayNav leaves out of the index. Its header in
   * the pane stays.
   * @param {HTMLElement} pop
   * @param {{ plan: VizSectionPlan, el: HTMLElement, titleEl: HTMLElement }[]} built
   * @param {string | null} current The group to show.
   * @param {(g: string) => void} onPick
   * @param {string} label
   */
  function assemble(pop, built, current, onPick, label) {
    const ui = /** @type {VizUi} */ (root.Blockr);
    const groups = GROUPS.map(([key, title]) => ({
      key, title,
      sections: built.filter((b) => b.plan.group === key)
        .map((b) => ({ el: b.el, title: '', titleEl: b.titleEl }))
    }));
    if (typeof ui.trayNav !== 'function') {
      pop.appendChild(plainList(groups));
      return;
    }
    const nav = ui.trayNav(groups, { current: current || undefined, onPick, label });
    pop.appendChild(nav.el);
  }

  let warned = false;

  /**
   * The tray without blockr.ui's trayNav (an older blockr.ui): every group
   * under its own heading, one after the other. Said once in the console.
   * @param {{ key: string, title: string, sections: { el: HTMLElement }[] }[]} groups
   */
  function plainList(groups) {
    if (!warned) {
      warned = true;
      console.warn('blockr.viz: this blockr.ui has no Blockr.trayNav; the gear ' +
        'shows its groups as a plain list. Update blockr.ui for the index.');
    }
    const list = document.createElement('div');
    list.className = 'vz-tray-groups';
    for (const g of groups) {
      if (!g.sections.length) continue;
      const head = document.createElement('div');
      head.className = 'vz-tray-group-title';
      head.textContent = g.title;
      list.appendChild(head);
      for (const s of g.sections) list.appendChild(s.el);
    }
    return list;
  }

  V.gear.sections = { GROUPS, plan, sectionEl, assemble, applies };
})();
