// @ts-check
/**
 * The gear's sections: which spec arguments go into which section, the
 * order of the sections, and how they are laid out in the tray.
 *
 * A section is a spec argument's `gear.group`. Sections fall into the tray's
 * groups (Data, Sort, Format, Titles, Interaction), so a user meets the same
 * groups in a chart and in a table. With blockr.ui's `Blockr.trayNav` one
 * group shows at a time; without it the sections stand in one column.
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
    'Look': ['format', 0], 'Color': ['format', 5], 'Numbers': ['format', 10],
    'Bars': ['format', 20], 'Marks': ['format', 30], 'Reference lines': ['format', 40],
    'Counts': ['format', 50], 'Facets': ['format', 60],
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
    const order = GROUPS.map((x) => x[0]);
    return Array.from(out.values())
      .filter((s) => s.keys.length || s.adds.length)
      .sort((a, b) => order.indexOf(a.group) - order.indexOf(b.group) || a.ord - b.ord);
  }

  /**
   * An empty section: a title over a body.
   * @param {string} title
   */
  function sectionEl(title) {
    const el = document.createElement('div');
    el.className = 'dd-section';
    el.setAttribute('data-section', title);
    const titleEl = document.createElement('div');
    titleEl.className = 'dd-section-title';
    titleEl.textContent = title;
    const body = document.createElement('div');
    body.className = 'dd-section-body';
    el.append(titleEl, body);
    return { el, titleEl, body };
  }

  /**
   * Put the built sections into the tray: grouped through Blockr.trayNav
   * when blockr.ui has it, else one after the other.
   * @param {HTMLElement} pop
   * @param {{ plan: VizSectionPlan, el: HTMLElement, titleEl: HTMLElement }[]} built
   * @param {string | null} current The group to show.
   * @param {(g: string) => void} onPick
   * @param {string} label
   */
  function assemble(pop, built, current, onPick, label) {
    const ui = /** @type {VizUi} */ (root.Blockr);
    if (typeof ui.trayNav !== 'function') {
      for (const b of built) pop.appendChild(b.el);
      return;
    }
    const groups = GROUPS.map(([key, title]) => ({
      key, title,
      sections: built.filter((b) => b.plan.group === key)
        .map((b) => ({ el: b.el, title: b.plan.title, titleEl: b.titleEl }))
    }));
    const nav = ui.trayNav(groups, { current: current || undefined, onPick, label });
    pop.appendChild(nav.el);
  }

  V.gear.sections = { GROUPS, plan, sectionEl, assemble, applies };
})();
