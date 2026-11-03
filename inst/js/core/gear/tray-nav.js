// @ts-check
/**
 * Blockr.trayNav, the groups of a long gear tray, until blockr.ui ships it.
 * Copied from blockr.ui branch feat/tray-nav (708ef52); the CSS is at the end
 * of inst/css/gear.css. Delete both once a blockr.ui release has trayNav: a
 * blockr.ui that has one keeps its own, as this only fills the gap.
 *
 * The tray's sections are sorted into a few groups and one group shows at a
 * time. A tray 560px and wider gets an index on the left: the groups, and
 * under the open one its sections. A narrower tray gets the groups as tabs
 * above the sections. The sections are moved into the navigation's pane. A
 * group with one section hides that section's title. A click on a section in
 * the index brings it into view and marks it briefly.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {{ trayNav?: unknown }} */ (root.Blockr = root.Blockr || {});

  let n = 0;

  /** @param {string} cls @param {string} [tag] */
  function make(cls, tag) {
    const el = document.createElement(tag || 'div');
    el.className = cls;
    return el;
  }

  /** @param {string} cls @param {string} text @param {() => void} onClick */
  function button(cls, text, onClick) {
    const b = /** @type {HTMLButtonElement} */ (make(cls, 'button'));
    b.type = 'button';
    b.textContent = text;
    b.addEventListener('click', onClick);
    return b;
  }

  /** @param {HTMLElement} el */
  function scrollTo(el) {
    if (typeof el.scrollIntoView !== 'function') return;
    const still = typeof window.matchMedia === 'function' &&
      window.matchMedia('(prefers-reduced-motion: reduce)').matches;
    el.scrollIntoView({ block: 'start', behavior: still ? 'auto' : 'smooth' });
  }

  // A section picked in the index moves to the top of the view, as far as
  // the page can scroll, and lights up for a moment, so the eye finds it
  // even where it could not move.
  /** @param {HTMLElement} sec */
  function point(sec) {
    scrollTo(sec);
    sec.classList.remove('blockr-tray-nav__flash');
    void sec.offsetWidth;   // restart the animation
    sec.classList.add('blockr-tray-nav__flash');
    // Off once played, or the group shown again would replay it.
    sec.addEventListener('animationend', function () {
      sec.classList.remove('blockr-tray-nav__flash');
    }, { once: true });
  }

  /**
   * @typedef {{ el: HTMLElement, title: string, titleEl?: HTMLElement | null }} NavSection
   * @typedef {{ key: string, title: string, sections: NavSection[] }} NavGroup
   */

  /**
   * @param {NavGroup[]} groups
   * @param {{ current?: string, onPick?: (key: string) => void, label?: string }} [opts]
   */
  function trayNav(groups, opts) {
    const o = opts || {};
    const list = groups.filter((g) => g.sections.length);
    let current = list.some((g) => g.key === o.current)
      ? /** @type {string} */ (o.current) : (list[0] ? list[0].key : '');
    const id = 'blockr-tray-nav-' + (++n);
    const label = o.label || 'Settings groups';

    const el = make('blockr-tray-nav');
    const index = make('blockr-tray-nav__index', 'nav');
    index.setAttribute('aria-label', label);
    const body = make('blockr-tray-nav__body');
    const tabs = make('blockr-tray-nav__tabs');
    tabs.setAttribute('role', 'tablist');
    tabs.setAttribute('aria-label', label);
    const pane = make('blockr-tray-nav__pane');
    pane.id = id + '-pane';
    pane.setAttribute('role', 'tabpanel');
    body.append(tabs, pane);
    el.append(index, body);

    /** @type {Record<string, HTMLButtonElement>} */
    const tabBtns = {};
    for (const g of list) {
      const one = g.sections.length === 1;
      for (const s of g.sections) {
        s.el.dataset.trayGroup = g.key;
        if (one && s.titleEl) s.titleEl.classList.add('blockr-tray-nav__hidden-title');
        pane.appendChild(s.el);
      }
      const t = button('blockr-tray-nav__tab', g.title, () => pick(g.key));
      t.id = id + '-tab-' + g.key;
      t.setAttribute('role', 'tab');
      t.setAttribute('aria-controls', pane.id);
      tabs.appendChild(t);
      tabBtns[g.key] = t;
    }

    // Arrow keys move between the tabs, as in any tab list; the index is a
    // list of buttons and takes Tab.
    tabs.addEventListener('keydown', (e) => {
      const keys = list.map((g) => g.key);
      const i = keys.indexOf(current);
      let next = e.key === 'ArrowRight' ? i + 1 : e.key === 'ArrowLeft' ? i - 1 :
        e.key === 'Home' ? 0 : e.key === 'End' ? keys.length - 1 : null;
      if (next === null) return;
      e.preventDefault();
      next = (next + keys.length) % keys.length;
      pick(keys[next]);
      tabBtns[keys[next]].focus();
    });

    /** @param {string} key */
    function pick(key) {
      if (key === current) return;
      current = key;
      draw();
      // A group opens at its top: when the pane's top has scrolled out of
      // view, it comes back.
      if (pane.getBoundingClientRect().top < 0) scrollTo(pane);
      if (o.onPick) o.onPick(key);
    }

    function draw() {
      index.innerHTML = '';
      for (const g of list) {
        const on = g.key === current;
        const b = button('blockr-tray-nav__group' + (on ? ' is-current' : ''), g.title,
          () => pick(g.key));
        if (on) b.setAttribute('aria-current', 'true');
        index.appendChild(b);
        if (on && g.sections.length > 1) {
          for (const s of g.sections) {
            if (s.title) index.appendChild(button('blockr-tray-nav__section', s.title, () => point(s.el)));
          }
        }
        tabBtns[g.key].classList.toggle('is-current', on);
        tabBtns[g.key].setAttribute('aria-selected', on ? 'true' : 'false');
        tabBtns[g.key].tabIndex = on ? 0 : -1;
        for (const s of g.sections) s.el.classList.toggle('blockr-tray-nav__off', !on);
      }
      pane.setAttribute('aria-labelledby', id + '-tab-' + current);
    }
    draw();

    return { el, show: (/** @type {string} */ key) => pick(key), current: () => current };
  }

  B.trayNav = B.trayNav || trayNav;
})();
