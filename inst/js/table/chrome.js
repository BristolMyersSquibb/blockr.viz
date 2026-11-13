// @ts-check
/**
 * The table block's frame, the same for every layout: the block frame
 * (core/frame.js) with a search box among the tools and the scroll box the
 * layout draws into. The search and download stay hidden until there is a
 * table to search or download. In a narrow block the tools take their own
 * row (table-view.css).
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  /** Wait this long after the last key before searching. */
  const SEARCH_MS = 150;

  /** Can table.css move the header with a scroll timeline? */
  const FOLLOW = typeof CSS !== 'undefined' && typeof CSS.supports === 'function' &&
    CSS.supports('animation-timeline', '--a');

  /**
   * @param {string} tag
   * @param {string} cls
   * @param {HTMLElement} [parent]
   */
  function el(tag, cls, parent) {
    const e = document.createElement(tag);
    e.className = cls;
    if (parent) parent.appendChild(e);
    return e;
  }

  class Frame extends V.Frame {
    /**
     * @param {HTMLElement} host The output element; the frame fills it.
     * @param {VizScope} scope
     */
    constructor(host, scope) {
      super(host, scope, { cls: 'vz-table' });
      this.searchInput = /** @type {HTMLInputElement} */ (el('input', 'vz-table-search'));
      this.searchInput.type = 'search';
      this.searchInput.placeholder = 'Search…';
      this.searchInput.setAttribute('aria-label', 'Search table');
      this.tools.insertBefore(this.searchInput, this.downloadSlot);
      this.box = el('div', 'blockr-table-wrapper', this.body);
      this.box.style.overflow = 'auto';
      /** @type {Array<(q: string) => void>} */
      this._search = [];
      const run = scope.debounce(() => this._search.forEach((f) => f(this.query())), SEARCH_MS);
      scope.on(this.searchInput, 'input', run);
      // Where the panel around the frame was scrolled to, and the frame's
      // height, as the browser reports them: a layout switch reads neither
      // off the page, which would restyle the old layout's rows.
      /** @type {{ sc: Element, top: number } | null} */
      this._scrolled = null;
      scope.on(document, 'scroll', (e) => {
        const t = e.target === document ? document.scrollingElement : /** @type {Element} */ (e.target);
        if (t && t !== this.box && t.contains(this.el)) this._scrolled = { sc: t, top: t.scrollTop };
      }, { capture: true, passive: true });
      this.height = 0;
      scope.resize(this.el, (entries) => {
        const r = entries[entries.length - 1];
        const b = r.borderBoxSize && r.borderBoxSize[0];
        this.height = b ? b.blockSize : r.contentRect.height;
      });
      this.showTools({ search: false, download: false });
    }

    /** The search text, trimmed and lower case. */
    query() {
      return this.searchInput.hidden ? '' : this.searchInput.value.trim().toLowerCase();
    }

    /**
     * @param {(q: string) => void} fn Called after a pause in typing.
     * @returns {() => void} Stops calling it; a layout's scope owns it, so a
     *   layout switch lets go of the old view.
     */
    onSearch(fn) {
      this._search.push(fn);
      return () => {
        const k = this._search.indexOf(fn);
        if (k >= 0) this._search.splice(k, 1);
      };
    }

    /**
     * Search and download, each only when on and there is a table. `onOpen`
     * runs when the reader reaches for the download.
     * @param {{ search: boolean, download: boolean, downloads?: VizDownload[] | null,
     *           onOpen?: () => void }} o
     */
    showTools(o) {
      const was = !this.searchInput.hidden;
      this.searchInput.hidden = !o.search;
      if (was && !o.search && this.searchInput.value) {
        this.searchInput.value = '';
        this._search.forEach((f) => f(''));
      }
      this.downloads(o.download ? o.downloads : null, o.onOpen);
    }

    /**
     * The panel's scroll as its last scroll event left it, while the frame
     * is still in that panel; null when it has not scrolled.
     * @returns {{ sc: Element, top: number } | null}
     */
    scrollPlace() {
      const s = this._scrolled;
      return s && s.sc.isConnected && s.sc.contains(this.el) ? s : null;
    }

    /**
     * The nearest ancestor that scrolls up and down: a dock panel, the page.
     * One that only scrolls sideways computes overflow-y to auto as well, so
     * the style alone is not enough.
     * @returns {Element | null}
     */
    scroller() {
      for (let n = this.el.parentElement; n; n = n.parentElement) {
        const oy = getComputedStyle(n).overflowY;
        if ((oy === 'auto' || oy === 'scroll') && n.scrollHeight > n.clientHeight) return n;
      }
      return null;
    }

    /**
     * Keeps the header in view while the rows scroll. A box with a height
     * limit (interactive_table()'s `max_height`, the data table under a
     * view) scrolls itself, and table.css makes the header sticky in it.
     * Otherwise the table runs its full length and the panel around it
     * scrolls; the box still scrolls sideways, so a sticky header would
     * stick to the box and scroll away with the panel. Here the header moves
     * down by as much of the table as has scrolled out of view, all its rows
     * together, and stops at the last row. A scroll-driven animation in
     * table.css moves it in step with the scroll; this gives it the
     * table's measures, which change on a draw or a resize, not on a scroll.
     * A browser without scroll timelines gets the move from here, a frame
     * or more behind the scroll.
     */
    followHeader() {
      const box = this.box;
      const thead = /** @type {HTMLElement | null} */ (box.querySelector('table.blockr-table > thead'));
      const table = thead && thead.parentElement;
      if (!thead || !table) return;
      if (this.boxScrolls()) {
        box.classList.remove('vz-follow');
        thead.style.transform = '';
        box.classList.toggle('scrolled', box.scrollTop > 0);
        return;
      }
      const sp = this.scroller();
      const top = sp ? sp.getBoundingClientRect().top : 0;
      // offsetHeight, not a rect: a rect would include the transform.
      const max = table.offsetHeight - thead.offsetHeight;
      const tableTop = table.getBoundingClientRect().top;
      const y = Math.max(0, Math.min(top - tableTop, max));
      box.classList.toggle('scrolled', y > 0);
      if (FOLLOW) {
        const at = Math.round(tableTop - box.getBoundingClientRect().top) + 'px';
        const mx = Math.max(0, max) + 'px';
        const st = box.style;
        if (st.getPropertyValue('--vz-follow-at') !== at) st.setProperty('--vz-follow-at', at);
        if (st.getPropertyValue('--vz-follow-max') !== mx) st.setProperty('--vz-follow-max', mx);
        box.classList.add('vz-follow');
        return;
      }
      thead.style.transform = y > 0 ? 'translateY(' + y + 'px)' : '';
    }

    /** Does the box scroll up and down itself (a height limit)? */
    boxScrolls() {
      const b = this.box;
      return b.scrollTop > 0 || b.scrollHeight > b.clientHeight + 1;
    }

    /**
     * The part of the box in view, in px from its top, for the row window
     * (dom.js): the box's own scroll, or the part of the panel around it
     * (or of the page) that the box covers. 0.2 windowed rows only inside
     * the table block's own box; here the panel scrolls most tables.
     * @returns {{ top: number, height: number }}
     */
    inView() {
      const b = this.box;
      if (this.boxScrolls()) return { top: b.scrollTop, height: b.clientHeight || 600 };
      const sc = this.scroller();
      const r = sc ? sc.getBoundingClientRect() : { top: 0, height: root.innerHeight || 0 };
      return { top: Math.max(0, r.top - b.getBoundingClientRect().top), height: r.height || 600 };
    }

    /** @param {boolean} on A table with sections or keyed rows. */
    structured(on) {
      this.el.classList.toggle('vz-table-structured', on);
    }
  }

  V.table.Frame = Frame;
})();
