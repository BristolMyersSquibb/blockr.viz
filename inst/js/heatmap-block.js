// @ts-check
/**
 * heatmap-block.js — wiring for new_heatmap_block(). The R side
 * (heatmap_html) emits the hmb-* markup fully rendered; this script only:
 *   - wires the toolbar (Top-n slider, cell-numbers toggle, search),
 *   - wires the row-click drill (+ active restore, Reset),
 *   - builds the gear band via the shared Blockr.DrilldownConfig engine.
 * Mirrors tile-block.js (scan + MutationObserver init, idempotent per root).
 */
(function () {
  /** @param {string} elemId @param {string} param @param {*} value */
  function sendConfig(elemId, param, value) {
    if (!elemId || !window.Shiny || !Shiny.setInputValue) return;
    Shiny.setInputValue(elemId + '_action',
      { action: 'config', param: param, value: value }, { priority: 'event' });
  }
  /** @param {string} elemId */
  function sendClearFilter(elemId) {
    if (!window.Shiny || !Shiny.setInputValue) return;
    Shiny.setInputValue(elemId + '_action', {
      action: 'filter', column: null, values: null
    }, { priority: 'event' });
  }

  // ---- toolbar --------------------------------------------------------
  /** @param {Element} root @param {string} elemId */
  function wireToolbar(root, elemId) {
    // Top n: a number field committing on Enter / blur (Blockr.textCommit,
    // the shared control -- it also owns the "Enter ↵" chip). Per-keystroke
    // would re-render the whole matrix on the way to "25".
    var topn = /** @type {HTMLInputElement|null} */ (root.querySelector('.hmb-topn'));
    if (topn) {
      // Read live, never captured: the toolbar is wired once and the body
      // payload rewrites `max` every time the frame changes.
      var maxOf = function () {
        return parseInt(topn.getAttribute('max') || '', 10);
      };
      var lastN = parseInt(topn.value, 10);
      /** @type {any} */
      var commit = null;
      /** @param {string} raw */
      var apply = function (raw) {
        var n = parseInt(raw, 10);
        if (!isFinite(n)) {
          if (commit) commit.sync(String(lastN));
          return;
        }
        // Clamp rather than reject: "400" on a 230-term frame means "all of
        // them", which is a reasonable thing to type.
        var maxN = maxOf();
        n = Math.max(1, isFinite(maxN) ? Math.min(n, maxN) : n);
        if (commit && String(n) !== raw) commit.sync(String(n));
        if (n === lastN) return;
        lastN = n;
        sendConfig(elemId, 'top_n', n);
      };
      if (typeof Blockr !== 'undefined' && Blockr.textCommit) {
        commit = Blockr.textCommit(topn, {
          onCommit: function (v) { apply(v); }
        });
      } else {
        // No shared helper (a page without blockr-core.js): same
        // Enter/blur contract by hand, rather than per-keystroke.
        topn.addEventListener('change', function () { apply(topn.value); });
      }
    }

    var nums = /** @type {HTMLInputElement|null} */ (
      root.querySelector('.hmb-nums input'));
    if (nums) {
      nums.addEventListener('change', function () {
        // Instant visual (CSS class), then persist -- the server render
        // comes back in the same state, no flicker.
        root.classList.toggle('hmb-nonum', !nums.checked);
        sendConfig(elemId, 'cell_numbers', nums.checked);
      });
    }
    var search = /** @type {HTMLInputElement|null} */ (root.querySelector('.hmb-search'));
    if (search) {
      search.addEventListener('input', function () { applySearch(root); });
    }
  }

  // The row filter, re-applied after every body swap -- the rows are new
  // nodes, so a search typed before the data changed would otherwise show
  // everything again.
  /** @param {Element} root */
  function applySearch(root) {
    var search = /** @type {HTMLInputElement|null} */ (
      root.querySelector('.hmb-search'));
    var q = search ? search.value.trim().toLowerCase() : '';
    // Hiding rows breaks the rail rowspans visually, so an active
    // search hides the rail column wholesale (hmb-searching).
    root.classList.toggle('hmb-searching', q !== '');
    root.querySelectorAll('tr.hmb-r').forEach(function (tr) {
      var id = (tr.getAttribute('data-hmb-id') || '').toLowerCase();
      (/** @type {HTMLElement} */ (tr)).style.display =
        (q === '' || id.indexOf(q) !== -1) ? '' : 'none';
    });
    root.querySelectorAll('tr.hmb-gsep').forEach(function (tr) {
      (/** @type {HTMLElement} */ (tr)).style.display = q === '' ? '' : 'none';
    });
  }

  // ---- drill ----------------------------------------------------------
  /** @param {Element} root @param {string} elemId */
  function wireDrill(root, elemId) {
    root.addEventListener('click', function (e) {
      var t = /** @type {Element|null} */ (e.target);
      if (t && t.closest('.hmb-reset')) {
        root.querySelectorAll('tr.hmb-active').forEach(function (n) {
          n.classList.remove('hmb-active');
        });
        sendClearFilter(elemId);
        return;
      }
      if (root.getAttribute('data-hmb-drill') !== '1') return;
      var tr = t && t.closest('tr.hmb-r');
      if (!tr || !root.contains(tr)) return;
      var id = tr.getAttribute('data-hmb-id');
      var col = root.getAttribute('data-hmb-row-col');
      if (!id || !col) return;
      if (tr.classList.contains('hmb-active')) {
        tr.classList.remove('hmb-active');
        sendClearFilter(elemId);
        return;
      }
      root.querySelectorAll('tr.hmb-active').forEach(function (n) {
        n.classList.remove('hmb-active');
      });
      tr.classList.add('hmb-active');
      if (window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue(elemId + '_action', {
          action: 'filter', column: col, values: [id]
        }, { priority: 'event' });
      }
    });
    markActive(root);
  }

  // Mark the drilled row(s) from `data-hmb-active`. Runs on wire AND after
  // every body swap: a restore, or a data refresh under a live filter, ships
  // fresh <tr>s that have never carried the class.
  /** @param {Element} root */
  function markActive(root) {
    var activeJson = root.getAttribute('data-hmb-active');
    /** @type {any} */
    var vals = null;
    if (activeJson) {
      try { vals = JSON.parse(activeJson); } catch (err) { vals = null; }
    }
    if (!vals || !vals.length) return;
    root.querySelectorAll('tr.hmb-r').forEach(function (tr) {
      if (vals.indexOf(tr.getAttribute('data-hmb-id')) !== -1) {
        tr.classList.add('hmb-active');
      }
    });
  }

  // ---- gear band via the shared DrilldownConfig engine ------------------
  var HM_ROLES = {
    row:   { label: 'Row',      kind: 'column', colType: 'any' },
    col:   { label: 'Column',   kind: 'column', colType: 'any' },
    color: { label: 'Color by', kind: 'column', colType: 'any' },
    group: { label: 'Group by', kind: 'column', colType: 'cat' },
    download: { label: 'Download', kind: 'segmented',
                options: [{ value: 'on', label: 'Download' },
                          { value: 'off', label: 'No download' }] }
  };
  /** @param {boolean} hasCols */
  function hmSections(hasCols) {
    return {
      requiredMap: hasCols ? ['row', 'col'] : [],
      optionalMap: hasCols ? ['color', 'group'] : [],
      mapping: [],
      summaries: false,
      aggregatable: false,
      colorSection: null,
      ctrlSection: true,
      presentation: ['download'],
      titles: []
    };
  }

  /** @type {Record<string, boolean>} */
  var bandOpen = {};

  /** @param {Element} root @param {string} elemId */
  function buildCogwheel(root, elemId) {
    /** @type {any[]} */
    var cols = [];
    try { cols = JSON.parse(root.getAttribute('data-hmb-cols') || '[]'); }
    catch (e) { cols = []; }
    /** @type {Record<string, any>} */
    var cfg;
    try { cfg = JSON.parse(root.getAttribute('data-hmb-config') || '{}'); }
    catch (e) { cfg = {}; }
    cfg.drill = cfg.drill ? 'auto' : '';

    var header = document.createElement('div');
    header.className = 'blockr-gear-header';
    var btn = document.createElement('button');
    btn.type = 'button';
    btn.className = 'blockr-gear-btn';
    btn.title = 'Heatmap settings';
    btn.setAttribute('aria-label', 'Heatmap settings');
    btn.setAttribute('aria-haspopup', 'dialog');
    btn.setAttribute('aria-expanded', 'false');
    btn.innerHTML = (typeof Blockr !== 'undefined' && Blockr.icons)
      ? Blockr.icons.gear : '⚙';
    header.appendChild(btn);

    var wasOpen = !!bandOpen[elemId];
    var pop = document.createElement('div');
    pop.className = 'blockr-settings blockr-settings--beak dd-popover';
    pop.setAttribute('data-dd-pop-for', elemId);

    var DDC = (typeof Blockr !== 'undefined' && Blockr.DrilldownConfig) ||
      window.DrilldownConfig;
    if (!DDC) {
      // The toolbar carries the gear slot; without the engine there is no
      // band, but the block still works.
      var bar0 = root.querySelector('.hmb-toolbar');
      if (bar0) bar0.appendChild(header); else root.insertBefore(header, root.firstChild);
      return;
    }

    new DDC({
      popoverEl: function () { return pop; },
      roles: HM_ROLES,
      config: function () { return cfg; },
      columns: function () { return cols; },
      context: function () { return 'all'; },
      currentType: function () { return null; },
      sections: function () { return hmSections(cols.length > 0); },
      sectionsForFamily: function () { return hmSections(cols.length > 0); },
      secondary: new Set(),
      typeKey: null,
      typeGroups: null,
      familyFor: null,
      entryRequired: function (/** @type {string} */ role) {
        return role === 'row' || role === 'col';
      },
      drillHint: function () {
        return cfg.row
          ? 'Clicking a row filters downstream on ' + cfg.row + '.'
          : null;
      },
      metricsList: function () { return []; },
      onMetricsChange: function () {},
      title: 'Heatmap settings',
      onChange: function (/** @type {string} */ key) {
        var v = (key === 'drill')
          ? (cfg.drill !== '' && cfg.drill != null)
          : cfg[key];
        sendConfig(elemId, key, v);
      },
      onMults: function () {},
      onClearFilter: function () {
        root.querySelectorAll('tr.hmb-active').forEach(function (n) {
          n.classList.remove('hmb-active');
        });
        sendClearFilter(elemId);
      },
      ensureDefaults: function () {},
      afterTypeChange: function () {},
      isOpen: function () { return pop.classList.contains('blockr-settings--open'); },
      reopen: function () { openPop(); }
    }).render();

    function openPop() {
      pop.classList.add('blockr-settings--open');
      btn.classList.add('blockr-gear-active');
      btn.setAttribute('aria-expanded', 'true');
      bandOpen[elemId] = true;
    }
    function closePop() {
      pop.classList.remove('blockr-settings--open');
      btn.classList.remove('blockr-gear-active');
      btn.setAttribute('aria-expanded', 'false');
      bandOpen[elemId] = false;
    }
    btn.addEventListener('click', function (e) {
      e.stopPropagation();
      if (pop.classList.contains('blockr-settings--open')) closePop(); else openPop();
    });

    // The gear sits at the END of the toolbar row (search, then gear), so
    // "Top n" and "Cell numbers" read as block controls LEFT of it.
    var bar = root.querySelector('.hmb-toolbar');
    if (bar) {
      bar.appendChild(header);
      root.insertBefore(pop, bar.nextSibling);
    } else {
      root.insertBefore(header, root.firstChild);
      root.insertBefore(pop, header.nextSibling);
    }
    if (wasOpen) openPop();
  }

  // ---- body payloads ---------------------------------------------------
  // A PERSISTENT store, not a one-shot queue (the table and rank blocks'
  // shape): a payload that arrives before its chrome exists waits here, and
  // a chrome re-created later -- dock panel re-mount, view switch -- paints
  // from the store with no R round trip.
  /** @type {Record<string, any>} */
  var store = {};
  /** @type {Record<string, string>} */
  var gearBuiltWith = {};

  // ---- body assembly ---------------------------------------------------
  // The client half of the cell model (R/heatmap-html.R): R sends the
  // <table> shell with its rotated header and a sparse model, this pastes
  // the rows. The matrix is ~90% empty, so shipping the model instead of
  // the HTML is ~157 KB -> ~7 KB on a 194 x 25 AE heatmap.
  //
  // The markup MUST match hmb_assemble_rows() byte for byte: same classes,
  // same attribute order, same escaping (& < > escaped, quotes not, which
  // is htmltools' own rule for text and non-attribute content).
  /** @param {string} x */
  function esc(x) {
    return String(x).replace(/&/g, '&amp;').replace(/</g, '&lt;')
      .replace(/>/g, '&gt;');
  }

  /** @param {any} m @returns {string} */
  function assembleRows(m) {
    var n = m.n, k = m.k, i, j;
    // Column-major, the layout R's matrix already has: idx %% n is the row.
    var cells = new Array(n * k);
    for (i = 0; i < n * k; i++) cells[i] = '<td class="hmb-c"></td>';
    for (j = 0; j < m.idx.length; j++) {
      var slot = m.pal[j] - 1;
      cells[m.idx[j]] = '<td class="hmb-c" style="background:' + m.bg[slot] +
        ';color:' + m.fg[slot] + '"><span>' + m.cnt[j] + '</span></td>';
    }

    // Rail runs and the group separators that follow each group's last row.
    /** @type {Record<number, string>} */
    var railAt = {};
    /** @type {Record<number, boolean>} */
    var sepAt = {};
    var groups = m.groups || [];
    var ncols = k + 2;
    var at = 0;
    for (i = 0; i < groups.length; i++) {
      railAt[at] = '<td class="hmb-rail" rowspan="' + groups[i].n +
        '" title="' + esc(groups[i].label) + ' \u00b7 ' + groups[i].n +
        ' rows"><span>' + esc(groups[i].label) + '</span></td>';
      at += groups[i].n;
      if (i < groups.length - 1) sepAt[at - 1] = true;
    }

    var out = new Array(n);
    for (i = 0; i < n; i++) {
      var id = esc(m.rows[i]);
      var row = '<tr class="hmb-r" data-hmb-id="' + id +
        '"><td class="hmb-stub" data-raw="' + id + '">' + id + '</td>';
      for (j = 0; j < k; j++) row += cells[j * n + i];
      if (railAt[i]) row += railAt[i];
      if (sepAt[i]) {
        row += '</tr><tr class="hmb-gsep"><td colspan="' + ncols +
          '"></td>';
      }
      out[i] = row + '</tr>';
    }
    return out.join('');
  }

  /** @param {Element} root @param {any} p */
  function applyPayload(root, p) {
    if (!p) return;
    var elemId = root.getAttribute('data-hmb-elem-id') || '';

    // Root state the gear and the drill read back off the DOM.
    if (p.cols != null) root.setAttribute('data-hmb-cols', p.cols);
    if (p.config != null) root.setAttribute('data-hmb-config', p.config);
    root.setAttribute('data-hmb-drill', p.drill ? '1' : '0');
    root.setAttribute('data-hmb-row-col', p.rowCol || '');
    var active = p.active || [];
    if (active.length) {
      root.setAttribute('data-hmb-active', JSON.stringify(active));
    } else {
      root.removeAttribute('data-hmb-active');
    }

    // Cell numbers: the checkbox is applied instantly on click, so this only
    // corrects it when the server disagrees (a restore, a gear edit).
    var nums = /** @type {HTMLInputElement|null} */ (
      root.querySelector('.hmb-nums input'));
    if (nums && nums.checked !== !!p.cellNumbers) nums.checked = !!p.cellNumbers;
    root.classList.toggle('hmb-nonum', !p.cellNumbers);

    // Top n: the frame decides the ceiling. Never fight a field being typed
    // into -- the commit that follows will bring the server round anyway.
    var topn = /** @type {HTMLInputElement|null} */ (
      root.querySelector('.hmb-topn'));
    if (topn) {
      if (p.topMax) {
        topn.setAttribute('max', String(p.topMax));
        topn.title = 'Columns shown, most frequent first (1-' + p.topMax +
          '). Enter to apply.';
      }
      if (p.topVal && document.activeElement !== topn) {
        topn.value = String(p.topVal);
      }
    }

    var legend = root.querySelector('.hmb-legend-slot');
    var scroll = root.querySelector('.hmb-scroll');
    var count = root.querySelector('.hmb-count');
    if (p.err) {
      if (legend) legend.innerHTML = '';
      if (scroll) scroll.innerHTML = '<div class="hmb-empty"></div>';
      var empty = scroll && scroll.firstChild;
      if (empty) empty.textContent = p.err;
      if (count) count.textContent = '';
    } else {
      if (legend) legend.innerHTML = p.legend || '';
      if (scroll) {
        scroll.innerHTML = p.head || '';
        var tbody = scroll.querySelector('tbody');
        if (tbody && p.model) tbody.innerHTML = assembleRows(p.model);
      }
      if (count) count.textContent = p.count || '';
      markActive(root);
      applySearch(root);
    }

    // The gear is built from the columns and the config; rebuild it only
    // when one of those actually changed, so a plain data refresh leaves an
    // open band alone.
    var key = (p.cols || '') + '|' + (p.config || '');
    if (elemId && gearBuiltWith[elemId] !== key) {
      gearBuiltWith[elemId] = key;
      var oldHeader = root.querySelector('.hmb-toolbar .blockr-gear-header');
      if (oldHeader && oldHeader.parentNode) {
        oldHeader.parentNode.removeChild(oldHeader);
      }
      var oldPop = root.querySelector('[data-dd-pop-for="' + elemId + '"]');
      if (oldPop && oldPop.parentNode) oldPop.parentNode.removeChild(oldPop);
      buildCogwheel(root, elemId);
    }
  }

  /** @param {Element} root */
  function init(root) {
    if (!root || root.getAttribute('data-hmb-initialized') === '1') return;
    root.setAttribute('data-hmb-initialized', '1');
    var elemId = root.getAttribute('data-hmb-elem-id');
    if (!elemId) return;
    buildCogwheel(root, elemId);
    gearBuiltWith[elemId] = (root.getAttribute('data-hmb-cols') || '') + '|' +
      (root.getAttribute('data-hmb-config') || '');
    wireToolbar(root, elemId);
    wireDrill(root, elemId);
    if (store[elemId]) {
      applyPayload(root, store[elemId]);
    } else if (window.Shiny && Shiny.setInputValue) {
      // Nothing to paint: tell the server this chrome is up. Shiny DROPS a
      // custom message with no handler registered, and this script only
      // loads with the first heatmap in the page -- on a board that opens on
      // a view without one, the startup payload would be lost.
      Shiny.setInputValue(elemId + '_ready', Date.now(), { priority: 'event' });
    }
  }

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler('blockr-viz-heatmap-data',
      function (/** @type {any} */ msg) {
        if (!msg || !msg.id) return;
        /** @type {any} */
        var p = null;
        try { p = JSON.parse(msg.payload); } catch (e) { return; }
        store[msg.id] = p;
        var root = document.querySelector(
          '.hmb-block[data-hmb-elem-id="' + msg.id + '"]');
        if (root) applyPayload(root, p);
      });
  }

  var SCAN_SEL = '.hmb-block[data-hmb-elem-id]';
  /** @param {Document | Element} [ctx] */
  function scan(ctx) {
    var nodes = (ctx || document)
      .querySelectorAll(SCAN_SEL + ':not([data-hmb-initialized])');
    Array.prototype.forEach.call(nodes, init);
  }
  /** @param {EventTarget | Element | null | undefined} el */
  function scanAround(el) {
    var e = /** @type {Element | null} */ (
      el && /** @type {Node} */ (el).nodeType === 1 ? el : null);
    if (!e) { scan(); return; }
    var root = e.closest(SCAN_SEL);
    if (root) init(root);
    else scan(e);
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', function () { scan(); });
  } else {
    scan();
  }
  if (typeof window.jQuery === 'function') {
    jQuery(document).on('shiny:value shiny:bound', function (/** @type {any} */ e) {
      var t = e.target;
      setTimeout(function () { scanAround(t); }, 0);
    });
  }
  /** @type {Element[]} */
  var pendingNodes = [];
  var flushScheduled = false;
  /** @param {Element} n */
  function queueWire(n) {
    pendingNodes.push(n);
    if (flushScheduled) return;
    flushScheduled = true;
    window.requestAnimationFrame(function () {
      flushScheduled = false;
      var nodes = pendingNodes;
      pendingNodes = [];
      nodes.forEach(scanAround);
    });
  }
  var mo = new MutationObserver(function (muts) {
    for (var i = 0; i < muts.length; i++) {
      var added = muts[i].addedNodes;
      for (var j = 0; j < added.length; j++) {
        var n = /** @type {Element} */ (added[j]);
        if (n.nodeType !== 1) continue;
        if (n.matches(SCAN_SEL) || n.querySelector(SCAN_SEL)) queueWire(n);
      }
    }
  });
  mo.observe(document.documentElement, { childList: true, subtree: true });
})();
