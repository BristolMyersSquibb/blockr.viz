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
    var slider = /** @type {HTMLInputElement|null} */ (root.querySelector('.hmb-topn'));
    var badge = root.querySelector('.hmb-topn-badge');
    if (slider) {
      // Live badge while dragging; the (re-rendering) config send only on
      // release, so a drag is one render, not thirty.
      slider.addEventListener('input', function () {
        if (badge) badge.textContent = slider.value;
      });
      slider.addEventListener('change', function () {
        sendConfig(elemId, 'top_n', parseInt(slider.value, 10));
      });
    }
    var nums = /** @type {HTMLInputElement|null} */ (root.querySelector('.hmb-nums'));
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
      search.addEventListener('input', function () {
        var q = search.value.trim().toLowerCase();
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
      });
    }
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
    // Restore / server re-render: mark the active row(s).
    var activeJson = root.getAttribute('data-hmb-active');
    if (activeJson) {
      /** @type {any} */
      var vals = null;
      try { vals = JSON.parse(activeJson); } catch (err) { vals = null; }
      if (vals && vals.length) {
        root.querySelectorAll('tr.hmb-r').forEach(function (tr) {
          if (vals.indexOf(tr.getAttribute('data-hmb-id')) !== -1) {
            tr.classList.add('hmb-active');
          }
        });
      }
    }
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

  /** @param {Element} root */
  function init(root) {
    if (!root || root.getAttribute('data-hmb-initialized') === '1') return;
    root.setAttribute('data-hmb-initialized', '1');
    var elemId = root.getAttribute('data-hmb-elem-id');
    if (!elemId) return;
    buildCogwheel(root, elemId);
    wireToolbar(root, elemId);
    wireDrill(root, elemId);
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
