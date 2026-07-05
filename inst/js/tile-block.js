// @ts-check
/**
 * tile-block.js — gear-popover config + drill for new_tile_block(). The R side
 * (tile_html) emits the tk-* markup with values and fill widths already at
 * their final state (no animation — numbers should read instantly). This
 * script only:
 *   - builds the gear popover via the shared Blockr.DrilldownConfig engine,
 *   - wires a card / matrix-row click to a categorical filter on the group.
 *
 * Mirrors table.js (scan + MutationObserver init, body-portaled
 * popover keyed by the ns()-based elem id, so two tiles never collide).
 */
(function () {
  // Shared aggregation vocabulary (group/value/func roles + AGG_FNS) — the
  // identical control the chart / table use. Must load before this script.
  var DAgg = /** @type {VizDrilldownAgg} */ (
    (typeof Blockr !== "undefined" && Blockr.DrilldownAgg) || window.DrilldownAgg);

  // Emit the filter-clear action (chart parity: _sendClearFilter). The R
  // side's filter branch reads the null column/values as "clear" and resets
  // the filter reactiveVals, so downstream recovers.
  /** @param {string} elemId */
  function sendClearFilter(elemId) {
    if (!window.Shiny || !Shiny.setInputValue) return;
    Shiny.setInputValue(elemId + '_action', {
      action: 'filter', column: null, values: null, filter_type: 'categorical'
    }, { priority: 'event' });
  }

  // ---- drill: card / row click -> categorical filter on the group ----------
  /** @param {Element} root */
  function wireDrill(root) {
    if (root.getAttribute('data-tk-drill') !== '1') return;
    var elemIdAttr = root.getAttribute('data-tk-elem-id');
    var col = root.getAttribute('data-tk-group');
    if (!elemIdAttr || !col) return;
    // Rebound after the guard so the closures below see a plain string.
    const elemId = elemIdAttr;
    root.addEventListener('click', function (e) {
      var tgt = /** @type {Element | null} */ (e.target);
      var hit = tgt && tgt.closest('[data-group]');
      if (!hit || !root.contains(hit)) return;
      var val = hit.getAttribute('data-group');
      if (val == null || val === '') return;
      // Click-to-toggle (chart parity): re-clicking the active card / row
      // clears the filter and the highlight.
      if (hit.classList.contains('tk-active')) {
        hit.classList.remove('tk-active');
        sendClearFilter(elemId);
        return;
      }
      if (window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue(elemId + '_action', {
          action: 'filter', column: col, values: [val], filter_type: 'categorical'
        }, { priority: 'event' });
      }
      root.querySelectorAll('.tk-active').forEach(function (n) {
        n.classList.remove('tk-active');
      });
      hit.classList.add('tk-active');
    });
  }

  // Active-filter indication (restore + server re-render): the R renderer
  // stamps the active drill value(s) on the wrapper (data-tk-active, a JSON
  // array) and renders the status footer with its Reset control; mark the
  // matching card(s)/row(s) and delegate Reset clicks. The tile re-renders
  // per filter change, so this runs against fresh markup every time.
  /** @param {Element} root */
  function wireActive(root) {
    var elemIdAttr = root.getAttribute('data-tk-elem-id');
    if (!elemIdAttr) return;
    // Rebound after the guard so the closure sees a plain string.
    const elemId = elemIdAttr;
    root.addEventListener('click', function (e) {
      var t = /** @type {Element | null} */ (e.target);
      if (!t || !t.closest('.dd-status-reset')) return;
      root.querySelectorAll('.tk-active').forEach(function (n) {
        n.classList.remove('tk-active');
      });
      sendClearFilter(elemId);
    });
    var activeJson = root.getAttribute('data-tk-active');
    if (!activeJson) return;
    /** @type {any} */
    var vals = null;
    try { vals = JSON.parse(activeJson); } catch (err) { vals = null; }
    if (!vals || !vals.length) return;
    root.querySelectorAll('[data-group]').forEach(function (n) {
      if (vals.indexOf(n.getAttribute('data-group')) !== -1) {
        n.classList.add('tk-active');
      }
    });
  }

  // ---- gear popover via the shared DrilldownConfig engine ------------------
  // Roles: the shared aggregation triple (group / value / func, single-group
  // like the chart — a KPI clusters by one dimension) plus the tile's display
  // roles. `group` comes from DAgg.aggRoles(); the old standalone `by` role is
  // gone (renamed to the aggregation `group`).
  var TILE_ROLES = Object.assign({}, DAgg.aggRoles({ multiple: false }), {
    value:     { label: 'Value',     kind: 'column', colType: 'num' },
    // "Name" = the column naming each KPI (long input: one row per KPI).
    // The name shows above the value. There is deliberately NO separate
    // overline role: two pickers fed one visual slot with an invisible
    // fallback (overline defaulted to the name) and read as duplicates —
    // the ctor arg still works for saved boards, it just has no gear control.
    name:      { label: 'Name',      kind: 'column', colType: 'cat',
                 ph: 'column naming each KPI…' },
    secondary: { label: 'Secondary', kind: 'column', colType: 'any' },
    caption:   { label: 'Caption',   kind: 'column', colType: 'any' },
    style:     { label: 'Secondary style', kind: 'select',
                 options: ['plain', 'delta', 'fill', 'pill'] },
    // No good_when role: polarity is always "up" (an increase reads good) —
    // Christoph cut the control; the ctor arg is legacy-ignored.
    format:    { label: 'Number format', kind: 'select',
                 options: ['number', 'compact', 'percent'] },
    unit:      { label: 'Unit', kind: 'text', ph: 'e.g. USD, CHF, apples' },
    layout:    { label: 'Layout', kind: 'segmented',
                 options: [{ value: 'cards', label: 'Cards' }, { value: 'table', label: 'Table' }] }
  });
  // A FUNCTION of the config (mirrors table.js): the display roles map under
  // "Mapping"; the Aggregation checkbox section (group + repeatable summaries)
  // renders separately via aggregatable/summaries. hasCols is false only for an
  // empty input, where the column pickers have nothing to offer.
  /** @param {Record<string, any>} cfg @param {boolean} hasCols */
  function tileSections(cfg, hasCols) {
    return {
      requiredMap: hasCols ? ['value'] : [],
      // `group` is a MAPPING for the tile — it clusters precomputed cards /
      // drives the matrix rows even with no aggregation (and doubles as the
      // group_by column when summaries aggregate). It therefore lives under
      // Mapping, NOT inside the Aggregation section (the table's group IS
      // its aggregation; the tile's isn't).
      optionalMap: hasCols ? ['group', 'name', 'secondary', 'caption'] : [],
      mapping: [],
      summaries: hasCols,        // repeatable "[agg] of [cols]" list
      aggregatable: hasCols,   // Variant A: Aggregation checkbox section
      // Aggregation is active iff METRICS are set; the checkbox must not
      // seed from (or clear) the clustering group.
      aggSeedsFromGroup: false,
      // Empty cols on a numeric aggregation = all numeric columns (the
      // dd_metric_plan override rule — the tile shares the table's R path).
      metricsDefaultAll: true,
      // COLOR — plain section, "Color by" only (a tile has no cell matrix to
      // shade). Like the drill, the tint keys on what a card structurally IS
      // (its group level / its Name value), so the section shows only when
      // the tile has such a column; the picker offers exactly those (see the
      // per-instance `color` role built in buildCogwheel).
      colorSection: (cfg && (cfg.group || cfg.name))
        ? { colorKey: 'color', shadings: false } : null,
      // Value formatting is presentation, not data mapping. "Secondary
      // style" only applies when a secondary is mapped — hidden otherwise
      // (removing the Secondary role re-renders the popover, so the list
      // re-evaluates live).
      presentation: (cfg && cfg.secondary ? ['style'] : [])
        .concat(['format', 'unit', 'layout'])
    };
  }

  /** @param {string | null} elemId @param {string} param @param {*} value */
  function sendConfig(elemId, param, value) {
    if (!elemId || !window.Shiny || !Shiny.setInputValue) return;
    Shiny.setInputValue(elemId + '_action',
      { action: 'config', param: param, value: value }, { priority: 'event' });
  }

  // renderUI rebuilds the whole tile (settings band included) on every config
  // edit; remember the band's open state per element id so it survives.
  /** @type {Record<string, boolean>} */
  var bandOpen = {};

  /** @param {Element} root */
  function buildCogwheel(root) {
    var elemIdAttr = root.getAttribute('data-tk-elem-id');
    if (!elemIdAttr) return;
    // Rebound after the guard so the closures below see a plain string.
    const elemId = elemIdAttr;

    /** @type {VizColumn[]} */
    var cols = [];
    try { cols = JSON.parse(root.getAttribute('data-tk-cols') || '[]'); }
    catch (e) { cols = []; }
    /** @type {Record<string, any>} */
    var cfg;
    try { cfg = JSON.parse(root.getAttribute('data-tk-config') || '{}'); }
    catch (e) { cfg = {}; }
    // The engine's drill section is value-based ('' off / 'auto' on); the tile
    // always filters on the group column, so map the boolean to 'auto'/''.
    cfg.drill = cfg.drill ? 'auto' : '';
    // The repeatable aggregation list rides on its own attribute (func a
    // scalar, cols an array), parsed into cfg.summaries for the engine.
    try { cfg.summaries = JSON.parse(root.getAttribute('data-tk-summaries') || '[]'); }
    catch (e) { cfg.summaries = []; }

    // Per-instance roles: the "Color by" picker offers exactly the tile's
    // STRUCTURAL columns (its group, its Name column) — a card's tint keys
    // on what the card IS, like the drill. Computed at build time; a group /
    // Name change re-renders the whole tile, which rebuilds this.
    var colorOpts = [{ value: '', label: '(none)' }];
    if (cfg.group) colorOpts.push(cfg.group);
    if (cfg.name && cfg.name !== cfg.group) colorOpts.push(cfg.name);
    var roles = Object.assign({}, TILE_ROLES, {
      color: { label: 'Color by', kind: 'select', options: colorOpts }
    });

    var header = document.createElement('div');
    header.className = 'blockr-gear-header';
    var btn = document.createElement('button');
    btn.type = 'button';
    btn.className = 'blockr-gear-btn';
    btn.title = 'Tile settings';
    btn.setAttribute('aria-label', 'Tile settings');
    btn.setAttribute('aria-haspopup', 'dialog');
    btn.setAttribute('aria-expanded', 'false');
    btn.innerHTML = (typeof Blockr !== 'undefined' && Blockr.icons)
      ? Blockr.icons.gear : '⚙';
    header.appendChild(btn);

    // In-flow settings band (design-system pilot — blockr.ui/dev/
    // gear-panel-proposals.html, variant B): inside the tile below the gear
    // header, no <body> portal, no fixed positioning.
    var wasOpen = !!bandOpen[elemId];

    // --beak: gear connector T1 (settings-band.css) — the open band grows a
    // notch pointing at the gear that opened it.
    var pop = document.createElement('div');
    pop.className = 'blockr-settings blockr-settings--beak dd-popover';
    pop.setAttribute('data-dd-pop-for', elemId);

    var DDC = (typeof Blockr !== 'undefined' && Blockr.DrilldownConfig) ||
      window.DrilldownConfig;
    if (!DDC) { root.insertBefore(header, root.firstChild); return; }

    new DDC({
      popoverEl: function () { return pop; },
      roles: roles,
      config: function () { return cfg; },
      columns: function () { return cols; },
      context: function () { return 'all'; },
      currentType: function () { return null; },
      sections: function () { return tileSections(cfg, cols.length > 0); },
      sectionsForFamily: function () { return tileSections(cfg, cols.length > 0); },
      // Paired-tail roles (func behind value) render inside their partner's
      // row, never standalone — mirror the chart / table.
      secondary: new Set(Object.keys(TILE_ROLES)
        .map(function (k) { return TILE_ROLES[k].pairedWith; })
        .filter(Boolean)),
      // No mappingTitle override: the display roles keep the "Mapping" header
      // and the aggregation checkbox section gets "Aggregation" (its default).
      typeKey: null,
      typeGroups: null,
      familyFor: null,
      entryRequired: function (/** @type {string} */ role) { return role === 'value'; },
      // Picker-less drill section: the tile's drill target is structurally
      // determined (the group column when grouped, the Name column on an
      // ungrouped KPI list — data-tk-drill-col, computed in R). No target
      // (bare KPI, grand totals) -> null -> the section is hidden entirely.
      drillHint: function () {
        var col = root.getAttribute('data-tk-drill-col') || '';
        if (!col) return null;
        return 'Clicking a card or row filters downstream on ' + col + '.';
      },
      // Repeatable aggregation list: one "[agg] of [cols]" row per value. A
      // single count is the floor, so an empty list surfaces as one count row.
      metricsList: function () {
        return (cfg.summaries && cfg.summaries.length)
          ? cfg.summaries : [{ func: 'count', cols: [] }];
      },
      onMetricsChange: function (/** @type {any[]} */ ms) {
        cfg.summaries = ms;
        sendConfig(elemId, 'summaries', JSON.stringify(ms));
      },
      title: 'Tile settings',
      onChange: function (key) {
        var v = (key === 'drill') ? (cfg.drill !== '' && cfg.drill != null) : cfg[key];
        sendConfig(elemId, key, v);
      },
      onMults: function () {},
      // Engine hook: a drill section-uncheck / re-aim must drop the emitted
      // filter (or downstream stays filtered forever with clicks inert).
      onClearFilter: function () {
        root.querySelectorAll('.tk-active').forEach(function (n) {
          n.classList.remove('tk-active');
        });
        sendClearFilter(elemId);
      },
      ensureDefaults: function () {},
      afterTypeChange: function () {},
      isOpen: function () { return pop.classList.contains('blockr-settings--open'); },
      reopen: function () { openPop(); }
    }).render();

    // The band is in flow: opening is a class toggle — no positioning, no
    // scroll/resize listeners, no outside-click dismissal (it is a panel,
    // not a menu; the gear is the only toggle).
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

    root.insertBefore(header, root.firstChild);
    root.insertBefore(pop, header.nextSibling);
    // Each config edit re-renders the tile from the server and rebuilds this
    // band closed; restore the remembered open state.
    if (wasOpen) openPop();
  }

  /** @param {Element} root */
  function init(root) {
    if (!root || root.getAttribute('data-tk-initialized') === '1') return;
    root.setAttribute('data-tk-initialized', '1');
    buildCogwheel(root);
    wireDrill(root);
    wireActive(root);
  }

  /** @param {Document | Element} [ctx] */
  function scan(ctx) {
    var nodes = (ctx || document)
      .querySelectorAll('.tk-block[data-tk-elem-id]:not([data-tk-initialized])');
    Array.prototype.forEach.call(nodes, init);
  }

  var SCAN_SEL = '.tk-block[data-tk-elem-id]';

  // Wire exactly the part of the DOM an event touched: the enclosing tile if
  // the node sits inside one, else any tiles within the subtree. init() is
  // idempotent (data-tk-initialized), so overlapping paths never re-wire.
  // Same pattern as table.js.
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

  // Primary wiring path: shiny:value / shiny:bound fire on the output element
  // that just (re)rendered — wire only that subtree (deferred past Shiny's
  // DOM swap; shiny:value fires before the new HTML is applied).
  if (typeof window.jQuery === 'function') {
    jQuery(document).on('shiny:value shiny:bound', function (/** @type {any} */ e) {
      var t = e.target;
      setTimeout(function () { scanAround(t); }, 0);
    });
  }

  // Fallback for tiles that enter the DOM outside a Shiny render event:
  // queue relevant added nodes and wire those subtrees, coalesced to one
  // flush per animation frame. Irrelevant churn (tooltips, dock relayout)
  // never queues anything.
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
