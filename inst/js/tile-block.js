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
  // ---- drill: card / row click -> categorical filter on the group ----------
  /** @param {Element} root */
  function wireDrill(root) {
    if (root.getAttribute('data-tk-drill') !== '1') return;
    var elemId = root.getAttribute('data-tk-elem-id');
    var col = root.getAttribute('data-tk-group');
    if (!elemId || !col) return;
    root.addEventListener('click', function (e) {
      var tgt = /** @type {Element | null} */ (e.target);
      var hit = tgt && tgt.closest('[data-group]');
      if (!hit || !root.contains(hit)) return;
      var val = hit.getAttribute('data-group');
      if (val == null || val === '') return;
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

  // ---- gear popover via the shared DrilldownConfig engine ------------------
  var TILE_ROLES = {
    value:     { label: 'Value',     kind: 'column', colType: 'num' },
    by:        { label: 'Group',     kind: 'column', colType: 'cat' },
    measure:   { label: 'Measure',   kind: 'column', colType: 'cat' },
    secondary: { label: 'Secondary', kind: 'column', colType: 'any' },
    overline:  { label: 'Overline',  kind: 'column', colType: 'any' },
    caption:   { label: 'Caption',   kind: 'column', colType: 'any' },
    style:     { label: 'Secondary style', kind: 'select',
                 options: ['plain', 'delta', 'fill', 'pill'] },
    good_when: { label: 'Good when', kind: 'segmented',
                 options: [{ value: 'up', label: 'Up' }, { value: 'down', label: 'Down' }] },
    format:    { label: 'Number format', kind: 'select',
                 options: ['number', 'compact', 'percent'] },
    unit:      { label: 'Unit', kind: 'text', ph: 'e.g. USD, CHF, apples' },
    layout:    { label: 'Layout', kind: 'segmented',
                 options: [{ value: 'cards', label: 'Cards' }, { value: 'table', label: 'Table' }] }
  };
  var TILE_SECTIONS = {
    requiredMap: ['value'],
    optionalMap: ['by', 'measure', 'secondary', 'overline', 'caption'],
    mapping: [],
    // Value formatting (style / good-when / number format / unit) is presentation,
    // not data mapping — it lives alongside layout under the Presentation header.
    presentation: ['style', 'good_when', 'format', 'unit', 'layout']
  };

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

    var pop = document.createElement('div');
    pop.className = 'blockr-settings dd-popover';
    pop.setAttribute('data-dd-pop-for', elemId);

    var DDC = (typeof Blockr !== 'undefined' && Blockr.DrilldownConfig) ||
      window.DrilldownConfig;
    if (!DDC) { root.insertBefore(header, root.firstChild); return; }

    new DDC({
      popoverEl: function () { return pop; },
      roles: TILE_ROLES,
      config: function () { return cfg; },
      columns: function () { return cols; },
      context: function () { return 'all'; },
      currentType: function () { return null; },
      sections: function () { return TILE_SECTIONS; },
      sectionsForFamily: function () { return TILE_SECTIONS; },
      secondary: new Set(),
      typeKey: null,
      typeGroups: null,
      familyFor: null,
      entryRequired: function (/** @type {string} */ role) { return role === 'value'; },
      drillAutoLabel: function () {
        return cfg.by ? ('each ' + cfg.by) : 'the group';
      },
      title: 'Tile settings',
      onChange: function (key) {
        var v = (key === 'drill') ? (cfg.drill !== '' && cfg.drill != null) : cfg[key];
        sendConfig(elemId, key, v);
      },
      onMults: function () {},
      onClearFilter: function () {},
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
  }

  /** @param {Document | Element} [ctx] */
  function scan(ctx) {
    var nodes = (ctx || document)
      .querySelectorAll('.tk-block[data-tk-elem-id]:not([data-tk-initialized])');
    Array.prototype.forEach.call(nodes, init);
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', function () { scan(); });
  } else {
    scan();
  }

  var mo = new MutationObserver(function (muts) {
    for (var i = 0; i < muts.length; i++) {
      if (muts[i].addedNodes && muts[i].addedNodes.length) { scan(); break; }
    }
  });
  mo.observe(document.documentElement, { childList: true, subtree: true });

  if (typeof window.jQuery === 'function') {
    jQuery(document).on('shiny:value shiny:bound', function () {
      setTimeout(scan, 0);
    });
  }
})();
