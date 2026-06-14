/**
 * tile-block.js — gear-popover config + drill for new_tile_block(). The R side
 * (tile_html) emits the tk-* markup with values and fill widths already at
 * their final state (no animation — numbers should read instantly). This
 * script only:
 *   - builds the gear popover via the shared Blockr.DrilldownConfig engine,
 *   - wires a card / matrix-row click to a categorical filter on the group.
 *
 * Mirrors drilldown-table.js (scan + MutationObserver init, body-portaled
 * popover keyed by the ns()-based elem id, so two tiles never collide).
 */
(function () {
  // ---- drill: card / row click -> categorical filter on the group ----------
  function wireDrill(root) {
    if (root.getAttribute('data-tk-drill') !== '1') return;
    var elemId = root.getAttribute('data-tk-elem-id');
    var col = root.getAttribute('data-tk-group');
    if (!elemId || !col) return;
    root.addEventListener('click', function (e) {
      var hit = e.target.closest('[data-group]');
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

  function sendConfig(elemId, param, value) {
    if (!elemId || !window.Shiny || !Shiny.setInputValue) return;
    Shiny.setInputValue(elemId + '_action',
      { action: 'config', param: param, value: value }, { priority: 'event' });
  }

  function buildCogwheel(root) {
    var elemId = root.getAttribute('data-tk-elem-id');
    if (!elemId) return;

    var cols = [];
    try { cols = JSON.parse(root.getAttribute('data-tk-cols') || '[]'); }
    catch (e) { cols = []; }
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

    var esc = (window.CSS && CSS.escape) ? CSS.escape(elemId) : elemId;
    var staleP = document.querySelector('.dd-popover[data-dd-pop-for="' + esc + '"]');
    // renderUI rebuilds the whole tile (this popover included) on every config
    // edit; remember whether the prior instance was open so we can restore it.
    var wasOpen = !!(staleP && staleP.style.display === 'block');
    if (staleP && staleP.parentNode) staleP.parentNode.removeChild(staleP);

    var pop = document.createElement('div');
    pop.className = 'blockr-popover dd-popover';
    pop.setAttribute('data-dd-pop-for', elemId);
    pop.style.display = 'none';

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
      entryRequired: function (role) { return role === 'value'; },
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
      isOpen: function () { return pop.style.display === 'block'; },
      reopen: function () { openPop(); }
    }).render();

    function positionPop() {
      var g = btn.getBoundingClientRect();
      var vw = window.innerWidth, vh = window.innerHeight;
      pop.style.position = 'fixed';
      pop.style.right = 'auto';
      pop.style.maxHeight = (vh - 16) + 'px';
      var pw = pop.offsetWidth, ph = pop.offsetHeight;
      var left = Math.min(g.right, vw - 8) - pw;
      left = Math.max(8, Math.min(left, vw - pw - 8));
      var top = g.bottom + 6;
      if (top + ph > vh - 8) top = Math.max(8, vh - 8 - ph);
      pop.style.left = left + 'px';
      pop.style.top = top + 'px';
      pop.style.maxHeight = (vh - top - 8) + 'px';
    }
    var reposition = function () { if (pop.style.display === 'block') positionPop(); };
    function openPop() {
      pop.style.display = 'block';
      btn.classList.add('blockr-gear-active');
      btn.setAttribute('aria-expanded', 'true');
      positionPop();
      requestAnimationFrame(positionPop);
      window.addEventListener('scroll', reposition, true);
      window.addEventListener('resize', reposition);
    }
    function closePop() {
      pop.style.display = 'none';
      btn.classList.remove('blockr-gear-active');
      btn.setAttribute('aria-expanded', 'false');
      window.removeEventListener('scroll', reposition, true);
      window.removeEventListener('resize', reposition);
    }
    btn.addEventListener('click', function (e) {
      e.stopPropagation();
      if (pop.style.display === 'block') closePop(); else openPop();
    });
    // Decide on mousedown, not click: a Blockr.Select dropdown is portaled to
    // <body> (outside this popover) and tears itself down on the option click,
    // so by click time e.target is already detached and the dropdown ancestor
    // is gone. At mousedown the dropdown is still attached, so the exclusion
    // below holds and picking a value no longer dismisses the settings form.
    document.addEventListener('mousedown', function (e) {
      if (pop.style.display !== 'block') return;
      var t = e.target;
      if (pop.contains(t) || btn.contains(t)) return;
      if (t.closest && t.closest('.blockr-select__dropdown')) return;
      closePop();
    });

    document.body.appendChild(pop);
    root.insertBefore(header, root.firstChild);
    // The form must stay open until the user clicks away, but each config edit
    // re-renders the tile from the server and rebuilds this popover closed.
    // Restore the open state captured from the prior instance above.
    if (wasOpen) openPop();
  }

  function init(root) {
    if (!root || root.getAttribute('data-tk-initialized') === '1') return;
    root.setAttribute('data-tk-initialized', '1');
    buildCogwheel(root);
    wireDrill(root);
  }

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

  if (window.jQuery) {
    jQuery(document).on('shiny:value shiny:bound', function () {
      setTimeout(scan, 0);
    });
  }
})();
