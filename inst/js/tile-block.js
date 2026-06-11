/**
 * tile-block.js — Shiny input binding for click-through pill groups.
 *
 * Replaces shiny::radioButtons / checkboxGroupInput for the tile block so
 * we don't have to reskin Bootstrap form controls. Markup:
 *
 *   <div class="tb-pill-group" data-select="single|multi" id="inputId">
 *     <button class="tb-pill" data-value="x">Label</button>
 *     ...
 *   </div>
 *
 * Value: character for single-select, character vector for multi.
 */
(() => {
  'use strict';

  const binding = new Shiny.InputBinding();

  Object.assign(binding, {
    find(scope) {
      return $(scope).find('.tb-pill-group');
    },

    getId(el) {
      return el.id;
    },

    getValue(el) {
      const multi = el.dataset.select === 'multi';
      const active = el.querySelectorAll('.tb-pill.tb-pill--active');
      const vals = Array.from(active).map((p) => p.dataset.value);
      return multi ? vals : (vals[0] ?? null);
    },

    setValue(el, value) {
      const multi = el.dataset.select === 'multi';
      const want = multi ? (Array.isArray(value) ? value : [value]).filter(Boolean)
                         : (value == null ? [] : [value]);
      el.querySelectorAll('.tb-pill').forEach((p) => {
        p.classList.toggle('tb-pill--active', want.includes(p.dataset.value));
      });
    },

    subscribe(el, callback) {
      el.addEventListener('click.tbPill', () => {});
      el.addEventListener('click', (e) => {
        const pill = e.target.closest('.tb-pill');
        if (!pill || !el.contains(pill)) return;
        const multi = el.dataset.select === 'multi';
        if (multi) {
          pill.classList.toggle('tb-pill--active');
        } else {
          el.querySelectorAll('.tb-pill').forEach((p) =>
            p.classList.toggle('tb-pill--active', p === pill)
          );
        }
        // Propagate showcase pill to the settings wrapper so per-showcase
        // CSS rules can hide/show aesthetic rows.
        if (el.classList.contains('tb-showcase-picker')) {
          const settings = el.closest('.tile-block-settings');
          if (settings) settings.dataset.showcase = pill.dataset.value;
        }
        callback(true);
      });
    },

    unsubscribe(el) {
      el.replaceWith(el.cloneNode(true));
    },

    receiveMessage(el, data) {
      if (data && 'value' in data) this.setValue(el, data.value);
    },

    getRatePolicy() {
      return { policy: 'debounce', delay: 100 };
    }
  });

  Shiny.inputBindings.register(binding, 'blockr.bi.tbPillGroup');

  // Server signals which tile panels have facet-eligible columns; we
  // toggle a class so the Facets section can fade when there's nothing
  // useful to map there.
  Shiny.addCustomMessageHandler('blockr-bi-tile-flags', function(msg) {
    if (!msg || !msg.ns_id) return;
    const settings = document.getElementById(msg.ns_id);
    if (!settings) return;
    settings.classList.toggle('tb-no-facets', !msg.has_categoricals);
  });

  // Server tells us the active template changed; sync the data-template
  // attr on the settings wrapper (CSS uses it to show/hide rows).
  Shiny.addCustomMessageHandler('blockr-bi-tile-template', function(msg) {
    if (!msg || !msg.ns_id || !msg.template) return;
    const settings = document.getElementById(msg.ns_id);
    if (!settings) return;
    settings.dataset.template = msg.template;
  });

  // Gear button → toggle popover. Plain DOM listener attached on first
  // pointerdown anywhere in document (event-delegation style) so we
  // don't have to find every freshly-bound block individually.
  document.addEventListener('click', function (e) {
    const btn = e.target.closest('.tb-gear-btn');
    if (btn) {
      // Find the matching popover within the same .tile-block-settings.
      const settings = btn.closest('.tile-block-settings');
      if (!settings) return;
      const popover = settings.querySelector('.tb-popover');
      if (!popover) return;
      const open = popover.style.display !== 'none';
      popover.style.display = open ? 'none' : 'block';
      btn.classList.toggle('blockr-gear-active', !open);
      e.stopPropagation();
      return;
    }
    // Click outside any popover closes all open ones.
    if (!e.target.closest('.tb-popover')) {
      document.querySelectorAll('.tile-block-settings .tb-popover').forEach((pop) => {
        if (pop.style.display !== 'none') {
          pop.style.display = 'none';
          const owner = pop.closest('.tile-block-settings');
          owner?.querySelector('.tb-gear-btn')?.classList.remove('blockr-gear-active');
        }
      });
    }
  });
})();
