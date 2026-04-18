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
      return scope.querySelectorAll('.tb-pill-group');
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
})();
