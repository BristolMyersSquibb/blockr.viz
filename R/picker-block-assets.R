# Per-instance init script: renders the gear band's picker rows (into text
# input, choices multi-select, multiple checkbox, remove) and the face
# controls (one Blockr.Select per picker, labelled by `into`), and syncs
# whole-state with the server (value-filter style). The message handler
# registers at parse time so no push is ever dropped; payloads queue until
# the Blockr namespace is ready. Inline while the block settles -- then this
# becomes an inst/js asset behind htmlDependency.
# viz: long the deprecated picker's inline script, kept for saved boards
picker_block_assets <- function(ns) {
  # Only the ns() substitution goes through sprintf; the JS body is a plain
  # string appended via paste0. sprintf caps its format string at 8192 chars
  # and the full script exceeds that.
  head_js <- sprintf(
    "var NS = { gear: '%s', band: '%s', rows: '%s', add: '%s', face: '%s' };
      var IN = { pickers: '%s', ready: '%s' };
      var MSG = '%s';",
    ns("gear"), ns("band"), ns("rows"), ns("add"), ns("face"),
    ns("pickers"), ns("picker_block_ready"), ns("pk_update")
  )
  js <- paste0(
    "(function() {
      ", head_js, "

      var state = { cfgOptions: [], pickers: [] };
      var pending = null, ready = false;
      // Sentinel for the viewer-facing '(none)' choice of an OPTIONAL picker.
      // Never sent to R: selecting it clears the pick (selected = []), which
      // makes the picker inert -- no output column -- so a downstream chart
      // drops the aesthetic. Blockr.Select shows an option's VALUE as its
      // primary text, so the sentinel IS the label the viewer reads; a real
      // column literally named '(none)' is not a concern for chart data.
      var NONE = '(none)';

      function toArr(x) {
        if (x === null || x === undefined) return [];
        return Array.isArray(x) ? x : [x];
      }
      function norm(p) {
        return {
          into: String(p.into || ''),
          choices: toArr(p.choices).map(String),
          selected: toArr(p.selected).map(String),
          multiple: !!p.multiple,
          optional: !!p.optional
        };
      }
      function sig(pks) { return JSON.stringify(pks); }
      function optionFor(v) {
        var hit = state.cfgOptions.find(function (o) { return o.value === v; });
        return hit || { value: v, label: '' };
      }
      function el(id) { return document.getElementById(id); }
      function send() {
        // Stringified: a bare array of objects would be simplified into a
        // data.frame on the R side.
        Shiny.setInputValue(IN.pickers, JSON.stringify(state.pickers),
          { priority: 'event' });
      }

      Shiny.addCustomMessageHandler(MSG, function(msg) {
        if (ready) apply(msg); else pending = msg;
      });

      // Ask R for the control state. `pending` only parks a push that arrived
      // after this script parsed; on a deferred dock panel the script ships
      // WITH the panel, so the boot push was dropped before any handler
      // existed and no client-side queue can recover it. Announcing
      // unconditionally keeps this branch-free: when nothing was dropped the
      // answer is identical and apply()'s `changed` guard makes it a no-op.
      function announce() {
        if (window.Shiny && Shiny.setInputValue) {
          Shiny.setInputValue(IN.ready, Date.now(), { priority: 'event' });
        }
      }
      function announceWhenConnected() {
        // On a late mount the socket is long open, so announce now. On first
        // page load this script runs before the socket exists and an input
        // set then is lost -- wait for the connection (nothing was dropped in
        // that case anyway, so the wait costs nothing).
        var app = window.Shiny && Shiny.shinyapp;
        if (app && app.isConnected && app.isConnected()) { announce(); return; }
        if (window.jQuery) {
          window.jQuery(document).one('shiny:connected', announce);
        } else {
          announce();
        }
      }

      function apply(msg) {
        var cfg = toArr(msg.cfg_options).map(function (o) {
          return { value: String(o.value), label: String(o.label || '') };
        });
        var pks = toArr(msg.pickers).map(norm);
        var changed = sig(pks) !== sig(state.pickers) ||
          JSON.stringify(cfg) !== JSON.stringify(state.cfgOptions);
        state.cfgOptions = cfg;
        if (changed) {
          state.pickers = pks;
          renderBand();
          renderFace();
        }
      }

      function defaultInto() {
        var taken = state.pickers.map(function (p) { return p.into; });
        if (taken.indexOf('value') < 0) return 'value';
        var i = 2;
        while (taken.indexOf('value' + i) >= 0) i++;
        return 'value' + i;
      }

      // Reorder a picker within the list. The list order IS the order of the
      // face controls (and of the copies in the expression), so this is the
      // builder's only way to arrange the viewer's controls -- before, moving
      // one meant removing it and adding it back at the end.
      function moveRow(from, to) {
        if (to < 0 || to >= state.pickers.length || from === to) return;
        var p = state.pickers.splice(from, 1)[0];
        state.pickers.splice(to, 0, p);
        renderBand();
        renderFace();
        send();
      }

      function moveBtn(dir, from, to) {
        var b = document.createElement('button');
        b.type = 'button';
        b.className = 'pk-row-move pk-row-move--' + dir;
        var tip = dir === 'up' ? 'Move picker up' : 'Move picker down';
        b.setAttribute('data-blockr-tooltip', tip);
        b.setAttribute('aria-label', tip);
        b.innerHTML = Blockr.icons.chevron;
        if (to < 0 || to >= state.pickers.length) {
          b.disabled = true;
        } else {
          b.addEventListener('click', function () { moveRow(from, to); });
        }
        return b;
      }

      function renderBand() {
        var host = el(NS.rows);
        host.innerHTML = '';
        state.pickers.forEach(function (p, i) {
          var row = document.createElement('div');
          row.className = 'pk-row blockr-row';

          var intoWrap = document.createElement('div');
          intoWrap.className = 'pk-into-wrap';
          var intoLabel = document.createElement('label');
          intoLabel.className = 'blockr-label';
          intoLabel.textContent = 'Into';
          var into = document.createElement('input');
          into.type = 'text';
          into.className = 'blockr-text-input pk-into';
          into.value = p.into;
          into.addEventListener('change', function () {
            p.into = into.value;
            renderFace();
            send();
          });
          intoWrap.appendChild(intoLabel);
          intoWrap.appendChild(into);
          row.appendChild(intoWrap);

          var chWrap = document.createElement('div');
          chWrap.className = 'pk-choices';
          var chLabel = document.createElement('label');
          chLabel.className = 'blockr-label';
          chLabel.textContent = 'Columns offered';
          var chHost = document.createElement('div');
          chHost.className = 'blockr-select--bordered';
          chWrap.appendChild(chLabel);
          chWrap.appendChild(chHost);
          row.appendChild(chWrap);
          Blockr.Select.multi(chHost, {
            options: state.cfgOptions,
            selected: p.choices,
            placeholder: 'Columns offered to the viewer\\u2026',
            reorderable: true,
            onChange: function (sel) {
              // May go empty mid-edit (clear, then refill): the picker
              // goes inert until it has choices again.
              var vals = toArr(sel).filter(Boolean);
              p.choices = vals;
              p.selected = p.selected.filter(function (v) {
                return vals.indexOf(v) >= 0;
              });
              if (!p.selected.length && vals.length) p.selected = [vals[0]];
              renderFace();
              send();
            }
          });

          var boxWrap = document.createElement('div');
          boxWrap.className = 'pk-multiple blockr-checkbox-row';
          var box = Blockr.checkbox('Multiple', p.multiple, function (checked) {
            if (checked && state.pickers.some(function (q, j) {
              return j !== i && q.multiple;
            })) {
              box.set(false);
              return;
            }
            p.multiple = checked;
            if (!checked && p.selected.length > 1) p.selected = [p.selected[0]];
            // Optional is single-picker only; turning Multiple on retires it.
            if (checked) p.optional = false;
            renderBand();
            renderFace();
            send();
          });
          boxWrap.appendChild(box.el);
          row.appendChild(boxWrap);

          // Optional: lets the viewer pick '(none)' to turn the aesthetic off.
          // Meaningless for a multiple picker (deselecting all already yields
          // no pivot), so it is only offered on single pickers.
          if (!p.multiple) {
            var optWrap = document.createElement('div');
            optWrap.className = 'pk-optional blockr-checkbox-row';
            var optBox = Blockr.checkbox('Optional', p.optional,
              function (checked) {
                p.optional = checked;
                // Leaving optional while on '(none)': restore a real pick so a
                // now-required picker is not stuck inert.
                if (!checked && !p.selected.length && p.choices.length) {
                  p.selected = [p.choices[0]];
                }
                renderFace();
                send();
              });
            optWrap.appendChild(optBox.el);
            row.appendChild(optWrap);
          }

          // Card tools, top-right, revealed on hover like the remove button
          // everywhere else in the design system.
          var tools = document.createElement('div');
          tools.className = 'pk-row-tools';
          if (state.pickers.length > 1) {
            tools.appendChild(moveBtn('up', i, i - 1));
            tools.appendChild(moveBtn('down', i, i + 1));
          }

          var rm = document.createElement('button');
          rm.type = 'button';
          rm.className = 'blockr-row-remove';
          rm.setAttribute('data-blockr-tooltip', 'Remove picker');
          rm.setAttribute('aria-label', 'Remove picker');
          rm.innerHTML = Blockr.icons.x;
          rm.addEventListener('click', function () {
            if (state.pickers.length <= 1) return;
            state.pickers.splice(i, 1);
            renderBand();
            renderFace();
            send();
          });
          tools.appendChild(rm);
          row.appendChild(tools);

          host.appendChild(row);
        });
      }

      function renderFace() {
        var host = el(NS.face);
        host.innerHTML = '';
        state.pickers.forEach(function (p) {
          var field = document.createElement('div');
          field.className = 'pk-field';
          var label = document.createElement('label');
          label.className = 'blockr-label';
          label.textContent = p.into;
          var selHost = document.createElement('div');
          selHost.className = 'blockr-select--bordered';
          field.appendChild(label);
          field.appendChild(selHost);
          host.appendChild(field);
          var mode = p.multiple ? 'multi' : 'single';
          var isOpt = !!p.optional && !p.multiple;
          // Optional single pickers offer a leading '(none)' entry; picking it
          // (or clearing) commits an empty selection = inert = aesthetic off.
          var opts = p.choices.map(optionFor);
          if (isOpt) opts = [{ value: NONE, label: '' }].concat(opts);
          function faceSel() {
            if (p.multiple) return p.selected;
            return p.selected[0] || (isOpt ? NONE : null);
          }
          var handle = Blockr.Select[mode](selHost, {
            options: opts,
            selected: faceSel(),
            placeholder: 'Select\\u2026',
            onChange: function (sel) {
              var vals = toArr(sel).filter(Boolean);
              if (isOpt && (!vals.length || vals[0] === NONE)) {
                // '(none)' or cleared: commit the empty pick (no restore).
                p.selected = [];
                send();
                return;
              }
              if (!vals.length) {
                // Required picker: the last pick stays -- restore, don't commit.
                handle.setOptions(opts, faceSel());
                return;
              }
              p.selected = vals.filter(function (v) { return v !== NONE; });
              send();
            }
          });
        });
      }

      function init() {
        if (!window.Blockr || !Blockr.Select || !Blockr.checkbox) {
          setTimeout(init, 50); return;
        }
        var gear = el(NS.gear), band = el(NS.band), add = el(NS.add);
        if (!gear || !band || !add) { setTimeout(init, 50); return; }
        gear.innerHTML = Blockr.icons.gear;
        var addIcon = add.querySelector('.blockr-add-icon');
        if (addIcon) addIcon.innerHTML = Blockr.icons.plus;
        gear.addEventListener('click', function () {
          var open = band.classList.toggle('blockr-settings--open');
          gear.classList.toggle('blockr-gear-active', open);
        });
        add.addEventListener('click', function () {
          // Start empty: the builder curates which columns this picker offers.
          state.pickers.push({
            into: defaultInto(),
            choices: [],
            selected: [],
            multiple: false,
            optional: false
          });
          renderBand();
          renderFace();
          send();
        });
        ready = true;
        if (pending) { apply(pending); pending = null; }
        announceWhenConnected();
      }
      init();
    })();"
  )
  shiny::tagList(
    shiny::tags$style(shiny::HTML(
      ".blockr-picker .pk-field { margin: 2px 0 8px; }
       .blockr-picker .blockr-label { display: block; margin-bottom: 4px; }
       /* The rows host must own the full band width in the flex settings band
          (the band is display:flex;flex-wrap:wrap), else it sizes to content
          and each picker card sits in a cramped flex track. */
       .blockr-picker .pk-rows { flex: 1 1 100%; min-width: 0; }
       /* Each picker is a vertical card (was a horizontal row that overflowed
          narrow blocks): fields stack full width and shrink to fit, mirroring
          blockr.dm's value filter. .blockr-row supplies the card border +
          hover-reveal remove; we only flip its axis to column. */
       .blockr-picker .pk-row { flex-direction: column; align-items: stretch;
         gap: 8px; min-height: 0; padding: 10px 12px; position: relative; }
       .blockr-picker .pk-into-wrap { width: 100%; min-width: 0; }
       .blockr-picker .pk-into { width: 100%; box-sizing: border-box; }
       .blockr-picker .pk-choices { width: 100%; min-width: 0; }
       .blockr-picker .pk-choices .blockr-select--bordered { min-width: 0; }
       .blockr-picker .pk-multiple,
       .blockr-picker .pk-optional { padding-bottom: 0; }
       /* Move up / move down / remove, stacked in the card's top-right
          corner. The move buttons copy .blockr-row-remove's geometry and
          hover-reveal so the three read as one set; only the danger colour
          stays exclusive to remove. */
       .blockr-picker .pk-row-tools { position: absolute; top: 6px; right: 6px;
         display: flex; align-items: center; gap: 2px; }
       .blockr-picker .pk-row .blockr-row-remove { position: static; margin: 0; }
       .blockr-picker .pk-row-move { display: flex; align-items: center;
         justify-content: center; width: 26px; height: 26px; padding: 0;
         background: none; border: none; border-radius: 4px;
         color: var(--blockr-grey-400, #9ca3af); cursor: pointer; opacity: 0;
         flex-shrink: 0; transition: opacity 0.15s ease, color 0.15s ease; }
       .blockr-picker .pk-row:hover .pk-row-move { opacity: 1; }
       .blockr-picker .pk-row-move:hover:not(:disabled) {
         color: var(--blockr-color-primary, #2563eb);
         background: var(--blockr-grey-100, #f3f4f6); }
       .blockr-picker .pk-row:hover .pk-row-move:disabled { opacity: 0.3;
         cursor: default; }
       .blockr-picker .pk-row-move--up svg { transform: rotate(180deg); }"
    )),
    shiny::tags$script(shiny::HTML(js))
  )
}
