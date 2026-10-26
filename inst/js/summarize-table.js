/**
 * Summarize table: search, sort, expand, row-click drill.
 *
 * Self-contained on purpose. table.js owns `.drilldown-table-container` and
 * its payload-push render path; the summarize table is its own markup
 * in `.blockr-summarize-container`, so it binds its own handlers rather
 * than reaching into table.js internals.
 *
 * Contract with R (R/summarize-table-html.R):
 *   container  data-summarize-elem-id  the ns()-ed id the `_action` input hangs off
 *              data-summarize-drill    the drill column, absent = display only
 *   table      data-summarize-nested   "1" when parent rows are present
 *   row        data-summarize-label / data-summarize-parent / data-summarize-level
 *   num cell   data-v             the raw number to sort on
 */
(function () {
  "use strict";

  var BOUND = "summarizeTableBound";

  // Where this file was served from: snapdom.js sits next to it and is loaded
  // only when a picture is first asked for.
  var SUMMARIZE_JS_BASE = (function () {
    var sc = document.currentScript;
    return sc && sc.src ? sc.src.replace(/[^\/]*$/, "") : "";
  })();

  /** @param {Element} root */
  function rows(root) {
    return Array.prototype.slice.call(
      // Table rows, or the rows of the cards look (table-cards.js), which
      // carry the same classes and attributes.
      root.querySelectorAll(".blockr-summarize-row")
    );
  }

  /** Parent rows start collapsed; a child is visible only when its parent is
   * expanded AND it survives the search. Both conditions are recomputed here
   * so the two features cannot fight over the same class. */
  function applyVisibility(root) {
    var open = {};
    rows(root).forEach(function (r) {
      if (r.classList.contains("is-parent")) {
        // The table block's contract: the ROW carries `collapsed`, which is
        // what rotates the shared chevron.
        var label = r.getAttribute("data-summarize-label");
        var key = r.getAttribute("data-summarize-key") || label;
        var isOpen = !r.classList.contains("collapsed");
        open[label] = isOpen;
        open[key] = isOpen;
      }
    });
    rows(root).forEach(function (r) {
      if (!r.classList.contains("is-child")) return;
      var vis = open[r.getAttribute("data-summarize-parent")] === true;
      if (vis) r.classList.remove("collapsed-hidden");
      else r.classList.add("collapsed-hidden");
    });
    syncFoldAll(root);
  }

  // ---------- fold all ----------
  // A nested table gets the table block's corner chevron in its stub header
  // (table.js buildFoldAll, same button and CSS): one click opens or folds
  // every parent row. The head is re-rendered with each payload, so the
  // button is (re)built here and its state read off the rows.
  function syncFoldAll(root) {
    var table = root.querySelector("table.blockr-summarize-table");
    if (!table || table.getAttribute("data-summarize-nested") !== "1") return;
    var parents = root.querySelectorAll("tbody tr.blockr-summarize-row.is-parent");
    if (!parents.length) return;
    var ths = table.querySelectorAll("thead th.blockr-stub-header");
    var th = ths.length ? ths[ths.length - 1] : null;
    if (!th) return;
    var btn = th.querySelector(".dt-foldall");
    if (!btn) {
      btn = document.createElement("button");
      btn.type = "button";
      btn.className = "dt-foldall";
      btn.innerHTML = CHEV;
      var wrap = document.createElement("div");
      wrap.className = "dt-foldall-wrap";
      wrap.appendChild(btn);
      if ((th.textContent || "").trim()) {
        var lab = document.createElement("span");
        lab.className = "dt-stub-title";
        while (th.firstChild) lab.appendChild(th.firstChild);
        wrap.appendChild(lab);
      } else {
        var all = document.createElement("span");
        all.className = "dt-foldall-all";
        all.textContent = "All";
        btn.appendChild(all);
      }
      th.appendChild(wrap);
      th.classList.add("dt-has-foldall");
    }
    var open = Array.prototype.every.call(parents, function (r) {
      return !r.classList.contains("collapsed");
    });
    var act = open ? "Collapse all groups" : "Expand all groups";
    btn.setAttribute("aria-expanded", open ? "true" : "false");
    btn.setAttribute("aria-label", act);
    btn.setAttribute("data-blockr-tooltip", act);
  }

  // ---------- search ----------
  // Row text read once per row (the body is static between renders), and a
  // parent stays visible when any of its children match -- the same rule the
  // table block's search follows.
  function rowText(root, r) {
    if (!root._summarizeCache) root._summarizeCache = new WeakMap();
    var t = root._summarizeCache.get(r);
    if (t == null) {
      // Segment tooltips (the swimlane's event labels) live in attributes,
      // not text -- fold them in so searching "pruritus" finds the
      // subjects who had it.
      var tips = [];
      r.querySelectorAll("[data-tip]").forEach(function (e) {
        tips.push(e.getAttribute("data-tip"));
      });
      t = (r.textContent + " " + tips.join(" ")).toLowerCase();
      root._summarizeCache.set(r, t);
    }
    return t;
  }

  /** Re-apply the search box's current query to the rows now in the DOM. */
  function runSearch(root) {
    var input = root.querySelector("input.blockr-search");
    var q = input ? (input.value || "").trim().toLowerCase() : "";
    var all = rows(root);
    var matched = {};
    all.forEach(function (r) {
      var hit = !q || rowText(root, r).indexOf(q) !== -1;
      if (hit) {
        r.classList.remove("blockr-summarize-hidden-search");
        var pr = r.getAttribute("data-summarize-parent");
        if (pr) matched[pr] = true;
      } else {
        r.classList.add("blockr-summarize-hidden-search");
      }
    });
    // A matching child keeps its parent on screen, and auto-expands it, so
    // searching a leaf never returns an empty-looking table.
    all.forEach(function (r) {
      if (!r.classList.contains("is-parent")) return;
      var label = r.getAttribute("data-summarize-label");
      var key = r.getAttribute("data-summarize-key") ||
        label;
      if (matched[key] || matched[label]) {
        r.classList.remove("blockr-summarize-hidden-search");
        if (q) setOpen(r, true);
      }
    });
    applyVisibility(root);
    var foldRow = root.querySelector("tr.blockr-summarize-fold");
    if (foldRow) foldRow.style.display = q ? "none" : "";
    // Segment emphasis: a query that matches segment tooltips also LIGHTS
    // the matching segments (same dimming as the hover highlight), so a
    // term search shows WHERE in each timeline the event sits -- not just
    // which rows survive. Only armed when at least one segment matches;
    // a subject-id query must not dim everything.
    var anyHit = false;
    root.querySelectorAll(".lane-seg").forEach(function (s) {
      var hit = !!q &&
        (s.getAttribute("data-tip") || "").toLowerCase().indexOf(q) !== -1;
      s.classList.toggle("is-hit", hit);
      if (hit) anyHit = true;
    });
    root.classList.toggle("seg-search", anyHit);
  }

  function bindSearch(root) {
    var input = root.querySelector("input.blockr-search");
    if (!input) return;
    var timer = null;
    function run() {
      runSearch(root);
    }
    input.addEventListener("input", function () {
      if (timer) clearTimeout(timer);
      timer = setTimeout(run, 120);
    });
  }

  // ---------- sort ----------
  // Client-side, on `data-v` for numbers and the label text otherwise. In a
  // nested table only whole parent blocks move, so a child never leaves its
  // parent; within a block the children sort too.
  //
  // Every row carries data-summarize-ord: the order it ARRIVED in. The server's
  // order (sort_by / sort_dir, e.g. visits in visit order) is a real state,
  // so header sorting cycles back to it on the third click instead of
  // stranding the table in an alphabet it was configured out of.

  function bindSort(root) {
    // Looked up per interaction: with the data-push transport the <table> is
    // replaced on every payload, so nothing may be captured here.
    var tbl = function () { return root.querySelector("table.blockr-summarize-table"); };
    // The sort state lives on the CONTAINER, read fresh per click: a new
    // payload replaces it (new columns, new server order), so the next header
    // click starts at click 1 again.
    function sortState() {
      if (!root._summarizeSort) root._summarizeSort = { key: null, dir: 0 };
      return root._summarizeSort;
    }

    // Same contract as the table block's wireSort(): the header carries
    // data-col-index, the cell carries the raw number in data-v. Index 0 is
    // the stub (label) column, which sorts on its text.
    function cellValue(r, th) {
      var idx = parseInt(th.getAttribute("data-col-index"), 10);
      if (!idx) return r.getAttribute("data-summarize-label") || "";
      var td = r.children[idx];
      if (!td) return null;
      var raw = td.getAttribute("data-v");
      if (raw === null || raw === "") return null;
      var n = parseFloat(raw);
      // A text field column carries its text in data-v: sort it as text.
      return isNaN(n) ? raw : n;
    }

    function cmp(a, b, th, dir) {
      var av = cellValue(a, th);
      var bv = cellValue(b, th);
      if (typeof av === "string" || typeof bv === "string") {
        return dir * String(av).localeCompare(String(bv));
      }
      if (av === null && bv === null) return 0;
      if (av === null) return 1;
      if (bv === null) return -1;
      return dir * (av - bv);
    }

    function blocks(nested) {
      var out = [];
      var current = null;
      rows(root).forEach(function (r) {
        if (nested && r.classList.contains("is-child")) {
          if (current) current.kids.push(r);
          return;
        }
        current = { head: r, kids: [] };
        out.push(current);
      });
      return out;
    }

    function sortBy(th) {
      var table = tbl();
      if (!table) return;
      var tbody = table.querySelector("tbody");
      if (!tbody) return;
      var nested = table.getAttribute("data-summarize-nested") === "1";
      var key = th.getAttribute("data-col-index");
      var state = sortState();
      // Three states per column: the first click sorts, the second reverses,
      // the third gives the server's order back (no icon, nothing claimed).
      var first = key === "0" ? 1 : -1;
      var restore = false;
      if (state.key === key) {
        if (state.dir === first) {
          state.dir = -first;
        } else {
          state.key = null;
          state.dir = 0;
          restore = true;
        }
      } else {
        state.key = key;
        state.dir = first;
      }
      // The arrow is the table block's .blockr-sort-icon, driven by the same
      // asc/desc classes, so the two headers behave and read identically.
      table.querySelectorAll("th .blockr-sort-icon").forEach(function (ic) {
        ic.classList.remove("blockr-sort-icon-asc", "blockr-sort-icon-desc");
      });
      if (!restore) {
        var icon = th.querySelector(".blockr-sort-icon");
        if (icon) {
          icon.classList.add(state.dir === 1
            ? "blockr-sort-icon-asc" : "blockr-sort-icon-desc");
        }
      }

      var ordOf = function (r) {
        return parseInt(r.getAttribute("data-summarize-ord"), 10) || 0;
      };
      var bs = blocks(nested);
      bs.sort(function (x, y) {
        return restore
          ? ordOf(x.head) - ordOf(y.head)
          : cmp(x.head, y.head, th, state.dir);
      });
      var frag = document.createDocumentFragment();
      bs.forEach(function (b) {
        frag.appendChild(b.head);
        b.kids.sort(function (x, y) {
          return restore ? ordOf(x) - ordOf(y) : cmp(x, y, th, state.dir);
        });
        b.kids.forEach(function (k) { frag.appendChild(k); });
      });
      var fold = tbody.querySelector("tr.blockr-summarize-fold");
      tbody.appendChild(frag);
      if (fold) tbody.appendChild(fold);
    }

    root.addEventListener("click", function (e) {
      // The fold-all chevron sits in the stub header; it never sorts.
      if (e.target.closest(".dt-foldall")) return;
      var th = e.target.closest("th.blockr-sortable[data-col-index]");
      if (!th || !root.contains(th)) return;
      e.stopPropagation();
      sortBy(th);
    });
  }

  // ---------- expand / collapse ----------
  /** @param {Element} row @param {boolean} open */
  function setOpen(row, open) {
    if (open) row.classList.remove("collapsed");
    else row.classList.add("collapsed");
    var btn = row.querySelector(".blockr-indent-btn");
    if (btn) btn.setAttribute("aria-expanded", open ? "true" : "false");
  }

  function bindToggle(root) {
    root.addEventListener("click", function (e) {
      var all = e.target.closest(".dt-foldall");
      if (all && root.contains(all)) {
        e.stopPropagation();
        e.preventDefault();
        var shut = all.getAttribute("aria-expanded") === "true";
        root.querySelectorAll("tbody tr.blockr-summarize-row.is-parent")
          .forEach(function (r) { setOpen(r, !shut); });
        applyVisibility(root);
        return;
      }
      var btn = e.target.closest(".blockr-indent-btn");
      if (!btn || !root.contains(btn)) return;
      e.stopPropagation();
      e.preventDefault();
      var row = btn.closest(".blockr-summarize-row");
      if (!row) return;
      setOpen(row, row.classList.contains("collapsed"));
      applyVisibility(root);
    });
  }

  // ---------- drill ----------
  // Row click emits the same categorical filter contract as the chart and
  // table blocks: {type, column, values}. Clicking the active row clears it.
  // Transient drill: with a ctrl_target the row click is an EVENT sent to that
  // block, not a selection this table holds. Nothing latches, nothing toggles
  // off, and the claim carries a click counter so re-clicking one row sends
  // again. Same rule as the chart, the table and the heatmap; the undo lives
  // at the target.
  function summarizeTransient(root) {
    var t = root.getAttribute("data-summarize-ctrl-target");
    return !!(t && t.trim());
  }

  // Click counter: changes on a real click and NOT on a board update, which is
  // the distinction the server's send-once skip has to make.
  var summarizeDrillSeq = 0;

  // Light the clicked row, then release it. One at a time: two lit rows would
  // read as two selections where only the last click counts.
  function summarizeFlash(root, tr) {
    root.querySelectorAll(".summarize-flash").forEach(function (n) {
      n.classList.remove("summarize-flash");
    });
    tr.classList.add("summarize-flash");
    var drop = function () {
      tr.classList.remove("summarize-flash");
      tr.removeEventListener("animationend", drop);
    };
    tr.addEventListener("animationend", drop);
  }

  // What a row stands for: its grouping path, outer -> inner. A parent row
  // claims the outer column, a child row both, a flat row its group column.
  // `path` rides on the payload (R/summarize-drill.R); a payload without one
  // falls back to the drill column.
  function rowClaim(root, tr, col) {
    var pay = root._summarizePayload;
    var path = (pay && pay.path && pay.path.length) ? pay.path : [col];
    var label = tr.getAttribute("data-summarize-label");
    if (path.length < 2) return { cols: [path[0]], vals: [label] };
    if (tr.classList.contains("is-parent")) {
      return {
        cols: [path[0]],
        vals: [tr.getAttribute("data-summarize-key") || label]
      };
    }
    return {
      cols: [path[0], path[1]],
      vals: [
        tr.getAttribute("data-summarize-parent-key") ||
          tr.getAttribute("data-summarize-parent"),
        label
      ]
    };
  }

  // "AEBODSYS = X, AEDECOD = Y": the footer's and the receipt's words.
  function claimPhrase(c) {
    return c.cols.map(function (col, i) {
      return col + " = " + c.vals[i];
    }).join(", ");
  }

  function bindDrill(root) {
    var elemId = root.getAttribute("data-summarize-elem-id");
    if (!elemId) return;

    function send(claim) {
      if (!window.Shiny || !Shiny.setInputValue) return;
      Shiny.setInputValue(
        elemId + "_action",
        {
          action: claim === null ? "clear_filter" : "filter",
          type: "categorical",
          column: claim === null ? null : claim.cols,
          values: claim === null ? null : claim.vals,
          nonce: ++summarizeDrillSeq
        },
        { priority: "event" }
      );
    }

    function select(phrase) {
      root._summarizeSel = phrase;
      paintStatus(root);
    }
    root._summarizeClear = function () {
      rows(root).forEach(function (r) { r.classList.remove("is-on"); });
      send(null);
      select(null);
    };

    root.addEventListener("click", function (e) {
      var tr = e.target.closest(".blockr-summarize-row.is-pick");
      if (!tr || !root.contains(tr)) return;
      if (e.target.closest(".blockr-indent-btn")) return;
      // Read per click: the gear turns the drill on and off through the
      // payload, without re-rendering the container.
      var col = root.getAttribute("data-summarize-drill");
      if (!col) return;
      var claim = rowClaim(root, tr, col);
      // Transient: no toggle. A second click on the same row means "send it
      // again", never "un-drill" -- that is the target's job.
      if (summarizeTransient(root)) {
        send(claim);
        summarizeFlash(root, tr);
        root._summarizeReceipt = {
          text: "Drilled down to " + claimPhrase(claim),
          at: Date.now()
        };
        paintStatus(root);
        clearTimeout(root._summarizeReceiptTimer);
        root._summarizeReceiptTimer = setTimeout(function () {
          dropReceipt(root);
        }, SUMMARIZE_RECEIPT_HOLD_MS + SUMMARIZE_RECEIPT_FADE_MS + 200);
        return;
      }
      var was = tr.classList.contains("is-on");
      rows(root).forEach(function (r) { r.classList.remove("is-on"); });
      if (was) {
        send(null);
        select(null);
      } else {
        tr.classList.add("is-on");
        send(claim);
        select(claimPhrase(claim));
      }
    });
  }

  // The drill's footer line, in the chart's words and markup (chart/chrome.js
  // _updateStatus, chart.css .dd-status-*), so a table and a chart on one
  // board report a drill the same way. A latched drill reads "No filter
  // active" / "Filtered: COL = value" with Reset. With a ctrl_target the click
  // is an event: nothing at rest, and a receipt ("Drilled down to COL =
  // value") that holds, then fades.
  var SUMMARIZE_RECEIPT_HOLD_MS = 1400;
  var SUMMARIZE_RECEIPT_FADE_MS = 1100;

  function dropReceipt(root) {
    if (!root._summarizeReceipt) return;
    root._summarizeReceipt = null;
    paintStatus(root, true);
  }

  function paintStatus(root, returning) {
    var box = root.querySelector(".blockr-summarize-footer .dd-status-footer");
    if (!box) return;
    box.textContent = "";
    var col = root.getAttribute("data-summarize-drill");
    if (!col) return;
    var rc = root._summarizeReceipt;
    if (rc) {
      var age = Date.now() - rc.at;
      if (age < SUMMARIZE_RECEIPT_HOLD_MS + SUMMARIZE_RECEIPT_FADE_MS) {
        var rec = document.createElement("span");
        rec.className = "dd-status-text dd-status-receipt";
        rec.textContent = rc.text;
        // Resume, do not restart: a repaint mid-fade picks the fade up where
        // it is (chart/chrome.js does the same).
        rec.style.animation = "dd-receipt-out " + SUMMARIZE_RECEIPT_FADE_MS +
          "ms linear " + (SUMMARIZE_RECEIPT_HOLD_MS - age) + "ms both";
        rec.addEventListener("animationend", function () {
          dropReceipt(root);
        });
        box.appendChild(rec);
        return;
      }
      root._summarizeReceipt = null;
    }
    if (summarizeTransient(root)) return;
    var sel = root._summarizeSel;
    var span = document.createElement("span");
    span.className = "dd-status-text" + (returning ? " dd-status-returning" : "");
    span.textContent = sel ? "Filtered: " + sel : "No filter active";
    box.appendChild(span);
    if (sel) {
      var reset = document.createElement("button");
      reset.type = "button";
      reset.className = "dd-status-reset";
      reset.textContent = "Reset";
      reset.addEventListener("click", function (e) {
        e.stopPropagation();
        if (root._summarizeClear) root._summarizeClear();
      });
      box.appendChild(reset);
    }
  }



  // ==========================================================================
  // Data-push body (dev/table-data-push-design.md, the table block's shape).
  // The server ships the body as a column-oriented cell model over the
  // "blockr-viz-summarize-data" custom message instead of rendering it through
  // Shiny: ~93-95% smaller than the equivalent HTML at 790 rows, and a payload
  // cached per elem id re-renders a re-mounted dock panel with no R round trip.
  //
  // The markup assembled here must match R/summarize-push.R's summarize_cells_html()
  // byte for byte (test-summarize-push.R pins them), including the escaping rules
  // htmltools applies: & < > and the attribute quote.
  // ==========================================================================

  /** @type {Record<string, {rev: number, payload: any}>} */
  var payloadStore = {};

  function esc(x) {
    return String(x == null ? "" : x)
      .replace(/&/g, "&amp;").replace(/</g, "&lt;")
      .replace(/>/g, "&gt;").replace(/"/g, "&quot;");
  }
  // R prints widths with format(trim=TRUE): an integral value has no decimals,
  // so match that rather than emitting "57.00".
  function w(x) {
    var n = Number(x) || 0;
    return String(n);
  }
  function dataV(v) {
    return " data-v=\"" + (v == null || v === "" ? "" : esc(String(v))) + "\"";
  }

  var CHEV = '<svg class="blockr-chev" viewBox="0 0 24 24" fill="none"' +
    ' stroke="currentColor" stroke-width="2.4" stroke-linecap="round"' +
    ' stroke-linejoin="round" aria-hidden="true"><path d="M6 9l6 6 6-6"/></svg>';

  // Nothing to draw is an EMPTY track: an NA width (shipped as null) and a
  // zero alike. The fill's 2px floor is for small values, so a zero must not
  // emit a fill at all or it reads as a little.
  function trackHtml(width, fill, sub) {
    return '<div class="blockr-summarize-track' + (sub ? " is-sub" : "") + '">' +
      (!(width > 0) ? "" :
        '<div class="blockr-summarize-fill" style="width:' + w(width) + "%" +
        (fill ? ";background:" + fill : "") + '"></div>') +
      "</div>";
  }

  // The in-bar value label (R: summarize_barwrap): track left, the value in a
  // fixed-width right-aligned slot -- one width per column so tracks align.
  function barWrap(inner, c, i) {
    if (!c.disp) return inner;
    var pct = c.pct && c.pct[i] ?
      ' <span class="blockr-summarize-pct">' + c.pct[i] + "</span>" : "";
    return '<div class="blockr-summarize-barwrap">' + inner +
      '<span class="blockr-summarize-barval" style="width:' + c.dw + 'ch">' +
      c.disp[i] + pct + "</span></div>";
  }

  function splitHtml(c, i) {
    var grouped = c.mode === "grouped";
    var out = "";
    for (var j = 0; j < c.names.length; j++) {
      // A stacked segment's own number, inside it (R: summarize_cells()).
      var lab = (!grouped && c.slab) ? c.slab[j][i] : "";
      var body = '<div class="blockr-summarize-fill' + (lab ? " has-lab" : "") +
        '" style="width:' +
        w(c.seg[j][i]) + "%;background:" + c.fills[j] +
        '" data-summarize-tip="' + esc(c.names[j]) + ": " + c.segv[j][i] +
        '">' + (lab ? '<span class="blockr-summarize-seglab w' + lab.length +
          '" style="color:' + c.sink[j] + '">' + lab + "</span>" : "") +
        "</div>";
      var has = c.segv[j][i] > 0;
      if (grouped && c.ldisp) {
        // With value labels each level is its own barwrap: a thin track,
        // then the level's value in the column's one slot width.
        if (has) {
          out += '<div class="blockr-summarize-lv blockr-summarize-barwrap">' +
            '<div class="blockr-summarize-track is-lv">' + body + "</div>" +
            '<span class="blockr-summarize-barval" style="width:' + c.dw +
            'ch">' + c.ldisp[j][i] + "</span></div>";
        }
      } else if (grouped) {
        // A level with no value in this row gets no track (the lollipop
        // draws no lane for it).
        if (has) out += '<div class="blockr-summarize-row3">' + body + "</div>";
      } else if (has) {
        out += body;
      }
    }
    if (grouped && c.ldisp) {
      return '<div class="blockr-summarize-multi">' + out + "</div>";
    }
    return '<div class="blockr-summarize-track' + (grouped ? " is-tall" : "") +
      '">' + out + "</div>";
  }

  function dvHtml(width, pos) {
    if (!(width > 0)) return '<div class="blockr-summarize-dv"></div>';
    return '<div class="blockr-summarize-dv"><div class="blockr-summarize-fill ' +
      (pos ? "is-pos" : "is-neg") + '" style="width:' + w(width) +
      '%"></div></div>';
  }

  // ---- lane mark emitters ----
  // Byte-identical twins of summarize_box_html / summarize_pr_html / summarize_iv_html /
  // summarize_sp_html in R/summarize-push.R (test-summarize-push.R pins the pair). Every
  // geometric number arrives pre-rounded; emission conditions key on shipped
  // nulls, never re-derived arithmetic. `p(x)` is String(): positions and
  // widths were rounded R-side so the two prints agree.
  function p(x) { return String(x); }

  function boxHtml(c, i) {
    var cls = c.bare ? " is-bare" : "";
    if (c.bc[i] == null) {
      return '<div class="blockr-summarize-lane blockr-summarize-boxcell' + cls +
        '"></div>';
    }
    var s = '<div class="blockr-summarize-lane blockr-summarize-boxcell' + cls +
      '" data-summarize-tip="' + c.tip[i] + '">';
    if (c.w1[i] != null) {
      s += '<i class="lane-wh" style="left:' + p(c.wl[i]) + "%;width:" +
        p(c.w1[i]) + '%"></i>';
    }
    if (c.w2[i] != null) {
      s += '<i class="lane-wh" style="left:' + p(c.b2[i]) + "%;width:" +
        p(c.w2[i]) + '%"></i>';
    }
    if (c.w1[i] != null) {
      s += '<i class="lane-cap" style="left:' + p(c.wl[i]) + '%"></i>';
    }
    if (c.w2[i] != null) {
      s += '<i class="lane-cap" style="left:' + p(c.wh[i]) + '%"></i>';
    }
    if (c.bw[i] != null) {
      s += '<i class="lane-box" style="left:' + p(c.bl[i]) + "%;width:" +
        p(c.bw[i]) + '%"></i>';
    }
    return s + '<i class="lane-med" style="left:' + p(c.bc[i]) +
      '%"></i></div>';
  }

  function prHtml(c, i) {
    var cls = c.bare ? " is-bare" : "";
    if (c.c[i] == null) {
      return '<div class="blockr-summarize-lane blockr-summarize-prcell' + cls +
        '"></div>';
    }
    var s = '<div class="blockr-summarize-lane blockr-summarize-prcell' + cls +
      '" data-summarize-tip="' + c.tip[i] + '">';
    if (c.ow && c.ow[i] != null) {
      s += '<i class="lane-fence" style="left:' + p(c.ol[i]) + "%;width:" +
        p(c.ow[i]) + '%"></i>';
    }
    if (c.rw[i] != null) {
      s += '<i class="lane-rng" style="left:' + p(c.l[i]) + "%;width:" +
        p(c.rw[i]) + '%"></i>';
    }
    return s + '<i class="lane-ctr" style="left:' + p(c.c[i]) +
      '%"></i></div>';
  }

  // The pair cell (dumbbell): band, reference line, link, then the two
  // marks. Byte-identical to summarize_pair_html().
  function pairHtml(c, i) {
    var s = '<div class="blockr-summarize-lane blockr-summarize-pacell' +
      (c.dash[i] ? " is-dash" : "") + '"' +
      (c.fill[i] != null ? ' style="--blockr-summarize-fill:' + c.fill[i] + '"' : "") +
      (c.tip[i] ? ' data-summarize-tip="' + c.tip[i] + '"' : "") + ">";
    if (c.bw[i] != null) {
      s += '<i class="lane-band" style="left:' + p(c.bl[i]) + "%;width:" +
        p(c.bw[i]) + '%"></i>';
    }
    if (c.rf != null) {
      s += '<i class="lane-ref" style="left:' + p(c.rf) + '%"></i>';
    }
    if (c.w[i] != null) {
      s += '<i class="lane-link" style="left:' + p(c.l[i]) + "%;width:" +
        p(c.w[i]) + '%"></i>';
    }
    if (c.a[i] != null) {
      s += '<i class="lane-from" style="left:' + p(c.a[i]) + '%"></i>';
    }
    if (c.b[i] != null) {
      s += '<i class="lane-to' + (c.open[i] ? " is-open" : "") +
        '" style="left:' + p(c.b[i]) + '%"></i>';
    }
    return s + "</div>";
  }

  function ivHtml(c, i) {
    var s = '<div class="blockr-summarize-lane blockr-summarize-ivcell" data-d0="' +
      p(c.d0) + '" data-d1="' + p(c.d1) + '"' +
      (c.dd ? ' data-dd="1"' : "") + ">";
    var segs = c.segs[i] || [];
    for (var j = 0; j < segs.length; j++) {
      s += '<i class="lane-seg" style="left:' + p(segs[j][0]) + "%;width:" +
        p(segs[j][1]) + "%;background:" + c.fills[segs[j][2] - 1] + '"' +
        (segs[j].length > 3 ? ' data-l="' + segs[j][3] + '"' : "") +
        ' data-tip="' + c.tips[i][j] + '"></i>';
    }
    return s + "</div>";
  }

  function spHtml(c, i) {
    var s = '<div class="blockr-summarize-lane blockr-summarize-spcell" data-xs="' +
      c.xs[i] + '" data-ys="' + c.ys[i] + '">' +
      '<svg viewBox="0 0 100 36" preserveAspectRatio="none">';
    if (c.rby != null) {
      s += '<rect class="lane-refband" x="0" y="' + p(c.rby) +
        '" width="100" height="' + p(c.rbh) + '"></rect>';
    }
    if (c.bd[i] != null) {
      s += '<polygon class="lane-band" points="' + c.bd[i] + '"></polygon>';
    }
    if (c.rc != null) {
      s += '<line class="lane-refline" x1="0" y1="' + p(c.rc) +
        '" x2="100" y2="' + p(c.rc) +
        '" vector-effect="non-scaling-stroke"></line>';
    }
    if (c.pl[i] !== "") {
      s += '<polyline class="lane-ln" points="' + c.pl[i] +
        '" vector-effect="non-scaling-stroke"></polyline>';
    }
    s += "</svg>";
    if (c.dx[i] != null) {
      s += '<i class="lane-dot" style="left:' + p(c.dx[i]) + "%;top:" +
        p(c.dy[i]) + '%"></i>';
    }
    return s + "</div>";
  }

  // A colour-split distribution cell: one lane per level, in level order,
  // each in the level's colour. A level with no rows in this group draws no
  // lane at all (a table grouped by subject reads as one coloured glyph per
  // row). The colour rides as a CSS custom property on the wrapper, so the
  // glyph emitters are reused untouched. Byte-identical to summarize_multi_html.
  function multiHtml(c, i) {
    var s = '<div class="blockr-summarize-multi">';
    for (var j = 0; j < c.lv.length; j++) {
      var g = c.lv[j];
      if (!lvDrawn(c, g, i)) continue;
      var inner;
      if (c.kind === "pair") {
        g.rf = c.rf;
        inner = pairHtml(g, i);
      } else {
        inner = c.kind === "box" ? boxHtml(g, i) : prHtml(g, i);
      }
      // With value labels on, each level is its own barwrap: the lane, then
      // the level's number in the column's one slot width.
      var lab = g.disp && c.dw != null;
      s += '<div class="blockr-summarize-lv' +
        (lab ? " blockr-summarize-barwrap" : "") +
        '" style="--blockr-summarize-fill:' + c.fills[j] + '">' + inner +
        (lab ? '<span class="blockr-summarize-barval" style="width:' + c.dw +
          'ch">' + g.disp[i] + "</span>" : "") + "</div>";
    }
    return s + "</div>";
  }

  // Does level `g` of a colour-split cell draw anything in row i?
  function lvDrawn(c, g, i) {
    if (c.kind === "pair") return g.a[i] != null || g.b[i] != null;
    return (c.kind === "box" ? g.bc[i] : g.c[i]) != null;
  }

  /** A row's classes and data attributes: the table row's, and the card
   *  row's in the cards look, so drill, search and collapse read one shape. */
  function rowMeta(p, i) {
    var expanded = !!p.expanded;
    var parent = !!(p.parent_row && p.parent_row[i]);
    var child = !!(p.level && p.level[i] > 0);
    var cls = "blockr-summarize-row" +
      (parent ? " is-parent blockr-indent-toggle" +
        (expanded ? "" : " collapsed") : "") +
      (child ? " is-child" + (expanded ? "" : " collapsed-hidden") : "") +
      (p.pick ? " is-pick" : "") +
      (p.on && p.on[i] ? " is-on" : "");
    var attrs = ' data-summarize-label="' + esc(p.label[i]) + '"' +
      (parent && p.parent_key ? ' data-summarize-key="' +
        esc(p.parent_key[i]) + '"' : "") +
      (child && p.parent_key ? ' data-summarize-parent-key="' +
        esc(p.parent_key[i]) + '"' : "") +
      (child ? ' data-summarize-parent="' + esc(p.parent[i]) + '"' : "") +
      ' data-summarize-level="' + (child ? p.level[i] : 0) +
      '" data-summarize-ord="' + i + '"';
    var btn = parent ? '<button class="blockr-indent-btn" type="button"' +
      ' tabindex="-1" aria-expanded="' + (expanded ? "true" : "false") +
      '">' + CHEV + "</button>" : "";
    return { cls: cls, attrs: attrs, parent: parent, child: child, btn: btn };
  }

  /** The mark of one cell, without its value label: the one emitter per
   *  kind, shared by the table's cells and the cards' glyphs. "" for a
   *  column that draws no mark (numbers, text, tiles). */
  function glyphHtml(c, i) {
    switch (c.kind) {
      case "bar": return trackHtml(c.w[i], c.fill, c.sub && c.sub[i]);
      case "barsplit": return splitHtml(c, i);
      case "bardiv": return dvHtml(c.w[i], c.pos[i]);
      case "box": return c.multi ? multiHtml(c, i) : boxHtml(c, i);
      case "pointrange": return c.multi ? multiHtml(c, i) : prHtml(c, i);
      case "pair": return c.multi ? multiHtml(c, i) : pairHtml(c, i);
      case "interval": return ivHtml(c, i);
      case "sparkline": return spHtml(c, i);
      default: return "";
    }
  }

  /** A column as a card draws it: the card prints the number above the
   *  mark, so the mark drops its own value labels. */
  function cardCol(c) {
    if (!c) return c;
    var g = {};
    for (var k in c) g[k] = c[k];
    g.slab = null;
    g.ldisp = null;
    g.dw = null;
    return g;
  }

  /** Assemble the whole tbody from the cell model. */
  function assembleBody(p) {
    var out = [];
    for (var i = 0; i < p.n; i++) {
      var rm = rowMeta(p, i);
      var parent = rm.parent;
      var child = rm.child;
      var row = '<tr class="' + rm.cls + '"' + rm.attrs + ">";
      row += '<td class="blockr-summarize-label-col blockr-stub' +
        (parent ? " blockr-has-toggle" : "") + '"' +
        (child ? ' style="padding-left:48px;"' : "") + ">" + rm.btn +
        '<span class="blockr-summarize-label">' + esc(p.label[i]) + "</span></td>";
      for (var k = 0; k < p.cols.length; k++) {
        var c = p.cols[k];
        if (c.kind === "tile") {
          row += '<td class="blockr-summarize-tile-col"' + dataV(c.v[i]) + ">" +
            (c.bg[i] == null ? "" :
              '<span class="blockr-summarize-tile" style="background:' +
              c.bg[i] + ";color:" + c.fg[i] + '">' + c.disp[i] + "</span>") +
            "</td>";
        } else if (c.kind === "num" && c.text) {
          row += '<td class="blockr-summarize-txt' +
            (c.failed ? " is-failed" : "") + '"' + dataV(c.v[i]) + ">" +
            c.disp[i] + "</td>";
        } else if (c.kind === "num") {
          row += '<td class="blockr-summarize-num dt-col-num"' + dataV(c.v[i]) +
            ">" + c.disp[i] +
            (c.pct ? ' <span class="blockr-summarize-pct">' + c.pct[i] + "</span>" : "") +
            "</td>";
        } else if (c.kind === "interval") {
          row += '<td class="blockr-summarize-bar-col' +
            (c.lg ? " blockr-summarize-wide" : "") + '"' + dataV(c.v[i]) + ">" +
            glyphHtml(c, i) + "</td>";
        } else {
          row += '<td class="blockr-summarize-bar-col"' + dataV(c.v[i]) + ">" +
            barWrap(glyphHtml(c, i), c, i) + "</td>";
        }
      }
      out.push(row + "</tr>");
    }
    if (p.fold) {
      out.push('<tr class="blockr-summarize-fold"><td colspan="' + p.ncol + '">' +
        esc(p.fold) + "</td></tr>");
    }
    return out.join("");
  }

  // ---------- the cards look ----------
  // The payload's `cards` (R/table-cards.R), drawn by table-cards.js with
  // this file's emitters and row attributes. That script loads only with a
  // table in the cards look, so it may arrive after the payload: the table
  // waits for it (the "blockr-table-cards-ready" event below).
  function renderCards(root, p) {
    var host = root.querySelector(".blockr-cards-host");
    var TC = window.BlockrTableCards;
    if (!host) return;
    if (!TC) {
      root._tcPending = true;
      return;
    }
    root._tcPending = false;
    var cols = (p.cols || []).map(cardCol);
    TC.render(host, p.cards, {
      glyph: function (k, i) { return cols[k] ? glyphHtml(cols[k], i) : ""; },
      row: function (i) {
        var rm = rowMeta(p, i);
        return { cls: rm.cls, attrs: rm.attrs, pre: rm.btn };
      }
    });
  }

  document.addEventListener("blockr-table-cards-ready", function () {
    document.querySelectorAll(".blockr-summarize-container").forEach(function (root) {
      if (!root._tcPending || !root._summarizePayload) return;
      renderCards(root, root._summarizePayload);
      root._summarizeCache = null;
      var input = root.querySelector("input.blockr-search");
      if (input && input.value) runSearch(root);
      else applyVisibility(root);
    });
  });

  // A downloaded page draws its cards without a block: table-cards.js reads
  // the glyphs from here.
  window.BlockrSummarizeGlyphs = {
    glyph: function (c, i) { return glyphHtml(cardCol(c), i); }
  };

  /** The container's own settings, which its one-shot render does not
   *  follow: search box, drill column, ctrl_target, height. A key the
   *  payload lacks leaves that setting alone. */
  function applySettings(root, ch) {
    if (ch.drill !== undefined) {
      if (ch.drill) root.setAttribute("data-summarize-drill", ch.drill);
      else root.removeAttribute("data-summarize-drill");
    }
    if (ch.ctrl_target !== undefined) {
      root.setAttribute("data-summarize-ctrl-target", ch.ctrl_target || "");
    }
    var input = root.querySelector("input.blockr-search");
    if (input && ch.search !== undefined) {
      input.style.display = ch.search ? "" : "none";
      // A hidden box must not keep filtering the rows.
      if (!ch.search) input.value = "";
    }
    var wrap = root.querySelector(".blockr-table-wrapper");
    if (wrap && ch.max_height !== undefined) {
      var page = !ch.max_height;
      wrap.classList.toggle("dt-scroll-page", page);
      wrap.style.maxHeight = page ? "" : ch.max_height;
      wrap.style.overflow = page ? "" : "auto";
    }
  }

  /** Title / subtitle / caption / legend / footer, refreshed in place. */
  function applyChrome(root, ch) {
    if (!ch) return;
    applySettings(root, ch);
    paintTitles(root, ch);
    if (root._summarizeBand) root._summarizeBand();
    var lg = root.querySelector(".blockr-summarize-legend");
    if (lg) {
      // Cards carry their legends themselves (table-cards.js).
      if (!ch.legend || root._summarizeLook === "cards") {
        lg.style.display = "none";
        lg.innerHTML = "";
      } else {
        // One titled group per colour column: colour is mapped per summary
        // column, so a table can decode more than one dimension at once.
        var h = "";
        (ch.legend.groups || []).forEach(function (g) {
          h += '<span class="blockr-summarize-legend-group">' +
            '<span class="blockr-summarize-legend-title">' + esc(g.title) +
            "</span>";
          (g.items || []).forEach(function (it) {
            h += '<span class="blockr-summarize-legend-item"><i style="background:' +
              it.color + '"></i>' + esc(it.label) + "</span>";
          });
          h += "</span>";
        });
        lg.innerHTML = h;
        lg.style.display = "";
      }
    }
    var f = ch.foot || {};
    var note = root.querySelector(".blockr-summarize-note");
    if (note) note.textContent = f.note || "";
    root._summarizeSel = f.filter || null;
    paintStatus(root);
  }

  // The three bands, painted as the chart paints them (chart/chrome.js
  // _updateTitles): a piece naming one of the block's settings is a word that
  // opens it, and a setting whose clause dropped is offered beside the
  // sentence. Without the gear's engine (a static page) the text alone.
  function paintTitles(root, ch) {
    var band = root.querySelector(".dd-table-titles");
    var tEl = band && band.querySelector(".dd-table-title");
    var sEl = band && band.querySelector(".dd-table-subtitle");
    var cap = root.querySelector(".dd-table-caption");
    var slots = root._summarizeSlots;
    if (slots) slots.close();
    function paint(el, text, parts, offers) {
      if (!el) return;
      if (slots) {
        slots.paint(el, text || "", parts);
        slots.paintOffers(el, offers);
      } else {
        el.textContent = text || "";
      }
      el.style.display = el.childNodes.length ? "" : "none";
    }
    paint(tEl, ch.title, ch.title_parts, ch.title_offers);
    paint(sEl, ch.subtitle, ch.subtitle_parts, ch.subtitle_offers);
    paint(cap, ch.caption, ch.caption_parts, ch.caption_offers);
    if (band) {
      var shown = (tEl && tEl.style.display !== "none") ||
        (sEl && sEl.style.display !== "none");
      band.style.display = shown ? "" : "none";
    }
  }

  /** Apply one payload: inject the body, refresh the bands, re-arm the JS. */
  function applyPayload(root, p) {
    var wrap = root.querySelector(".blockr-table-wrapper");
    if (!wrap) return;
    var cards = p.kind !== "html" && p.look === "cards" && p.cards;
    root._summarizeLook = cards ? "cards" : "table";
    wrap.classList.toggle("tc-on", !!cards);
    if (p.kind === "html") {
      wrap.innerHTML = p.html;
    } else if (cards) {
      // The table stays, empty and hidden: the gear and the download menu
      // read their state off its attributes.
      wrap.innerHTML = p.head + '<div class="blockr-cards-host"></div>';
      root._summarizePayload = p;
      renderCards(root, p);
    } else {
      wrap.innerHTML = p.head;
      var tb = wrap.querySelector("tbody");
      if (tb) tb.innerHTML = assembleBody(p);
    }
    // Fresh rows, fresh server order: the next header click starts at click 1.
    root._summarizeSort = { key: null, dir: 0 };
    // The hover card reads its numbers from here (cardHtml).
    root._summarizePayload = p;
    applyChrome(root, p.chrome);
    // The row set changed: drop the search text cache and re-apply the current
    // query + collapse state to the fresh rows.
    root._summarizeCache = null;
    var input = root.querySelector("input.blockr-search");
    if (input && input.value) runSearch(root);
    else applyVisibility(root);
  }

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    // A deck's request (R/chart-capture.R): the picture at the width it
    // asks for, the height the table's own. Exactly one reply per request.
    Shiny.addCustomMessageHandler("blockr-viz-summarize-capture", function (msg) {
      var replied = false;
      var reply = function (x) {
        if (replied) return;
        replied = true;
        var out = { req: msg.req };
        for (var k in x) out[k] = x[k];
        Shiny.setInputValue("blockr_viz_capture_result", out,
                            { priority: "event" });
      };
      setTimeout(function () {
        reply({ error: "the table did not draw in time" });
      }, 20000);
      var payload = null;
      try { payload = JSON.parse(msg.payload); } catch (e) { payload = null; }
      if (!payload) {
        reply({ error: "the table's payload could not be read" });
        return;
      }
      summarizePicture(payload, Number(msg.width) || 900, msg.ratio, msg.css)
        .then(reply, function (e) { reply({ error: String(e) }); });
    });

    Shiny.addCustomMessageHandler("blockr-viz-summarize-data",
      function (msg) {
        var entry = payloadStore[msg.id];
        var payload = null;
        if (entry && entry.rev === msg.rev) {
          payload = entry.payload;
        } else {
          try { payload = JSON.parse(msg.payload); } catch (e) { payload = null; }
          if (!payload) return;
          payloadStore[msg.id] = { rev: msg.rev, payload: payload };
        }
        var eid = (window.CSS && CSS.escape)
          ? CSS.escape(msg.id)
          : String(msg.id).replace(/"/g, '\\"');
        var root = document.querySelector(
          '.blockr-summarize-container[data-summarize-elem-id="' + eid + '"]');
        // No container yet: the payload waits in the store, and bind() picks it
        // up when the chrome turns up (no timers, no expiring delivery window).
        if (root) applyPayload(root, payload);
      });
  }

  /** A stored payload for this container, or null. */
  function storedFor(root) {
    var id = root.getAttribute("data-summarize-elem-id");
    var e = id ? payloadStore[id] : null;
    return e ? e.payload : null;
  }

  // ---------- the export picture ----------
  // The table an export carries is the one the browser draws, the chart
  // block's rule (R/chart-capture.R): a fresh table mounted offscreen at the
  // export's width from the same payload the block renders, every row and no
  // scroll box, turned into one PNG by snapdom. No second renderer to keep in
  // step with this one.
  var snapdomLoading = null;
  function loadSnapdom() {
    if (window.snapdom) return Promise.resolve(window.snapdom);
    if (!snapdomLoading) {
      snapdomLoading = new Promise(function (resolve, reject) {
        var sc = document.createElement("script");
        sc.src = SUMMARIZE_JS_BASE + "snapdom.js";
        sc.onload = function () {
          if (window.snapdom) resolve(window.snapdom);
          else reject(new Error("snapdom.js loaded but defined nothing"));
        };
        sc.onerror = function () {
          snapdomLoading = null;
          reject(new Error("snapdom.js could not be loaded"));
        };
        document.head.appendChild(sc);
      });
    }
    return snapdomLoading;
  }

  // The container the export draws: the block's chrome without the tools
  // (no gear, search, download or drill line). `css` is the table's inline
  // stylesheet, for a page where no summarize table is mounted to carry it.
  function captureRoot(width, css) {
    var host = document.createElement("div");
    host.className = "blockr-capture-host";
    host.style.cssText = "position:fixed;left:-20000px;top:0;width:" +
      width + "px;";
    if (css) {
      var st = document.createElement("style");
      st.textContent = css;
      host.appendChild(st);
    }
    var root = document.createElement("div");
    root.className = "blockr-html-table-container blockr-summarize-container " +
      "blockr-summarize-capture";
    root.innerHTML = '<div class="dd-table-titles">' +
      '<div class="dd-table-title"></div>' +
      '<div class="dd-table-subtitle"></div></div>' +
      '<div class="blockr-summarize-legend" style="display:none"></div>' +
      '<div class="blockr-table-wrapper"></div>' +
      '<div class="dd-table-caption"></div>';
    host.appendChild(root);
    document.body.appendChild(host);
    return root;
  }

  // The picture is as wide as the table, whatever the panel's width: laid
  // out at `width` first, then the host grows until no table runs past the
  // root's right edge (the wrapper would scroll it, and snapdom draws only
  // the root's box). A table that fits keeps the panel's width.
  function widenToContent(host, root) {
    for (var i = 0; i < 4; i++) {
      var box = root.getBoundingClientRect();
      var edge = box.right - (parseFloat(getComputedStyle(root).paddingRight) || 0);
      var over = 0;
      root.querySelectorAll("table").forEach(function (t) {
        over = Math.max(over, t.getBoundingClientRect().right - edge);
      });
      if (over < 0.5) return;
      host.style.width = Math.ceil(host.offsetWidth + over) + "px";
    }
  }

  /** One picture of a payload at `width` CSS px: {png, width, height}, the
   * size in CSS px (R turns it into inches at 96 dpi). With a slide `box`
   * (R: capture_page_box()) it also carries `pages` when the table is too
   * long for one slide (capture-pages.js), and the title they repeat. */
  function summarizePicture(payload, width, ratio, css, box) {
    return loadSnapdom().then(function (snap) {
      var root = captureRoot(width, css);
      var host = root.parentNode;
      var drop = function () {
        if (host.parentNode) host.parentNode.removeChild(host);
      };
      try {
        applyPayload(root, payload);
      } catch (e) {
        drop();
        throw e;
      }
      var fonts = (document.fonts && document.fonts.ready) ?
        document.fonts.ready : Promise.resolve();
      return fonts.then(function () {
        return new Promise(function (r) {
          requestAnimationFrame(function () { requestAnimationFrame(r); });
        });
      }).then(function () {
        widenToContent(host, root);
        var w = Math.ceil(root.offsetWidth);
        var h = Math.ceil(root.offsetHeight);
        // dpr 1: snapdom multiplies the scale by the device pixel ratio, and
        // the ratio is already the export's to choose.
        return snap.toCanvas(root, {
          scale: Number(ratio) || 2, dpr: 1, embedFonts: true,
          backgroundColor: "#ffffff"
        }).then(function (canvas) {
          var cut = window.BlockrCapturePages;
          var pages = (box && cut) ? cut(canvas, root, box, null) : [];
          var t = root.querySelector(".dd-table-title");
          drop();
          return { png: canvas.toDataURL("image/png"), width: w, height: h,
                   pages: pages, title: t ? t.textContent : "" };
        });
      }, function (e) { drop(); throw e; });
    });
  }

  // Opening the download menu posts the picture, so it is in R before a
  // format is picked (chart/chrome.js does the same). At the panel's own width, so
  // the file is the table on screen, unscrolled.
  function bindCapture(root, header) {
    var elemId = root.getAttribute("data-summarize-elem-id");
    header.addEventListener("click", function (e) {
      var t = e.target;
      if (!t || !t.closest ||
          !t.closest(".blockr-action-menu__trigger, .blockr-tool")) return;
      var tbl = root.querySelector("table.blockr-summarize-table");
      var c = {};
      try { c = JSON.parse((tbl && tbl.getAttribute("data-summarize-cfg")) || "{}"); }
      catch (err) { c = {}; }
      var stored = storedFor(root);
      if (!c.capture_export || !stored || !elemId) return;
      var w = Math.round(root.getBoundingClientRect().width) || 900;
      var box = c.capture_page;
      summarizePicture(stored, w, c.capture_ratio, null, box).then(function (r) {
        if (window.Shiny && Shiny.setInputValue) {
          Shiny.setInputValue(elemId + "_capture", r, { priority: "event" });
        }
      }, function (err) {
        if (window.console) console.warn("summarize table picture:", err);
      });
    }, true);
  }

  // ---------- gear ----------
  // Same engine, same structure, same vocabulary as the chart and table blocks:
  // Blockr.DrilldownConfig renders the Mapping / Presentation sections plus the
  // Drill-down capability section from this role spec. Keys are the block's R
  // config params, so onChange(key) round-trips straight to the reactiveVals.
  // The chart's aggregate picker verbatim: the shared labeled AGG_FNS plus the
  // chart-only "None (as is)" identity (drilldown-agg.js documents it) -- for
  // pre-aggregated data such as one value per subject.
  var DAggR = (typeof Blockr !== "undefined" && Blockr.DrilldownAgg) ||
    window.DrilldownAgg;
  var FUNC_OPT = ((DAggR && DAggR.AGG_FNS) ||
    ["count", "count_distinct", "sum", "mean", "median", "min", "max"])
    .concat([{ value: "identity", label: "None (as is)" }]);
  // The chart's words, so the two trays read alike.
  var SORT_DIR_OPT = [{ value: "desc", label: "Descending" },
                      { value: "asc", label: "Ascending" }];
  // The table block's three looks (R: TABLE_LOOKS). Cards need a row per
  // group; the variables layout has none, and its gear greys them out.
  var LOOK_OPT = [{ value: "table", label: "Table" },
                  { value: "compact", label: "Compact" },
                  { value: "cards", label: "Cards" }];
  var SEARCH_OPT = [{ value: "on", label: "Search bar" },
                    { value: "off", label: "No search bar" }];
  // Header sorting off: the configured order is the exhibit. A visit table
  // ordered by AVISITN has nothing to gain from an accidental A-Z click.
  var SORTABLE_OPT = [{ value: "on", label: "Header sorting" },
                      { value: "off", label: "No header sorting" }];
  // The per-column tick strip under the header: the domain named ONCE, for
  // every mark that sits on a scale. Off for a dense exhibit that carries its
  // scale in the numbers beside the marks.
  var VALUES_OPT = [{ value: "on", label: "Show values" },
                    { value: "off", label: "Off" }];
  var AXIS_OPT = [{ value: "on", label: "Column axis" },
                  { value: "off", label: "No column axis" }];
  // The length of the labelled marks (R: summarize_bar_width). Fit fills the
  // panel up to a ceiling; the others are fixed and leave the slack blank.
  var BAR_WIDTH_OPT = [{ value: "narrow", label: "Narrow" },
                       { value: "medium", label: "Medium" },
                       { value: "wide", label: "Wide" },
                       { value: "fit", label: "Fit" }];
  // One toggle, every format the machine can write: "can people take this
  // table away" is one decision, and which file the reader wants is theirs.
  // (The table block settled this the same way.)
  var DOWNLOAD_OPT = [{ value: "on", label: "Downloads" },
                      { value: "off", label: "No downloads" }];

  // The lane statistic vocabulary: MUST mirror R's LANE_STATS/LANE_STAT_META
  // (R/lane-stats.R, drift-tested) -- chart/roles.js SUMMARY_STATS plus
  // mean_ci95, which exists only on this surface because R has qt().
  var LANE_STATS = [
    { value: "median_q1_q3", label: "Median · Q1–Q3" },
    { value: "mean_sd", label: "Mean ± SD" },
    { value: "mean_2sd", label: "Mean ± 2 SD" },
    { value: "mean_se", label: "Mean ± SE" },
    { value: "mean_ci95", label: "Mean · 95% CI" },
    { value: "p5_p95", label: "5th–95th percentile" },
    { value: "p10_p90", label: "10th–90th percentile" },
    { value: "min_max", label: "Min–Max" }
  ];
  var LANE_WHISKERS = [{ value: "tukey", label: "Tukey (1.5×IQR)" }]
    .concat(LANE_STATS);

  // Glyphs for the columns editor's display tiles, the chart block's icon
  // style (14px, viewBox 16, currentColor, stroke-width 1.4).
  var TYPE_ICONS = {
    bar:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="currentColor">' +
      '<rect x="2" y="2.5" width="11" height="3"/>' +
      '<rect x="2" y="6.5" width="8" height="3"/>' +
      '<rect x="2" y="10.5" width="5" height="3"/></svg>',
    box:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ' +
      'stroke="currentColor" stroke-width="1.4">' +
      '<path d="M1.5 8h3M11.5 8h3M4.5 4.5h7v7h-7zM8 4.5v7"/></svg>',
    pointrange:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ' +
      'stroke="currentColor" stroke-width="1.4">' +
      '<path d="M2 8h12M2 5.5v5M14 5.5v5"/>' +
      '<circle cx="8" cy="8" r="2.4" fill="currentColor" stroke="none"/></svg>',
    interval:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ' +
      'stroke="currentColor" stroke-width="3">' +
      '<path d="M2 5h6M7 11h7"/></svg>',
    sparkline:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ' +
      'stroke="currentColor" stroke-width="1.4">' +
      '<path d="M1.5 11l3.5-3 2.5 1.5L11 5l3.5 3"/></svg>'
  };

  var SUMMARIZE_ROLES = {
    // The table's one grouping (outer -> inner, at most two): one row per
    // key combination, the outer an expandable parent. Empty is one Total
    // row over all input rows, so the placeholder says that. A change
    // re-renders the gear: sorting, search and drill show only with a
    // grouping.
    by:     { label: "Group by", kind: "columns", colType: "cat",
              placeholder: "One Total row", rerender: true },
    // Words of the sentence only, never rows of the gear: a title can name
    // the outer grouping column and the first column's colour and facet
    // (summarize_title_args() in R/summarize-exhibit.R).
    group:  { label: "Group", kind: "column", colType: "cat" },
    color:  { label: "Color", kind: "column", colType: "cat" },
    facet:  { label: "Facet", kind: "column", colType: "cat" },
    // Facet column order in the summarize-table mode: each summary's level
    // copies adjacent (the comparison affordance), or level groups spanning
    // the summaries (the Table-1 reading).
    facet_layout: { label: "Facet layout", kind: "segmented", options: [
      { value: "by_summary", label: "By summary" },
      { value: "by_level", label: "By level" }
    ] },
    // Presentation.
    sort_by:  { label: "Sort", kind: "select", options: [] },
    sort_dir: { label: "Order", kind: "segmented", options: SORT_DIR_OPT },
    search:   { label: "Search", kind: "segmented", options: SEARCH_OPT },
    sortable: { label: "Header sorting", kind: "segmented",
                options: SORTABLE_OPT },
    axis:     { label: "Column axis", kind: "segmented", options: AXIS_OPT },
    // The Values switch, the chart block's word for it: every mark's number
    // beside it, one per level on a colour split.
    value_labels: { label: "Values", kind: "segmented", options: VALUES_OPT },
    bar_width: { label: "Bar width", kind: "segmented",
                 options: BAR_WIDTH_OPT },
    // A count's percentage base and missing groups (the table block's
    // `pct_of` / `missing`).
    pct_of: { label: "Percent of", kind: "segmented", options: [
      { value: "facet", label: "Column" }, { value: "total", label: "Table" }
    ] },
    missing: { label: "Missing", kind: "segmented", options: [
      { value: "drop", label: "Drop" }, { value: "level", label: "Show" }
    ] },
    download: { label: "Download", kind: "segmented", options: DOWNLOAD_OPT },
    // The table, the matrix look (square cells, titles on their side) or
    // cards. `hint` is set per render (summarizeSections) to name the
    // summaries the cards leave out.
    look:     { label: "Look", kind: "segmented", options: LOOK_OPT },
    // Drill-down: a plain column role, like the table block's.
    drill:    { label: "Filter on", kind: "column", colType: "any" },
    title:    { label: "Title", kind: "text", ph: "e.g. AEs by {ARM}",
                autoValue: function (cfg) {
                  return (cfg.title == null && cfg.title_auto) ? cfg.title_auto : "";
                } },
    subtitle: { label: "Subtitle", kind: "text", ph: "e.g. N = {n_distinct(USUBJID)}",
                autoValue: function (cfg) {
                  return (cfg.subtitle == null && cfg.subtitle_auto) ? cfg.subtitle_auto : "";
                } },
    caption:  { label: "Caption", kind: "text", ph: "e.g. Source: ADAE",
                autoValue: function (cfg) {
                  return (cfg.caption == null && cfg.caption_auto) ? cfg.caption_auto : "";
                } }
  };

  // ==========================================================================
  // The summarize-table columns editor (_blockr.design/open/summarize-table/):
  // one row per summary, typed (simple / dist / field / series / spans /
  // expr), each with its own display and scope. Rendered into the engine's
  // customSections seam; every edit sends the FULL array as one config
  // value. Presets reproduce the single-mark blocks in one click.
  // ==========================================================================

  var SUMMARY_TYPES = {
    simple: { label: "simple", shows: ["bar", "number", "dot", "tile"] },
    dist:   { label: "dist", shows: ["pointrange", "box", "text"] },
    field:  { label: "field", shows: ["text"] },
    series: { label: "series", shows: ["sparkline"] },
    spans:  { label: "spans", shows: ["interval"] },
    pair:   { label: "pair", shows: ["dumbbell"] },
    expr:   { label: "expr", shows: ["text"] },
    // A function of the rows, grouped by table row. "auto" reads the mark
    // off what the
    // function returns (R's lane_custom_setup()); the rest name one.
    custom: { label: "custom",
              shows: ["auto", "bar", "number", "dumbbell", "box",
                      "pointrange", "text"] }
  };
  var SHOW_ICONS = {
    number: '<svg width="14" height="14" viewBox="0 0 16 16">' +
      '<text x="1" y="12" font-size="9" fill="currentColor">123</text></svg>',
    text: '<svg width="14" height="14" viewBox="0 0 16 16">' +
      '<text x="0" y="12" font-size="8" fill="currentColor">1 (2)</text></svg>',
    dot: '<svg width="14" height="14" viewBox="0 0 16 16">' +
      '<circle cx="8" cy="8" r="2.6" fill="currentColor"/></svg>',
    bar: TYPE_ICONS.bar,
    box: TYPE_ICONS.box,
    pointrange: TYPE_ICONS.pointrange,
    interval: TYPE_ICONS.interval,
    sparkline: TYPE_ICONS.sparkline,
    dumbbell: '<svg width="14" height="14" viewBox="0 0 16 16">' +
      '<line x1="4" y1="8" x2="12" y2="8" stroke="currentColor" stroke-width="1.5"/>' +
      '<rect x="1.8" y="5.8" width="4.4" height="4.4" fill="none" stroke="currentColor"' +
      ' stroke-width="1.2" transform="rotate(45 4 8)"/>' +
      '<circle cx="12" cy="8" r="2.6" fill="currentColor"/></svg>',
    tile: '<svg width="14" height="14" viewBox="0 0 16 16">' +
      '<rect x="1" y="1" width="6" height="6" rx="1" fill="currentColor"/>' +
      '<rect x="9" y="1" width="6" height="6" rx="1" fill="currentColor" opacity=".45"/>' +
      '<rect x="1" y="9" width="6" height="6" rx="1" fill="currentColor" opacity=".45"/>' +
      '<rect x="9" y="9" width="6" height="6" rx="1" fill="currentColor"/></svg>'
  };
  SHOW_ICONS.auto = '<svg width="14" height="14" viewBox="0 0 16 16">' +
    '<text x="1" y="12" font-size="9" font-style="italic" fill="currentColor">fx</text></svg>';
  // Tile captions: friendlier than the raw enum where it helps.
  var SHOW_LABELS = { pointrange: "dot range", interval: "swimlane",
                      auto: "from result" };
  // The marks as the Show control and the row's glyph name them.
  var SHOW_WORDS = { bar: "Bar", number: "Number", dot: "Dot", tile: "Tile",
                     pointrange: "Dot range", box: "Box", text: "Text",
                     sparkline: "Sparkline", interval: "Swimlane",
                     dumbbell: "Dumbbell", auto: "From result" };
  // The seed a new custom column starts from: a working function.
  var CUSTOM_SEED = "\\(d) data.frame(value = nrow(d))";

  // A distribution glyph is ONE mark -- a centre, an inner range and an
  // outer range -- and `show` picks the STYLE it is drawn in. "(none)" on a
  // range is a first-class choice, not an absence: it is how the degenerate
  // family (IQR bar, plain dot range, bare dot) is expressed. These mirror
  // R's lane_norm_summaries(); the legacy `stat` / `whiskers` fields seed
  // them so a saved board opens on what it drew.
  var NONE_OPT = { value: "none", label: "(none)" };
  var RANGE_OPT = [NONE_OPT].concat(LANE_STATS);
  var OUTER_OPT = [NONE_OPT].concat(LANE_WHISKERS);
  // Public keys (R/lane-summaries.R): `summary` is the inner range (and a
  // text column's statistic), `whiskers` the outer one. The old names are
  // read too, for an entry written before the rename.
  function distInner(s) {
    return s.summary || s.inner || s.stat || "median_q1_q3";
  }
  function distOuter(s) {
    if (s.whiskers) return s.whiskers;
    if (s.outer) return s.outer;
    return s.show === "pointrange" ? "none" : "tukey";
  }
  function rangeWord(v) { return v === "none" ? "" : v; }

  // Colour and facet are the summary's OWN optional mappings: a column
  // splits by one, repeats per level of the other, or carries neither.
  // `ok` is the JS half of R's lane_color_capable() / the field rule.
  var SUMMARY_MAPS = [
    { key: "color", label: "Color",
      hint: "Split this column by a column's levels",
      phrase: function (v) { return "split by " + v; },
      ok: function (s) {
        // A custom column's split is a column of what it returns.
        if (s.type === "custom") return false;
        if (s.type === "dist") return (s.show || "pointrange") !== "text";
        if (s.type === "simple") {
          return ["number", "tile"].indexOf(s.show || "bar") < 0;
        }
        return s.type === "spans" || s.type === "pair";
      } },
    { key: "facet",  label: "Facet",
      hint: "Repeat this column once per level of a column",
      phrase: function (v) { return "faceted by " + v; },
      ok: function (s) { return s.type !== "field"; } },
    // A tile's paint: the worst level of this column among the cell's rows
    // (an AE heatmap's severity). Without it the tile is painted by value.
    { key: "fill", label: "Paint by",
      hint: "Paint each cell by the worst level of a column",
      phrase: function (v) { return "painted by the worst " + v; },
      ok: function (s) { return s.type === "simple" && s.show === "tile"; } },
    // A custom count's population: distinct values of this column in the
    // cell's slice are its N, so it prints "54 (18%)".
    { key: "denom", label: "Percent of", want: "any",
      hint: "Show the count as a share of this column's distinct values",
      phrase: function (v) { return "% of " + v; },
      seed: function (cols) {
        for (var i = 0; i < cols.length; i++) {
          if (/^USUBJID$|SUBJ/i.test(cols[i].name)) return cols[i].name;
        }
        return cols.length ? cols[0].name : "";
      },
      ok: function (s) {
        return s.type === "custom" &&
          ["auto", "bar", "number"].indexOf(markOf(s)) > -1;
      } }
  ];

  function summaryBody(s) {
    switch (s.type) {
      case "simple": return (s.func || "count") +
        (s.col ? "(" + s.col + ")" : "()") + " · " + (s.show || "bar");
      case "dist": {
        if (s.show === "text") {
          return distInner(s) + "(" + (s.col || "?") + ") · text";
        }
        // The pieces in the order they are drawn, then the style.
        var pieces = [rangeWord(distInner(s)), rangeWord(distOuter(s))]
          .filter(function (x) { return x; });
        return (s.col || "?") + " · " + (pieces.join(" + ") || "centre only") +
          " · " + (SHOW_LABELS[s.show] || s.show || "dot range");
      }
      case "field": return (s.col || "?") + " (distinct values)";
      case "series": return (s.col || "?") + " over " + (s.x || "?") +
        (s.band && s.band.length === 2 ? ", band " + s.band[0] + "–" + s.band[1] : "") +
        (s.ref && s.ref !== "none" ? ", ref " + s.ref : "");
      case "spans": return (s.x || "?") + " → " + (s.xend || "?") +
        (s.label ? ", label " + s.label : "") +
        (s.size === "lg" ? ", wide" : "");
      case "pair": return (s.x_func && s.x_func !== "identity" ?
          s.x_func + "(" + (s.x || "?") + ")" : (s.x || "?")) +
        " → " + (s.xend_func || "max") + "(" + (s.xend || "?") + ")" +
        (s.ref_lo != null && s.ref_lo !== "" || s.ref_hi != null && s.ref_hi !== "" ?
          ", range " + (s.ref_lo == null ? "" : s.ref_lo) + "–" + (s.ref_hi == null ? "" : s.ref_hi) : "") +
        (s.ref != null && s.ref !== "" ? ", ref " + s.ref : "") +
        (s.dash ? ", dashed by " + s.dash : "");
      case "expr": return s.expr || "";
      case "custom": {
        var first = String(s.fn || "").split("\n").map(function (x) {
          return x.trim();
        }).filter(function (x) { return x; }).join(" ");
        if (first.length > 60) first = first.slice(0, 59) + "…";
        return first + " · " + (SHOW_LABELS[s.show] || s.show || "from result");
      }
      default: return "";
    }
  }

  // ---- the column list as sentences (gear mock-up, mock-summary-mark) ----

  var FUNC_WORDS = { count: "Count", count_distinct: "Count distinct",
                     sum: "Sum", mean: "Mean", median: "Median", min: "Min",
                     max: "Max", identity: "As is" };

  /** One line saying what the column computes, in words. */
  function summaryDesc(s) {
    var fw = function (f) { return FUNC_WORDS[f] || f; };
    switch (s.type) {
      case "simple": {
        var f = s.func || "count";
        if (f === "count") return "Count of rows";
        if (f === "identity") return (s.col || "?") + " as is";
        return fw(f) + " " + (f === "count_distinct" ? "" : "of ") + (s.col || "?");
      }
      case "dist": return "Distribution of " + (s.col || "?");
      case "field": return "Values of " + (s.col || "?");
      case "series": return (s.col || "?") + " over " + (s.x || "?");
      case "spans": return "Events, " + (s.x || "?") + " to " + (s.xend || "?");
      case "pair": {
        var a = s.x_func && s.x_func !== "identity"
          ? fw(s.x_func) + " " + (s.x || "?") : (s.x || "?");
        var b = (s.xend_func || "max") === "identity" ? (s.xend || "?")
          : fw(s.xend_func || "max").toLowerCase() + " " + (s.xend || "?");
        return a + " to " + b;
      }
      case "expr": return s.expr || "";
      case "custom": return summaryBody(s).replace(/ · [^·]*$/, "");
      default: return "";
    }
  }

  /** What splits, repeats or paints the column, as words after its
   *  description: ", split by sex, faceted by arm". */
  function mapPhrase(s) {
    var out = "";
    SUMMARY_MAPS.forEach(function (m) {
      if (s[m.key] && m.ok(s)) out += ", " + m.phrase(s[m.key]);
    });
    return out;
  }

  /** The collapsed row as a sentence: a lead in the row's weight (the
   *  column's name, else the word for what it computes) and the rest after
   *  it, muted. "Count of rows, split by sex"; "Subjects: count distinct
   *  USUBJID". */
  function summaryLead(s) {
    var desc = summaryDesc(s);
    var maps = mapPhrase(s);
    var name = String(s.name || "").trim();
    if (name && desc.toLowerCase().indexOf(name.toLowerCase()) !== 0) {
      return { lead: name,
               rest: (desc ? desc.charAt(0).toLowerCase() + desc.slice(1) : "") +
                 maps };
    }
    var heads = Object.keys(FUNC_WORDS).map(function (k) { return FUNC_WORDS[k]; })
      .concat(["Distribution", "Values", "Events"])
      .sort(function (a, b) { return b.length - a.length; });
    for (var i = 0; i < heads.length; i++) {
      var h = heads[i];
      if (desc === h || desc.indexOf(h + " ") === 0) {
        return { lead: h, rest: (desc.slice(h.length).trim() + maps)
          .replace(/^, /, "") };
      }
    }
    return { lead: desc, rest: maps.replace(/^, /, "") };
  }

  /** The default header R gives an unnamed column
   *  (lane_summary_auto_name()), for the Name field's placeholder: a data
   *  column reads as its variable label where it has one. `cols` is the
   *  gear's column list ({ name, label }). */
  function autoName(s, cols) {
    var nm = function (c) {
      if (!c) return "";
      var hit = (cols || []).filter(function (x) { return x.name === c; })[0];
      return (hit && hit.label && hit.label !== c) ? hit.label : c;
    };
    switch (s.type) {
      case "simple": {
        var f = s.func || "count";
        if (f === "count") return "Rows";
        if (f === "identity") return nm(s.col) || "Value";
        var word = { count_distinct: "Distinct", mean: "Mean", median: "Median",
                     sum: "Sum", min: "Min", max: "Max" }[f] || f;
        return word + " " + nm(s.col);
      }
      case "dist": case "field": case "series": return nm(s.col);
      case "spans": return nm(s.x) + " \u2192 " + nm(s.xend);
      case "pair": return nm(s.x) + " \u2192 " + nm(s.xend);
      default: return "Value";
    }
  }

  /** The mark a row draws: its `show`, or the type's first (custom: "auto"). */
  function markOf(s) {
    var t = SUMMARY_TYPES[s.type] || SUMMARY_TYPES.simple;
    return s.show || t.shows[0];
  }

  /** A preset row as the custom function that computes the same frame, so
   *  the code switch starts from working code. NULL where no custom mark
   *  draws the preset's (series, spans). */
  function presetToFn(s) {
    var col = function (c) { return "d$" + (/^[A-Za-z.][A-Za-z0-9._]*$/.test(c) ? c : "`" + c + "`"); };
    var agg = function (f, c) {
      switch (f) {
        case "count": return "dplyr::n()";
        case "count_distinct": return "dplyr::n_distinct(" + c + ")";
        case "identity": return "dplyr::first(" + c + ")";
        default: return f + "(" + c + ", na.rm = TRUE)";
      }
    };
    var by = s.color ? ", .by = " + s.color : "";
    switch (s.type) {
      case "simple": {
        var f = s.func || "count";
        var c = s.col || "";
        return "\\(d) d |>\n  dplyr::summarise(value = " + agg(f, c) + by + ")";
      }
      case "pair":
        return "\\(d) d |>\n  dplyr::summarise(\n    from = " +
          agg(s.x_func || "identity", s.x || "") + ",\n    to = " +
          agg(s.xend_func || "max", s.xend || "") + by + "\n  )";
      case "dist": {
        var v = s.col || "";
        // Quartiles and the range: the presets' whisker rules (Tukey, the
        // CIs) are statistics of their own; edit the function to match.
        return "\\(d) d |>\n  dplyr::summarise(\n" +
          "    lo = min(" + v + ", na.rm = TRUE),\n" +
          "    q1 = stats::quantile(" + v + ", 0.25, na.rm = TRUE),\n" +
          "    mid = stats::median(" + v + ", na.rm = TRUE),\n" +
          "    q3 = stats::quantile(" + v + ", 0.75, na.rm = TRUE),\n" +
          "    hi = max(" + v + ", na.rm = TRUE)" + by + "\n  )";
      }
      case "field":
        return "\\(d) data.frame(text = unique(as.character(" + col(s.col || "") + ")))";
      case "expr":
        return "\\(d) d |>\n  dplyr::summarise(value = " + (s.expr || "NA") + ")";
      default: return null;
    }
  }

  /** The custom row a preset becomes under the code switch. */
  function presetToCustom(s) {
    var fn = presetToFn(s);
    if (fn == null) return null;
    var show = { bar: "bar", number: "number", dot: "bar", dumbbell: "dumbbell",
                 box: "box", pointrange: "pointrange", text: "text" }[markOf(s)];
    if (s.type === "field" || s.type === "expr") show = s.type === "field" ? "text" : "number";
    if (s.type === "dist" && markOf(s) === "text") show = "box";
    var out = { type: "custom", name: s.name || "", fn: fn };
    if (show) out.show = show;
    if (s.facet) out.facet = s.facet;
    // A count against its population keeps its N and percent.
    if (s.type === "simple" && s.func === "count_distinct" && s.col) out.denom = s.col;
    return out;
  }

  var GRIP_SVG = '<svg width="10" height="16" viewBox="0 0 10 16"><g fill="currentColor">' +
    '<circle cx="3" cy="3.5" r="1"/><circle cx="7" cy="3.5" r="1"/><circle cx="3" cy="8" r="1"/>' +
    '<circle cx="7" cy="8" r="1"/><circle cx="3" cy="12.5" r="1"/><circle cx="7" cy="12.5" r="1"/></g></svg>';
  var CHEV_SVG = '<svg width="12" height="12" viewBox="0 0 12 12"><path d="M3 4.5l3 3 3-3" fill="none" ' +
    'stroke="currentColor" stroke-width="1.4" stroke-linecap="round" stroke-linejoin="round"/></svg>';
  var CODE_SVG = '<svg width="14" height="14" viewBox="0 0 16 16"><path d="M5.5 4.5L2 8l3.5 3.5M10.5 4.5L14 8l-3.5 3.5" ' +
    'fill="none" stroke="currentColor" stroke-width="1.4" stroke-linecap="round" stroke-linejoin="round"/></svg>';

  /** The seed for a colour / facet mapping: the first categorical that a
   *  reader can actually decode. `n_lev` rides on the gear's column list
   *  (summarize_gear_cols), so adding a mapping lands on AESEV rather than on
   *  USUBJID, whose 200 levels R rejects outright (LANE_MAX_LEVELS). */
  var MAX_MAP_LEVELS = 15;
  function firstMapCol(cols) {
    for (var i = 0; i < cols.length; i++) {
      var c = cols[i];
      if (c.type !== "numeric" && c.n_lev >= 2 && c.n_lev <= MAX_MAP_LEVELS) {
        return c.name;
      }
    }
    return "";
  }

  /** First column of a type, for preset seeds. */
  function firstCol(cols, want, skip) {
    for (var i = 0; i < cols.length; i++) {
      var ok = want === "any" || (want === "num"
        ? cols[i].type === "numeric" : cols[i].type !== "numeric");
      if (ok && (!skip || skip.indexOf(cols[i].name) === -1)) return cols[i].name;
    }
    return "";
  }



  /** The Columns section: the table's columns as a list of rows (design
   *  system, "Rows"). A row is one line that says what the column computes;
   *  a click opens its editor below that line, inside the same row, with the
   *  fields on the tray's grid. One row open at a time.
   *  ctx: { cfg(), cols(), send(list), rerender(), open: Set, stash: {} } */
  function renderSummariesEditor(sec, ctx) {
    var cfg = ctx.cfg();
    var cols = ctx.cols();
    var list = Array.isArray(cfg.summaries) ? cfg.summaries : (cfg.summaries = []);
    var B = (typeof Blockr !== "undefined") ? Blockr : null;
    var S = (B && B.Select) || null;
    var commit = function () { ctx.send(list.slice()); };
    var edit = function () { commit(); ctx.rerender(); };

    function colOpts(want) {
      return cols.filter(function (c) {
        return want === "any" ||
          (want === "num" ? c.type === "numeric" : c.type !== "numeric");
      }).map(function (c) { return { value: c.name, label: c.label || c.name }; });
    }
    function tip(el, text) {
      el.setAttribute("data-blockr-tooltip", text);
      el.setAttribute("aria-label", text);
    }
    // One field of the grid: the label above its control, as the engine's
    // rows (.dd-form-row). `onRemove` makes it an optional mapping: the
    // label carries the engine's remove button, as the chart's optional
    // roles do.
    function field(parent, label, opts) {
      opts = opts || {};
      var f = document.createElement("div");
      f.className = "lane-sum-ctl" + (opts.wide ? " lane-sum-ctl-wide" : "");
      var head = document.createElement("div");
      head.className = "dd-row-head";
      var l = document.createElement("span");
      l.className = "blockr-label";
      l.textContent = label;
      head.appendChild(l);
      if (opts.onRemove) {
        var x = document.createElement("button");
        x.type = "button";
        x.className = "dd-role-remove";
        tip(x, "Remove " + label.toLowerCase());
        x.textContent = "✕";
        x.addEventListener("click", opts.onRemove);
        head.appendChild(x);
      }
      f.appendChild(head);
      parent.appendChild(f);
      return f;
    }
    function selectCtl(parent, label, want, selected, onChange, onRemove) {
      var f = field(parent, label, { onRemove: onRemove });
      var wrap = document.createElement("div");
      wrap.className = "dd-picker-wrap";
      var opts = typeof want === "string" ? colOpts(want) : want;
      var sel = selected || "";
      var pick = onChange;
      if (typeof want !== "string") {
        // A fixed option set shows its label alone ("Mean", not "mean
        // Mean"), as the chart tray does; columns keep name and label.
        var toVal = {};
        opts = opts.map(function (o) {
          if (!o || typeof o !== "object" || !o.label) return o;
          toVal[o.label] = o.value;
          if (o.value === selected) sel = o.label;
          return o.label;
        });
        pick = function (v) {
          onChange(Object.prototype.hasOwnProperty.call(toVal, v) ? toVal[v] : v);
        };
      }
      if (S && S.single) {
        S.single(wrap, { bordered: true, options: opts, selected: sel,
                         label: label, onChange: pick });
      }
      f.appendChild(wrap);
    }
    function multiCtl(parent, label, selected, onChange) {
      var f = field(parent, label, { wide: true });
      var wrap = document.createElement("div");
      wrap.className = "dd-picker-wrap";
      if (S && S.multi) {
        S.multi(wrap, {
          bordered: true,
          options: colOpts("any"),
          selected: (selected || []).slice(),
          placeholder: "None",
          label: label,
          onChange: onChange
        });
      }
      f.appendChild(wrap);
    }
    // Two or three fixed short values: the segmented control.
    function segCtl(parent, label, options, selected, onChange) {
      var f = field(parent, label);
      var wrap = document.createElement("div");
      wrap.className = "dd-pill-wrap";
      if (B && B.segmented) {
        wrap.appendChild(B.segmented(options, selected, onChange,
                                     { label: label }).el);
      }
      f.appendChild(wrap);
    }
    // A name or a number: the text field that commits on Enter or blur,
    // with the ↵ chip while it holds an edit (Blockr.textCommit).
    function textCtl(parent, label, value, placeholder, onCommit, wide) {
      var f = field(parent, label, { wide: wide });
      var wrap = document.createElement("div");
      wrap.className = "dd-text-wrap";
      var inp = document.createElement("input");
      inp.type = "text";
      inp.className = "blockr-text-input";
      inp.value = value == null ? "" : String(value);
      if (placeholder) inp.placeholder = placeholder;
      inp.setAttribute("aria-label", label);
      wrap.appendChild(inp);
      f.appendChild(wrap);
      if (B && B.textCommit) {
        B.textCommit(inp, { onCommit: onCommit });
      } else {
        inp.addEventListener("change", function () { onCommit(inp.value); });
      }
    }
    // What the column draws: the segmented control for two or three marks,
    // a select past that (design system, "Which control for which
    // decision"). A tile is the compact look's mark, offered there and on a
    // column that already draws one.
    function showCtl(parent, s, t) {
      var shows = ((SUMMARY_TYPES[t] || {}).shows || []).filter(function (sh) {
        return sh !== "tile" || cfg.look === "compact" || s.show === "tile";
      });
      if (shows.length < 2) return;
      var cur = (t === "custom" && !s.show) ? "auto" : (s.show || shows[0]);
      var opts = shows.map(function (sh) {
        return { value: sh, label: SHOW_WORDS[sh] || sh };
      });
      var apply = function (sh) {
        if (t === "custom" && sh === "auto") { delete s.show; edit(); return; }
        s.show = sh;
        // For a distribution the mark is the STYLE: switching does not
        // change which numbers the column holds, so the ranges are pinned
        // to what they already were rather than left to the default the
        // new `show` would imply.
        if (t === "dist" && sh !== "text") {
          s.style = sh === "box" ? "box" : "dot";
          s.inner = distInner(s);
          s.outer = distOuter(s);
        }
        edit();
      };
      if (opts.length <= 3) segCtl(parent, "Show", opts, cur, apply);
      else selectCtl(parent, "Show", opts, cur, apply);
    }

    var wrap = document.createElement("div");
    wrap.className = "dd-summaries lane-summaries";

    var moveTo = function (from, to) {
      if (to < 0 || to >= list.length || to === from) return;
      var wasOpen = ctx.open.has(from);
      list.splice(to, 0, list.splice(from, 1)[0]);
      ctx.open.clear();
      if (wasOpen) ctx.open.add(to);
      ctx.stash = {};
      ctx.focus = to;
      edit();
    };

    list.forEach(function (s, i) {
      var isOpen = ctx.open.has(i);
      var row = document.createElement("div");
      row.className = "lane-sum-row" + (isOpen ? " is-open" : "");
      row.setAttribute("data-sum-index", String(i));

      var head = document.createElement("div");
      head.className = "lane-sum-head";
      var grip = document.createElement("span");
      grip.className = "lane-sum-grip";
      grip.setAttribute("aria-hidden", "true");
      grip.innerHTML = GRIP_SVG;
      head.appendChild(grip);

      // The row's one line, a button: a click or Enter opens the editor,
      // Alt+Up / Alt+Down move the column (design system, "Block lists").
      var toggle = document.createElement("button");
      toggle.type = "button";
      toggle.className = "lane-sum-toggle";
      toggle.setAttribute("aria-expanded", isOpen ? "true" : "false");
      var glyph = document.createElement("span");
      glyph.className = "lane-sum-glyph";
      glyph.innerHTML = SHOW_ICONS[markOf(s)] || "";
      tip(glyph, SHOW_WORDS[markOf(s)] || markOf(s));
      var sentence = summaryLead(s);
      var line = document.createElement("span");
      line.className = "lane-sum-line";
      var lead = document.createElement("span");
      lead.className = "lane-sum-lead";
      lead.textContent = sentence.lead;
      line.appendChild(lead);
      if (sentence.rest) {
        line.appendChild(document.createTextNode(" " + sentence.rest));
      }
      toggle.appendChild(glyph);
      toggle.appendChild(line);
      toggle.addEventListener("keydown", function (e) {
        if (!e.altKey || (e.key !== "ArrowUp" && e.key !== "ArrowDown")) return;
        e.preventDefault();
        moveTo(i, i + (e.key === "ArrowUp" ? -1 : 1));
      });
      head.appendChild(toggle);

      if (isOpen) {
        // The code switch: a preset becomes the function that computes the
        // same frame; back again only to the preset it came from.
        var code = document.createElement("button");
        code.type = "button";
        code.className = "lane-sum-code";
        var asCode = s.type === "custom";
        code.setAttribute("aria-pressed", asCode ? "true" : "false");
        code.innerHTML = CODE_SVG;
        var canGo = asCode ? !!ctx.stash[i] : presetToFn(s) != null;
        code.disabled = !canGo;
        tip(code, asCode
          ? (canGo ? "Back to the preset" : "Started as code")
          : (canGo ? "Edit as code" : "No code form for this mark"));
        code.addEventListener("click", function (e) {
          e.stopPropagation();
          if (asCode) {
            list[i] = ctx.stash[i];
            delete ctx.stash[i];
          } else {
            var cu = presetToCustom(s);
            if (!cu) return;
            ctx.stash[i] = JSON.parse(JSON.stringify(s));
            list[i] = cu;
          }
          edit();
        });
        head.appendChild(code);
      }
      // The table always has a column: the last one cannot go, and says so.
      var rm = document.createElement("button");
      rm.type = "button";
      rm.className = "lane-sum-rm";
      rm.disabled = list.length < 2;
      tip(rm, rm.disabled ? "A table needs one column" : "Remove column");
      rm.innerHTML = '<svg width="12" height="12" viewBox="0 0 12 12"><path d="M3 3l6 6M9 3l-6 6" ' +
        'fill="none" stroke="currentColor" stroke-width="1.3" stroke-linecap="round"/></svg>';
      rm.addEventListener("click", function (e) {
        e.stopPropagation();
        if (list.length < 2) return;
        list.splice(i, 1);
        ctx.open.clear();
        ctx.stash = {};
        edit();
      });
      head.appendChild(rm);
      var chev = document.createElement("span");
      chev.className = "lane-sum-chev";
      chev.setAttribute("aria-hidden", "true");
      chev.innerHTML = (B && B.icons && B.icons.chevron) || CHEV_SVG;
      head.appendChild(chev);
      // A click anywhere on the line but its buttons opens or closes it.
      head.addEventListener("click", function (e) {
        if (e.target.closest && e.target.closest(".lane-sum-code, .lane-sum-rm")) return;
        var was = ctx.open.has(i);
        ctx.open.clear();
        if (!was) ctx.open.add(i);
        ctx.focus = i;
        ctx.rerender();
      });
      // Reorder by dragging the row.
      row.draggable = true;
      row.addEventListener("dragstart", function (e) {
        e.dataTransfer.effectAllowed = "move";
        e.dataTransfer.setData("text/plain", String(i));
        row.classList.add("is-dragging");
      });
      row.addEventListener("dragend", function () { row.classList.remove("is-dragging"); });
      row.addEventListener("dragover", function (e) {
        e.preventDefault();
        row.classList.add("is-drop");
      });
      row.addEventListener("dragleave", function () { row.classList.remove("is-drop"); });
      row.addEventListener("drop", function (e) {
        e.preventDefault();
        row.classList.remove("is-drop");
        var from = Number(e.dataTransfer.getData("text/plain"));
        if (isNaN(from)) return;
        moveTo(from, i);
      });
      row.appendChild(head);

      if (isOpen) {
        var body = document.createElement("div");
        body.className = "lane-sum-body";
        // The fields of an open row are not draggable: a drag that starts
        // in a select or a text field is a text selection.
        body.addEventListener("dragstart", function (e) {
          if (e.target !== row) e.stopPropagation();
        });
        renderColumnFields(body, s);
        row.appendChild(body);
      }
      wrap.appendChild(row);
    });

    // The fields of one column, in the order they are set: what it is
    // called, what it computes, how it is drawn, what splits it.
    function renderColumnFields(body, s) {
      textCtl(body, "Name", s.name, autoName(s, cols), function (v) {
        s.name = v;
        edit();
      });

      var t = s.type || "simple";
      if (t === "simple") {
        // The full aggregate set INCLUDING identity ("None (as is)"): a
        // value already reduced upstream, one row per group, drawn as is.
        // The chart block's duality and its label.
        selectCtl(body, "Aggregate", FUNC_OPT, s.func || "count", function (v) {
          s.func = v;
          // A distinct count reads the subject column; the rest read a
          // number.
          if (v !== "count" && !s.col) {
            s.col = v === "count_distinct"
              ? SUMMARY_MAPS.filter(function (m) { return m.key === "denom"; })[0].seed(cols)
              : firstCol(cols, "num");
          }
          edit();
        });
        if ((s.func || "count") !== "count") {
          selectCtl(body,
            s.func === "count_distinct" ? "Distinct of" : "Of column",
            s.func === "count_distinct" ? "any" : "num",
            s.col, function (v) { s.col = v; edit(); });
        }
      } else if (t === "dist") {
        selectCtl(body, "Value", "num", s.col, function (v) { s.col = v; edit(); });
      } else if (t === "field") {
        selectCtl(body, "Column", "any", s.col, function (v) { s.col = v; edit(); });
      } else if (t === "series") {
        selectCtl(body, "Order (x)", "num", s.x, function (v) { s.x = v; edit(); });
        selectCtl(body, "Value (y)", "num", s.col, function (v) { s.col = v; edit(); });
      } else if (t === "spans") {
        selectCtl(body, "Start", "num", s.x, function (v) { s.x = v; edit(); });
        selectCtl(body, "End", "num", s.xend, function (v) { s.xend = v; edit(); });
      } else if (t === "pair") {
        // Two summaries of the group on one segment.
        var PAIR_FUNCS = [
          { value: "identity", label: "None (as is)" },
          { value: "mean", label: "Mean" },
          { value: "median", label: "Median" },
          { value: "min", label: "Min" },
          { value: "max", label: "Max" }
        ];
        selectCtl(body, "From ◇", "num", s.x, function (v) { s.x = v; edit(); });
        selectCtl(body, "From function", PAIR_FUNCS, s.x_func || "identity",
          function (v) { s.x_func = v; edit(); });
        selectCtl(body, "To ●", "num", s.xend, function (v) { s.xend = v; edit(); });
        selectCtl(body, "To function", PAIR_FUNCS, s.xend_func || "max",
          function (v) { s.xend_func = v; edit(); });
      } else if (t === "expr") {
        textCtl(body, "Expression", s.expr, "e.g. sd(DUR)/mean(DUR) * 100",
          function (v) { s.expr = v; edit(); }, true);
      } else if (t === "custom") {
        // Code commits on its own button (or Mod+Enter), never per
        // keystroke: every commit reruns the function for every level.
        var fnCtl = field(body, "Summary of d, grouped by table row", { wide: true });
        var fnIn = document.createElement("textarea");
        fnIn.className = "blockr-text-input dd-script-editor lane-sum-fn";
        fnIn.rows = Math.max(3, String(s.fn || "").split("\n").length + 1);
        fnIn.spellcheck = false;
        fnIn.value = s.fn || "";
        fnIn.placeholder = "\\(d) d |> dplyr::count(AESEV, name = \"value\")";
        fnIn.setAttribute("aria-label", "Summary function");
        var fnFoot = document.createElement("div");
        fnFoot.className = "lane-sum-fn-foot";
        var fnHint = document.createElement("span");
        fnHint.className = "dd-form-help";
        fnHint.textContent = "Return value; from, to; lo, q1, mid, q3, hi; " +
          "or text. One more column splits the cell.";
        var fnApply = document.createElement("button");
        fnApply.type = "button";
        fnApply.className = "blockr-btn blockr-btn--main blockr-btn--xs";
        fnApply.textContent = "Apply";
        var applyFn = function () {
          if (fnIn.value === (s.fn || "")) return;
          s.fn = fnIn.value;
          edit();
        };
        fnApply.addEventListener("click", applyFn);
        fnIn.addEventListener("keydown", function (e) {
          if (e.key === "Enter" && (e.metaKey || e.ctrlKey)) {
            e.preventDefault();
            applyFn();
          }
        });
        fnFoot.appendChild(fnHint);
        fnFoot.appendChild(fnApply);
        fnCtl.appendChild(fnIn);
        fnCtl.appendChild(fnFoot);
      }

      showCtl(body, s, t);

      // How the mark is drawn, once the mark is picked.
      if (t === "dist") {
        // The glyph is a centre plus two optional ranges. Both offer
        // "(none)", which is where the IQR bar, the plain dot range and the
        // bare dot live -- so the list never needs a mark per shape.
        if (s.show !== "text") {
          selectCtl(body, "Inner range", RANGE_OPT, distInner(s), function (v) {
            s.summary = v; delete s.inner; delete s.stat; edit();
          });
          selectCtl(body, "Outer range", OUTER_OPT, distOuter(s), function (v) {
            s.whiskers = v; delete s.outer; edit();
          });
        } else {
          selectCtl(body, "Statistic", LANE_STATS, distInner(s), function (v) {
            s.summary = v; delete s.inner; delete s.stat; edit();
          });
        }
      } else if (t === "series") {
        var band = (s.band && s.band.length === 2) ? s.band : ["", ""];
        selectCtl(body, "Band low", "num", band[0], function (v) {
          s.band = [v, band[1]]; edit();
        });
        selectCtl(body, "Band high", "num", band[1], function (v) {
          s.band = [band[0], v]; edit();
        });
        // The computed reference: a pooled line or band, computed in R
        // over the column's values (per facet level under a facet).
        selectCtl(body, "Reference", [
          { value: "none", label: "None" },
          { value: "mean", label: "Mean" },
          { value: "mean_sd", label: "Mean ± SD" },
          { value: "median_iqr", label: "Median · IQR" }
        ], s.ref || "none", function (v) { s.ref = v; edit(); });
      } else if (t === "spans") {
        // Event identity on hover: the label headlines the segment tooltip
        // and keys the same-event highlight; the tooltip fields append.
        selectCtl(body, "Label (hover)", "any", s.label, function (v) {
          s.label = v; edit();
        });
        segCtl(body, "Width", [
          { value: "md", label: "Regular" },
          { value: "lg", label: "Wide" }
        ], s.size || "md", function (v) { s.size = v; edit(); });
        multiCtl(body, "Tooltip fields", s.tooltip, function (vals) {
          s.tooltip = vals; edit();
        });
      } else if (t === "pair") {
        // A band bound or the reference takes a number; a band bound may
        // also name a column (a per-subject range such as ANRLO / ANRHI).
        [["ref_lo", "Range low", "Number or column"],
         ["ref_hi", "Range high", "Number or column"],
         ["ref", "Reference line", "e.g. 20"]].forEach(function (d) {
          textCtl(body, d[1], s[d[0]], d[2], function (v) {
            v = String(v).trim();
            if (v === "") delete s[d[0]];
            else s[d[0]] = isFinite(Number(v)) ? Number(v) : v;
            edit();
          });
        });
      }

      // The column's own optional mappings: a set one is a field with its
      // remove button; an unset one is an offer, a dashed "+ Color" as the
      // sentence offers a setting it does not name. A pair's "Dashed by"
      // is one of them.
      var maps = SUMMARY_MAPS.filter(function (m) { return m.ok(s); });
      if (t === "pair") {
        maps = maps.concat([{ key: "dash", label: "Dashed by",
          hint: "Draw the link dashed for this column's non-first levels" }]);
      }
      var offers = [];
      maps.forEach(function (m) {
        if (!s[m.key]) { offers.push(m); return; }
        selectCtl(body, m.label, m.want || "cat", s[m.key], function (v) {
          s[m.key] = v; edit();
        }, function () { delete s[m.key]; edit(); });
        // A colour-split bar's layout, the chart's `position`, after the
        // split it lays out.
        if (m.key === "color" && t === "simple" && (s.show || "bar") === "bar") {
          segCtl(body, "Split", [
            { value: "stack", label: "Stacked" },
            { value: "dodge", label: "Grouped" },
            { value: "fill", label: "100%" }
          ], s.position || "stack", function (v) { s.position = v; edit(); });
        }
      });
      if (offers.length) {
        var bar = document.createElement("div");
        bar.className = "lane-sum-offers";
        offers.forEach(function (m) {
          var seedOf = function () {
            return m.seed ? m.seed(cols) : firstMapCol(cols);
          };
          var b = document.createElement("button");
          b.type = "button";
          b.className = "lane-sum-offer";
          b.textContent = "+ " + m.label;
          b.setAttribute("data-blockr-tooltip", m.hint);
          // Nothing decodable to map: the offer would only produce an
          // error message in the table.
          b.disabled = !seedOf();
          b.addEventListener("click", function () {
            s[m.key] = seedOf();
            edit();
          });
          bar.appendChild(b);
        });
        body.appendChild(bar);
      }
    }

    // Add: one menu of summaries, each named by what it computes and shown
    // with the mark it starts with; the last starts from code.
    var seedFor = function (t, f) {
      var ns = { type: t, name: "", show: SUMMARY_TYPES[t].shows[0] };
      if (t === "simple") {
        ns.func = f || "count";
        if (ns.func === "count_distinct") {
          ns.col = SUMMARY_MAPS.filter(function (m) { return m.key === "denom"; })[0].seed(cols);
          ns.name = "Patients";
        } else if (ns.func !== "count") {
          ns.col = firstCol(cols, "num");
          ns.show = "number";
        } else {
          ns.name = "Rows";
        }
      }
      if (t === "custom") {
        delete ns.show;
        ns.name = "Value";
        ns.fn = CUSTOM_SEED;
      }
      if (t === "dist") {
        ns.col = firstCol(cols, "num");
        ns.style = "dot";
        ns.inner = "median_q1_q3";
        ns.outer = "tukey";
      }
      if (t === "field") ns.col = firstCol(cols, "cat");
      if (t === "series") {
        ns.x = firstCol(cols, "num");
        ns.col = firstCol(cols, "num", [ns.x]);
      }
      if (t === "spans") {
        ns.x = firstCol(cols, "num");
        ns.xend = firstCol(cols, "num", [ns.x]);
      }
      if (t === "pair") {
        ns.x = firstCol(cols, "num");
        ns.xend = firstCol(cols, "num", [ns.x]) || ns.x;
        ns.x_func = "identity";
        ns.xend_func = "max";
      }
      return ns;
    };
    var addOne = function (t, f) {
      list.push(seedFor(t, f));
      ctx.open.clear();
      ctx.open.add(list.length - 1);
      ctx.focus = list.length - 1;
      edit();
    };
    var ADD_ITEMS = [
      { title: "Counts" },
      { label: "Count", meta: "rows", icon: SHOW_ICONS.bar, t: "simple", f: "count" },
      { label: "Count distinct", meta: "patients", icon: SHOW_ICONS.bar, t: "simple", f: "count_distinct" },
      { title: "Values" },
      { label: "One value", meta: "mean, sum, max", icon: SHOW_ICONS.number, t: "simple", f: "mean" },
      { label: "Distribution", meta: "box or dot range", icon: SHOW_ICONS.box, t: "dist" },
      { label: "Two values, from → to", meta: "onset to end", icon: SHOW_ICONS.dumbbell, t: "pair" },
      { title: "Rows of the cell" },
      { label: "Events", meta: "a swimlane", icon: SHOW_ICONS.interval, t: "spans" },
      { label: "Series", meta: "a sparkline", icon: SHOW_ICONS.sparkline, t: "series" },
      { label: "Text", meta: "the distinct values", icon: SHOW_ICONS.text, t: "field" },
      { label: "Expression", meta: "one R expression", icon: SHOW_ICONS.number, t: "expr" },
      { divider: true },
      { label: "Custom summary", meta: "a function of the rows", icon: CODE_SVG, t: "custom" }
    ];
    // "Add column" under the list: the quiet add button of a row list
    // (blockr.ui's .blockr-add-link, as "Add condition"), opening the menu.
    var addRow = document.createElement("div");
    addRow.className = "blockr-add-row lane-sum-addrow";
    var addBtn = document.createElement("button");
    addBtn.type = "button";
    addBtn.className = "blockr-add-link lane-sum-addcol";
    addBtn.innerHTML = '<span class="blockr-add-icon">' +
      ((B && B.icons && B.icons.plus) ||
       '<svg width="12" height="12" viewBox="0 0 12 12"><path d="M6 2v8M2 6h8" ' +
       'fill="none" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"/></svg>') +
      "</span>Add column";
    if (B && B.menu) {
      addBtn.setAttribute("aria-haspopup", "menu");
      addBtn.addEventListener("click", function () {
        B.menu(addBtn, {
          minWidth: 340,
          items: ADD_ITEMS.map(function (it) {
            if (it.title || it.divider) return it;
            return { label: it.label, meta: it.meta, icon: it.icon,
                     onSelect: function () { addOne(it.t, it.f); } };
          })
        });
      });
      addRow.appendChild(addBtn);
    } else {
      // Without blockr.ui's menu: the same entries as offers.
      var addWrap = document.createElement("div");
      addWrap.className = "lane-sum-offers";
      ADD_ITEMS.forEach(function (it) {
        if (it.title || it.divider) return;
        var b = document.createElement("button");
        b.type = "button";
        b.className = "lane-sum-offer";
        b.textContent = "+ " + it.label;
        b.addEventListener("click", function () { addOne(it.t, it.f); });
        addWrap.appendChild(b);
      });
      addRow.appendChild(addWrap);
    }
    wrap.appendChild(addRow);
    sec.appendChild(wrap);

    // Keep the keyboard on the row it acted on: a re-render replaces it.
    if (ctx.focus != null) {
      var again = wrap.querySelector('[data-sum-index="' + ctx.focus + '"] .lane-sum-toggle');
      ctx.focus = null;
      if (again && again.focus) again.focus();
    }
  }

  // The groups layout's gear: the grouping, then the column list, then how
  // the table looks. One model, `by` + `summaries`; the column list is
  // never empty (a new table starts with one count column).
  function summarizeSections(cfg, ctx) {
    var list = Array.isArray(cfg.summaries) ? cfg.summaries : [];

    // The cards look leaves out what a card cannot draw (R:
    // CARDS_SKIP_TYPES), and says so under the look selector.
    var skip = list
      .filter(function (s) { return s && (s.type === "spans" || s.type === "expr"); })
      .map(function (s) { return s.name || s.col || s.type; });
    SUMMARIZE_ROLES.look.hint = (cfg.look === "cards" && skip.length)
      ? "Not drawn as cards: " + skip.join(", ") +
        ". The table look shows them."
      : "";

    // Which facet columns the table uses: the by-level reading spans ONE
    // column's groups across the summaries, so it is offered only when
    // every faceted column agrees on it.
    var fcols = [];
    list.forEach(function (s) {
      if (s && s.facet && fcols.indexOf(s.facet) === -1) fcols.push(s.facet);
    });
    // Without `by` the table is one row over all input rows: nothing to
    // sort, search or drill, so those controls leave.
    var grouped = Array.isArray(cfg.by) && cfg.by.length > 0;
    var pres = fcols.length === 1
      ? ["sort_by", "sort_dir", "facet_layout", "value_labels", "search",
         "sortable", "axis", "pct_of", "missing", "download", "look",
         "bar_width"]
      : ["sort_by", "sort_dir", "value_labels", "search", "sortable",
         "axis", "pct_of", "missing", "download", "look", "bar_width"];
    if (!grouped) {
      pres = pres.filter(function (k) {
        return ["sort_by", "sort_dir", "search", "sortable"].indexOf(k) === -1;
      });
    }
    return {
      requiredMap: [],
      // Colour and facet are the COLUMN's mappings, set in the column list.
      // Only the grouping is the whole table's, optional. Under the layout
      // switch it moves into the "Rows are" section (withLayoutSwitch()).
      optionalMap: [],
      mapping: ["by"],
      aggTitle: null,
      customSections: ctx ? [{
        title: "Columns",
        render: function (sec) { renderSummariesEditor(sec, ctx); }
      }] : [],
      presentation: pres,
      drillToggle: grouped ? "drill" : null,
      drillDefault: grouped ? cfg.by[cfg.by.length - 1] : "",
      // A click claims the row's grouping path, so there is no column to
      // pick: `drill` is the on switch.
      drillPicker: false,
      ctrlSection: grouped,
      drillHint: cfg.drill
        ? "Clicking a row filters downstream on " + cfg.drill + "."
        : "Clicking a row filters downstream blocks to that row.",
      titles: ["title", "subtitle", "caption"]
    };
  }


  /** Read the gear's working state off the rendered table. */
  function readGearState(table) {
    var cfg = {};
    try { cfg = JSON.parse(table.getAttribute("data-summarize-cfg") || "{}"); }
    catch (e) { cfg = {}; }
    var cols = cfg.columns || [];
    var levels = cfg.facet_levels || [];
    var titles = cfg.titles || {};
    delete cfg.columns;
    delete cfg.facet_levels;
    delete cfg.titles;
    // The prepare script's control values (sv_<name>) sit on the config
    // itself, where the engine's script roles read and write them.
    var sv = cfg.script_values || {};
    delete cfg.script_values;
    Object.keys(sv).forEach(function (k) { cfg[k] = sv[k]; });
    // The three text slots need null (auto) vs "" (explicitly none), which the
    // JSON carries; `*_auto` surfaces the inherited text so clearing the field
    // commits "" and turns the auto title off.
    cfg.title = titles.title_state === undefined ? null : titles.title_state;
    cfg.subtitle = titles.subtitle_state === undefined ? null : titles.subtitle_state;
    cfg.caption = titles.caption_state === undefined ? null : titles.caption_state;
    cfg.title_auto = titles.title || "";
    cfg.subtitle_auto = titles.subtitle || "";
    cfg.caption_auto = titles.caption || "";
    // A `null` column pick reads as "" for the pickers (the engine's
    // no-value). `group`, `color` and `facet` are sentence words only.
    ["group", "color", "facet", "drill"]
      .forEach(function (k) { if (cfg[k] == null) cfg[k] = ""; });
    // The summarize-table config: the column list and the grouping vector.
    if (!Array.isArray(cfg.summaries)) {
      cfg.summaries = cfg.summaries ? [cfg.summaries] : [];
    }
    if (!Array.isArray(cfg.by)) cfg.by = cfg.by ? [String(cfg.by)] : [];
    if (!cfg.facet_layout) cfg.facet_layout = "by_summary";
    // The ctrl-send tail wants a string target and an array of choices.
    if (cfg.ctrl_target == null) cfg.ctrl_target = "";
    if (!Array.isArray(cfg.ctrl_choices)) cfg.ctrl_choices = [];
    // Sort targets: the measure / the name, plus facet levels, plus (in the
    // summarize-table mode) each summary column by its name.
    var sumNames = (cfg.summaries || []).map(function (s) {
      return s && s.name ? { value: s.name, label: s.name } : null;
    }).filter(Boolean);
    SUMMARIZE_ROLES.sort_by.options = [
      { value: "value", label: "Measure" },
      // The order the data itself carries -- factor levels, else
      // first-appearance in the rows (chart parity: "Data order"). A visit
      // or dose-group table reads wrong alphabetically.
      { value: "data", label: "Data order" },
      { value: "label", label: "Name" }
    ].concat(sumNames)
      .concat(levels.map(function (lv) { return { value: lv, label: lv }; }))
      // Numeric columns of the input (the engine expands "#num"): the row's
      // MINIMUM of that column orders the table. AVISITN is the ordering a
      // character AVISIT does not carry.
      .concat(["#num"]);
    return { cfg: cfg, cols: cols };
  }

  // The table block's "Rows are" switch (R/table-block.R): records or
  // groups. The block's server reads it off this engine's action input.
  // The grouping is what the switch decides a row is, so it sits under the
  // switch, in the same section, rather than in a Mapping section of its
  // own (design system: a mode selector gates the fields below it).
  function withLayoutSwitch(spec, elemId) {
    spec.layoutSwitch = {
      fields: spec.mapping || [],
      value: "groups",
      options: [
        { value: "records", label: "Records" },
        { value: "groups", label: "Groups" },
        { value: "variables", label: "Variables" }
      ],
      onPick: function (v) { sendConfig(elemId, "layout", v); }
    };
    spec.mapping = [];
    return spec;
  }

  function sendConfig(elemId, param, value) {
    if (!window.Shiny || !Shiny.setInputValue) return;
    Shiny.setInputValue(
      elemId + "_action",
      { action: "config", param: param, value: value },
      { priority: "event" }
    );
  }

  function buildGear(root) {
    var elemId = root.getAttribute("data-summarize-elem-id");
    if (!elemId) return;
    // The table arrives with the first payload, after this chrome: start from
    // whatever is there (possibly nothing). State is re-read ONLY on popover
    // open (followed by engine.refresh(), which rebuilds every control
    // against the fresh objects) -- the table block's contract. NEVER on a
    // payload: the engine's controls capture the cfg OBJECT at build time
    // (drilldown-config.js `const cfg = this._cfg()`), so reassigning it
    // between builds orphans them -- their edits then land in the old object
    // while onChange reads the new one and transmits the STALE value (the
    // second-gear-edit-after-a-render bug).
    var table = root.querySelector("table.blockr-summarize-table");
    var st = table ? readGearState(table) : { cfg: {}, cols: [] };
    var cfg = st.cfg;
    var cols = st.cols;

    // Context for the summaries editor (the customSections seam): live
    // accessors, the transport, a re-render hook (assigned after the
    // engine exists) and the per-row expansion memory.
    var ctx = {
      cfg: function () { return cfg; },
      cols: function () { return cols; },
      send: function (list) { sendConfig(elemId, "summaries", list); },
      rerender: function () {},
      open: new Set(),
      // The preset a column was switched to code from, by row index, so the
      // switch can go back (this session only; a saved board keeps the code).
      stash: {}
    };

    var header = document.createElement("div");
    header.className = "blockr-gear-header";
    var btn = document.createElement("button");
    btn.type = "button";
    btn.className = "blockr-gear-btn";
    btn.setAttribute("data-blockr-tooltip", "Settings");
    btn.setAttribute("aria-label", "Summarize table settings");
    btn.setAttribute("aria-haspopup", "dialog");
    btn.setAttribute("aria-expanded", "false");
    btn.innerHTML = (typeof Blockr !== "undefined" && Blockr.icons)
      ? Blockr.icons.gear : "\u2699";
    header.appendChild(btn);

    // In-flow settings band (not a floating popover): opening pushes the table
    // down, so what is being configured stays visible.
    var pop = document.createElement("div");
    pop.className = "blockr-settings blockr-settings--beak dd-popover";
    pop.setAttribute("data-dd-pop-for", elemId);

    // The prepare script's controls, on the card under the gear row (the
    // chart's strip, chart.css .dd-mapping-band). The engine fills it with
    // the same rows the gear draws, and hides it while the script declares
    // nothing.
    var band = document.createElement("div");
    band.className = "dd-mapping-band";
    band.style.display = "none";

    var DDC = (typeof Blockr !== "undefined" && Blockr.DrilldownConfig) ||
      window.DrilldownConfig;
    if (!DDC) return;
    var engine;
    engine = new DDC({
      popoverEl: function () { return pop; },
      bandEl: function () { return band; },
      roles: SUMMARIZE_ROLES,
      config: function () { return cfg; },
      columns: function () { return cols; },
      context: function () { return "all"; },
      currentType: function () { return null; },
      sections: function () {
        return withLayoutSwitch(summarizeSections(cfg, ctx), elemId);
      },
      sectionsForFamily: function () {
        return withLayoutSwitch(summarizeSections(cfg, ctx), elemId);
      },
      secondary: new Set(),
      // Outside the table block (no layout switch) the grouping keeps the
      // chart's section name.
      mappingTitle: function () { return "Mapping"; },
      // No block-level type picker: the type belongs to the COLUMN (the
      // columns editor's row types + display tiles).
      typeKey: null,
      typeGroups: null,
      familyFor: null,
      // `by` is optional: without it the table is one row over all rows.
      entryRequired: function () { return false; },
      drillAutoLabel: null,
      title: "Summarize table settings",
      onChange: function (key) {
        sendConfig(elemId, key, cfg[key]);
        // The default order is descending; the data's own order read
        // backwards is exactly what "data order" is asked for to avoid, so
        // the pick carries the direction with it. One control change, two
        // params sent -- and the direction stays a control, so reverse
        // chronological is still one click away.
        if (key === "sort_by" && cfg.sort_by === "data" &&
              cfg.sort_dir !== "asc") {
          cfg.sort_dir = "asc";
          sendConfig(elemId, "sort_dir", "asc");
          engine.render();
        }
      },
      onMults: function () {},
      onClearFilter: function () {
        rows(root).forEach(function (r) { r.classList.remove("is-on"); });
        sendConfig(elemId, "filter_values", null);
      },
      ensureDefaults: function () {},
      afterTypeChange: function () {},
      isOpen: function () {
        return pop.classList.contains("blockr-settings--open");
      },
      reopen: function () { openPop(); }
    });
    ctx.rerender = function () { engine.render(); };
    engine.render();

    // Between gear sessions the words and the control strip read the state
    // the server last sent, not the gear's copy from its last open (the gear
    // is built before the first payload, so that copy may be empty). The gear
    // re-reads and rebuilds its controls on open, so this cannot orphan them.
    var sync = function () {
      if (pop.classList.contains("blockr-settings--open")) return;
      var t = root.querySelector("table.blockr-summarize-table");
      if (!t) return;
      var s3 = readGearState(t);
      cfg = s3.cfg;
      cols = s3.cols;
    };
    // The strip is decided by what the sentence names and what the script
    // declares. The gear writes none of those keys, so they are copied onto
    // its config object even while it is open (replacing the object would
    // orphan the open controls): a word added to the subtitle in the gear
    // takes its control off the strip at once, a word removed puts it back.
    var BAND_KEYS = ["sentence_args", "script_inputs", "script_error"];
    root._summarizeBand = function () {
      if (pop.classList.contains("blockr-settings--open")) {
        var t = root.querySelector("table.blockr-summarize-table");
        if (t) {
          var fresh = readGearState(t).cfg;
          BAND_KEYS.forEach(function (k) { cfg[k] = fresh[k]; });
        }
      } else {
        sync();
      }
      engine.renderBand();
    };

    // The words in the title bands: the chart's painter over this gear's
    // engine, so a word and the gear's row cannot disagree. `by` is a list
    // of columns, which the engine keeps out of the sentence (a menu picks
    // one); the word names the innermost column, the one each row is, so
    // it opens a single pick that replaces that column.
    var B = (typeof Blockr !== "undefined") ? Blockr : null;
    if (B && B.SentenceSlots) {
      var lastBy = function () {
        var b = Array.isArray(cfg.by) ? cfg.by : [];
        return b.length ? b[b.length - 1] : "";
      };
      var words = {
        // Synced like the menus below: the sentence paints with the first
        // payload, before the gear has read the script's controls, and a
        // script word's label comes from those.
        _role: function (k) { sync(); return engine._role(k); },
        _slotFlag: function (k) { sync(); return engine._slotFlag(k); },
        _slotNumber: function (k) { sync(); return engine._slotNumber(k); },
        _slotOptionsFor: function (k) {
          sync();
          if (k !== "by") return engine._slotOptionsFor(k);
          return {
            options: engine._colOptionsFor("by", { required: true }),
            selected: lastBy()
          };
        },
        _setRoleValue: function (k, v) {
          if (k !== "by") { engine._setRoleValue(k, v); return; }
          var b = Array.isArray(cfg.by) ? cfg.by.slice() : [];
          if (b.length) b[b.length - 1] = v; else b = [v];
          cfg.by = b;
          sendConfig(elemId, "by", b);
          if (pop.classList.contains("blockr-settings--open")) engine.render();
        }
      };
      root._summarizeSlots = new B.SentenceSlots({
        ddc: function () { return words; },
        config: function () { sync(); return cfg; },
        openGear: function () {
          if (!pop.classList.contains("blockr-settings--open")) btn.click();
        }
      });
    }

    function openPop() {
      pop.classList.add("blockr-settings--open");
      btn.setAttribute("aria-expanded", "true");
    }
    function closePop() {
      pop.classList.remove("blockr-settings--open");
      btn.setAttribute("aria-expanded", "false");
      // Back to the server's state for everything the gear held.
      if (root._summarizeBand) root._summarizeBand();
    }
    btn.addEventListener("click", function (e) {
      e.stopPropagation();
      if (pop.classList.contains("blockr-settings--open")) { closePop(); return; }
      // Re-read state before opening: the config may have moved server-side
      // (state restore, AI / external_ctrl edit) since the gear was built.
      var t = root.querySelector("table.blockr-summarize-table");
      if (t) {
        var s2 = readGearState(t);
        cfg = s2.cfg;
        cols = s2.cols;
      }
      // A table with one column opens on that column's editor, so a new
      // table shows what its first column computes. With more, every row
      // starts closed.
      ctx.open.clear();
      if (Array.isArray(cfg.summaries) && cfg.summaries.length === 1) {
        ctx.open.add(0);
      }
      engine.refresh();
      openPop();
    });

    root.insertBefore(header, root.firstChild);
    root.insertBefore(pop, header.nextSibling);
    root.insertBefore(band, pop.nextSibling);
    // The title band moves into the gear row, on its left: the chart's header
    // (chart/chrome.js _buildDOM). The settings band then opens below the title.
    var titles = root.querySelector(".dd-table-titles");
    if (titles) header.insertBefore(titles, header.firstChild);
    bindCapture(root, header);
    // One control row, chart / table parity: the search box MOVES UP into the
    // gear row and sits LEFT of the gear, which keeps its canonical top-right
    // spot (the cross-block anchor). The emptied chrome row is hidden so it
    // leaves no stray padded border. The legend is NOT in this row -- it has
    // its own row below, so it can never push the search box around.
    var toolbar = root.querySelector(".blockr-html-table-toolbar");
    if (toolbar) {
      header.insertBefore(toolbar, btn);
      var hdrRow = root.querySelector(".blockr-html-table-header");
      if (hdrRow) hdrRow.style.display = "none";
    }
  }

  function bind(root) {
    if (root.dataset[BOUND] === "1") return;
    root.dataset[BOUND] = "1";
    bindSearch(root);
    bindSort(root);
    bindToggle(root);
    bindDrill(root);
    buildGear(root);
    // A payload that arrived before this container existed waits in the store;
    // a container re-created later (dock panel re-mount, view switch) renders
    // from it with no R round trip.
    var stored = storedFor(root);
    if (stored) {
      applyPayload(root, stored);
    } else {
      applyVisibility(root);
      // Nothing for us yet: either the payload has not been built, or it was
      // pushed before this script existed (Shiny drops a custom message with
      // no registered handler). Announce, and let R re-send.
      var id = root.getAttribute("data-summarize-elem-id");
      if (id && window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue(id + "_ready", Date.now(), { priority: "event" });
      }
    }
  }

  function scan(scope) {
    var host = scope && scope.querySelectorAll ? scope : document;
    host.querySelectorAll(".blockr-summarize-container").forEach(bind);
  }

  // ---------- hover readout ----------
  // One fixed tooltip fed by ONE delegated listener for every lane cell on
  // the page: the inverse of the percentage layout. Over an interval segment
  // the payload's exact span wins (data-tip); over the empty track it is the
  // approximate domain value under the cursor (day = fraction × domain).
  // Sparklines snap to the nearest point and read the shipped display
  // values. No per-row handlers, so cost is flat in the row count.
  var laneTip = null;
  // Same-event hover state: the currently highlighted label, cleared when
  // the cursor leaves the segments.
  var laneHl = { root: null, label: null };
  function laneHighlight(root, label) {
    if (laneHl.label === label && laneHl.root === root) return;
    if (laneHl.root) {
      laneHl.root.classList.remove("seg-hover");
      laneHl.root.querySelectorAll(".lane-seg.is-same").forEach(function (s) {
        s.classList.remove("is-same");
      });
    }
    laneHl = { root: null, label: null };
    if (root && label) {
      root.classList.add("seg-hover");
      root.querySelectorAll(".lane-seg").forEach(function (s) {
        if (s.getAttribute("data-l") === label) s.classList.add("is-same");
      });
      laneHl = { root: root, label: label };
    }
  }
  function tipEl() {
    if (!laneTip) {
      laneTip = document.createElement("div");
      laneTip.className = "blockr-lane-tip";
      laneTip.hidden = true;
      document.body.appendChild(laneTip);
    }
    return laneTip;
  }
  function fmtDomain(v, isDate) {
    if (isDate) {
      // Days since epoch (R Date semantics).
      return new Date(Math.round(v) * 86400000).toISOString().slice(0, 10);
    }
    return String(Math.round(v * 10) / 10);
  }
  // ---------- hover card ----------
  // A bar, a box, a dot range or a dumbbell answers a hover with the chart's
  // card (chart/common.js tipHead / tipRow, the .dd-tt-* rules in chart.css): the
  // row as the headline, the column's arm and N under it, then one labelled
  // line per number. The numbers come from the payload the row was built
  // from, so the card can say what the lane only draws: every segment of a
  // split bar with its count and share, the total against the arm's N, and
  // a box's statistics by name.
  function ttNum(v) {
    if (typeof v !== "number" || !isFinite(v)) return v == null ? "" : String(v);
    if (Number.isInteger(v)) return v.toLocaleString();
    return Number(v.toPrecision(6)).toLocaleString(undefined, {
      maximumFractionDigits: 4
    });
  }
  function ttPct(v, den) {
    if (!den || typeof v !== "number" || !isFinite(v)) return "";
    // Whole percents, as the cells print them.
    return " (" + Math.round(v / den * 100) + "%)";
  }
  function ttSw(color) {
    return color ? '<span class="dd-tt-sw" style="background:' + esc(color) +
      '"></span>' : "";
  }
  function ttHead(text, color) {
    return '<div class="dd-tt-head">' + ttSw(color) + "<span>" + esc(text) +
      "</span></div>";
  }
  // `hit`, in a card listing every level of a colour split: true for the
  // level under the pointer, false for the others (chart/common.js tipRow).
  function ttRow(label, value, color, hit) {
    return '<div class="dd-tt-row' +
      (hit === true ? " is-hit" : hit === false ? " is-dim" : "") + '">' + ttSw(color) +
      '<span class="dd-tt-label">' + esc(label) + '</span>' +
      '<span class="dd-tt-value">' + esc(value) + "</span></div>";
  }
  function ttNote(text) {
    return '<div class="dd-tt-row dd-tt-label">' + esc(text) + "</div>";
  }
  var TT_SEP = '<div class="dd-tt-sep"></div>';
  function closestIn(t, root, selector) {
    var el = t.closest(selector);
    return el && root.contains(el) ? el : null;
  }
  function staticTipTarget(t, own) {
    if (!own.classList) return true;
    if (own.classList.contains("blockr-summarize-pacell")) {
      return !!closestIn(t, own, ".lane-link, .lane-from, .lane-to");
    }
    if (own.classList.contains("blockr-summarize-boxcell")) {
      return !!closestIn(t, own, ".lane-wh, .lane-cap, .lane-box, .lane-med");
    }
    if (own.classList.contains("blockr-summarize-prcell")) {
      return !!closestIn(t, own, ".lane-fence, .lane-rng, .lane-ctr");
    }
    return true;
  }

  // A distribution glyph's lines, the chart's boxplot tooltip words: n, the
  // centre, the body's range, the whiskers'.
  function ttDist(kind, g, i, words) {
    var r = g.r || {};
    var at = function (k) { return r[k] ? r[k][i] : null; };
    var span = function (lo, hi) {
      return (lo == null || hi == null) ? "undefined (n < 2)"
        : ttNum(lo) + " \u2013 " + ttNum(hi);
    };
    var h = "";
    if (g.nn && g.nn[i] != null) h += ttRow("n", ttNum(g.nn[i]));
    h += ttRow(words.center || "Center", ttNum(at("bc")));
    if (words.range) {
      h += ttRow(kind === "box" ? "Box (" + words.range + ")" : words.range,
                 span(at("bl"), at("bh")));
    }
    if (words.whisk) {
      h += ttRow(kind === "box" ? "Whiskers (" + words.whisk + ")" : words.whisk,
                 span(at("wl"), at("wh")));
    }
    return h;
  }

  /** The card for the cell under the pointer, or "" for none. */
  function cardHtml(td, t) {
    var root = td.closest(".blockr-summarize-container");
    // A card's glyph names its column; a table cell is placed by its index.
    var card = td.hasAttribute("data-k");
    var tr = card ? td.closest("[data-summarize-ord]") : td.parentNode;
    var p = root && root._summarizePayload;
    var i = tr ? Number(tr.getAttribute("data-summarize-ord")) : NaN;
    var ci = card ? Number(td.getAttribute("data-k")) : td.cellIndex - 1;
    var c = (p && p.kind === "flat" && p.cols) ? p.cols[ci] : null;
    if (!c || isNaN(i) || i >= p.n) {
      // No payload (a static page): the one-line text the cell carries.
      var own = closestIn(t, td, "[data-summarize-tip]");
      if (own && !staticTipTarget(t, own)) return "";
      var txt = own ? own.getAttribute("data-summarize-tip") : "";
      return txt ? ttNote(txt) : "";
    }
    var tt = c.tt || {};
    var ctx = [];
    [tt.head, tt.sub].forEach(function (x) {
      if (x && ctx.indexOf(x) < 0) ctx.push(x);
    });
    var h = ttHead(p.label[i]) + (ctx.length ? ttNote(ctx.join(" \u00b7 ")) : "");
    // A note naming the statistics, unless the column's own line already does.
    var statNote = function (text) {
      return text && ctx.indexOf(text) < 0 ? ttNote(text) : "";
    };
    var den = tt.den;
    var meas = tt.meas || "Value";
    if (c.kind === "bar") {
      if (!closestIn(t, td, ".blockr-summarize-fill")) return "";
      if (c.disp && c.disp[i] != null && c.disp[i] !== "") {
        return h + ttRow(meas, c.disp[i] + (c.pct && c.pct[i] ? " " + c.pct[i] : ""));
      }
      return "";
    }
    if (c.kind === "barsplit") {
      var fillEl = closestIn(t, td, ".blockr-summarize-fill");
      if (!fillEl) return "";
      var hitName = null;
      var fills = Array.prototype.slice.call(
        td.querySelectorAll(".blockr-summarize-fill"));
      var drawn = [];
      // The levels splitHtml() drew, in order: only those with a value
      // in this row, stacked or grouped alike.
      for (var j0 = 0; j0 < c.names.length; j0++) {
        if (c.segv[j0][i] > 0) drawn.push(j0);
      }
      var at = fills.indexOf(fillEl);
      if (at >= 0 && at < drawn.length) hitName = c.names[drawn[at]];
      var rows = "";
      var shown = 0;
      for (var j = 0; j < c.names.length; j++) {
        var v = c.segv[j][i];
        // A level with nothing drawn in this row has no line either.
        if (!(v > 0)) continue;
        shown++;
        rows += ttRow((tt.cvar ? tt.cvar + " " : "") + c.names[j],
                      ttNum(v) + ttPct(v, den), c.fills[j],
                      hitName == null ? null : c.names[j] === hitName);
      }
      if (!shown) return "";
      var foot = "";
      if (c.mode !== "grouped" && c.disp && c.disp[i]) {
        foot = TT_SEP + ttRow(meas, c.disp[i] +
          (c.pct && c.pct[i] ? " " + c.pct[i] : "") +
          (den ? " of " + ttNum(den) : ""));
      }
      return h + rows + foot;
    }
    if (c.kind === "box" || c.kind === "pointrange") {
      var distMark = closestIn(t, td,
        ".lane-wh, .lane-cap, .lane-box, .lane-med, .lane-fence, .lane-rng, .lane-ctr");
      if (!distMark) return "";
      var words = tt.words || {};
      if (!c.multi) {
        if (!c.r || !c.r.bc || c.r.bc[i] == null) return "";
        return h + ttDist(c.kind, c, i, words);
      }
      // A colour-split cell: every level, one line each, the one under the
      // pointer marked and its n and whiskers under the list.
      var lvEl = distMark.closest(".blockr-summarize-lv");
      var drawnLv = [];
      for (var k = 0; k < c.lv.length; k++) {
        if (lvDrawn(c, c.lv[k], i)) drawnLv.push(k);
      }
      if (!drawnLv.length) return "";
      var hitLv = null;
      if (lvEl) {
        var all = Array.prototype.slice.call(
          lvEl.parentNode.querySelectorAll(".blockr-summarize-lv"));
        var pos = all.indexOf(lvEl);
        if (pos >= 0 && pos < drawnLv.length) hitLv = drawnLv[pos];
      }
      var statAt = function (g, key) { return g.r && g.r[key] ? g.r[key][i] : null; };
      var spanOf = function (lo, hi) {
        return (lo == null || hi == null) ? "undefined (n < 2)"
          : ttNum(lo) + " \u2013 " + ttNum(hi);
      };
      var statHead = words.range
        ? (words.center || "Center") + " (" + words.range + ")" : (words.center || "");
      var body = statNote(statHead);
      drawnLv.forEach(function (k2) {
        var g = c.lv[k2];
        var lo = statAt(g, "bl"), hi = statAt(g, "bh");
        body += ttRow((tt.cvar ? tt.cvar + " " : "") + c.levels[k2],
                      ttNum(statAt(g, "bc")) +
                        (words.range && (lo != null || hi != null)
                          ? " (" + spanOf(lo, hi) + ")" : ""),
                      c.fills[k2], hitLv == null ? null : k2 === hitLv);
      });
      if (hitLv != null) {
        var gh = c.lv[hitLv];
        var footLv = [(tt.cvar ? tt.cvar + " " : "") + c.levels[hitLv]];
        if (gh.nn && gh.nn[i] != null) footLv.push("n " + ttNum(gh.nn[i]));
        if (words.whisk) {
          footLv.push((c.kind === "box" ? "Whiskers (" + words.whisk + ")" : words.whisk) +
                      " " + spanOf(statAt(gh, "wl"), statAt(gh, "wh")));
        }
        body += TT_SEP + ttNote(footLv.join(" \u00b7 "));
      }
      return h + body;
    }
    if (c.kind === "pair") {
      var pairMark = closestIn(t, td, ".lane-link, .lane-from, .lane-to");
      if (!pairMark) return "";
      var notes = function (tip, skip) {
        return String(tip).split(" \u00b7 ").slice(skip).map(function (x) {
          return ttNote(x);
        }).join("");
      };
      if (!c.multi) {
        var tip = c.tip && c.tip[i];
        return tip ? h + notes(tip, 0) : "";
      }
      // A colour-split cell, as for the glyph: every level on one line, the
      // ends' words once above them, the one under the pointer marked. Its
      // tip's remaining clauses (the range) go under the list.
      var lvP = pairMark.closest(".blockr-summarize-lv");
      var drawnP = [];
      for (var q = 0; q < c.lv.length; q++) {
        if (lvDrawn(c, c.lv[q], i)) drawnP.push(q);
      }
      if (!drawnP.length) return "";
      var hitP = null;
      if (lvP) {
        var allP = Array.prototype.slice.call(
          lvP.parentNode.querySelectorAll(".blockr-summarize-lv"));
        var posP = allP.indexOf(lvP);
        if (posP >= 0 && posP < drawnP.length) hitP = drawnP[posP];
      }
      var wordsP = tt.words || {};
      var bodyP = wordsP.from && wordsP.to
        ? statNote(wordsP.from + " \u2192 " + wordsP.to) : "";
      drawnP.forEach(function (k3) {
        bodyP += ttRow((tt.cvar ? tt.cvar + " " : "") + c.levels[k3],
                       c.lv[k3].ab ? c.lv[k3].ab[i] : "", c.fills[k3],
                       hitP == null ? null : k3 === hitP);
      });
      var restP = hitP == null ? "" : notes(c.lv[hitP].tip[i], 2);
      return h + bodyP + (restP ? TT_SEP + restP : "");
    }
    return "";
  }

  function placeTip(tip, e) {
    tip.hidden = false;
    tip.style.left = Math.min(e.clientX + 12,
      window.innerWidth - tip.offsetWidth - 8) + "px";
    var top = e.clientY + 16;
    if (top + tip.offsetHeight > window.innerHeight - 8) {
      top = Math.max(8, e.clientY - tip.offsetHeight - 12);
    }
    tip.style.top = top + "px";
  }

  document.addEventListener("mousemove", function (e) {
    var t = /** @type {Element} */ (e.target);
    if (!t || !t.closest) { return; }
    var lane = t.closest(".blockr-summarize-ivcell, .blockr-summarize-spcell");
    var tip = tipEl();
    if (!lane) {
      laneHighlight(null, null);
      var td = t.closest("td.blockr-summarize-bar-col, .tc-glyph");
      var html = td && t.closest(".blockr-summarize-container") ? cardHtml(td, t) : "";
      if (!html) {
        tip.hidden = true;
        return;
      }
      tip.className = "blockr-lane-tip is-card";
      tip.innerHTML = html;
      placeTip(tip, e);
      return;
    }
    var r = lane.getBoundingClientRect();
    if (!r.width) { tip.hidden = true; return; }
    var fx = Math.min(Math.max((e.clientX - r.left) / r.width, 0), 1);
    var txt = "";
    var swatch = null;
    if (lane.classList.contains("blockr-summarize-ivcell")) {
      var seg = t.closest(".lane-seg");
      // Same-event highlight: hovering a labelled segment dims every other
      // segment in the table and lifts the matches -- the table's answer
      // to cross-lane identity emphasis, keyed on data-l.
      laneHighlight(lane.closest(".blockr-summarize-container"),
                    seg ? seg.getAttribute("data-l") : null);
      if (seg) {
        txt = seg.getAttribute("data-tip") || "";
        swatch = seg.style.background || null;
      } else {
        var d0 = parseFloat(lane.getAttribute("data-d0"));
        var d1 = parseFloat(lane.getAttribute("data-d1"));
        if (isNaN(d0) || isNaN(d1)) { tip.hidden = true; return; }
        txt = "~" + fmtDomain(d0 + fx * (d1 - d0),
          lane.getAttribute("data-dd") === "1");
      }
    } else {
      var xs = (lane.getAttribute("data-xs") || "").split(",");
      var ys = (lane.getAttribute("data-ys") || "").split(",");
      if (!xs.length || xs[0] === "") { tip.hidden = true; return; }
      var i = Math.round(fx * (xs.length - 1));
      txt = xs[i] + " · " + ys[i];
    }
    if (!txt) { tip.hidden = true; return; }
    // The same card as the bars: the first clause headlines (with the
    // segment's colour), a "name: value" clause is a row, the rest notes.
    // getAttribute already decoded the payload text; the card escapes it.
    var parts = txt.split(" \u00b7 ");
    var html = ttHead(parts[0], swatch);
    parts.slice(1).forEach(function (pc) {
      var k = pc.indexOf(": ");
      html += k > 0 ? ttRow(pc.slice(0, k), pc.slice(k + 2)) : ttNote(pc);
    });
    tip.className = "blockr-lane-tip is-card";
    tip.innerHTML = html;
    placeTip(tip, e);
  });
  // Leaving the window (or scrolling the lane away) must not strand the tip
  // or the highlight.
  document.addEventListener("mouseout", function (e) {
    if (!e.relatedTarget) {
      if (laneTip) laneTip.hidden = true;
      laneHighlight(null, null);
    }
  });
  document.addEventListener("scroll", function () {
    if (laneTip) laneTip.hidden = true;
  }, true);

  // The container arrives with the block's UI, and again on every re-render
  // (a gear edit, new upstream data), so watch rather than bind once.
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", function () { scan(document); });
  } else {
    scan(document);
  }
  new MutationObserver(function (muts) {
    for (var i = 0; i < muts.length; i++) {
      var added = muts[i].addedNodes;
      for (var j = 0; j < added.length; j++) {
        var n = added[j];
        if (n.nodeType !== 1) continue;
        if (n.classList && n.classList.contains("blockr-summarize-container")) bind(n);
        else scan(n);
      }
    }
  }).observe(document.documentElement, { childList: true, subtree: true });
})();
