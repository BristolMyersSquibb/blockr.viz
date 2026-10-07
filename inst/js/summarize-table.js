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
      root.querySelectorAll("tbody tr.blockr-summarize-row")
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
        open[r.getAttribute("data-summarize-label")] = !r.classList.contains("collapsed");
      }
    });
    rows(root).forEach(function (r) {
      if (!r.classList.contains("is-child")) return;
      var vis = open[r.getAttribute("data-summarize-parent")] === true;
      if (vis) r.classList.remove("collapsed-hidden");
      else r.classList.add("collapsed-hidden");
    });
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
      if (matched[r.getAttribute("data-summarize-label")]) {
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
      var btn = e.target.closest(".blockr-indent-btn");
      if (!btn || !root.contains(btn)) return;
      e.stopPropagation();
      e.preventDefault();
      var row = btn.closest("tr.blockr-summarize-row");
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
    root.querySelectorAll("tr.summarize-flash").forEach(function (n) {
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
      return { cols: [path[0]], vals: [label] };
    }
    return {
      cols: [path[0], path[1]],
      vals: [tr.getAttribute("data-summarize-parent"), label]
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
      var tr = e.target.closest("tr.blockr-summarize-row.is-pick");
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

  /** Assemble the whole tbody from the cell model. */
  function assembleBody(p) {
    var out = [];
    for (var i = 0; i < p.n; i++) {
      var parent = !!(p.parent_row && p.parent_row[i]);
      var child = !!(p.level && p.level[i] > 0);
      var cls = "blockr-summarize-row" +
        (parent ? " is-parent blockr-indent-toggle collapsed" : "") +
        (child ? " is-child collapsed-hidden" : "") +
        (p.pick ? " is-pick" : "") +
        (p.on && p.on[i] ? " is-on" : "");
      var row = '<tr class="' + cls + '" data-summarize-label="' +
        esc(p.label[i]) + '"' +
        (child ? ' data-summarize-parent="' + esc(p.parent[i]) + '"' : "") +
        ' data-summarize-level="' + (child ? p.level[i] : 0) +
        '" data-summarize-ord="' + i + '">';
      row += '<td class="blockr-summarize-label-col blockr-stub' +
        (parent ? " blockr-has-toggle" : "") + '"' +
        (child ? ' style="padding-left:48px;"' : "") + ">" +
        (parent ? '<button class="blockr-indent-btn" type="button"' +
          ' tabindex="-1" aria-expanded="false">' + CHEV + "</button>" : "") +
        '<span class="blockr-summarize-label">' + esc(p.label[i]) + "</span></td>";
      for (var k = 0; k < p.cols.length; k++) {
        var c = p.cols[k];
        if (c.kind === "num" && c.text) {
          row += '<td class="blockr-summarize-txt"' + dataV(c.v[i]) + ">" +
            c.disp[i] + "</td>";
        } else if (c.kind === "num") {
          row += '<td class="blockr-summarize-num dt-col-num"' + dataV(c.v[i]) +
            ">" + c.disp[i] +
            (c.pct ? ' <span class="blockr-summarize-pct">' + c.pct[i] + "</span>" : "") +
            "</td>";
        } else if (c.kind === "barsplit") {
          row += '<td class="blockr-summarize-bar-col"' + dataV(c.v[i]) + ">" +
            barWrap(splitHtml(c, i), c, i) + "</td>";
        } else if (c.kind === "bardiv") {
          row += '<td class="blockr-summarize-bar-col"' + dataV(c.v[i]) + ">" +
            barWrap(dvHtml(c.w[i], c.pos[i]), c, i) + "</td>";
        } else if (c.kind === "box" || c.kind === "pointrange") {
          var glyph = c.multi
            ? multiHtml(c, i)
            : (c.kind === "box" ? boxHtml(c, i) : prHtml(c, i));
          row += '<td class="blockr-summarize-bar-col"' + dataV(c.v[i]) + ">" +
            barWrap(glyph, c, i) + "</td>";
        } else if (c.kind === "pair") {
          row += '<td class="blockr-summarize-bar-col"' + dataV(c.v[i]) + ">" +
            barWrap(c.multi ? multiHtml(c, i) : pairHtml(c, i), c, i) +
            "</td>";
        } else if (c.kind === "interval") {
          row += '<td class="blockr-summarize-bar-col' +
            (c.lg ? " blockr-summarize-wide" : "") + '"' + dataV(c.v[i]) + ">" +
            ivHtml(c, i) + "</td>";
        } else if (c.kind === "sparkline") {
          row += '<td class="blockr-summarize-bar-col"' + dataV(c.v[i]) + ">" +
            barWrap(spHtml(c, i), c, i) + "</td>";
        } else {
          row += '<td class="blockr-summarize-bar-col"' + dataV(c.v[i]) + ">" +
            barWrap(trackHtml(c.w[i], c.fill, c.sub && c.sub[i]), c, i) +
            "</td>";
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
      if (!ch.legend) {
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
    if (p.kind === "html") {
      wrap.innerHTML = p.html;
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
  var BAR_MODE_OPT = [{ value: "stacked", label: "Stacked" },
                      { value: "grouped", label: "Grouped" },
                      { value: "percent", label: "100%" }];
  // The chart's words, so the two trays read alike.
  var SORT_DIR_OPT = [{ value: "desc", label: "Descending" },
                      { value: "asc", label: "Ascending" }];
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
    // Mapping — the chart block's labels verbatim (Group / Color / Facet), so
    // the two gears read as the same system. `parent` is the ranked-bar surface's extra.
    group:  { label: "Group", kind: "column", colType: "cat" },
    // The summarize-table path's grouping vector (outer -> inner, at most
    // two): one row per key combo, the outer becomes the expandable parent.
    by:     { label: "Group by", kind: "columns", colType: "cat",
              placeholder: "add columns…" },
    parent: { label: "Nest under", kind: "column", colType: "cat" },
    color:  { label: "Color", kind: "column", colType: "cat" },
    facet:  { label: "Facet", kind: "column", colType: "cat" },
    // Extra row columns beside the bar — the chart's tooltip fields, shown
    // as real columns. Offered only for the as-is measure (summarizeSections):
    // any aggregation has no underlying row to read a column from.
    fields: { label: "More columns", kind: "columns", colType: "any",
              placeholder: "add columns…" },
    func:   { label: "Aggregate", kind: "select", options: FUNC_OPT,
              rerender: true },
    value:  { label: "Of column", kind: "column", colType: "num" },
    id_var: { label: "Count distinct", kind: "column", colType: "any" },
    // Facet column order in the summarize-table mode: each summary's level
    // copies adjacent (the comparison affordance), or level groups spanning
    // the summaries (the Table-1 reading).
    facet_layout: { label: "Facet layout", kind: "segmented", options: [
      { value: "by_summary", label: "By summary" },
      { value: "by_level", label: "By level" }
    ] },
    // Presentation.
    sort_by:  { label: "Sort", kind: "select", options: [] },
    bar_mode: { label: "Split layout", kind: "segmented", options: BAR_MODE_OPT },
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
    download: { label: "Download", kind: "segmented", options: DOWNLOAD_OPT },
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
    simple: { label: "simple", shows: ["bar", "number", "dot"] },
    dist:   { label: "dist", shows: ["pointrange", "box", "text"] },
    field:  { label: "field", shows: ["text"] },
    series: { label: "series", shows: ["sparkline"] },
    spans:  { label: "spans", shows: ["interval"] },
    pair:   { label: "pair", shows: ["dumbbell"] },
    expr:   { label: "expr", shows: ["text"] },
    // A function of one cell's rows. "auto" reads the mark off what the
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
      '<circle cx="12" cy="8" r="2.6" fill="currentColor"/></svg>'
  };
  SHOW_ICONS.auto = '<svg width="14" height="14" viewBox="0 0 16 16">' +
    '<text x="1" y="12" font-size="9" font-style="italic" fill="currentColor">fx</text></svg>';
  // Tile captions: friendlier than the raw enum where it helps.
  var SHOW_LABELS = { pointrange: "dot range", interval: "swimlane",
                      auto: "from result" };
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
  function distInner(s) { return s.inner || s.stat || "median_q1_q3"; }
  function distOuter(s) {
    if (s.outer) return s.outer;
    if (s.whiskers) return s.whiskers;
    return s.show === "pointrange" ? "none" : "tukey";
  }
  function rangeWord(v) { return v === "none" ? "" : v; }

  // Colour and facet are the summary's OWN optional mappings: a column
  // splits by one, repeats per level of the other, or carries neither.
  // `ok` is the JS half of R's lane_color_capable() / the field rule.
  var SUMMARY_MAPS = [
    { key: "color", label: "Color",
      hint: "Split this column by a column's levels",
      ok: function (s) {
        // A custom column's split is a column of what it returns.
        if (s.type === "custom") return false;
        if (s.type === "dist") return (s.show || "pointrange") !== "text";
        if (s.type === "simple") return (s.show || "bar") !== "number";
        return s.type === "spans" || s.type === "pair";
      } },
    { key: "facet",  label: "Facet",
      hint: "Repeat this column once per level of a column",
      ok: function (s) { return s.type !== "field"; } },
    // A custom count's population: distinct values of this column in the
    // cell's slice are its N, so it prints "54 (18%)".
    { key: "denom", label: "Percent of", want: "any",
      hint: "Show the count as a share of this column's distinct values",
      badge: function (v) { return "% of " + v; },
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
  SUMMARY_MAPS.forEach(function (m) {
    if (!m.badge) {
      m.badge = function (v) { return m.label.toLowerCase() + " " + v; };
    }
  });

  // The mapped dimensions, appended to any type's line so a collapsed row
  // still says what it is split by and what it repeats over.
  function mapWords(s) {
    var out = "";
    SUMMARY_MAPS.forEach(function (m) {
      if (s[m.key] && m.ok(s)) {
        out += " · " + m.label.toLowerCase() + " " + s[m.key];
      }
    });
    return out;
  }

  function summaryLine(s) {
    return summaryBody(s) + mapWords(s);
  }

  function summaryBody(s) {
    switch (s.type) {
      case "simple": return (s.func || "count") +
        (s.col ? "(" + s.col + ")" : "()") + " · " + (s.show || "bar");
      case "dist": {
        if (s.show === "text") {
          return (s.stat || "median_q1_q3") + "(" + (s.col || "?") + ") · text";
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
      case "pair": return (s.from_func && s.from_func !== "identity" ?
          s.from_func + "(" + (s.from || "?") + ")" : (s.from || "?")) +
        " → " + (s.to_func || "max") + "(" + (s.to || "?") + ")" +
        (s.lo != null && s.lo !== "" || s.hi != null && s.hi !== "" ?
          ", range " + (s.lo == null ? "" : s.lo) + "–" + (s.hi == null ? "" : s.hi) : "") +
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
        var a = s.from_func && s.from_func !== "identity"
          ? fw(s.from_func) + " " + (s.from || "?") : (s.from || "?");
        var b = (s.to_func || "max") === "identity" ? (s.to || "?")
          : fw(s.to_func || "max").toLowerCase() + " " + (s.to || "?");
        return a + " to " + b;
      }
      case "expr": return s.expr || "";
      case "custom": return summaryBody(s).replace(/ · [^·]*$/, "");
      default: return "";
    }
  }

  /** The badges after the description: what splits or repeats the column. */
  function summaryBadges(s) {
    var out = "";
    SUMMARY_MAPS.forEach(function (m) {
      if (s[m.key] && m.ok(s)) {
        out += '<span class="lane-sum-badge">' + esc(m.badge(s[m.key])) +
          "</span>";
      }
    });
    return out;
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
          agg(s.from_func || "identity", s.from || "") + ",\n    to = " +
          agg(s.to_func || "max", s.to || "") + by + "\n  )";
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



  /** The editor body. ctx: { cfg(), cols(), send(list), rerender(), open:Set } */
  function renderSummariesEditor(sec, ctx) {
    var cfg = ctx.cfg();
    var cols = ctx.cols();
    var list = Array.isArray(cfg.summaries) ? cfg.summaries : (cfg.summaries = []);
    var S = (typeof Blockr !== "undefined" && Blockr.Select) || null;
    var commit = function () { ctx.send(list.slice()); };

    function colOpts(want) {
      return cols.filter(function (c) {
        return want === "any" ||
          (want === "num" ? c.type === "numeric" : c.type !== "numeric");
      }).map(function (c) { return { value: c.name, label: c.label || c.name }; });
    }
    // `onRemove` makes the control an OPTIONAL mapping: the label grows the
    // same ✕ the grouping roles carry, and clicking it drops the mapping
    // rather than setting it to some empty value.
    function selectCtl(parent, label, want, selected, onChange, onRemove) {
      var ctl = document.createElement("div");
      ctl.className = "lane-sum-ctl";
      var l = document.createElement("span");
      l.className = "blockr-label";
      l.textContent = label;
      if (onRemove) {
        var x = document.createElement("button");
        x.type = "button";
        x.className = "dd-role-remove lane-sum-map-rm";
        x.setAttribute("data-blockr-tooltip", "Remove " + label.toLowerCase());
        x.setAttribute("aria-label", "Remove " + label.toLowerCase());
        x.innerHTML = "✕";
        x.addEventListener("click", onRemove);
        l.appendChild(x);
      }
      ctl.appendChild(l);
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
                         onChange: pick });
      }
      ctl.appendChild(wrap);
      parent.appendChild(ctl);
    }
    function multiCtl(parent, label, selected, onChange) {
      var ctl = document.createElement("div");
      ctl.className = "lane-sum-ctl";
      var l = document.createElement("span");
      l.className = "blockr-label";
      l.textContent = label;
      ctl.appendChild(l);
      var wrap = document.createElement("div");
      wrap.className = "dd-picker-wrap";
      if (S && S.multi) {
        S.multi(wrap, {
          bordered: true,
          options: colOpts("any"),
          selected: (selected || []).slice(),
          placeholder: "add columns…",
          onChange: onChange
        });
      }
      ctl.appendChild(wrap);
      parent.appendChild(ctl);
    }
    function segCtl(parent, label, options, selected, onChange) {
      var ctl = document.createElement("div");
      ctl.className = "lane-sum-ctl";
      var l = document.createElement("span");
      l.className = "blockr-label";
      l.textContent = label;
      ctl.appendChild(l);
      var seg = document.createElement("div");
      seg.className = "lane-sum-seg";
      options.forEach(function (o) {
        var b = document.createElement("button");
        b.type = "button";
        b.className = "lane-sum-seg-btn" + (o.value === selected ? " is-on" : "");
        b.textContent = o.label;
        b.addEventListener("click", function () {
          onChange(o.value);
        });
        seg.appendChild(b);
      });
      ctl.appendChild(seg);
      parent.appendChild(ctl);
    }

    // dd-summaries: the engine's full-width escape from the section's
    // narrow grid cells (the metrics editor's own trick).
    var wrap = document.createElement("div");
    wrap.className = "dd-summaries lane-summaries";

    list.forEach(function (s, i) {
      var row = document.createElement("div");
      row.className = "lane-sum-row" + (ctx.open.has(i) ? " is-open" : "");

      var head = document.createElement("div");
      head.className = "lane-sum-head";
      var isOpen = ctx.open.has(i);
      // The row reads as a sentence: the mark it draws, its name, what it
      // computes, what splits or repeats it.
      head.innerHTML =
        '<span class="lane-sum-grip" aria-hidden="true">' + GRIP_SVG + "</span>" +
        '<span class="lane-sum-glyph">' + (SHOW_ICONS[markOf(s)] || "") + "</span>" +
        '<span class="lane-sum-name">' + esc(s.name || "Untitled") + "</span>" +
        '<span class="lane-sum-line">' + esc(summaryDesc(s)) + "</span>" +
        summaryBadges(s);
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
        code.setAttribute("data-blockr-tooltip", asCode
          ? (canGo ? "Back to the preset" : "Started as code")
          : (canGo ? "Edit as code" : "No code form for this mark"));
        code.setAttribute("aria-label", code.getAttribute("data-blockr-tooltip"));
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
          commit();
          ctx.rerender();
        });
        head.appendChild(code);
      }
      var rm = document.createElement("button");
      rm.type = "button";
      rm.className = "lane-sum-rm";
      rm.setAttribute("data-blockr-tooltip", "Remove column");
      rm.setAttribute("aria-label", "Remove column");
      rm.innerHTML = '<svg width="12" height="12" viewBox="0 0 12 12"><path d="M3 3l6 6M9 3l-6 6" ' +
        'fill="none" stroke="currentColor" stroke-width="1.3" stroke-linecap="round"/></svg>';
      rm.addEventListener("click", function (e) {
        e.stopPropagation();
        list.splice(i, 1);
        ctx.open.clear();
        ctx.stash = {};
        commit();
        ctx.rerender();
      });
      head.appendChild(rm);
      var chev = document.createElement("span");
      chev.className = "lane-sum-chev";
      chev.innerHTML = CHEV_SVG;
      head.appendChild(chev);
      // One column open at a time.
      head.addEventListener("click", function () {
        var was = ctx.open.has(i);
        ctx.open.clear();
        if (!was) ctx.open.add(i);
        ctx.rerender();
      });
      // Reorder by dragging the row (design system: rows drag, no arrows).
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
        if (isNaN(from) || from === i) return;
        list.splice(i, 0, list.splice(from, 1)[0]);
        ctx.open.clear();
        ctx.stash = {};
        commit();
        ctx.rerender();
      });
      row.appendChild(head);

      if (ctx.open.has(i)) {
        var body = document.createElement("div");
        body.className = "lane-sum-body";
        // Name.
        var nameCtl = document.createElement("div");
        nameCtl.className = "lane-sum-ctl";
        nameCtl.innerHTML = '<span class="blockr-label">Name</span>';
        var nameIn = document.createElement("input");
        nameIn.type = "text";
        nameIn.className = "lane-sum-name-input";
        nameIn.value = s.name || "";
        nameIn.addEventListener("change", function () {
          s.name = nameIn.value;
          commit();
          ctx.rerender();
        });
        nameCtl.appendChild(nameIn);
        body.appendChild(nameCtl);

        var t = s.type || "simple";
        if (t === "simple") {
          // The full aggregate set INCLUDING identity ("None (as is)"): a
          // value already reduced upstream — one row per group — drawn
          // as-is, as a bar or a number. The chart block's duality.
          selectCtl(body, "Function", FUNC_OPT, s.func || "count",
            function (v) {
              s.func = v;
              commit();
              ctx.rerender();
            });
          if ((s.func || "count") !== "count") {
            selectCtl(body,
              s.func === "count_distinct" ? "Distinct of" : "Of column",
              s.func === "count_distinct" ? "any" : "num",
              s.col, function (v) { s.col = v; commit(); ctx.rerender(); });
          }
        } else if (t === "dist") {
          selectCtl(body, "Value", "num", s.col, function (v) {
            s.col = v;
            commit();
            ctx.rerender();
          });
          // The glyph is a centre plus two optional ranges; the Display
          // tiles pick how they are drawn. Both ranges offer "(none)",
          // which is where the IQR bar, the plain dot range and the bare
          // dot live -- so the list never needs a mark per shape.
          if (s.show !== "text") {
            selectCtl(body, "Inner range", RANGE_OPT, distInner(s),
              function (v) { s.inner = v; commit(); ctx.rerender(); });
            selectCtl(body, "Outer range", OUTER_OPT, distOuter(s),
              function (v) { s.outer = v; commit(); ctx.rerender(); });
          } else {
            selectCtl(body, "Statistic", LANE_STATS, s.stat || "median_q1_q3",
              function (v) { s.stat = v; commit(); ctx.rerender(); });
          }
        } else if (t === "field") {
          selectCtl(body, "Column", "any", s.col, function (v) {
            s.col = v;
            commit();
            ctx.rerender();
          });
        } else if (t === "series") {
          selectCtl(body, "Order (x)", "num", s.x, function (v) {
            s.x = v;
            commit();
            ctx.rerender();
          });
          selectCtl(body, "Value (y)", "num", s.col, function (v) {
            s.col = v;
            commit();
            ctx.rerender();
          });
          var band = (s.band && s.band.length === 2) ? s.band : ["", ""];
          selectCtl(body, "Band low", "num", band[0], function (v) {
            s.band = [v, band[1]];
            commit();
            ctx.rerender();
          });
          selectCtl(body, "Band high", "num", band[1], function (v) {
            s.band = [band[0], v];
            commit();
            ctx.rerender();
          });
          // The computed reference: pooled orientation line/band, computed
          // in R over the column's values (per facet level under facet).
          selectCtl(body, "Reference", [
            { value: "none", label: "None" },
            { value: "mean", label: "Mean" },
            { value: "mean_sd", label: "Mean ± SD" },
            { value: "median_iqr", label: "Median · IQR" }
          ], s.ref || "none", function (v) {
            s.ref = v;
            commit();
            ctx.rerender();
          });
        } else if (t === "spans") {
          selectCtl(body, "Start", "num", s.x, function (v) {
            s.x = v;
            commit();
            ctx.rerender();
          });
          selectCtl(body, "End", "num", s.xend, function (v) {
            s.xend = v;
            commit();
            ctx.rerender();
          });
          // Event identity on hover: label headlines the segment tooltip
          // and keys the same-event highlight; fields append extras.
          selectCtl(body, "Label (hover)", "any", s.label, function (v) {
            s.label = v;
            commit();
            ctx.rerender();
          });
          multiCtl(body, "Tooltip fields", s.fields, function (vals) {
            s.fields = vals;
            commit();
            ctx.rerender();
          });
          segCtl(body, "Width", [
            { value: "md", label: "Regular" },
            { value: "lg", label: "Wide" }
          ], s.size || "md", function (v) {
            s.size = v;
            commit();
            ctx.rerender();
          });
        } else if (t === "pair") {
          // Two summaries of the group on one segment. A band bound or the
          // reference takes a number; a band bound may also name a column
          // (a per-subject reference range such as ANRLO / ANRHI).
          var PAIR_FUNCS = [
            { value: "identity", label: "None (as is)" },
            { value: "mean", label: "Mean" },
            { value: "median", label: "Median" },
            { value: "min", label: "Min" },
            { value: "max", label: "Max" }
          ];
          selectCtl(body, "From ◇", "num", s.from, function (v) {
            s.from = v; commit(); ctx.rerender();
          });
          selectCtl(body, "From function", PAIR_FUNCS,
            s.from_func || "identity",
            function (v) { s.from_func = v; commit(); ctx.rerender(); });
          selectCtl(body, "To ●", "num", s.to, function (v) {
            s.to = v; commit(); ctx.rerender();
          });
          selectCtl(body, "To function", PAIR_FUNCS, s.to_func || "max",
            function (v) { s.to_func = v; commit(); ctx.rerender(); });
          [["lo", "Range low", "number or column"],
           ["hi", "Range high", "number or column"],
           ["ref", "Reference line", "e.g. 20"]].forEach(function (d) {
            var tc = document.createElement("div");
            tc.className = "lane-sum-ctl";
            tc.innerHTML = '<span class="blockr-label">' + d[1] +
              "</span>";
            var ti = document.createElement("input");
            ti.type = "text";
            ti.className = "lane-sum-name-input";
            ti.value = s[d[0]] == null ? "" : String(s[d[0]]);
            ti.placeholder = d[2];
            ti.addEventListener("change", function () {
              var v = ti.value.trim();
              if (v === "") delete s[d[0]];
              else s[d[0]] = isFinite(Number(v)) ? Number(v) : v;
              commit();
              ctx.rerender();
            });
            tc.appendChild(ti);
            body.appendChild(tc);
          });
          if (s.dash) {
            selectCtl(body, "Dashed by", "cat", s.dash, function (v) {
              s.dash = v; commit(); ctx.rerender();
            }, function () { delete s.dash; commit(); ctx.rerender(); });
          } else {
            var dm = document.createElement("div");
            dm.className = "lane-sum-ctl lane-sum-addmaps";
            var db = document.createElement("button");
            db.type = "button";
            db.className = "lane-sum-add";
            db.textContent = "+ dashed by";
            db.disabled = !firstMapCol(cols);
            db.addEventListener("click", function () {
              s.dash = firstMapCol(cols); commit(); ctx.rerender();
            });
            dm.appendChild(db);
            body.appendChild(dm);
          }
        } else if (t === "expr") {
          var exCtl = document.createElement("div");
          exCtl.className = "lane-sum-ctl lane-sum-ctl-wide";
          exCtl.innerHTML =
            '<span class="blockr-label">Expression</span>';
          var exIn = document.createElement("input");
          exIn.type = "text";
          exIn.className = "lane-sum-name-input";
          exIn.value = s.expr || "";
          exIn.placeholder = "e.g. sd(DUR)/mean(DUR) * 100";
          exIn.addEventListener("change", function () {
            s.expr = exIn.value;
            commit();
            ctx.rerender();
          });
          exCtl.appendChild(exIn);
          body.appendChild(exCtl);
        } else if (t === "custom") {
          // Code commits on its own button (or Mod+Enter), never per
          // keystroke: every commit reruns the function for every cell.
          var fnCtl = document.createElement("div");
          fnCtl.className = "lane-sum-ctl lane-sum-ctl-wide";
          fnCtl.innerHTML = '<span class="blockr-label">Summary of one ' +
            "cell's rows, <code>d</code></span>";
          var fnIn = document.createElement("textarea");
          fnIn.className = "blockr-text-input dd-script-editor lane-sum-fn";
          fnIn.rows = Math.max(3, String(s.fn || "").split("\n").length + 1);
          fnIn.spellcheck = false;
          fnIn.value = s.fn || "";
          fnIn.placeholder = "\\(d) d |> dplyr::count(AESEV, name = \"value\")";
          var fnFoot = document.createElement("div");
          fnFoot.className = "lane-sum-fn-foot";
          var fnHint = document.createElement("span");
          fnHint.className = "lane-sum-hint";
          fnHint.textContent = "Return value; from, to; lo, q1, mid, q3, hi; " +
            "or text. One more column splits the cell.";
          var fnApply = document.createElement("button");
          fnApply.type = "button";
          fnApply.className = "lane-sum-apply";
          fnApply.textContent = "Apply";
          var applyFn = function () {
            if (fnIn.value === (s.fn || "")) return;
            s.fn = fnIn.value;
            commit();
            ctx.rerender();
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
          body.appendChild(fnCtl);
        }

        // Display tiles (only where the type offers a choice).
        var shows = (SUMMARY_TYPES[t] || {}).shows || [];
        if (shows.length > 1) {
          var dCtl = document.createElement("div");
          dCtl.className = "lane-sum-ctl";
          dCtl.innerHTML = '<span class="blockr-label">Mark</span>';
          var tiles = document.createElement("div");
          tiles.className = "dd-type-grid lane-sum-tiles";
          shows.forEach(function (sh) {
            var b = document.createElement("button");
            b.type = "button";
            b.className = "dd-type-tile" +
              ((s.show || shows[0]) === sh ? " dd-type-active" : "");
            if (t === "custom" && sh === "auto" && !s.show) {
              b.className += " dd-type-active";
            }
            b.innerHTML = '<span class="dd-type-tile-icon">' +
              (SHOW_ICONS[sh] || "") + '</span>' +
              '<span class="dd-type-tile-label">' +
              (SHOW_LABELS[sh] || sh) + "</span>";
            b.addEventListener("click", function () {
              if (t === "custom" && sh === "auto") {
                delete s.show;
                commit();
                ctx.rerender();
                return;
              }
              s.show = sh;
              // For a distribution the tiles are the STYLE: switching does
              // not change which numbers the column holds, so the ranges
              // are pinned to what they already were rather than left to
              // the legacy default the new `show` would imply.
              if (t === "dist" && sh !== "text") {
                s.style = sh === "box" ? "box" : "dot";
                s.inner = distInner(s);
                s.outer = distOuter(s);
              }
              commit();
              ctx.rerender();
            });
            tiles.appendChild(b);
          });
          dCtl.appendChild(tiles);
          body.appendChild(dCtl);
        }

        // The column's own optional mappings. Nothing shows until one is
        // added, so a plain column stays plain, and each is dropped from
        // its own ✕ rather than through an inert "none" state.
        var addable = [];
        SUMMARY_MAPS.forEach(function (m) {
          if (!m.ok(s)) return;
          if (!s[m.key]) { addable.push(m); return; }
          selectCtl(body, m.label, m.want || "cat", s[m.key], function (v) {
            s[m.key] = v;
            commit();
            ctx.rerender();
          }, function () {
            delete s[m.key];
            commit();
            ctx.rerender();
          });
        });
        if (addable.length) {
          var maps = document.createElement("div");
          maps.className = "lane-sum-ctl lane-sum-addmaps";
          addable.forEach(function (m) {
            var b = document.createElement("button");
            b.type = "button";
            b.className = "lane-sum-add";
            b.textContent = "+ " + m.label.toLowerCase();
            // Nothing decodable to map: the button would only produce an
            // error message in the table.
            var seedOf = function () {
              return m.seed ? m.seed(cols) : firstMapCol(cols);
            };
            b.disabled = !seedOf();
            b.addEventListener("click", function () {
              s[m.key] = seedOf();
              commit();
              ctx.rerender();
            });
            maps.appendChild(b);
          });
          body.appendChild(maps);
        }
        row.appendChild(body);
      }
      wrap.appendChild(row);
    });

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
        ns.from = firstCol(cols, "num");
        ns.to = firstCol(cols, "num", [ns.from]) || ns.from;
        ns.from_func = "identity";
        ns.to_func = "max";
      }
      return ns;
    };
    var addOne = function (t, f) {
      list.push(seedFor(t, f));
      ctx.open.clear();
      ctx.open.add(list.length - 1);
      commit();
      ctx.rerender();
    };
    var ADD_ITEMS = [
      { title: "Counts" },
      { label: "Count", meta: "rows", icon: SHOW_ICONS.bar, t: "simple", f: "count" },
      { label: "Count distinct", meta: "patients", icon: SHOW_ICONS.bar, t: "simple", f: "count_distinct" },
      { title: "Values" },
      { label: "One value", meta: "mean, sum, max", icon: SHOW_ICONS.number, t: "simple", f: "mean" },
      { label: "Distribution", meta: "box or dot range", icon: SHOW_ICONS.box, t: "dist" },
      { label: "Two values, from \u2192 to", meta: "onset to end", icon: SHOW_ICONS.dumbbell, t: "pair" },
      { title: "Rows of the cell" },
      { label: "Events", meta: "a swimlane", icon: SHOW_ICONS.interval, t: "spans" },
      { label: "Series", meta: "a sparkline", icon: SHOW_ICONS.sparkline, t: "series" },
      { label: "Text", meta: "the distinct values", icon: SHOW_ICONS.text, t: "field" },
      { label: "Expression", meta: "one R expression", icon: SHOW_ICONS.number, t: "expr" },
      { divider: true },
      { label: "Custom summary", meta: "a function of the cell's rows", icon: CODE_SVG, t: "custom" }
    ];
    var addRow = document.createElement("div");
    addRow.className = "lane-sum-addrow";
    var addBtn = document.createElement("button");
    addBtn.type = "button";
    addBtn.className = "lane-sum-addcol";
    addBtn.innerHTML = '<svg width="12" height="12" viewBox="0 0 12 12"><path d="M6 2v8M2 6h8" ' +
      'fill="none" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"/></svg>Add column';
    var B = (typeof Blockr !== "undefined") ? Blockr : null;
    if (B && B.menu) {
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
      // Without blockr.ui's menu: the same entries as plain buttons.
      var addWrap = document.createElement("div");
      addWrap.className = "lane-sum-add-types";
      ADD_ITEMS.forEach(function (it) {
        if (it.title || it.divider) return;
        var b = document.createElement("button");
        b.type = "button";
        b.className = "lane-sum-add";
        b.textContent = "+ " + it.label;
        b.addEventListener("click", function () { addOne(it.t, it.f); });
        addWrap.appendChild(b);
      });
      addRow.appendChild(addWrap);
    }
    wrap.appendChild(addRow);

    if (!list.length) {
      var hint = document.createElement("div");
      hint.className = "lane-sum-hint";
      hint.textContent = "Adding columns switches the block to the " +
        "multi-column table and replaces the single mark above.";
      wrap.appendChild(hint);
    }
    sec.appendChild(wrap);
  }

  // Which controls apply depends on the picks: `Of column` only for the
  // aggregations that reduce a column, `Count distinct` only for
  // count_distinct, the split layout only with a colour split, Compare only
  // with a facet. Conditional rows beat a wall of inert ones.
  function summarizeSections(cfg, ctx) {
    var mapping = [];
    var optional = [];
    var pres = ["sort_by", "sort_dir"];
    var aggTitle = null;

    // The summarize-table mode: the column list IS the config. Grouping
    // (`by`) and facet below it; the flat mark mappings never render.
    if (Array.isArray(cfg.summaries) && cfg.summaries.length) {
      // Which facet columns the table uses: the by-level reading spans ONE
      // column's groups across the summaries, so it is offered only when
      // every faceted column agrees on it.
      var fcols = [];
      cfg.summaries.forEach(function (s) {
        if (s && s.facet && fcols.indexOf(s.facet) === -1) fcols.push(s.facet);
      });
      return {
        requiredMap: ["by"],
        // Colour and facet are the COLUMN's mappings, added per summary in
        // the columns editor below: one column can split by severity while
        // the next repeats per sex, and a column that maps neither is plain.
        // Only the grouping is the whole table's.
        optionalMap: [],
        mapping: [],
        aggTitle: null,
        customSections: ctx ? [{
          title: "Columns",
          render: function (sec) { renderSummariesEditor(sec, ctx); }
        }] : [],
        presentation: fcols.length === 1
          ? ["sort_by", "sort_dir", "facet_layout", "value_labels", "search",
             "sortable", "axis", "download", "bar_width"]
          : ["sort_by", "sort_dir", "value_labels", "search", "sortable",
             "axis", "download", "bar_width"],
        drillToggle: "drill",
        drillDefault: (cfg.by && cfg.by.length)
          ? cfg.by[cfg.by.length - 1] : (cfg.group || ""),
        // A click claims the row's grouping path, so there is no column to
        // pick: `drill` is the on switch.
        drillPicker: false,
        ctrlSection: true,
        drillHint: cfg.drill
          ? "Clicking a row filters downstream on " + cfg.drill + "."
          : "Clicking a row filters downstream blocks to that row.",
        titles: ["title", "subtitle", "caption"]
      };
    }

    // The ranked-bar surface (the block's original form, kept for boards
    // that never opt into the column list): func / value / id_var, colour
    // split, facet, comparison. Every other glyph lives in the columns
    // editor above -- the type belongs to the COLUMN.
    var needsValue = ["identity", "sum", "mean", "median", "min", "max"]
      .indexOf(cfg.func) > -1;
    mapping = ["func"];
    if (needsValue) mapping.push("value");
    if (cfg.func === "count_distinct") mapping.push("id_var");
    optional = ["parent", "color", "facet"];
    if (cfg.func === "identity") optional.push("fields");
    // color + facet compose (split bars inside each facet column), so the
    // split layout applies whenever a colour split exists.
    if (cfg.color) pres.push("bar_mode");
    // The measure sits in Mapping, as the chart's does: no Aggregation
    // section of its own.
    aggTitle = null;
    pres.push("value_labels");
    pres.push("search");
    pres.push("sortable");
    pres.push("axis");
    pres.push("download");
    // Last: four segments overflow a grid cell, and the end of the row is
    // the one place with nothing to their right.
    pres.push("bar_width");

    return {
      requiredMap: ["group"],
      optionalMap: optional,
      mapping: mapping,
      aggTitle: aggTitle,
      // The entry point into the summarize-table mode: an empty column
      // list with the add/preset row. Adding the first column commits a
      // non-empty `summaries` and this spec switches to the branch above.
      customSections: ctx ? [{
        title: "Columns",
        render: function (sec) { renderSummariesEditor(sec, ctx); }
      }] : [],
      presentation: pres,
      drillToggle: "drill",
      drillDefault: cfg.group || "",
      drillPicker: false,
      // "Send to filter (beta)" inside the open Drill-down section, chart /
      // table parity: the engine reads cfg.ctrl_target / cfg.ctrl_choices.
      ctrlSection: true,
      // Spec-level hint (the table block's shape). NOT a host-level drillHint:
      // that one triggers the chart/tile drill section too and rendered a
      // second, empty "Drill-down" heading.
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
    // A `null` column pick reads as "" for the pickers (the engine's no-value).
    ["group", "parent", "color", "facet", "value", "id_var", "drill"]
      .forEach(function (k) { if (cfg[k] == null) cfg[k] = ""; });
    // The summarize-table config: the column list and the grouping vector.
    if (!Array.isArray(cfg.summaries)) {
      cfg.summaries = cfg.summaries ? [cfg.summaries] : [];
    }
    if (!Array.isArray(cfg.by)) cfg.by = cfg.by ? [String(cfg.by)] : [];
    if (!cfg.facet_layout) cfg.facet_layout = "by_summary";
    // The multi-column picker wants an array.
    if (!Array.isArray(cfg.fields)) {
      cfg.fields = cfg.fields ? [String(cfg.fields)] : [];
    }
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
      sections: function () { return summarizeSections(cfg, ctx); },
      sectionsForFamily: function () { return summarizeSections(cfg, ctx); },
      secondary: new Set(),
      // "Mapping" in both modes, as on the chart: the trays share their
      // section names.
      mappingTitle: function () { return "Mapping"; },
      // No block-level type picker: the type belongs to the COLUMN (the
      // columns editor's row types + display tiles).
      typeKey: null,
      typeGroups: null,
      familyFor: null,
      entryRequired: function (role) {
        return role === "by" || role === "group";
      },
      drillAutoLabel: null,
      title: "Summarize table settings",
      onChange: function (key) {
        sendConfig(elemId, key, cfg[key]);
        // The ranked default is descending; the data's own order read
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
        _role: function (k) { return engine._role(k); },
        _slotFlag: function (k) { return engine._slotFlag(k); },
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
  function ttRow(label, value, color, hit) {
    return '<div class="dd-tt-row"' +
      (hit ? ' style="font-weight:600"' : "") + ">" + ttSw(color) +
      '<span class="dd-tt-label">' + esc(label) + '</span>' +
      '<span class="dd-tt-value">' + esc(value) + "</span></div>";
  }
  function ttNote(text) {
    return '<div class="dd-tt-row dd-tt-label">' + esc(text) + "</div>";
  }
  var TT_SEP = '<div class="dd-tt-sep"></div>';

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
    var tr = td.parentNode;
    var p = root && root._summarizePayload;
    var i = tr ? Number(tr.getAttribute("data-summarize-ord")) : NaN;
    var c = (p && p.kind === "flat" && p.cols) ? p.cols[td.cellIndex - 1] : null;
    if (!c || isNaN(i) || i >= p.n) {
      // No payload (a static page): the one-line text the cell carries.
      var own = t.closest("[data-summarize-tip]");
      var txt = own ? own.getAttribute("data-summarize-tip") : "";
      return txt ? ttNote(txt) : "";
    }
    var tt = c.tt || {};
    var ctx = [];
    [tt.head, tt.sub].forEach(function (x) {
      if (x && ctx.indexOf(x) < 0) ctx.push(x);
    });
    var h = ttHead(p.label[i]) + (ctx.length ? ttNote(ctx.join(" \u00b7 ")) : "");
    var den = tt.den;
    var meas = tt.meas || "Value";
    if (c.kind === "bar") {
      if (c.disp && c.disp[i] != null && c.disp[i] !== "") {
        return h + ttRow(meas, c.disp[i] + (c.pct && c.pct[i] ? " " + c.pct[i] : ""));
      }
      return "";
    }
    if (c.kind === "barsplit") {
      var fillEl = t.closest(".blockr-summarize-fill");
      var hitName = null;
      if (fillEl) {
        var fills = Array.prototype.slice.call(
          fillEl.parentNode.parentNode.querySelectorAll(".blockr-summarize-fill"));
        var drawn = [];
        for (var j0 = 0; j0 < c.names.length; j0++) {
          if (c.mode === "grouped" || c.segv[j0][i] > 0) drawn.push(j0);
        }
        var at = fills.indexOf(fillEl);
        if (at >= 0 && at < drawn.length) hitName = c.names[drawn[at]];
      }
      var rows = "";
      var shown = 0;
      for (var j = 0; j < c.names.length; j++) {
        var v = c.segv[j][i];
        if (!(v > 0) && c.mode !== "grouped") continue;
        shown++;
        rows += ttRow((tt.cvar ? tt.cvar + " " : "") + c.names[j],
                      ttNum(v) + ttPct(v, den), c.fills[j],
                      c.names[j] === hitName);
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
      var words = tt.words || {};
      if (!c.multi) {
        if (!c.r || !c.r.bc || c.r.bc[i] == null) return "";
        return h + ttDist(c.kind, c, i, words);
      }
      // A colour-split cell: the level under the pointer, else every level.
      var lvEl = t.closest(".blockr-summarize-lv");
      var drawnLv = [];
      for (var k = 0; k < c.lv.length; k++) {
        if (lvDrawn(c, c.lv[k], i)) drawnLv.push(k);
      }
      var pick = drawnLv;
      if (lvEl) {
        var all = Array.prototype.slice.call(
          lvEl.parentNode.querySelectorAll(".blockr-summarize-lv"));
        var pos = all.indexOf(lvEl);
        if (pos >= 0 && pos < drawnLv.length) pick = [drawnLv[pos]];
      }
      if (!pick.length) return "";
      var body = "";
      pick.forEach(function (k2, n2) {
        if (n2) body += TT_SEP;
        // The level's own line, unless the column header already names it
        // (colour and facet on the same column: one glyph per arm).
        if (c.levels[k2] !== tt.head || pick.length > 1) {
          body += ttRow((tt.cvar ? tt.cvar + " " : "") + c.levels[k2], "",
                        c.fills[k2]);
        }
        body += ttDist(c.kind, c.lv[k2], i, words);
      });
      return h + body;
    }
    if (c.kind === "pair") {
      var notes = function (tip, skip) {
        return String(tip).split(" \u00b7 ").slice(skip).map(function (x) {
          return ttNote(x);
        }).join("");
      };
      if (!c.multi) {
        var tip = c.tip && c.tip[i];
        return tip ? h + notes(tip, 0) : "";
      }
      // A colour-split cell, as for the glyph: the level under the pointer,
      // else every level. Each level's tip starts with its level name,
      // which the coloured row below already says.
      var lvP = t.closest(".blockr-summarize-lv");
      var drawnP = [];
      for (var q = 0; q < c.lv.length; q++) {
        if (lvDrawn(c, c.lv[q], i)) drawnP.push(q);
      }
      var pickP = drawnP;
      if (lvP) {
        var allP = Array.prototype.slice.call(
          lvP.parentNode.querySelectorAll(".blockr-summarize-lv"));
        var posP = allP.indexOf(lvP);
        if (posP >= 0 && posP < drawnP.length) pickP = [drawnP[posP]];
      }
      if (!pickP.length) return "";
      var bodyP = "";
      pickP.forEach(function (k3, n3) {
        if (n3) bodyP += TT_SEP;
        bodyP += ttRow((tt.cvar ? tt.cvar + " " : "") + c.levels[k3], "",
                       c.fills[k3]) + notes(c.lv[k3].tip[i], 1);
      });
      return h + bodyP;
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
      var td = t.closest("td.blockr-summarize-bar-col");
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
    tip.className = "blockr-lane-tip";
    var r = lane.getBoundingClientRect();
    if (!r.width) { tip.hidden = true; return; }
    var fx = Math.min(Math.max((e.clientX - r.left) / r.width, 0), 1);
    var txt = "";
    if (lane.classList.contains("blockr-summarize-ivcell")) {
      var seg = t.closest(".lane-seg");
      // Same-event highlight: hovering a labelled segment dims every other
      // segment in the table and lifts the matches -- the table's answer
      // to cross-lane identity emphasis, keyed on data-l.
      laneHighlight(lane.closest(".blockr-summarize-container"),
                    seg ? seg.getAttribute("data-l") : null);
      if (seg) {
        txt = seg.getAttribute("data-tip") || "";
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
    // getAttribute already decoded the escaped payload text.
    tip.textContent = txt;
    tip.hidden = false;
    tip.style.left = Math.min(e.clientX + 12,
      window.innerWidth - tip.offsetWidth - 8) + "px";
    tip.style.top = (e.clientY + 16) + "px";
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
