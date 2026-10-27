// The cards look of the table block (R/table-cards.R).
//
// One renderer for both layouts. The engines' own scripts call it:
// summarize-table.js for the groups layout, table.js for the records layout.
// They hand it the card model from the payload and two callbacks, `glyph`
// (the summarize table's own emitter for one cell, so a card draws exactly
// the mark the table draws) and `row` (the classes and data attributes of a
// row, the same ones the engine's table rows carry, so its drill, search and
// highlight code works on card rows unchanged).
//
// A downloaded page has no engine: its host carries the model as JSON and
// this file draws it on load, with the glyph emitters of summarize-table.js
// when the page ships them.
//
// The layout follows the width of the host, read with a ResizeObserver:
//   wide    one row head column, then one column per card, column heads on
//   medium  each row its own line: the row head above, the cards wrapping
//   narrow  one card per line, the glyph to the right of the number, no legend
(function () {
  "use strict";

  var NARROW = 320;
  var CARD_MIN = 150;
  var HEAD_W = 120;

  function esc(x) {
    return String(x == null ? "" : x)
      .replace(/&/g, "&amp;").replace(/</g, "&lt;")
      .replace(/>/g, "&gt;").replace(/"/g, "&quot;");
  }

  function legendHtml(items) {
    if (!items || !items.length) return "";
    return '<div class="tc-legend">' + items.map(function (it) {
      return '<span><i style="background:' + esc(it.color) + '"></i>' +
        esc(it.label) + "</span>";
    }).join("") + "</div>";
  }

  function cardHtml(cards, k, i, glyph) {
    var c = cards.cols[k];
    var main = c.main ? c.main[i] : "";
    var sub = c.sub ? c.sub[i] : null;
    var g = (c.glyph != null && glyph) ? glyph(c.glyph, i) : "";
    var out = '<div class="tc' + (c.text ? " is-text" : "") + '">' +
      '<div class="tc-lab" title="' + esc(c.label) + '">' + esc(c.label) +
      "</div>" +
      '<div class="tc-val">' + (main === "" || main == null ?
        '<span class="tc-na">—</span>' : esc(main)) + "</div>";
    if (c.sub) {
      // The statistic's word prints in the card of a single row; a column
      // of cards names it in its head (CSS hides it there).
      out += '<div class="tc-sub">' + (sub && c.word ?
        '<span class="tc-word">' + esc(c.word) + ": </span>" : "") +
        esc(sub || "") + "</div>";
    }
    if (g) {
      out += '<div class="tc-glyph" data-k="' + c.glyph + '">' + g + "</div>";
    }
    if (!cards.multi) out += legendHtml(c.legend);
    return out + "</div>";
  }

  function headHtml(cards) {
    var out = '<div class="tc-corner"></div>';
    cards.cols.forEach(function (c) {
      out += '<div class="tc-colhead"><b title="' + esc(c.label) + '">' +
        esc(c.label) + "</b>" +
        (c.word || c.range ? "<span>" + esc([c.word, c.range].filter(Boolean)
          .join(" \u00b7 ")) + "</span>" : "") +
        legendHtml(c.legend) + "</div>";
    });
    return out;
  }

  /**
   * Draw a card model into `host`.
   * @param {HTMLElement} host
   * @param {any} cards the payload's `cards`
   * @param {{glyph?: function(number, number): string,
   *          row?: function(number): {cls?: string, attrs?: string, pre?: string},
   *          order?: number[]}} [opts]
   */
  function render(host, cards, opts) {
    opts = opts || {};
    if (!host || !cards || !cards.cols) return;
    var n = cards.n || 0;
    var order = opts.order || null;
    if (!order) {
      order = [];
      for (var i = 0; i < n; i++) order.push(i);
    }
    var multi = !!cards.multi;
    var out = '<div class="tc-grid' + (multi ? " is-multi" : "") +
      '" style="--tc-k:' + cards.cols.length + '">';
    if (multi) out += headHtml(cards);
    order.forEach(function (i) {
      var r = opts.row ? (opts.row(i) || {}) : {};
      var hex = cards.hex ? cards.hex[i] : null;
      out += '<div class="tc-row' + (r.cls ? " " + r.cls : "") + '"' +
        (r.attrs || "") + ">";
      if (multi) {
        out += '<div class="tc-rowhead">' + (r.pre || "") +
          (cards.hex ? '<i class="tc-strip"' + (hex ? ' style="background:' +
            esc(hex) + '"' : ' style="visibility:hidden"') + "></i>" : "") +
          '<span class="tc-rowlab">' + esc(cards.label[i]) + "</span></div>";
      }
      for (var k = 0; k < cards.cols.length; k++) {
        out += cardHtml(cards, k, i, opts.glyph);
      }
      out += "</div>";
    });
    out += "</div>";
    if (cards.fold) out += '<div class="tc-fold">' + esc(cards.fold) + "</div>";
    if (!cards.cols.length) {
      out = '<div class="tc-empty">No summary to show as a card.</div>';
    }
    host.innerHTML = out;
    host._tcCards = cards;
    fit(host);
    watch(host);
  }

  // The layout mode, from the host's width.
  function fit(host) {
    var grid = host.querySelector(".tc-grid");
    var cards = host._tcCards;
    if (!grid || !cards) return;
    var w = host.clientWidth || host.getBoundingClientRect().width || 0;
    var k = cards.cols.length;
    var mode;
    if (w > 0 && w < NARROW) {
      mode = "narrow";
    } else if (cards.multi && w > 0 && w < HEAD_W + k * CARD_MIN) {
      mode = "medium";
    } else {
      mode = "wide";
    }
    if (grid.getAttribute("data-tc-mode") !== mode) {
      grid.setAttribute("data-tc-mode", mode);
    }
  }

  var observer = (typeof ResizeObserver === "function") ?
    new ResizeObserver(function (entries) {
      entries.forEach(function (e) { fit(e.target); });
    }) : null;

  function watch(host) {
    if (host._tcWatched || !observer) return;
    host._tcWatched = true;
    observer.observe(host);
  }

  // A downloaded page: the model sits in the host as JSON.
  function renderStatic(host) {
    if (host._tcStatic) return;
    var s = host.querySelector('script[type="application/json"]');
    if (!s) return;
    var data = null;
    try { data = JSON.parse(s.textContent); } catch (e) { data = null; }
    if (!data || !data.cards) return;
    host._tcStatic = true;
    var G = window.BlockrSummarizeGlyphs;
    var cols = data.cols || [];
    render(host, data.cards, {
      glyph: (G && cols.length) ? function (k, i) {
        return cols[k] ? G.glyph(cols[k], i) : "";
      } : null
    });
  }

  function scanStatic() {
    document.querySelectorAll(".blockr-cards-host[data-cards-static]")
      .forEach(renderStatic);
  }

  window.BlockrTableCards = { render: render, fit: fit };

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", scanStatic);
  } else {
    scanStatic();
  }
  // The engines may have a payload waiting for this script: tell them.
  document.dispatchEvent(new CustomEvent("blockr-table-cards-ready"));
})();
