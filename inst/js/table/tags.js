// @ts-check
/**
 * Tags on one swimlane that would overlap split above and below the centre
 * line. The tags are drawn by marks.js; this runs on the drawn rows.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  /**
   * Where each tag on a lane sits: 0 on the centre line, 1 below it, -1
   * above it. A tag whose box would run into the one before it starts a
   * pair: the earlier one moves down, the later one up, each at its own x.
   * A third tag in the same run goes down again, and so on.
   * @param {number[]} xs Left edges in px, in drawing order.
   * @param {number[]} ws Widths in px.
   * @param {number} [gap] Space a tag needs before the next one.
   * @returns {number[]}
   */
  function tagStack(xs, ws, gap = 2) {
    const ord = xs.map((_, k) => k).sort((a, b) => xs[a] - xs[b] || a - b);
    const out = xs.map(() => 0);
    let prev = -1;
    let run = 0;
    ord.forEach((k) => {
      if (prev >= 0 && xs[k] < xs[prev] + ws[prev] + gap) {
        if (run === 0) out[prev] = 1;
        run += 1;
        out[k] = run % 2 ? -1 : 1;
      } else {
        run = 0;
      }
      prev = k;
    });
    return out;
  }

  /**
   * Split overlapping tags on every swimlane in `host` above and below the
   * centre line (tagStack()). Needs the drawn widths, so it runs after the
   * rows are in the page and again on a resize. All widths are read before
   * any class is set, so the table is laid out once.
   * @param {ParentNode} host
   */
  function stackTags(host) {
    /** @type {{ tags: HTMLElement[], xs: number[], ws: number[] }[]} */
    const lanes = [];
    host.querySelectorAll('.vz-table-groups-ivcell').forEach((el) => {
      const tags = /** @type {HTMLElement[]} */ (Array.from(el.children)
        .filter((t) => t.classList.contains('is-tag')));
      if (tags.length < 2) return;
      const w = /** @type {HTMLElement} */ (el).clientWidth;
      if (!w) return;
      // A tag's box starts 3px after its position (the stylesheet's offset).
      lanes.push({
        tags,
        xs: tags.map((t) => parseFloat(t.style.left) / 100 * w + 3),
        ws: tags.map((t) => t.offsetWidth)
      });
    });
    lanes.forEach((l) => {
      tagStack(l.xs, l.ws).forEach((p, k) => {
        l.tags[k].classList.toggle('is-lo', p === 1);
        l.tags[k].classList.toggle('is-hi', p === -1);
      });
    });
  }

  V.table.tags = { tagStack, stackTags };
})();
