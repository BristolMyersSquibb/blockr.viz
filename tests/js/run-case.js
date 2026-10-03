/* Draw one case from cases.js and reduce it to its snapshot. */
'use strict';

const test = require('node:test');
const h = require('./harness');
const { F } = require('./cases');
const { matchSnapshot } = require('./snapshot');

/** The snapshot value of a case: what it was given and what it drew. */
function caseSnapshot(cs) {
  const data = cs.data || (cs.dict ? F.data_dict : F.data);
  const columns = cs.columns || F.columns;
  const r = h.draw(cs.config, data, columns,
                   { width: cs.width, height: cs.height, theme: cs.theme,
                     keepOpen: !!cs.resend });
  let { error } = r;
  if (cs.resend) {
    try {
      h.send(r.env, r.id, { columns, data, config: { ...cs.config, ...cs.resend } });
    } catch (e) {
      error = String(e);
    }
    r.options = h.slotLogs(r.block);
    r.dom = h.domSummary(r.block);
    r.env.close();
  }
  // Through JSON: the snapshot is JSON, and this also leaves no object from
  // the window's realm behind.
  return JSON.parse(JSON.stringify({
    config: cs.config,
    resend: cs.resend || null,
    error,
    dom: r.dom,
    slots: r.options,
    disposed: r.disposed,
    inputs: r.inputs
  }));
}

/** One node:test per case, compared with its snapshot file. */
function snapshotTests(list) {
  for (const cs of list) {
    test(cs.name, () => matchSnapshot(cs.name, caseSnapshot(cs)));
  }
}

module.exports = { caseSnapshot, snapshotTests };
