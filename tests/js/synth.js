/* Synthetic subject-level data for the scaling tests: `n` patients, five
 * rows each, so patients and rows grow together. Deterministic (an LCG, no
 * Math.random), and shaped like an ADaM frame: an arm per patient, a
 * 200-level term, a 5-level grade, five visits, a day, a value, an interval.
 */
'use strict';

const N_TERMS = 200;
const ROWS_PER_PATIENT = 5;

function synth(n) {
  let seed = 12345;
  const rand = () => {
    seed = (seed * 1103515245 + 12345) % 2147483648;
    return seed / 2147483648;
  };
  const cols = {
    USUBJID: [], ARM: [], TERM: [], GRADE: [], AVISIT: [], AVISITN: [],
    ADY: [], AVAL: [], ASTDY: [], AENDY: []
  };
  for (let p = 0; p < n; p++) {
    const id = 'P' + String(p + 1).padStart(6, '0');
    const arm = ['Placebo', 'Low Dose', 'High Dose'][p % 3];
    for (let v = 0; v < ROWS_PER_PATIENT; v++) {
      const start = Math.floor(rand() * 300);
      cols.USUBJID.push(id);
      cols.ARM.push(arm);
      cols.TERM.push('Term ' + String(Math.floor(rand() * N_TERMS)).padStart(3, '0'));
      cols.GRADE.push('Grade ' + (1 + Math.floor(rand() * 5)));
      cols.AVISIT.push('Visit ' + (v + 1));
      cols.AVISITN.push(v + 1);
      cols.ADY.push(v * 14 + Math.floor(rand() * 7));
      cols.AVAL.push(Math.round((40 + 10 * rand() + v) * 100) / 100);
      cols.ASTDY.push(start);
      cols.AENDY.push(start + 1 + Math.floor(rand() * 30));
    }
  }
  const nUnique = (k) => new Set(cols[k]).size;
  const cat = (name) => ({ name, type: 'categorical', n_unique: nUnique(name) });
  const numc = (name) => ({ name, type: 'numeric', n_unique: nUnique(name) });
  const columns = [cat('USUBJID'), cat('ARM'), cat('TERM'), cat('GRADE'), cat('AVISIT'),
                   numc('AVISITN'), numc('ADY'), numc('AVAL'), numc('ASTDY'), numc('AENDY')];
  return { columns, data: cols, rows: n * ROWS_PER_PATIENT };
}

module.exports = { synth, ROWS_PER_PATIENT };
