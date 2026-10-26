/* The sentence on the chart's face: words that are controls (title_parts /
 * subtitle_parts / caption_parts with an `arg`), offers for settings whose
 * clause dropped, and what a pick through them sends to R. */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const I = require('./interact');

const AE = { group: 'AETERM', drill: 'auto' };

/** Each band's pieces: plain text, a slot word, an offer chip. */
function sentence(block) {
  const band = (el) => {
    if (!el || el.style.display === 'none') return null;
    return Array.from(el.childNodes, (n) => {
      if (n.nodeType === 3) return n.textContent;
      const cls = n.className || '';
      if (/blockr-slot-offer/.test(cls)) return { offer: n.textContent, more: /--more/.test(cls) };
      if (/blockr-slot-input/.test(cls)) return { input: n.value };
      if (/blockr-slot/.test(cls)) return { slot: n.textContent, open: /blockr-slot--open/.test(cls) };
      return { other: cls, text: n.textContent };
    });
  };
  return { title: band(block.titleEl), subtitle: band(block.subtitleEl),
           caption: band(block.captionEl) };
}

/** The menu a slot opened last, while its list is up (blockr.ui's
 *  Select.menu owns this DOM; the chart's side of it is the open slot key). */
const openMenu = (env) => {
  const m = Array.from(env.win.document.querySelectorAll('.blockr-select__dropdown--menu')).pop();
  return m && m.querySelector('.blockr-select__option') ? m : null;
};

/** The slot key the chart holds open, and that menu's title and options. */
function menu(c) {
  const key = c.block._slotsInst ? c.block._slotsInst._key : null;
  if (!key) return { key: null };
  const m = openMenu(c.env);
  const title = m && m.querySelector('.blockr-select__menu-title');
  return {
    key,
    title: title ? title.textContent : null,
    options: m ? Array.from(m.querySelectorAll('.blockr-select__option'), (o) =>
      (o.classList.contains('blockr-select__option--selected') ? '*' : '') +
      o.getAttribute('data-value')) : null
  };
}

const word = (text, band = 'subtitleEl') => [`click word "${text}"`, (c) => {
  const w = Array.from(c.block[band].querySelectorAll('.blockr-slot'))
    .find((x) => x.textContent === text);
  if (!w) throw new Error(`no word ${text}`);
  w.click();
}];
const offer = (text) => [`click offer "${text}"`, (c) => {
  const o = Array.from(c.block.subtitleEl.querySelectorAll('.blockr-slot-offer'))
    .find((x) => x.textContent === text);
  if (!o) throw new Error(`no offer ${text}`);
  o.click();
}];
const pick = (value) => [`pick ${value}`, (c) => {
  const m = openMenu(c.env);
  if (!m) throw new Error('no menu open');
  const o = Array.from(m.querySelectorAll('.blockr-select__option'))
    .find((x) => x.getAttribute('data-value') === value);
  if (!o) throw new Error(`no option ${value}`);
  o.click();
}];
const typeWord = (value) => [`type "${value}" + Enter over the word`, (c) => {
  const inp = c.block.el.querySelector('.blockr-slot-input');
  inp.value = value;
  inp.dispatchEvent(new c.env.win.KeyboardEvent('keydown', { key: 'Enter', bubbles: true }));
}];
const echo = () => ['R echoes the config', (c) => c.echo()];

const parts = [
  { text: 'Count of rows by ' },
  { text: 'Reported Term', arg: 'group', by: 'label' },
  { text: ', coloured by ' },
  { text: 'AESEV', arg: 'color' },
  { text: ', line at ' },
  { text: '5', arg: 'value_lines' },
  { text: ', ' },
  { text: 'Serious only', arg: 'serious' },
  { text: ', ' },
  { text: 'Count', arg: 'func' }
];

const config = {
  chart_type: 'bar', ...AE, color: 'AESEV', value_lines: [5],
  subtitle_resolved: 'Count of rows by Reported Term, coloured by AESEV, line at 5',
  subtitle_parts: parts,
  subtitle_offers: ['facet', 'count_on', 'tt_fields', 'sort_by'],
  title_resolved: 'Adverse events',
  title_offers: ['facet'],
  script: 'serious <- TRUE',
  script_inputs: [{ name: 'serious', key: 'sv_serious', label: 'Serious only', kind: 'segmented',
                    options: [{ value: 'on', label: 'Serious only' },
                              { value: 'off', label: 'Serious only' }] }],
  sv_serious: 'on',
  sentence_args: ['serious']
};

test('sentence words and offers', () => {
  const r = I.runSteps(config, [
    ['painted', () => {}],
    word('AESEV'),
    word('AESEV'),
    word('AESEV'),
    pick('SEX'),
    // The word just picked through, again.
    word('AESEV'),
    word('Reported Term'),
    pick('AVISIT'),
    word('Count'),
    pick('mean'),
    word('5'),
    typeWord('7, 9'),
    word('Serious only'),
    word('Serious only'),
    offer('+ Facet'),
    pick('ARM'),
    ['click offer "More settings"', (c) =>
      c.block.subtitleEl.querySelector('.blockr-slot-offer--more').click()],
    echo()
  ], { extra: (c) => ({ sentence: sentence(c.block), menu: menu(c),
                        gearOpen: !!c.block._popoverOpen,
                        band: I.bandOutline(c.block) }) });
  I.snap('sentence-slots', r);
});

// A segmented setting in the sentence ("stacked") opens its options as a
// list, as the gear's button row would offer them.
test('a segmented word opens its list', () => {
  const cfg = {
    chart_type: 'bar', ...AE, color: 'AESEV', bar_mode: 'stacked',
    subtitle_resolved: 'stacked',
    subtitle_parts: [{ text: 'stacked', arg: 'bar_mode' }],
    sentence_args: ['bar_mode']
  };
  const r = I.runSteps(cfg, [word('stacked'), pick('grouped')],
                       { extra: (c) => ({ menu: menu(c) }) });
  I.snap('sentence-segmented', r);
});

// A number whose word lists its values (`{@bins in 10, 30, 60}`) opens
// that list; without a list the same word is typed over.
test('a number word with listed values opens them as a list', () => {
  const cfg = {
    chart_type: 'histogram', x: 'AVAL', group: 'AVAL_bin', value: '.count', func: 'count',
    drill: 'auto', bins: 30,
    subtitle_resolved: '30 bins',
    subtitle_parts: [{ text: '30', arg: 'bins' }, { text: ' bins' }],
    slot_choices: { bins: ['10', '30', '60'] },
    sentence_args: ['bins']
  };
  const r = I.runSteps(cfg, [word('30'), pick('10')],
                       { extra: (c) => ({ menu: menu(c), sentence: sentence(c.block) }) });
  I.snap('sentence-listed-number', r);
});

// A number word is typed over, not a list: its range beside the field, the
// arrows stepping it, and a value outside the range held at the nearest end.
test('a number word is typed over, held inside its range', () => {
  const numConfig = {
    chart_type: 'bar', ...AE,
    subtitle_resolved: 'Terms reported by at least 5 patients',
    subtitle_parts: [{ text: 'Terms reported by at least ' },
                     { text: '5', arg: 'min_n' }, { text: ' patients' }],
    title_resolved: 'Adverse events',
    script: 'min_n <- 5',
    script_inputs: [{ name: 'min_n', key: 'sv_min_n', label: 'Minimum patients',
                      kind: 'number', min: 0, max: 50, step: 1 }],
    sv_min_n: 5,
    sentence_args: ['min_n']
  };
  const field = (c) => c.block.el.querySelector('.blockr-slot-input');
  const range = (c) => {
    const n = c.block.el.querySelector('.blockr-slot-range');
    return n ? { text: n.textContent, out: n.classList.contains('blockr-slot-range--out') } : null;
  };
  const key = (k, shift = false) => [`press ${shift ? 'Shift+' : ''}${k}`, (c) =>
    field(c).dispatchEvent(new c.env.win.KeyboardEvent('keydown', { key: k, shiftKey: shift, bubbles: true }))];
  // The test's R does not re-resolve the subtitle, so the word keeps its
  // first text; it is found by position.
  const numWord = ['click the number word', (c) =>
    c.block.subtitleEl.querySelector('.blockr-slot').click()];
  const typeOnly = (v) => [`type "${v}"`, (c) => {
    const f = field(c);
    f.value = v;
    f.dispatchEvent(new c.env.win.Event('input', { bubbles: true }));
  }];
  const r = I.runSteps(numConfig, [
    numWord,
    typeWord('12'),
    echo(),
    numWord,
    typeOnly('60'),
    key('Enter'),
    echo(),
    numWord,
    key('ArrowDown'),
    key('ArrowDown', true),
    key('Enter'),
    echo(),
    numWord,
    typeOnly(''),
    key('Enter'),
    numWord,
    typeOnly('3'),
    key('Escape')
  ], { extra: (c) => ({ sentence: sentence(c.block), field: field(c) ? field(c).value : null,
                        range: range(c), menu: menu(c).key }) });
  I.snap('sentence-number', r);
});

// A list written in the sentence (`{@color in SEX, ARM}`) narrows the word's
// menu to those choices, in that order. The current pick stays, a name the
// data does not carry is left out, and "(none)" stays on an optional role.
test('a sentence list narrows the menu behind its word', () => {
  const assert = require('node:assert');
  const listConfig = { ...config, slot_choices: { color: ['SEX', 'NOPE', 'ARM'],
                                                  func: ['mean', 'median'] } };
  const seen = {};
  I.runSteps(listConfig, [
    word('AESEV'),
    ['read colour menu', (c) => { seen.color = menu(c).options; }],
    word('AESEV'),
    word('Count'),
    ['read func menu', (c) => { seen.func = menu(c).options; }]
  ]);
  assert.deepStrictEqual(seen.color, ['(none)', 'SEX', 'ARM', '*AESEV']);
  assert.deepStrictEqual(seen.func, ['mean', 'median', '*count']);
});

// The gear's title editor warns about names nothing answers to. A list is
// not part of the name, so `{@func in mean, median}` is not flagged.
test('the title editor does not flag a sentence list', () => {
  const assert = require('node:assert');
  let warn;
  I.runSteps(config, [['check', (c) => {
    const doc = c.env.win.document;
    const row = doc.createElement('div');
    const after = doc.createElement('div');
    row.appendChild(after);
    const fake = { _atNames: () => new Set(['func', '@func', 'value', '@value']) };
    c.env.win.DrilldownConfig.prototype._paintTitleWarning.call(
      fake, row, after,
      '{@func in mean, median} {label(@value in AVAL, CHG)} {@nope in a}');
    const el = row.querySelector('.dd-title-warn');
    warn = el ? el.textContent : null;
  }]]);
  assert.strictEqual(warn, 'Nothing is called {@nope in a}. It prints nothing, and takes its [ ] clause with it.');
});

test('a min-only number word does not pollute the sentence while valid', () => {
  const minOnlyConfig = {
    chart_type: 'bar', ...AE,
    subtitle_resolved: 'terms in at least 1 Patients',
    subtitle_parts: [{ text: 'terms in at least ' },
                     { text: '1', arg: 'min_share' },
                     { text: ' Patients' }],
    title_resolved: 'Adverse events',
    script: 'min_share <- 1',
    script_inputs: [{ name: 'min_share', key: 'sv_min_share',
                      label: 'Minimum threshold', kind: 'number',
                      min: 0, step: 1 }],
    sv_min_share: 1,
    sentence_args: ['min_share']
  };

  const c = I.open(minOnlyConfig);
  try {
    c.block.subtitleEl.querySelector('.blockr-slot').click();

    const input = c.block.subtitleEl.querySelector('.blockr-slot-input');
    const note = c.block.subtitleEl.querySelector('.blockr-slot-range');

    assert.ok(input);
    assert.ok(note);
    assert.equal(note.hidden, true);
    assert.equal(note.textContent, '');

    input.value = '-1';
    input.dispatchEvent(new c.env.win.Event('input', { bubbles: true }));

    assert.equal(note.hidden, false);
    assert.equal(note.textContent, 'at least 0');
  } finally {
    c.close();
  }
});
