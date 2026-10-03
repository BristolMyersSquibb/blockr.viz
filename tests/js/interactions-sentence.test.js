/* The sentence on the chart's face: words that are controls (title_parts /
 * subtitle_parts / caption_parts with an `arg`), offers for settings whose
 * clause dropped, and what a pick through them sends to R. */
'use strict';

const test = require('node:test');
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
