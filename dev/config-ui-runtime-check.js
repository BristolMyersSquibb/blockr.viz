// Headless runtime smoke-test of drilldown-chart.js config paths.
// Stubs DOM/Shiny/echarts/Blockr, injects an export, exercises the new
// role-spec rendering + family switch + sticky memory + add/remove.
const fs = require('fs');
const vm = require('vm');

function makeEl(tag) {
  const set = new Set();
  const el = {
    tagName: (tag || 'div').toUpperCase(),
    children: [], parentNode: null, _l: {}, style: {}, dataset: {}, _attr: {},
    _class: '',
    get className() { return this._class; },
    set className(v) { this._class = v; },
    classList: {
      add: (...c) => c.forEach(x => set.add(x)),
      remove: (...c) => c.forEach(x => set.delete(x)),
      toggle: (c, on) => { const has = set.has(c); const want = on === undefined ? !has : on; want ? set.add(c) : set.delete(c); return want; },
      contains: (c) => set.has(c),
    },
    set innerHTML(_v) { this.children = []; },
    get innerHTML() { return ''; },
    textContent: '', title: '', type: '', value: '', min: '', max: '', step: '',
    appendChild(c) { this.children.push(c); c.parentNode = this; return c; },
    removeChild(c) { this.children = this.children.filter(x => x !== c); return c; },
    insertBefore(c) { this.children.unshift(c); c.parentNode = this; return c; },
    addEventListener(ev, fn) { (this._l[ev] = this._l[ev] || []).push(fn); },
    removeEventListener() {},
    querySelectorAll() { return []; },
    querySelector() { return null; },
    contains() { return false; },
    setAttribute(k, v) { this._attr[k] = v; },
    getAttribute(k) { return this._attr[k]; },
    focus() {},
    getBoundingClientRect() { return { top: 0, left: 0, width: 100, height: 100, bottom: 100, right: 100 }; },
  };
  return el;
}

const documentStub = {
  createElement: (t) => makeEl(t),
  createElementNS: (_ns, t) => makeEl(t),
  body: makeEl('body'),
  getElementById: () => null,
  addEventListener() {}, removeEventListener() {},
};

const chartStub = () => ({
  setOption() {}, on() {}, off() {}, dispose() {}, resize() {},
  dispatchAction() {}, getZr: () => ({ on() {}, off() {} }),
  getWidth: () => 100, getHeight: () => 100,
  getOption: () => ({ series: [{}] }), getModel: () => ({}),
});

const sandbox = {
  console,
  setTimeout, clearTimeout, setInterval: () => 0, clearInterval() {},
  document: documentStub,
  window: { addEventListener() {}, removeEventListener() {}, devicePixelRatio: 1 },
  ResizeObserver: class { observe() {} disconnect() {} unobserve() {} },
  Shiny: { InputBinding: class { constructor() {} }, inputBindings: { register() {} }, addCustomMessageHandler() {}, setInputValue() {}, onInputChange() {} },
  echarts: { init: () => chartStub(), graphic: {}, },
  Blockr: {
    Select: { single: (wrap, o) => ({ destroy() {}, getValue: () => o.selected, setOptions() {}, el: makeEl('div') }) },
    icons: { gear: '⚙', plus: '+' },
  },
  jsonlite: null,
};
sandbox.globalThis = sandbox;
sandbox.self = sandbox;

const jsdir = require('path').join(__dirname, '..', 'inst', 'js');
vm.createContext(sandbox);
// The shared config engine must load first (drilldown-chart.js references
// Blockr.DrilldownConfig).
vm.runInContext(fs.readFileSync(require('path').join(jsdir, 'drilldown-config.js'), 'utf8'),
  sandbox, { filename: 'drilldown-config.js' });

let code = fs.readFileSync(require('path').join(jsdir, 'drilldown-chart.js'), 'utf8');
// Expose the class for introspection: inject before the final IIFE close.
code = code.replace(/\}\)\(\);\s*$/, 'globalThis.__DrilldownChart = DrilldownChart;\n})();');

vm.runInContext(code, sandbox, { filename: 'drilldown-chart.js' });
const DrilldownChart = sandbox.__DrilldownChart;
if (!DrilldownChart) { console.error('FAIL: class not exposed'); process.exit(1); }

const columns = [
  { name: 'cyl', type: 'categorical', n_unique: 3 },
  { name: 'mpg', type: 'numeric', n_unique: 25, label: 'Miles per gallon' },
  { name: 'hp', type: 'numeric', n_unique: 22 },
  { name: 'gear', type: 'categorical', n_unique: 3 },
  { name: 'am', type: 'categorical', n_unique: 2 },
];
const data = JSON.stringify({
  cyl: ['4', '6', '8', '4'], mpg: [21, 18, 15, 24],
  hp: [110, 120, 175, 95], gear: ['4', '4', '3', '4'], am: ['manual', 'auto', 'auto', 'manual'],
});

let pass = 0, fail = 0;
function step(name, fn) {
  try { fn(); pass++; console.log('  ok  ', name); }
  catch (e) { fail++; console.log('  FAIL', name, '->', e && e.message, '\n', e && e.stack && e.stack.split('\n').slice(1,4).join('\n')); }
}

const el = makeEl('div');
let inst;
step('construct + _buildDOM', () => { inst = new DrilldownChart(el); el._block = inst; });
step('setData (bar) + render + renderConfig', () => inst.setData(columns, data, { chart_type: 'bar', group: 'cyl', metric: '.count', agg_fn: 'count', sort_by: 'value', sort_dir: 'desc', orientation: 'horizontal', drill: 'cyl' }));
step('open popover (renderConfig)', () => inst._renderConfig());
step('add optional role (color)', () => inst._addRole('color'));
step('remove optional role (color)', () => inst._removeRole('color'));
step('orientation vertical', () => { inst.config.orientation = 'vertical'; inst._render(); });
step('switch family bar -> scatter (clears x/y)', () => inst._onChartType('scatter'));
step('switch scatter -> bar (memory restore)', () => inst._onChartType('bar'));
step('switch bar -> gantt', () => inst._onChartType('gantt'));
step('switch gantt -> boxplot', () => inst._onChartType('boxplot'));
step('required empty-state (clear group)', () => { inst._onChartType('scatter'); inst.config.x = ''; inst.config.y = ''; inst._render(); });

// --- Behavioral assertions: identity-carry + sticky memory ---
function assert(cond, msg) { if (!cond) throw new Error('assert: ' + msg); }
const el2 = makeEl('div');
const b = new DrilldownChart(el2); el2._block = b;
step('behavior: setup bar with group + shared color', () => {
  b.setData(columns, data, { chart_type: 'bar', group: 'cyl', metric: '.count', agg_fn: 'count' });
  b.config.color = 'am'; b._rememberRole('color', 'am');
});
step('behavior: bar->scatter keeps shared color, clears positional group', () => {
  b._onChartType('scatter');
  assert(b.config.color === 'am', 'color (shared) should carry across families');
  assert(!b._hasVal(b.config.group), 'group (positional) should clear on cross-family switch');
});
step('behavior: scatter->bar restores group from sticky memory', () => {
  b._onChartType('bar');
  assert(b.config.group === 'cyl', 'group should be restored from sticky memory on switch-back');
});

console.log(`\n${pass} passed, ${fail} failed`);
process.exit(fail ? 1 : 0);
