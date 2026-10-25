// @ts-check
// Register the "blockr" echarts theme directly in the browser. The drill-down
// block initialises echarts via `echarts.init(el, name)`, so the name must be
// known to `window.echarts` before init runs. Echarts' built-in themes (dark,
// vintage, etc.) are loaded from echarts4r's bundled theme directory via a
// separate htmlDependency; this file only handles the custom "blockr" palette.
(() => {
  if (typeof window === 'undefined' || !window.echarts) return;
  if (typeof window.echarts.registerTheme !== 'function') return;
  window.echarts.registerTheme('blockr', {
    color: ['#0072B2', '#E69F00', '#009E73', '#CC79A7', '#56B4E9', '#D55E00'],
    backgroundColor: '#ffffff',
    textStyle: { color: '#333333', fontFamily: 'Open Sans' }
  });
})();
