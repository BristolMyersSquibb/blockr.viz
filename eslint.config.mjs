// Run: npx eslint inst/js
//
// The old scripts in inst/js/*.js keep their one rule. The rewritten ones
// (inst/js/core/, and the block folders as they arrive) get the full set,
// and may set up listeners, observers, timers and chart instances only
// through a Scope (inst/js/core/scope.js), so all of it goes with its block.
const browserGlobals = {
  window: 'readonly', document: 'readonly', globalThis: 'readonly',
  setTimeout: 'readonly', clearTimeout: 'readonly', setInterval: 'readonly',
  clearInterval: 'readonly', requestAnimationFrame: 'readonly',
  cancelAnimationFrame: 'readonly', ResizeObserver: 'readonly',
  MutationObserver: 'readonly', console: 'readonly', Node: 'readonly',
  Element: 'readonly', HTMLElement: 'readonly', EventTarget: 'readonly'
};

const outsideScope = [
  { selector: "CallExpression[callee.property.name='addEventListener']",
    message: 'Listen through scope.on(), so the listener goes with its block.' },
  { selector: "CallExpression[callee.name=/^(setTimeout|setInterval|requestAnimationFrame)$/]",
    message: 'Use scope.timeout(), scope.interval() or scope.frame().' },
  { selector: "NewExpression[callee.name=/^(ResizeObserver|MutationObserver)$/]",
    message: 'Use scope.resize() or scope.mutate().' },
  { selector: "CallExpression[callee.object.name='echarts'][callee.property.name='init']",
    message: 'Hand the instance to scope.own().' }
];

export default [
  {
    files: ['inst/js/*.js'],
    ignores: ['inst/js/snapdom.js'],
    languageOptions: { ecmaVersion: 2022, sourceType: 'script' },
    rules: { 'no-shadow': 'error' }
  },
  {
    files: ['inst/js/core/**/*.js', 'inst/js/spec/**/*.js'],
    languageOptions: { ecmaVersion: 2022, sourceType: 'script', globals: browserGlobals },
    rules: {
      'no-shadow': 'error',
      'no-unused-vars': 'error',
      'no-undef': 'error',
      'no-implicit-globals': 'error',
      'no-var': 'error',
      'prefer-const': 'error',
      eqeqeq: ['error', 'always', { null: 'ignore' }],
      'no-console': 'error',
      'no-empty': 'error',
      'no-loop-func': 'error',
      'no-redeclare': 'error',
      'no-restricted-syntax': ['error', ...outsideScope]
    }
  },
  {
    files: ['inst/js/core/scope.js'],
    rules: { 'no-restricted-syntax': 'off' }
  }
];
