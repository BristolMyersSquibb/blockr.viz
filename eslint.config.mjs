// eslint for the hand-written JS in inst/js. Run: npm run lint
//
// Besides the usual correctness rules it enforces two exit criteria of the
// rewrite (design.md section 11): no function over 80 lines (blank lines and
// comments not counted) and no file over 600 lines. A function that has to
// be longer says why on the line before:
//
//   // eslint-disable-next-line max-lines-per-function -- viz: long <reason>
//
// The file-wrapping IIFEs are not counted as functions.
import globals from 'globals';

export default [
  { ignores: ['inst/js/snapdom.js', 'inst/js/spec/**'] },
  {
    files: ['inst/js/**/*.js'],
    languageOptions: {
      ecmaVersion: 2022,
      sourceType: 'script',
      globals: {
        ...globals.browser,
        Shiny: 'readonly', echarts: 'readonly', HTMLWidgets: 'readonly',
        $: 'readonly', jQuery: 'readonly', Blockr: 'readonly',
        // The node harness reads what a file exports this way.
        module: 'writable'
      }
    },
    linterOptions: { reportUnusedDisableDirectives: 'error' },
    rules: {
      'no-shadow': 'error',
      'no-undef': 'error',
      'no-unused-vars': ['error', { args: 'none', caughtErrors: 'none' }],
      'no-implicit-globals': 'error',
      'eqeqeq': ['error', 'always', { null: 'ignore' }],
      'no-console': ['error', { allow: ['warn', 'error'] }],
      'prefer-const': 'error',
      'no-empty': 'error',
      'no-debugger': 'error',
      'no-redeclare': 'error',
      'no-loop-func': 'error',
      'no-useless-escape': 'error',
      'max-lines': ['error', { max: 600, skipBlankLines: false, skipComments: false }],
      'max-lines-per-function': ['error', {
        max: 80, skipBlankLines: true, skipComments: true, IIFEs: false
      }]
    }
  }
];
