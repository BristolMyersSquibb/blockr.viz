// Guard against loop/local variables shadowing destructured config names in
// the widget JS (a shadowed `value`/`facet` silently broke count_distinct
// aggregation and faceted panels). Run: npx eslint inst/js
export default [
  {
    files: ['inst/js/*.js'],
    languageOptions: { ecmaVersion: 2022, sourceType: 'script' },
    rules: { 'no-shadow': 'error' }
  }
];
