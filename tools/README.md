# tools/

Checks for the exit criteria of the rewrite (design.md section 11). Not part
of the built package (`.Rbuildignore`). CI runs them all in the
`exit-criteria` job; locally:

    tools/run-checks.sh          # everything, about 10 minutes
    tools/run-checks.sh --fast   # without R tests, coverage and R CMD check

| check | script | fails when |
|---|---|---|
| bundles | `bundle.js --check`, `tests/testthat/test-bundle.R` | a bundle in `inst/js/dist` or `inst/css/dist` was not rebuilt after a source edit |
| dead R | `dead-r.R` | a namespace object no root reaches; second list: those tests still use |
| dead JS | `dead-js.js` | a function nothing references (locals: eslint `no-unused-vars`) |
| dead CSS | `dead-css.js` | a class no R or JS source emits |
| R lengths | `lengths.R` | a function over 80 lines without `# viz: long <reason>` before it, a file over 600 lines |
| JS lengths | eslint `max-lines-per-function`, `max-lines` | the same; the JS marker is `// eslint-disable-next-line max-lines-per-function -- viz: long <reason>` |
| size budget | `check-budget.R`, `budget.csv` | a part over its budget, a file in no part or in two |
| gates | lintr, `npm run lint`, `npm run typecheck`, `check-rcmdcheck.R` with `allowed-notes.txt` | any lint, type error, ERROR, WARNING or unlisted NOTE |
| browser tests | `check-skips.R` | a shinytest2 or chromote test skipped |
| coverage | `check-coverage-r.R`, `npm run coverage` | R or JS line coverage under 90% |
| features | `check-features.R`, `features/*.md` | a catalogue row without `[test: <file> :: <test name>]`, or a link to a test that is missing or did not pass |
| comments | `check-comments.R` | never; lists history comments for the final audit |

Results land in `results/` (JUnit from testthat and node) and `coverage/`.

## Bundles

The page loads one script per view (`inst/js/dist/core.js`, `chart.js`,
`table.js`) and one stylesheet each for the core and the table
(`inst/css/dist/`). `node tools/bundle.js` writes them from the lists in
`inst/js/<view>/scripts.txt` and `inst/css/styles.txt`; they are committed,
since R cannot run node at install time. After a merge that touches
`inst/js` or `inst/css`, rebuild rather than resolve conflicts in `dist/`:
take either side and run `node tools/bundle.js`. With
`options(blockr.viz.dev_js = TRUE)` the dependencies list the single sources
instead, so a JS edit shows on reload without a rebuild. The node tests,
`dead-js.js` and coverage read the sources and never the bundles.
