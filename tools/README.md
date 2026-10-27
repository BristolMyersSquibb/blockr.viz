# tools/

Checks for the exit criteria of the rewrite (design.md section 11). Not part
of the built package (`.Rbuildignore`). CI runs them all in the
`exit-criteria` job; locally:

    tools/run-checks.sh          # everything, about 10 minutes
    tools/run-checks.sh --fast   # without R tests, coverage and R CMD check

| check | script | fails when |
|---|---|---|
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
