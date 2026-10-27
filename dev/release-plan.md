# blockr.viz 0.3.0: what has to happen before CRAN

What this branch settled is marked done. The rest is ordered: each step needs
the one above it.

## Dependencies on CRAN, in order

| package | where | on CRAN | needed |
|---|---|---|---|
| blockr.core | Imports | 0.1.3 | `>= 0.1.4`: `new_arg_spec()` / `new_arg_specs()` and 5cb1b32 (core#379, restoring a block whose kind changed). Main is 0.1.4 and has both. Release blockr.core 0.1.4 first. |
| blockr.ui | Imports | no | viz calls `controls_dep()`, `table_preview_css()`, `column_widths_px()`, `action_menu()`, `tool_button()`, `menu_item()`, and its JS (`Blockr.Select`, `Blockr.layer`, `trayNav`, ...). Release blockr.ui second; blockr.dock imports it too. Vendoring is not an option: the JS is shared by every blockr package on a page. |
| blockr.dock | Suggests | 0.1.2 | the browser tests and `new_ctrl_bridge_extension()`. Check the tests against CRAN's 0.1.2; if they need 0.2.0, release blockr.dock before viz or skip them on an older dock. |
| blockr.dag | Suggests | 0.1.5 | one example. Fine as is. |
| blockr.theme, blockr.ai, blockr.assistant, blockr.dm | Suggests | no | every use sits behind `requireNamespace()`. Either serve them from an r-universe and add `Additional_repositories: https://bristolmyerssquibb.r-universe.dev` (one line, if the org publishes there), or drop them from Suggests and their tests. The NOTE is allowed in `tools/allowed-notes.txt` until then. |

`Remotes` stays in DESCRIPTION on the development branch, so CI and
`pak` install the development versions. The tarball sent to CRAN is built
without it: remove the field in the release commit, after blockr.core 0.1.4
and blockr.ui are on CRAN, and before `R CMD build`.

## DESCRIPTION

Done on this branch:

- Title and Description name the two blocks and quote 'blockr', 'ECharts',
  'Excel' and 'PowerPoint'.
- `blockr.core (>= 0.1.4)`.
- SnapDOM's copyright holder (ZumerLab) as `cph`; `inst/COPYRIGHTS` with
  version, licence and source.
- Suggests used nowhere were dropped: blockr.dplyr, blockr.extra, blockr.io
  (named only in comments), and base64enc (jsonlite, already imported,
  writes the data URIs). blockr.dm was added: test-table-drill.R uses it.
- `RoxygenNote` was dropped; `Config/roxygen2/version: 8.1.0` stays. The
  first full `roxygenise()` with roxygen2 8 rewrites the cross-package links
  in about ten Rd files; do it once, in its own commit.

Can go when the old files are deleted:

| package | used only by | goes with |
|---|---|---|
| gt (Suggests) | `R/gt-table-block.R` | the gt block, dropped for good |
| tibble (Imports) | `R/summary-table.R` (three calls) | the port of `summary_table()` to plain data frames |
| blockr.assistant (Suggests) | `R/describe-chart.R` (the old chart's `describe_block()` method) | the old chart block, unless the rewrite keeps the method |
| blockr.ai (Suggests) | `R/drilldown-ai-effect.R` (`config_effect()` methods for the old blocks) | the old blocks, unless the rewrite keeps the methods |
| callr, safetyData (Suggests) | one test each (`test-write-exhibit.R`, `test-html-table.R`) | those tests, if they go with the old renderers |

Every other Import is used by rewritten code: blockr.core, blockr.ui,
dplyr, echarts4r (the ECharts library and themes, until design.md S7
decides to vendor them), graphics, grDevices, htmltools, jsonlite, png and
ragg (the download footer and the R painter), rlang, shiny, stats, tidyr,
utils.

## Tests and checks

Done on this branch:

- No `library(shinytest2)`; the tests call `shinytest2::AppDriver`.
- One gate for every browser test, `run_browser_tests()` in
  `tests/testthat/helper-chromote.R`: never under R CMD check or covr.
  The two tests that used only `skip_on_cran()` started Chrome during check
  on CI (`NOT_CRAN=true`), which left the detritus NOTE.
- `tools/allowed-notes.txt` is matched line by line: a NOTE passes only when
  every line of it is allowed.
- A `browser` job in `ci.yaml` runs the browser tests with Chrome outside
  R CMD check, and fails when one was skipped.

Left:

- The full check matrix (macOS, Windows, ubuntu devel and oldrel) runs only
  on a push to main or on a PR with the `release` label (quality.md S13).
  Open the PR with the label before submitting.
- `R CMD check --as-cran` keeps a WARNING for blockr.ui until it is on CRAN.
