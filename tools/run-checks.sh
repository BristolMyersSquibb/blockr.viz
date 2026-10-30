#!/usr/bin/env bash
# The rewrite's exit criteria (design.md section 11), one after another.
# Every check runs; the script prints each report and a summary, and exits 1
# if any check failed.
#
#   tools/run-checks.sh [--fast]
#
# --fast leaves out the slow ones (R tests, coverage, R CMD check). Needs
# the package's R dependencies, node with `npm ci` done, and BLOCKR_UI_JS
# pointing at blockr.ui's inst/assets/js for the JS tests.

set -u
cd "$(dirname "$0")/.."
fast=0
[ "${1:-}" = "--fast" ] && fast=1
out=$PWD/results
mkdir -p "$out"
summary=()
failed=0

step() {
  local name=$1
  shift
  echo
  echo "== $name"
  if "$@"; then
    summary+=("pass  $name")
  else
    summary+=("FAIL  $name")
    failed=1
  fi
}

step "dead R"               Rscript tools/dead-r.R .
step "dead JS"              node tools/dead-js.js .
step "dead CSS"             node tools/dead-css.js .
step "R lengths"            Rscript tools/lengths.R .
step "lintr"                Rscript -e 'l <- lintr::lint_package(); print(l); quit(status = length(l) > 0)'
step "eslint (incl. JS lengths, dead locals)" npm run --silent lint
step "tsc"                  npm run --silent typecheck
step "JS tests"             node --test --test-concurrency=1 \
                              --test-reporter=spec --test-reporter-destination=stdout \
                              --test-reporter=junit --test-reporter-destination="$out/node.xml" \
                              tests/js/*.test.js tests/js/chart/*.test.js \
                              tests/js/core/*.test.js tests/js/table/*.test.js
if [ "$fast" = 0 ]; then
  step "R tests"            Rscript -e '
    r <- testthat::test_local(".", reporter = testthat::MultiReporter$new(list(
      testthat::SummaryReporter$new(),
      testthat::JunitReporter$new(file = file.path(getwd(), "results/testthat.xml")))),
      stop_on_failure = FALSE)
    d <- as.data.frame(r)
    quit(status = sum(d$failed) + sum(d$error) > 0)'
  step "browser tests ran"  Rscript tools/check-skips.R "$out/testthat.xml"
  step "features"           Rscript tools/check-features.R \
                              --results "$out/testthat.xml" "$out/node.xml" -- tools/features/*.md
  step "R coverage"         Rscript tools/check-coverage-r.R . 90
  step "JS coverage"        npm run --silent coverage
  step "R CMD check"        Rscript tools/check-rcmdcheck.R .
fi
echo
echo "== size budget (report only)"
Rscript tools/check-budget.R . || true

echo
echo "== comments (report only)"
Rscript tools/check-comments.R .

echo
echo "== summary"
printf '%s\n' "${summary[@]}"
exit $failed
