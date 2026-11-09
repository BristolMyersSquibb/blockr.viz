# Test helpers for the browser-driven (shinytest2) interaction test.
#
# This container ships no bundled Chrome; it has system chromium whose kernel
# does not support the sandbox. Point chromote at it with --no-sandbox so a
# headless browser can actually start. These settings are process-global and
# harmless when a normal browser is available.
configure_chromote <- function() {
  if (!nzchar(Sys.getenv("CHROMOTE_CHROME"))) {
    for (cand in c("/usr/bin/chromium", "/usr/bin/chromium-browser",
                   "/usr/bin/google-chrome")) {
      if (file.exists(cand)) {
        Sys.setenv(CHROMOTE_CHROME = cand)
        break
      }
    }
  }
  if (requireNamespace("chromote", quietly = TRUE)) {
    try(
      chromote::set_chrome_args(
        c("--no-sandbox", "--disable-gpu", "--disable-dev-shm-usage")
      ),
      silent = TRUE
    )
  }
  invisible(NULL)
}

# TRUE only if a headless chromium can actually be launched here. Used to
# skip the shinytest2 test where no browser exists rather than fail.
chromote_works <- function() {
  if (!requireNamespace("chromote", quietly = TRUE)) {
    return(FALSE)
  }
  configure_chromote()
  # A busy runner can be slow to start Chrome the first time; the answer
  # holds for the whole run (run_browser_tests()), so try more than once.
  for (i in 1:3) {
    ok <- tryCatch({
      sess <- chromote::ChromoteSession$new()
      on.exit(try(sess$close(), silent = TRUE), add = TRUE)
      sess$Page$navigate("about:blank")
      TRUE
    }, error = function(e) {
      message("chromote_works(): ", conditionMessage(e))
      FALSE
    })
    if (isTRUE(ok)) return(TRUE)
  }
  FALSE
}

# Whether the browser tests run here. Never under R CMD check (Chrome leaves
# temp detritus, a check NOTE, and CRAN runs no browser) and never under covr;
# BLOCKR_VIZ_BROWSER_TESTS=false switches them off. Elsewhere they run when
# shinytest2, chromote and a working Chrome are there: devtools::test(), the
# CI `browser` job, and the exit-criteria job's R tests. Asked once per run,
# since starting Chrome to find out is slow.
run_browser_tests <- local({
  answer <- NULL
  function() {
    if (is.null(answer)) {
      answer <<- !nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) &&
        !identical(Sys.getenv("R_COVR"), "true") &&
        !identical(tolower(Sys.getenv("BLOCKR_VIZ_BROWSER_TESTS")), "false") &&
        requireNamespace("shinytest2", quietly = TRUE) &&
        chromote_works()
    }
    answer
  }
})

skip_without_browser <- function() {
  testthat::skip_if_not(run_browser_tests(),
                        "browser tests: not under R CMD check, needs Chrome")
}
