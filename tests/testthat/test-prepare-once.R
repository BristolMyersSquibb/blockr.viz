# The prepare script runs once per input for the block's view and its
# expression (R/prepare-once.R).

once_fixture <- function() {
  s <- shiny::MockShinySession$new()
  runs <- new.env()
  runs$n <- 0L
  body <- quote(transform(data, y = x * 2))
  run_on <- function(data) {
    function() {
      runs$n <- runs$n + 1L
      eval(body, list(data = data))
    }
  }
  list(session = s, runs = runs, body = body, run_on = run_on)
}

core_eval <- function(session, expr, data) {
  env <- blockr.core::eval_env(list(data = data))
  shiny::withReactiveDomain(session, {
    eval(prep_once_splice(expr, env), env)
  })
}

test_that("the expression takes the frame the view made", {
  f <- once_fixture()
  d <- data.frame(x = 1:3)
  once <- prep_once(f$session$makeScope("tl"))
  view <- once$run(f$body, d, f$run_on(d))
  expect_equal(f$runs$n, 1L)
  # The table's expression: its click filter around the script, which
  # reads `identity(data)` without a click.
  ex <- bquote(dplyr::filter(.(do.call(substitute, list(
    f$body, list(data = quote(identity(data)))
  ))), y > 2))
  env <- blockr.core::eval_env(list(data = d))
  swapped <- shiny::withReactiveDomain(f$session, prep_once_splice(ex, env))
  expect_identical(swapped[[2L]], as.name(".blockr_prepared_1"))
  expect_identical(get(".blockr_prepared_1", envir = env), view)
  expect_equal(eval(swapped, env)$y, c(4, 6))
})

test_that("the view takes the frame the expression made", {
  f <- once_fixture()
  d <- data.frame(x = 1:3)
  once <- prep_once(f$session$makeScope("tl"))
  once$expect(f$body)
  out <- core_eval(f$session, f$body, d)
  expect_equal(out$y, c(2, 4, 6))
  view <- once$run(f$body, d, f$run_on(d))
  expect_equal(f$runs$n, 0L)
  expect_identical(view, out)
})

test_that("another input or other code runs the script again", {
  f <- once_fixture()
  d <- data.frame(x = 1:3)
  once <- prep_once(f$session$makeScope("tl"))
  once$run(f$body, d, f$run_on(d))
  d2 <- data.frame(x = 4:5)
  expect_equal(core_eval(f$session, f$body, d2)$y, c(8, 10))
  # The expression's run on d2 is the view's frame for d2 now.
  once$run(f$body, d2, f$run_on(d2))
  expect_equal(f$runs$n, 1L)
  other <- quote(transform(data, y = x * 3))
  once$expect(other)
  expect_equal(core_eval(f$session, other, d2)$y, c(12, 15))
  expect_equal(once$run(other, d2, f$run_on(d2))$y, c(12, 15))
  expect_equal(f$runs$n, 1L)
})

test_that("two blocks with the same script keep their own input apart", {
  f <- once_fixture()
  d1 <- data.frame(x = 1:3)
  d2 <- data.frame(x = 10:11)
  a <- prep_once(f$session$makeScope("a"))
  b <- prep_once(f$session$makeScope("b"))
  a$run(f$body, d1, f$run_on(d1))
  b$run(f$body, d2, f$run_on(d2))
  expect_equal(core_eval(f$session, f$body, d1)$y, c(2, 4, 6))
  expect_equal(core_eval(f$session, f$body, d2)$y, c(20, 22))
  expect_equal(f$runs$n, 2L)
})

test_that("outside a session the script runs as written", {
  expect_null(prep_once(NULL))
  ex <- quote(transform(data, y = x * 2))
  env <- blockr.core::eval_env(list(data = data.frame(x = 1)))
  expect_identical(prep_once_splice(ex, env), ex)
})

test_that("a table block's evaluation goes through the store", {
  f <- once_fixture()
  d <- data.frame(x = 1:3)
  once <- prep_once(f$session$makeScope("tl"))
  view <- once$run(f$body, d, f$run_on(d))
  env <- blockr.core::eval_env(list(data = d))
  out <- shiny::withReactiveDomain(f$session, {
    blockr.core::block_eval(new_table_block(), f$body, env)
  })
  expect_identical(out, view)
  expect_equal(f$runs$n, 1L)
})
