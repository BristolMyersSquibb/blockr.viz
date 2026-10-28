# Parsing half of the code block's declaration convention (blockr.extra,
# R/code-block-inputs.R). The band's JS renderer draws the controls.

#' The inputs layer of the chart block's prepare script
#'
#' The script is ordinary R. A top-level assignment whose right-hand side is a
#' *plain value* ([CB_VALUE_CALLS]) becomes a control on the card; every other
#' statement is code. A name starting with a dot is never a control. Both are
#' decided by reading the line, without evaluating anything.
#'
#' @name chart-prepare-parse
#' @keywords internal
NULL

#' Calls whose result counts as a plain value
#'
#' The levels of `factor()` are the choice list of a select.
#'
#' @keywords internal
CB_VALUE_CALLS <- c("c", "factor", "as.Date", "as.POSIXct")

# Annotation keys accepted after `#|`.
CB_ANN_KEYS <- c("label", "min", "max", "step", "placeholder")

# Call names accepted as an annotation wrapper (`#| number(min = 1)`).
CB_ANN_CALLS <- c("number", "text", "select", "flag", "date", "input")

# Is this statement an assignment to a bare name?
cb_is_assign_stmt <- function(e) {
  is.call(e) && length(e) == 3L &&
    (identical(e[[1L]], quote(`<-`)) || identical(e[[1L]], quote(`=`))) &&
    is.name(e[[2L]])
}

# Is this statement an input declaration? A name with a leading dot is
# scaffolding for the controls around it, never a control.
cb_is_input_stmt <- function(e) {
  cb_is_assign_stmt(e) && !startsWith(as.character(e[[2L]]), ".") &&
    cb_is_value_rhs(e[[3L]])
}

# Is this right-hand side a plain value? Purely syntactic and conservative:
# `6L * 2` is arithmetic, so it stays code.
cb_is_value_rhs <- function(rhs) {
  if (is.atomic(rhs) && length(rhs) == 1L) return(TRUE)
  if (!is.call(rhs)) return(FALSE)
  head <- rhs[[1L]]
  # `-1` / `+1` parse as unary calls, not literals.
  if (is.name(head) && as.character(head) %in% c("-", "+") &&
      length(rhs) == 2L && is.numeric(rhs[[2L]])) {
    return(TRUE)
  }
  nm <- cb_call_name(head)
  !is.null(nm) && nm %in% CB_VALUE_CALLS
}

# The bare name of a call head, following a `::` prefix.
cb_call_name <- function(head) {
  if (is.name(head)) return(as.character(head))
  if (is.call(head) && identical(head[[1L]], quote(`::`))) {
    return(as.character(head[[3L]]))
  }
  NULL
}

# Split a script into input declarations and body statements. Returns `ok`,
# `error` (parse message or NULL), `stmts` (per statement: `expr`, `line`,
# `input`, `name`) and `lines` (the source lines, for annotation lookup).
cb_parse <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(list(ok = TRUE, error = NULL, stmts = list(), lines = character()))
  }
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  exprs <- tryCatch(
    parse(text = text, keep.source = TRUE),
    error = function(e) e
  )
  if (inherits(exprs, "error")) {
    return(list(ok = FALSE, error = conditionMessage(exprs), stmts = list(),
                lines = lines))
  }

  refs <- utils::getSrcref(exprs)
  stmts <- lapply(seq_along(exprs), function(i) {
    e <- exprs[[i]]
    is_input <- cb_is_input_stmt(e)
    list(
      expr = e,
      line = if (is.null(refs) || is.null(refs[[i]])) NA_integer_ else
        as.integer(refs[[i]])[1L],
      input = is_input,
      name = if (is_input) as.character(e[[2L]]) else NA_character_
    )
  })

  list(ok = TRUE, error = NULL, stmts = stmts, lines = lines)
}

# Read the `#|` annotation off a source line: the arguments a value cannot say
# (range, label, placeholder). `#| number(min = 1)` and `#| min = 1` both work.
cb_annotation <- function(line_text) {
  if (is.na(line_text)) return(list())
  m <- regmatches(line_text, regexpr("#\\|.*$", line_text))
  body <- trimws(sub("^#\\|", "", m))
  if (!length(m) || !nzchar(body)) return(list())

  e <- tryCatch(parse(text = body)[[1L]], error = function(e) NULL)
  if (is.call(e) && is.name(e[[1L]]) &&
      as.character(e[[1L]]) %in% CB_ANN_CALLS) {
    args <- as.list(e)[-1L]
  } else {
    e <- tryCatch(parse(text = paste0("list(", body, ")"))[[1L]],
                  error = function(e) NULL)
    args <- if (!is.null(e)) as.list(e)[-1L]
  }
  args <- args[names(args) %in% CB_ANN_KEYS]
  if (!length(args)) return(list())
  lapply(args, function(a) tryCatch(eval(a, baseenv()), error = function(e) NULL))
}

# The widget a declaration's value maps to: `kind`, and for selects `choices`
# and `multiple`. The declaration decides, never the live value. One pick is a
# single select; zero or several is a multi-select. NULL when no widget fits.
cb_widget_for <- function(v) {
  one <- length(v) == 1L
  if (is.factor(v)) {
    list(kind = "select", choices = levels(v), multiple = !one)
  } else if (inherits(v, "Date")) {
    list(kind = "date")
  } else if (is.logical(v) && one && !is.na(v)) {
    list(kind = "flag")
  } else if (is.numeric(v) && one) {
    list(kind = "number")
  } else if (is.character(v) && one) {
    list(kind = "text")
  } else if ((is.character(v) || is.numeric(v)) && length(v) > 1L) {
    # A bare vector is its own pool.
    list(kind = "select", choices = as.character(v), multiple = TRUE)
  }
}

# The spec for one input statement. Binds the value in `env` so later
# declarations can read it.
cb_input_spec <- function(st, env, lines) {
  fail <- function(msg) {
    list(name = st$name, line = st$line, kind = NA_character_, error = msg)
  }
  value <- tryCatch(eval(st$expr[[3L]], env), error = function(e) e)
  if (inherits(value, "error")) return(fail(conditionMessage(value)))
  assign(st$name, value, envir = env)

  w <- cb_widget_for(value)
  if (is.null(w)) {
    return(fail(paste0(
      "no control for a value of class ", paste(class(value), collapse = "/")
    )))
  }
  if (identical(w$kind, "select") && !length(w$choices)) {
    # An empty factor is usually a mistyped column name.
    return(fail("no choices - is the column name right?"))
  }

  ann <- cb_annotation(lines[st$line])
  c(
    list(
      name = st$name,
      line = st$line,
      kind = w$kind,
      choices = w$choices,
      multiple = isTRUE(w$multiple),
      default = if (is.factor(value)) as.character(value) else value,
      error = NULL
    ),
    ann[setdiff(names(ann), c("name", "line", "kind", "choices", "multiple",
                              "default", "error"))]
  )
}

# Build the input specs for a script. Declarations are evaluated in the block's
# evaluation environment. Preceding non-input assignments are bound lazily, so
# `lv <- unique(data$site)` is available to a later `factor(x, lv)` without
# running the pipeline. A non-assignment statement above the last declaration
# is run; nothing below it is.
cb_specs <- function(parsed, data = NULL) {
  if (!parsed$ok || !length(parsed$stmts)) return(list())

  env <- blockr.core::eval_env(list(data = data))
  is_input <- vapply(parsed$stmts, `[[`, logical(1L), "input")
  last_decl <- if (any(is_input)) max(which(is_input)) else 0L
  demoted <- cb_demoted(parsed)
  specs <- list()

  for (i in seq_along(parsed$stmts)) {
    st <- parsed$stmts[[i]]
    if (!st$input || demoted[[i]]) {
      if (cb_is_assign_stmt(st$expr)) {
        cb_delay(env, as.character(st$expr[[2L]]), st$expr[[3L]])
      } else if (i < last_decl) {
        # A failure is not reported: a declaration below that depends on this
        # statement fails on its own account when the block runs.
        try(eval(st$expr, env), silent = TRUE)
      }
      next
    }
    specs[[length(specs) + 1L]] <- cb_input_spec(st, env, parsed$lines)
  }

  specs
}

# Bind a name to an expression that is only evaluated when something reads it
# (an active binding that caches), so the pipeline between a helper line and
# the declaration that uses it does not run.
cb_delay <- function(env, name, expr) {
  force(expr)
  force(env)
  cached <- NULL
  forced <- FALSE
  # `makeActiveBinding()` refuses to shadow a regular binding. `x <- f(x)`
  # reads the old `x`, so the old binding moves into a child environment that
  # the new one evaluates in. `exists()` would force the binding, so the names
  # are listed.
  where <- env
  if (name %in% ls(env, all.names = TRUE)) {
    where <- new.env(parent = env)
    if (bindingIsActive(name, env)) {
      makeActiveBinding(name, activeBindingFunction(name, env), where)
    } else {
      assign(name, get(name, envir = env, inherits = FALSE), envir = where)
    }
    rm(list = name, envir = env)
  }
  # Assigning over an active binding calls it with the new value.
  makeActiveBinding(
    name,
    function(v) {
      if (!missing(v)) {
        cached <<- v
        forced <<- TRUE
        return(invisible(v))
      }
      if (!forced) {
        cached <<- eval(expr, where)
        forced <<- TRUE
      }
      cached
    },
    env
  )
}

# Coerce a value coming back from a widget to the declared type. Selects send
# strings and JS arrays arrive as lists; numeric `NA` ships as `"NA"`.
cb_coerce <- function(value, spec) {
  if (is.null(value)) return(NULL)
  value <- if (is.list(value)) unlist(value, use.names = FALSE) else value
  if (!length(value)) {
    return(if (identical(spec$kind, "select") && isTRUE(spec$multiple))
      character() else NULL)
  }
  switch(
    spec$kind,
    number = {
      v <- suppressWarnings(as.numeric(value))
      if (all(is.na(v))) NULL else v
    },
    flag = as.logical(value),
    date = as.Date(value),
    text = as.character(value),
    select = {
      out <- as.character(value)
      # A numeric-valued select round-trips through the DOM as text.
      num <- suppressWarnings(as.numeric(out))
      if (is.numeric(spec$default) && !anyNA(num)) num else out
    },
    value
  )
}
