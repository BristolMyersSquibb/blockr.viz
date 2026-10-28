# ---------------------------------------------------------------------------
# Copied from blockr.extra (R/code-block-inputs.R).
#
# The chart block's `script` argument uses the code block's declaration
# convention: a top-level assignment whose right-hand side is a plain value is
# a control, everything else is code. The copy has the parsing half only. The
# chart's script sits in a plain textarea in the gear, and the band's own JS
# renderer draws the controls.
# ---------------------------------------------------------------------------

#' The inputs layer of the chart block's prepare script
#'
#' The script is ordinary R. A top-level assignment whose right-hand side is a
#' *plain value* ([CB_VALUE_CALLS]) becomes a control on the card; every other
#' statement is code. A name starting with a dot is never a control.
#'
#' Both are decided by reading the line, without evaluating anything and
#' wherever it sits in the script. There is no fenced region or marker
#' comment.
#'
#' @name chart-prepare-parse
#' @keywords internal
NULL

#' Calls whose result counts as a plain value
#'
#' Every addition widens what silently becomes a control, so the list is short.
#' The levels of `factor()` are the choice list of a select.
#'
#' @keywords internal
CB_VALUE_CALLS <- c("c", "factor", "as.Date", "as.POSIXct")

#' Annotation keys accepted after `#|`
#' @noRd
CB_ANN_KEYS <- c("label", "min", "max", "step", "placeholder")

#' Call names accepted as an annotation wrapper (`#| number(min = 1)`)
#' @noRd
CB_ANN_CALLS <- c("number", "text", "select", "flag", "date", "input")

#' Is this statement an assignment to a bare name?
#'
#' The shape a declaration has, whether or not it is one.
#'
#' @param e A top-level expression from [parse()].
#' @noRd
cb_is_assign_stmt <- function(e) {
  if (!is.call(e) || length(e) != 3L) {
    return(FALSE)
  }
  op <- e[[1L]]
  if (!identical(op, quote(`<-`)) && !identical(op, quote(`=`))) {
    return(FALSE)
  }
  is.name(e[[2L]])
}

#' Is this a name the block keeps to itself?
#'
#' A leading dot means internal, as in `ls()`. It marks a line that would
#' otherwise be read as a control as scaffolding for the controls around it.
#'
#' @param name A variable name.
#' @noRd
cb_is_private_name <- function(name) {
  !is.na(name) && startsWith(name, ".")
}

#' Is this statement an input declaration?
#'
#' @param e A top-level expression from [parse()].
#' @return `TRUE` for `name <- <plain value>`, the name not starting with a dot.
#' @noRd
cb_is_input_stmt <- function(e) {
  cb_is_assign_stmt(e) && !cb_is_private_name(as.character(e[[2L]])) &&
    cb_is_value_rhs(e[[3L]])
}

#' Is this right-hand side a plain value?
#'
#' Purely syntactic, and conservative: `6L * 2` is arithmetic, so it stays
#' code. A line never becomes a widget by surprise.
#'
#' @param rhs The right-hand side expression.
#' @noRd
cb_is_value_rhs <- function(rhs) {
  # A bare literal: 6, "total", TRUE, 2.5.
  if (is.atomic(rhs) && length(rhs) == 1L) {
    return(TRUE)
  }
  if (!is.call(rhs)) {
    return(FALSE)
  }
  head <- rhs[[1L]]
  # `-1` / `+1` parse as unary calls, not literals.
  if (is.name(head) && as.character(head) %in% c("-", "+") &&
      length(rhs) == 2L && is.numeric(rhs[[2L]])) {
    return(TRUE)
  }
  nm <- cb_call_name(head)
  !is.null(nm) && nm %in% CB_VALUE_CALLS
}

#' The bare name of a call head, following a `::` prefix.
#' @noRd
cb_call_name <- function(head) {
  if (is.name(head)) {
    return(as.character(head))
  }
  if (is.call(head) && identical(head[[1L]], quote(`::`))) {
    return(as.character(head[[3L]]))
  }
  NULL
}

#' Split a script into input declarations and body statements
#'
#' @param text The script.
#' @return A list with `ok`, `error` (parse message or `NULL`), `stmts` (one
#'   record per top-level statement: `expr`, `line`, `input`, `name`) and
#'   `lines` (the raw source lines, for annotation lookup).
#' @noRd
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

#' Read the `#|` annotation off a source line
#'
#' The widget kind follows from the value, so the annotation only carries what
#' a value cannot say (a numeric range, a label, a placeholder). Both
#' `#| number(min = 1, max = 50)` and the bare `#| min = 1, max = 50` work.
#'
#' @param line_text One line of source.
#' @return A named list of evaluated annotation arguments (possibly empty).
#' @noRd
cb_annotation <- function(line_text) {
  if (is.na(line_text) || !grepl("#|", line_text, fixed = TRUE)) {
    return(list())
  }
  m <- regmatches(line_text, regexpr("#\\|.*$", line_text))
  if (!length(m)) {
    return(list())
  }
  body <- trimws(sub("^#\\|", "", m))
  if (!nzchar(body)) {
    return(list())
  }

  e <- tryCatch(parse(text = body)[[1L]], error = function(e) NULL)
  args <- NULL
  if (!is.null(e) && is.call(e) && is.name(e[[1L]]) &&
      as.character(e[[1L]]) %in% CB_ANN_CALLS) {
    args <- as.list(e)[-1L]
  } else {
    e2 <- tryCatch(parse(text = paste0("list(", body, ")"))[[1L]],
                   error = function(e) NULL)
    if (!is.null(e2)) {
      args <- as.list(e2)[-1L]
    }
  }
  if (is.null(args) || !length(args) || is.null(names(args))) {
    return(list())
  }
  args <- args[names(args) %in% CB_ANN_KEYS]
  if (!length(args)) {
    return(list())
  }
  lapply(args, function(a) tryCatch(eval(a, baseenv()), error = function(e) NULL))
}

#' Derive the widget from a declaration's value
#'
#' Kind, choice pool and single or multiple come from the declaration, never
#' from the live value, so a multi-select narrowed to one pick stays a
#' multi-select.
#'
#' Exactly one pick declares a single select; anything else declares a
#' multi-select. Zero counts as multi, so a script can offer choices with
#' nothing chosen yet (`factor(character(0), levels = lv)`).
#'
#' @param v The evaluated right-hand side.
#' @return A list with `kind`, and for selects `choices` and `multiple`; `NULL`
#'   when the value maps to no widget.
#' @noRd
cb_widget_for <- function(v) {
  if (is.factor(v)) {
    return(list(kind = "select", choices = levels(v),
                multiple = length(v) != 1L))
  }
  if (inherits(v, "Date")) {
    return(list(kind = "date"))
  }
  if (is.logical(v) && length(v) == 1L && !is.na(v)) {
    return(list(kind = "flag"))
  }
  if (is.numeric(v) && length(v) == 1L) {
    return(list(kind = "number"))
  }
  if (is.character(v) && length(v) == 1L) {
    return(list(kind = "text"))
  }
  # A bare vector is its own pool.
  if ((is.character(v) || is.numeric(v)) && length(v) > 1L) {
    return(list(kind = "select", choices = as.character(v), multiple = TRUE))
  }
  NULL
}

#' Build the input specs for a script
#'
#' Declarations are evaluated in the block's evaluation environment, so they
#' reach what the body reaches. Preceding non-input assignments are bound
#' lazily, so a helper line like `lv <- unique(data$site)` is available to a
#' later `factor(x, lv)` without running the pipeline.
#'
#' A statement above the last declaration that is not an assignment cannot be
#' bound lazily and is run. Nothing below the last declaration is run.
#'
#' @param parsed The result of [cb_parse()].
#' @param data The upstream data frame (may be `NULL` before it arrives).
#' @return A list of spec records.
#' @noRd
cb_specs <- function(parsed, data = NULL) {
  if (!parsed$ok || !length(parsed$stmts)) {
    return(list())
  }

  env <- blockr.core::eval_env(list(data = data))
  is_input <- vapply(parsed$stmts, `[[`, logical(1L), "input")
  last_decl <- if (any(is_input)) max(which(is_input)) else 0L
  # A declaration the script uses as scaffolding is bound like a helper line
  # and offers no control (see cb_demoted()).
  demoted <- cb_demoted(parsed)
  specs <- list()

  for (i in seq_along(parsed$stmts)) {
    st <- parsed$stmts[[i]]
    if (!st$input || demoted[[i]]) {
      # Bound lazily: forced only if a later declaration reads it.
      if (cb_is_assign_stmt(st$expr)) {
        cb_delay(env, as.character(st$expr[[2L]]), st$expr[[3L]])
      } else if (i < last_decl) {
        # A failure here is not reported: a declaration below that depends on
        # it fails on its own account when the block runs.
        try(eval(st$expr, env), silent = TRUE)
      }
      next
    }

    fail <- function(msg) {
      list(name = st$name, line = st$line, kind = NA_character_, error = msg)
    }

    value <- tryCatch(eval(st$expr[[3L]], env), error = function(e) e)
    if (inherits(value, "error")) {
      specs[[length(specs) + 1L]] <- fail(conditionMessage(value))
      next
    }
    # Make it visible to later declarations too.
    assign(st$name, value, envir = env)

    w <- cb_widget_for(value)
    if (!is.null(w) && identical(w$kind, "select") && !length(w$choices)) {
      # An empty factor is usually a mistyped column name.
      specs[[length(specs) + 1L]] <- fail(
        "no choices - is the column name right?"
      )
      next
    }
    if (is.null(w)) {
      specs[[length(specs) + 1L]] <- fail(paste0(
        "no control for a value of class ", paste(class(value), collapse = "/")
      ))
      next
    }

    ann <- cb_annotation(
      if (is.na(st$line) || st$line > length(parsed$lines)) NA_character_
      else parsed$lines[[st$line]]
    )

    specs[[length(specs) + 1L]] <- c(
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

  specs
}

#' Bind a name to an expression that is only evaluated if something reads it
#'
#' A declaration may depend on a helper line above it (`lv <- unique(data$site)`
#' feeding `factor("Basel", lv)`), but the statements in between, usually the
#' block's pipeline, must not run to build that scope. An active binding
#' evaluates on first read and caches.
#'
#' @noRd
cb_delay <- function(env, name, expr) {
  force(expr)
  force(env)
  cached <- NULL
  forced <- FALSE
  # A later statement may re-assign a name already bound; script order wins,
  # and `makeActiveBinding()` refuses to shadow a regular binding. The names
  # are checked directly because `exists()` would force an active binding.
  #
  # `x <- f(x)` reads the old `x`, so the old binding is carried into a child
  # environment that the new one evaluates in. Otherwise the binding reads
  # itself and fails with "infinite recursion".
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
  # The binding accepts a write too, because assigning over an active binding
  # calls it with the new value.
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

#' Coerce a value coming back from a widget to the declared type
#'
#' Shiny inputs arrive as strings from selects and as lists from JS arrays, so
#' the declared type drives the coercion. Numeric `NA` ships as the string
#' `"NA"`.
#'
#' @param value The raw input value.
#' @param spec The spec record it belongs to.
#' @noRd
cb_coerce <- function(value, spec) {
  if (is.null(value)) {
    return(NULL)
  }
  if (is.list(value)) {
    value <- unlist(value, use.names = FALSE)
  }
  if (is.null(value) || !length(value)) {
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
      if (is.numeric(spec$default)) {
        num <- suppressWarnings(as.numeric(out))
        if (!any(is.na(num))) out <- num
      }
      out
    },
    value
  )
}

