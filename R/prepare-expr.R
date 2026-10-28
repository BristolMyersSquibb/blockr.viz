# ---------------------------------------------------------------------------
# Copied from blockr.extra (R/code-block-expr.R), which blockr.viz does not
# import because that would add broom, mirai, DT and evaluate to every deploy.
# The names stay identical to blockr.extra's.
# ---------------------------------------------------------------------------

#' Compiling a prepare script into part of the chart block's expression
#'
#' The block does not wrap the user's code in a function. It substitutes the
#' current input values into the body as literals. So:
#'
#' * The expression contains no assignment, `blockr.code`'s `has_assignment()`
#'   is false and the exported script is emitted bare, with no `local({...})`
#'   wrapper.
#' * The exported code is the code an analyst would have written, with the
#'   control values written in: `dplyr::filter(block_1, Species == "setosa")`.
#'
#' @name chart-prepare-expr
#' @keywords internal
NULL


#' Marker left behind by a folded-away `if` with no `else`.
#' @noRd
CB_DROP <- quote(`__cb_drop__`)


#' Is this the empty symbol (a missing argument, as in `x[, 1]`)?
#' @noRd
cb_is_empty_sym <- function(x) {
  is.name(x) && !nzchar(as.character(x))
}


#' Turn a value into an AST node that deparses idiomatically
#'
#' Most values are inlined as themselves. Dates and datetimes would deparse to
#' `structure(20318, class = "Date")`, so they are re-emitted as the
#' constructor call.
#'
#' @param v A value.
#' @noRd
cb_literal <- function(v) {
  if (inherits(v, "Date")) {
    return(bquote(as.Date(.(format(v, "%Y-%m-%d")))))
  }
  if (inherits(v, "POSIXct")) {
    tz <- attr(v, "tzone")
    txt <- format(v, "%Y-%m-%d %H:%M:%S")
    if (is.null(tz) || !nzchar(tz)) {
      return(bquote(as.POSIXct(.(txt))))
    }
    return(bquote(as.POSIXct(.(txt), tz = .(tz))))
  }
  if (is.factor(v)) {
    return(as.character(v))
  }
  v
}


#' Substitute symbols throughout an expression
#'
#' Only value positions are touched: never a call head, the right side of `$`
#' or `@`, or an argument name, where a symbol is not a variable reference.
#'
#' A `function()` formal rebinds the name for that body, so the substitution
#' stops there. A `for` variable rebinds it in the same environment, so the
#' declaration is demoted to code instead (see [cb_demoted()]).
#'
#' @param e An expression.
#' @param subs A named list of replacement nodes.
#' @noRd
cb_subst <- function(e, subs) {
  if (is.name(e)) {
    nm <- as.character(e)
    if (nzchar(nm) && nm %in% names(subs)) {
      return(subs[[nm]])
    }
    return(e)
  }
  if (!is.call(e)) {
    return(e)
  }
  # `function(x) ...` opens a scope: a formal shadows the control of the same
  # name in the body and in the defaults, which R evaluates in the function's
  # frame.
  if (identical(e[[1L]], quote(`function`)) && length(e) >= 3L) {
    inner <- subs[setdiff(names(subs), names(e[[2L]]))]
    fmls <- e[[2L]]
    for (i in seq_along(fmls)) {
      if (!cb_is_empty_sym(fmls[[i]])) {
        fmls[[i]] <- cb_subst(fmls[[i]], inner)
      }
    }
    e[[2L]] <- fmls
    e[[3L]] <- cb_subst(e[[3L]], inner)
    # Drop the srcref, which would deparse the source before substitution.
    if (length(e) >= 4L) {
      e[[4L]] <- NULL
    }
    return(e)
  }
  # `x <- v`: the left side is the name being bound, not a reference. An
  # indexed target (`x[i] <- v`) is a call whose index is a reference, so only
  # a bare name is skipped.
  if (cb_binds_name(e)) {
    e[[3L]] <- cb_subst(e[[3L]], subs)
    return(e)
  }
  # `x$name` / `x@name`: the right side is a literal name.
  if (length(e) == 3L &&
      (identical(e[[1L]], quote(`$`)) || identical(e[[1L]], quote(`@`)))) {
    e[[2L]] <- cb_subst(e[[2L]], subs)
    return(e)
  }
  cb_map_args(e, function(a) cb_subst(a, subs))
}


#' Apply `f` to each argument of a call
#'
#' A NULL result leaves the argument as it was: `e[[i]] <- NULL` would delete
#' the element and shorten the call. Intentional removal uses CB_DROP.
#' @noRd
cb_map_args <- function(e, f) {
  if (length(e) > 1L) {
    for (i in 2L:length(e)) {
      if (cb_is_empty_sym(e[[i]])) {
        next
      }
      new <- f(e[[i]])
      if (is.null(new)) {
        next
      }
      e[[i]] <- new
    }
  }
  e
}


#' Is this call an assignment to a bare name?
#' @noRd
cb_binds_name <- function(e) {
  length(e) == 3L && is.name(e[[2L]]) &&
    (identical(e[[1L]], quote(`<-`)) || identical(e[[1L]], quote(`=`)) ||
       identical(e[[1L]], quote(`<<-`)))
}


#' Fold `if` on a condition that substitution turned into a literal
#'
#' Without this, every flag would put `if (TRUE) ...` in the exported script.
#' The condition is known at export time, so the untaken branch is dropped.
#' This is the one place the emitted code is not a literal substitution of the
#' source.
#'
#' @param e An expression.
#' @noRd
cb_fold <- function(e) {
  if (!is.call(e)) {
    return(e)
  }
  e <- cb_map_args(e, cb_fold)

  if (identical(e[[1L]], quote(`if`))) {
    cond <- e[[2L]]
    if (is.logical(cond) && length(cond) == 1L && !is.na(cond)) {
      if (cond) {
        return(e[[3L]])
      }
      if (length(e) >= 4L) {
        return(e[[4L]])
      }
      return(CB_DROP)
    }
  }

  cb_prune(e)
}


#' Remove folded-away operands from the shapes they can appear in
#'
#' `p + if (FALSE) geom_smooth()` should emit `p`, not `p + NULL`; a dropped
#' statement inside `{}` should vanish rather than leave a blank.
#'
#' @param e A call whose arguments have already been folded.
#' @noRd
cb_prune <- function(e) {
  is_drop <- function(x) identical(x, CB_DROP)

  if (identical(e[[1L]], quote(`{`)) && length(e) > 1L) {
    keep <- !vapply(as.list(e)[-1L], is_drop, logical(1L))
    return(as.call(c(list(quote(`{`)), as.list(e)[-1L][keep])))
  }
  # Binary operators where an absent operand means "leave the other one".
  if (length(e) == 3L && is.name(e[[1L]]) &&
      as.character(e[[1L]]) %in% c("+", "|>", "%>%")) {
    if (is_drop(e[[3L]])) {
      return(e[[2L]])
    }
    if (is_drop(e[[2L]])) {
      return(e[[3L]])
    }
  }
  # Anywhere else a dropped branch is NULL, set with `e[i] <- list(NULL)`
  # because `e[[i]] <- NULL` deletes the element. On `x <- if (FALSE) 1` that
  # would leave a one-argument `<-`, which fails when the block evaluates it.
  if (length(e) > 1L) {
    for (i in 2L:length(e)) {
      if (!cb_is_empty_sym(e[[i]]) && is_drop(e[[i]])) {
        e[i] <- list(NULL)
      }
    }
  }
  e
}


#' Names assigned anywhere in a set of statements
#'
#' An input whose name the body also assigns is not an input: the script means
#' what plain R means, so the declaration stays code and no control is offered.
#'
#' @param exprs A list of expressions.
#' @noRd
cb_assigned_names <- function(exprs) {
  found <- character()
  walk <- function(e) {
    if (!is.call(e)) {
      return(invisible(NULL))
    }
    if (cb_binds_name(e)) {
      found <<- c(found, as.character(e[[2L]]))
    }
    # A loop variable is an assignment in the same environment, so a
    # declaration the body loops over is demoted like any re-assignment.
    if (identical(e[[1L]], quote(`for`)) && length(e) == 4L &&
          is.name(e[[2L]])) {
      found <<- c(found, as.character(e[[2L]]))
    }
    if (length(e) > 1L) {
      for (i in 2L:length(e)) {
        if (!cb_is_empty_sym(e[[i]])) {
          walk(e[[i]])
        }
      }
    }
    invisible(NULL)
  }
  for (e in exprs) walk(e)
  unique(found)
}


#' Which declarations are not controls after all?
#'
#' A declaration is a control unless the script assigns the name again: a body
#' statement writes it, or a second declaration further down replaces it (only
#' the last one is the control). This is decided by reading the script, without
#' evaluating anything.
#'
#' A helper line starts its name with a dot, as in `.lv <- unique(data$site)`
#' feeding `site <- factor("Basel", .lv)`.
#'
#' @param parsed The result of [cb_parse()].
#' @return A logical vector over `parsed$stmts`.
#' @noRd
cb_demoted <- function(parsed) {
  n <- length(parsed$stmts)
  if (!parsed$ok || !n) {
    return(logical(n))
  }
  is_input <- vapply(parsed$stmts, `[[`, logical(1L), "input")
  nms <- vapply(parsed$stmts, `[[`, character(1L), "name")

  body_assigned <- cb_assigned_names(lapply(parsed$stmts[!is_input], `[[`, "expr"))

  reason <- vapply(seq_len(n), function(i) {
    if (!is_input[[i]]) {
      return("")
    }
    nm <- nms[[i]]
    if (nm %in% body_assigned) {
      return("assigned")
    }
    later <- is_input & seq_len(n) > i
    if (any(later & !is.na(nms) & nms == nm)) {
      return("redeclared")
    }
    ""
  }, character(1L))

  out <- nzchar(reason)
  attr(out, "reason") <- reason
  attr(out, "name") <- nms
  out
}


#' Compile the script into the block's expression
#'
#' @param parsed The result of [cb_parse()].
#' @param specs Input specs from [cb_specs()].
#' @param values Named list of current input values.
#' @param data_name Name of the block's data slot.
#' @param slot Emit the `.(data)` bquote marker for the data name (`TRUE`, for a
#'   block expression), or leave the bare symbol (`FALSE`, for local
#'   evaluation). The chart block needs both forms: one to run the script on
#'   the frame it draws, one for its expression.
#' @return A quoted expression carrying the `.(data)` slot, or `NULL` when the
#'   script has no body.
#' @noRd
cb_expr <- function(parsed, specs, values = list(), data_name = "data",
                    slot = TRUE) {
  if (!parsed$ok) {
    return(NULL)
  }
  demoted <- cb_demoted(parsed)
  usable <- vapply(
    specs,
    function(s) is.null(s$error) && !is.na(s$kind),
    logical(1L)
  )
  specs <- specs[usable]

  # A declaration stays in the body as code unless a usable control replaced
  # it. That covers demoted lines and declarations whose value could not be
  # read, which would otherwise leave a reference to a missing symbol.
  live <- vapply(specs, `[[`, character(1L), "name")
  keep <- vapply(seq_along(parsed$stmts), function(i) {
    st <- parsed$stmts[[i]]
    !st$input || demoted[[i]] || !st$name %in% live
  }, logical(1L))
  body <- lapply(parsed$stmts[keep], `[[`, "expr")
  if (!length(body)) {
    return(NULL)
  }

  subs <- list()
  subs[[data_name]] <- if (slot) call(".", as.name(data_name)) else as.name(data_name)
  for (s in specs) {
    # A restored board returns what survived JSON, so the declaration drives
    # the coercion before the value is inlined.
    v <- cb_coerce(values[[s$name]], s)
    if (is.null(v) || !length(v)) {
      v <- s$default
    }
    subs[[s$name]] <- cb_literal(v)
  }

  body <- lapply(body, function(e) cb_fold(cb_subst(e, subs)))
  body <- Filter(function(e) !identical(e, CB_DROP), body)
  if (!length(body)) {
    return(NULL)
  }
  if (length(body) == 1L) {
    return(body[[1L]])
  }
  as.call(c(list(quote(`{`)), body))
}
