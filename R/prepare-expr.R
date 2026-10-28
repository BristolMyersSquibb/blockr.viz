# Compiling half of the code block's convention (blockr.extra,
# R/code-block-expr.R), copied because importing blockr.extra would add broom,
# mirai, DT and evaluate to every deploy. The names match blockr.extra's.

#' Compiling a prepare script into part of the chart block's expression
#'
#' The block substitutes the current input values into the script body as
#' literals instead of wrapping the code in a function. The expression contains
#' no assignment, so `blockr.code` emits the exported script bare, with the
#' control values written in: `dplyr::filter(block_1, Species == "setosa")`.
#'
#' @name chart-prepare-expr
#' @keywords internal
NULL

# Marker for a folded-away `if` with no `else`.
CB_DROP <- quote(`__cb_drop__`)

# Is this the empty symbol (a missing argument, as in `x[, 1]`)?
cb_is_empty_sym <- function(x) {
  is.name(x) && !nzchar(as.character(x))
}

# A value as an AST node that deparses idiomatically. Dates and datetimes
# become their constructor call instead of `structure(20318, class = "Date")`.
cb_literal <- function(v) {
  if (inherits(v, "Date")) return(bquote(as.Date(.(format(v, "%Y-%m-%d")))))
  if (inherits(v, "POSIXct")) {
    tz <- attr(v, "tzone")
    txt <- format(v, "%Y-%m-%d %H:%M:%S")
    return(if (is.null(tz) || !nzchar(tz)) bquote(as.POSIXct(.(txt)))
           else bquote(as.POSIXct(.(txt), tz = .(tz))))
  }
  if (is.factor(v)) return(as.character(v))
  v
}

# Substitute symbols throughout an expression, in value positions only: never
# a call head, the right side of `$` or `@`, or an argument name. A
# `function()` formal rebinds the name for that body, so substitution stops
# there. A `for` variable rebinds it in the same environment, so that
# declaration is demoted to code instead (see cb_demoted()).
cb_subst <- function(e, subs) {
  if (is.name(e)) {
    nm <- as.character(e)
    if (nzchar(nm) && nm %in% names(subs)) return(subs[[nm]])
    return(e)
  }
  if (!is.call(e)) return(e)
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
  # `x <- v`: the bound name is skipped. An indexed target (`x[i] <- v`) has
  # an index that is a reference, so only a bare name counts.
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

# Apply `f` to each argument of a call. A NULL result keeps the argument,
# because `e[[i]] <- NULL` would shorten the call. Removal uses CB_DROP.
cb_map_args <- function(e, f) {
  for (i in seq_along(e)[-1L]) {
    if (cb_is_empty_sym(e[[i]])) next
    new <- f(e[[i]])
    if (!is.null(new)) {
      e[[i]] <- new
    }
  }
  e
}

# Is this call an assignment to a bare name?
cb_binds_name <- function(e) {
  length(e) == 3L && is.name(e[[2L]]) &&
    (identical(e[[1L]], quote(`<-`)) || identical(e[[1L]], quote(`=`)) ||
       identical(e[[1L]], quote(`<<-`)))
}

# Fold `if` on a condition that substitution turned into a literal, dropping
# the untaken branch, so a flag does not put `if (TRUE) ...` in the exported
# script.
cb_fold <- function(e) {
  if (!is.call(e)) return(e)
  e <- cb_map_args(e, cb_fold)

  if (identical(e[[1L]], quote(`if`))) {
    cond <- e[[2L]]
    if (is.logical(cond) && length(cond) == 1L && !is.na(cond)) {
      if (cond) return(e[[3L]])
      return(if (length(e) >= 4L) e[[4L]] else CB_DROP)
    }
  }

  cb_prune(e)
}

# Remove folded-away operands: `p + if (FALSE) geom_smooth()` emits `p`, and a
# dropped statement inside `{}` vanishes.
cb_prune <- function(e) {
  is_drop <- function(x) identical(x, CB_DROP)

  if (identical(e[[1L]], quote(`{`)) && length(e) > 1L) {
    keep <- !vapply(as.list(e)[-1L], is_drop, logical(1L))
    return(as.call(c(list(quote(`{`)), as.list(e)[-1L][keep])))
  }
  # Binary operators where an absent operand means "leave the other one".
  if (length(e) == 3L && is.name(e[[1L]]) &&
      as.character(e[[1L]]) %in% c("+", "|>", "%>%")) {
    if (is_drop(e[[3L]])) return(e[[2L]])
    if (is_drop(e[[2L]])) return(e[[3L]])
  }
  # Anywhere else a dropped branch is NULL. `e[i] <- list(NULL)` keeps the
  # argument: `e[[i]] <- NULL` on `x <- if (FALSE) 1` would leave a
  # one-argument `<-`.
  for (i in seq_along(e)[-1L]) {
    if (!cb_is_empty_sym(e[[i]]) && is_drop(e[[i]])) {
      e[i] <- list(NULL)
    }
  }
  e
}

# Names assigned anywhere in a set of statements. A loop variable counts, as
# an assignment in the same environment.
cb_assigned_names <- function(exprs) {
  found <- character()
  walk <- function(e) {
    if (!is.call(e)) return(invisible(NULL))
    if (cb_binds_name(e) ||
        (identical(e[[1L]], quote(`for`)) && length(e) == 4L &&
           is.name(e[[2L]]))) {
      found <<- c(found, as.character(e[[2L]]))
    }
    for (i in seq_along(e)[-1L]) {
      if (!cb_is_empty_sym(e[[i]])) {
        walk(e[[i]])
      }
    }
    invisible(NULL)
  }
  for (e in exprs) walk(e)
  unique(found)
}

# Which declarations are not controls after all? A declaration is demoted to
# code when a body statement assigns the same name, or a later declaration
# replaces it (only the last one is the control). Decided by reading the
# script. Returns a logical vector over `parsed$stmts`.
cb_demoted <- function(parsed) {
  n <- length(parsed$stmts)
  if (!parsed$ok || !n) return(logical(n))
  is_input <- vapply(parsed$stmts, `[[`, logical(1L), "input")
  nms <- vapply(parsed$stmts, `[[`, character(1L), "name")
  body_assigned <- cb_assigned_names(
    lapply(parsed$stmts[!is_input], `[[`, "expr")
  )

  vapply(seq_len(n), function(i) {
    is_input[[i]] &&
      (nms[[i]] %in% body_assigned ||
         nms[[i]] %in% nms[is_input & seq_len(n) > i])
  }, logical(1L))
}

# Compile the script into the block's expression. `values` are the current
# input values. With `slot = TRUE` the data name becomes the `.(data)` bquote
# marker (a block expression); with `FALSE` it stays a bare symbol (local
# evaluation). Returns NULL when the script has no body.
cb_expr <- function(parsed, specs, values = list(), data_name = "data",
                    slot = TRUE) {
  if (!parsed$ok) return(NULL)
  demoted <- cb_demoted(parsed)
  specs <- Filter(dd_spec_ok, specs)

  # A declaration stays in the body as code unless a usable control replaces it.
  live <- vapply(specs, `[[`, character(1L), "name")
  keep <- vapply(seq_along(parsed$stmts), function(i) {
    st <- parsed$stmts[[i]]
    !st$input || demoted[[i]] || !st$name %in% live
  }, logical(1L))
  body <- lapply(parsed$stmts[keep], `[[`, "expr")
  if (!length(body)) return(NULL)

  subs <- list()
  subs[[data_name]] <- if (slot) call(".", as.name(data_name)) else as.name(data_name)
  for (s in specs) {
    # A restored board returns what survived JSON, so the declaration coerces.
    v <- cb_coerce(values[[s$name]], s)
    if (is.null(v) || !length(v)) {
      v <- s$default
    }
    subs[[s$name]] <- cb_literal(v)
  }

  body <- lapply(body, function(e) cb_fold(cb_subst(e, subs)))
  body <- Filter(function(e) !identical(e, CB_DROP), body)
  if (!length(body)) return(NULL)
  if (length(body) == 1L) return(body[[1L]])
  as.call(c(list(quote(`{`)), body))
}
