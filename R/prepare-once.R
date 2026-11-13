# The prepare script runs for two readers on the same input: the block's
# view, which draws the prepared frame (viz_script_sources()), and the
# block's expression, which blockr.core evaluates for the block's result
# (block_eval()). The script sits in that expression as the code the view
# runs: dd_splice_slot() puts it in with a `.(data)` slot, which blockr.core
# quotes to `data`, the symbol the view's code reads. So whichever reader
# comes second takes the first one's frame instead of running the script
# again. On a large input the script is the slowest step of a board load.
#
# The store is per Shiny session, one entry per block: the script's code,
# and once it ran, the input and the frame. A frame is taken only for the
# same code on an identical input. Outside a Shiny session nothing is kept.

# The session's store, or NULL outside a session.
prep_once_store <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session)) {
    return(NULL)
  }
  ud <- session$userData
  if (is.null(ud$blockr_viz_prepared)) {
    ud$blockr_viz_prepared <- new.env(parent = emptyenv())
  }
  ud$blockr_viz_prepared
}

# A block's entry in the store. NULL outside a session, where the script
# simply runs. A list of
#   expect(body)          the code the block's expression will run, given
#                         when the expression is built, so block_eval() can
#                         run it first and keep the frame for the view
#   run(body, data, run)  the frame for the view: the kept one, or `run()`
prep_once <- function(session = shiny::getDefaultReactiveDomain()) {
  store <- prep_once_store(session)
  if (is.null(store)) {
    return(NULL)
  }
  key <- session$ns("prepared")
  list(
    expect = function(body) {
      hit <- store[[key]]
      if (!is.null(body) && (is.null(hit) || !identical(hit$body, body))) {
        store[[key]] <- list(body = body)
      }
    },
    run = function(body, data, run) {
      hit <- store[[key]]
      if (prep_once_hit(hit, body, data)) {
        return(hit$out)
      }
      out <- run()
      store[[key]] <- list(body = body, data = data, out = out, ran = TRUE)
      out
    }
  )
}

prep_once_hit <- function(hit, body, data) {
  isTRUE(hit$ran) && identical(hit$body, body) && identical(hit$data, data)
}

# The block's expression with each script of the store replaced by its
# frame, bound in `env` (the evaluation environment blockr.core made for
# this run). A script that has not run on this input runs here, and its
# frame is kept for the view.
prep_once_splice <- function(expr, env) {
  store <- prep_once_store()
  if (is.null(store)) {
    return(expr)
  }
  data <- get0("data", envir = env, inherits = FALSE)
  keys <- ls(store, sorted = TRUE)
  seen <- list()
  for (key in keys) {
    body <- store[[key]]$body
    if (any(vapply(seen, identical, NA, body))) next
    seen <- c(seen, list(body))
    sym <- as.name(paste0(".blockr_prepared_", length(seen)))
    swapped <- prep_once_find(expr, body, sym)
    if (is.null(swapped)) next
    # Blocks with the same script share a frame when their input is the
    # same; when none has run on this input, it runs here for all of them.
    same <- Filter(function(k) identical(store[[k]]$body, body), keys)
    hit <- Find(function(k) prep_once_hit(store[[k]], body, data), same)
    if (is.null(hit)) {
      kept <- list(body = body, data = data, out = eval(body, env),
                   ran = TRUE)
      for (k in same) store[[k]] <- kept
    } else {
      kept <- store[[hit]]
    }
    assign(as.character(sym), kept$out, envir = env)
    expr <- swapped
  }
  expr
}

# `expr` with the script `body` replaced by `sym`, or NULL when the script is
# not in it. Without a click the table's expression gives the script
# `identity(data)` (table_drill_expr()), the same input.
prep_once_find <- function(expr, body, sym) {
  plain <- do.call(substitute, list(body, list(data = quote(identity(data)))))
  for (b in list(body, plain)) {
    swapped <- prep_once_swap(expr, b, sym)
    if (!identical(swapped, expr)) {
      return(swapped)
    }
  }
  NULL
}

# `e` with every occurrence of the call `body` replaced by `sym`.
prep_once_swap <- function(e, body, sym) {
  if (identical(e, body)) {
    return(sym)
  }
  if (!is.call(e)) {
    return(e)
  }
  cb_map_args(e, function(a) prep_once_swap(a, body, sym))
}

#' @exportS3Method blockr.core::block_eval
block_eval.table_block <- function(x, expr, env, ...) {
  expr <- prep_once_splice(expr, env)
  NextMethod()
}

#' @exportS3Method blockr.core::block_eval
block_eval.chart_block <- function(x, expr, env, ...) {
  expr <- prep_once_splice(expr, env)
  NextMethod()
}
