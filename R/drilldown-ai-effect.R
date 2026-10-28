# config_effect() methods for the blockr.ai assistant. A chart or table block
# passes its data through, so the data effect shows no change. These methods
# describe the block's config, the column-to-role bindings, and flag bindings
# that name columns absent from the input. They are registered onto blockr.ai's
# generic at load; without blockr.ai, or with a version that does not export
# config_effect, registration is a no-op.

# Column-valued roles of the chart block, in display order. y_lines and
# x_lines hold line positions, not column names, and are reported separately.
dd_chart_roles <- c("x", "y", "xend", "color", "facet", "series", "label",
                    "drill", "ymin", "ymax", "id")

# The first element of `v` as a string; "" for NULL.
first_chr <- function(v) as.character(v %||% "")[1]

#' @noRd
config_effect.chart_block <- function(block, args, data = NULL, ...) {
  cols <- if (is.data.frame(data)) names(data) else NULL
  ct <- as.character(args$type %||% "bar")[1]

  parts <- character()
  bad <- character()
  for (r in dd_chart_roles) {
    v <- args[[r]]
    if (is.null(v) || !nzchar(first_chr(v))) next
    v <- first_chr(v)
    parts <- c(parts, paste0(r, "=", v))
    # ".count" and "auto" are sentinels; every other value must be a column.
    if (!is.null(cols) && !(v %in% c(".count", "auto")) && !(v %in% cols)) {
      bad <- c(bad, paste0(r, " references '", v, "'"))
    }
  }
  # Helper line positions are echoed as given.
  for (r in c("y_lines", "x_lines")) {
    v <- unlist(args[[r]])
    if (length(v)) parts <- c(parts, paste0(r, "=", paste(v, collapse = ",")))
  }
  # Distribution statistics of boxplot and pointrange.
  if (ct %in% c("boxplot", "pointrange")) {
    s <- first_chr(args$summary)
    if (nzchar(s)) parts <- c(parts, paste0("summary=", s))
    w <- first_chr(args$whiskers)
    if (identical(ct, "boxplot") && nzchar(w)) {
      parts <- c(parts, paste0("whiskers=", w))
    }
    if (isTRUE(args$connect_centers) || identical(args$connect_centers, "on")) {
      parts <- c(parts, "connect_centers=on")
    }
  }
  agg <- first_chr(args$func)

  desc <- paste0(
    ct, " chart configured: ",
    if (length(parts)) paste(parts, collapse = ", ") else "(no column bindings)",
    if (nzchar(agg)) paste0(" agg=", agg),
    if (!nzchar(first_chr(args$drill))) {
      " -- drill OFF (set `drill` to enable click-to-filter)"
    }
  )
  if (length(bad)) {
    desc <- paste0(
      desc, " -- INVALID column binding(s): ", paste(bad, collapse = "; "),
      ". Available columns: ", paste(cols, collapse = ", ")
    )
  }
  desc
}

#' @noRd
config_effect.table_block <- function(block, args, data = NULL, ...) {
  cols <- if (is.data.frame(data)) names(data) else NULL
  parts <- paste0("layout=", as.character(args$layout %||% "records")[1])
  bad <- character()
  check <- function(r, v) {
    v <- as.character(unlist(v))
    v <- v[nzchar(v)]
    if (!length(v)) return()
    parts <<- c(parts, paste0(r, "=", paste(v, collapse = "/")))
    miss <- setdiff(v, cols %||% v)
    if (length(miss)) {
      bad <<- c(bad, paste0(r, " references '", paste(miss, collapse = "', '"),
                            "'"))
    }
  }
  check("rowname", args$rowname)
  check("columns", args$columns)
  check("by", args$by)
  # `drill` is a mode ("auto", "source") or a column.
  dr <- first_chr(args$drill)
  if (dr %in% c("auto", "source")) {
    parts <- c(parts, paste0("drill=", dr))
  } else {
    check("drill", dr)
  }
  if (length(args$summaries)) {
    parts <- c(parts, paste0(length(args$summaries), " summaries"))
  }
  desc <- paste0("table configured: ", paste(parts, collapse = ", "))
  if (length(bad)) {
    desc <- paste0(desc, " -- INVALID: ", paste(bad, collapse = "; "),
                   ". Available columns: ", paste(cols, collapse = ", "))
  }
  desc
}

# Registers the methods on blockr.ai's generic.
register_drilldown_ai_effect <- function() {
  if (!requireNamespace("blockr.ai", quietly = TRUE)) {
    return(invisible(FALSE))
  }
  ns <- asNamespace("blockr.ai")
  if (!exists("config_effect", envir = ns, inherits = FALSE)) {
    return(invisible(FALSE))
  }
  registerS3method("config_effect", "chart_block",
                   config_effect.chart_block, envir = ns)
  registerS3method("config_effect", "table_block",
                   config_effect.table_block, envir = ns)
  invisible(TRUE)
}
