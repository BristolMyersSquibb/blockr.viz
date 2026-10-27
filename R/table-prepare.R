# The table block's prepare step: R code run on the incoming rows before any
# layout builds its table (the chart block's `script`, R/prepare-apply.R). It
# belongs to the block, not to one engine, so the records, groups and
# variables layouts all draw from what it returns and a layout switch keeps
# it. Call inside the block's moduleServer.
#
# Returns a list of
#   script, values  reactiveVals (the block's state)
#   specs           the controls the script declares
#   prepared        list(data, error) from dd_prepare_run()
#   data            the rows the engines draw from
#   config(key, v)  applies a gear edit of `script` or a script value
#                   (`sv_<name>`); TRUE if the key was one of them
#   gear()          what a gear needs: the text, the declared controls, the
#                   error and each control's value
#   wrap_after(ex)  the block expression with the script applied to its
#                   input (records, variables: the click filters the rows
#                   the table shows)
#   wrap_around(ex) the script wrapped around the expression (groups: the
#                   click filters the source rows, as on the chart)
#   wrap_drill(filters, members) the groups click filter with the script
#                   around it; a column only the script makes is filtered
#                   after it
#
# A block on the kit passes its state's `script` and `script_values` as
# `r_script` and `r_values`; the old engines pass the starting values.
#' @noRd
table_prepare <- function(data, script = NULL, values = list(),
                          r_script = NULL, r_values = NULL) {
  r_script <- r_script %||% shiny::reactiveVal(cb_script_text(script))
  r_values <- r_values %||%
    shiny::reactiveVal(if (is.list(values)) values else list())
  r_parsed <- shiny::reactive(cb_parse(r_script()))
  has_script <- function() {
    s <- r_script()
    !is.null(s) && nzchar(trimws(paste(s, collapse = "\n")))
  }

  # The script reads the input as a data frame; a finished table (composer,
  # gtsummary) is converted first. Without a script the input passes as it
  # is, so each engine sees exactly what it saw before.
  raw <- shiny::reactive({
    d <- data()
    if (is.data.frame(d)) d else as_annotated_df(d)
  })

  # Specs keep their last good value while the data is momentarily gone, or
  # a hidden panel would flip the compiled script.
  last_specs <- new.env(parent = emptyenv())
  last_specs$value <- list()
  r_specs <- shiny::reactive({
    parsed <- r_parsed()
    d <- if (has_script()) tryCatch(raw(), error = function(e) NULL)
    if (is.null(d) && length(last_specs$value)) {
      return(last_specs$value)
    }
    out <- cb_specs(parsed, d)
    if (!identical(out, last_specs$value)) last_specs$value <- out
    last_specs$value
  })
  r_prepared <- shiny::reactive({
    if (!has_script()) return(list(data = NULL, error = NULL))
    dd_prepare_run(raw(), r_parsed(), r_specs(), r_values())
  })

  # A failed script leaves an empty frame with the input's columns, so the
  # error reaches the card and the gear instead of a blank block.
  r_data <- shiny::reactive({
    if (!has_script()) return(data())
    prep <- r_prepared()
    if (is.null(prep$error)) return(prep$data)
    d <- tryCatch(raw(), error = function(e) NULL)
    if (is.data.frame(d)) d[0, , drop = FALSE] else NULL
  })

  config <- function(key, value) {
    if (identical(key, "script")) {
      # Committed on blur or Apply, never per keystroke.
      new <- cb_script_text(as.character(value %||% ""))
      if (!identical(shiny::isolate(r_script()), new)) r_script(new)
      return(TRUE)
    }
    if (startsWith(key, "sv_")) {
      # A script value, read against the declared type.
      sv <- dd_script_values(stats::setNames(list(value), key),
                             shiny::isolate(r_specs()),
                             shiny::isolate(r_values()))
      if (!identical(sv, shiny::isolate(r_values()))) r_values(sv)
      return(TRUE)
    }
    FALSE
  }

  gear <- function() {
    list(
      script = r_script() %||% "",
      script_inputs = dd_script_roles(r_specs()),
      script_error = r_prepared()$error,
      script_values = dd_script_cfg(r_specs(), r_values())
    )
  }

  compiled <- function() {
    if (!has_script()) return(NULL)
    cb_expr(r_parsed(), r_specs(), r_values(), slot = TRUE)
  }
  wrap_after <- function(ex) {
    sx <- compiled()
    if (is.null(sx)) ex else table_sub_data(ex, sx)
  }
  wrap_around <- function(ex) {
    sx <- compiled()
    if (is.null(sx)) ex else dd_splice_slot(sx, ex)
  }

  # The groups click filter around the script. A clicked column the source
  # lacks (a `by` the script makes) is filtered after the script instead
  # (summarize_drill_wrap()).
  wrap_drill <- function(filters, members = NULL) {
    sx <- compiled()
    src <- if (!is.null(sx) && length(filters)) {
      tryCatch(names(raw()), error = function(e) NULL)
    }
    summarize_drill_wrap(sx, filters, members, src)
  }

  list(
    script = r_script, values = r_values, specs = r_specs,
    prepared = r_prepared, data = r_data, config = config, gear = gear,
    wrap_after = wrap_after, wrap_around = wrap_around,
    wrap_drill = wrap_drill
  )
}
