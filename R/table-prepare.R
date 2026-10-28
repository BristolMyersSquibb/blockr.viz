# The table's prepare step: the block's `script` run on the input before any
# layout builds its table (as on the chart, R/prepare-apply.R), so a layout
# switch keeps it. `r_script` and `r_values` are the state's reactives.
# Returns `specs` (the controls the script declares), `prepared`
# (`list(data, error)`), `data` (the rows the layouts draw from),
# `wrap_after(ex)` (the script applied to the expression's input) and
# `wrap_drill(filters, members)` (the groups click filter around the script).
table_prepare <- function(data, r_script, r_values) {
  r_parsed <- shiny::reactive(cb_parse(r_script()))
  has_script <- function() {
    s <- r_script()
    !is.null(s) && nzchar(trimws(paste(s, collapse = "\n")))
  }
  # The script reads a data frame; a finished table is converted first.
  raw <- shiny::reactive({
    d <- data()
    if (is.data.frame(d)) d else as_annotated_df(d)
  })
  # Specs keep their last value while the data is gone, or a hidden panel
  # would flip the compiled script.
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
  # error reaches the face.
  r_data <- shiny::reactive({
    if (!has_script()) return(data())
    prep <- r_prepared()
    if (is.null(prep$error)) return(prep$data)
    d <- tryCatch(raw(), error = function(e) NULL)
    if (is.data.frame(d)) d[0, , drop = FALSE] else NULL
  })
  compiled <- function() {
    if (!has_script()) return(NULL)
    cb_expr(r_parsed(), r_specs(), r_values(), slot = TRUE)
  }
  wrap_after <- function(ex) {
    sx <- compiled()
    if (is.null(sx)) ex else table_sub_data(ex, sx)
  }
  # A clicked column only the script makes is filtered after it.
  wrap_drill <- function(filters, members = NULL) {
    sx <- compiled()
    src <- if (!is.null(sx) && length(filters)) {
      tryCatch(names(raw()), error = function(e) NULL)
    }
    table_drill_wrap(sx, filters, members, src)
  }
  list(specs = r_specs, prepared = r_prepared, data = r_data,
       wrap_after = wrap_after, wrap_drill = wrap_drill)
}
