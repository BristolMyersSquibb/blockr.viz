# What the block registry and the title slots read from the spec.

#' The registry's argument specs
#'
#' The arguments with `ai = TRUE`, as blockr.core's `new_arg_specs()`, typed
#' with its JSON Schema subset. `values` and `format` take numbers or text,
#' which the subset cannot say; they carry their description only.
#'
#' @param spec A `viz_spec`.
#' @return An `arg_specs` object.
#' @noRd
viz_registry_args <- function(spec) {
  args <- Filter(function(a) a$ai, spec$args)
  specs <- lapply(args, function(a) {
    blockr.core::new_arg_spec(a$describe, example = registry_example(a),
                              type = registry_type(a$type, a$values))
  })
  do.call(blockr.core::new_arg_specs, specs)
}

# An example as blockr.core reads it: an array is a list, whatever its
# length, here and in the fields of a record.
registry_example <- function(a) {
  registry_example_value(a$type, a$example)
}

registry_example_value <- function(type, x) {
  if (is.null(x)) {
    return(x)
  }
  switch(
    type$name,
    record = registry_example_record(type, x),
    records = lapply(x, function(r) registry_example_record(type, r)),
    if (type$name %in% VIZ_ARRAY_TYPES) as.list(x) else x
  )
}

registry_example_record <- function(type, x) {
  for (nm in intersect(names(x), names(type$fields))) {
    x[nm] <- list(registry_example_value(type$fields[[nm]]$type, x[[nm]]))
  }
  x
}

registry_type <- function(type, values = NULL, required = TRUE) {
  str <- function() blockr.core::arg_string(required = required)
  switch(
    type$name,
    column = , string = , template = str(),
    columns = , strings = blockr.core::arg_array(str(), required = required),
    enum = blockr.core::arg_enum(values, required = required),
    enums = blockr.core::arg_array(blockr.core::arg_enum(values),
                                   required = required),
    number = blockr.core::arg_number(required = required),
    integer = blockr.core::arg_integer(required = required),
    bool = blockr.core::arg_boolean(required = required),
    numbers = blockr.core::arg_array(blockr.core::arg_number(),
                                     required = required),
    record = registry_object(type, required),
    records = blockr.core::arg_array(registry_object(type, TRUE),
                                     required = required),
    NULL
  )
}

# A record field is required in the schema when the spec requires it.
registry_object <- function(type, required) {
  fields <- lapply(type$fields, function(f) {
    registry_type(f$type, f$values, required = isTRUE(f$required))
  })
  fields <- Filter(Negate(is.null), fields)
  do.call(blockr.core::arg_object, c(fields, list(required = required)))
}

#' What the title slots print
#'
#' The arguments with `slot = TRUE`, keyed by name, for the title resolver
#' and the gear's `@` menu. A single value prints as it is; a vector prints
#' as its values joined by ", ".
#'
#' @param spec A `viz_spec`.
#' @param values A named list of the block's values.
#' @return A named list.
#' @noRd
viz_slot_values <- function(spec, values) {
  out <- list()
  for (nm in names(spec$args)) {
    if (!spec$args[[nm]]$slot) {
      next
    }
    v <- values[[nm]]
    if (length(v) > 1L || spec$args[[nm]]$type$name %in% VIZ_ARRAY_TYPES) {
      v <- slot_list_text(v)
    }
    out[nm] <- list(v)
  }
  out
}

slot_list_text <- function(v) {
  if (!length(v)) {
    return(NULL)
  }
  paste(vapply(v, format, character(1L), scientific = FALSE, trim = TRUE),
        collapse = ", ")
}
