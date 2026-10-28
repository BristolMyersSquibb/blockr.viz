# A board option with one input in the settings sidebar. `input(inputId)`
# builds the widget; `update(session, id, value)` pushes a changed value to it.
# `help` is an optional line of help text under the widget.
new_input_option <- function(option_id, default, input, update, category,
                             help = NULL, ...) {
  blockr.core::new_board_option(
    id = option_id,
    default = default,
    ui = function(id) {
      shiny::tagList(
        input(shiny::NS(id, option_id)),
        if (!is.null(help)) shiny::helpText(help)
      )
    },
    server = function(..., session) {
      shiny::observeEvent(
        blockr.core::get_board_option_or_null(option_id, session),
        update(session, option_id,
               blockr.core::get_board_option_value(option_id, session))
      )
    },
    category = category,
    ...
  )
}
