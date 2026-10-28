# A board option with one input in the settings sidebar. `input(inputId)`
# builds the widget; `update(session, id, value)` pushes a changed value to it.
# `help` is an optional line of help text under the widget.
#
# The option records the function that called this one as its constructor
# (`ctor`, as new_board_option() does by default), so a saved board restores
# it through new_exhibit_font_option() and the like; recording this helper
# would break serialization. A restore passes `ctor` back in.
new_input_option <- function(option_id, default, input, update, category,
                             help = NULL, ..., ctor = sys.parent()) {
  force(ctor)
  blockr.core::new_board_option(
    ctor = ctor,
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
