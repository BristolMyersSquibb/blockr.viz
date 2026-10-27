# A block's title, subtitle and caption, resolved for its view. The chart
# and the table resolve them the same way (R/title-template.R): against the
# data the view shows, with the input's own display attributes as the auto
# tier and the block's settings as the `{@arg}` slots.

#' The title bands of a block view
#'
#' @param v The block's values.
#' @param d The data the templates are resolved against.
#' @param auto The input's display attributes (input_display_attrs()).
#' @param args The settings a template can name, `{@arg}`: a named list.
#' @return A list: `title`, `subtitle` and `caption` as text; `parts`, the
#'   pieces the face paints, per band; `offers`, the settings a band's
#'   dropped `[ ]` clause offers; `sentence_args`, every setting the text
#'   names; `arg_values`, the slot values as text; `slot_choices`, the
#'   choices a slot word opens.
#' @noRd
viz_titles <- function(v, d, auto, args) {
  bands <- c("title", "subtitle", "caption")
  tier <- c(title = "label", subtitle = "subtitle", caption = "caption")
  parts <- lapply(stats::setNames(nm = bands), function(b) {
    block_title_parts(v[[b]], d, auto = auto[[tier[[b]]]], args = args)
  })
  text <- lapply(stats::setNames(nm = bands), function(b) {
    resolve_block_title(v[[b]], d, auto = auto[[tier[[b]]]], args = args)
  })
  c(text, list(
    parts = parts,
    offers = lapply(parts, function(p) as.list(attr(p, "offers"))),
    sentence_args = as.list(sentence_args(parts$title, parts$subtitle,
                                          parts$caption)),
    arg_values = lapply(stats::setNames(nm = names(args)), arg_token_value,
                        args = args),
    slot_choices = lapply(
      template_slot_choices(list(v$title, v$subtitle, v$caption)), as.list
    )
  ))
}
