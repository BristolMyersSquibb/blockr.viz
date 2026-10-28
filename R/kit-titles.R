# A block's title, subtitle and caption, resolved for its view the same way
# for chart and table (R/title-template.R): against the data the view shows,
# with the input's display attributes as the auto tier and the block's
# settings as the `{@arg}` slots.

# The title bands of a block view: one per line, each the text and the pieces
# the face paints. A piece that names a setting is a word that opens it.
# `v` the block's values, `d` the data the templates resolve against, `auto`
# the input's display attributes (input_display_attrs()), `args` the named
# list of settings a template can name. Returns `title`, `subtitle` and
# `caption`, each NULL (no line) or `list(text, parts, offers)`; `args`, what
# each `{@name}` prints now; `choices`, the lists a template writes as
# `{@name in a, b}`.
viz_titles <- function(v, d, auto, args) {
  tier <- c(title = "label", subtitle = "subtitle", caption = "caption")
  bands <- lapply(stats::setNames(nm = names(tier)), function(b) {
    text <- resolve_block_title(v[[b]], d, auto = auto[[tier[[b]]]],
                                args = args)
    if (!is.character(text) || !length(text) || !nzchar(text)) {
      return(NULL)
    }
    parts <- block_title_parts(v[[b]], d, auto = auto[[tier[[b]]]],
                               args = args)
    offers <- as.list(attr(parts, "offers"))
    attr(parts, "offers") <- NULL
    list(text = text, parts = parts, offers = offers)
  })
  c(bands, list(
    args = lapply(stats::setNames(nm = names(args)), arg_token_value,
                  args = args),
    choices = lapply(
      template_slot_choices(list(v$title, v$subtitle, v$caption)), as.list
    )
  ))
}
