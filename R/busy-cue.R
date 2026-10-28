# The busy cue's two messages (inst/js/core/busy.js). `id` is the id of the
# output element the cue dims. Blocks outside this package that render
# through viz send the same two messages.

busy_cue_start <- function(session, id, label = "computing") {
  session$sendCustomMessage("blockr-busy", list(id = id, label = label))
}

busy_cue_done <- function(session, id) {
  session$sendCustomMessage("blockr-busy-done", list(id = id))
}
