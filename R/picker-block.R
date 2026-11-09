#' Picker block (deprecated)
#'
#' Deprecated: bind chart and table mappings to the source columns directly
#' and put a group switch in the board's filter. The block stays so that
#' boards saved with it restore with the block and its links.
#'
#' A curated column picker for locked, dashboard-style boards (the
#' value-selection sibling of blockr.dm's value filter block): the builder
#' defines one or more *pickers* in the gear, the viewer operates one
#' select per picker and nothing else. Each picker fixes which columns are
#' offered (`choices`), which is picked (`selected`), and -- crucially --
#' the name of the **output column** the pick lands in (`into`). Downstream
#' blocks map to the `into` names, which never change, so switching the
#' pick never requires reconfiguration.
#'
#' Semantics per picker:
#' * single (default): the picked column is *copied* into `into`, with its
#'   label attribute carried along (a chart's axis title follows the pick)
#'   and a `blockr_source` attribute naming the source column -- a drill on
#'   the copy claims the *original* column, so a cross-block send still hits
#'   a value filter sitting on the source table. Copies, not renames, so two
#'   pickers may pick the same source column (e.g. a measure against itself).
#' * `multiple = TRUE` (at most one picker): the picked columns pivot long --
#'   values into `into`, the measure identity into `<into>_measure` (a
#'   factor of column *labels*, ready for facetting). Offered-but-unpicked
#'   columns of this picker are dropped so the schema never depends on the
#'   pick.
#' * `optional = TRUE` (single pickers only): the face gains a leading
#'   `(none)` entry. Selecting it leaves the picker inert -- no output
#'   column -- so a downstream chart mapped to `into` drops that aesthetic
#'   (no legend / no facet strip) rather than showing a single phantom
#'   group. Use it for optional roles like colour or facet; leave it off for
#'   required roles (x/y axes).
#'
#' UI follows the blockr design system: `Blockr.Select` controls on the
#' block face (one per picker, labelled by `into`), and the picker
#' definitions in the gear-toggled settings band, like the value filter's
#' filter list. Each gear row carries move-up / move-down buttons next to its
#' remove button: the order of the pickers is the order of the controls on the
#' face, so that is how the builder arranges them. Every column qualifies as a
#' candidate -- a picker can just
#' as well drive a grouping or color dimension as a measure. A picker's
#' offer list starts empty: the builder curates which columns the viewer
#' sees. An empty offer list is inert (no output column) until it has
#' choices, which also covers clearing it mid-edit to refill.
#'
#' The state is externally controllable (`external_ctrl`): a board
#' modification carrying `list(pickers = ...)` sets the pick from outside --
#' another block, the assistant, or the block card's control panel -- and the
#' face follows. Such a payload PATCHES the authored definition: entries match
#' by `into`, only the fields they carry override, and pickers the payload
#' does not mention keep their state. So steering one picker is
#' `list(pickers = list(list(into = "value", selected = "AEBODSYS")))`, with
#' the curated offer list, the flags and the sibling pickers left alone. An
#' entry whose `into` matches no picker adds one, but only when it also brings
#' `choices` -- otherwise it is a typo'd `into`, not a request for a control
#' that offers nothing. A payload that is not shaped like a picker list at all
#' is ignored, and the block keeps its last good definition.
#'
#' Design records: blockr.docs design-system/target select-controls.html
#' (control style) and measure-switch-proposals.html (schema; the picker
#' generalizes the measure switch to n pickers with editable output names).
#'
#' @param state List with `pickers` -- a list of picker entries, each
#'   `list(into, choices, selected, multiple, optional)`. Empty (default)
#'   seeds one empty, inert picker (`into = "value"`) on first data arrival
#'   for the builder to fill.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_picker_block(
#'       state = list(pickers = list(
#'         list(into = "value", choices = c("Sepal.Length", "Sepal.Width"),
#'              selected = "Sepal.Length", multiple = FALSE)
#'       ))
#'     ),
#'     data = list(data = iris)
#'   )
#' }
#'
#' @return A transform block of class `picker_block`
#' @export
# viz: long deprecated, kept for saved boards
new_picker_block <- function(
  state = list(pickers = list()),
  ...
) {
  if (!isTRUE(picker_quiet$on)) {
    blockr.core::log_warn(
      "new_picker_block() is deprecated. Bind chart and table mappings to ",
      "the source columns and put a group switch in the board's filter.",
      use_glue = FALSE
    )
  }

  pickers <- if (is.null(state$pickers)) list() else state$pickers

  blockr.core::new_transform_block(
    # viz: long deprecated, kept for saved boards
    function(id, data) {
      # viz: long deprecated, kept for saved boards
      shiny::moduleServer(id, function(input, output, session) {
        # The `state` field must BE a reactiveVal, not a derived reactive:
        # under `external_ctrl` blockr.core writes the controlled value
        # straight into it (block-server.R rejects anything else). So the
        # reactiveVal holds the whole state blob, and an external write (a
        # cross-block send, the assistant, the block card's control panel)
        # lands exactly where a client edit lands.
        r_state <- shiny::reactiveVal(list(pickers = pickers))

        # ... but everything else reads the REPAIRED list, never the raw blob:
        # the expression must not see, even for one flush, whatever an
        # external writer happened to send.
        r_pickers <- shiny::reactiveVal(normalize_pickers(pickers))

        # Which of the two wrote last. A write from inside the block (client
        # edit, data seeding, the repair below) is authoritative and only
        # normalized; anything else is external and gets patched into the
        # authored definition first.
        own <- new.env(parent = emptyenv())
        own$write <- TRUE

        set_pickers <- function(pks) {
          if (!identical(shiny::isolate(r_state()), list(pickers = pks))) {
            own$write <- TRUE
            r_state(list(pickers = pks))
          }
        }

        # Repair whatever landed in the state, publish it, and write the
        # canonical form back so the board saves that. Terminates because
        # normalize_pickers() is idempotent: the write-back arrives as an own
        # write, normalizes to itself, and `set_pickers()` stops on identical.
        shiny::observeEvent(r_state(), {
          raw <- picker_payload(r_state())
          pks <- normalize_pickers(
            if (own$write) raw else merge_pickers(r_pickers(), raw)
          )
          own$write <- FALSE
          if (!identical(shiny::isolate(r_pickers()), pks)) {
            r_pickers(pks)
          }
          set_pickers(pks)
        })

        col_labels <- shiny::reactive({
          d <- data()
          cols <- colnames(d)
          vapply(
            cols,
            function(nm) {
              lb <- attr(d[[nm]], "label", exact = TRUE)
              if (is.null(lb) || !nzchar(lb)) "" else as.character(lb)
            },
            character(1)
          )
        })

        # Reconcile with incoming data; seed one EMPTY picker on first arrival
        # so the builder has a row to fill. The offer list starts empty on
        # purpose -- the builder curates which columns the viewer sees; the
        # block does nothing until at least one column is offered.
        shiny::observeEvent(data(), {
          pks <- r_pickers()
          if (!length(pks) && ncol(data())) {
            pks <- list(list(
              into = "value",
              choices = character(),
              selected = character(),
              multiple = FALSE
            ))
          }
          set_pickers(pks)
        })

        # Push the full control state. Length-1 vectors unbox to scalars over
        # the wire; the JS side re-wraps them. ALL columns are candidates in
        # the gear's "Columns offered" pool (a picker may drive a grouping or
        # color dimension, not just a measure); the builder chooses from it.
        # Reads its reactives directly, so the observe() below tracks them and
        # the announce handler (isolated by observeEvent) does not.
        send_state <- function() {
          shiny::req(data())
          labs <- col_labels()
          session$sendCustomMessage(
            session$ns("pk_update"),
            list(
              cfg_options = lapply(colnames(data()), function(nm) {
                list(value = nm, label = labs[[nm]])
              }),
              pickers = r_pickers()
            )
          )
        }

        shiny::observe(send_state())

        # Deferred dock panel: the push above went out at boot, before this
        # block's per-instance script existed on the page (it ships with the
        # panel), and Shiny drops a custom message that has no handler. The
        # script's own `pending` slot cannot catch that -- it only parks a
        # message that arrived after the script parsed. So the client
        # announces when it comes up with nothing parked, and we send again;
        # without this the gear band is empty and the face carries no
        # controls at all, restored pickers or not. Same fix as the chart
        # (`drilldown_block_ready`) and the summarize table.
        shiny::observeEvent(input$picker_block_ready, send_state())

        # JS -> R: the whole picker list on any change (value-filter style).
        # Sent as a JSON string: a bare array of objects would arrive
        # simplified into a data.frame by Shiny's deserializer.
        shiny::observeEvent(input$pickers, {
          raw <- tryCatch(
            jsonlite::fromJSON(input$pickers, simplifyVector = FALSE),
            error = function(e) NULL
          )
          pks <- normalize_pickers(raw)
          if (length(pks)) {
            set_pickers(pks)
          }
        })

        list(
          expr = shiny::reactive(make_picker_expr(r_pickers())),
          state = list(state = r_state)
        )
      })
    },
    function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        blockr.ui::controls_dep(),
        shiny::div(
          class = "block-container blockr-picker",
          shiny::div(
            class = "blockr-gear-header",
            shiny::tags$button(
              id = ns("gear"),
              type = "button",
              class = "blockr-gear-btn",
              `aria-label` = "Pickers",
              `data-blockr-tooltip` = "Settings"
            )
          ),
          shiny::div(
            id = ns("band"),
            class = "blockr-settings blockr-settings--beak",
            shiny::div(class = "blockr-settings__title", "Pickers"),
            shiny::div(id = ns("rows"), class = "pk-rows"),
            shiny::div(
              class = "blockr-add-row",
              shiny::tags$span(
                id = ns("add"),
                class = "blockr-add-link",
                shiny::tags$span(class = "blockr-add-icon"),
                "Add picker"
              )
            )
          ),
          shiny::div(id = ns("face"))
        ),
        picker_block_assets(ns)
      )
    },
    dat_valid = function(data) {
      stopifnot(is.data.frame(data))
    },
    expr_type = "bquoted",
    allow_empty_state = TRUE,
    # The viewer control of a locked dashboard is exactly the thing another
    # block (or the assistant) wants to steer, so `state` is externally
    # controllable: a board modification carrying
    # `list(pickers = list(list(into = ..., selected = ...)))` sets the pick,
    # the push observer above echoes it to the face, and the normalizing
    # observer repairs a partial payload.
    external_ctrl = TRUE,
    class = "picker_block",
    ...
  )
}

# Coerce a client- or ctor-supplied picker list into canonical form. This is a
# STRUCTURAL normalization only -- it deliberately does NOT prune choices
# against the data columns. The authored picker definition is the source of
# truth and must survive a data frame that transiently lacks its columns (e.g.
# on board restore, while an upstream block is still restoring its own state and
# briefly emits a frame without them). Pruning here was destructive: intersect
# against such a transient frame wiped the offer/selection permanently, so the
# gear's "Columns offered" and the face pick came back empty after loading.
# Restriction to the columns actually present happens instead in
# make_picker_expr() at eval time, so a picker self-heals when its columns
# return. Rules: selected is a subset of choices (first choice as fallback when
# empty), single pick unless multiple, at most ONE multiple picker (a second
# pivot would cross-multiply rows), output names non-empty and unique.
normalize_pickers <- function(pickers) {
  if (is.null(pickers) || !length(pickers)) {
    return(list())
  }
  out <- list()
  multi_seen <- FALSE
  for (p in pickers) {
    ch <- as.character(unlist(p$choices))
    sel <- intersect(as.character(unlist(p$selected)), ch)
    mult <- isTRUE(p$multiple) && !multi_seen
    multi_seen <- multi_seen || mult
    # `optional` (single pickers only): the viewer may pick "(none)", leaving
    # the picker inert so it emits no output column -- the downstream chart
    # then drops that aesthetic (no legend / no facet). For an OPTIONAL picker
    # an empty selection is a legitimate "(none)", so it is NOT auto-filled to
    # the first choice; a required picker keeps the >=1 backstop.
    opt <- isTRUE(p$optional) && !mult
    # An empty-choices picker is kept, inert: the builder may have just
    # cleared the offer list to refill it (expr skips it meanwhile).
    if (!length(sel) && length(ch) && !opt) {
      sel <- ch[[1L]]
    }
    if (!mult && length(sel) > 1L) {
      sel <- sel[[1L]]
    }
    into <- as.character(if (is.null(p$into)) "" else p$into)[[1L]]
    if (is.na(into) || !nzchar(into)) {
      into <- "value"
    }
    out[[length(out) + 1L]] <- list(
      into = into,
      choices = ch,
      selected = sel,
      multiple = mult,
      optional = opt
    )
  }
  if (length(out)) {
    intos <- make.unique(vapply(out, `[[`, character(1), "into"), sep = "")
    for (i in seq_along(out)) {
      out[[i]]$into <- intos[[i]]
    }
  }
  out
}

# The picker list inside a `state` blob, or NULL when there is nothing usable
# in it. What lands in `state` from outside is not the block's own writing: a
# cross-block send, an LLM tool call, or core's default `ctrl_block_ui()` --
# which offers a plain text field per controllable input, so submitting it
# writes a bare STRING where the block expects a list. The repair observer
# runs on its own, outside the try()/rollback the ctrl plugin wraps around
# `eval()`, so reaching into such a payload would take the block's session
# down rather than be reported as a failed control attempt. Keep only what is
# shaped like a picker list; anything else is no payload at all and the block
# holds its last good definition, the same way it holds it against a transient
# frame that lacks its columns.
picker_payload <- function(x) {
  if (!is.list(x) || !is.list(x$pickers)) {
    return(NULL)
  }
  Filter(is.list, x$pickers)
}

# Patch an EXTERNAL picker payload onto the authored definition. External
# control is about the PICK: a sender saying "show AEBODSYS in the AE Term
# picker" should not have to restate the offer list, the flags and every other
# picker just to keep them. So incoming entries are matched to the current
# ones by `into` (by position when `into` is omitted) and only the fields they
# actually carry override, and current pickers the payload does not mention
# survive untouched. Removing a picker is builder work in the gear, not
# something a send can do by omission.
#
# An entry matching nothing is a NEW picker, so it is appended -- but only if
# it brings `choices`. Without them it would land as an inert control the
# viewer can do nothing with, permanently, on a board someone curated: that is
# what a typo in `into` looks like ("show x in the y_value picker" against a
# board whose picker is called `y`), and it is the more likely reading by far.
# An empty offer list means "mid-edit" only when the builder is the one
# holding the gear.
#
# Deliberately NOT applied to the block's own writes: there, an empty
# `choices` is a legitimate inert state (the builder clearing the offer list
# to refill it), and a patch would keep resurrecting the cleared list.
merge_pickers <- function(cur, incoming) {
  if (is.null(incoming) || !length(incoming)) {
    return(cur)
  }
  if (!length(cur)) {
    return(Filter(function(p) is.list(p) && length(p$choices), incoming))
  }
  intos <- vapply(
    cur,
    function(p) if (length(p$into)) as.character(p$into)[[1L]] else "",
    character(1)
  )
  out <- cur
  for (i in seq_along(incoming)) {
    p <- incoming[[i]]
    if (!is.list(p)) {
      next
    }
    hit <- if (length(p$into)) {
      match(as.character(p$into)[[1L]], intos)
    } else if (i <= length(cur)) {
      i
    } else {
      NA_integer_
    }
    if (is.na(hit)) {
      if (length(p$choices)) {
        out[[length(out) + 1L]] <- p
      }
      next
    }
    for (f in names(p)) {
      if (!is.null(p[[f]])) {
        out[[hit]][[f]] <- p[[f]]
      }
    }
  }
  out
}

# Build the transform expression: per picker a labelled copy, or (multiple)
# a pivot into `into` + `<into>_measure`.
#
# The data slot is `.(data)`, NOT a free `data` symbol. The block is
# expr_type = "bquoted", so blockr.core substitutes `.()` placeholders and
# nothing else: a free `data` resolves at RUNTIME (the block's eval env
# binds it) and so looks fine in the app, while the EXPORTED code carries
# the bare name. In a generated script or report chunk it then falls
# through to utils::data -- the block "evaluates" to a function, and every
# dependent fails with "no applicable method for 'filter' applied to an
# object of class \"function\"" pointing at nothing. Bound once at the top of
# the local() so the body below can keep reading `data`.
#
# The placeholder is built by hand rather than through blockr.core::bbquote,
# which is what usually emits it. bbquote walks the expression for splices,
# and that walk DELETES any NULL element of a call -- including the srcref
# slot of a `function` definition, which is NULL exactly when the package
# was installed (keep.source = FALSE) and a srcref under load_all(). This
# body defines two functions, so bbquote crashed it on Connect while every
# dev run passed: "'names' attribute [4] must be the same length as the
# vector [3]", fatal on view switch. `.(data)` is a plain call, so base
# bquote can splice it in verbatim.
dot_slot <- function(nm) call(".", as.name(nm))

make_picker_expr <- function(pickers) {
  dot <- dot_slot("data")
  if (!length(pickers)) {
    return(bquote(identity(.(dot)), list(dot = dot)))
  }
  # Inert pickers (choices cleared mid-edit) contribute nothing.
  pickers <- Filter(function(p) length(p$selected) > 0L, pickers)
  if (!length(pickers)) {
    return(bquote(identity(.(dot)), list(dot = dot)))
  }
  # Copies run before the pivot: the multiple picker consumes (and drops)
  # source columns that a single picker may also offer, so singles must
  # read the still-wide data.
  is_mult <- vapply(pickers, function(p) isTRUE(p$multiple), logical(1))
  pickers <- c(pickers[!is_mult], pickers[is_mult])
  bquote(
    local({
      data <- .(dot)
      pks <- .(pks)
      out <- data
      for (p in pks) {
        # Restrict to columns actually present in the data at eval time. A
        # picker whose columns are temporarily absent (an upstream block still
        # settling on restore) is skipped and recovers on its own once they
        # return -- the stored definition is never mutated.
        present <- intersect(p$choices, names(data))
        sel <- intersect(p$selected, present)
        if (!length(sel)) next
        labs <- vapply(
          present,
          function(nm) {
            lb <- attr(data[[nm]], "label", exact = TRUE)
            if (is.null(lb) || !nzchar(lb)) nm else as.character(lb)
          },
          character(1)
        )
        # Provenance for drill claims: a drill on the copy must claim the
        # ORIGINAL source column (dd_ctrl_claims resolves this attribute), or
        # a cross-block send names a column the source table does not have.
        # When the picked column is itself a picker copy (chained pickers),
        # keep ITS recorded source rather than the intermediate copy's name.
        chain_src <- function(v, fallback) {
          cs <- attr(v, "blockr_source", exact = TRUE)
          if (is.character(cs) && length(cs) == 1L && nzchar(cs)) cs
          else fallback
        }
        if (isTRUE(p$multiple)) {
          # Offered-but-unpicked columns of a multiple picker are dropped:
          # the output schema must not depend on which choices are picked.
          # Read the provenance BEFORE the pivot: pivot_longer rebuilds the
          # value column and drops its attributes.
          src1 <- if (length(sel) == 1L) chain_src(out[[sel]], unname(sel))
          measure_col <- paste0(p$into, "_measure")
          drop <- setdiff(present, sel)
          out <- tidyr::pivot_longer(
            out[setdiff(names(out), drop)],
            cols = tidyr::all_of(sel),
            names_to = measure_col,
            values_to = p$into
          )
          out[[measure_col]] <- factor(
            labs[out[[measure_col]]],
            levels = unname(labs[sel])
          )
          if (length(sel) == 1L) {
            attr(out[[p$into]], "label") <- unname(labs[sel])
            attr(out[[p$into]], "blockr_source") <- src1
          }
        } else {
          s1 <- sel[[1L]]
          out[[p$into]] <- out[[s1]]
          attr(out[[p$into]], "label") <- unname(labs[[s1]])
          attr(out[[p$into]], "blockr_source") <- chain_src(out[[s1]], s1)
        }
      }
      out
    }),
    list(pks = pickers, dot = dot)
  )
}

# Registering builds the block once, which is no reason to warn.
picker_quiet <- new.env(parent = emptyenv())

register_picker_block <- function() {
  picker_quiet$on <- TRUE
  on.exit(picker_quiet$on <- FALSE)
  blockr.core::register_block(
    "new_picker_block",
    name = "Picker (deprecated)",
    description = paste0(
      "Deprecated, kept for saved boards. Curated column pickers for locked dashboards: each picker offers a ",
      "fixed set of columns and lands the pick in a stable, named output ",
      "column, so downstream mappings never change. A multiple picker ",
      "pivots its picks long (into + into_measure) for facetting."
    ),
    category = "transform",
    # ui-radios: an option list with one picked -- the block's whole gesture.
    # Header color comes from the category (transform = #009E73 green).
    icon = "ui-radios",
    guidance = paste0(
      "Use upstream of a chart or table whose mappings should stay fixed ",
      "while the viewer switches what is shown. Define one picker per ",
      "viewer control: `into` names the output column (downstream maps to ",
      "it), `choices` fixes what is offered, `selected` is the current ",
      "pick. Single pickers copy the picked column into `into` (label ",
      "carried), so two pickers may pick the same source. At most one ",
      "picker may set `multiple`: its picks pivot long into `into` + ",
      "`<into>_measure` (factor of column labels) and its unpicked choices ",
      "are dropped -- map a chart's facet to `<into>_measure`."
    ),
    arguments = blockr.core::new_arg_specs(
      state = blockr.core::new_arg_spec(
        "Object with `pickers`: array of picker entries",
        example = list(pickers = list(list(
          into = "value",
          choices = list("Sepal.Length", "Sepal.Width"),
          selected = list("Sepal.Length"),
          multiple = FALSE,
          optional = FALSE
        ))),
        type = blockr.core::arg_object(
          pickers = blockr.core::arg_array(
            blockr.core::arg_object(
              into = blockr.core::arg_string(),
              choices = blockr.core::arg_array(blockr.core::arg_string()),
              selected = blockr.core::arg_array(blockr.core::arg_string()),
              multiple = blockr.core::arg_boolean(),
              optional = blockr.core::arg_boolean()
            )
          )
        )
      )
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
