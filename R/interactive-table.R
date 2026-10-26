#' Interactive table
#'
#' Draws a table the way [new_table_block()] draws it on screen, for use in
#' another block's UI. `x` is anything [as_annotated_df()] accepts: a plain
#' data frame, an annotated data frame (shown with its sections and spanner
#' headers), or a table object such as a composer table. [static_table()] is
#' the print counterpart.
#'
#' The table searches, sorts and collapses in the browser. A row click is
#' sent to the server as `input[[paste0(elem_id, "_action")]]`, so the
#' calling block handles the drill; see [dd_ctrl_sender()].
#'
#' `update_interactive_table()` replaces the table inside a drawn
#' `interactive_table()` without redrawing the container, so the search text
#' and the scroll position stay. Draw the container with `x = NULL` and fill
#' it from the server.
#'
#' @param x A data frame, an object with an [as_annotated_df()] method, or
#'   `NULL` for an empty container. `update_interactive_table()` also takes
#'   an htmltools tag (a message), which is shown as it is.
#' @param drill `TRUE` makes rows clickable: an annotated table claims the
#'   clicked row, a plain one its first column. A column name picks the
#'   column of a plain table. `FALSE` turns clicks off.
#' @param drill_target Id of the block a click is sent to, or `NULL`. With a
#'   target, a click is an event (a flash and a send) rather than a
#'   selection that stays highlighted.
#' @param sortable Can column headers sort the rows?
#' @param collapsible Can section headers collapse?
#' @param search Draw a search box above the table?
#' @param download A download control, from [dt_download_control()].
#' @param status A tag for the footer's status line, such as a
#'   `shiny::uiOutput()` that names the active filter.
#' @param scroll `"box"` scrolls inside the table, up to `max_height`;
#'   `"page"` lets the table grow and the page scroll.
#' @param max_height CSS height limit of the scroll box.
#' @param elem_id The table's namespaced id. Required for a drill and for
#'   `update_interactive_table()`.
#'
#' @return `interactive_table()`: an [htmltools::tagList()].
#'   `update_interactive_table()`: called for its side effect.
#' @seealso [static_table()], [new_table_block()]
#' @examples
#' interactive_table(head(mtcars), drill = FALSE)
#' @export
interactive_table <- function(x = NULL, drill = TRUE, drill_target = NULL,
                              sortable = TRUE, collapsible = TRUE,
                              search = TRUE, download = NULL, status = NULL,
                              scroll = c("box", "page"),
                              max_height = "600px", elem_id = NULL) {
  scroll <- match.arg(scroll)
  if (is.null(x)) {
    structured <- NULL
    inner <- htmltools::tags$div(class = "dt-table-slot")
  } else {
    d <- interactive_table_data(x)
    structured <- dt_is_structured(d)
    inner <- interactive_table_body(d, drill, drill_target, sortable,
                                    collapsible)
  }
  dt_chrome(
    elem_id       = elem_id,
    structured    = structured,
    max_height    = max_height,
    inner         = inner,
    search        = search,
    download_slot = download,
    status_slot   = status,
    gear          = FALSE,
    scroll        = scroll
  )
}

#' @rdname interactive_table
#' @param session The Shiny session (a module session works).
#' @export
update_interactive_table <- function(session, elem_id, x, drill = TRUE,
                                     drill_target = NULL, sortable = TRUE,
                                     collapsible = TRUE) {
  tag <- if (is.null(x)) {
    ""
  } else if (inherits(x, c("shiny.tag", "shiny.tag.list", "html"))) {
    x
  } else {
    interactive_table_body(interactive_table_data(x), drill, drill_target,
                           sortable, collapsible)
  }
  # The browser skips a payload whose rev it has seen, so each send counts up
  # per table.
  revs <- session$userData$blockr_viz_table_rev %||% list()
  rev <- (revs[[elem_id]] %||% 0L) + 1L
  revs[[elem_id]] <- rev
  session$userData$blockr_viz_table_rev <- revs
  session$sendCustomMessage("blockr-viz-table-data", list(
    id = elem_id,
    rev = rev,
    payload = dt_payload_json(list(kind = "html", html = as.character(tag)))
  ))
  invisible(NULL)
}

# The input as the renderer reads it: the annotated frame, wide.
interactive_table_data <- function(x) {
  fmt_to_wide(as_annotated_df(x))
}

# The <table> alone, without the container.
interactive_table_body <- function(d, drill = TRUE, drill_target = NULL,
                                   sortable = TRUE, collapsible = TRUE) {
  structured <- dt_is_structured(d)
  drill <- if (isTRUE(drill)) {
    if (structured) "auto" else names(d)[!startsWith(names(d), ".")][1L]
  } else if (is.character(drill) && length(drill) == 1L && nzchar(drill)) {
    drill
  }
  tag <- dt_table_tag(d, drill = drill, sortable = sortable,
                      collapsible = collapsible)
  if (length(drill) && is.character(drill_target) &&
        length(drill_target) == 1L && nzchar(trimws(drill_target))) {
    tag <- htmltools::tagAppendAttributes(
      tag, `data-dt-ctrl-target` = drill_target
    )
  }
  tag
}
