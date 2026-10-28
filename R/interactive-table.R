#' Interactive table
#'
#' Draws a table the way [new_table_block()] draws it on screen, for use in
#' another block's UI. `x` is anything [as_annotated_df()] accepts: a plain
#' data frame, an annotated data frame (shown with its sections and spanner
#' headers), or a table object such as a composer table. [static_table()] is
#' the print counterpart.
#'
#' The table searches, sorts and collapses in the browser. A row click is
#' sent to the server as `input[[paste0(elem_id, "_select")]]`, the
#' selection the table block holds, so the calling block handles the drill.
#'
#' `update_interactive_table()` replaces the table inside a drawn
#' `interactive_table()`, so the search text and the scroll position stay.
#' Draw it with `x = NULL` and fill it from the server.
#'
#' @param x A data frame, an object with an [as_annotated_df()] method, or
#'   `NULL` for an empty table. `update_interactive_table()` also takes a
#'   string or an htmltools tag, whose text is shown instead of a table.
#' @param drill `TRUE` makes rows clickable: an annotated table claims the
#'   clicked row, a plain one its first column. A column name picks the
#'   column of a plain table. `FALSE` turns clicks off.
#' @param drill_target Id of the block a click is sent to, or `NULL`. With a
#'   target, a click is an event (a flash and a send) rather than a
#'   selection that stays highlighted.
#' @param sortable Can column headers sort the rows?
#' @param collapsible Can section headers collapse?
#' @param search Draw a search box above the table?
#' @param max_height CSS height limit of the scroll box, or `NULL` to let
#'   the table grow with the page.
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
                              search = TRUE, max_height = "600px",
                              elem_id = NULL) {
  payload <- interactive_table_payload(x, drill, drill_target, sortable,
                                       collapsible, search)
  # A "</script>" inside a label must not end the block.
  json <- as.character(jsonlite::toJSON(
    payload, auto_unbox = TRUE, null = "null", na = "null", digits = NA,
    json_verbatim = TRUE
  ))
  json <- gsub("</", "<\\/", json, fixed = TRUE)
  id <- elem_id %||% paste0("viz-table-", substr(rlang::hash(json), 1L, 12L))
  htmltools::tagList(
    table_view_dep(),
    if (!is.null(max_height)) {
      htmltools::tags$style(htmltools::HTML(sprintf(
        "#%s .blockr-table-wrapper { max-height: %s; }", id, max_height
      )))
    },
    htmltools::div(
      class = "blockr-viz-static", id = id, `data-viz-view` = "table",
      `data-viz-gear` = "off", `data-viz-still` = "off",
      htmltools::tags$script(type = "application/json", htmltools::HTML(json))
    )
  )
}

#' @rdname interactive_table
#' @param session The Shiny session (a module session works).
#' @export
update_interactive_table <- function(session, elem_id, x, drill = TRUE,
                                     drill_target = NULL, sortable = TRUE,
                                     collapsible = TRUE) {
  payload <- interactive_table_payload(x, drill, drill_target, sortable,
                                       collapsible, search = TRUE)
  # The view skips a payload whose revision it has drawn, so each send
  # counts up per table.
  revs <- session$userData$blockr_viz_table_rev %||% list()
  rev <- (revs[[elem_id]] %||% 0L) + 1L
  revs[[elem_id]] <- rev
  session$userData$blockr_viz_table_rev <- revs
  payload$state_rev <- rev
  payload$data_rev <- rev
  session$sendCustomMessage("blockr-viz-static",
                            list(id = elem_id, payload = payload))
  invisible(NULL)
}

# The payload of the table view's records layout: the table of `x`, or a
# line to show instead.
interactive_table_payload <- function(x, drill, drill_target, sortable,
                                      collapsible, search) {
  v <- list(layout = "records", sortable = isTRUE(sortable),
            collapsible = isTRUE(collapsible), search = isTRUE(search),
            download = FALSE)
  built <- if (is.null(x)) {
    list(empty = list(kind = "no_data", message = ""))
  } else if (is.character(x) ||
               inherits(x, c("shiny.tag", "shiny.tag.list", "html"))) {
    msg <- gsub("<[^>]+>", "", paste(as.character(x), collapse = " "))
    list(empty = list(kind = "message", message = trimws(msg)))
  } else {
    d <- fmt_to_wide(as_annotated_df(x))
    v$drill <- interactive_table_drill(drill, d)
    target <- is.character(drill_target) && length(drill_target) == 1L &&
      nzchar(trimws(drill_target))
    if (length(v$drill) && target) v$drill_target <- drill_target
    table_frame_view(viz_check(viz_block_spec("table"), v), d,
                     list(scale_map = function() NULL), "records")
  }
  list(
    state_rev = 1L, data_rev = 1L, columns = list(),
    config = viz_wire(viz_block_spec("table"),
                      viz_check(viz_block_spec("table"), v)),
    derived = if (!is.null(built$empty)) {
      list(empty = built$empty)
    } else {
      structure(list(), names = character())
    },
    data = built$model %||% structure(list(), names = character())
  )
}

# `TRUE`: an annotated table claims its row ("auto"), a plain one its first
# column. A string names the column. Anything else: no drill.
interactive_table_drill <- function(drill, d) {
  if (isTRUE(drill)) {
    if (dt_is_structured(d)) "auto" else names(d)[!startsWith(names(d), ".")][1L]
  } else if (is.character(drill) && length(drill) == 1L && nzchar(drill)) {
    drill
  }
}
