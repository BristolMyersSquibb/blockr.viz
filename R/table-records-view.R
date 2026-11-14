# The table block's data view: the records behind the table, under it, as
# the chart's "Show as table" shows its rows (R/chart-data-view.R). A prepare
# script hands them along with the frame it returns (viz_records()); the
# patient timeline hands its ADAE records. Without records the block has no
# data view and no table tool.
#
# The records narrow to the table's click: each clicked column the records
# carry filters them, the others are left out (a timeline's Row is not an
# ADAE column, its USUBJID is). They are drawn by the table view's records
# layout, so they sort and search, and the download menu's Data section
# writes them (viz_data_downloads()).

#' Hand records to a table block's data view
#'
#' Called at the end of a table block's prepare script (or a function it
#' calls) on the frame the script returns. The block then offers a table
#' tool, left of its download button, that opens `records` under the table,
#' narrowed to the rows a click selects: each clicked column that `records`
#' carries filters them. The download menu's Data section writes them as
#' CSV and Excel.
#'
#' @param x The frame the prepare script returns.
#' @param records A data frame, or `NULL` for none.
#' @param label What the records are, for the view and the download menu
#'   ("Adverse events"). `NULL`: "Records".
#' @return `x`, with the records attached.
#' @examples
#' rows <- data.frame(USUBJID = c("01", "02"), n = c(2L, 1L))
#' ae <- data.frame(USUBJID = c("01", "01", "02"),
#'                  AETERM = c("HEADACHE", "NAUSEA", "RASH"))
#' x <- viz_records(rows, ae, "Adverse events")
#' @export
viz_records <- function(x, records, label = NULL) {
  stopifnot(is.data.frame(x), is.null(records) || is.data.frame(records),
            is.null(label) || (is.character(label) && length(label) == 1L))
  attr(x, "viz_records") <- if (!is.null(records)) {
    list(data = records, label = label %||% "Records")
  }
  x
}

# The records a frame carries, `list(data, label)`, or NULL.
records_of <- function(d) {
  r <- if (is.data.frame(d)) attr(d, "viz_records", exact = TRUE)
  if (is.list(r) && is.data.frame(r$data)) r
}

# The clicked columns the records carry, as the click holds them.
records_filters <- function(records, filters) {
  filters[intersect(names(filters), names(records))]
}

# The records the click selects: every filter on a column they carry. A
# missing value (NA in the click) matches NA and the empty string.
records_narrow <- function(records, filters) {
  f <- records_filters(records, filters)
  keep <- rep(TRUE, nrow(records))
  for (col in names(f)) {
    vals <- f[[col]]
    v <- as.character(records[[col]])
    hit <- v %in% vals[!is.na(vals)]
    if (anyNA(vals)) hit <- hit | is.na(v) | v == ""
    keep <- keep & hit
  }
  out <- records[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' The table block's data view
#'
#' @param kit The table's kit.
#' @param src The table's sources (table_sources()).
#' @return A list: `tool()`, whether the block has records; `derived(msg)`,
#'   the payload's `data_view` entry; `tables()` and `shape()`, for the data
#'   downloads (viz_data_downloads()).
#' @noRd
table_records_view <- function(kit, src) {
  recs <- shiny::reactive(records_of(tryCatch(src$rows(),
                                              error = function(e) NULL)))
  # A click sent to a drill target is not held, so it narrows nothing.
  filters <- function() table_selection_groups(kit$state$selection())
  rows <- shiny::reactive({
    r <- recs()
    if (!is.null(r)) records_narrow(r$data, filters())
  })
  narrowed <- function() {
    r <- recs()
    !is.null(r) && length(records_filters(r$data, filters())) > 0L
  }
  table <- viz_data_view_table(search = TRUE)
  list(
    tool = function() !is.null(recs()),
    derived = function(msg) {
      r <- recs()
      if (is.null(r)) {
        return(NULL)
      }
      d <- rows()
      list(tab = "records", label = r$label, rows = nrow(d),
           total = nrow(r$data),
           table = table(d, drill = FALSE, selection = NULL, req = msg$n))
    },
    tables = function() {
      r <- recs()
      list(rows = rows(), total = if (!is.null(r)) nrow(r$data),
           all = if (narrowed()) r$data)
    },
    shape = function() {
      r <- recs()
      if (is.null(r)) {
        return(list(tables = character(), selected = FALSE))
      }
      sel <- narrowed()
      list(tables = c("rows", if (sel) "all"), selected = sel,
           label = r$label)
    }
  )
}
