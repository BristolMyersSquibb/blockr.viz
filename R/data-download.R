# The download menu's Data section: the data view's tables as files. A chart
# gives its Summary (one row per mark) and its Rows (narrowed by what the chart
# shows as selected), each as Excel and CSV; a chart that draws rows has no
# Summary. Under a selection the Rows entry reads "Selected rows" and an "All
# rows" entry joins it. The menu names each table with its count, as the data
# view's tabs do, so what a file holds does not depend on the open tab.
#
# The tables are built when a file or the counts are asked for, not for the
# menu: the payload lists the entries from `shape()`, and the browser asks
# for the counts when the reader reaches for the menu (`dl_counts`; the
# answer is the `blockr-viz:counts` message, inst/js/core/download.js).

# The formats a table is written in: an Excel sheet under the exhibit's titles
# with its footer below, or a CSV of the header row and the values.
VIZ_DATA_FORMATS <- list(
  xlsx = list(needs = "openxlsx", write = function(df, file, sheet, t) {
    write_data_xlsx(df, file, sheet, t)
  }),
  csv = list(needs = character(), write = function(df, file, sheet, t) {
    utils::write.csv(df, file, row.names = FALSE, na = "",
                     fileEncoding = "UTF-8")
  })
)

VIZ_DATA_TABLES <- c(summary = "Summary", rows = "Rows", all = "All rows")

#' A block's data downloads
#'
#' Registers `dl_data_<table>_<format>` for every table and format, and
#' returns the menu entries for the tables `shape()` names.
#'
#' @param tables A function of no arguments returning `list(summary, rows,
#'   total, all)`: the two frames, either NULL, the row count with nothing
#'   selected, and under a selection the rows with nothing selected, else
#'   NULL (chart_data_view()).
#' @param shape A function of no arguments returning `list(tables,
#'   selected)`: the names of the tables `tables()` would hold, and whether a
#'   selection narrows the rows. It builds none of them.
#' @param exhibit The block's exhibit function, for the titles, the footer and
#'   the file name.
#' @param session,filename As viz_downloads().
#' @return A function of no arguments returning the entries, `list(id, label,
#'   ext, section, ask)`.
#' @noRd
viz_data_downloads <- function(tables, shape, exhibit, session, filename) {
  stopifnot(is.function(shape))
  formats <- Filter(function(f) dt_pkg_installed(f$needs), VIZ_DATA_FORMATS)
  for (tbl in names(VIZ_DATA_TABLES)) {
    for (ext in names(formats)) {
      viz_data_download_handler(tbl, ext, formats[[ext]], tables, exhibit,
                                session, filename)
    }
  }
  entry_id <- function(tbl, ext) {
    session$ns(paste0("dl_data_", tbl, "_", ext))
  }
  shiny::observeEvent(session$input$dl_counts, {
    tb <- tryCatch(tables(), error = function(e) NULL)
    counts <- list()
    for (tbl in names(VIZ_DATA_TABLES)) {
      df <- tb[[tbl]]
      if (!is.data.frame(df)) next
      count <- data_download_count(nrow(df), if (tbl == "rows") tb$total)
      for (ext in names(formats)) counts[[entry_id(tbl, ext)]] <- count
    }
    session$sendCustomMessage("blockr-viz:counts", list(counts = counts))
  })
  ask <- session$ns("dl_counts")
  function() {
    sh <- tryCatch(shape(), error = function(e) NULL)
    out <- list()
    for (tbl in intersect(names(VIZ_DATA_TABLES), sh$tables)) {
      label <- if (isTRUE(sh$selected) && tbl == "rows") {
        "Selected rows"
      } else {
        VIZ_DATA_TABLES[[tbl]]
      }
      for (ext in names(formats)) {
        out[[length(out) + 1L]] <- list(
          id = entry_id(tbl, ext), label = label, ext = ext,
          section = "Data", ask = ask
        )
      }
    }
    out
  }
}

# "6 rows", "40 of 254": the count the data view's tab shows.
data_download_count <- function(n, total = NULL) {
  if (is.numeric(total) && length(total) == 1L && !is.na(total) &&
        total != n) {
    return(paste(n, "of", total))
  }
  paste(n, if (n == 1L) "row" else "rows")
}

viz_data_download_handler <- function(tbl, ext, fmt, tables, exhibit, session,
                                      filename) {
  force(fmt)
  id <- paste0("dl_data_", tbl, "_", ext)
  frame <- function() {
    df <- tables()[[tbl]]
    if (!is.data.frame(df)) {
      stop("the chart has no ", VIZ_DATA_TABLES[[tbl]], " table",
           call. = FALSE)
    }
    if (identical(tbl, "summary")) chart_summary_flat(df) else df
  }
  session$output[[id]] <- shiny::downloadHandler(
    filename = function() {
      x <- tryCatch(exhibit(), error = function(e) NULL)
      if (!inherits(x, "viz_exhibit")) {
        return(paste0(filename, "_", tbl, ".", ext))
      }
      dl_filename(x$meta$dataset %||% dl_dataset(x$data), x$titles$title,
                  filename, ext, what = tbl)
    },
    content = function(file) {
      label <- paste(VIZ_DATA_TABLES[[tbl]], toupper(ext))
      dl_guard(label, {
        x <- exhibit_footed(exhibit(), session)
        fmt$write(frame(), file, VIZ_DATA_TABLES[[tbl]], exhibit_titles(x))
      })
    }
  )
  shiny::outputOptions(session$output, id, suspendWhenHidden = FALSE)
}

#' The Summary as a plain table: every key in its own column, then the
#' numbers. The data view's frame keeps the outer keys as row groups
#' (chart_summary_annotate()); a file wants them as columns.
#' @noRd
chart_summary_flat <- function(s) {
  stats <- s[!startsWith(names(s), ".")]
  rownames(stats) <- NULL
  if (!nrow(s) || !".variable" %in% names(s)) {
    return(stats)
  }
  groups <- grep("^\\.group[0-9]+$", names(s), value = TRUE)
  groups <- groups[order(as.integer(sub("^\\.group", "", groups)))]
  keys <- list()
  for (g in groups) {
    keys[[s[[g]][[1L]]]] <- s[[paste0(g, "_level")]]
  }
  keys[[s$.variable[[1L]]]] <- as.character(s$.label)
  cbind(as.data.frame(keys, check.names = FALSE, stringsAsFactors = FALSE),
        stats)
}

#' One table as an Excel sheet: the titles above, the header with a filter,
#' frozen, the values as they are, the caption (with the footer) below.
#' @noRd
write_data_xlsx <- function(df, file, sheet, titles) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet)
  n_col <- max(1L, ncol(df))
  r <- xlsx_write_titles(wb, sheet, n_col, titles$title, titles$subtitle)
  openxlsx::writeData(
    wb, sheet, df, startRow = r, withFilter = ncol(df) > 0L,
    headerStyle = openxlsx::createStyle(textDecoration = "bold",
                                        border = "bottom")
  )
  openxlsx::freezePane(wb, sheet, firstActiveRow = r + 1L)
  xlsx_write_caption(wb, sheet, titles$caption, r + nrow(df) + 2L, n_col)
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}
