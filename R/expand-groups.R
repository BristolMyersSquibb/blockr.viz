#' Expand a group column into one copy of each group's rows
#'
#' The board-wide `Group` column (stamped by blockr.pharma) can carry a group
#' definition in its `blockr_groups` attribute:
#' `list(groups = <named list: group name -> raw members>, overlap = <lgl>)`.
#' The data keeps one row per subject; this function makes the copies a
#' consumer that splits by the column needs, at the last moment.
#'
#' * No `blockr_groups` attribute: `data` is returned unchanged.
#' * Partition (`overlap = FALSE`): the column already holds group names. Rows
#'   whose value is not a group name (`NA` for a dropped level) are removed,
#'   and the column becomes a factor in group order.
#' * Overlap (`overlap = TRUE`): the column holds the raw level. Each group
#'   gets one copy of its member rows with the column set to the group name,
#'   the copies are stacked in group order, and the column becomes a factor in
#'   group order. A subject in two groups appears twice.
#'
#' The result keeps every data-frame attribute and every attribute of the
#' other columns. The group column keeps its `blockr_source` and `label`, and
#' its `blockr_groups` becomes the identity partition (each name maps to
#' itself), so calling `expand_groups()` a second time changes nothing. The
#' definition that was applied is kept in `blockr_groups_expanded`, so drill
#' claims and colours can still map a group back to its members.
#'
#' Use it only in code that splits by the column. Anything that counts across
#' groups (a total, an overall N) must read the unexpanded data.
#'
#' @param data A data frame or tibble.
#' @param col Name of the group column.
#'
#' @return `data`, expanded as described above.
#'
#' @examples
#' d <- data.frame(id = 1:4, arm = c("Pbo", "Low", "High", "High"))
#' d$Group <- d$arm
#' attr(d$Group, "blockr_source") <- "arm"
#' attr(d$Group, "blockr_groups") <- list(
#'   groups = list(All = c("Low", "High"), Pbo = "Pbo", High = "High"),
#'   overlap = TRUE
#' )
#' table(expand_groups(d)$Group)
#'
#' @export
expand_groups <- function(data, col = "Group") {

  stopifnot(is.data.frame(data), is.character(col), length(col) == 1L)

  if (!col %in% names(data)) {
    return(data)
  }

  x <- data[[col]]
  def <- group_def(x)

  if (is.null(def)) {
    return(data)
  }

  if (!group_expand_needed(x)) {
    return(data)
  }

  nms <- names(def$groups)

  raw <- as.character(x)

  if (isTRUE(def$overlap)) {
    # A group with no members is the all-subjects group.
    idx <- lapply(def$groups, function(m) {
      if (length(m)) which(raw %in% m) else which(!is.na(raw))
    })
    rows <- unlist(idx, use.names = FALSE)
    lab <- rep(nms, lengths(idx))
  } else {
    rows <- which(raw %in% nms)
    lab <- raw[rows]
  }

  # dplyr_row_slice() keeps column attributes (labels, provenance) and the
  # data-frame attributes (filter trail, column kinds); base `[` drops the
  # former. It also resets base row names, which would otherwise read "1.1".
  out <- dplyr::dplyr_row_slice(data, rows)

  keep <- attributes(x)
  keep <- keep[setdiff(names(keep), c("class", "levels", "names", "dim",
                                      "dimnames", "blockr_groups"))]

  new <- factor(lab, levels = nms)
  for (nm in names(keep)) {
    attr(new, nm) <- keep[[nm]]
  }
  attr(new, "blockr_groups") <- list(
    groups = stats::setNames(as.list(nms), nms),
    overlap = FALSE
  )
  # The definition that was applied, kept for the code that has to map a
  # group name back to raw members after the fact (drill claims, colours).
  if (is.null(attr(new, "blockr_groups_expanded", exact = TRUE))) {
    attr(new, "blockr_groups_expanded") <- def
  }

  out[[col]] <- new

  # Some data-frame subclasses rebuild their attributes on `[[<-`; put back
  # anything the column assignment dropped.
  da <- attributes(data)
  for (nm in setdiff(names(da), c("names", "row.names", "class"))) {
    if (is.null(attr(out, nm, exact = TRUE))) {
      attr(out, nm) <- da[[nm]]
    }
  }

  out
}

# The column's group definition, validated, or NULL. A definition is a list
# with a named, non-empty `groups` list and a logical `overlap`.
group_def <- function(x) {
  def <- attr(x, "blockr_groups", exact = TRUE)
  if (!is.list(def) || !is.list(def$groups) || !length(def$groups)) {
    return(NULL)
  }
  nms <- names(def$groups)
  if (is.null(nms) || any(!nzchar(nms))) {
    return(NULL)
  }
  list(
    groups = lapply(def$groups, function(m) as.character(unlist(m))),
    overlap = isTRUE(def$overlap)
  )
}

# The definition the column's DATA was built with: what expand_groups()
# applied when the column was expanded, the current one otherwise.
group_def_origin <- function(x) {
  orig <- attr(x, "blockr_groups_expanded", exact = TRUE)
  if (is.list(orig)) {
    return(group_def(structure(list(), blockr_groups = orig)))
  }
  group_def(x)
}

# Would expand_groups() change anything? No for a column without a
# definition, and no for one already expanded: an identity partition stored
# as a factor in group order with no missing values.
group_expand_needed <- function(x) {
  def <- group_def(x)
  if (is.null(def)) {
    return(FALSE)
  }
  !(group_def_is_identity(def) && is.factor(x) &&
      identical(levels(x), names(def$groups)) && !anyNA(x))
}

group_def_is_identity <- function(def) {
  !isTRUE(def$overlap) && all(vapply(
    names(def$groups),
    function(g) identical(def$groups[[g]], g),
    logical(1L)
  ))
}

# Does the column carry a group definition at all (before or after expansion)?
has_group_def <- function(x) {
  !is.null(group_def_origin(x))
}

# The values a column holds for the given group names. `origin = FALSE`: the
# column as it is now (an unexpanded overlap column holds raw members, an
# expanded or partition column holds names). `origin = TRUE`: the column as
# the data upstream holds it, which is what a filter on another block sees.
# Values that are not group names pass through unchanged.
group_column_values <- function(x, vals, origin = FALSE) {
  vals <- as.character(unlist(vals))
  def <- if (isTRUE(origin)) group_def_origin(x) else group_def(x)
  if (is.null(def) || !isTRUE(def$overlap)) {
    return(vals)
  }
  # A group with no members is the all-subjects group: every raw level. An
  # unexpanded overlap column holds raw levels, so it can say which; an
  # expanded one holds names, so the members of the other groups stand in.
  all_lv <- if (isTRUE(group_def(x)$overlap)) {
    lv <- unique(as.character(x))
    lv[!is.na(lv)]
  } else {
    unique(unlist(def$groups, use.names = FALSE))
  }
  unique(unlist(lapply(vals, function(v) {
    if (!v %in% names(def$groups)) {
      return(v)
    }
    m <- def$groups[[v]]
    if (length(m)) m else all_lv
  }), use.names = FALSE))
}

# Expand `data` by every column in `cols` that carries a group definition.
# `cols` are the columns a chart splits by (group, colour, facet, ...); a
# column bound to no such role is never expanded, so a chart that does not
# split by the group column draws one row per subject as before.
expand_role_groups <- function(data, cols) {
  if (!is.data.frame(data)) {
    return(data)
  }
  cols <- unique(as.character(unlist(cols)))
  cols <- cols[!is.na(cols) & nzchar(cols) & cols %in% names(data)]
  for (col in cols) {
    if (!is.null(group_def(data[[col]]))) {
      data <- expand_groups(data, col)
    }
  }
  data
}

# Pools that should borrow a member's colour: group name -> the raw level
# whose colour it takes. A group that is a raw level under its own name keeps
# its own colour. The member used is the first one that is not also shown as
# a group of its own (All Xanomeline = Low + High next to High takes Low's
# colour), falling back to the first member.
group_pool_colour_levels <- function(x) {
  def <- group_def_origin(x)
  if (is.null(def)) {
    return(character())
  }
  nms <- names(def$groups)
  out <- character()
  for (g in nms) {
    m <- def$groups[[g]]
    if (!length(m) || identical(m, g)) {
      next
    }
    free <- setdiff(m, nms)
    out[[g]] <- if (length(free)) free[[1L]] else m[[1L]]
  }
  out
}

# The columns a chart splits its rows by, from its role bindings. Value roles
# (value, the measure) are never categorical splits; x and y are listed
# because a categorical axis is a split too, and a numeric column never
# carries a group definition.
chart_split_roles <- function(group = NULL, color = NULL, facet = NULL,
                              series = NULL, x = NULL, y = NULL) {
  out <- as.character(unlist(list(group, color, facet, series, x, y)))
  unique(out[!is.na(out) & nzchar(out)])
}

# The members a block's own click filter should match, for a click on a
# group of an OVERLAP definition. The block's input holds raw levels there, so
# a filter `Group == "All Xanomeline"` would match nothing; the emitted
# expression has to be `Group %in% c(<members>)`, with the members written in
# as literals. They are read off the input's `blockr_groups` attribute.
#
# Returns a function `(col, vals)` giving the member vector to filter on, or
# NULL when the click needs no translation (no definition, a partition, or
# the input not seen yet). Backed by a reactiveVal that only moves when the
# answer does, so the emitted expression does not follow every data update.
# The answer is keyed by the click it was computed for: while the input is
# unavailable (a hidden panel) the last answer holds, and it is never applied
# to a different click.
dd_group_filter_members <- function(r_column, r_values, r_data,
                                    r_active = function() TRUE) {
  rv <- shiny::reactiveVal(NULL)
  set <- function(v) {
    if (!identical(shiny::isolate(rv()), v)) rv(v)
  }
  shiny::observe({
    col <- as.character(unlist(r_column()))
    vals <- as.character(unlist(r_values()))
    if (!isTRUE(r_active()) || length(col) != 1L || !length(vals)) {
      set(NULL)
      return()
    }
    d <- tryCatch(r_data(), error = function(e) NULL)
    if (!is.data.frame(d) || !col %in% names(d)) {
      return()
    }
    x <- d[[col]]
    set(list(
      col = col, vals = vals,
      members = if (isTRUE(group_def(x)$overlap)) group_column_values(x, vals)
    ))
  })
  function(col, vals) {
    cur <- rv()
    col <- as.character(unlist(col))
    vals <- as.character(unlist(vals))
    if (is.null(cur) || !identical(cur$col, col) ||
          !identical(cur$vals, vals) || !length(cur$members)) {
      return(NULL)
    }
    cur$members
  }
}
