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
#' its `blockr_groups` becomes the identity partition, so a second call changes
#' nothing. The applied definition is kept in `blockr_groups_expanded`, so
#' drill claims and colours can map a group back to its members.
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

  if (is.null(def) || !group_expand_needed(x)) {
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

  # dplyr_row_slice() keeps column and data-frame attributes, which base `[`
  # drops, and resets row names.
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
  # The applied definition, for mapping a group name back to raw members.
  if (is.null(attr(new, "blockr_groups_expanded", exact = TRUE))) {
    attr(new, "blockr_groups_expanded") <- def
  }

  out[[col]] <- new

  # Some data-frame subclasses drop attributes on `[[<-`; restore them.
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

# The definition the column's data was built with: the one expand_groups()
# applied, else the current one.
group_def_origin <- function(x) {
  orig <- attr(x, "blockr_groups_expanded", exact = TRUE)
  if (is.list(orig)) {
    return(group_def(structure(list(), blockr_groups = orig)))
  }
  group_def(x)
}

# Would expand_groups() change anything? Not for a column without a
# definition, nor for one already expanded (an identity partition stored as a
# factor in group order with no missing values).
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
# the data upstream holds it. Values that are not group names pass through.
group_column_values <- function(x, vals, origin = FALSE) {
  vals <- as.character(unlist(vals))
  def <- if (isTRUE(origin)) group_def_origin(x) else group_def(x)
  if (is.null(def) || !isTRUE(def$overlap)) {
    return(vals)
  }
  # A group with no members is the all-subjects group: every raw level. An
  # expanded column holds names, so the members of the other groups stand in.
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
# `cols` are the columns a chart splits by; a column bound to no such role is
# never expanded.
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

# Pools that borrow a member's colour: group name -> the raw level whose
# colour it takes. A group that is a raw level under its own name keeps its
# colour. The member used is the first one not shown as a group of its own
# (All Xanomeline = Low + High next to High takes Low's colour), else the
# first member.
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
# are never splits. x and y are listed because a categorical axis is a split.
chart_split_roles <- function(group = NULL, color = NULL, facet = NULL,
                              series = NULL, x = NULL, y = NULL) {
  out <- as.character(unlist(list(group, color, facet, series, x, y)))
  unique(out[!is.na(out) & nzchar(out)])
}

# The members a block's own click filter should match, for a click on a group
# of an overlap definition. The input holds raw levels there, so the emitted
# expression has to be `Group %in% c(<members>)`, not `Group == "<group>"`.
#
# `r_filters` gives the click filter, a named list column -> values (NA for a
# missing value, which no group holds). Returns a function `(filters)` giving
# a named list column -> members for the columns that need translating, or
# NULL when none does. A reactiveVal that only moves when the answer does
# backs it. The answer is keyed by the filter it was computed for, so it
# holds while the input is unavailable and never applies to another filter.
dd_group_filters_members <- function(r_filters, r_data,
                                     r_active = function() TRUE) {
  rv <- shiny::reactiveVal(NULL)
  set <- function(v) {
    if (!identical(shiny::isolate(rv()), v)) rv(v)
  }
  shiny::observe({
    f <- r_filters()
    if (!isTRUE(r_active()) || !length(f)) {
      set(NULL)
      return()
    }
    d <- tryCatch(r_data(), error = function(e) NULL)
    if (!is.data.frame(d) || !all(names(f) %in% names(d))) {
      return()
    }
    members <- list()
    for (col in names(f)) {
      x <- d[[col]]
      vals <- f[[col]][!is.na(f[[col]])]
      if (length(vals) && isTRUE(group_def(x)$overlap)) {
        members[[col]] <- group_column_values(x, vals)
      }
    }
    set(list(filters = f, members = members))
  })
  function(filters) {
    cur <- rv()
    if (is.null(cur) || !identical(cur$filters, filters) ||
          !length(cur$members)) {
      return(NULL)
    }
    cur$members
  }
}
