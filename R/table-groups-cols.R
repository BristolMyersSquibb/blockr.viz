# The groups view's TableCol parts: each column's text, its mark and its
# axis.

# Text per row, `NA` (null on the wire) where the cell is empty.
table_groups_text <- function(x) {
  x <- as.character(x)
  x[!nzchar(x)] <- NA_character_
  x
}

# The mark of a column: the cell model's own fields less what the TableCol
# holds itself (`disp`, `v`, `pct`, tile paint, claims), per-row vectors as
# arrays. Flags travel only when true, empty references not at all. A number
# or text has no mark.
table_groups_mark <- function(c) {
  if (identical(c$kind, "num")) {
    return(NULL)
  }
  g <- c[setdiff(names(c), c("fkey", "scol", "disp", "v", "pct", "bg", "fg"))]
  if (!any(c$child %in% TRUE)) g$child <- NULL
  for (k in c("rf", "rc", "rby", "rbh")) {
    if (length(g[[k]]) && is.na(g[[k]])) g[[k]] <- NULL
  }
  if (!isTRUE(c$multi)) g$ab <- NULL
  if (!length(c$tt)) g$tt <- NULL
  labelled <- !is.null(c$disp) || isTRUE(c$multi) ||
    (identical(c$kind, "barsplit") && !is.null(c$ldisp))
  if (!labelled) g$dw <- NULL
  table_groups_wire(
    groups_drop_null(table_groups_pack(table_groups_fence(g), c$kind,
                                       top = TRUE)),
    c$kind
  )
}

# The names the browser reads for the cell model's short keys, by mark kind.
# The cell model and the PowerPoint painter keep the short ones.
TABLE_GROUPS_KEYS <- list(
  all = c(dw = "valueWidth", tt = "hover", nn = "count", lv = "lanes",
          rf = "reference", ab = "fromTo", r = "raw"),
  bar = c(w = "width", child = "sub"),
  bardiv = c(w = "width", pos = "positive"),
  barsplit = c(seg = "segWidths", segv = "segValues", slab = "segLabels",
               sink = "segInk", ldisp = "levelValues"),
  box = c(wl = "whiskerLow", w1 = "whiskerLowWidth", bl = "boxLeft",
          bw = "boxWidth", bc = "median", b2 = "boxRight",
          w2 = "whiskerHighWidth", wh = "whiskerHigh"),
  pointrange = c(c = "center", l = "rangeLeft", rw = "rangeWidth",
                 ol = "fenceLeft", ow = "fenceWidth"),
  pair = c(a = "from", b = "to", l = "linkLeft", w = "linkWidth",
           bl = "bandLeft", bw = "bandWidth"),
  sparkline = c(pl = "line", bd = "band", dx = "dotX", dy = "dotY",
                xs = "xValues", ys = "yValues", rc = "refY",
                rby = "refBandY", rbh = "refBandHeight"),
  interval = c(d0 = "domainStart", d1 = "domainEnd", dd = "dates",
               lg = "large")
)
# The raw statistics a hover card prints (`r`).
TABLE_GROUPS_RAW <- c(bc = "center", bl = "low", bh = "high",
                      wl = "whiskerLow", wh = "whiskerHigh")

table_groups_wire <- function(g, kind) {
  rename <- function(nm, map) {
    hit <- nm %in% names(map)
    nm[hit] <- map[nm[hit]]
    nm
  }
  if (!is.null(g$r)) names(g$r) <- rename(names(g$r), TABLE_GROUPS_RAW)
  if (!is.null(g$lv)) g$lv <- lapply(g$lv, table_groups_wire, kind = kind)
  names(g) <- rename(names(g),
                     c(TABLE_GROUPS_KEYS$all, TABLE_GROUPS_KEYS[[kind]]))
  g
}

# A point range's fence band only where it has an outer range, per level.
table_groups_fence <- function(g) {
  if (!is.null(g$ow) && all(is.na(g$ow))) g[c("ol", "ow")] <- NULL
  if (!is.null(g$lv)) g$lv <- lapply(g$lv, table_groups_fence)
  g
}

# Column-level fields stay scalars (a bar's `fill` is its colour, a pair's is
# one per row); everything else is per row, an array also at one row.
TABLE_GROUPS_SCALARS <- c("kind", "mode", "rf", "d0", "d1", "dw", "rc", "rby",
                          "rbh", "tt")
TABLE_GROUPS_FLAGS <- c("multi", "bare", "dd", "lg")

table_groups_pack <- function(g, kind, top) {
  out <- list()
  for (nm in names(g)) {
    x <- g[[nm]]
    if (is.null(x) || (nm %in% TABLE_GROUPS_FLAGS && !isTRUE(x))) next
    out[[nm]] <- if (nm %in% TABLE_GROUPS_FLAGS ||
                       (top && nm %in% TABLE_GROUPS_SCALARS) ||
                       (top && nm == "fill" && identical(kind, "bar"))) {
      x
    } else if (nm == "segs") {
      lapply(x, function(ss) if (length(ss)) ss else groups_arr(list()))
    } else if (nm == "lv") {
      lapply(x, table_groups_pack, kind = kind, top = FALSE)
    } else if (is.list(x)) {
      lapply(x, groups_arr)
    } else {
      groups_arr(x)
    }
  }
  out
}

# The column axis the header draws: tick positions (percent of the cell) and
# labels, with the "%" already on the last label. `foot` marks a large swimlane, which repeats the axis under the
# table. `NULL` without a scale or with fewer than two ticks.
table_groups_axis <- function(p, prep) {
  dom <- cells_axis_domain(p, prep)
  if (is.null(dom)) {
    return(NULL)
  }
  t <- if (isTRUE(dom$date)) {
    cells_axis_date_ticks(dom$d0, dom$d1)
  } else {
    at <- cells_axis_ticks(
      dom$d0, dom$d1, k = if (identical(p$kind, "interval")) 7L else 4L
    )
    list(at = at, labels = groups_fmt(at))
  }
  if (length(t$at) < 2L) {
    return(NULL)
  }
  if (isTRUE(dom$pct)) {
    last <- length(t$labels)
    t$labels[[last]] <- paste0(t$labels[[last]], "%")
  }
  pos <- round((t$at - dom$d0) / (dom$d1 - dom$d0) * 100, 2L)
  table_groups_compact(list(
    at = groups_arr(pos), labels = groups_arr(t$labels),
    foot = if (identical(p$kind, "interval") && identical(p$size, "lg")) TRUE
  ))
}

# A vector as a JSON array, also at length one.
groups_arr <- function(x) {
  I(unname(if (is.factor(x)) as.character(x) else x))
}

groups_drop_null <- function(x) {
  if (!is.list(x)) return(x)
  x <- x[!vapply(x, is.null, logical(1L))]
  lapply(x, function(e) {
    if (is.list(e) && !inherits(e, "AsIs")) groups_drop_null(e) else e
  })
}
