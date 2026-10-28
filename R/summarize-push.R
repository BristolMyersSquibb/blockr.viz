#' @noRd
summarize_arr <- function(x) I(unname(x))

# Constant-false vectors are omitted (4-5 KB per column at 790 rows); the
# assembler reads absent as all-false.
#' @noRd
summarize_arr_if <- function(x) {
  if (any(x, na.rm = TRUE)) summarize_arr(x) else NULL
}

#' @noRd
summarize_payload_col <- function(c) {
  arr <- summarize_arr
  out <- list(kind = c$kind)
  # What a click on this column's cell adds to the row's claim.
  out$fkey <- c$fkey
  out$scol <- c$scol
  if (identical(c$kind, "tile")) {
    out$bg <- arr(c$bg)
    out$fg <- arr(c$fg)
    out$disp <- arr(as.character(c$disp))
    out$v <- arr(c$v)
    if (length(c$tt)) out$tt <- c$tt
    return(out)
  }
  if (identical(c$kind, "num")) {
    out$disp <- arr(as.character(c$disp))
    if (!is.null(c$pct)) out$pct <- arr(as.character(c$pct))
    if (isTRUE(c$text)) out$text <- TRUE
  } else if (identical(c$kind, "barsplit")) {
    out <- summarize_payload_split(out, c)
  } else if (identical(c$kind, "bardiv")) {
    out$w <- arr(c$w)
    out$pos <- arr(c$pos)
  } else if (c$kind %in% c("box", "pointrange")) {
    out <- summarize_payload_lane(out, c)
  } else if (identical(c$kind, "pair")) {
    out <- summarize_payload_pair(out, c)
  } else if (identical(c$kind, "interval")) {
    out <- summarize_payload_interval(out, c)
  } else if (identical(c$kind, "sparkline")) {
    out <- summarize_payload_spark(out, c)
  } else {
    out$w <- arr(c$w)
    out$sub <- summarize_arr_if(c$sub)
    if (!is.null(c$fill)) out$fill <- c$fill
  }
  # In-bar value label: display strings plus the column's one slot width.
  if (!identical(c$kind, "num") && !is.null(c$disp)) {
    out$disp <- arr(as.character(c$disp))
    if (!is.null(c$pct)) out$pct <- arr(as.character(c$pct))
    out$dw <- c$dw
  } else if (isTRUE(c$multi) && !is.null(c$dw)) {
    # A colour-split column's labels ride on its levels.
    out$dw <- c$dw
  }
  out$v <- arr(c$v)
  if (length(c$tt)) out$tt <- c$tt
  out
}

#' @noRd
summarize_payload_split <- function(out, c) {
  arr <- summarize_arr
  out$mode <- c$mode
  out$names <- arr(as.character(c$names))
  out$fills <- arr(as.character(c$fills))
  out$seg <- lapply(c$seg, arr)
  out$segv <- lapply(c$segv, arr)
  if (!is.null(c$ldisp)) {
    out$ldisp <- lapply(c$ldisp, function(x) arr(as.character(x)))
    out$dw <- c$dw
  }
  if (!is.null(c$slab)) {
    out$slab <- lapply(c$slab, function(x) arr(as.character(x)))
    out$sink <- arr(as.character(c$sink))
  }
  out
}

# Box and point range: one geometry set, or one per level for a colour split.
#' @noRd
summarize_payload_lane <- function(out, c) {
  arr <- summarize_arr
  if (isTRUE(c$multi)) {
    out$multi <- TRUE
    out$levels <- arr(as.character(c$levels))
    out$fills <- arr(as.character(c$fills))
    out$lv <- lapply(c$lv, function(g) summarize_payload_lane_pack(g, c))
  } else {
    out <- c(out, summarize_payload_lane_pack(c, c))
  }
  out
}

#' @noRd
summarize_payload_lane_pack <- function(g, c) {
  arr <- summarize_arr
  geom <- if (identical(c$kind, "box")) {
    c("wl", "w1", "bl", "bw", "bc", "b2", "w2", "wh", "nn")
  } else {
    c("c", "l", "rw", "nn")
  }
  o <- list()
  for (nm in geom) o[[nm]] <- arr(g[[nm]])
  # The dot style's fence band, only when the column has an outer range.
  if (!identical(c$kind, "box") && any(!is.na(g$ow))) {
    o$ol <- arr(g$ol)
    o$ow <- arr(g$ow)
  }
  o$tip <- arr(as.character(g$tip))
  if (isTRUE(c$multi) && !is.null(g$disp)) {
    o$disp <- arr(as.character(g$disp))
  }
  if (length(g$r)) o$r <- lapply(g$r, arr)
  # Column-level, only when true; absent means the mark draws its own rail.
  if (isTRUE(g$bare)) o$bare <- TRUE
  o
}

#' @noRd
summarize_payload_pair <- function(out, c) {
  arr <- summarize_arr
  if (!is.na(c$rf)) out$rf <- c$rf
  if (isTRUE(c$multi)) {
    out$multi <- TRUE
    out$levels <- arr(as.character(c$levels))
    out$fills <- arr(as.character(c$fills))
    out$lv <- lapply(c$lv, function(g) summarize_payload_pair_pack(g, c))
  } else {
    out <- c(out, summarize_payload_pair_pack(c, c))
  }
  out
}

#' @noRd
summarize_payload_pair_pack <- function(g, c) {
  arr <- summarize_arr
  o <- list()
  for (nm in c("a", "b", "l", "w", "bl", "bw")) o[[nm]] <- arr(g[[nm]])
  o$fill <- arr(g$fill)
  o$dash <- arr(g$dash)
  o$open <- arr(g$open)
  o$tip <- arr(as.character(g$tip))
  if (isTRUE(c$multi) && !is.null(g$disp)) {
    o$disp <- arr(as.character(g$disp))
  }
  if (isTRUE(c$multi)) o$ab <- arr(as.character(g$ab))
  o
}

#' @noRd
summarize_payload_interval <- function(out, c) {
  arr <- summarize_arr
  # Keep length-one lists as arrays (the auto_unbox trap).
  out$segs <- lapply(c$segs, function(ss) if (length(ss)) ss else arr(list()))
  out$tips <- lapply(c$tips, function(tt) arr(as.character(tt)))
  out$fills <- arr(as.character(c$fills))
  out$d0 <- c$d0
  out$d1 <- c$d1
  if (isTRUE(c$dd)) out$dd <- TRUE
  if (isTRUE(c$lg)) out$lg <- TRUE
  out
}

#' @noRd
summarize_payload_spark <- function(out, c) {
  arr <- summarize_arr
  out$pl <- arr(as.character(c$pl))
  out$bd <- arr(c$bd)
  out$dx <- arr(c$dx)
  out$dy <- arr(c$dy)
  out$xs <- arr(as.character(c$xs))
  out$ys <- arr(as.character(c$ys))
  out$nn <- arr(c$nn)
  # Column-level reference coordinates; NA drops to absent.
  if (!is.na(c$rc)) out$rc <- c$rc
  if (!is.na(c$rby)) {
    out$rby <- c$rby
    out$rbh <- c$rbh
  }
  out
}

#' @noRd
summarize_drop_null <- function(x) {
  if (!is.list(x)) return(x)
  x <- x[!vapply(x, is.null, logical(1L))]
  lapply(x, function(e) {
    if (is.list(e) && !inherits(e, "AsIs")) summarize_drop_null(e) else e
  })
}
