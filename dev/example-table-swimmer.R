# An efficacy swimmer plot in the groups table: one row per patient, nested
# in its arm, listing columns beside a swimlane. The swimlane is a `spans`
# summary with its four overlays:
#
#   segments   assessment to next assessment, coloured by the response read
#              at its start ("Not assessed" before the first)
#   ticks      the assessment visits, cut across the segments
#   band_x/_xend  duration of response, first CR/PR to progression or the end
#   event_x/_shape  death, which can sit past the last segment
#   open       still on treatment: an arrow at the lane's end
#
# All of it is one long frame: segment rows, tick rows, band rows and event
# rows, each overlay reading only the rows where its own columns are set.
#
#   Rscript blockr.viz/dev/example-table-swimmer.R [port]
#
# Run from the workspace root. The port defaults to 4010 (the dev container
# holds 3838-3847 on the Mac).

# Package roots: this checkout of blockr.viz (also from a worktree), and the
# workspace above it that holds the other packages.
.self <- grep("^--file=", commandArgs(FALSE), value = TRUE)[1]
.viz <- if (!is.na(.self) && grepl("example-table-swimmer", .self)) {
  normalizePath(file.path(dirname(sub("^--file=", "", .self)), ".."))
} else {
  getOption("swimmer.viz", normalizePath("blockr.viz"))
}
.ws <- .viz
while (!dir.exists(file.path(.ws, "blockr.core")) && dirname(.ws) != .ws) {
  .ws <- dirname(.ws)
}
.port <- as.integer(c(commandArgs(TRUE), Sys.getenv("BLOCKR_PORT"), "4010")[1])

options(shiny.port = .port, shiny.launch.browser = FALSE)
options(blockr.dock_is_locked = FALSE)

pkgload::load_all(file.path(.ws, "blockr.core"))
pkgload::load_all(Sys.getenv("BLOCKR_UI", file.path(.ws, "blockr.ui")))
pkgload::load_all(file.path(.ws, "blockr.dock"))
pkgload::load_all(file.path(.ws, "blockr.dag"))
pkgload::load_all(file.path(.ws, "blockr.theme"))
pkgload::load_all(.viz)

# The mockup's patients: last day, ongoing, first PR, first CR, PD, death.
pts <- data.frame(
  id = c("1001", "1002", "1004", "1006", "1008", "1010", "1013",
         "1003", "1005", "1007", "1009", "1011", "1012", "1014"),
  arm = rep(c("Arm A", "Arm B"), c(7, 7)),
  end = c(448, 392, 330, 270, 210, 150, 84, 365, 301, 238, 182, 126, 98, 63),
  ongoing = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE,
              FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
  pr = c(42, 84, NA, 42, NA, NA, NA, 42, 84, 126, 84, NA, NA, NA),
  cr = c(168, NA, NA, NA, NA, NA, NA, NA, 210, NA, NA, NA, NA, NA),
  pd = c(NA, NA, NA, 252, 189, 126, NA, 294, NA, NA, 168, NA, 84, 42),
  death = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 190, NA, 110, NA),
  stringsAsFactors = FALSE
)

RESP <- c("Not assessed", "CR", "PR", "SD", "PD")

swimmer_frame <- function(pts, every = 42) {
  state_at <- function(p, d) {
    if (!is.na(p$pd) && d >= p$pd) "PD"
    else if (!is.na(p$cr) && d >= p$cr) "CR"
    else if (!is.na(p$pr) && d >= p$pr) "PR"
    else "SD"
  }
  out <- lapply(seq_len(nrow(pts)), function(i) {
    p <- pts[i, ]
    visits <- seq(every, p$end, by = every)
    visits <- visits[visits < p$end]
    cuts <- c(0, visits, p$end)
    seg <- data.frame(
      start = head(cuts, -1), stop = cuts[-1],
      resp = c("Not assessed", vapply(visits, function(d) state_at(p, d), "")),
      visit = NA_real_, dor_from = NA_real_, dor_to = NA_real_,
      event_day = NA_real_, event = NA_character_
    )
    tick <- data.frame(start = NA_real_, stop = NA_real_, resp = NA_character_,
                       visit = visits, dor_from = NA_real_, dor_to = NA_real_,
                       event_day = NA_real_, event = NA_character_)
    first <- suppressWarnings(min(c(p$pr, p$cr), na.rm = TRUE))
    band <- if (is.finite(first)) {
      data.frame(start = NA_real_, stop = NA_real_, resp = NA_character_,
                 visit = NA_real_, dor_from = first,
                 dor_to = if (is.na(p$pd)) p$end else p$pd,
                 event_day = NA_real_, event = NA_character_)
    }
    ev <- if (!is.na(p$death)) {
      data.frame(start = NA_real_, stop = NA_real_, resp = NA_character_,
                 visit = NA_real_, dor_from = NA_real_, dor_to = NA_real_,
                 event_day = p$death, event = "Death")
    }
    d <- rbind(seg, tick, band, ev)
    bor <- if (!is.na(p$cr)) "CR" else if (!is.na(p$pr)) "PR" else
      if (!is.na(p$pd) && p$pd <= every) "PD" else "SD"
    d$USUBJID <- paste0("01-", p$id)
    d$ARM <- p$arm
    d$BOR <- bor
    d$TRTDUR <- p$end
    d$TTR <- if (is.finite(first)) first else NA_real_
    d$DOR <- if (is.finite(first)) (if (is.na(p$pd)) p$end else p$pd) - first
      else NA_real_
    d$ongoing <- c(p$ongoing, rep(NA, nrow(d) - 1L))
    d
  })
  out <- do.call(rbind, out)
  out$resp <- factor(out$resp, levels = RESP)
  # The level order picks the shape: CR filled triangle, PR open triangle,
  # PD cross, Death square. Only Death is drawn here; the colour carries the
  # responses.
  out$event <- factor(out$event, levels = c("CR", "PR", "PD", "Death"))
  out
}

swim <- swimmer_frame(pts)
attr(swim$event_day, "label") <- "Day"

swimmer_summaries <- list(
  list(type = "field", name = "BOR", col = "BOR"),
  list(type = "simple", name = "Days on tx", func = "max", col = "TRTDUR",
       show = "number"),
  list(type = "simple", name = "TTR", func = "max", col = "TTR",
       show = "number"),
  list(type = "simple", name = "DOR", func = "max", col = "DOR",
       show = "number"),
  list(type = "spans", name = "Time on treatment, days", x = "start",
       xend = "stop", color = "resp", size = "lg",
       ticks = "visit", band_x = "dor_from", band_xend = "dor_to",
       event_x = "event_day", event_shape = "event", open = "ongoing")
)

scales <- new_scale_map(
  scale_binding("resp", color = c(
    "Not assessed" = "#dad9d5", CR = "#008300", PR = "#1baf7a",
    SD = "#b9b8b2", PD = "#e34948"
  ))
)

if (!isTRUE(getOption("swimmer.no_serve"))) {
  board <- new_dock_board(
    blocks = c(
      data = new_static_block(swim),
      swimmer = new_table_block(
        layout = "groups", by = c("ARM", "USUBJID"),
        summaries = swimmer_summaries, missing = "drop",
        sort_by = "Days on tx", sort_dir = "desc"
      )
    ),
    links = list(new_link("data", "swimmer", "data")),
    options = new_board_options(new_scale_map_option(scales))
  )
  serve(board)
}
