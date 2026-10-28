# A staffing table in the groups table: who at a consulting company works on
# which project, when. One row per person, nested in their team, listing
# columns beside a swimlane. The swimlane is a `spans` summary with its four
# overlays:
#
#   segments   each assignment, coloured by project; gaps are bench time
#   ticks      the project's sprint reviews or steering meetings
#   band_x/_xend  weeks on site at the client
#   event_x/_shape  milestone delivered, milestone planned, blocker raised,
#              leave day
#   open       assignment still running today: an arrow at the lane's end
#
# All of it is one long frame: segment rows, tick rows, band rows and event
# rows, each overlay reading only the rows where its own columns are set.
#
#   Rscript blockr.viz/dev/example-table-projects.R [port]
#
# Run from the workspace root. The port defaults to 4013 (the dev container
# holds 3838-3847 on the Mac). BLOCKR_UI points at another blockr.ui
# checkout when the workspace one is behind main.

# Package roots: this checkout of blockr.viz (also from a worktree), and the
# workspace above it that holds the other packages.
.self <- grep("^--file=", commandArgs(FALSE), value = TRUE)[1]
.viz <- if (!is.na(.self) && grepl("example-table-projects", .self)) {
  normalizePath(file.path(dirname(sub("^--file=", "", .self)), ".."))
} else {
  getOption("projects.viz", normalizePath("blockr.viz"))
}
.ws <- .viz
while (!dir.exists(file.path(.ws, "blockr.core")) && dirname(.ws) != .ws) {
  .ws <- dirname(.ws)
}
.port <- as.integer(c(commandArgs(TRUE), Sys.getenv("BLOCKR_PORT"), "4013")[1])

options(shiny.port = .port, shiny.launch.browser = FALSE)
options(blockr.dock_is_locked = FALSE)

pkgload::load_all(file.path(.ws, "blockr.core"))
pkgload::load_all(Sys.getenv("BLOCKR_UI", file.path(.ws, "blockr.ui")))
pkgload::load_all(file.path(.ws, "blockr.dock"))
pkgload::load_all(file.path(.ws, "blockr.dag"))
pkgload::load_all(file.path(.ws, "blockr.theme"))
pkgload::load_all(.viz)

d <- function(x) as.Date(if (length(x)) paste0("2026-", x) else character())
TODAY <- d("10-28")
PERIOD <- c(d("05-04"), TODAY)

# Projects: the recurring meeting (first date, step in days) and the
# milestones (date, name). A milestone up to today is delivered, after it
# planned.
projects <- list(
  "Harbor Bank cloud migration" = list(
    meet = d("05-15"), every = 14,
    ms = list(c("06-26", "Wave 1 cut-over"), c("09-04", "Wave 2 cut-over"),
              c("11-13", "Wave 3 cut-over"))
  ),
  "Meridian CRM rollout" = list(
    meet = d("06-12"), every = 14,
    ms = list(c("08-14", "Pilot region live"), c("11-27", "Go-live"))
  ),
  "Fjord Energy data platform" = list(
    meet = d("07-10"), every = 14,
    ms = list(c("09-11", "Platform MVP"), c("10-23", "Handover to client"))
  ),
  "Northwind pricing review" = list(
    meet = d("05-28"), every = 28,
    ms = list(c("06-19", "Pricing model"), c("07-17", "Final report"))
  ),
  "Atlas supply chain audit" = list(
    meet = d("08-20"), every = 28,
    ms = list(c("09-25", "Interim findings"), c("11-20", "Final report"))
  )
)
PROJ <- names(projects)

# People: team, role, assignments (project, from, to; to = NA runs on past
# today), on-site weeks, blockers raised, leave days.
people <- list(
  list("Strategy", "Lena Brandt", "Partner",
       list(c(4, "05-04", "07-17"), c(5, "08-03", NA)),
       site = list(c("06-01", "06-12"), c("09-07", "09-18")),
       block = character(), leave = c("07-27", "07-28", "07-29")),
  list("Strategy", "Tom Ekberg", "Senior consultant",
       list(c(4, "05-04", "07-31"), c(5, "08-17", NA)),
       site = list(c("08-24", "09-25")),
       block = "10-14", leave = c("08-10", "08-11")),
  list("Strategy", "Priya Nair", "Consultant",
       list(c(4, "05-18", "06-26"), c(2, "07-06", "09-25")),
       site = list(c("07-27", "08-14")),
       block = character(), leave = c("10-05", "10-06", "10-07")),
  list("Strategy", "Jonas Weber", "Analyst",
       list(c(4, "05-04", "07-17"), c(5, "08-03", "10-16")),
       site = list(),
       block = character(), leave = "07-24"),
  list("Data & Analytics", "Sofia Martins", "Data lead",
       list(c(1, "05-04", "06-19"), c(3, "07-01", "09-30"),
            c(1, "10-05", NA)),
       site = list(c("07-06", "07-10")),
       block = character(), leave = c("06-22", "06-23", "06-24")),
  list("Data & Analytics", "Daniel Okafor", "Data engineer",
       list(c(1, "05-04", "08-28"), c(3, "09-07", "10-23")),
       site = list(),
       block = "08-21", leave = c("08-31", "09-01", "09-02", "09-03")),
  list("Data & Analytics", "Mei Chen", "Data scientist",
       list(c(4, "05-11", "07-10"), c(3, "07-13", "10-09")),
       site = list(c("05-11", "05-15")),
       block = "08-27", leave = character()),
  list("Data & Analytics", "Arjun Rao", "Analytics engineer",
       list(c(3, "07-01", "10-23"), c(2, "10-26", NA)),
       site = list(),
       block = character(), leave = c("08-17", "08-18", "08-19")),
  list("Engineering", "Marta Kowalski", "Engineering manager",
       list(c(1, "05-04", "07-31"), c(2, "08-03", NA)),
       site = list(c("06-22", "06-26")),
       block = "10-09", leave = c("08-24", "08-25")),
  list("Engineering", "Lukas Hoffmann", "Backend developer",
       list(c(1, "05-04", NA)),
       site = list(c("08-31", "09-04")),
       block = "08-21", leave = c("07-13", "07-14", "07-15")),
  list("Engineering", "Ana Ribeiro", "Frontend developer",
       list(c(2, "06-01", NA)),
       site = list(c("08-10", "08-14")),
       block = "10-09", leave = c("09-14", "09-15")),
  list("Engineering", "Kenji Sato", "Cloud engineer",
       list(c(1, "05-18", "09-11"), c(2, "09-21", NA)),
       site = list(c("06-22", "06-26"), c("08-31", "09-04")),
       block = character(), leave = "05-25"),
  list("Engineering", "Elif Demir", "QA engineer",
       list(c(2, "06-15", "08-14"), c(1, "08-24", NA)),
       site = list(),
       block = "10-20", leave = c("07-20", "07-21"))
)

EVENTS <- c("Milestone delivered", "Milestone planned", "Blocker raised",
            "Leave")

weekdays_in <- function(from, to) {
  if (to < from) return(0L)
  days <- seq(from, to, by = "day")
  sum(!format(days, "%u") %in% c("6", "7"))
}

row_frame <- function(n) {
  data.frame(
    from = rep(as.Date(NA), n), to = rep(as.Date(NA), n),
    project = rep(NA_character_, n), meeting = rep(as.Date(NA), n),
    site_from = rep(as.Date(NA), n), site_to = rep(as.Date(NA), n),
    event_day = rep(as.Date(NA), n), event = rep(NA_character_, n)
  )
}

staffing_frame <- function(people) {
  out <- lapply(people, function(p) {
    a <- p[[4L]]
    asg <- data.frame(
      project = PROJ[as.integer(vapply(a, `[`, "", 1L))],
      from = d(vapply(a, `[`, "", 2L)),
      to = as.Date(vapply(a, function(x) {
        if (is.na(x[[3L]])) format(TODAY) else paste0("2026-", x[[3L]])
      }, ""))
    )
    running <- any(is.na(vapply(a, `[`, "", 3L)))

    seg <- row_frame(nrow(asg))
    seg$from <- asg$from
    seg$to <- asg$to
    seg$project <- asg$project

    # The project's meetings that fall inside the assignment.
    meets <- do.call(c, lapply(seq_len(nrow(asg)), function(i) {
      pr <- projects[[asg$project[[i]]]]
      m <- seq(pr$meet, d("12-31"), by = pr$every)
      m[m > asg$from[[i]] & m < asg$to[[i]]]
    }))
    tick <- row_frame(length(meets))
    tick$meeting <- meets

    band <- row_frame(length(p$site))
    if (length(p$site)) {
      band$site_from <- d(vapply(p$site, `[`, "", 1L))
      band$site_to <- d(vapply(p$site, `[`, "", 2L))
    }

    # Milestones of the projects the person is on at the time: delivered up
    # to today, planned after it (only for an assignment still running).
    ms <- do.call(rbind, lapply(seq_len(nrow(asg)), function(i) {
      pr <- projects[[asg$project[[i]]]]
      day <- d(vapply(pr$ms, `[`, "", 1L))
      end <- if (running && i == nrow(asg)) d("12-31") else asg$to[[i]]
      keep <- day >= asg$from[[i]] & day <= end
      if (!any(keep)) return(NULL)
      data.frame(day = day[keep],
                 ev = ifelse(day[keep] <= TODAY, EVENTS[[1L]], EVENTS[[2L]]))
    }))
    ev <- rbind(
      ms,
      if (length(p$block)) data.frame(day = d(p$block), ev = EVENTS[[3L]]),
      if (length(p$leave)) data.frame(day = d(p$leave), ev = EVENTS[[4L]])
    )
    evr <- row_frame(NROW(ev))
    if (NROW(ev)) {
      evr$event_day <- ev$day
      evr$event <- ev$ev
    }

    x <- rbind(seg, tick, band, evr)
    x$team <- p[[1L]]
    x$person <- p[[2L]]

    # Per-person figures sit on the first row only, so a team row sums or
    # averages over people rather than over the frame's rows.
    leave <- d(p$leave)
    booked <- sum(mapply(function(f, t) {
      t <- min(t, TODAY)
      weekdays_in(f, t) - sum(leave >= f & leave <= t)
    }, asg$from, asg$to))
    avail <- weekdays_in(PERIOD[[1L]], PERIOD[[2L]]) - length(leave)
    first <- c(TRUE, rep(FALSE, nrow(x) - 1L))
    x$role <- ifelse(first, p[[3L]], NA_character_)
    x$util <- ifelse(first, round(100 * booked / avail), NA_real_)
    x$booked <- ifelse(first, booked, NA_real_)
    x$blockers <- ifelse(first, sum(d(p$block) > d("10-01")), NA_real_)
    x$running <- ifelse(first, running, NA)
    x
  })
  out <- do.call(rbind, out)
  out$project <- factor(out$project, levels = PROJ)
  # The level order picks the shape: delivered filled triangle, planned open
  # triangle, blocker cross, leave square.
  out$event <- factor(out$event, levels = EVENTS)
  out
}

staff <- staffing_frame(people)
attr(staff$event_day, "label") <- "Date"
attr(staff$project, "label") <- "Project"
attr(staff$event, "label") <- "Event"

projects_summaries <- list(
  list(type = "field", name = "Role", col = "role"),
  list(type = "simple", name = "Utilisation %", func = "mean", col = "util",
       show = "bar"),
  list(type = "simple", name = "Days booked", func = "sum", col = "booked",
       show = "number"),
  list(type = "simple", name = "Open blockers", func = "sum", col = "blockers",
       show = "number"),
  list(type = "spans", name = "Assignments, May to November", x = "from",
       xend = "to", color = "project", label = "project", size = "lg",
       ticks = "meeting", band_x = "site_from", band_xend = "site_to",
       event_x = "event_day", event_shape = "event", open = "running")
)

scales <- new_scale_map(
  scale_binding("project", color = c(
    "Harbor Bank cloud migration" = "#2a6fdb",
    "Meridian CRM rollout" = "#1baf7a",
    "Fjord Energy data platform" = "#8a5cd1",
    "Northwind pricing review" = "#e0a019",
    "Atlas supply chain audit" = "#d9534f"
  ))
)

if (!isTRUE(getOption("projects.no_serve"))) {
  board <- new_dock_board(
    blocks = c(
      data = new_static_block(staff),
      staffing = new_table_block(
        layout = "groups", by = c("team", "person"),
        summaries = projects_summaries, missing = "drop",
        sort_by = "Days booked", sort_dir = "desc"
      )
    ),
    links = list(new_link("data", "staffing", "data")),
    options = new_board_options(new_scale_map_option(scales))
  )
  serve(board)
}
