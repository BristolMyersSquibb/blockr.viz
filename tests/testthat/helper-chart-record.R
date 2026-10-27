# Recordings of what crosses the chart block's edges, for the rewrite.
#
# A scenario is a constructor call, a data frame and a list of steps, all
# in the public argument names of api-proposal.md. Running one records,
# after the first flush and after every step: the state (public names), the
# emitted expression, the result, the drill sends over the control channel,
# downloads, and what the browser got (rows, columns, settings as drawn).
# Each step also keeps the raw browser messages ("wire"), compared only
# while the target speaks the reference's messages.
#
# helper-chart-reference.R translates between the public names and the
# reference. dev/chart-record.R writes the recordings to
# fixtures/chart-record/; test-chart-record.R replays them.

record_df <- function() {
  d <- data.frame(
    cat = c("a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "d"),
    grp = rep(c("g1", "g2"), 6),
    fac = factor(rep(c("lo", "hi", "mid"), 4), levels = c("lo", "mid", "hi")),
    num = c(1.5, 2, 3.25, 4, 5, 6.5, 7, 8, 9.75, 10, 11, 12),
    num2 = c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8),
    int = 1:12,
    id = sprintf("S%02d", c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)),
    day = as.Date("2024-01-01") + c(0, 7, 14, 0, 7, 14, 0, 7, 14, 0, 7, 14),
    flag = c(TRUE, FALSE, NA, TRUE, FALSE, TRUE, TRUE, FALSE, NA, TRUE,
             FALSE, TRUE),
    cat_na = c("a", NA, "b", "a", "b", NA, "a", "b", "a", NA, "b", "a"),
    stringsAsFactors = FALSE
  )
  d$lo <- d$num - 1
  d$lo2 <- d$num - 2
  d$hi <- d$num + 1
  d$hi2 <- d$num + 2
  d$end <- d$day + 3
  attr(d$num, "label") <- "Numeric value"
  attr(d$cat, "label") <- "Category"
  d
}

record_long_df <- function() {
  i <- seq_len(240)
  d <- data.frame(
    x = rep(1:20, each = 12),
    subj = sprintf("P%03d", rep(1:12, 20)),
    arm = rep(c("A", "B"), 120),
    stringsAsFactors = FALSE
  )
  d$y <- round(sin(d$x / 3) * 10 + (i %% 7) - 3 + ifelse(d$arm == "A", 2, 0), 3)
  d$ulim <- 12
  d$llim <- -8
  d
}

record_visit_df <- function() {
  data.frame(
    AVISIT = rep(c("Week 2", "Baseline", "Week 4"), 4),
    AVISITN = rep(c(2, 0, 4), 4),
    AVAL = c(5, 3, 7, 6, 2, 8, 4, 3, 9, 5, 1, 6),
    TRT = rep(c("X", "Y"), each = 6),
    stringsAsFactors = FALSE
  )
}

record_data <- function(name) {
  switch(name,
    df = record_df(),
    long = record_long_df(),
    visit = record_visit_df(),
    df_titled = local({
      d <- record_df()
      attr(d, "label") <- "Data label"
      attr(d, "subtitle") <- "Data subtitle"
      attr(d, "caption") <- "Data caption"
      d
    }),
    df_no_grp = record_df()[setdiff(names(record_df()), "grp")],
    df_empty = record_df()[0, ],
    # A table-producing object: coerced through as_annotated_df(), its
    # annotation columns dropped before the chart sees it.
    tbl = local({
      d <- record_df()
      d$.label <- d$cat
      d$.indent <- 0L
      attr(d, "blockr_filters") <- "grp == g1"
      structure(list(df = d), class = "record_tbl")
    }),
    df_changed = local({
      d <- record_df()
      d$num <- d$num * 2
      d
    }),
    stop("unknown golden data ", name)
  )
}


record_register_tbl <- function() {
  registerS3method("as_annotated_df", "record_tbl", function(x, ...) x$df,
                   envir = asNamespace("blockr.viz"))
}

record_scenarios <- function() {
  # Steps, in public names and R values:
  #   set(...)                  settings, as the gear changes them
  #   click(col = values, ...)  a categorical click; click() clears
  #   point(x_col, y_col, x, y) a click on one dot without a drill column
  #   brush(x_col, x_range, y_col, y_range, panel)  a brush; brush() clears
  #   data("<name>")            upstream hands over another frame
  #   capture()                 the browser posts its bitmap
  #   download("<id>")          a download link is clicked
  #   wire(list(<input> = <value>))  a raw browser input; reference only
  set <- function(...) list(kind = "set", args = list(...))
  click <- function(...) list(kind = "click", args = list(...))
  point <- function(x_col, y_col, x, y) {
    list(kind = "point", x_col = x_col, y_col = y_col, x = x, y = y)
  }
  brush <- function(x_col = NULL, x_range = NULL, y_col = NULL,
                    y_range = NULL, panel = NULL) {
    list(kind = "brush", x_col = x_col, x_range = x_range, y_col = y_col,
         y_range = y_range, panel = panel)
  }
  data_step <- function(name) list(kind = "data", name = name)
  capture <- function() list(kind = "capture")
  download <- function(id) list(kind = "download", id = id)
  wire <- function(...) list(kind = "wire", inputs = list(...))
  gear <- function(...) wire(drilldown_block_action = list(action = "config", ...))

  list(
    # --- one per type -----------------------------------------------------
    bar_count = list(args = list(type = "bar", y = "cat")),
    bar_mean_dodge = list(args = list(
      type = "bar", x = "cat", y = "num", func = "mean", color = "grp",
      position = "dodge", value_labels = TRUE, format = "1", unit = "kg",
      legend = "none", sort_by = "value", sort_dir = "asc"
    )),
    bar_fill_facet = list(args = list(
      type = "bar", x = "cat", y = "num", func = "sum", color = "grp",
      position = "fill", facet = "fac", facet_cols = 2,
      facet_scales = "free", show_n = "both", id = "id",
      percent_of = "color"
    )),
    bar_missing_drop = list(args = list(
      type = "bar", y = "cat_na", missing = "drop", format = "compact",
      y_transform = "log10", tooltip = "grp", drill = "cat_na"
    )),
    bar_distinct = list(args = list(
      type = "bar", x = "cat", y = "id", func = "count_distinct",
      orientation = "vertical", label = "grp"
    )),
    bar_pct_distinct = list(args = list(
      type = "bar", x = "cat", y = "id", func = "pct_distinct",
      color = "grp", percent_of = "total"
    )),
    histogram = list(
      args = list(type = "histogram", x = "num", bins = 4, drill = "auto"),
      steps = list(
        click(num_bin = "4–6"),
        set(bins = "x"),
        set(bins = 6, sort_by = "value", sort_dir = "desc")
      )
    ),
    waterfall = list(args = list(
      type = "waterfall", x = "cat", y = "num", func = "sum",
      waterfall_totals = "d"
    )),
    pie = list(args = list(type = "pie", x = "cat", y = "num",
                           func = "sum", legend = "marks")),
    treemap = list(args = list(type = "treemap", x = "cat", y = "num",
                               func = "sum")),
    boxplot = list(args = list(
      type = "boxplot", x = "cat", y = "num", summary = "mean_sd",
      whiskers = "min_max", box_points = "outliers", color = "grp"
    )),
    pointrange = list(args = list(
      type = "pointrange", x = "fac", y = "num", summary = "mean_se",
      connect_centers = TRUE
    )),
    scatter_full = list(args = list(
      type = "scatter", x = "num", y = "num2", color = "grp",
      smoother = "lm", identity_line = TRUE,
      y_lines = c(Target = 4, 6), x_lines = c(Listed = 5),
      x_ranges = list(Early = c(1, 3)), highlight = list(grp = "g1"),
      label = "id", tooltip = c("id", "day"), x_transform = "log10",
      point_size = 2, legend = "top", facet = "fac", facet_scales = "free_y"
    )),
    scatter_loess = list(args = list(
      type = "scatter", x = "x", y = "y", color = "arm", smoother = "loess"
    ), data = "long"),
    line_full = list(args = list(
      type = "line", x = "day", y = "num", series = "id", color = "grp",
      connect = "step_start", ymin = c("lo", "lo2"), ymax = c("hi", "hi2"),
      linetype = "grp", line_width = 2, x_lines = c(Cut = "2024-01-08")
    )),
    area_dodge = list(args = list(type = "area", x = "day", y = "num",
                                  color = "grp", position = "dodge")),
    band = list(args = list(
      type = "band", x = "x", y = "y", id = "subj", ref_max = "ulim",
      ref_min = "llim", band_window = "fixed", band_size = 3,
      band_min_n = 4, summary = "mean_sd", whiskers = "min_max",
      color = "arm"
    ), data = "long"),
    band_default = list(args = list(type = "band", x = "x", y = "y"),
                        data = "long"),
    band_too_thin = list(args = list(type = "band", x = "num", y = "num2",
                                     band_min_n = 50)),
    gantt = list(args = list(type = "gantt", x = "day", xend = "end",
                             y = "id", color = "grp", sort_by = "onset")),
    visit_order = list(args = list(type = "line", x = "AVISIT", y = "AVAL",
                                   color = "TRT"), data = "visit"),

    # --- text -------------------------------------------------------------
    titles_template = list(
      args = list(
        type = "bar", x = "cat", y = "num", func = "mean", color = "grp",
        title = "{n} rows of {label(cat)}",
        subtitle = "{@func in mean, median} of {label(@y)}[, by {@color}]",
        caption = ""
      ),
      data = "df_titled",
      steps = list(set(title = "", caption = "{n_distinct(id)} ids"))
    ),
    titles_auto = list(args = list(type = "bar", y = "cat"),
                       data = "df_titled"),
    titles_offer = list(args = list(
      type = "bar", y = "cat",
      subtitle = "Counts[, coloured by {@color}][, in {@facet_cols} columns]"
    )),
    titles_highlight = list(args = list(
      type = "scatter", x = "num", y = "num2", highlight = list(grp = "g2"),
      caption = "Highlighted: {highlight}"
    )),

    # --- clicks -----------------------------------------------------------
    drill_categorical = list(
      args = list(type = "bar", y = "cat", drill = "cat"),
      steps = list(
        click(cat = "b"),
        click(cat = c("a", "c")),
        click(cat_na = NA),
        click()
      )
    ),
    drill_point = list(
      args = list(type = "scatter", x = "num", y = "num2"),
      steps = list(point("num", "num2", 4, 1), click())
    ),
    drill_point_date = list(
      args = list(type = "scatter", x = "day", y = "num"),
      steps = list(point("day", "num", as.Date("2024-01-08"), 2))
    ),
    drill_range = list(
      args = list(type = "scatter", x = "num", y = "num2", facet = "fac"),
      steps = list(
        brush("num", c(2, 8), "num2", c(1, 5), panel = list(fac = "lo")),
        brush("num", c(4, 4)),
        brush()
      )
    ),
    drill_range_date = list(
      args = list(type = "line", x = "day", y = "num"),
      steps = list(brush("day", as.Date(c("2024-01-05", "2024-01-10"))))
    ),
    drill_restore = list(args = list(
      type = "bar", y = "cat", drill = "cat",
      selection = list(type = "categorical", filters = list(cat = "a"))
    )),
    drill_transient = list(
      args = list(type = "bar", y = "cat", drill = "cat",
                  drill_target = "vf1", drill_table = "adsl"),
      steps = list(
        click(cat = "b"),
        click(cat = "b"),
        click(),
        set(drill_target = "", drill_table = " t2 ")
      )
    ),

    # --- settings ---------------------------------------------------------
    set_roles = list(
      args = list(type = "bar", y = "cat"),
      steps = list(
        set(type = "bar", x = "num", y = "cat", func = "mean",
            orientation = "horizontal"),
        set(type = "waterfall", x = "cat", y = "num", func = "sum"),
        set(type = "scatter", x = "num", y = "num2"),
        set(type = "histogram", x = "num", sort_by = "alpha",
            sort_dir = "desc"),
        set(type = "boxplot", x = "fac", y = "num")
      )
    ),
    set_aesthetics = list(
      args = list(type = "scatter", x = "num", y = "num2"),
      steps = list(
        set(color = "grp", facet = "fac", series = "id", label = "cat",
            tooltip = "id", drill = "id"),
        set(color = NULL, facet = NULL, series = NULL, label = NULL,
            tooltip = NULL, drill = NULL),
        set(format = "percent", unit = " %", y_transform = "log10",
            x_transform = "log10"),
        set(format = "bogus", y_transform = "identity"),
        set(value_labels = TRUE, identity_line = TRUE, connect = "monotone",
            smoother = "lm", connect_centers = TRUE),
        set(value_labels = FALSE, identity_line = FALSE,
            connect = "monotone", connect_centers = FALSE),
        set(y_lines = c(Target = 4, 6), x_lines = c(2, Cut = 7),
            x_ranges = list(Early = c(1, 3))),
        set(y_lines = NULL, x_lines = NULL, x_ranges = NULL),
        set(highlight = list(grp = "g2")),
        set(highlight = NULL),
        set(line_width = 1.5, point_size = 3),
        set(legend = "nowhere"),
        set(legend = "none"),
        set(download = FALSE),
        set(download = TRUE),
        set(facet_scales = "free", facet_cols = 3),
        set(facet_cols = NULL),
        set(facet_cols = "junk")
      )
    ),
    set_distribution = list(
      args = list(type = "band", x = "x", y = "y", id = "subj"),
      data = "long",
      steps = list(
        set(box_points = "outliers", summary = "p5_p95", whiskers = "tukey"),
        set(band_window = "fixed", band_size = 5, band_min_n = 2),
        set(band_size = "junk", band_min_n = 0),
        set(id = "arm"),
        set(id = NULL),
        set(ref_max = "ulim", ref_min = NULL),
        set(show_n = "axis", missing = "drop", percent_of = "total")
      )
    ),
    set_misc = list(
      args = list(type = "line", x = "day", y = "num", ymin = "lo",
                  ymax = "hi"),
      steps = list(
        set(ymin = "lo2", ymax = "hi2", linetype = "grp"),
        set(ymin = NULL, ymax = NULL),
        set(linetype = NULL),
        set(waterfall_totals = "d"),
        set(waterfall_totals = NULL),
        set(drill_target = "auto", drill_table = "adsl"),
        set(position = "fill"),
        set(position = "dodge")
      )
    ),
    heal_missing_column = list(
      args = list(type = "bar", x = "cat", color = "grp", facet = "fac",
                  series = "id", linetype = "grp"),
      steps = list(
        data_step("df_no_grp"),
        set(color = NULL, linetype = NULL),
        set(color = "fac"),
        data_step("df")
      )
    ),
    data_changes = list(
      args = list(type = "bar", y = "cat"),
      steps = list(
        set(sort_dir = "asc"),
        data_step("df_changed"),
        set(sort_dir = "asc"),
        data_step("df_empty")
      )
    ),
    script = list(
      args = list(
        type = "bar", x = "cat", y = "num2", func = "sum",
        script = c("k <- 2", "transform(data, num2 = num * k)"),
        script_values = list(k = 3)
      ),
      steps = list(
        set(script_values = list(k = 4)),
        set(script = "stop('boom')"),
        set(script = "")
      )
    ),
    script_drill = list(
      args = list(
        type = "bar", x = "kind", drill = "kind",
        script = "transform(data, kind = ifelse(num > 5, 'big', 'small'))"
      ),
      steps = list(click(kind = "big"), click(kind = "big", grp = "g1"))
    ),
    input_tbl = list(
      args = list(type = "bar", y = "cat", drill = "cat",
                  caption = "{filters}"),
      data = "tbl",
      steps = list(click(cat = "a"))
    ),

    # --- downloads --------------------------------------------------------
    download_bar = list(
      args = list(type = "bar", x = "cat", y = "num", func = "mean",
                  color = "grp", title = "Means"),
      steps = list(download("dl_xlsx"))
    ),
    download_scatter = list(
      args = list(type = "scatter", x = "num", y = "num2"),
      steps = list(download("dl_xlsx"), set(download = FALSE))
    ),
    download_capture = list(
      args = list(type = "bar", y = "cat"),
      steps = list(
        download("dl_png"),
        capture(),
        download("dl_png"),
        download("dl_html"),
        download("dl_pptx")
      )
    ),

    # --- constructor ------------------------------------------------------
    ctor_normalize = list(args = list(
      type = list("bar"), x = list(), y = "cat", color = list(),
      identity_line = "on", value_labels = "off", box_points = "all",
      facet_cols = "junk", bins = "0", format = 2, unit = NA,
      percent_of = c("facet", "color"), tooltip = list("id", "grp"),
      title = "", subtitle = list()
    )),
    ctor_legacy = list(args = list(
      chart_type = "bar", group = "cat", value = "num", bar_mode = "grouped",
      tt_fields = "id", na_group = "drop", count_on = "axis",
      line_width_mult = 2
    )),
    ctor_legacy_hi = list(args = list(
      type = "line", x = "day", y = "num", lo = "lo", hi = "hi",
      direct_labels = TRUE
    )),

    # --- the reference's browser messages ---------------------------------
    # What the gear and the chart send today, beyond what a public setting
    # says. Replayed only against a target that speaks these messages.
    wire_click_forms = list(
      wire = TRUE,
      args = list(type = "bar", y = "cat", drill = "cat"),
      steps = list(
        wire(drilldown_block_action = list(
          action = "filter", filter_type = "categorical", column = "cat",
          values = list("b"), nonce = 1
        )),
        wire(drilldown_block_action = list(
          action = "filter", filter_type = "categorical", column = NULL,
          values = list(), nonce = 2
        )),
        wire(drilldown_block_action = list(
          action = "filter", filter_type = "point", x_col = "num"
        ))
      )
    ),
    wire_gear_echoes = list(
      wire = TRUE,
      args = list(type = "band", x = "x", y = "y", id = "subj",
                  color = "arm"),
      data = "long",
      steps = list(
        gear(chart_type = "bar", baseline = "cumulative", group = "arm",
             value = "y", func = "sum"),
        gear(tt_fields = list("subj", "")),
        gear(highlight_by = "arm", highlight = list()),
        gear(band_id = "arm", count_col = "subj"),
        gear(band_id = "arm", count_col = "arm"),
        gear(band_id = "", count_col = "subj"),
        gear(value_labels = "off", connect_centers = "off"),
        gear(position = "x", bar_mode = "grouped")
      )
    ),
    wire_transport = list(
      wire = TRUE,
      args = list(type = "bar", y = "cat"),
      steps = list(
        gear(sort_dir = "asc"),
        wire(drilldown_block_need = 1),
        wire(drilldown_block_ready = 1),
        data_step("df_changed"),
        wire(drilldown_block_ready = 2)
      )
    )
  )
}

# Constructor calls that fail or warn, recorded without a server.
record_ctor_cases <- function() {
  list(
    bad_type = list(type = "radar"),
    bad_orientation = list(orientation = "up"),
    bad_position = list(position = "side"),
    bad_interval = list(ymin = c("a", "b"), ymax = "c"),
    bad_legend = list(legend = "left"),
    bad_format = list(format = "fancy"),
    bad_percent_of = list(percent_of = "row"),
    bad_connect = list(connect = "curvy"),
    bad_missing = list(missing = "keep"),
    expose = list(expose = "color"),
    legacy_value = list(type = "bar", y = "cat", value = TRUE),
    full = list(type = "scatter", x = "num", y = "num2", caption = "C")
  )
}

record_capture_png <- function() {
  # A 1x1 transparent png.
  paste0("data:image/png;base64,",
         "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR4nGNgYGD4",
         "DwABBAEAwS2OUAAAAABJRU5ErkJggg==")
}

# The mock session's id counts sessions per process ("proxy3-expr-..."), so
# it depends on what ran before. Recorded as "blk-".
record_scrub <- function(x) {
  if (is.character(x)) {
    a <- attributes(x)
    x <- gsub("proxy[0-9]+-", "blk-", x)
    x <- gsub("^(cap-.*-)[0-9]+$", "\\1<n>", x)
    attributes(x) <- a
    return(x)
  }
  if (is.list(x) && !is.data.frame(x)) {
    a <- attributes(x)
    x <- lapply(x, record_scrub)
    attributes(x) <- a
  }
  x
}

record_value <- function(f) {
  tryCatch(f(), error = function(e) {
    structure(list(message = conditionMessage(e)), class = "record_error")
  })
}

record_download <- function(output, id) {
  path <- record_value(function() output[[id]])
  if (inherits(path, "record_error") || !is.character(path) ||
        !file.exists(path)) {
    return(path)
  }
  if (grepl("xlsx$", path)) {
    return(openxlsx::read.xlsx(path, colNames = FALSE))
  }
  # Pictures, pages and decks depend on the machine's fonts and zip times;
  # that one was written is what is compared.
  paste("written:", tools::file_ext(path))
}

record_run <- function(sc, target = record_target()) {
  record_register_tbl()
  withr::local_options(list(
    blockr.viz.facet_cols = NULL,
    blockr.viz.default_caption = NULL,
    useFancyQuotes = FALSE
  ))
  src <- new.env(parent = emptyenv())
  src$d <- record_data(sc$data %||% "df")
  tick <- shiny::reactiveVal(0L)
  dat <- shiny::reactive({
    tick()
    src$d
  })
  out <- new.env(parent = emptyenv())
  out$warnings <- character()
  out$steps <- list()
  blk <- withCallingHandlers(
    target$ctor(sc$args),
    warning = function(w) {
      out$warnings <- c(out$warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  out$initial <- target$state(blockr.core:::initial_block_state(blk))

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      sent <- new.env(parent = emptyenv())
      sent$msgs <- list()
      root <- session$rootScope()
      root$sendCustomMessage <- function(type, message) {
        sent$msgs <- c(sent$msgs, list(list(type = type, message = message)))
        invisible(NULL)
      }
      ctrl <- new.env(parent = emptyenv())
      ctrl$log <- list()
      log_ctrl <- function(op) {
        function(target, args, author = NULL) {
          ctrl$log <- c(ctrl$log, list(list(
            op = op, target = target, args = args, author = author
          )))
          TRUE
        }
      }
      session$userData$blockr_ctrl_send <- log_ctrl("send")
      session$userData$blockr_ctrl_clear <- log_ctrl("clear")
      expr <- session$makeScope("expr")
      st <- session$returned$state
      cx <- new.env(parent = emptyenv())
      cx$nonce <- 0L

      cx$from <- 1L
      record <- function(dl = NULL) {
        msgs <- if (length(sent$msgs) >= cx$from) {
          sent$msgs[cx$from:length(sent$msgs)]
        } else {
          list()
        }
        cx$from <- length(sent$msgs) + 1L
        list(
          state = target$state(lapply(st, record_value)),
          expr = record_value(function() session$returned$expr()),
          result = record_value(function() {
            r <- session$returned$result()
            if (is.data.frame(r)) as.data.frame(r) else r
          }),
          ctrl = ctrl$log,
          download = dl,
          browser = target$payload(msgs),
          wire = msgs
        )
      }

      session$flushReact()
      out$steps[[1L]] <- record()

      for (i in seq_along(sc$steps)) {
        s <- sc$steps[[i]]
        dl <- NULL
        if (s$kind %in% c("set", "click", "point", "brush", "wire")) {
          if (identical(s$kind, "click")) cx$nonce <- cx$nonce + 1L
          cx$state <- lapply(st, function(rv) shiny::isolate(rv()))
          cx$data <- src$d
          inputs <- target$inputs(s, cx)
          for (k in seq_along(inputs)) {
            do.call(expr$setInputs, inputs[k])
            session$flushReact()
          }
        } else if (identical(s$kind, "data")) {
          src$d <- record_data(s$name)
          tick(i)
        } else if (identical(s$kind, "capture")) {
          expr$setInputs(drilldown_block_capture = list(
            png = record_capture_png(), width = 640, height = 400
          ))
        } else if (identical(s$kind, "download")) {
          session$flushReact()
          dl <- record_download(expr$output, s$id)
        }
        session$flushReact()
        out$steps[[i + 1L]] <- record(dl)
      }
    },
    args = list(x = blk, data = list(data = dat))
  )
  record_scrub(list(warnings = out$warnings, initial = out$initial,
                    steps = out$steps))
}

# Without the raw browser messages, for a target that speaks other ones.
record_public <- function(rec) {
  rec$steps <- lapply(rec$steps, function(s) {
    s$wire <- NULL
    s
  })
  rec
}

record_ctor_run <- function(args, target = record_target()) {
  withr::local_options(list(useFancyQuotes = FALSE))
  warns <- new.env(parent = emptyenv())
  warns$msg <- character()
  res <- tryCatch(
    withCallingHandlers(
      target$ctor(args),
      warning = function(w) {
        warns$msg <- c(warns$msg, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      structure(list(message = conditionMessage(e)), class = "record_error")
    }
  )
  if (!inherits(res, "record_error")) {
    keep <- !vapply(attributes(res), is.function, logical(1L))
    attrs <- attributes(res)[keep & names(attributes(res)) != "class"]
    attrs$external_ctrl <- names(target$state(
      stats::setNames(as.list(attrs$external_ctrl), attrs$external_ctrl)
    ))
    attrs$allow_empty_state <- names(target$state(
      stats::setNames(as.list(attrs$allow_empty_state),
                      attrs$allow_empty_state)
    ))
    res <- list(
      initial = target$state(blockr.core:::initial_block_state(res)),
      class = class(res),
      attrs = attrs,
      ui = as.character(res$expr_ui("blk"))
    )
  }
  list(warnings = warns$msg, value = res)
}

record_dir <- function() {
  testthat::test_path("fixtures", "chart-record")
}
