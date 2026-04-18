# Data shaping for tile_block (§3 design, §4 requirements).
#
# tile_shape() is called by the block's `expr` reactive via bquote.
# Takes the raw input frame plus the full aesthetic / stat / format
# spec and returns a long tidy frame with one row per
# (facet cell × measure × stat), suitable for tile rendering.

#' Shape a data frame for tile_block rendering.
#'
#' @param data Input data frame.
#' @param showcase One of `"number"`, `"spark"`, `"progress"`.
#' @param aesthetics Named list of aesthetic → column name(s).
#' @param stats Named list of aesthetic → stat function name(s).
#' @param formats Named list of aesthetic → list(kind, digits) override.
#' @return A tibble with columns .row, .col, .measure, .stat, .value,
#'   .label, .target, .max, .status, .spark, .format, .digits.
#' @export
tile_shape <- function(
  data,
  showcase = "number",
  aesthetics = list(),
  stats = list(),
  formats = list()
) {
  if (!is.data.frame(data)) stop("tile_shape: `data` must be a data frame.")
  data <- tibble::as_tibble(data)

  aes <- normalize_aesthetics(aesthetics)
  st  <- normalize_stats(stats)

  value_cols <- aes$value
  value_stats <- st$value
  if (length(value_cols) == 0) {
    # Auto-default: pick all numeric columns as measures.
    value_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  }
  if (length(value_cols) == 0) {
    return(empty_tile_frame())
  }
  if (length(value_stats) == 0) value_stats <- "mean"

  # Facet grouping columns.
  facet_cols <- c(aes$rows, aes$cols)
  facet_cols <- facet_cols[nzchar(facet_cols)]
  facet_cols <- intersect(facet_cols, names(data))

  # Label-as-implicit-facet: if `label` is mapped to a column and
  # that column is not already a facet, treat it as rows. Handles
  # the "one-row-per-metric" shape (the kpis_with_goals case).
  if (length(facet_cols) == 0 &&
      nzchar(aes$label) && aes$label %in% names(data) &&
      !is.numeric(data[[aes$label]])) {
    facet_cols <- aes$label
    aes$rows <- aes$label
  }

  # Build one long frame: one row per (facet cell × measure × stat).
  chunks <- list()
  for (m in value_cols) {
    for (s in value_stats) {
      chunk <- summarise_one(data, m, s, facet_cols)
      chunk$.measure <- m
      chunk$.stat <- s
      chunks[[length(chunks) + 1L]] <- chunk
    }
  }
  out <- dplyr::bind_rows(chunks)

  # Fill .row / .col from the facet columns (character for stable
  # sorting on the JS side).
  out$.row <- if (length(aes$rows) > 0 && aes$rows %in% names(out)) {
    as.character(out[[aes$rows]])
  } else ""
  out$.col <- if (length(aes$cols) > 0 && aes$cols %in% names(out)) {
    as.character(out[[aes$cols]])
  } else ""

  # .label: use `label` aesthetic column if mapped, otherwise measure name.
  out$.label <- if (length(aes$label) > 0 && aes$label %in% names(out)) {
    as.character(out[[aes$label]])
  } else {
    pretty_label(out$.measure)
  }

  # .unit: use `unit` aesthetic column if mapped, otherwise empty.
  out$.unit <- if (length(aes$unit) > 0 && aes$unit %in% names(out)) {
    as.character(out[[aes$unit]])
  } else ""

  # Optional aesthetics: target, max, status.
  out$.target <- attach_scalar(data, aes$target, st$target, facet_cols, out)
  out$.max    <- attach_scalar(data, aes$max,    st$max,    facet_cols, out)
  out$.status <- attach_scalar_chr(data, aes$status, st$status, facet_cols, out)

  # .spark: list column of {x, y} — only for spark showcase.
  if (showcase == "spark") {
    out$.spark <- attach_spark(
      data,
      spark_value = aes$spark_value,
      spark_x     = aes$spark_x,
      facet_cols  = facet_cols,
      out         = out
    )
  } else {
    out$.spark <- vector("list", nrow(out))
  }

  # Format inference per measure (single call per measure for perf).
  fmt_by_measure <- list()
  for (m in unique(out$.measure)) {
    vals <- out$.value[out$.measure == m]
    fmt_by_measure[[m]] <- infer_format(m, vals)
  }
  out$.format <- vapply(out$.measure, function(m) fmt_by_measure[[m]]$kind,
    character(1))
  out$.digits <- vapply(out$.measure, function(m) fmt_by_measure[[m]]$digits,
    integer(1))

  # Apply format overrides from `formats` list if present.
  if (length(formats$value) > 0) {
    if (!is.null(formats$value$kind) && nzchar(formats$value$kind)) {
      out$.format <- formats$value$kind
    }
    if (!is.null(formats$value$digits) && is.finite(formats$value$digits)) {
      out$.digits <- as.integer(formats$value$digits)
    }
  }

  # Attach showcase + any facet columns that aren't already in output.
  attr(out, "showcase") <- showcase
  tibble::as_tibble(out)
}

# --- helpers ----------------------------------------------------------

normalize_aesthetics <- function(aes) {
  defaults <- list(
    value = character(), rows = "", cols = "", label = "", unit = "",
    status = "", target = "", spark_value = "", spark_x = "", max = ""
  )
  for (nm in names(defaults)) {
    if (is.null(aes[[nm]])) aes[[nm]] <- defaults[[nm]]
    # Coerce single-element lists to character for robustness.
    if (is.list(aes[[nm]])) aes[[nm]] <- unlist(aes[[nm]])
  }
  aes$value <- as.character(aes$value)
  aes$value <- aes$value[nzchar(aes$value)]
  for (nm in setdiff(names(defaults), "value")) {
    aes[[nm]] <- if (length(aes[[nm]]) == 0) "" else as.character(aes[[nm]])[1]
  }
  aes
}

normalize_stats <- function(st) {
  defaults <- list(
    value = "mean", target = "first", max = "first",
    spark_value = "identity", spark_x = "identity", status = "first"
  )
  for (nm in names(defaults)) {
    if (is.null(st[[nm]])) st[[nm]] <- defaults[[nm]]
    if (is.list(st[[nm]])) st[[nm]] <- unlist(st[[nm]])
  }
  st$value <- as.character(st$value)
  st$value <- st$value[nzchar(st$value)]
  if (length(st$value) == 0) st$value <- "mean"
  for (nm in setdiff(names(defaults), "value")) {
    st[[nm]] <- if (length(st[[nm]]) == 0) defaults[[nm]] else as.character(st[[nm]])[1]
  }
  st
}

#' Apply a stat function name to a numeric vector.
#' @noRd
apply_stat <- function(stat, col_expr, data) {
  if (stat == "count") return(nrow(data))
  if (stat == "n_distinct") return(dplyr::n_distinct(data[[col_expr]]))
  x <- data[[col_expr]]
  if (stat == "identity") return(x)
  if (stat == "first") return(dplyr::first(stats::na.omit(x)))
  if (stat == "last")  return(dplyr::last(stats::na.omit(x)))
  fn <- switch(stat,
    mean     = mean,
    sum      = sum,
    median   = stats::median,
    min      = min,
    max      = max,
    mean
  )
  fn(x, na.rm = TRUE)
}

#' Summarise one (measure, stat) across facets.
#' @noRd
summarise_one <- function(data, measure, stat, facet_cols) {
  if (!measure %in% names(data)) {
    return(tibble::tibble(.value = NA_real_))
  }
  if (length(facet_cols) == 0) {
    out <- tibble::tibble(.value = apply_stat(stat, measure, data))
    return(out)
  }
  grouped <- dplyr::group_by(data, dplyr::across(dplyr::all_of(facet_cols)))
  groups_df <- dplyr::group_keys(grouped)
  vals <- dplyr::group_split(grouped) |> vapply(function(d) {
    v <- apply_stat(stat, measure, d)
    if (length(v) != 1) v <- NA_real_
    as.numeric(v)
  }, numeric(1))
  groups_df$.value <- vals
  groups_df
}

#' Attach a scalar aesthetic (target, max) to the output frame.
#' @noRd
attach_scalar <- function(data, col, stat, facet_cols, out) {
  if (!nzchar(col) || !(col %in% names(data))) {
    return(rep(NA_real_, nrow(out)))
  }
  if (length(facet_cols) == 0) {
    v <- apply_stat(stat, col, data)
    return(rep(as.numeric(v), nrow(out)))
  }
  grouped <- dplyr::group_by(data, dplyr::across(dplyr::all_of(facet_cols)))
  groups_df <- dplyr::group_keys(grouped)
  vals <- dplyr::group_split(grouped) |> vapply(function(d) {
    v <- apply_stat(stat, col, d)
    if (length(v) != 1) v <- NA_real_
    as.numeric(v)
  }, numeric(1))
  key <- do.call(paste, c(groups_df[facet_cols], sep = "\u0001"))
  out_key <- do.call(paste, c(out[facet_cols], sep = "\u0001"))
  idx <- match(out_key, key)
  vals[idx]
}

attach_scalar_chr <- function(data, col, stat, facet_cols, out) {
  if (!nzchar(col) || !(col %in% names(data))) {
    return(rep(NA_character_, nrow(out)))
  }
  if (is.numeric(data[[col]]) || inherits(data[[col]], c("Date", "POSIXct"))) {
    return(as.character(attach_scalar(data, col, stat, facet_cols, out)))
  }
  # Character / factor / logical: take first-in-group.
  if (length(facet_cols) == 0) {
    return(rep(as.character(data[[col]][1]), nrow(out)))
  }
  grouped <- dplyr::group_by(data, dplyr::across(dplyr::all_of(facet_cols)))
  groups_df <- dplyr::group_keys(grouped)
  vals <- dplyr::group_split(grouped) |> vapply(function(d) {
    as.character(d[[col]][1])
  }, character(1))
  key <- do.call(paste, c(groups_df[facet_cols], sep = "\u0001"))
  out_key <- do.call(paste, c(out[facet_cols], sep = "\u0001"))
  idx <- match(out_key, key)
  vals[idx]
}

attach_spark <- function(data, spark_value, spark_x, facet_cols, out) {
  if (!nzchar(spark_value) || !(spark_value %in% names(data))) {
    return(vector("list", nrow(out)))
  }
  get_xs <- function(d) {
    if (nzchar(spark_x) && spark_x %in% names(d)) {
      as.character(d[[spark_x]])
    } else {
      as.character(seq_len(nrow(d)))
    }
  }
  if (length(facet_cols) == 0) {
    one <- list(x = get_xs(data), y = as.numeric(data[[spark_value]]))
    return(rep(list(one), nrow(out)))
  }
  grouped <- dplyr::group_by(data, dplyr::across(dplyr::all_of(facet_cols)))
  groups_df <- dplyr::group_keys(grouped)
  sparks <- dplyr::group_split(grouped) |> lapply(function(d) {
    list(x = get_xs(d), y = as.numeric(d[[spark_value]]))
  })
  key <- do.call(paste, c(groups_df[facet_cols], sep = "\u0001"))
  out_key <- do.call(paste, c(out[facet_cols], sep = "\u0001"))
  idx <- match(out_key, key)
  sparks[idx]
}

pretty_label <- function(x) {
  # Snake / dot case → Title Case
  x <- as.character(x)
  x <- gsub("[_.]", " ", x)
  tools::toTitleCase(x)
}

empty_tile_frame <- function() {
  tibble::tibble(
    .row = character(), .col = character(), .measure = character(),
    .stat = character(), .value = numeric(), .label = character(),
    .unit = character(), .target = numeric(), .max = numeric(),
    .status = character(), .spark = list(), .format = character(),
    .digits = integer()
  )
}
