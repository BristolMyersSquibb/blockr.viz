# A realistic worst case for the pptx table exporter: long stub labels
# (SOC / preferred terms), four arms, two leaf columns each, Big-N headers.
# Shared by the layout study so every variant is measured on the same table.
#
# `sections = FALSE` (default) is the shape the table block emits today: the
# SOC is a bold row of the stub, carrying its own counts, and the preferred
# terms indent under it -- so the stub column has to be wide enough for the
# longest SOC name. `sections = TRUE` is the same data with the SOC lifted
# into a merged full-width section row (the annotated-df `.group1_level`
# route), which is one of the layout ideas under study.

demo_ae_table <- function(n_soc = 6L, sections = FALSE, arms = NULL) {

  soc <- c(
    "Gastrointestinal disorders",
    "General disorders and administration site conditions",
    "Infections and infestations",
    "Musculoskeletal and connective tissue disorders",
    "Skin and subcutaneous tissue disorders",
    "Blood and lymphatic system disorders"
  )
  pt <- list(
    c("Nausea", "Vomiting", "Diarrhoea", "Abdominal pain upper",
      "Constipation", "Gastrooesophageal reflux disease"),
    c("Fatigue", "Pyrexia", "Oedema peripheral", "Asthenia",
      "Infusion site erythema"),
    c("Upper respiratory tract infection", "Urinary tract infection",
      "Nasopharyngitis", "Pneumonia"),
    c("Arthralgia", "Back pain", "Musculoskeletal chest pain",
      "Pain in extremity"),
    c("Rash maculo-papular", "Pruritus", "Alopecia", "Dry skin"),
    c("Anaemia", "Neutropenia", "Thrombocytopenia")
  )

  soc <- soc[seq_len(n_soc)]
  pt <- pt[seq_len(n_soc)]

  if (sections) {
    lab <- unlist(pt)
    grp <- rep(soc, lengths(pt))
    is_soc <- rep(FALSE, length(lab))
    ind <- rep(0L, length(lab))
  } else {
    lab <- unlist(lapply(seq_along(soc), function(i) c(soc[[i]], pt[[i]])))
    grp <- rep(soc, lengths(pt) + 1L)
    is_soc <- lab %in% soc
    ind <- ifelse(is_soc, 0L, 1L)
  }
  n <- length(lab)

  if (is.null(arms)) arms <- names(demo_arm_n)
  bigN <- demo_arm_n[arms]

  set.seed(42)
  out <- data.frame(
    .label = lab,
    .indent = ind,
    .strong = is_soc,
    stringsAsFactors = FALSE
  )
  if (sections) {
    out$.group1 <- "SOC"
    out$.group1_level <- grp
  }

  for (a in seq_along(arms)) {
    cnt <- pmax(1L, stats::rpois(n, if (sections) 9 else ifelse(is_soc, 30, 9)))
    pct <- round(100 * cnt / bigN[[a]], 1)
    ev <- cnt + stats::rpois(n, 4)
    top <- sprintf("%s (N=%d)", arms[[a]], bigN[[a]])

    out[[paste0(top, "||n (%)")]] <-
      structure(sprintf("%d (%.1f%%)", cnt, pct), label = "n (%)")
    out[[paste0(top, "||Events")]] <-
      structure(as.character(ev), label = "Events")
  }

  attr(out$.label, "label") <- if (sections) "Preferred term" else
    "System organ class / Preferred term"
  attr(out, "label") <-
    "Adverse events by system organ class and preferred term"
  attr(out, "subtitle") <- "Safety analysis set"
  attr(out, "caption") <- paste(
    "Percentages are based on the number of subjects in the safety analysis",
    "set. A subject is counted once per preferred term."
  )
  out
}

demo_arm_n <- c("Placebo" = 143, "Drug A 50 mg QD" = 141,
                "Drug A 100 mg QD" = 138, "Drug A 100 mg BID" = 140)
