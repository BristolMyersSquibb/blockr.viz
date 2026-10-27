# Data for the drill board (app.R) and for the counts test-table-drill.R
# expects. Deterministic and with unequal group sizes, so a drill that lands
# on the wrong key gives a different row count.

drill_arms <- c("Placebo", "Low dose", "High dose")

drill_adsl <- local({
  n <- 60
  i <- seq_len(n)
  arm <- drill_arms[c(rep(1, 30), rep(2, 18), rep(3, 12))]
  sex <- ifelse(i %% 3 == 0, "M", "F")
  sex[c(5, 11, 17, 23, 41)] <- NA
  data.frame(
    USUBJID = sprintf("S-%03d", i),
    ARM = factor(arm, levels = drill_arms),
    SEX = sex,
    AGE = 50 + (i * 7) %% 30,
    stringsAsFactors = FALSE
  )
})

drill_terms <- data.frame(
  AEDECOD = c("Headache", "Dizziness", "Nausea", "Vomiting", "Rash", "Cough"),
  AESOC = c("Nervous system", "Nervous system", "Gastrointestinal",
            "Gastrointestinal", "Skin", "Respiratory"),
  stringsAsFactors = FALSE
)

drill_adae <- local({
  k <- (seq_len(nrow(drill_adsl)) %% 4)
  subj <- rep(seq_len(nrow(drill_adsl)), k)
  j <- seq_along(subj)
  term <- c(1, 1, 1, 1, 2, 2, 3, 3, 3, 4, 5, 5, 6)[(j %% 13) + 1]
  data.frame(
    USUBJID = drill_adsl$USUBJID[subj],
    ARM = drill_adsl$ARM[subj],
    AEDECOD = drill_terms$AEDECOD[term],
    AESOC = drill_terms$AESOC[term],
    AESEV = factor(c("MILD", "MODERATE", "SEVERE")[(j %% 3) + 1],
                   levels = c("MILD", "MODERATE", "SEVERE")),
    stringsAsFactors = FALSE
  )
})

drill_arm_frame <- data.frame(
  Arm = drill_arms,
  Subjects = as.numeric(table(drill_adsl$ARM)),
  stringsAsFactors = FALSE
)
