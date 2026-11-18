# A select whose annotation names its choices (`labels = ...`, which may
# read the script's own values): the gear and the subtitle show the labels,
# the script stores the levels.

lab_script <- function(labels = "names(fields)") {
  paste(
    paste0("fields <- stats::setNames(c(\"AETOXGR\", \"AESEV\"), ",
           "c(\"Toxicity grade\", \"Severity\"))"),
    paste0("f <- factor(\"AESEV\", levels = c(\"AETOXGR\", \"AESEV\"))  ",
           "#| select(label = \"Colour by\", labels = ", labels, ")"),
    "data",
    sep = "\n"
  )
}

test_that("a labelled select shows its labels and stores its levels", {
  specs <- cb_specs(cb_parse(lab_script()), data.frame(x = 1))
  expect_identical(specs[[1]]$choices, c("AETOXGR", "AESEV"))
  expect_identical(specs[[1]]$labels, c("Toxicity grade", "Severity"))
  rec <- script_control_records(specs)[[1]]
  expect_identical(unlist(rec$options), c("AETOXGR", "AESEV"))
  expect_identical(unlist(rec$labels), c("Toxicity grade", "Severity"))
  # The subtitle word is the label; the stored value the level.
  expect_identical(script_title_args(list(), specs)$f, "Severity")
  set <- viz_script_values(list(f = "AETOXGR"), specs)
  expect_identical(as.character(set$f), "AETOXGR")
  expect_identical(script_title_args(list(), specs, set)$f, "Toxicity grade")
  # A saved level the choices lack falls back to the declared one.
  specs2 <- cb_specs(cb_parse(lab_script()), data.frame(x = 1),
                     values = list(f = "AEACN"))
  expect_identical(script_title_args(list(), specs2)$f, "Severity")
  # Literal labels work too.
  specs3 <- cb_specs(cb_parse(lab_script("c(\"G\", \"S\")")),
                     data.frame(x = 1))
  expect_identical(specs3[[1]]$labels, c("G", "S"))
})

test_that("labels of the wrong length, or that fail, are left out", {
  for (lab in c("\"A\"", "stop(\"no\")")) {
    specs <- cb_specs(cb_parse(lab_script(lab)), data.frame(x = 1))
    expect_null(specs[[1]]$labels)
    expect_null(script_control_records(specs)[[1]]$labels)
    expect_identical(script_title_args(list(), specs)$f, "AESEV")
  }
})

test_that("a multi-select's notes, placeholder and long picks", {
  script <- paste(
    paste0("terms <- paste(\"PT:\", c(\"Rash\", \"Nausea\", \"Fatigue\", ",
           "\"Cough\", \"Fall\"))"),
    paste0("ae_terms <- factor(character(), levels = terms)  #| select(",
           "label = \"Showing\", multiple = TRUE, ",
           "labels = sub(\"PT: \", \"\", terms), ",
           "notes = paste(5:1, \"patients\"), placeholder = \"all\")"),
    "data",
    sep = "\n"
  )
  specs <- cb_specs(cb_parse(script), data.frame(x = 1))
  expect_identical(specs[[1]]$notes, paste(5:1, "patients"))
  rec <- script_control_records(specs)[[1]]
  expect_identical(unlist(rec$notes), paste(5:1, "patients"))
  # No pick reads as the placeholder, not "none".
  expect_identical(script_title_args(list(), specs)$ae_terms, "all")
  # The picks by their labels, three, then how many more.
  set <- viz_script_values(
    list(ae_terms = c("PT: Rash", "PT: Nausea", "PT: Fatigue", "PT: Cough")),
    specs
  )
  expect_identical(script_title_args(list(), specs, set)$ae_terms,
                   "Rash, Nausea, Fatigue +1 more")
  # Every choice picked reads "all", though the word shows labels.
  every <- viz_script_values(list(ae_terms = specs[[1]]$choices), specs)
  expect_identical(script_title_args(list(), specs, every)$ae_terms, "all")
  # Notes of the wrong length are left out.
  bad <- sub("5:1", "1:2", script, fixed = TRUE)
  expect_null(cb_specs(cb_parse(bad), data.frame(x = 1))[[1]]$notes)
})
