# The table block in every layout and look on one dock board, for the
# affordance sweep (test-table-sweep.R, sweep.js). Small synthetic data, so a
# full sweep runs in a few minutes.
#
# Loads blockr.viz from BLOCKR_VIZ_SRC (the source tree under test) when set;
# library(blockr.viz) would test whatever version is installed.

src <- Sys.getenv("BLOCKR_VIZ_SRC")
if (nzchar(src)) pkgload::load_all(src, quiet = TRUE) else library(blockr.viz)
library(blockr.core)
library(blockr.dock)

options(blockr.dock_is_locked = FALSE)

# --- data ---------------------------------------------------------------------

set.seed(1)
arms <- c("Placebo", "Low dose", "High dose")
n <- 90
adsl <- data.frame(
  USUBJID = sprintf("S-%03d", seq_len(n)),
  ARM = factor(rep(arms, each = n / 3), levels = arms),
  AGE = round(rnorm(n, 72, 8)),
  SEX = sample(c("F", "M"), n, replace = TRUE),
  RACE = sample(c("White", "Black", "Asian"), n, replace = TRUE,
                prob = c(0.7, 0.2, 0.1)),
  stringsAsFactors = FALSE
)
adsl$AGEGR1 <- ifelse(adsl$AGE < 65, "<65", ifelse(adsl$AGE <= 80, "65-80", ">80"))
# A few unknown races, so the variables layout's "Missing" has rows to show.
adsl$RACE[c(5, 50)] <- NA

terms <- data.frame(
  AEDECOD = c("Headache", "Dizziness", "Nausea", "Vomiting", "Rash",
              "Pruritus", "Cough", "Fatigue"),
  AESOC = c("Nervous system", "Nervous system", "Gastrointestinal",
            "Gastrointestinal", "Skin", "Skin", "Respiratory", "General"),
  stringsAsFactors = FALSE
)
k <- rpois(n, 2)
adae <- data.frame(
  USUBJID = rep(adsl$USUBJID, k),
  ARM = rep(adsl$ARM, k),
  stringsAsFactors = FALSE
)
idx <- sample(nrow(terms), nrow(adae), replace = TRUE)
adae$AEDECOD <- terms$AEDECOD[idx]
adae$AESOC <- terms$AESOC[idx]
adae$AESEV <- factor(sample(c("MILD", "MODERATE", "SEVERE"), nrow(adae),
                            replace = TRUE, prob = c(0.6, 0.3, 0.1)),
                     levels = c("MILD", "MODERATE", "SEVERE"))

# Subject rows stacked on AE rows, as the cards demo does: each summary reads
# the rows that carry its column.
cards <- rbind(
  data.frame(USUBJID = adsl$USUBJID, ARM = adsl$ARM, SEX = adsl$SEX,
             AGE = adsl$AGE, AESEV = factor(NA, levels = levels(adae$AESEV)),
             AE = NA_real_),
  data.frame(USUBJID = adae$USUBJID, ARM = adae$ARM,
             SEX = adsl$SEX[match(adae$USUBJID, adsl$USUBJID)],
             AGE = NA_real_, AESEV = adae$AESEV, AE = 1)
)

arm_frame <- data.frame(
  Arm = arms,
  Subjects = as.numeric(table(adsl$ARM)),
  `Mean age` = round(tapply(adsl$AGE, adsl$ARM, mean), 1),
  check.names = FALSE
)

iris_sum <- aggregate(cbind(Sepal.Length, Petal.Length) ~ Species,
                      data = iris, FUN = function(x) round(mean(x), 2))
iris_sum$Size <- c("small", "medium", "large")
# A row without a group, so the nested table's "Missing" has a row to place.
iris_sum <- rbind(iris_sum, data.frame(Species = "hybrid", Sepal.Length = 6.1,
                                       Petal.Length = 4.4, Size = NA))

cor_mat <- as.data.frame(round(stats::cor(mtcars[, 1:7]), 2))
cor_mat <- cbind(var = rownames(cor_mat), cor_mat)
rownames(cor_mat) <- NULL

iris_summaries <- list(
  list(type = "simple", func = "count", show = "bar", name = "Flowers"),
  list(type = "dist", col = "Sepal.Length", show = "box", name = "Sepal"),
  list(type = "simple", func = "mean", col = "Petal.Length", show = "number",
       name = "Petal")
)

# --- board --------------------------------------------------------------------

board <- new_dock_board(
  blocks = c(
    iris = new_static_block(iris, block_name = "Iris"),
    iris_sum = new_static_block(iris_sum, block_name = "Iris, summarised"),
    cor_mat = new_static_block(cor_mat, block_name = "Correlations"),
    adsl = new_static_block(adsl, block_name = "Subjects"),
    adae = new_static_block(adae, block_name = "AEs"),
    cards = new_static_block(cards, block_name = "Subjects and AEs"),
    arm_frame = new_static_block(arm_frame, block_name = "Arms"),

    listing = new_table_block(
      rowname = "Species", drill = "Species",
      shadings = list(list(mode = "sequential", cols = "Sepal.Length")),
      title = "Iris", block_name = "Listing"
    ),
    nested = new_table_block(by = "Size", rowname = "Species",
                             title = "Nested", block_name = "Nested"),
    cor = new_table_block(
      rowname = "var", look = "compact",
      shadings = list(list(mode = "diverging", domain = c(-1, 1))),
      title = "Correlations", block_name = "Correlation"
    ),

    groups = new_table_block(
      layout = "groups", by = "Species", summaries = iris_summaries,
      title = "Iris by species", block_name = "Groups"
    ),
    ae_groups = new_table_block(
      layout = "groups", by = c("AESOC", "AEDECOD"), id = "USUBJID",
      summaries = list(list(type = "simple", func = "count_distinct",
                            col = "USUBJID", show = "bar", facet = "ARM",
                            name = "Subjects")),
      drill = "auto", title = "AEs by organ class", block_name = "AE summary"
    ),

    demog = new_table_block(
      layout = "variables", by = "ARM", id = "USUBJID",
      summaries = list(
        list(type = "dist", col = "AGE", show = "box"),
        list(type = "simple", func = "count", show = "bar", color = "SEX",
             name = "SEX"),
        list(type = "simple", func = "count", show = "bar", color = "RACE",
             name = "RACE")
      ),
      total = TRUE, title = "Demographics", block_name = "Table 1"
    ),

    # The sentence names the first summary's facet and fill and the outer
    # `by` column: each word must change the table, not only the sentence.
    heat = new_table_block(
      layout = "groups", by = c("ARM", "USUBJID"),
      summaries = list(list(type = "simple", func = "count", show = "tile",
                            facet = "AEDECOD", fill = "AESEV", name = "AEs")),
      look = "compact", title = "AEs by subject",
      subtitle = "{label(@facet)}[, painted by {label(@fill)}][, by {label(@group)}]",
      block_name = "Heatmap"
    ),

    dash = new_table_block(
      layout = "groups", by = "ARM", look = "cards", drill = "ARM",
      summaries = list(
        list(type = "simple", name = "Subjects", func = "count_distinct",
             col = "USUBJID", show = "bar"),
        list(type = "dist", name = "Age", col = "AGE", style = "box",
             summary = "median_q1_q3"),
        list(type = "simple", name = "AE events", func = "sum", col = "AE",
             color = "AESEV", show = "bar")
      ),
      title = "By arm", block_name = "Cards by arm"
    ),
    arm_cards = new_table_block(rowname = "Arm", look = "cards",
                                title = "Arms", block_name = "Records cards")
  ),
  links = links(
    from = c("iris", "iris_sum", "cor_mat", "iris", "adae", "adsl", "adae",
             "cards", "arm_frame"),
    to = c("listing", "nested", "cor", "groups", "ae_groups", "demog", "heat",
           "dash", "arm_cards")
  ),
  views = list(
    records = dock_view(c("listing", "nested", "cor"), name = "1. Records"),
    groups = dock_view(c("groups", "ae_groups"), name = "2. Groups"),
    variables = dock_view("demog", name = "3. Variables"),
    heat = dock_view("heat", name = "4. Heatmap"),
    cards = dock_view(c("dash", "arm_cards"), name = "5. Cards")
  ),
  active = "records",
  options = dock_board_options()
)

# Test-only: each table block's layout as the board holds it
# (test-table-layout-switch.R). exportTestValues() does nothing outside
# Shiny's test mode.
class(board) <- c("table_sweep_board", class(board))
registerS3method(
  "blockr_test_exports", "table_sweep_board",
  function(x, rv, ...) {
    NextMethod()
    shiny::exportTestValues(layouts = {
      blocks <- rv[["board"]]$blocks
      lays <- lapply(blocks, function(b) {
        st <- b$server$state
        if (is.function(st$layout)) st$layout()
      })
      Filter(Negate(is.null), lays)
    })
  },
  envir = asNamespace("blockr.core")
)

serve(board)
