# Overlaps cut to the worst level (`overlap = "worst"`), lanes on summary
# outer rows (`parent_lanes`) and outer rows that start open (`open`).

worst_fixture <- function() {
  data.frame(
    id = c("a", "a", "a", "a", "b"),
    term = c("x", "x", "y", "z", "x"),
    s = c(0, 10, 20, 60, 5),
    e = c(30, 20, 50, 70, 15),
    lvl = factor(c("low", "high", "mid", "low", "mid"),
                 levels = c("low", "mid", "high")),
    src = c("one", "one", "one", "two", "one"),
    stringsAsFactors = FALSE
  )
}

worst_spans <- function(...) {
  list(type = "spans", name = "Day", x = "s", xend = "e", color = "lvl",
       label = "term", ...)
}

# A row's segments as data: start, end and level.
seg_df <- function(m, label) {
  i <- match(label, m$prep$rows$.label)
  ss <- m$prep$rows$.s1_segs[[i]]
  data.frame(s = vapply(ss, `[[`, 0, "s"), e = vapply(ss, `[[`, 0, "e"),
             f = vapply(ss, function(x) as.integer(x$f), 0L))
}

test_that("worst cuts overlapping records into pieces of the highest level", {
  m <- sum_model(worst_fixture(), by = "id",
                 summaries = list(worst_spans(overlap = "worst")))
  expect_null(m$prep$err)
  a <- seg_df(m, "a")
  # low 0-10, high 10-20, mid 20-50 (over the low record's 20-30), a gap,
  # then low 60-70.
  expect_identical(a$s, c(0, 10, 20, 60))
  expect_identical(a$e, c(10, 20, 50, 70))
  expect_identical(a$f, c(1L, 3L, 2L, 1L))
  # One record alone is unchanged.
  expect_identical(nrow(seg_df(m, "b")), 1L)
  # The default draws every record.
  d <- sum_model(worst_fixture(), by = "id", summaries = list(worst_spans()))
  expect_identical(nrow(seg_df(d, "a")), 4L)
})

test_that("summary outer rows draw their lanes with parent_lanes", {
  sp <- worst_spans(lane = "src", overlap = "worst")
  m <- sum_model(worst_fixture(), by = c("id", "term"), group_rows = "summary",
                 summaries = list(sp))
  c1 <- m$cells$cols[[1]]
  pa <- which(m$prep$rows$.is_parent & m$prep$rows$.label == "a")
  # Off: the outer row pools only the main lane, which is empty here.
  expect_length(c1$sl[[pa]], 0L)
  m2 <- sum_model(worst_fixture(), by = c("id", "term"),
                  group_rows = "summary",
                  summaries = list(utils::modifyList(sp,
                                                     list(parent_lanes = TRUE))))
  c2 <- m2$cells$cols[[1]]
  lanes <- c2$sl[[pa]]
  expect_identical(vapply(lanes, function(l) l$n, ""), c("one", "two"))
  # The "one" lane is the patient's three records cut to the worst level.
  expect_length(lanes[[1]]$s, 3L)
})

test_that("open = all starts the outer rows open", {
  args <- list(by = c("id", "term"), group_rows = "summary",
               summaries = list(worst_spans()))
  shut <- do.call(sum_model, c(list(worst_fixture()), args))
  expect_true(table_groups_to_model(shut)$collapsed)
  open <- do.call(sum_model, c(list(worst_fixture()), args,
                               list(open = "all")))
  expect_null(table_groups_to_model(open)$collapsed)
})

test_that("nested inner rows keep their own data order under their outer row", {
  d <- data.frame(id = c("a", "a", "b", "b"), term = c("x", "y", "y", "x"),
                  s = c(0, 1, 0, 1), e = c(1, 2, 1, 2),
                  lvl = factor("low", levels = c("low", "mid", "high")),
                  stringsAsFactors = FALSE)
  m <- sum_model(d, by = c("id", "term"), group_rows = "summary",
                 sort_by = "data", sort_dir = "asc",
                 summaries = list(worst_spans()))
  r <- m$prep$rows
  expect_identical(r$.label, c("a", "x", "y", "b", "y", "x"))
})
