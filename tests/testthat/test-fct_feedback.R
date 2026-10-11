ref_plan <- function() {
  plan_validate(jsonlite::read_json(testthat::test_path("..", "..", "inst", "extdata", "referanse-okt.json"),
                                    simplifyVector = FALSE))
}

test_that("the approved version is evaluated, otherwise the newest", {
  expect_null(feedback_plan_id(NULL))
  v <- data.frame(id = c(9L, 8L, 7L), status = c("draft", "approved", "draft"))
  expect_equal(feedback_plan_id(v), 8L)
  v$status[2] <- "draft"
  expect_equal(feedback_plan_id(v), 9L)
})

test_that("an evaluation is checked and cleaned", {
  p <- ref_plan()
  rows <- list(
    list(exercise_no = 1, rating = "3", tags = c("goy", "ko"), actual_minutes = "17", body = " Bra flyt ", use_again = "yes"),
    list(exercise_no = 2, rating = NULL, tags = NULL, actual_minutes = "15", body = "", use_again = NULL),   # not filled in
    list(exercise_no = NA, rating = "2", actual_players = "18", body = "Litt kø"))
  fb <- feedback_validate(rows, p, names = c("Emma Haugen", "Emma"))
  expect_equal(nrow(fb), 2)
  expect_equal(fb$exercise_no, c(1L, NA))
  expect_equal(fb$exercise_code[1], p$ovelser[[1]]$kode)
  expect_equal(fb$tags[[1]], c("goy", "ko"))
  expect_equal(fb$body[1], "Bra flyt")
  expect_equal(fb$actual_players[2], 18L)
  expect_true(is.na(fb$actual_minutes[2]))

  bad <- function(r, pattern) expect_error(feedback_validate(list(r), p, names = c("Emma")), pattern)
  bad(list(exercise_no = 1, rating = NULL, tags = "goy"), "velg Bra, OK eller Dårlig")
  bad(list(exercise_no = 1, rating = "4"), "vurderingen")
  bad(list(exercise_no = 1, rating = "3", tags = "fin"), "ukjent stikkord")
  bad(list(exercise_no = 1, rating = "3", actual_minutes = "2.5"), "faktisk tid")
  bad(list(exercise_no = 1, rating = "3", body = strrep("x", 301)), "maks 300")
  bad(list(exercise_no = 1, rating = "3", body = "Han er skadet"), "sensitive")
  bad(list(exercise_no = 1, rating = "3", body = "Emma stod i mål"), "Skriv uten navn")
  bad(list(exercise_no = 9, rating = "3"), "Ukjent øvelse")
  bad(list(exercise_no = 1, rating = "3", use_again = "kanskje"), "Bruke igjen")
  expect_error(feedback_validate(list(list(exercise_no = 1)), p), "minst én")
  # The session needs no rating: the number of players alone is kept
  s <- feedback_validate(list(list(exercise_no = NA, rating = NULL, actual_players = "12")), p)
  expect_true(is.na(s$rating))
  expect_equal(s$actual_players, 12L)
})

test_that("evaluations are summed up per exercise", {
  p <- ref_plan()
  fb <- data.frame(plan_id = 1L, exercise_no = c(1L, 1L, NA, 2L), exercise_code = "",
                   rating = c(3L, 2L, 3L, 1L), actual_minutes = c(17L, 19L, NA, NA), actual_players = c(NA, NA, 18L, NA),
                   body = c("Bra", "", "Fin økt", ""), use_again = c("yes", "yes", "", "no"),
                   author = c("P-me", "P-x", "P-me", "P-x"))
  fb$tags <- list(c("goy", "ko"), "ko", character(), "kjedelig")
  sm <- feedback_summary(fb, p)
  expect_equal(sm$session$players, 18)
  expect_equal(sm$exercises[[1]]$n, 2)
  expect_equal(sm$exercises[[1]]$ratings, c(`3` = 1L, `2` = 1L))
  expect_equal(names(sm$exercises[[1]]$tags)[1], "ko")              # most first
  expect_equal(sm$exercises[[1]]$minutes, 18)
  expect_equal(sm$exercises[[1]]$use_again, c(yes = 2L))
  expect_equal(sm$exercises[[2]]$mean, 1)
  expect_equal(sm$exercises[[3]]$n, 0)
  expect_setequal(sm$authors, c("P-me", "P-x"))
  expect_match(feedback_ratings_text(sm$exercises[[1]]$ratings), "1 · .* 1")
})
