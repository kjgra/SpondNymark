test_that("query labels name the kind and the table, never values", {
  expect_equal(ds_sql_label("SELECT id, body FROM team_learnings WHERE spond_group_id = $1"), "db SELECT team_learnings")
  expect_equal(ds_sql_label("\n  INSERT INTO exercise_feedback (a) VALUES ('Noah')"), "db INSERT exercise_feedback")
  expect_equal(ds_sql_label("UPDATE maintenance SET last_run = now()"), "db UPDATE maintenance")
  expect_equal(ds_sql_label("WITH ends AS (SELECT 1 FROM training_plans) DELETE FROM training_plans p USING ends e"),
               "db WITH training_plans")
  expect_equal(ds_sql_label("SELECT 1"), "db SELECT")
  expect_false(grepl("Noah", ds_sql_label("INSERT INTO t (a) VALUES ('Noah')")))
})

test_that("timing lines follow SPONDNYMARK_TIMING", {
  withr::local_envvar(SPONDNYMARK_TIMING = "on")
  expect_message(timing_log("spond GET /x", 120.4, detail = "12 kB"), "^\\[tid\\] spond GET /x: 120 ms \\(12 kB\\)")
  expect_silent(timing_log("db SELECT x", 30, always = FALSE))          # fast query: not logged
  expect_message(timing_log("db SELECT x", 400, always = FALSE), "db SELECT x: 400 ms")
  expect_message(x <- timed("noe", 1 + 1), "\\[tid\\] noe: [0-9]+ ms")
  expect_equal(x, 2)
  expect_message(try(timed("feiler", stop("nei")), silent = TRUE), "feiler")   # logged also on error
  withr::local_envvar(SPONDNYMARK_TIMING = "all")
  expect_message(timing_log("db SELECT x", 30, always = FALSE), "30 ms")
  withr::local_envvar(SPONDNYMARK_TIMING = "off")
  expect_silent(timing_log("spond GET /x", 999))
  expect_silent(timing_note("ny økt"))
  expect_equal(timing_size(850), "850 B")
  expect_equal(timing_size(13000), "13 kB")
  expect_true(is.numeric(timing_process_age()))
})

test_that("Spond calls are timed with path, size and status, without the query", {
  withr::local_envvar(SPONDNYMARK_TIMING = "on")
  sess <- structure(list(token = "hemmelig", base_url = "https://api.example.no/core/v1/"), class = "spond_session")
  httr2::local_mocked_responses(function(req) {
    httr2::response(200, headers = list(`Content-Type` = "application/json"), body = charToRaw("[]"))
  })
  msgs <- testthat::capture_messages(spond_get_events(sess, group_id = "G1"))
  expect_match(msgs, "\\[tid\\] spond GET /core/v1/sponds/: [0-9]+ ms \\(2 B, HTTP 200\\)")
  expect_false(any(grepl("G1|hemmelig|groupId", msgs)))
})
