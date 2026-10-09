test_that("check_env passes when the required variables are set", {
  withr::local_envvar(SPONDNYMARK_DB_URL = "postgresql://u:p@h:5432/db")
  expect_true(check_env())
})

test_that("check_env names missing variables and never prints values", {
  withr::local_envvar(SPONDNYMARK_DB_URL = "", OTHER_SECRET = "hemmelig-verdi")
  err <- tryCatch(check_env(c("SPONDNYMARK_DB_URL", "OTHER_SECRET")), error = conditionMessage)
  expect_match(err, "SPONDNYMARK_DB_URL")
  expect_false(grepl("OTHER_SECRET", err))      # it is set, so not listed
  expect_false(grepl("hemmelig-verdi", err))
  expect_match(err, "Posit Connect")
})

test_that("the environment label is empty in production and shown otherwise", {
  expect_equal(app_env_label(""), "")
  expect_equal(app_env_label("prod"), "")
  expect_equal(app_env_label("Production"), "")
  expect_equal(app_env_label(" test "), "TEST")
  expect_equal(app_env_label("<script>"), "SCRIPT")
  expect_equal(app_title(""), "Nymark \u2013 Gruppeorganisering")
  expect_equal(app_title("test"), "Nymark \u2013 Gruppeorganisering (TEST)")
  expect_null(app_env_badge(""))
  expect_match(as.character(app_env_badge("test")), "sn-env")
})

test_that(".Renviron.example lists every required variable", {
  example <- testthat::test_path("..", "..", ".Renviron.example")
  skip_if_not(file.exists(example), ".Renviron.example finnes bare i kildekoden")
  keys <- sub("=.*$", "", grep("^[A-Z_]+=", readLines(example), value = TRUE))
  expect_true(all(required_env_vars %in% keys))
})
