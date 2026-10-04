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

test_that(".Renviron.example lists every required variable", {
  example <- testthat::test_path("..", "..", ".Renviron.example")
  skip_if_not(file.exists(example), ".Renviron.example finnes bare i kildekoden")
  keys <- sub("=.*$", "", grep("^[A-Z_]+=", readLines(example), value = TRUE))
  expect_true(all(required_env_vars %in% keys))
})
