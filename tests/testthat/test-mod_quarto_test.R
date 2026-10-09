# TEMPORARY (T0). Remove together with R/mod_quarto_test.R.

test_that("the Quarto test is shown only outside production", {
  expect_false(quarto_test_enabled(""))
  expect_false(quarto_test_enabled("prod"))
  expect_true(quarto_test_enabled("test"))
  expect_null(mod_quarto_test_ui("qt", env = ""))
  expect_match(as.character(mod_quarto_test_ui("qt", env = "test")), "qt-run")
  expect_null(mod_quarto_test_server("qt", env = ""))
})

test_that("the test files are written as UTF-8", {
  dir <- withr::local_tempdir()
  png_error <- quarto_test_inputs(dir)
  expect_true(all(file.exists(file.path(dir, c("test.qmd", "mal.typ", "data.json")))))
  if (!nzchar(png_error)) expect_true(file.exists(file.path(dir, "skisse.png")))
  qmd <- readLines(file.path(dir, "test.qmd"), encoding = "UTF-8")
  expect_true(any(grepl("æøå", qmd)))
  expect_false(any(grepl("```", qmd)))   # no R chunks, so knitr is not needed
})

test_that("the report shows a missing Quarto without failing", {
  none <- list(ok = FALSE, status = NA, secs = NA, out = "Fant ikke Quarto.")
  res <- list(bin = "", path = "/usr/bin", r = "R", os = "Linux", png = "ok",
              pkgs = c(quarto = FALSE), version = none, typst_version = none,
              render = none, typst = none)
  html <- as.character(quarto_test_report(res))
  expect_match(html, "ikke funnet")
  expect_match(html, "Fant ikke Quarto")
})
