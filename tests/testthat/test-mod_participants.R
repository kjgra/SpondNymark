testthat::test_that("mod_participants ui", {
  ui <- mod_participants_ui("test")
  golem::expect_shinytaglist(ui)
  fmls <- formals(mod_participants_ui)
  for (i in c("id")) {
    testthat::expect_true(i %in% names(fmls))
  }
})

testthat::test_that("mod_participants server", {
  testServer(mod_participants_server, args = list(id = "test"), {
    ns <- session$ns
    testthat::expect_true(inherits(ns, "function"))
    testthat::expect_true(grepl(id, ns("")))
    testthat::expect_true(grepl(id, ns("test")))
  })
})
