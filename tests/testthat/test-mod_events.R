testthat::test_that("mod_events ui", {
  ui <- mod_events_ui("test")
  golem::expect_shinytaglist(ui)
  fmls <- formals(mod_events_ui)
  for (i in c("id")) {
    testthat::expect_true(i %in% names(fmls))
  }
})

testthat::test_that("mod_events server", {
  testServer(mod_events_server, args = list(id = "test"), {
    ns <- session$ns
    testthat::expect_true(inherits(ns, "function"))
    testthat::expect_true(grepl(id, ns("")))
    testthat::expect_true(grepl(id, ns("test")))
  })
})
