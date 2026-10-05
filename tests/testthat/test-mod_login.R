testthat::test_that("mod_login ui", {
  ui <- mod_login_ui("test")
  testthat::expect_s3_class(ui, "shiny.tag.list")
  testthat::expect_true("id" %in% names(formals(mod_login_ui)))
  testthat::expect_s3_class(mod_login_bar_ui("test"), "shiny.tag")
})

testthat::test_that("successful login returns the user and minimised groups", {
  testServer(mod_login_server, args = list(spond = fake_spond_api(), role_names = c("Teamleder")), {
    expect_null(session$returned())
    session$setInputs(email = " trener@klubb.no ", password = "riktig", login = 1)
    u <- session$returned()
    expect_equal(u$profile$first_name, "Kjetil")
    expect_equal(names(u$groups), "G2016")
    expect_s3_class(u$spond, "spond_session")
    expect_equal(u$access$id, "G2016")
    expect_null(error_msg())
    expect_match(paste(output$bar$html), "Kjetil")
    expect_false(grepl("riktig", paste(capture.output(str(u)), collapse = "")))
  })
})

testthat::test_that("wrong password shows the message from Spond and keeps the user out", {
  testServer(mod_login_server, args = list(spond = fake_spond_api(), role_names = c("Teamleder")), {
    session$setInputs(email = "trener@klubb.no", password = "feil", login = 1)
    expect_null(session$returned())
    expect_match(error_msg(), "feil e-post eller passord")
  })
})

testthat::test_that("empty fields are caught before calling Spond", {
  api <- fake_spond_api()
  called <- FALSE
  api$login <- function(...) { called <<- TRUE; stop("should not be called") }
  testServer(mod_login_server, args = list(spond = api, role_names = c("Teamleder")), {
    session$setInputs(email = "", password = "", login = 1)
    expect_match(error_msg(), "både e-post og passord")
  })
  expect_false(called)
})

testthat::test_that("technical errors are not shown to the user", {
  testServer(mod_login_server,
             args = list(spond = fake_spond_api(fail_with = "Failed to perform HTTP request: timeout at 10.0.0.1"),
                         role_names = c("Teamleder")), {
    suppressMessages(session$setInputs(email = "trener@klubb.no", password = "riktig", login = 1))
    expect_null(session$returned())
    expect_match(error_msg(), "Fikk ikke kontakt med Spond")
    expect_false(grepl("10.0.0.1", error_msg(), fixed = TRUE))
  })
})

testthat::test_that("a user without a trainer/leader role sees why and gets no data", {
  testServer(mod_login_server,
             args = list(spond = fake_spond_api(profile_id = "P-other"), role_names = c("Teamleder", "Trener")), {
    session$setInputs(email = "trener@klubb.no", password = "riktig", login = 1)
    expect_null(session$returned())
    expect_equal(state()$status, "no_access")
    html <- paste(output$ui$html)
    expect_match(html, "Ingen tilgang")
    expect_match(html, "Teamleder, Trener")
    expect_null(state()$groups)
  })
})

testthat::test_that("logout forgets the user and the Spond session", {
  testServer(mod_login_server, args = list(spond = fake_spond_api(), role_names = c("Teamleder")), {
    session$setInputs(email = "trener@klubb.no", password = "riktig", login = 1)
    expect_false(is.null(session$returned()))
    session$setInputs(logout = 1)
    expect_null(session$returned())
    expect_equal(state(), list(status = "logged_out"))
  })
})

testthat::test_that("an expired Spond session logs the trainer out with an explanation", {
  testServer(mod_login_server, args = list(spond = fake_spond_api(), role_names = c("Teamleder")), {
    session$userData$sn_session_expired()          # not logged in: nothing happens
    expect_null(error_msg())
    session$setInputs(email = "trener@klubb.no", password = "riktig", login = 1)
    expect_false(is.null(session$returned()))
    session$userData$sn_session_expired()
    expect_null(session$returned())
    expect_match(error_msg(), "utløpt")
  })
})
