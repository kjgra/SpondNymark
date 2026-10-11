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
    expect_match(error_msg(), "feil e-post, mobilnummer eller passord")
  })
})

testthat::test_that("empty fields are caught before calling Spond", {
  api <- fake_spond_api()
  called <- FALSE
  api$login <- function(...) { called <<- TRUE; stop("should not be called") }
  testServer(mod_login_server, args = list(spond = api, role_names = c("Teamleder")), {
    session$setInputs(email = "", password = "", login = 1)
    expect_match(error_msg(), "både e-post eller mobilnummer og passord")
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
    session$setInputs(email = "trener@klubb.no", password = "riktig", login = 2)
    session$userData$sn_logout("Du har ikke lenger tilgang til appen.")
    expect_null(session$returned())
    expect_match(error_msg(), "ikke lenger tilgang")
  })
})

testthat::test_that("a mobile number can be used, and odd input is caught before Spond", {
  api <- fake_spond_api()
  seen <- NULL
  api$login <- function(username, password) { seen <<- username; structure(list(token = "t"), class = "spond_session") }
  testServer(mod_login_server, args = list(spond = api, role_names = c("Teamleder")), {
    session$setInputs(email = "998 87 766", password = "riktig", login = 1)
    expect_false(is.null(session$returned()))
  })
  expect_equal(seen, "+4799887766")
  seen <- NULL
  testServer(mod_login_server, args = list(spond = api, role_names = c("Teamleder")), {
    session$setInputs(email = "ola", password = "riktig", login = 1)
    expect_match(error_msg(), "mobilnummer med 8 siffer")
  })
  expect_null(seen)
})

testthat::test_that("someone without app access gets a clear message and no data", {
  asked <- NULL
  allow <- function(ident, profile_id) { asked <<- list(ident, profile_id); "denied" }
  testServer(mod_login_server, args = list(spond = fake_spond_api(), role_names = c("Teamleder"), allow = allow), {
    session$setInputs(email = "Trener@klubb.no", password = "riktig", login = 1)
    expect_null(session$returned())
    expect_equal(state()$status, "not_allowed")
    expect_null(state()$spond)
    html <- paste(output$ui$html)
    expect_match(html, "Ingen tilgang til appen")
    expect_match(html, "ikke fått tilgang")
    expect_match(html, "app-tilgang")
    session$setInputs(logout = 1)
    expect_equal(state(), list(status = "logged_out"))
  })
  expect_equal(asked[[1]]$value, "trener@klubb.no")
  expect_equal(asked[[2]], "P-me")
})

testthat::test_that("app access is not checked when Spond says no, and errors are explained", {
  asked <- FALSE
  allow <- function(ident, profile_id) { asked <<- TRUE; "ok" }
  testServer(mod_login_server, args = list(spond = fake_spond_api(), role_names = c("Teamleder"), allow = allow), {
    session$setInputs(email = "trener@klubb.no", password = "feil", login = 1)
    expect_match(error_msg(), "feil e-post, mobilnummer eller passord")
  })
  expect_false(asked)
  testServer(mod_login_server, args = list(spond = fake_spond_api(), role_names = c("Teamleder"),
                                           allow = function(ident, profile_id) "error"), {
    session$setInputs(email = "trener@klubb.no", password = "riktig", login = 1)
    expect_null(session$returned())
    expect_match(paste(output$ui$html), "Fikk ikke sjekket")
  })
  testServer(mod_login_server, args = list(spond = fake_spond_api(), role_names = c("Teamleder"),
                                           allow = function(ident, profile_id) "ok"), {
    session$setInputs(email = "trener@klubb.no", password = "riktig", login = 1)
    expect_false(is.null(session$returned()))
  })
})

testthat::test_that("failed logins are limited per browser, per account and in all", {
  t <- 1000
  stores <- list(session = login_throttle_new(), ident = login_throttle_new(), all = login_throttle_new())
  calls <- 0
  api <- fake_spond_api()
  login <- api$login
  api$login <- function(email, password) { calls <<- calls + 1; login(email, password) }
  testServer(mod_login_server, args = list(spond = api, role_names = c("Teamleder"), throttle = stores,
                                           now = function() t), {
    for (i in 1:5) session$setInputs(email = "trener@klubb.no", password = "feil", login = i)
    expect_equal(calls, 5)
    session$setInputs(password = "riktig", login = 6)       # blocked before Spond, even with the right password
    expect_equal(calls, 5)
    expect_match(error_msg(), "Prøv igjen om 1 minutt")
    t <<- t + 61
    session$setInputs(password = "riktig", login = 7)
    expect_equal(calls, 6)
    expect_false(is.null(session$returned()))               # success clears the failures
    expect_length(ls(stores$session), 0)
  })
  # The same account from new browser sessions: 10 failures per 15 min.
  for (s in 1:2) {
    testServer(mod_login_server, args = list(spond = api, role_names = c("Teamleder"),
                                             throttle = list(session = login_throttle_new(), ident = stores$ident,
                                                             all = stores$all), now = function() t), {
      for (i in 1:5) session$setInputs(email = "Trener@klubb.no", password = "feil", login = i)
    })
  }
  testServer(mod_login_server, args = list(spond = api, role_names = c("Teamleder"),
                                           throttle = list(session = login_throttle_new(), ident = stores$ident,
                                                           all = stores$all), now = function() t), {
    n <- calls
    session$setInputs(email = "trener@klubb.no", password = "riktig", login = 1)
    expect_equal(calls, n)
    expect_match(error_msg(), "minutter")
    session$setInputs(email = "annen@klubb.no", password = "feil", login = 2)   # another account still works
    expect_equal(calls, n + 1)
  })
  # Nothing but hashes is kept.
  expect_false(any(grepl("@", ls(stores$ident))))
})

testthat::test_that("the wait is worked out from the oldest failure in the window", {
  st <- list(all = login_throttle_new())
  lim <- list(all = list(max = 3, window = 100))
  for (x in c(10, 20, 30)) login_failed(st, c(all = "all"), now = x, limits = lim)
  expect_equal(login_wait(st, c(all = "all"), now = 40, limits = lim), 70)
  expect_equal(login_wait(st, c(all = "all"), now = 111, limits = lim), 0)
  expect_equal(login_wait_text(70), "2 minutter")
  expect_equal(login_wait_text(40), "40 sekunder")
})
