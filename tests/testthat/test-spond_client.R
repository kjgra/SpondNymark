login_ok <- function(req) {
  httr2::response_json(body = list(accessToken = list(token = "secret-token", expiration = "2026-10-05T10:00:00Z")))
}

test_that("login sends credentials to auth2/login and returns a session", {
  seen <- NULL
  session <- httr2::with_mocked_responses(function(req) {
    seen <<- req
    login_ok(req)
  }, spond_login("trener@klubb.no", "hemmelig"))

  expect_s3_class(session, "spond_session")
  expect_equal(session$token, "secret-token")
  expect_equal(seen$url, "https://api.spond.com/core/v1/auth2/login")
  body <- if (exists("req_get_body", asNamespace("httr2"))) httr2::req_get_body(seen) else seen$body$data
  expect_equal(body, list(email = "trener@klubb.no", password = "hemmelig"))
})

test_that("printing a session never shows the token", {
  session <- httr2::with_mocked_responses(login_ok, spond_login("a@b.no", "x"))
  out <- capture.output(print(session))
  expect_false(any(grepl("secret-token", out)))
})

test_that("wrong credentials give a clear Norwegian error", {
  expect_error(
    httr2::with_mocked_responses(function(req) httr2::response(401), spond_login("a@b.no", "feil")),
    "feil e-post, mobilnummer eller passord"
  )
})

test_that("a response without token reports field names only", {
  expect_error(
    httr2::with_mocked_responses(
      function(req) httr2::response_json(body = list(twoFactorRequired = TRUE, phone = "+47 999")),
      spond_login("a@b.no", "x")
    ),
    "twoFactorRequired, phone"
  )
  # ...and never the values
  err <- tryCatch(
    httr2::with_mocked_responses(
      function(req) httr2::response_json(body = list(phone = "+47 999")),
      spond_login("a@b.no", "x")
    ),
    error = conditionMessage
  )
  expect_false(grepl("999", err))
})

test_that("authenticated requests carry the bearer token", {
  session <- structure(list(token = "tok", base_url = spond_base_url()), class = "spond_session")
  req <- spond_request(session, "groups", "")
  expect_equal(req$url, "https://api.spond.com/core/v1/groups/")
  # httr2 >= 1.1 hides redacted headers behind req_get_headers(); older versions store them directly.
  hdrs <- if (exists("req_get_headers", asNamespace("httr2"))) {
    httr2::req_get_headers(req, redacted = "reveal")
  } else {
    req$headers
  }
  expect_equal(hdrs$Authorization, "Bearer tok")
})

test_that("event query uses Spond's parameter names and time format", {
  q <- spond_events_query(group_id = "G1", subgroup_id = "S1",
                          min_end = as.POSIXct("2026-10-04 12:30:00", tz = "UTC"),
                          max_events = 20)
  expect_equal(q$groupId, "G1")
  expect_equal(q$subGroupId, "S1")
  expect_equal(q$minEndTimestamp, "2026-10-04T12:30:00.000Z")
  expect_equal(q$max, "20")
  expect_equal(q$scheduled, "false")
  expect_null(q$maxEndTimestamp)
})

test_that("events are fetched from sponds/ with the query", {
  session <- structure(list(token = "tok", base_url = spond_base_url()), class = "spond_session")
  seen <- NULL
  res <- httr2::with_mocked_responses(function(req) {
    seen <<- req
    httr2::response_json(body = list(list(id = "E1", heading = "Trening")))
  }, spond_get_events(session, group_id = "G1", max_events = 5))
  expect_equal(res[[1]]$heading, "Trening")
  expect_match(seen$url, "^https://api.spond.com/core/v1/sponds/\\?")
  expect_match(seen$url, "groupId=G1")
  expect_match(seen$url, "max=5")
})

test_that("an expired session gives a clear error", {
  session <- structure(list(token = "tok", base_url = spond_base_url()), class = "spond_session")
  expect_error(
    httr2::with_mocked_responses(function(req) httr2::response(401), spond_get_profile(session)),
    "utløpt"
  )
})

test_that("our own errors have class spond_error so the app can show them", {
  expect_error(
    httr2::with_mocked_responses(function(req) httr2::response(401), spond_login("a@b.no", "feil")),
    class = "spond_error"
  )
})

test_that("an expired session has its own class, so the app can log out", {
  session <- structure(list(token = "tok", base_url = spond_base_url()), class = "spond_session")
  expect_error(
    httr2::with_mocked_responses(function(req) httr2::response(401), spond_get_events(session, group_id = "G1")),
    class = "spond_expired"
  )
})

test_that("a mobile number is sent in the email field as +47..., and junk is stopped", {
  seen <- NULL
  httr2::with_mocked_responses(function(req) { seen <<- req; login_ok(req) }, spond_login("+47 998 87 766", "x"))
  body <- if (exists("req_get_body", asNamespace("httr2"))) httr2::req_get_body(seen) else seen$body$data
  expect_equal(body, list(email = "+4799887766", password = "x"))
  expect_error(spond_login("ola", "x"), "e-postadressen eller mobilnummeret")
})
