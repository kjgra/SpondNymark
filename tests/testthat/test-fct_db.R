test_that("connects on first use only", {
  n <- 0
  h <- db_handle(function() { n <<- n + 1; structure(list(), class = "fake_con") })
  expect_equal(n, 0)
  local_mocked_bindings(dbIsValid = function(con) TRUE, .package = "DBI")
  expect_equal(h$run(function(con) "ok"), "ok")
  h$run(function(con) "again")
  expect_equal(n, 1)
})

test_that("a dead connection is replaced and the statement retried once", {
  n <- 0
  alive <- TRUE
  h <- db_handle(function() { n <<- n + 1; alive <<- TRUE; structure(list(id = n), class = "fake_con") })
  local_mocked_bindings(
    dbIsValid = function(con) TRUE,
    dbGetQuery = function(con, sql, ...) if (alive) data.frame(x = 1) else stop("server closed the connection"),
    dbDisconnect = function(con, ...) TRUE,
    .package = "DBI"
  )
  h$run(function(con) "first")
  alive <- FALSE
  calls <- 0
  res <- h$run(function(con) {
    calls <<- calls + 1
    if (con$id == 1) stop("server closed the connection")
    "after reconnect"
  })
  expect_equal(res, "after reconnect")
  expect_equal(n, 2)
  expect_equal(calls, 2)
})

test_that("errors on a working connection are not retried", {
  h <- db_handle(function() structure(list(), class = "fake_con"))
  local_mocked_bindings(dbIsValid = function(con) TRUE, dbGetQuery = function(con, sql, ...) data.frame(x = 1),
                        .package = "DBI")
  calls <- 0
  expect_error(h$run(function(con) { calls <<- calls + 1; stop("duplicate key") }), "duplicate key")
  expect_equal(calls, 1)
})
