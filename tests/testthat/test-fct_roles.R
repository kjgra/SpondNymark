test_that("rights: the superadmin has everything, others what app_roles gives", {
  expect_equal(user_rights("P-me", NULL, "P-me"), list(superadmin = TRUE, admin = TRUE, ai = TRUE, app = TRUE))
  expect_equal(user_rights("P-x", NULL, "P-me"), list(superadmin = FALSE, admin = FALSE, ai = FALSE, app = FALSE))
  r <- data.frame(spond_profile_id = "P-x", can_use_ai = TRUE, is_admin = FALSE, can_use_app = TRUE)
  expect_equal(user_rights("P-x", r, "P-me"), list(superadmin = FALSE, admin = FALSE, ai = TRUE, app = TRUE))
  r$is_admin <- TRUE
  r$can_use_app <- FALSE
  expect_true(user_rights("P-x", r, "")$admin)
  expect_true(user_rights("P-x", r, "")$app)                 # admins always have app access
})

test_that("only trainers with a Spond profile can get rights", {
  g <- fake_group()
  cand <- role_candidates(g)
  expect_equal(cand$profile_id, "P-me")       # Kjetil is the only trainer with a profile
  expect_equal(cand$roles, "Teamleder")
  expect_equal(nrow(role_candidates(list())), 0)
})

test_that("rights are read from the database, and missing rights do not stop the app", {
  con <- local_test_db()
  db <- db_handle(function() con)
  u <- reactiveVal(NULL)
  ds_set_role(con, list(superadmin = TRUE, admin = TRUE), "P-me", "P-x", "ai", TRUE, allowed_ids = "P-x")
  r <- rights_reactive(u, db, superadmin = "P-me")
  expect_equal(isolate(r()), no_rights())
  u(list(profile = list(id = "P-x")))
  expect_equal(isolate(r()), list(superadmin = FALSE, admin = FALSE, ai = TRUE, app = FALSE))
  broken <- db_handle(function() stop("ingen database"))
  r2 <- rights_reactive(reactive(list(profile = list(id = "P-x"))), broken, superadmin = "P-me")
  expect_message(expect_equal(isolate(r2()), no_rights()), "Lesing av rettigheter feilet")
})

test_that("rights are read again: a right taken away stops at once", {
  con <- local_test_db()
  db <- db_handle(function() con)
  boss <- list(superadmin = TRUE, admin = TRUE)
  ds_set_role(con, boss, "P-boss", "P-x", "ai", TRUE, allowed_ids = "P-x")
  ds_set_role(con, boss, "P-boss", "P-x", "app", TRUE, allowed_ids = "P-x")
  expect_true(rights_read("P-x", db, superadmin = "P-boss")$ai)
  ds_set_role(con, boss, "P-boss", "P-x", "ai", FALSE, allowed_ids = "P-x")
  expect_false(rights_read("P-x", db, superadmin = "P-boss")$ai)
  expect_true(rights_read("P-boss", db, superadmin = "P-boss")$admin)    # superadmin without the database
  expect_equal(rights_read(NULL, db), no_rights())
  broken <- db_handle(function() stop("ingen database"))
  expect_message(expect_null(rights_read("P-x", broken, superadmin = "P-boss")), "Lesing av rettigheter feilet")
  now <- rights_now_fn(reactive(list(profile = list(id = "P-x"))), broken, superadmin = "P-boss")
  expect_message(expect_equal(isolate(now()), no_rights()), "feilet")          # fails closed for actions
})

test_that("a trainer who loses app access is logged out of an open session", {
  con <- local_test_db()
  db <- db_handle(function() con)
  boss <- list(superadmin = TRUE, admin = TRUE)
  ds_set_role(con, boss, "P-boss", "P-x", "app", TRUE, allowed_ids = "P-x")
  out <- character()
  u <- reactiveVal(list(profile = list(id = "P-x")))
  testServer(function(input, output, session) {
    rights_watch(u, db, logout = function(m) out <<- c(out, m), superadmin = "P-boss", every_ms = 1000)
  }, {
    session$flushReact()
    expect_length(out, 0)
    ds_set_role(con, boss, "P-boss", "P-x", "app", FALSE, allowed_ids = "P-x")
    session$elapse(1001)
    expect_length(out, 1)
    expect_match(out, "ikke lenger tilgang")
  })
})
