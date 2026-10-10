test_that("rights: the superadmin has everything, others what app_roles gives", {
  expect_equal(user_rights("P-me", NULL, "P-me"), list(superadmin = TRUE, admin = TRUE, ai = TRUE))
  expect_equal(user_rights("P-x", NULL, "P-me"), list(superadmin = FALSE, admin = FALSE, ai = FALSE))
  r <- data.frame(spond_profile_id = "P-x", can_use_ai = TRUE, is_admin = FALSE)
  expect_equal(user_rights("P-x", r, "P-me"), list(superadmin = FALSE, admin = FALSE, ai = TRUE))
  r$is_admin <- TRUE
  expect_true(user_rights("P-x", r, "")$admin)
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
  expect_equal(isolate(r()), list(superadmin = FALSE, admin = FALSE, ai = TRUE))
  broken <- db_handle(function() stop("ingen database"))
  r2 <- rights_reactive(reactive(list(profile = list(id = "P-x"))), broken, superadmin = "P-me")
  expect_message(expect_equal(isolate(r2()), no_rights()), "Lesing av rettigheter feilet")
})
