# Who may log in (app access). E-mail and numbers below are made up.

test_that("e-mail and mobile numbers are normalised", {
  expect_equal(login_identifier(" Ola.Trener@Klubb.NO "), list(kind = "email", value = "ola.trener@klubb.no"))
  for (x in c("99887766", "998 87 766", "+47 998 87 766", "004799887766", "(+47) 998-87-766")) {
    expect_equal(login_identifier(x), list(kind = "phone", value = "+4799887766"), info = x)
  }
  expect_equal(login_identifier("+46 70 123 45 67")$value, "+46701234567")
  for (x in c("", "ola", "ola@", "@klubb.no", "ola@klubb", "1234567", "99887766a", "+47")) {
    expect_null(login_identifier(x), info = x)
  }
})

test_that("only the superadmin, admins and trainers with app access get in", {
  con <- local_test_db()
  db <- list(run = function(f) f(con))
  sa <- list(superadmin = TRUE, admin = TRUE)
  expect_equal(login_check(db, "P-ola", superadmin = "P-sa"), "denied")
  ds_set_role(con, sa, "P-sa", "P-ola", "app", TRUE, "P-ola")
  expect_equal(login_check(db, "P-ola", superadmin = "P-sa"), "ok")
  ds_set_role(con, sa, "P-sa", "P-ola", "app", FALSE, "P-ola")
  expect_equal(login_check(db, "P-ola", superadmin = "P-sa"), "denied")
  ds_set_role(con, sa, "P-sa", "P-kari", "ai", TRUE, "P-kari")
  expect_equal(login_check(db, "P-kari", superadmin = "P-sa"), "denied")     # KI alone is not app access
  ds_set_role(con, sa, "P-sa", "P-kari", "admin", TRUE, "P-kari")
  expect_equal(login_check(db, "P-kari", superadmin = "P-sa"), "ok")
  broken <- list(run = function(f) stop("nede"))
  expect_equal(login_check(broken, "P-sa", superadmin = "P-sa"), "ok")
  expect_equal(suppressMessages(login_check(broken, "P-ola", superadmin = "P-sa")), "error")
  expect_error(ds_set_role(con, list(superadmin = FALSE, admin = FALSE), "P-x", "P-ola", "app", TRUE, "P-ola"),
               "ikke tilgang")
})
