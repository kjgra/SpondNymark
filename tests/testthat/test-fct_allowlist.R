# Allowlist for login (migration 006). E-mail and numbers below are made up.

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

test_that("hashes are keyed and never contain the value", {
  a <- login_identifier("ola@klubb.no")
  h <- login_hash(a, key = "k1")
  expect_match(h, "^[0-9a-f]{64}$")
  expect_identical(h, login_hash(login_identifier("OLA@klubb.no "), key = "k1"))
  expect_false(identical(h, login_hash(a, key = "k2")))
  expect_false(identical(h, as.character(openssl::sha256("email:ola@klubb.no"))))   # not a plain hash
  expect_error(login_hash(a, key = ""), "SPONDNYMARK_LOGIN_KEY")
  expect_false(identical(login_hash(login_identifier("99887766"), "k1"), h))
})

test_that("hints are masked", {
  expect_equal(login_hint(login_identifier("ola.trener@klubb.no")), "o•••@klubb.no")
  expect_equal(login_hint(login_identifier("99887766")), "+47 •••• ••66")
  expect_false(grepl("9988", login_hint(login_identifier("99887766"))))
})

fake_db <- function(con) list(run = function(f) f(con))

test_that("the allowlist is kept by admins and checked at login", {
  con <- local_test_db()
  adm <- list(superadmin = FALSE, admin = TRUE)
  ola <- login_identifier("ola@klubb.no")
  expect_error(ds_add_allowlist(con, list(admin = FALSE), "P1", ola, key = "k"), "ikke tilgang")
  expect_error(ds_add_allowlist(con, adm, "P1", NULL, key = "k"), "e-postadresse eller et mobilnummer")
  expect_true(ds_add_allowlist(con, adm, "P1", ola, key = "k"))
  expect_false(ds_add_allowlist(con, adm, "P1", login_identifier("OLA@klubb.no"), key = "k"))
  expect_true(ds_add_allowlist(con, adm, "P1", login_identifier("99887766"), key = "k"))

  rows <- ds_list_allowlist(con, adm)
  expect_equal(nrow(rows), 2)
  expect_setequal(rows$kind, c("email", "phone"))
  # Nothing readable is stored.
  all_text <- paste(unlist(DBI::dbGetQuery(con, "SELECT * FROM login_allowlist")), collapse = " ")
  expect_false(grepl("ola@|9988", all_text))

  db <- fake_db(con)
  expect_equal(login_check(db, ola, "P-ola", superadmin = "P-sa", key = "k"), "ok")
  row <- DBI::dbGetQuery(con, "SELECT spond_profile_id, last_login_at FROM login_allowlist WHERE kind = 'email'")
  expect_equal(row$spond_profile_id, "P-ola")
  expect_false(is.na(row$last_login_at))
  expect_equal(login_check(db, login_identifier("kari@klubb.no"), "P-kari", superadmin = "P-sa", key = "k"), "denied")
  expect_equal(login_check(db, ola, "P-ola", superadmin = "P-sa", key = "annen"), "denied")   # changed key

  # Superadmin and admins always get in; a missing key stops everyone else.
  expect_equal(login_check(list(run = function(f) stop("nede")), NULL, "P-sa", superadmin = "P-sa", key = ""), "ok")
  ds_set_role(con, list(superadmin = TRUE, admin = TRUE), "P-sa", "P-kari", "admin", TRUE, "P-kari")
  expect_equal(login_check(db, login_identifier("kari@klubb.no"), "P-kari", superadmin = "P-sa", key = "k"), "ok")
  expect_equal(suppressMessages(login_check(db, ola, "P-ola", superadmin = "P-sa", key = "")), "error")
  expect_equal(suppressMessages(login_check(list(run = function(f) stop("nede")), ola, "P-ola", "P-sa", "k")), "error")

  ds_remove_allowlist(con, adm, "P2", rows$id_hash[rows$kind == "email"])
  expect_equal(login_check(db, ola, "P-ola", superadmin = "P-sa", key = "k"), "denied")
  log <- DBI::dbGetQuery(con, "SELECT action, actor, hint FROM login_allowlist_log ORDER BY id")
  expect_equal(log$action, c("add", "add", "remove"))
  expect_equal(log$actor[3], "P2")
  expect_equal(log$hint[3], "o•••@klubb.no")
  expect_error(ds_remove_allowlist(con, list(admin = FALSE), "P2", rows$id_hash[1]), "ikke tilgang")
})
