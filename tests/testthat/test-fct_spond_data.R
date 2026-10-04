test_that("only accessible groups are kept, with only the fields the app needs", {
  d <- spond_session_data(fake_spond_groups(), fake_spond_profile(), c("Teamleder", "Trener"))
  expect_equal(names(d$groups), "G2016")
  g <- d$groups[["G2016"]]
  expect_equal(g$name, "Nymark G/J 2016")
  expect_equal(g$my_roles, "Teamleder")
  expect_equal(g$subgroups$name, c("Nymark Ulv G10", "Nymark Gaupe G10"))
  expect_equal(names(g$members), c("id", "first_name", "last_name", "profile_id"))
  expect_equal(g$members$first_name, c("Kjetil", "Emma"))
  expect_equal(g$memberships, data.frame(member_id = "M-1", subgroup_id = "S-ulv"))
  expect_equal(d$profile, list(id = "P-me", first_name = "Kjetil", last_name = "Gramstad"))
  expect_equal(d$access$id, "G2016")
})

test_that("no personal data beyond names survives minimisation", {
  d <- spond_session_data(fake_spond_groups(), fake_spond_profile(), c("Teamleder"))
  flat <- paste(unlist(d, use.names = TRUE), names(unlist(d)), collapse = " ")
  for (secret in c("99999999", "12345678", "1980-01-01", "2016-03-04", "Gata 1", "k@example.no", "Mor", "Foreldre i 5B")) {
    expect_false(grepl(secret, flat, fixed = TRUE), info = secret)
  }
})

test_that("a user without trainer/leader roles gets no groups", {
  d <- spond_session_data(fake_spond_groups(), fake_spond_profile("P-other"), c("Teamleder"))
  expect_length(d$groups, 0)
  expect_equal(nrow(d$access), 0)
})
