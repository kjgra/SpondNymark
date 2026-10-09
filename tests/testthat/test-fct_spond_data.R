test_that("only accessible groups are kept, with only the fields the app needs", {
  d <- spond_session_data(fake_spond_groups(), fake_spond_profile(), c("Teamleder", "Trener"))
  expect_equal(names(d$groups), "G2016")
  g <- d$groups[["G2016"]]
  expect_equal(g$name, "Nymark G/J 2016")
  expect_equal(g$my_roles, "Teamleder")
  expect_equal(g$subgroups$name, c("Nymark Ulv G10", "Nymark Gaupe G10"))
  expect_equal(names(g$members), c("id", "first_name", "last_name", "profile_id", "kind", "roles"))
  expect_equal(g$members$kind, c("coach", "player"))
  expect_equal(g$members$roles, c("Teamleder", ""))
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

# A group with two trainers (one is parent of a player), a parent without a
# trainer role, a non-trainer role and a guardian without a Spond profile.
kinds_group <- function() {
  list(
    id = "G-k", name = "Lag",
    roles = list(list(id = "R1", name = "Teamleder"), list(id = "R3", name = "trener"),
                 list(id = "R4", name = "Materialforvalter")),
    members = list(
      list(id = "M-c1", firstName = "Kari", roles = list("R1"), profile = list(id = "P-c1")),
      list(id = "M-c2", firstName = "Per", roles = list("R3", "R1"), profile = list(id = "P-c2")),
      list(id = "M-a", firstName = "Anne", roles = list(), profile = list(id = "P-a")),
      list(id = "M-mat", firstName = "Ole", roles = list("R4"), profile = list(id = "P-mat")),
      list(id = "M-k1", firstName = "Ida", guardians = list(list(id = "GD-1", firstName = "Kari", profile = list(id = "P-c1")),
                                                         list(id = "GD-2", firstName = "Ukjent"))),
      list(id = "M-k2", firstName = "Jon", guardians = list(list(id = "GD-3", firstName = "Anne", profile = list(id = "P-a")))),
      list(id = "M-k3", firstName = "Siv", guardians = list(list(id = "GD-4", firstName = "Kari", profile = list(id = "P-c1"))))
    )
  )
}

test_that("members are trainers by role, other adults by being a guardian, else players", {
  k <- spond_member_kinds(kinds_group(), c("Teamleder", "Trener"))
  expect_equal(k$kind, c("coach", "coach", "adult", "player", "player", "player", "player"))
  expect_equal(k$roles, c("Teamleder", "Teamleder, trener", "", "", "", "", ""))
  # Only trainers' children are kept, as member ids
  expect_equal(k$parent_links, data.frame(parent_id = c("M-c1", "M-c1"), child_id = c("M-k1", "M-k3")))
})

test_that("member kinds handle empty groups and keep no guardian details", {
  k <- spond_member_kinds(list(id = "G", members = list()))
  expect_equal(k$kind, character())
  expect_equal(nrow(k$parent_links), 0)
  d <- spond_session_data(list(kinds_group()), list(id = "P-c1", firstName = "Kari"), "Teamleder", c("Teamleder", "Trener"))
  g <- d$groups[["G-k"]]
  expect_equal(g$parent_links$child_id, c("M-k1", "M-k3"))
  flat <- paste(unlist(d), collapse = " ")
  for (x in c("GD-1", "GD-3", "Ukjent")) expect_false(grepl(x, flat, fixed = TRUE), info = x)
})

test_that("the trainer role list comes from the config, separate from the access list", {
  expect_true(all(c("Teamleder", "Trener", "Lagleder", "Hjelpetrener") %in% coach_role_names()))
  expect_false("Materialforvalter" %in% coach_role_names())
})
