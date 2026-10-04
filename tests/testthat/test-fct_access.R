# Fixture shaped like Spond's group data (fields from the documented data model).
fake_groups <- function() {
  list(
    list(
      id = "G1", name = "Nymark J16",
      roles = list(list(id = "R1", name = "Trener"), list(id = "R2", name = "Lagleder"), list(id = "R3", name = "Materialforvalter")),
      subGroups = list(list(id = "S1", name = "Lag 1"), list(id = "S2", name = "Keepere")),
      members = list(
        list(id = "M1", roles = list("R1"), subGroups = list("S1"), profile = list(id = "P-trainer")),
        list(id = "M2", roles = list(), subGroups = list("S1", "S2")),
        list(id = "M3", subGroups = list("S2"),
             guardians = list(list(id = "GD1", profile = list(id = "P-parent"))))
      )
    ),
    list(
      id = "G2", name = "Nymark G14",
      roles = list(list(id = "R9", name = "Materialforvalter")),
      members = list(list(id = "M9", roles = list("R9"), profile = list(id = "P-trainer")))
    ),
    list(
      id = "G3", name = "Annen gruppe",
      members = list(list(id = "M10", profile = list(id = "P-trainer")))
    )
  )
}

test_that("member is found from profile id", {
  g <- fake_groups()[[1]]
  expect_equal(spond_member_for_profile(g, "P-trainer")$id, "M1")
  expect_null(spond_member_for_profile(g, "P-unknown"))
  # A guardian is not a member of the group
  expect_null(spond_member_for_profile(g, "P-parent"))
})

test_that("role ids are translated to role names", {
  g <- fake_groups()[[1]]
  expect_equal(spond_member_role_names(g, g$members[[1]]), "Trener")
  expect_equal(spond_member_role_names(g, g$members[[2]]), character())
  expect_equal(spond_member_role_names(g, NULL), character())
})

test_that("only groups where the user has a trainer/leader role are accessible", {
  acc <- spond_accessible_groups(fake_groups(), list(id = "P-trainer"), c("Trener", "Lagleder"))
  expect_equal(acc$id, "G1")
  expect_equal(acc$roles, "Trener")
  expect_equal(acc$n_subgroups, 2)
})

test_that("Teamleder gives access with the default role list", {
  groups <- list(list(
    id = "G2016", name = "Nymark G/J 2016",
    roles = list(list(id = "R1", name = "Teamleder"), list(id = "R2", name = "Utøver"), list(id = "R3", name = "Foresatt")),
    members = list(
      list(id = "M1", roles = list("R1"), profile = list(id = "P-me")),
      list(id = "M2", roles = list("R2"), profile = list(id = "P-player")),
      list(id = "M3", roles = list("R3"), profile = list(id = "P-parent"))
    )
  ))
  defaults <- c("Teamleder", "Trener", "Lagleder", "Hovedlagleder")
  expect_equal(spond_accessible_groups(groups, list(id = "P-me"), defaults)$roles, "Teamleder")
  # "Utøver" and "Foresatt" are roles in Spond too, but must not give access
  expect_equal(nrow(spond_accessible_groups(groups, list(id = "P-player"), defaults)), 0)
  expect_equal(nrow(spond_accessible_groups(groups, list(id = "P-parent"), defaults)), 0)
})

test_that("role matching ignores case and surrounding spaces", {
  acc <- spond_accessible_groups(fake_groups(), list(id = "P-trainer"), c(" trener "))
  expect_equal(acc$id, "G1")
})

test_that("parents and unknown users get no access", {
  expect_equal(nrow(spond_accessible_groups(fake_groups(), list(id = "P-parent"), c("Trener"))), 0)
  expect_equal(nrow(spond_accessible_groups(fake_groups(), list(id = "P-x"), c("Trener"))), 0)
  expect_equal(nrow(spond_accessible_groups(list(), list(id = "P-trainer"), c("Trener"))), 0)
})

test_that("subgroups are listed with member counts", {
  sgs <- spond_subgroups(fake_groups()[[1]])
  expect_equal(sgs$name, c("Lag 1", "Keepere"))
  expect_equal(sgs$n_members, c(2L, 2L))
  expect_equal(nrow(spond_subgroups(fake_groups()[[2]])), 0)
})

test_that("assert_group_access stops for groups outside the user's access", {
  acc <- spond_accessible_groups(fake_groups(), list(id = "P-trainer"), c("Trener"))
  expect_true(assert_group_access(acc, "G1"))
  expect_error(assert_group_access(acc, "G2"), "ikke tilgang")
  expect_error(assert_group_access(acc, NULL), "ikke tilgang")
})

test_that("default role names come from golem-config.yml", {
  expect_equal(access_role_names(), c("Teamleder", "Trener", "Lagleder", "Hovedlagleder"))
})
