test_that("short names keep the first name and initials for the rest", {
  expect_equal(short_name("Emma", "Haugen"), "Emma H.")
  expect_equal(short_name("Anna Maria", "Olsen"), "Anna M. O.")
  expect_equal(short_name("Cher", ""), "Cher")
  expect_equal(short_name(c("Emma", "Ola"), c("haugen", "Nordmann")), c("Emma H.", "Ola N."))
})

test_that("colliding short names fall back to full names", {
  m <- data.frame(first_name = c("Emma", "Emma", "Ola"), last_name = c("Haugen", "Hansen", "Nordmann"))
  expect_equal(member_display_names(m), c("Emma Haugen", "Emma Hansen", "Ola N."))
  expect_equal(member_display_names(m[0, ]), character())
})

test_that("members in context: whole group or one subgroup, sorted by name", {
  g <- spond_session_data(fake_spond_groups_two(), fake_spond_profile(), "Teamleder")$groups[["G2016"]]
  all <- members_in_context(g)
  expect_equal(all$display_name, c("Emma Hansen", "Emma Haugen", "Kjetil G.", "Noah S."))
  ulv <- members_in_context(g, "S-ulv")
  expect_equal(ulv$id, c("M-1", "M-3"))
  # The collision is decided on the whole group, so Emma Haugen keeps her full name in Ulv too
  expect_equal(ulv$display_name, c("Emma Haugen", "Noah S."))
  expect_equal(subgroup_sizes(g), c(`S-ulv` = 2L, `S-gaupe` = 2L))
})

test_that("member counts use singular and plural", {
  expect_equal(n_members(1), "1 medlem")
  expect_equal(n_members(0), "0 medlemmer")
  expect_equal(n_members(19L), "19 medlemmer")
})
