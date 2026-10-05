ev_ulv <- function() event_minimal(fake_spond_events()[[1]])  # M-1, M-3 coming; M-2 not answered; M-gone declined

test_that("group labels are numbered A, B, C ...", {
  expect_equal(next_group_label(character()), "Gruppe A")
  expect_equal(next_group_label(c("Gruppe A", "gruppe b")), "Gruppe C")
})

test_that("new drafts get a free name and two groups", {
  d <- draft_new(ev_ulv(), subgroup_id = "S-ulv", existing = c("Forslag 1"))
  expect_equal(d$name, "Forslag 2")
  expect_equal(d$labels, c("Gruppe A", "Gruppe B"))
  expect_null(d$subgroup_id)                  # event groups follow the event, not a subgroup
  u <- draft_new(NULL, subgroup_id = "S-ulv")
  expect_equal(u$name, "Utkast 1")
  expect_equal(u$subgroup_id, "S-ulv")
})

test_that("event drafts offer those coming, plus those already placed", {
  g <- fake_group()
  d <- draft_new(ev_ulv())
  m <- draft_members(d, g, members_in_context(g))
  expect_equal(m$member_id, c("M-1", "M-3"))
  d$assignments <- c(`M-2` = "Gruppe A")      # placed earlier, has not answered now
  m <- draft_members(d, g, members_in_context(g))
  expect_setequal(m$member_id, c("M-1", "M-2", "M-3"))
  expect_equal(m$status[m$member_id == "M-2"], "unanswered")
})

test_that("gruppeutkast offer the members of the context", {
  g <- fake_group()
  d <- draft_new(NULL, "S-ulv")
  m <- draft_members(d, g, members_in_context(g, "S-ulv"))
  expect_setequal(m$member_id, c("M-1", "M-3"))
  d$assignments <- c(`M-gone` = "Gruppe A")
  m <- draft_members(d, g, members_in_context(g, "S-ulv"))
  expect_equal(m$display_name[m$member_id == "M-gone"], "Tidligere medlem")
})

test_that("moves only accept allowed members and existing groups", {
  d <- draft_new(ev_ulv())
  d <- draft_move(d, "M-1", "Gruppe A", c("M-1", "M-3"))
  expect_equal(d$assignments, c(`M-1` = "Gruppe A"))
  expect_equal(draft_move(d, "M-2", "Gruppe A", c("M-1", "M-3")), d)      # not coming
  expect_equal(draft_move(d, "M-1", "Gruppe Z", c("M-1", "M-3")), d)      # no such group
  d <- draft_move(d, "M-1", "Gruppe B", c("M-1", "M-3"))
  expect_equal(d$assignments, c(`M-1` = "Gruppe B"))
  d <- draft_move(d, "M-1", "", c("M-1", "M-3"))
  expect_length(d$assignments, 0)
})

test_that("groups can be added, renamed and removed", {
  d <- draft_new(ev_ulv())
  d$assignments <- c(`M-1` = "Gruppe A", `M-3` = "Gruppe B")
  r <- draft_add_group(d, "")
  expect_equal(r$draft$labels, c("Gruppe A", "Gruppe B", "Gruppe C"))
  expect_match(draft_add_group(d, "gruppe a")$error, "finnes allerede")
  r <- draft_rename_group(d, "Gruppe A", "  Keepere ")
  expect_equal(r$draft$labels, c("Keepere", "Gruppe B"))
  expect_equal(r$draft$assignments[["M-1"]], "Keepere")
  expect_match(draft_rename_group(d, "Gruppe A", "Gruppe B")$error, "finnes allerede")
  expect_match(draft_rename_group(d, "Gruppe A", "")$error, "Skriv inn")
  r <- draft_remove_group(d, "Gruppe A")
  expect_equal(r$draft$labels, "Gruppe B")
  expect_equal(r$draft$assignments, c(`M-3` = "Gruppe B"))   # M-1 back to the pool
  expect_match(draft_remove_group(r$draft, "Gruppe B")$error, "minst én")
})

test_that("proposals are combined with their groups", {
  h <- data.frame(id = c(1L, 2L), spond_subgroup_id = c(NA, "S-ulv"), spond_event_id = c("E1", NA),
                  name = c("A", "B"), status = "draft", created_by = "P-me", updated_at = Sys.time())
  g <- list(labels = data.frame(proposal_id = c(1L, 1L, 2L), label = c("Y", "X", "Z"), sort_order = c(2L, 1L, 1L)),
            members = data.frame(proposal_id = c(1L, 2L), spond_member_id = c("M-1", "M-3"), label = c("X", "Z")))
  v <- proposals_view(h, g)
  expect_equal(v[[1]]$labels, c("X", "Y"))
  expect_equal(v[[1]]$assignments, c(`M-1` = "X"))
  expect_equal(v[[1]]$event_id, "E1")
  expect_null(v[[1]]$subgroup_id)
  expect_null(v[[2]]$event_id)
  expect_equal(proposals_view(h[0, ], g), list())
  lay <- proposal_layout(v[[1]]$labels, v[[1]]$assignments, eligible = c("M-1", "M-3"))
  expect_equal(lay$groups[[1]]$ids, "M-1")
  expect_equal(lay$unplaced, "M-3")
})

test_that("small helpers", {
  expect_true(proposal_editable("draft"))
  expect_false(proposal_editable("pending"))
  expect_equal(proposal_status_label("draft"), "Utkast")
  expect_null(response_note("accepted"))
  expect_equal(response_note("declined"), "Kommer ikke")
  expect_equal(profile_name(profile_names(fake_group()), "P-me"), "Kjetil G.")
  expect_equal(profile_name(profile_names(fake_group()), "P-ukjent"), "en annen trener")
  expect_false(event_is_past(ev_ulv(), fake_now()))
  expect_true(event_is_past(ev_ulv(), as.POSIXct("2026-11-01", tz = "UTC")))
})

test_that("a saved proposal is put into the list without waiting for a reload", {
  d <- draft_new(ev_ulv())
  d$assignments <- c(`M-1` = "Gruppe A")
  l <- proposal_upsert(list(), 7L, d, "Kamp", "P-me")
  expect_equal(l[[1]]$event_id, "E-ulv")
  expect_equal(l[[1]]$status, "draft")
  l[[1]]$created_by <- "P-annen"
  l2 <- proposal_upsert(l, 7L, d, "Kamp 2", "P-me")
  expect_length(l2, 1)
  expect_equal(l2[[1]]$name, "Kamp 2")
  expect_equal(l2[[1]]$created_by, "P-annen")    # the creator stays
})

test_that("actions follow the status", {
  expect_equal(names(proposal_actions("draft")), c("edit", "submit", "delete"))
  expect_equal(names(proposal_actions("pending")), c("approve", "reject"))
  expect_equal(names(proposal_actions("approved")), "rollback")
  expect_equal(names(proposal_actions("rejected")), c("edit", "delete"))
  expect_equal(names(proposal_actions("rolled_back")), c("edit", "delete"))
  expect_length(proposal_actions("deleted"), 0)
})

test_that("status changes are reflected locally like in the database", {
  mk <- function(id, status, ev = "E1") list(id = id, status = status, event_id = ev)
  l <- list(mk(1L, "approved"), mk(2L, "pending"), mk(3L, "approved", "E2"))
  a <- proposals_after(l, 2L, "approve")
  expect_equal(vapply(a, `[[`, "", "status"), c("rolled_back", "approved", "approved"))
  d <- proposals_after(l, 1L, "rollback")
  expect_equal(d[[1]]$status, "rolled_back")
  expect_length(proposals_after(list(mk(4L, "draft")), 4L, "delete"), 0)
  sorted <- proposals_sorted(list(mk(5L, "draft"), mk(6L, "approved"), mk(7L, "pending")))
  expect_equal(vapply(sorted, `[[`, 1L, "id"), c(6L, 7L, 5L))
})

test_that("comments are checked like tags", {
  expect_null(comment_problem("Fin fordeling, men bytt Emma og Noah"))
  expect_match(comment_problem("  "), "Skriv en kommentar")
  expect_match(comment_problem("Noah er syk"), "sensitive")
  expect_match(comment_problem(strrep("a", 2001)), "maks 2000")
})

test_that("history and times read well in Norwegian", {
  expect_equal(history_label(c("created", "approved", "ukjent")), c("laget forslaget", "godkjente", "ukjent"))
  t <- as.POSIXct("2026-10-05 16:40:00", tz = "UTC")
  expect_equal(short_time(t, now = fake_now()), "5. okt. kl. 18:40")
  expect_equal(short_time(as.POSIXct("2025-12-24 10:00:00", tz = "UTC"), now = fake_now()), "24. des. 2025 kl. 11:00")
})
