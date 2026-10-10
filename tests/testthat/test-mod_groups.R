# Runs against the test database (see helper-db.R); skipped without one.
ev_ulv <- function() event_minimal(fake_spond_events()[[1]])  # M-1, M-3 coming
acc <- function() fake_user(fake_spond_groups_two())$access

groups_args <- function(con, ctx = reactiveVal(teams_context(fake_group())), ev = reactiveVal(ev_ulv())) {
  list(context = ctx, user = reactive(fake_user(fake_spond_groups_two())), db = db_handle(function() con),
       event = ev, events = reactive(lapply(fake_spond_events()[1:2], event_minimal)),
       open_event_input = "events-open", now = fake_now, poll_ms = 60000)
}
html_of <- function(x) as.character(x$html)
locks_in <- function(con) DBI::dbGetQuery(con, "SELECT * FROM edit_locks")

test_that("a new event proposal is made, edited on the board and saved", {
  con <- local_test_db()
  testServer(mod_groups_server, args = groups_args(con), {
    session$flushReact()
    expect_match(html_of(output$event_cards), "Ingen gruppeforslag ennå")
    session$setInputs(new = "event")
    expect_true(session$returned$editing())
    expect_equal(draft()$name, "Forslag 1")
    expect_equal(nrow(locks_in(con)), 0)                          # taken just after the editor is shown
    session$elapse(500)
    expect_equal(nrow(locks_in(con)), 1)                          # others can see that we are editing
    expect_equal(locks_in(con)$spond_event_id, "E-ulv")
    board <- html_of(output$board)
    expect_match(board, "Kommer, ikke fordelt")
    expect_match(board, "data-member=\"M-1\"")
    expect_false(grepl("data-member=\"M-2\"", board))             # has not said "Kommer"

    session$setInputs(move = list(member = "M-1", group = "Gruppe A"))
    session$setInputs(move = list(member = "M-2", group = "Gruppe A"))  # ignored
    session$setInputs(move = list(member = "M-3", group = "Finnes ikke"))  # ignored
    expect_equal(draft()$assignments, c(`M-1` = "Gruppe A"))

    session$setInputs(add_group = "")
    session$setInputs(rename = list(from = "Gruppe C", to = "Keepere"))
    session$setInputs(rename = list(from = "Keepere", to = "gruppe a"))
    expect_match(html_of(output$editor_msg), "finnes allerede")
    session$setInputs(remove_group = "Gruppe B")
    expect_equal(draft()$labels, c("Gruppe A", "Keepere"))

    session$setInputs(name_1 = "Kamp 1", save = "x")
    expect_false(session$returned$editing())
    expect_match(html_of(output$event_cards), "Kamp 1")           # shown at once, before any reload
    session$elapse(500)
    expect_equal(nrow(locks_in(con)), 0)                          # lock released
    p <- DBI::dbGetQuery(con, "SELECT * FROM group_proposals")
    expect_equal(p$name, "Kamp 1")
    expect_equal(p$spond_event_id, "E-ulv")
    expect_equal(p$created_by, "P-me")
    expect_equal(p$status, "draft")
    session$flushReact()
    cards <- html_of(output$event_cards)
    expect_match(cards, "Kamp 1")
    expect_match(cards, "Laget av Kjetil G.")
    expect_match(cards, "Utkast")
    expect_match(cards, "Kommer, men ikke fordelt \\(1\\)")       # M-3
    expect_match(cards, "Rediger grupper")
    expect_match(as.character(session$returned$event_badge("E-ulv")), "1 gruppeforslag")
  })
})

test_that("editing an existing proposal, and a member who no longer comes is marked", {
  con <- local_test_db()
  pid <- ds_save_proposal(con, acc(), "P-me", "G2016", "Kamp 1", c("Rød", "Blå"),
                         c(`M-1` = "Rød", `M-2` = "Blå"), event_id = "E-ulv")
  testServer(mod_groups_server, args = groups_args(con), {
    session$flushReact()
    cards <- html_of(output$event_cards)
    expect_match(cards, "Ikke svart")                             # M-2 is placed but has not answered
    session$setInputs(edit = as.character(pid))
    expect_true(session$returned$editing())
    expect_equal(draft()$labels, c("Rød", "Blå"))
    expect_match(html_of(output$board), "data-member=\"M-2\"")    # placed members stay on the board
    session$setInputs(move = list(member = "M-2", group = ""))
    session$setInputs(name_1 = "Kamp 1", save = "x")
    expect_equal(DBI::dbGetQuery(con, "SELECT spond_member_id FROM group_proposal_members")$spond_member_id, "M-1")
    expect_equal(DBI::dbGetQuery(con, "SELECT decision FROM proposal_history ORDER BY id")$decision,
                 c("created", "edited"))
  })
})

test_that("someone else's lock gives a warning before editing", {
  con <- local_test_db()
  pid <- ds_save_proposal(con, acc(), "P-me", "G2016", "Kamp 1", "Alle", event_id = "E-ulv")
  ds_acquire_lock(con, acc(), "G2016", "P-annen", "annen-økt", proposal_id = pid)
  ds_acquire_lock(con, acc(), "G2016", "P-annen2", "tredje-økt", event_id = "E-ulv")
  testServer(mod_groups_server, args = groups_args(con), {
    session$flushReact()
    cards <- html_of(output$event_cards)
    expect_match(cards, "Redigeres av en annen trener")
    expect_match(cards, "lager et nytt forslag her nå")
    expect_match(as.character(session$returned$event_badge("E-ulv")), "Redigeres")
    session$setInputs(edit = as.character(pid))
    expect_false(session$returned$editing())                      # waits for "Rediger likevel"
    session$setInputs(force_edit = as.character(pid))
    expect_true(session$returned$editing())
  })
})

test_that("gruppeutkast belong to the context they were made in", {
  con <- local_test_db()
  ctx <- reactiveVal(teams_context(fake_group(), "S-ulv"))
  testServer(mod_groups_server, args = groups_args(con, ctx, ev = reactiveVal(NULL)), {
    session$flushReact()
    session$setInputs(new = "draft")
    expect_equal(draft()$name, "Utkast 1")
    expect_match(html_of(output$board), "Ikke fordelt")
    expect_match(html_of(output$editor), "Gruppeutkast i Nymark Ulv G10")
    session$setInputs(move = list(member = "M-3", group = "Gruppe B"))
    session$setInputs(name_1 = "Nye lag", save = "x")
    p <- DBI::dbGetQuery(con, "SELECT spond_subgroup_id, spond_event_id FROM group_proposals")
    expect_equal(p$spond_subgroup_id, "S-ulv")
    expect_true(is.na(p$spond_event_id))
    session$flushReact()
    expect_match(html_of(output$draft_cards), "Nye lag")
    expect_equal(session$returned$n_drafts(), 1)
  })
})

test_that("no new proposals for past events; bad input gives a message", {
  con <- local_test_db()
  past <- ev_ulv()
  past$start <- fake_now() - 7 * 86400
  past$end <- past$start + 3600
  testServer(mod_groups_server, args = groups_args(con, ev = reactiveVal(past)), {
    session$flushReact()
    expect_match(html_of(output$event_cards), "gjennomført")
    session$setInputs(new = "event")
    expect_false(session$returned$editing())
  })
  testServer(mod_groups_server, args = groups_args(con), {
    session$setInputs(new = "event")
    session$setInputs(name_1 = "   ", save = "x")
    expect_match(html_of(output$editor_msg), "Navnet på forslaget")
    expect_true(session$returned$editing())
    session$setInputs(cancel = "x")
    expect_false(session$returned$editing())
    expect_equal(nrow(DBI::dbGetQuery(con, "SELECT 1 FROM group_proposals")), 0)
  })
})

test_that("changing context closes the editor and releases the lock", {
  con <- local_test_db()
  ctx <- reactiveVal(teams_context(fake_group()))
  testServer(mod_groups_server, args = groups_args(con, ctx), {
    session$setInputs(new = "event")
    session$elapse(500)
    expect_equal(nrow(locks_in(con)), 1)
    ctx(teams_context(fake_group(), "S-gaupe"))
    session$flushReact()
    expect_false(session$returned$editing())
    session$elapse(500)
    expect_equal(nrow(locks_in(con)), 0)
  })
})

test_that("proposals from another group cannot be opened", {
  con <- local_test_db()
  DBI::dbExecute(con, "INSERT INTO group_proposals (spond_group_id, name, created_by) VALUES ('G-annen', 'X', 'P9')")
  other <- DBI::dbGetQuery(con, "SELECT id FROM group_proposals")$id
  testServer(mod_groups_server, args = groups_args(con), {
    session$flushReact()
    session$setInputs(edit = as.character(other))
    expect_false(session$returned$editing())
  })
})

test_that("the lock is renewed while the editor is open", {
  con <- local_test_db()
  args <- groups_args(con)
  args$heartbeat_ms <- 1000
  testServer(mod_groups_server, args = args, {
    session$setInputs(new = "event")
    session$elapse(500)
    DBI::dbExecute(con, "UPDATE edit_locks SET heartbeat_at = now() - interval '5 minutes'")
    session$elapse(1001)
    age <- DBI::dbGetQuery(con, "SELECT extract(epoch FROM now() - heartbeat_at) AS s FROM edit_locks")$s
    expect_lt(age, 60)
  })
})

test_that("opening and closing quickly leaves no lock behind", {
  con <- local_test_db()
  testServer(mod_groups_server, args = groups_args(con), {
    session$setInputs(new = "event")
    session$setInputs(cancel = "x")      # before the lock was taken
    session$elapse(500)
    expect_equal(nrow(locks_in(con)), 0)
  })
})

test_that("send for approval, approve, and a second approval rolls back the first", {
  con <- local_test_db()
  a <- ds_save_proposal(con, acc(), "P-me", "G2016", "Rød/blå", c("Rød", "Blå"), event_id = "E-ulv")
  b <- ds_save_proposal(con, acc(), "P-me", "G2016", "Tre lag", c("1", "2", "3"), event_id = "E-ulv")
  status_of <- function(id) DBI::dbGetQuery(con, "SELECT status FROM group_proposals WHERE id = $1", params = list(id))$status
  testServer(mod_groups_server, args = groups_args(con), {
    session$flushReact()
    cards <- html_of(output$event_cards)
    expect_match(cards, "Send til godkjenning")
    expect_match(cards, "Slett grupper")
    session$setInputs(act = paste0("approve:", a))           # not allowed for a draft: ignored
    expect_equal(status_of(a), "draft")
    session$setInputs(act = paste0("submit:", a))
    expect_equal(status_of(a), "pending")
    expect_equal(session$returned$n_pending(), 1)
    pend <- html_of(output$pending_cards)
    expect_match(pend, "For Trening Ulv")
    expect_match(pend, "data-sn-input=\"events-open\" data-sn-value=\"E-ulv\"")
    expect_match(pend, "Godkjenn")
    expect_match(as.character(session$returned$event_badge("E-ulv")), "Venter på godkjenning")
    session$setInputs(act = paste0("approve:", a))
    expect_equal(status_of(a), "approved")
    expect_match(as.character(session$returned$event_badge("E-ulv")), "Godkjente grupper")
    expect_match(html_of(output$event_cards), "Rull tilbake")

    session$setInputs(act = paste0("submit:", b))
    session$setInputs(act = paste0("approve:", b))
    expect_equal(status_of(b), "approved")
    expect_equal(status_of(a), "rolled_back")                # only one approved per event
    session$elapse(500)
    h <- html_of(output$event_cards)
    expect_match(h, "Rullet tilbake")
    expect_match(h, "Kjetil G.</strong>\\s+godkjente")
  })
})

test_that("deleting asks first, and the proposal disappears", {
  con <- local_test_db()
  a <- ds_save_proposal(con, acc(), "P-me", "G2016", "Rød/blå", "Rød", event_id = "E-ulv")
  testServer(mod_groups_server, args = groups_args(con), {
    session$flushReact()
    session$setInputs(act = paste0("delete:", a))
    expect_equal(DBI::dbGetQuery(con, "SELECT status FROM group_proposals")$status, "draft")   # not yet
    session$setInputs(confirm_delete = as.character(a))
    expect_equal(DBI::dbGetQuery(con, "SELECT status FROM group_proposals")$status, "deleted")
    expect_false(grepl("Rød/blå", html_of(output$event_cards)))
    session$elapse(500)
    expect_false(grepl("Rød/blå", html_of(output$event_cards)))
  })
})

test_that("comments are added, and sensitive ones refused", {
  con <- local_test_db()
  a <- ds_save_proposal(con, acc(), "P-me", "G2016", "Rød/blå", "Rød", event_id = "E-ulv")
  testServer(mod_groups_server, args = groups_args(con), {
    session$flushReact()
    expect_match(html_of(output$event_cards), "Kommentarer \\(0\\)")
    session$setInputs(comment = list(key = as.character(a), value = "  Fin fordeling  "))
    expect_equal(DBI::dbGetQuery(con, "SELECT body, author FROM proposal_comments")$body, "Fin fordeling")
    cards <- html_of(output$event_cards)
    expect_match(cards, "Kommentarer \\(1\\)")
    expect_match(cards, "Fin fordeling")
    session$setInputs(comment = list(key = as.character(a), value = "Emma er skadet i kneet"))
    expect_equal(nrow(DBI::dbGetQuery(con, "SELECT 1 FROM proposal_comments")), 1)
    session$setInputs(comment = list(key = "999999", value = "Hei"))   # unknown proposal: ignored
    expect_equal(nrow(DBI::dbGetQuery(con, "SELECT 1 FROM proposal_comments")), 1)
    session$elapse(500)
    expect_match(html_of(output$event_cards), "laget forslaget")
  })
})

test_that("the editor can save and send for approval in one go", {
  con <- local_test_db()
  testServer(mod_groups_server, args = groups_args(con), {
    session$setInputs(new = "event")
    session$setInputs(move = list(member = "M-1", group = "Gruppe A"))
    session$setInputs(name_1 = "Kamp", save_submit = "x")
    expect_equal(DBI::dbGetQuery(con, "SELECT status FROM group_proposals")$status, "pending")
    expect_equal(session$returned$n_pending(), 1)
  })
})

test_that("a name typed in an earlier editor is not reused", {
  con <- local_test_db()
  testServer(mod_groups_server, args = groups_args(con), {
    session$setInputs(new = "event")
    session$setInputs(name_1 = "Første", save = "x")
    session$setInputs(new = "event")
    expect_equal(draft()$name, "Forslag 1")      # "Første" is not "Forslag 1", so number 1 is free
    session$setInputs(save = "x")                # name field of editor 2 not sent yet
    expect_setequal(DBI::dbGetQuery(con, "SELECT name FROM group_proposals")$name, c("Første", "Forslag 1"))
  })
})

test_that("edit locks do not store the Shiny session token", {
  con <- local_test_db()
  testServer(mod_groups_server, args = groups_args(con), {
    expect_match(session_key, "^s-[A-Za-z0-9]{24}$")
    expect_false(grepl(session$token, session_key, fixed = TRUE))
  })
})
