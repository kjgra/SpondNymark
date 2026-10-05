# Runs against the test database (see helper-db.R); skipped without one.
tags_args <- function(con, ctx = reactiveVal(teams_context(fake_group())), poll_ms = 60000) {
  u <- fake_user(fake_spond_groups_two())
  list(context = ctx, user = reactive(u), db = db_handle(function() con), poll_ms = poll_ms,
       suggestions = c("Keeper", "Kaptein"))
}
acc_g2016 <- function() fake_user(fake_spond_groups_two())$access

test_that("tags of the main group are loaded", {
  con <- local_test_db()
  ds_add_tag(con, acc_g2016(), "G2016", "M-1", "Keeper", "P-x")
  testServer(mod_tags_server, args = tags_args(con), {
    session$flushReact()
    expect_equal(session$returned$table()$tag, "Keeper")
    expect_null(session$returned$error())
    chip <- as.character(session$returned$chip("M-1", "Emma Haugen"))
    expect_match(chip, "sn-minitag\">Keeper")
    expect_match(chip, "data-sn-input=\"proxy1-open\"")
  })
})

test_that("tags are added, reused, checked and removed in the editor", {
  con <- local_test_db()
  ds_add_tag(con, acc_g2016(), "G2016", "M-2", "Venstrebein", "P-x")
  testServer(mod_tags_server, args = tags_args(con), {
    session$flushReact()
    session$setInputs(open = "M-1")
    expect_match(as.character(output$current$html), "Ingen tagger")
    sugg <- as.character(output$suggest$html)
    expect_match(sugg, "Keeper")
    expect_match(sugg, "Venstrebein")              # used in the group

    session$setInputs(submit = "  venstrebein ")  # existing spelling is reused
    session$setInputs(quick = "Kaptein")
    t <- DBI::dbGetQuery(con, "SELECT spond_member_id, tag, created_by FROM member_tags WHERE spond_member_id = 'M-1' ORDER BY tag")
    expect_equal(t$tag, c("Kaptein", "Venstrebein"))
    expect_equal(unique(t$created_by), "P-me")     # the logged-in profile
    expect_match(as.character(output$current$html), "Kaptein")
    expect_false(grepl("Kaptein", as.character(output$suggest$html)))

    session$setInputs(submit = "Skadet kne")
    expect_match(as.character(output$msg$html), "sensitive")
    expect_equal(nrow(DBI::dbGetQuery(con, "SELECT 1 FROM member_tags WHERE tag ILIKE '%skad%'")), 0)

    session$setInputs(submit = "keeper")           # spelling from the suggestions
    expect_true("Keeper" %in% tags_for(session$returned$table(), "M-1"))
    session$setInputs(remove = "Keeper")
    session$setInputs(remove = "Kaptein")
    expect_equal(tags_for(session$returned$table(), "M-1"), "Venstrebein")
    expect_null(msg())
  })
})

test_that("ids from the browser must be members of the group", {
  con <- local_test_db()
  testServer(mod_tags_server, args = tags_args(con), {
    session$setInputs(open = "M-someone-else")
    expect_null(member())
    session$setInputs(open = "M-9")                # member of another group (Nymark Senior)
    expect_null(member())
    session$setInputs(submit = "Keeper")
    expect_equal(nrow(DBI::dbGetQuery(con, "SELECT 1 FROM member_tags")), 0)
  })
})

test_that("tags set by other trainers show up after the next poll", {
  con <- local_test_db()
  testServer(mod_tags_server, args = tags_args(con, poll_ms = 1000), {
    session$flushReact()
    expect_equal(nrow(session$returned$table()), 0)
    ds_add_tag(con, acc_g2016(), "G2016", "M-3", "Keeper", "P-other")
    session$elapse(1001)
    expect_equal(session$returned$table()$member_id, "M-3")
  })
})

test_that("changing main group closes the editor", {
  con <- local_test_db()
  ctx <- reactiveVal(teams_context(fake_group()))
  testServer(mod_tags_server, args = tags_args(con, ctx), {
    session$setInputs(open = "M-1")
    expect_equal(member(), "M-1")
    ctx(teams_context(fake_user(fake_spond_groups_two())$groups[["G-senior"]]))
    session$flushReact()
    expect_null(member())
  })
})

test_that("database errors give a message, not a crash", {
  broken <- list(run = function(f) stop("could not connect to server"), close = function() NULL)
  args <- tags_args(NULL)
  args$db <- broken
  logged <- capture_messages(
    testServer(mod_tags_server, args = args, {
      session$flushReact()
      expect_match(session$returned$error(), "kunne ikke hentes")
      session$setInputs(open = "M-1")
      session$setInputs(submit = "Keeper")
      expect_match(as.character(output$msg$html), "Kunne ikke lagre")
    })
  )
  # The technical error goes to the server log, not to the user
  expect_true(all(grepl("could not connect", logged)))
  expect_length(logged, 2)
})
