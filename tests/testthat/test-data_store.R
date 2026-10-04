test_that("migrations create the tables and are only applied once", {
  con <- local_test_db()
  tables <- DBI::dbGetQuery(con, "SELECT table_name FROM information_schema.tables WHERE table_schema = current_schema()")$table_name
  expect_setequal(tables, c("schema_migrations", "member_tags", "group_proposals", "group_proposal_labels",
                            "group_proposal_members", "proposal_history", "proposal_comments", "edit_locks"))
  expect_length(ds_migrate(con, dir = test_migrations_dir()), 0)
})

test_that("tags: add (trimmed, no duplicates), list, remove", {
  con <- local_test_db(); acc <- test_access()
  ds_add_tag(con, acc, "G1", "M1", " Keeper ", "P1")
  ds_add_tag(con, acc, "G1", "M1", "Keeper", "P1")
  ds_add_tag(con, acc, "G1", "M2", "Kaptein", "P1")
  tags <- ds_list_tags(con, acc, "G1")
  expect_equal(tags$spond_member_id, c("M1", "M2"))
  expect_equal(tags$tag, c("Keeper", "Kaptein"))
  ds_remove_tag(con, acc, "G1", "M1", "Keeper")
  expect_equal(nrow(ds_list_tags(con, acc, "G1")), 1)
  expect_error(ds_add_tag(con, acc, "G1", "M1", "  ", "P1"), "mellom 1 og 40")
})

test_that("no access to other groups' data", {
  con <- local_test_db(); acc <- test_access()
  expect_error(ds_list_tags(con, acc, "G2"), "ikke tilgang")
  expect_error(ds_add_tag(con, acc, "G2", "M1", "x", "P1"), "ikke tilgang")
  expect_error(ds_save_proposal(con, acc, "P1", "G2", "X", "A"), "ikke tilgang")
  # A proposal in G2 (inserted directly) cannot be read or changed by a G1 trainer
  DBI::dbExecute(con, "INSERT INTO group_proposals (spond_group_id, name, created_by) VALUES ('G2', 'Fremmed', 'P9')")
  other <- DBI::dbGetQuery(con, "SELECT id FROM group_proposals WHERE spond_group_id = 'G2'")$id
  expect_error(ds_get_proposal(con, acc, other), "ikke tilgang")
  expect_error(ds_transition(con, acc, other, "delete", "P1"), "ikke tilgang")
  expect_error(ds_add_comment(con, acc, other, "P1", "hei"), "ikke tilgang")
})

test_that("create a proposal with groups and members, then read it back", {
  con <- local_test_db(); acc <- test_access()
  id <- ds_save_proposal(con, acc, "P1", "G1", "Tirsdag", labels = c("Ulv", "Gaupe"),
                         assignments = c(M1 = "Ulv", M2 = "Gaupe", M3 = "Ulv"), event_id = "E1")
  p <- ds_get_proposal(con, acc, id)
  expect_equal(p$proposal$status, "draft")
  expect_equal(p$proposal$spond_event_id, "E1")
  expect_true(is.na(p$proposal$spond_subgroup_id))
  expect_equal(p$labels$label, c("Ulv", "Gaupe"))
  expect_equal(p$members$spond_member_id[p$members$label == "Ulv"], c("M1", "M3"))
  expect_equal(p$history$decision, "created")
})

test_that("invalid proposals are rejected before anything is written", {
  con <- local_test_db(); acc <- test_access()
  expect_error(ds_save_proposal(con, acc, "P1", "G1", " ", "A"), "Navnet")
  expect_error(ds_save_proposal(con, acc, "P1", "G1", "X", c("A", "A")), "samme navn")
  expect_error(ds_save_proposal(con, acc, "P1", "G1", "X", "A", c(M1 = "B")), "Ukjent gruppe")
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*)::integer AS n FROM group_proposals")$n, 0L)
})

test_that("visibility: event proposals follow events, gruppeutkast follow subgroup context", {
  con <- local_test_db(); acc <- test_access()
  ev1 <- ds_save_proposal(con, acc, "P1", "G1", "Kamp Ulv", "A", event_id = "E-ulv")
  ev2 <- ds_save_proposal(con, acc, "P1", "G1", "Trening gutter", "A", event_id = "E-gutter")
  ut_whole <- ds_save_proposal(con, acc, "P1", "G1", "Utkast hele", "A")
  ut_sub <- ds_save_proposal(con, acc, "P1", "G1", "Utkast gutter", "A", subgroup_id = "S-gutter")

  # Whole group: all events visible -> all proposals
  all <- ds_list_proposals(con, acc, "G1", event_ids = c("E-ulv", "E-gutter"))
  expect_setequal(all$id, c(ev1, ev2, ut_whole, ut_sub))
  # Subgroup "Gutter": only its event and its own utkast
  sub <- ds_list_proposals(con, acc, "G1", event_ids = "E-gutter", subgroup_id = "S-gutter")
  expect_setequal(sub$id, c(ev2, ut_sub))
  # No visible events
  expect_setequal(ds_list_proposals(con, acc, "G1", subgroup_id = "S-annen")$id, integer())
})

test_that("status flow: submit, approve, rollback, edit, delete (soft)", {
  con <- local_test_db(); acc <- test_access()
  id <- ds_save_proposal(con, acc, "P1", "G1", "Forslag", c("A", "B"), c(M1 = "A"))
  expect_equal(ds_transition(con, acc, id, "submit", "P1"), "pending")
  expect_error(ds_save_proposal(con, acc, "P1", "G1", "Endret", "A", id = id), "kan ikke redigeres")
  expect_equal(ds_transition(con, acc, id, "approve", "P2"), "approved")
  expect_error(ds_transition(con, acc, id, "delete", "P1"), "Godkjent")
  expect_equal(ds_transition(con, acc, id, "rollback", "P2"), "rolled_back")
  # Editing a rolled-back proposal puts it back to draft and replaces the groups
  ds_save_proposal(con, acc, "P1", "G1", "Endret", c("Ny"), c(M2 = "Ny"), id = id)
  p <- ds_get_proposal(con, acc, id)
  expect_equal(p$proposal$status, "draft")
  expect_equal(p$proposal$name, "Endret")
  expect_equal(p$labels$label, "Ny")
  expect_equal(p$members$spond_member_id, "M2")
  expect_equal(ds_transition(con, acc, id, "delete", "P1"), "deleted")
  expect_equal(nrow(ds_list_proposals(con, acc, "G1")), 0)
  # History is kept
  expect_equal(ds_get_proposal(con, acc, id)$history$decision,
               c("created", "sent_for_approval", "approved", "rolled_back", "edited", "deleted"))
  expect_error(ds_transition(con, acc, id, "fly", "P1"), "Ukjent handling")
})

test_that("only one of two concurrent approvals can succeed", {
  con <- local_test_db(); acc <- test_access()
  id <- ds_save_proposal(con, acc, "P1", "G1", "Forslag", "A", submit = TRUE)
  ds_transition(con, acc, id, "approve", "P1")
  expect_error(ds_transition(con, acc, id, "approve", "P2"), "Godkjent")
  expect_equal(sum(ds_get_proposal(con, acc, id)$history$decision == "approved"), 1)
})

test_that("comments", {
  con <- local_test_db(); acc <- test_access()
  id <- ds_save_proposal(con, acc, "P1", "G1", "Forslag", "A")
  ds_add_comment(con, acc, id, "P1", "  Ser bra ut  ")
  expect_error(ds_add_comment(con, acc, id, "P1", ""), "mellom 1 og 2000")
  p <- ds_get_proposal(con, acc, id)
  expect_equal(p$comments$body, "Ser bra ut")
  expect_equal(ds_list_proposals(con, acc, "G1")$n_comments, 1L)
})

test_that("edit locks: acquire, replace per session, heartbeat, release, expire", {
  con <- local_test_db(); acc <- test_access()
  id <- ds_save_proposal(con, acc, "P1", "G1", "Forslag", "A", event_id = "E1")
  ds_acquire_lock(con, acc, "G1", "P1", "sess-a", proposal_id = id, event_id = "E1")
  ds_acquire_lock(con, acc, "G1", "P2", "sess-b", event_id = "E1")   # P2 is creating a new proposal
  locks <- ds_active_locks(con, acc, "G1")
  expect_equal(nrow(locks), 2)
  expect_equal(locks$proposal_id[locks$editor == "P1"], id)
  expect_true(is.na(locks$proposal_id[locks$editor == "P2"]))
  # Same session opening another editor replaces its lock
  ds_acquire_lock(con, acc, "G1", "P1", "sess-a", event_id = "E1")
  expect_equal(nrow(ds_active_locks(con, acc, "G1")), 2)
  expect_true(ds_heartbeat_lock(con, "sess-a"))
  ds_release_lock(con, "sess-a")
  expect_equal(ds_active_locks(con, acc, "G1")$editor, "P2")
  # A lock without heartbeat for longer than max_age disappears
  DBI::dbExecute(con, "UPDATE edit_locks SET heartbeat_at = now() - interval '5 minutes'")
  expect_equal(nrow(ds_active_locks(con, acc, "G1")), 0)
  expect_false(ds_heartbeat_lock(con, "sess-b"))
})

test_that("SQL splitting ignores comment lines", {
  expect_equal(ds_split_sql("-- a; comment;\nCREATE TABLE a (x int);\n\nCREATE INDEX i ON a (x);\n"),
               c("CREATE TABLE a (x int)", "CREATE INDEX i ON a (x)"))
})

test_that("text array literal escapes quotes and backslashes", {
  expect_equal(ds_text_array(character()), "{}")
  expect_equal(ds_text_array(c("a", 'b"c', "d\\e")), '{"a","b\\"c","d\\\\e"}')
})

# Database setup (admin) --------------------------------------------------------

test_that("random passwords are long, alphanumeric and different", {
  a <- ds_random_password(); b <- ds_random_password()
  expect_match(a, "^[A-Za-z0-9]{32}$")
  expect_false(identical(a, b))
  expect_equal(nchar(ds_random_password(40)), 40)
})

test_that("passwords are put into Supabase connection strings and encoded", {
  tpl <- "postgresql://postgres.abcd:[YOUR-PASSWORD]@aws-0-eu-north-1.pooler.supabase.com:5432/postgres"
  expect_equal(ds_fill_password(tpl, "p@ss:w/rd#1"),
               "postgresql://postgres.abcd:p%40ss%3Aw%2Frd%231@aws-0-eu-north-1.pooler.supabase.com:5432/postgres")
  expect_equal(ds_fill_password("postgresql://postgres.abcd:old@host:5432/postgres", "ny"),
               "postgresql://postgres.abcd:ny@host:5432/postgres")
  expect_error(ds_fill_password("not a url", "x"), "Skjønte ikke")
})

test_that("the app user's connection string uses role.project-ref and sslmode=require", {
  admin <- "postgresql://postgres.abcd:secret@aws-0-eu-north-1.pooler.supabase.com:5432/postgres"
  expect_equal(ds_app_url(admin, "spondnymark_app", "Abc123"),
               "postgresql://spondnymark_app.abcd:Abc123@aws-0-eu-north-1.pooler.supabase.com:5432/postgres?sslmode=require")
  expect_equal(ds_app_url("postgresql://postgres:x@localhost:5432/db?application_name=a", "app", "P"),
               "postgresql://app:P@localhost:5432/db?application_name=a&sslmode=require")
  expect_equal(ds_app_url("postgresql://postgres:x@h:5432/db?sslmode=disable", "app", "P"),
               "postgresql://app:P@h:5432/db?sslmode=disable")
})

test_that(".Renviron lines are replaced, not duplicated", {
  f <- withr::local_tempfile()
  writeLines(c("SPOND_EMAIL=a@b.no", "SPONDNYMARK_DB_URL=old"), f)
  ds_write_renviron("SPONDNYMARK_DB_URL", "new", f)
  expect_equal(readLines(f), c("SPOND_EMAIL=a@b.no", "SPONDNYMARK_DB_URL=new"))
  g <- withr::local_tempfile()
  ds_write_renviron("X", "1", g)
  expect_equal(readLines(g), "X=1")
})

test_that("the app user can use the app tables but cannot change the schema", {
  con <- local_test_db(); acc <- test_access()
  role <- paste0("spondnymark_app_t", paste(sample(letters, 6, replace = TRUE), collapse = ""))
  pw <- ds_random_password()
  expect_true(ds_setup_app_role(con, role, pw))
  withr::defer({
    DBI::dbExecute(con, paste0("DROP OWNED BY ", role))
    DBI::dbExecute(con, paste0("DROP ROLE ", role))
  })
  expect_false(ds_setup_app_role(con, role))   # second run: no new user, same rights
  expect_error(ds_setup_app_role(con, "Robert'); DROP TABLE x;--", pw), "Ugyldig rollenavn")
  expect_error(ds_setup_app_role(con, role, "kort"), "minst 24")

  schema <- DBI::dbGetQuery(con, "SELECT current_schema() AS s")$s
  admin_made <- ds_save_proposal(con, acc, "P1", "G1", "Laget av admin", "A")

  app <- test_db_connect(ds_app_url(Sys.getenv("SPONDNYMARK_TEST_DB_URL"), role, pw))
  withr::defer(DBI::dbDisconnect(app))
  DBI::dbExecute(app, paste0("SET search_path TO ", schema))
  DBI::dbExecute(app, "SET client_min_messages TO warning")

  # Allowed: the normal data functions, and seeing rows written by others (RLS policy)
  id <- ds_save_proposal(app, acc, "P1", "G1", "Laget av appen", c("A", "B"), c(M1 = "A"))
  expect_equal(ds_transition(app, acc, id, "submit", "P1"), "pending")
  ds_add_tag(app, acc, "G1", "M1", "Keeper", "P1")
  ds_add_comment(app, acc, id, "P1", "Fint")
  ds_acquire_lock(app, acc, "G1", "P1", "s1", proposal_id = id)
  expect_equal(nrow(ds_active_locks(app, acc, "G1")), 1)
  expect_setequal(ds_list_proposals(app, acc, "G1")$id, c(admin_made, id))

  # Not allowed: schema changes, migrations table
  expect_error(suppressWarnings(DBI::dbExecute(app, "CREATE TABLE x (a int)")))
  expect_error(suppressWarnings(DBI::dbExecute(app, "DROP TABLE group_proposals")))
  expect_error(suppressWarnings(DBI::dbExecute(app, "ALTER TABLE group_proposals DISABLE ROW LEVEL SECURITY")))
  # (checked via the catalog: RPostgreSQL does not raise on a failed SELECT)
  priv <- function(table, what) DBI::dbGetQuery(con, "SELECT has_table_privilege($1, $2, $3) AS ok",
                                               params = list(role, paste0(schema, ".", table), what))$ok
  expect_false(priv("schema_migrations", "SELECT"))
  expect_true(priv("group_proposals", "SELECT"))
  expect_false(priv("group_proposals", "TRUNCATE"))
})
