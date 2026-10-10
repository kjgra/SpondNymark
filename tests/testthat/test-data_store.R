test_that("migrations create the tables and are only applied once", {
  con <- local_test_db()
  tables <- DBI::dbGetQuery(con, "SELECT table_name FROM information_schema.tables WHERE table_schema = current_schema()")$table_name
  expect_setequal(tables, c("schema_migrations", "member_tags", "group_proposals", "group_proposal_labels",
                            "group_proposal_members", "proposal_history", "proposal_comments", "edit_locks",
                            "season_themes", "team_settings", "exercises", "training_plans",
                            "app_roles", "app_role_log"))
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

  # Whole group: all events visible, and only its own utkast
  all <- ds_list_proposals(con, acc, "G1", event_ids = c("E-ulv", "E-gutter"))
  expect_setequal(all$id, c(ev1, ev2, ut_whole))
  # Subgroup "Gutter": only its event and its own utkast
  sub <- ds_list_proposals(con, acc, "G1", event_ids = "E-gutter", subgroup_id = "S-gutter")
  expect_setequal(sub$id, c(ev2, ut_sub))
  # No visible events
  expect_setequal(ds_list_proposals(con, acc, "G1", subgroup_id = "S-annen")$id, integer())
  # An utkast waiting for approval is returned everywhere (for "Godkjenning")
  ds_transition(con, acc, ut_sub, "submit", "P1")
  expect_true(ut_sub %in% ds_list_proposals(con, acc, "G1")$id)
  expect_true(ut_sub %in% ds_list_proposals(con, acc, "G1", subgroup_id = "S-annen")$id)
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
  # askpass returns NULL when the dialog is closed (e.g. switching windows)
  expect_error(ds_fill_password(tpl, NULL), "Mangler passord")
  expect_error(ds_fill_password(tpl, ""), "Mangler passord")
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
  url <- Sys.getenv("SPONDNYMARK_TEST_DB_URL")
  # Remove test users left behind by earlier runs that were stopped halfway.
  drop_test_roles(con)
  role <- paste0("spondnymark_app_t", paste(sample(letters, 6, replace = TRUE), collapse = ""))
  pw <- ds_random_password()
  expect_true(ds_setup_app_role(con, role, pw))
  # The test user is removed after the temporary schema is gone (priority
  # "last"): its rights and policies disappear with the schema, so DROP ROLE
  # works without superuser rights (Supabase's postgres user is not superuser
  # and may not run DROP OWNED BY on roles it is not a member of).
  withr::defer({
    admin <- test_db_connect(url)
    on.exit(DBI::dbDisconnect(admin))
    DBI::dbExecute(admin, paste0("DROP ROLE IF EXISTS ", DBI::dbQuoteIdentifier(admin, role)))
  }, priority = "last")
  expect_false(ds_setup_app_role(con, role))   # second run: no new user, same rights
  expect_error(ds_setup_app_role(con, "Robert'); DROP TABLE x;--", pw), "Ugyldig rollenavn")
  expect_error(ds_setup_app_role(con, role, "kort"), "minst 24")

  schema <- DBI::dbGetQuery(con, "SELECT current_schema() AS s")$s
  admin_made <- ds_save_proposal(con, acc, "P1", "G1", "Laget av admin", "A")

  app <- test_db_connect(ds_app_url(url, role, pw))
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
  # (checked via the catalog, which works the same with every driver)
  priv <- function(table, what) DBI::dbGetQuery(con, "SELECT has_table_privilege($1, $2, $3) AS ok",
                                               params = list(role, paste0(schema, ".", table), what))$ok
  expect_false(priv("schema_migrations", "SELECT"))
  expect_true(priv("group_proposals", "SELECT"))
  expect_false(priv("group_proposals", "TRUNCATE"))
})

test_that(".Renviron keeps comments and position when a value is replaced", {
  f <- withr::local_tempfile()
  writeLines(c("# kommentar", "SPONDNYMARK_DB_URL=", "", "# annet", "SPOND_EMAIL=a@b.no"), f)
  ds_write_renviron("SPONDNYMARK_DB_URL", "postgresql://x", f)
  expect_equal(readLines(f), c("# kommentar", "SPONDNYMARK_DB_URL=postgresql://x", "", "# annet", "SPOND_EMAIL=a@b.no"))
})

test_that("connection strings are split into separate arguments for RPostgres", {
  url <- "postgresql://spondnymark_app.abcd:p%40ss%3Aw%2Frd@aws-0-eu-west-1.pooler.supabase.com:5432/postgres?sslmode=require"
  a <- ds_connect_args(url)
  expect_equal(a$host, "aws-0-eu-west-1.pooler.supabase.com")
  expect_equal(a$port, 5432L)
  expect_equal(a$user, "spondnymark_app.abcd")
  expect_equal(a$password, "p@ss:w/rd")
  expect_equal(a$dbname, "postgres")
  expect_equal(a$sslmode, "require")
  # the admin URL produced by ds_fill_password round-trips to the original password
  tpl <- "postgresql://postgres.abcd:[YOUR-PASSWORD]@aws-0-eu-west-1.pooler.supabase.com:5432/postgres"
  pw <- "Æ#ø %å!*'()+,;=&$?[]"
  expect_equal(ds_connect_args(ds_fill_password(tpl, pw))$password, pw)
  expect_equal(ds_connect_args(ds_fill_password(tpl, pw))$user, "postgres.abcd")
  # defaults and errors
  d <- ds_connect_args("postgresql://u:p@localhost")
  expect_equal(c(d$port, d$dbname), c(5432L, "postgres"))
  expect_error(ds_connect_args("localhost:5432"), "Skjønte ikke")
  expect_error(ds_connect(""), "ikke satt")
})

test_that("groups of several proposals are read in one go, only from the own group", {
  con <- local_test_db(); acc <- test_access()
  a <- ds_save_proposal(con, acc, "P1", "G1", "A", c("Rød", "Blå"), c(M1 = "Blå", M2 = "Rød"), event_id = "E1")
  b <- ds_save_proposal(con, acc, "P1", "G1", "B", "Alle", c(M3 = "Alle"))
  DBI::dbExecute(con, "INSERT INTO group_proposals (spond_group_id, name, created_by) VALUES ('G2', 'Fremmed', 'P9')")
  other <- DBI::dbGetQuery(con, "SELECT id FROM group_proposals WHERE spond_group_id = 'G2'")$id
  DBI::dbExecute(con, "INSERT INTO group_proposal_labels (proposal_id, label, sort_order) VALUES ($1, 'X', 1)", params = list(other))
  g <- ds_proposal_groups(con, acc, "G1", c(a, b, other))
  expect_equal(g$labels$label, c("Rød", "Blå", "Alle"))
  expect_equal(g$labels$proposal_id, c(a, a, b))
  expect_equal(g$members$spond_member_id, c("M1", "M2", "M3"))
  expect_equal(nrow(ds_proposal_groups(con, acc, "G1", integer())$labels), 0)
  expect_error(ds_proposal_groups(con, acc, "G2", other), "ikke tilgang")
})

test_that("parameters are quoted safely when put into the SQL", {
  con <- local_test_db(); acc <- test_access()
  evil <- "x'); DROP TABLE member_tags; --"
  ds_add_tag(con, acc, "G1", "M1", evil, "P1")
  expect_equal(ds_list_tags(con, acc, "G1")$tag, evil)
  expect_equal(ds_interpolate(con, "SELECT $2, $1, $10", c(list("a", "b"), as.list(3:10))),
               "SELECT 'b', 'a', 10::int4")
  expect_error(ds_interpolate(con, "SELECT $3", list("a")), "For få parametere")
  # Empty results keep their columns
  res <- ds_query(con, "SELECT spond_member_id, tag FROM member_tags WHERE spond_group_id = $1", list("ingen"))
  expect_equal(names(res), c("spond_member_id", "tag"))
  expect_equal(nrow(res), 0)
})

test_that("saving a proposal takes few statements, however many members", {
  con <- local_test_db(); acc <- test_access()
  members <- stats::setNames(rep(c("A", "B"), 15), paste0("M", 1:30))
  n <- 0
  exec <- ds_exec
  query <- ds_query
  local_mocked_bindings(ds_exec = function(...) { n <<- n + 1; exec(...) },
                        ds_query = function(...) { n <<- n + 1; query(...) })
  ds_save_proposal(con, acc, "P1", "G1", "Stor", c("A", "B"), members, event_id = "E1")
  expect_lte(n, 1)                       # one round trip for a new proposal
  id <- DBI::dbGetQuery(con, "SELECT id FROM group_proposals WHERE name = 'Stor'")$id
  n <- 0
  ds_save_proposal(con, acc, "P1", "G1", "Stor 2", c("A", "B"), members, event_id = "E1", id = id)
  expect_lte(n, 2)                       # access check + one round trip for the edit
  expect_equal(nrow(DBI::dbGetQuery(con, "SELECT 1 FROM group_proposal_members WHERE proposal_id = $1", params = list(id))), 30)
})

test_that("a proposal that is not editable is left untouched by an edit", {
  con <- local_test_db(); acc <- test_access()
  id <- ds_save_proposal(con, acc, "P1", "G1", "Til godkjenning", "A", c(M1 = "A"), submit = TRUE)
  expect_error(ds_save_proposal(con, acc, "P1", "G1", "Endret", "B", c(M2 = "B"), id = id), "kan ikke redigeres")
  g <- ds_get_proposal(con, acc, id)
  expect_equal(g$proposal$name, "Til godkjenning")
  expect_equal(g$labels$label, "A")
  expect_equal(g$members$spond_member_id, "M1")
  expect_equal(g$history$decision, c("created", "sent_for_approval"))
})

test_that("approving a proposal rolls back the event's previously approved one", {
  con <- local_test_db(); acc <- test_access()
  a <- ds_save_proposal(con, acc, "P1", "G1", "A", "X", event_id = "E1", submit = TRUE)
  b <- ds_save_proposal(con, acc, "P1", "G1", "B", "X", event_id = "E1", submit = TRUE)
  other <- ds_save_proposal(con, acc, "P1", "G1", "Annet", "X", event_id = "E2", submit = TRUE)
  ds_transition(con, acc, a, "approve", "P1")
  ds_transition(con, acc, other, "approve", "P1")
  ds_transition(con, acc, b, "approve", "P2")
  st <- DBI::dbGetQuery(con, "SELECT id, status FROM group_proposals ORDER BY id")
  expect_equal(st$status, c("rolled_back", "approved", "approved"))   # E2 is not touched
  expect_equal(ds_get_proposal(con, acc, a)$history$decision,
               c("created", "sent_for_approval", "approved", "rolled_back"))
  expect_equal(tail(ds_get_proposal(con, acc, a)$history$actor, 1), "P2")
})

test_that("comments and history of several proposals come in one query", {
  con <- local_test_db(); acc <- test_access()
  a <- ds_save_proposal(con, acc, "P1", "G1", "A", "X", submit = TRUE)
  b <- ds_save_proposal(con, acc, "P1", "G1", "B", "X")
  ds_add_comment(con, acc, a, "P2", "Bra")
  DBI::dbExecute(con, "INSERT INTO group_proposals (spond_group_id, name, created_by) VALUES ('G2', 'X', 'P9')")
  foreign <- DBI::dbGetQuery(con, "SELECT id FROM group_proposals WHERE spond_group_id = 'G2'")$id
  DBI::dbExecute(con, "INSERT INTO proposal_comments (proposal_id, author, body) VALUES ($1, 'P9', 'hemmelig')",
                 params = list(foreign))
  th <- ds_proposal_threads(con, acc, "G1", c(a, b, foreign))
  expect_equal(th$text[th$proposal_id == a], c("created", "sent_for_approval", "Bra"))
  expect_equal(th$kind[th$proposal_id == a], c("history", "history", "comment"))
  expect_false("hemmelig" %in% th$text)
  expect_equal(names(ds_proposal_threads(con, acc, "G1", integer())), c("proposal_id", "kind", "actor", "text", "created_at"))
})

# Training plans, phase T1 (migration 002) ---------------------------------------

test_that("season plan: save a year, change it, and other years are left alone", {
  con <- local_test_db(); acc <- test_access()
  themes <- rep("", 12); themes[c(9, 10)] <- c("Vending", "Samhandling")
  desc <- rep("", 12); desc[10] <- "Spille på lag"
  ds_save_season(con, acc, "G1", 2026, themes, desc, "P1")
  ds_save_season(con, acc, "G1", 2027, c("Ballmestring", rep("", 11)), actor = "P1")
  s <- ds_list_season_themes(con, acc, "G1", 2026)
  expect_equal(s$month, c(9L, 10L))
  expect_equal(s$description, c("", "Spille på lag"))
  expect_equal(ds_season_theme(con, acc, "G1", as.Date("2026-10-14"))$theme, "Samhandling")
  expect_null(ds_season_theme(con, acc, "G1", as.Date("2026-11-01")))

  themes[9] <- ""; themes[11] <- "Avslutning"
  ds_save_season(con, acc, "G1", 2026, themes, desc, "P2")
  s <- ds_list_season_themes(con, acc, "G1", 2026)
  expect_equal(s$month, c(10L, 11L))
  expect_equal(s$updated_by, c("P1", "P2"))        # unchanged months keep who wrote them
  expect_equal(nrow(ds_list_season_themes(con, acc, "G1", 2027)), 1)
  ds_save_season(con, acc, "G1", 2026, rep("", 12), actor = "P1")
  expect_equal(nrow(ds_list_season_themes(con, acc, "G1", 2026)), 0)
})

test_that("team settings: empty until saved, then updated in place", {
  con <- local_test_db(); acc <- test_access()
  s <- ds_get_team_settings(con, acc, "G1")
  expect_equal(s$session_minutes, "")
  ds_save_team_settings(con, acc, "G1", list(age_group = "G10", session_minutes = "75", pitch = "7er"), "P1")
  ds_save_team_settings(con, acc, "G1", list(age_group = "G10", session_minutes = "60", equipment = "Kjegler"), "P2")
  s <- ds_get_team_settings(con, acc, "G1")
  expect_equal(s$session_minutes, "60")
  expect_equal(s$pitch, "")
  expect_equal(s$equipment, "Kjegler")
  expect_equal(s$updated_by, "P2")
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*)::integer AS n FROM team_settings")$n, 1L)
})

test_that("exercises: create, list, update (code stays), delete", {
  con <- local_test_db(); acc <- test_access()
  id <- ds_save_exercise(con, acc, "G1", list(name = "Rondo 4 mot 1", category = "pasning_mottak",
                                              themes = c("Samhandling", "Pasning"), min_players = "5",
                                              learning_points = "Åpne kroppen"), "P1")
  id2 <- ds_save_exercise(con, acc, "G1", list(name = "Rondo 4 mot 1", category = "smaaspill"), "P1")
  ex <- ds_list_exercises(con, acc, "G1")
  expect_equal(ex$code, c("rondo-4-mot-1", "rondo-4-mot-1-2"))
  expect_equal(ex$themes[[1]], c("Samhandling", "Pasning"))
  expect_equal(ex$themes[[2]], character())
  expect_equal(ex$min_players[1], 5L)
  expect_true(is.na(ex$max_players[1]))
  expect_equal(ex$learning_points[1], "Åpne kroppen")
  expect_equal(ex$easier[1], "")
  expect_equal(ex$source[1], "manual")

  ds_save_exercise(con, acc, "G1", list(name = "Rondo med to touch", category = "pasning_mottak",
                                        themes = "Samhandling"), "P2", id = id)
  e <- ds_get_exercise(con, acc, id)
  expect_equal(e$name, "Rondo med to touch")
  expect_equal(e$code, "rondo-4-mot-1")
  expect_equal(e$learning_points, "")
  expect_equal(c(e$created_by, e$updated_by), c("P1", "P2"))

  ds_delete_exercise(con, acc, id2)
  expect_equal(nrow(ds_list_exercises(con, acc, "G1")), 1)
  expect_error(ds_get_exercise(con, acc, id2), "Fant ikke")
})

test_that("training data in other groups cannot be read or changed", {
  con <- local_test_db(); acc <- test_access()
  expect_error(ds_list_season_themes(con, acc, "G2", 2026), "ikke tilgang")
  expect_error(ds_save_season(con, acc, "G2", 2026, rep("", 12), actor = "P1"), "ikke tilgang")
  expect_error(ds_get_team_settings(con, acc, "G2"), "ikke tilgang")
  expect_error(ds_save_team_settings(con, acc, "G2", list(), "P1"), "ikke tilgang")
  expect_error(ds_list_exercises(con, acc, "G2"), "ikke tilgang")
  DBI::dbExecute(con, "INSERT INTO exercises (spond_group_id, code, name, category, created_by, updated_by)
                       VALUES ('G2', 'fremmed', 'Fremmed', 'annet', 'P9', 'P9')")
  other <- DBI::dbGetQuery(con, "SELECT id FROM exercises WHERE spond_group_id = 'G2'")$id
  expect_error(ds_get_exercise(con, acc, other), "ikke tilgang")
  expect_error(ds_save_exercise(con, acc, "G1", list(name = "X", category = "annet"), "P1", id = other), "ikke tilgang")
  expect_error(ds_delete_exercise(con, acc, other), "ikke tilgang")
  expect_equal(DBI::dbGetQuery(con, "SELECT name FROM exercises WHERE id = $1", list(other))$name, "Fremmed")
})

test_that("invalid training data is rejected before anything is written", {
  con <- local_test_db(); acc <- test_access()
  expect_error(ds_save_exercise(con, acc, "G1", list(name = "X", category = "tull"), "P1"), "kategori")
  expect_error(ds_save_season(con, acc, "G1", 2026, c("Skadeforebygging", rep("", 11)), actor = "P1"), "sensitive")
  expect_equal(DBI::dbGetQuery(con, "SELECT ((SELECT count(*) FROM exercises) + (SELECT count(*) FROM season_themes))::integer AS n")$n, 0L)
})

test_that("an exercise keeps its drawing as JSON, and can drop it again", {
  con <- local_test_db(); acc <- test_access()
  id <- ds_save_exercise(con, acc, "G1", list(name = "Rondo", category = "annet", drawing = drawing_example_json()), "P1")
  e <- ds_get_exercise(con, acc, id)
  d <- drawing_validate(e$drawing)
  expect_equal(d$skisser[[1]]$tittel, "3 mot 1")
  expect_length(d$skisser[[1]]$objekter, 9)
  ds_save_exercise(con, acc, "G1", list(name = "Rondo", category = "annet", drawing = ""), "P1", id = id)
  expect_true(is.na(ds_get_exercise(con, acc, id)$drawing))
  expect_error(ds_save_exercise(con, acc, "G1", list(name = "Y", category = "annet", drawing = "{x"), "P1"), "Tegning")
})

# Training plans (migration 003) ---------------------------------------------------

test_that("plans are saved as versions per event and read back", {
  con <- local_test_db(); acc <- test_access()
  ref <- jsonlite::fromJSON(testthat::test_path("..", "..", "inst", "extdata", "referanse-okt.json"), simplifyVector = FALSE)
  v1 <- ds_save_plan(con, acc, "G1", "E1", ref, "P1")
  ref$tittel <- "Ny tittel"
  v2 <- ds_save_plan(con, acc, "G1", "E1", ref, "P2")
  v3 <- ds_save_plan(con, acc, "G1", "E2", ref, "P1")
  expect_equal(c(v1$version, v2$version, v3$version), c(1L, 2L, 1L))

  latest <- ds_list_plans(con, acc, "G1", c("E1", "E2", "E9"))
  expect_equal(latest$spond_event_id, c("E1", "E2"))
  expect_equal(latest$version, c(2L, 1L))
  expect_equal(latest$n_versions, c(2L, 1L))
  expect_equal(latest$title[1], "Ny tittel")

  vers <- ds_plan_versions(con, acc, "G1", "E1")
  expect_equal(vers$version, c(2L, 1L))
  expect_equal(vers$created_by, c("P2", "P1"))

  p <- ds_get_plan(con, acc, v1$id)
  expect_equal(p$plan$tittel, "Samhandling – spille på lag")
  expect_equal(p$event_id, "E1")
  expect_length(p$plan$ovelser[[1]]$tegning$skisser, 2)
  expect_error(ds_save_plan(con, acc, "G1", "E1", list(tittel = "X", ovelser = list()), "P1"), "1 til 6")
})

test_that("plans in other groups cannot be read or saved", {
  con <- local_test_db(); acc <- test_access()
  expect_error(ds_list_plans(con, acc, "G2", "E1"), "ikke tilgang")
  expect_error(ds_save_plan(con, acc, "G2", "E1", list(tittel = "X", ovelser = list(list(navn = "A"))), "P1"), "ikke tilgang")
  DBI::dbExecute(con, "INSERT INTO training_plans (spond_group_id, spond_event_id, version, plan, created_by)
                       VALUES ('G2', 'E1', 1, '{\"tittel\": \"X\", \"ovelser\": [{\"navn\": \"A\"}]}', 'P9')")
  other <- DBI::dbGetQuery(con, "SELECT id FROM training_plans WHERE spond_group_id = 'G2'")$id
  expect_error(ds_get_plan(con, acc, other), "ikke tilgang")
})

test_that("an exercise from a plan is put into the bank once, with its code", {
  con <- local_test_db(); acc <- test_access()
  ref <- jsonlite::fromJSON(testthat::test_path("..", "..", "inst", "extdata", "referanse-okt.json"), simplifyVector = FALSE)
  e <- plan_validate(ref)$ovelser[[2]]
  r1 <- ds_save_plan_exercise(con, acc, "G1", e, "P1", themes = "Samhandling")
  r2 <- ds_save_plan_exercise(con, acc, "G1", e, "P1")
  expect_true(r1$created)
  expect_false(r2$created)
  expect_equal(r2$id, r1$id)
  ex <- ds_get_exercise(con, acc, r1$id)
  expect_equal(ex$code, "hjem-bak-ballen")
  expect_equal(ex$category, "annet")                  # "Overgang – angrep og forsvar" is not a bank category
  expect_equal(ex$themes[[1]], "Samhandling")
  expect_match(ex$learning_points, "Mistet ball")
  expect_false(is.na(ex$drawing))
})

# Rights (migration 004) -----------------------------------------------------------

test_that("rights are given and taken, logged, and checked", {
  con <- local_test_db()
  sa <- list(superadmin = TRUE, admin = TRUE)
  adm <- list(superadmin = FALSE, admin = TRUE)
  trainer <- list(superadmin = FALSE, admin = FALSE)
  ids <- c("P-a", "P-b")
  ds_set_role(con, sa, "P-me", "P-a", "admin", TRUE, ids)
  ds_set_role(con, adm, "P-a", "P-b", "ai", TRUE, ids)
  ds_set_role(con, adm, "P-a", "P-b", "ai", FALSE, ids)
  roles <- ds_list_roles(con, ids)
  expect_equal(roles$is_admin[roles$spond_profile_id == "P-a"], TRUE)
  expect_equal(roles$can_use_ai[roles$spond_profile_id == "P-b"], FALSE)
  expect_equal(roles$updated_by[roles$spond_profile_id == "P-b"], "P-a")
  expect_equal(nrow(ds_get_role(con, "P-a")), 1)
  expect_equal(nrow(ds_get_role(con, "P-zz")), 0)
  log <- DBI::dbGetQuery(con, "SELECT spond_profile_id, role, granted, actor FROM app_role_log ORDER BY id")
  expect_equal(log$granted, c(TRUE, TRUE, FALSE))
  expect_equal(log$actor, c("P-me", "P-a", "P-a"))

  expect_error(ds_set_role(con, adm, "P-a", "P-b", "admin", TRUE, ids), "Bare superadmin")
  expect_error(ds_set_role(con, trainer, "P-b", "P-a", "ai", TRUE, ids), "ikke tilgang")
  expect_error(ds_set_role(con, sa, "P-me", "P-c", "ai", TRUE, ids), "ikke med i dette laget")
  expect_error(ds_set_role(con, sa, "P-me", "P-a", "kaffe", TRUE, ids), "Ukjent")
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*)::integer AS n FROM app_role_log")$n, 3L)
})
