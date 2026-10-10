# Runs against the test database (see helper-db.R); skipped without one.
plan_event <- function(match = FALSE) {
  e <- event_minimal(fake_spond_events()[[1]])   # Trening Ulv, 10. okt. 2026 10:00
  e$match <- match
  e
}
plan_args <- function(con, ev = reactiveVal(plan_event()), now = function() as.POSIXct("2026-10-09 12:00", tz = "UTC"),
                      make_pdf = function(...) stop("ikke i denne testen")) {
  list(context = reactiveVal(teams_context(fake_group())), user = reactive(fake_user(fake_spond_groups_two())),
       db = db_handle(function() con), event = ev, now = now, make_pdf = make_pdf)
}
plan_acc <- function() fake_user(fake_spond_groups_two())$access
add_bank <- function(con) {
  ds_save_exercise(con, plan_acc(), "G2016", list(name = "Rondo 4 mot 1", category = "pasning_mottak",
                                                  learning_points = "- Se opp\n- Åpne kroppen",
                                                  drawing = drawing_example_json()), "P-x", rights = test_admin())
  ds_save_exercise(con, plan_acc(), "G2016", list(name = "4 mot 4", category = "smaaspill"), "P-x", rights = test_admin())
}

test_that("matches get no plan section", {
  con <- local_test_db()
  testServer(mod_plans_server, args = plan_args(con, ev = reactiveVal(plan_event(match = TRUE))), {
    expect_null(output$section$html)
  })
})

test_that("a plan is put together from the bank, saved, changed and kept as versions", {
  con <- local_test_db()
  add_bank(con)
  themes <- rep("", 12); themes[10] <- "Samhandling"
  ds_save_season(con, plan_acc(), "G2016", 2026, themes, actor = "P-x", rights = test_admin())
  testServer(mod_plans_server, args = plan_args(con), {
    html <- as.character(output$section$html)
    expect_match(html, "Ingen opplegg enn")
    expect_match(html, "Lag opplegg")

    session$setInputs(new = 1)
    expect_false(is.null(editor()))
    expect_equal(nrow(editor()$bank), 2)

    # Nothing chosen: a clear message, nothing saved
    session$setInputs(ed_tittel = "Samhandling", ed_tema = "Samhandling", ed_ovelser = character(),
                      ed_rotasjon = "ja", ed_oppvarming = 10, ed_stasjon = 15, ed_bytte = 2, ed_avslutning = 8,
                      ed_save = 1)
    expect_match(ed_msg(), "minst én")
    expect_equal(nrow(ds_plan_versions(con, plan_acc(), "G2016", "E-ulv")), 0)

    session$setInputs(ed_ovelser = c("rondo-4-mot-1", "4-mot-4"))
    expect_match(as.character(output$ed_summary$html), "Totalt 50 min")
    session$setInputs(ed_save = 2)
    expect_null(editor())
    expect_equal(msg()$type, "ok")
    html <- as.character(output$section$html)
    expect_match(html, "Versjon 1")
    expect_match(html, "Rondo 4 mot 1")
    expect_match(html, "2 stasjoner med rotasjon")
    expect_match(html, "Last ned PDF")

    p1 <- current()$plan
    expect_equal(p1$ovelser[[1]]$kilde, "bank")
    expect_equal(p1$ovelser[[1]]$laeringsmomenter, list("Se opp", "Åpne kroppen"))

    # Change: the kept exercise keeps its copy even if the bank changes
    ex <- ds_list_exercises(con, plan_acc(), "G2016")
    ds_save_exercise(con, plan_acc(), "G2016", list(name = "Rondo 4 mot 1", category = "pasning_mottak",
                                                    learning_points = "Ny tekst"), "P-x",
                     id = ex$id[ex$code == "rondo-4-mot-1"], rights = test_admin())
    session$setInputs(edit = 1)
    expect_equal(vapply(editor()$kept, `[[`, "", "kode"), c("rondo-4-mot-1", "4-mot-4"))
    session$setInputs(ed_ovelser = c("4-mot-4", "rondo-4-mot-1"), ed_rotasjon = "nei", ed_save = 3)
    p2 <- current()$plan
    expect_equal(current()$version, 2L)
    expect_equal(vapply(p2$ovelser, `[[`, "", "navn"), c("4 mot 4", "Rondo 4 mot 1"))
    expect_equal(p2$ovelser[[2]]$laeringsmomenter, list("Se opp", "Åpne kroppen"))
    expect_equal(p2$tidsplan$stasjoner$grupper, 1L)
    expect_match(as.character(output$section$html), "Versjon 2 av 2")

    session$setInputs(version = as.character(ds_plan_versions(con, plan_acc(), "G2016", "E-ulv")$id[2]))
    expect_equal(current()$version, 1L)
  })
})

test_that("the editor stops sensitive words", {
  con <- local_test_db()
  add_bank(con)
  testServer(mod_plans_server, args = plan_args(con), {
    session$setInputs(new = 1, ed_tittel = "Økt", ed_tema = "", ed_ovelser = "4-mot-4", ed_rotasjon = "nei",
                      ed_oppvarming = 10, ed_stasjon = 15, ed_bytte = 0, ed_avslutning = 5,
                      ed_merknad = "Ola er skadet", ed_save = 1)
    expect_match(ed_msg(), "sensitive")
    expect_equal(nrow(ds_plan_versions(con, plan_acc(), "G2016", "E-ulv")), 0)
  })
})

test_that("past events show their plan, but cannot get a new one", {
  con <- local_test_db()
  add_bank(con)
  later <- function() as.POSIXct("2026-10-20 12:00", tz = "UTC")
  testServer(mod_plans_server, args = plan_args(con, now = later), {
    expect_match(as.character(output$section$html), "Ingen opplegg for denne treningen")
    session$setInputs(new = 1)
    expect_null(editor())
  })
  p <- plan_from_exercises(ds_list_exercises(con, plan_acc(), "G2016")[1, ], plan_settings_default())
  ds_save_plan(con, plan_acc(), "G2016", "E-ulv", p, "P-me")
  testServer(mod_plans_server, args = plan_args(con, now = later), {
    html <- as.character(output$section$html)
    expect_match(html, "Last ned PDF")
    expect_false(grepl("Endre", html))
    expect_match(html, "laget av Kjetil G.")
  })
})

test_that("the PDF gets group names from the approved group proposal", {
  con <- local_test_db()
  acc <- plan_acc()
  add_bank(con)
  ds_save_exercise(con, acc, "G2016", list(name = "Haien", category = "oppvarming"), "P-x", rights = test_admin())
  id <- ds_save_proposal(con, acc, "P-me", "G2016", "Tirsdag", labels = c("Gruppe A", "Gruppe B"),
                         assignments = c(`M-1` = "Gruppe A", `M-3` = "Gruppe B"), event_id = "E-ulv")
  ds_transition(con, acc, id, "submit", "P-me")
  ds_transition(con, acc, id, "approve", "P-me")
  testServer(mod_plans_server, args = plan_args(con), {
    session$setInputs(new = 1)
    expect_equal(editor()$n_groups, 2L)
    session$setInputs(ed_tittel = "X", ed_tema = "", ed_ovelser = c("rondo-4-mot-1", "4-mot-4", "haien"),
                      ed_rotasjon = "ja", ed_oppvarming = 5, ed_stasjon = 10, ed_bytte = 1, ed_avslutning = 5)
    g <- print_groups()
    expect_equal(vapply(g, `[[`, "", "navn"), c("Gruppe A", "Gruppe B"))
    expect_equal(g[[1]]$spillere, "Emma Haugen")
    expect_equal(g[[2]]$spillere, "Noah S.")
    expect_match(as.character(output$ed_summary$html), "Godkjent gruppeforslag har 2 grupper, men rotasjonen har 3")
    session$setInputs(ed_ovelser = c("rondo-4-mot-1", "4-mot-4"))
    expect_false(grepl("Godkjent gruppeforslag", as.character(output$ed_summary$html)))
  })
})

test_that("groups for the PDF follow the proposal's order and skip unknown members", {
  g <- plan_print_groups(data.frame(label = c("B", "A"), sort_order = c(2, 1)),
                         data.frame(spond_member_id = c("m1", "m2", "m9"), label = c("A", "B", "A")),
                         data.frame(id = c("m1", "m2"), display_name = c("Emma H.", "Ola N.")))
  expect_equal(vapply(g, `[[`, "", "navn"), c("A", "B"))
  expect_equal(g[[1]]$spillere, "Emma H.")
})

# KI («Lag med KI», T3b) ---------------------------------------------------------------

ki_args <- function(con, rights = list(superadmin = FALSE, admin = FALSE, ai = TRUE),
                    answer = fake_answer(example_text()), seen = new.env()) {
  c(plan_args(con), list(
    rights = reactive(rights),
    ai_http = function(path, body) {
      seen$count_path <- path
      list(status = 200L, body = list(input_tokens = 9000))
    },
    ai_async = function(path, body) {
      seen$body <- body
      promises::promise_resolve(answer)
    },
    ai_key = function() "sk-test"))
}

# Let the ExtendedTask finish.
ki_wait <- function(session, task) {
  for (i in 1:100) {
    later::run_now(0.01)
    session$flushReact()
    if (!identical(isolate(task$status()), "running")) break
  }
}

test_that("trainers without KI access see no KI button", {
  con <- local_test_db()
  testServer(mod_plans_server, args = ki_args(con, rights = no_rights()), {
    expect_false(grepl("Lag med KI", as.character(output$section$html)))
    session$setInputs(ki = 1)
    expect_null(ki_open())
  })
})

test_that("KI makes a plan in the background, with price before and cost after", {
  con <- local_test_db()
  add_bank(con)
  themes <- rep("", 12); themes[10] <- "Samhandling"
  ds_save_season(con, plan_acc(), "G2016", 2026, themes, actor = "P-x", rights = test_admin())
  ds_save_team_settings(con, plan_acc(), "G2016", list(age_group = "G10", session_minutes = "65"), "P-x", rights = test_admin())
  seen <- new.env()
  testServer(mod_plans_server, args = ki_args(con, seen = seen), {
    html <- as.character(output$section$html)
    expect_match(html, "Lag med KI")
    session$setInputs(ki = 1)
    o <- ki_open()
    expect_equal(o$ctx$theme, "Samhandling")
    expect_equal(o$ctx$minutes, 65L)
    expect_equal(seen$count_path, "/v1/messages/count_tokens")
    expect_equal(o$est$per_model[["claude-haiku-5-5"]]$text, "ca. 4 øre")     # 9000 in, 6000 out
    expect_match(o$facts, "påmeldte")

    session$setInputs(ki_theme = "Samhandling", ki_minutes = 65, ki_wish = "Ola er skadet", ki_model = "claude-haiku-5-5",
                      ki_go = 1)
    expect_match(ki_msg(), "sensitive")
    expect_identical(ki_task$status(), "initial")

    session$setInputs(ki_wish = "Emma trenger mer avslutning.", ki_go = 2)        # a player's name
    expect_match(ki_msg(), "Navn sendes ikke til KI")
    expect_identical(ki_task$status(), "initial")

    session$setInputs(ki_wish = "Mer avslutning.", ki_go = 3)
    expect_null(ki_open())
    ki_wait(session, ki_task)
    expect_identical(ki_task$status(), "success")
    expect_match(seen$body$messages[[1]]$content, "Ønsker fra treneren: Mer avslutning.", fixed = TRUE)
    expect_false(grepl("Kjetil|Emma|Haugen", seen$body$messages[[1]]$content))   # no names
    expect_equal(msg()$type, "ok")
    expect_match(msg()$text, "versjon 1")
    html <- as.character(output$section$html)
    expect_match(html, "Samhandling – spille på lag")
    expect_match(html, "med KI \\(Haiku 5.5, ca\\. 3 øre\\)")
    expect_match(html, "Nytt med KI")
  })
  plan <- DBI::dbGetQuery(con, "SELECT source, model FROM training_plans")
  expect_equal(plan$source, "ai")
  expect_equal(DBI::dbGetQuery(con, "SELECT status FROM ai_usage")$status, "ok")
})

test_that("KI taken away while the dialog is open: the call is not made", {
  con <- local_test_db()
  allowed <- TRUE
  seen <- new.env()
  args <- ki_args(con, seen = seen)
  args$rights_now <- function() list(superadmin = FALSE, admin = FALSE, ai = allowed, app = TRUE)
  testServer(mod_plans_server, args = args, {
    session$setInputs(ki = 1)
    expect_false(is.null(ki_open()))
    allowed <<- FALSE
    session$setInputs(ki_theme = "", ki_minutes = 60, ki_wish = "", ki_model = "claude-haiku-5-5", ki_go = 1)
    expect_match(ki_msg(), "ikke tilgang til KI")
    expect_identical(ki_task$status(), "initial")
    expect_null(seen$body)
  })
})

test_that("a failed KI answer is logged and explained", {
  con <- local_test_db()
  testServer(mod_plans_server, args = ki_args(con, answer = fake_answer("{}", stop_reason = "max_tokens")), {
    session$setInputs(ki = 1)
    session$setInputs(ki_theme = "", ki_minutes = 60, ki_wish = "", ki_model = "claude-sonnet-5-5", ki_go = 1)
    ki_wait(session, ki_task)
    expect_identical(ki_task$status(), "success")          # handled: the result carries the message
    expect_equal(msg()$type, "error")
    expect_match(msg()$text, "for langt")
  })
  log <- DBI::dbGetQuery(con, "SELECT status, error_code, model FROM ai_usage")
  expect_equal(log$status, "error")
  expect_equal(log$error_code, "max_tokens")
  expect_equal(log$model, "claude-sonnet-5-5")
  expect_equal(nrow(DBI::dbGetQuery(con, "SELECT id FROM training_plans")), 0)
})

test_that("an empty budget stops KI before the dialog", {
  con <- local_test_db()
  DBI::dbExecute(con, "INSERT INTO ai_usage (spond_profile_id, spond_group_id, kind, model, cost_usd, status)
                       VALUES ('P-x', 'G2016', 'draft', 'claude-haiku-5-5', 5, 'ok')")
  testServer(mod_plans_server, args = ki_args(con), {
    session$setInputs(ki = 1)
    expect_null(ki_open())
    expect_match(msg()$text, "av 30 kr")
  })
})

test_that("Juster med KI sends the plan shown and saves a new version", {
  con <- local_test_db()
  ref <- jsonlite::fromJSON(testthat::test_path("..", "..", "inst", "extdata", "referanse-okt.json"), simplifyVector = FALSE)
  ds_save_plan(con, plan_acc(), "G2016", "E-ulv", ref, "P-me")
  seen <- new.env()
  answer <- fake_answer(example_text(function(a) { a$tittel <- "Justert økt"; a }))
  testServer(mod_plans_server, args = ki_args(con, seen = seen, answer = answer), {
    expect_match(as.character(output$section$html), "Juster med KI")
    session$setInputs(ki_revise = 1)
    o <- ki_open()
    expect_equal(o$base$version, 1L)
    expect_equal(o$ctx$minutes, 65)
    session$setInputs(ki_minutes = 65, ki_wish = "", ki_model = "claude-haiku-5-5", ki_go = 1)
    expect_match(ki_msg(), "hva som skal endres")
    session$setInputs(ki_wish = "Legg til en øvelse og bruk mindre grupper.", ki_go = 2)
    ki_wait(session, ki_task)
    expect_match(seen$body$messages[[1]]$content, "^# Gjeldende opplegg")
    expect_match(seen$body$messages[[1]]$content, "Legg til en øvelse og bruk mindre grupper.", fixed = TRUE)
    expect_match(msg()$text, "Det justerte opplegget er lagret som versjon 2")
    expect_match(as.character(output$section$html), "Justert økt")
  })
  expect_equal(DBI::dbGetQuery(con, "SELECT kind FROM ai_usage")$kind, "revision")
})

test_that("trainers comment on a plan, and KI can take the comments into account", {
  con <- local_test_db()
  ref <- jsonlite::fromJSON(testthat::test_path("..", "..", "inst", "extdata", "referanse-okt.json"), simplifyVector = FALSE)
  ds_save_plan(con, plan_acc(), "G2016", "E-ulv", ref, "P-me")
  seen <- new.env()
  testServer(mod_plans_server, args = ki_args(con, seen = seen), {
    expect_match(as.character(output$comment_form$html), "Øvelse 2: Hjem bak ballen")
    session$setInputs(c_where = "", c_text = "Ola er skadet", c_add = 1)
    expect_match(c_msg(), "sensitive")
    session$setInputs(c_where = "", c_text = "Noah Sand bør stå i mål", c_add = 11)    # a player's name
    expect_match(c_msg(), "navn sendes ikke dit")
    expect_equal(nrow(ds_list_plan_comments(con, plan_acc(), "G2016", "E-ulv")), 0)
    session$setInputs(c_where = "2", c_text = "For mye kø.", c_add = 2)
    session$setInputs(c_where = "", c_text = "Kortere oppvarming.", c_add = 3)
    expect_null(c_msg())
    html <- as.character(output$comments$html)
    expect_match(html, "Øvelse 2 \\(Hjem bak ballen\\)")
    expect_match(html, "Kortere oppvarming.")
    expect_match(html, "Kjetil G.")

    session$setInputs(ki_revise = 1)
    o <- ki_open()
    expect_equal(nrow(o$comments), 2)
    # Only the second comment is ticked; no other wish.
    session$setInputs(ki_minutes = 65, ki_wish = "", ki_model = "claude-haiku-5-5",
                      ki_comments = as.character(o$comments$id[2]), ki_go = 1)
    ki_wait(session, ki_task)
    sent <- seen$body$messages[[1]]$content
    expect_match(sent, "# Kommentarer fra trenerne\n\n- Hele økta: Kortere oppvarming.", fixed = TRUE)
    expect_false(grepl("For mye kø|Kjetil", sent))
    expect_false(grepl("# Endringsønske", sent, fixed = TRUE))
    expect_match(as.character(output$comments$html), "tatt med i versjon 2")
  })
  used <- DBI::dbGetQuery(con, "SELECT body, used_in_plan_id FROM training_plan_comments ORDER BY id")
  expect_true(is.na(used$used_in_plan_id[1]))
  expect_false(is.na(used$used_in_plan_id[2]))
})

test_that("approving a version puts the chosen exercises into the bank as candidates", {
  con <- local_test_db()
  ds_save_exercise(con, plan_acc(), "G2016", list(name = "Haien og fiskene", category = "oppvarming"), "P-x", rights = test_admin())
  ref <- jsonlite::fromJSON(testthat::test_path("..", "..", "inst", "extdata", "referanse-okt.json"), simplifyVector = FALSE)
  ref$ovelser[[2]]$kilde <- "justert"; ref$ovelser[[2]]$basert_pa <- "haien-og-fiskene"
  ds_save_plan(con, plan_acc(), "G2016", "E-ulv", ref, "P-me", source = "ai")
  testServer(mod_plans_server, args = plan_args(con), {
    expect_match(as.character(output$section$html), "Godkjenn versjon 1")
    session$setInputs(approve = 1)
    a <- approving()
    expect_length(a$cands, 3)
    expect_equal(vapply(a$cands, `[[`, "", "default"), c("save", "skip", "save"))
    session$setInputs(ap_1 = "save", ap_2 = "variant", ap_3 = "skip", ap_go = 1)
    expect_null(approving())
    expect_match(msg()$text, "Versjon 1 er godkjent. 2 øvelser er lagt i banken som kandidat.")
    html <- as.character(output$section$html)
    expect_match(html, "Godkjent av Kjetil G.")
    expect_match(html, "Angre godkjenning")
    expect_false(grepl("Godkjenn versjon", html))
    session$setInputs(unapprove = 1)
    expect_match(msg()$text, "angret")
    expect_match(as.character(output$section$html), "Godkjenn versjon 1")
  })
  bank <- ds_list_exercises(con, plan_acc(), "G2016")
  new <- bank[bank$source == "ai", , drop = FALSE]
  expect_equal(sort(new$code), c("3-mot-1-alltid-to-alternativer", "hjem-bak-ballen"))
  expect_true(all(new$status == "candidate"))
  expect_equal(new$based_on[new$code == "hjem-bak-ballen"], "haien-og-fiskene")
  expect_equal(new$themes[[1]], "Samhandling – spille på lag")
})

test_that("a plan made only from the bank is approved at once", {
  con <- local_test_db()
  add_bank(con)
  bank <- ds_list_exercises(con, plan_acc(), "G2016")
  p <- plan_from_exercises(bank, plan_settings_default("Pasning"))
  ds_save_plan(con, plan_acc(), "G2016", "E-ulv", p, "P-me")
  testServer(mod_plans_server, args = plan_args(con), {
    session$setInputs(approve = 1)
    expect_null(approving())
    expect_equal(msg()$text, "Versjon 1 er godkjent.")
  })
})
