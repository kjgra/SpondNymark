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
                                                  drawing = drawing_example_json()), "P-x")
  ds_save_exercise(con, plan_acc(), "G2016", list(name = "4 mot 4", category = "smaaspill"), "P-x")
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
  ds_save_season(con, plan_acc(), "G2016", 2026, themes, actor = "P-x")
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
                     id = ex$id[ex$code == "rondo-4-mot-1"])
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
  ds_save_exercise(con, acc, "G2016", list(name = "Haien", category = "oppvarming"), "P-x")
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
