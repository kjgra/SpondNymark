admin_args <- function(con = NULL, superadmin = "P-me", ctx = reactiveVal(teams_context(fake_group()))) {
  list(context = ctx, user = reactive(fake_user(fake_spond_groups_two())),
       db = db_handle(function() if (is.null(con)) stop("ingen database") else con),
       superadmin = superadmin, today = function() as.Date("2026-10-09"))
}

test_that("only the superadmin from the environment is superadmin", {
  expect_true(is_superadmin("P-me", "P-me"))
  expect_true(is_superadmin("P-me", " P-me "))
  expect_false(is_superadmin("P-me", ""))
  expect_false(is_superadmin("P-other", "P-me"))
  expect_false(is_superadmin(NA_character_, "P-me"))
  expect_false(is_superadmin(NULL, "P-me"))
  expect_s3_class(mod_admin_bar_ui("admin"), "shiny.tag")
})

test_that("the gear is shown only to admins, right after login (before a team is chosen)", {
  # No database here: reading the rights fails (logged), so no gear.
  suppressMessages(testServer(mod_admin_server, args = admin_args(superadmin = "P-other"), {
    expect_null(output$bar$html)
  }))
  suppressMessages(testServer(mod_admin_server, args = admin_args(superadmin = ""), {
    expect_null(output$bar$html)
  }))
  ctx <- reactiveVal(NULL)
  # No database here: the panel's outputs log a read error, which is expected.
  suppressMessages(testServer(mod_admin_server, args = admin_args(ctx = ctx), {
    expect_match(as.character(output$bar$html), "aria-label=\"Innstillinger\"")
    expect_match(as.character(output$bar$html), "proxy1-open")
    # Opened without a team: the panel uses the first team the trainer has.
    session$setInputs(open = 1)
    expect_equal(group_id(), user()$access$id[1])
  }))
})

test_that("season plan: shown for the chosen year, saved, and checked", {
  con <- local_test_db()
  testServer(mod_admin_server, args = admin_args(con), {
    session$setInputs(open = 1)
    html <- as.character(output$season_form$html)
    expect_match(html, "Januar")
    expect_match(html, "proxy1-theme_12")
    session$setInputs(theme_10 = "Samhandling", desc_10 = "Spille på lag", theme_9 = "Vending")
    session$setInputs(season_save = 1)
    expect_equal(msg()$type, "ok")
    expect_match(msg()$text, "2026")
    rows <- ds_list_season_themes(con, user()$access, "G2016", 2026)
    expect_equal(rows$theme, c("Vending", "Samhandling"))
    expect_equal(rows$updated_by, c("P-me", "P-me"))
    expect_match(as.character(output$season_form$html), "value=\"Samhandling\"")

    session$setInputs(year = "2027")
    expect_false(grepl("Samhandling", as.character(output$season_form$html)))

    session$setInputs(theme_1 = "", desc_1 = "Uten tema", season_save = 2)
    expect_equal(msg()$type, "error")
    expect_match(msg()$text, "januar")
    expect_match(as.character(output$season_msg$html), "alert-danger")
    expect_equal(nrow(ds_list_season_themes(con, user()$access, "G2016", 2027)), 0)
  })
})

test_that("team settings are saved and shown again", {
  con <- local_test_db()
  testServer(mod_admin_server, args = admin_args(con), {
    session$setInputs(open = 1)
    expect_match(as.character(output$team_form$html), "Øktlengde")
    session$setInputs(age_group = "G10", session_minutes = 75, pitch = "7er", equipment = "", principles = "",
                      team_save = 1)
    expect_equal(msg()$type, "ok")
    s <- ds_get_team_settings(con, user()$access, "G2016")
    expect_equal(c(s$age_group, s$session_minutes, s$pitch), c("G10", "75", "7er"))
    expect_match(as.character(output$team_form$html), "value=\"75\"")

    session$setInputs(session_minutes = 5, team_save = 2)
    expect_equal(msg()$type, "error")
    expect_equal(ds_get_team_settings(con, user()$access, "G2016")$session_minutes, "75")
  })
})

test_that("exercises: new, edit, delete with confirmation", {
  con <- local_test_db()
  acc <- fake_user(fake_spond_groups_two())$access
  themes <- rep("", 12); themes[10] <- "Samhandling"
  ds_save_season(con, acc, "G2016", 2026, themes, actor = "P-x")
  testServer(mod_admin_server, args = admin_args(con), {
    session$setInputs(open = 1)
    expect_match(as.character(output$ex_body$html), "Ingen øvelser")

    session$setInputs(ex_new = 1)
    form <- as.character(output$ex_body$html)
    expect_match(form, "Ny øvelse")
    expect_match(form, "Samhandling")              # theme from the season plan is offered
    session$setInputs(ex_name = "Rondo 4 mot 1", ex_category = "", ex_save = 1)
    expect_equal(msg()$type, "error")
    expect_match(msg()$text, "kategori")
    expect_equal(editing(), "new")                 # the form stays open
    expect_match(as.character(output$ex_form_msg$html), "kategori")   # shown next to the buttons
    expect_null(output$ex_msg$html)

    session$setInputs(ex_category = "pasning_mottak", ex_themes = c("Samhandling", "Pasning"),
                      ex_min_players = 5, ex_max_players = NA, ex_learning_points = "Åpne kroppen", ex_save = 2)
    expect_equal(msg()$type, "ok")
    expect_null(editing())
    ex <- ds_list_exercises(con, acc, "G2016")
    expect_equal(ex$code, "rondo-4-mot-1")
    expect_equal(ex$themes[[1]], c("Samhandling", "Pasning"))
    list_html <- as.character(output$ex_body$html)
    expect_match(list_html, "Pasning og mottak")
    expect_match(list_html, "minst 5 spillere")

    id <- as.character(ex$id)
    session$setInputs(ex_edit = id)
    expect_match(as.character(output$ex_body$html), "value=\"Rondo 4 mot 1\"")
    session$setInputs(ex_name = "Rondo med to touch", ex_save = 3)
    expect_equal(ds_get_exercise(con, acc, ex$id)$name, "Rondo med to touch")

    session$setInputs(ex_edit = "999999")          # unknown id: ignored
    expect_null(editing())

    session$setInputs(ex_delete = id)
    expect_match(as.character(output$ex_body$html), "Slette øvelsen?")
    session$setInputs(ex_delete_cancel = id)
    expect_null(confirm_delete())
    session$setInputs(ex_delete_ok = id)           # not confirmed first: nothing happens
    expect_equal(nrow(ds_list_exercises(con, acc, "G2016")), 1)
    session$setInputs(ex_delete = id)
    session$setInputs(ex_delete_ok = id)
    expect_equal(nrow(ds_list_exercises(con, acc, "G2016")), 0)
    expect_match(msg()$text, "slettet")
    expect_match(as.character(output$ex_msg$html), "slettet")
  })
})

test_that("saving is refused on the server for others than the superadmin", {
  con <- local_test_db()
  testServer(mod_admin_server, args = admin_args(con, superadmin = "P-other"), {
    session$setInputs(theme_10 = "Samhandling", season_save = 1)
    expect_match(msg()$text, "ikke tilgang")
    session$setInputs(ex_new = 1, ex_name = "X", ex_category = "annet", ex_save = 1)
    expect_equal(DBI::dbGetQuery(con, "SELECT (SELECT count(*) FROM season_themes) + (SELECT count(*) FROM exercises) AS n")$n |>
                   as.integer(), 0L)
  })
})

test_that("a database error gives a general message, and the details go to the log", {
  logged <- character()
  withCallingHandlers(
    testServer(mod_admin_server, args = admin_args(con = NULL), {
      session$setInputs(open = 1)
      expect_null(output$team_form$html)
      expect_match(msg()$text, "Fikk ikke hentet")
      session$setInputs(age_group = "G10", session_minutes = 60, team_save = 1)
      expect_match(msg()$text, "Fikk ikke lagret")
      expect_false(grepl("ingen database", msg()$text))   # no details for the user
    }),
    message = function(m) {
      logged <<- c(logged, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_true(any(grepl("Lesing i adminpanelet feilet: ingen database", logged)))
  expect_true(any(grepl("Lagring i adminpanelet feilet: ingen database", logged)))
})

test_that("the drawing of an exercise can be previewed and is saved with it", {
  con <- local_test_db()
  acc <- fake_user(fake_spond_groups_two())$access
  testServer(mod_admin_server, args = admin_args(con), {
    session$setInputs(open = 1, ex_new = 1)
    expect_match(as.character(output$ex_body$html), "Tegning \\(JSON")
    session$setInputs(ex_drawing = "{ikke json", ex_preview = 1)
    expect_match(as.character(output$ex_preview_box$html), "ikke gyldig JSON")
    session$setInputs(ex_drawing = drawing_example_json(), ex_preview = 2)
    box <- as.character(output$ex_preview_box$html)
    expect_match(box, "data:image/png;base64,")
    expect_match(box, "Ingen kollisjoner")
    session$setInputs(ex_name = "Med tegning", ex_category = "annet", ex_save = 1)
    expect_equal(msg()$type, "ok")
    ex <- ds_list_exercises(con, acc, "G2016")
    expect_false(is.na(ex$drawing))
    expect_match(as.character(output$ex_body$html), "med tegning")
    session$setInputs(ex_edit = as.character(ex$id))
    expect_null(preview())                        # a new form starts without a preview
    form <- as.character(output$ex_body$html)
    expect_match(form, '"skisser"')
    expect_match(form, '\\{"type":"kjegle","x":0,"y":0\\}')   # readable key order after jsonb
  })
})

test_that("trainers' rights are shown and changed in the panel", {
  con <- local_test_db()
  user <- fake_user(fake_spond_groups_two())
  args <- admin_args(con)
  # A second trainer with a profile in G2016, so there is someone to give rights to
  g <- fake_group()
  g$members <- rbind(g$members, transform(g$members[g$members$id == "M-me", ], id = "M-t2", first_name = "Trine",
                                          last_name = "Trener", profile_id = "P-t2"))
  args$context <- reactiveVal(teams_context(g))
  testServer(mod_admin_server, args = args, {
    session$setInputs(open = 1)
    html <- as.character(output$roles_body$html)
    expect_match(html, "Superadmin")
    expect_match(html, "Trine T.")
    session$setInputs(role = list(p = "P-t2", f = "ai", v = TRUE))
    expect_equal(msg()$type, "ok")
    expect_match(msg()$text, "Trine T. har fått KI-tilgang")
    session$setInputs(role = list(p = "P-t2", f = "admin", v = TRUE))
    r <- ds_get_role(con, "P-t2")
    expect_true(r$can_use_ai)
    expect_true(r$is_admin)
    expect_match(as.character(output$roles_body$html), "Endret av Kjetil G.")
    # Unknown trainers and the superadmin's own row are ignored
    session$setInputs(role = list(p = "P-other", f = "ai", v = TRUE))
    session$setInputs(role = list(p = "P-me", f = "ai", v = FALSE))
    expect_equal(nrow(ds_get_role(con, "P-other")), 0)
    expect_equal(nrow(ds_get_role(con, "P-me")), 0)
  })
})

test_that("an admin gets the gear and can give KI, but not admin", {
  con <- local_test_db()
  ds_set_role(con, list(superadmin = TRUE, admin = TRUE), "P-boss", "P-me", "admin", TRUE, "P-me")
  g <- fake_group()
  g$members <- rbind(g$members, transform(g$members[g$members$id == "M-me", ], id = "M-t2", first_name = "Trine",
                                          last_name = "Trener", profile_id = "P-t2"))
  args <- admin_args(con, superadmin = "P-boss")      # Kjetil is admin here, not superadmin
  args$context <- reactiveVal(teams_context(g))
  testServer(mod_admin_server, args = args, {
    expect_match(as.character(output$bar$html), "Innstillinger")
    session$setInputs(open = 1)
    html <- as.character(output$roles_body$html)
    expect_match(html, "aria-label=\"Admin for Trine T.\" disabled")
    session$setInputs(role = list(p = "P-t2", f = "ai", v = TRUE))
    expect_true(ds_get_role(con, "P-t2")$can_use_ai)
    session$setInputs(role = list(p = "P-t2", f = "admin", v = TRUE))
    expect_match(msg()$text, "Bare superadmin")
    expect_false(ds_get_role(con, "P-t2")$is_admin)
  })
})

test_that("an admin whose right is taken away cannot change anything in an open panel", {
  con <- local_test_db()
  boss <- list(superadmin = TRUE, admin = TRUE)
  ds_set_role(con, boss, "P-boss", "P-me", "admin", TRUE, "P-me")
  args <- admin_args(con, superadmin = "P-boss")
  testServer(mod_admin_server, args = args, {
    session$setInputs(open = 1)
    ds_set_role(con, boss, "P-boss", "P-me", "admin", FALSE, "P-me")   # taken away while the panel is open
    session$setInputs(theme_10 = "Samhandling", season_save = 1)
    expect_equal(msg()$type, "error")
    expect_match(msg()$text, "ikke tilgang")
    expect_equal(nrow(ds_list_season_themes(con, user()$access, "G2016", 2026)), 0)
  })
})

test_that("the panel has its own team picker", {
  con <- local_test_db()
  suppressMessages(testServer(mod_admin_server, args = admin_args(con), {
    session$setInputs(open = 1)
    expect_equal(group_id(), "G2016")
    session$setInputs(admin_group = "G-senior")
    expect_equal(group_id(), "G-senior")
    session$setInputs(admin_group = "G-annen")                  # not one of the trainer's teams: ignored
    expect_equal(group_id(), "G-senior")
  }))
})

test_that("admins give app access; admins always have it", {
  con <- local_test_db()
  ds_set_role(con, list(superadmin = TRUE, admin = TRUE), "P-boss", "P-me", "admin", TRUE, "P-me")
  g <- fake_group()
  g$members <- rbind(g$members, transform(g$members[g$members$id == "M-me", ], id = "M-t2", first_name = "Trine",
                                          last_name = "Trener", profile_id = "P-t2"))
  args <- admin_args(con, superadmin = "P-boss")
  args$context <- reactiveVal(teams_context(g))
  suppressMessages(testServer(mod_admin_server, args = args, {
    session$setInputs(open = 1)
    html <- as.character(output$roles_body$html)
    expect_match(html, "aria-label=\"App-tilgang for Kjetil G.\" checked disabled")   # admin
    expect_match(html, "aria-label=\"App-tilgang for Trine T.\" onchange")
    session$setInputs(role = list(p = "P-t2", f = "app", v = TRUE))
    expect_match(msg()$text, "Trine T. har fått app-tilgang")
  }))
  expect_true(ds_get_role(con, "P-t2")$can_use_app)
  expect_equal(login_check(list(run = function(f) f(con)), "P-t2", superadmin = "P-boss"), "ok")
  expect_equal(login_check(list(run = function(f) f(con)), "P-me", superadmin = "P-boss"), "ok")       # admin
  expect_equal(login_check(list(run = function(f) f(con)), "P-x", superadmin = "P-boss"), "denied")
  expect_equal(DBI::dbGetQuery(con, "SELECT role FROM app_role_log WHERE spond_profile_id = 'P-t2'")$role, "app")
})

test_that("the KI tab shows this month's use, and only the superadmin changes the limits", {
  con <- local_test_db()
  u <- list(kind = "draft", model = "claude-haiku-5-5", input_tokens = 1000, output_tokens = 6000,
            cache_read_tokens = 0, cache_write_tokens = 7000, cost_usd = 0.012, duration_ms = 30000,
            status = "ok", error_code = "")
  ds_log_ai_usage(con, fake_user(fake_spond_groups_two())$access, "G2016", "P-me", u)
  args <- admin_args(con)
  args$today <- function() Sys.Date()
  suppressMessages(testServer(mod_admin_server, args = args, {
    session$setInputs(open = 1)
    html <- as.character(output$ai_overview$html)
    expect_match(html, "0,12 kr")
    expect_match(html, "Kjetil G.")
    expect_match(html, "Haiku 5.5")
    expect_match(as.character(output$ai_save_button$html), "Lagre grenser og modell")
    session$setInputs(ai_model = "claude-sonnet-5-5", ai_group_limit = 50, ai_total_limit = 9, ai_max_new = 1,
                      ai_usd_nok = 9.6, ai_save = 1)
    expect_equal(msg()$type, "ok")
    session$setInputs(ai_group_limit = -5, ai_save = 2)
    expect_match(msg()$text, "Grensen per lag")
  }))
  s <- ds_get_ai_settings(con)
  expect_equal(s$model, "claude-sonnet-5-5")
  expect_equal(s$group_limit_nok, 50)

  # An admin (not superadmin) sees the numbers but cannot change the limits.
  ds_set_role(con, list(superadmin = TRUE, admin = TRUE), "P-boss", "P-me", "admin", TRUE, "P-me")
  args <- admin_args(con, superadmin = "P-boss")
  suppressMessages(testServer(mod_admin_server, args = args, {
    session$setInputs(open = 1)
    expect_match(as.character(output$ai_settings_form$html), "Bare superadmin kan endre")
    expect_null(output$ai_save_button$html)
    session$setInputs(ai_model = "claude-haiku-5-5", ai_group_limit = 10, ai_total_limit = 9, ai_max_new = 1,
                      ai_usd_nok = 9.6, ai_save = 1)
    expect_match(msg()$text, "Bare superadmin")
  }))
  expect_equal(ds_get_ai_settings(con)$group_limit_nok, 50)
})
