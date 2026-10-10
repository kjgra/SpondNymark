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

test_that("the gear is shown only to the superadmin, and only when a team is chosen", {
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
    expect_null(output$bar$html)
    ctx(teams_context(fake_group()))
    session$flushReact()
    expect_match(as.character(output$bar$html), "aria-label=\"Innstillinger\"")
    expect_match(as.character(output$bar$html), "proxy1-open")
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

test_that("admins add and remove e-mail addresses and numbers on the allowlist", {
  con <- local_test_db()
  withr::local_envvar(SPONDNYMARK_LOGIN_KEY = "testnokkel")
  suppressMessages(testServer(mod_admin_server, args = admin_args(con), {
    session$setInputs(open = 1)
    expect_match(as.character(output$allow_body$html), "Listen er tom")
    session$setInputs(allow_new = "ola", allow_add = 1)
    expect_equal(msg()$type, "error")
    expect_match(msg()$text, "8 siffer")
    session$setInputs(allow_new = " Ola@Klubb.no ", allow_add = 2)
    expect_equal(msg()$type, "ok")
    expect_match(msg()$text, "o•••@klubb.no er lagt til")
    session$setInputs(allow_new = "ola@klubb.no", allow_add = 3)
    expect_match(msg()$text, "stod allerede")
    session$setInputs(allow_new = "99887766", allow_add = 4)
    html <- as.character(output$allow_body$html)
    expect_match(html, "+47 •••• ••66", fixed = TRUE)
    expect_match(html, "Ikke logget inn ennå")
    expect_match(html, "lagt til av Kjetil G.")
    expect_false(grepl("ola@klubb|99887766", html))

    # After the first login the name from Spond is shown.
    ds_allowlist_login(con, login_hash(login_identifier("ola@klubb.no")), "P-me")
    session$setInputs(allow_new = "", allow_add = 5)            # refresh happens after any save attempt
    rows <- ds_list_allowlist(con, list(admin = TRUE))
    session$setInputs(allow_remove = rows$id_hash[rows$kind == "phone"])
    expect_match(msg()$text, "Fjernet")
    expect_equal(nrow(ds_list_allowlist(con, list(admin = TRUE))), 1)
    expect_match(as.character(output$allow_body$html), "Kjetil G., sist innlogget")
    session$setInputs(allow_remove = "ikke-en-hash")            # ignored
    expect_equal(nrow(ds_list_allowlist(con, list(admin = TRUE))), 1)
  }))
})

test_that("the allowlist needs the key", {
  con <- local_test_db()
  withr::local_envvar(SPONDNYMARK_LOGIN_KEY = "")
  suppressMessages(testServer(mod_admin_server, args = admin_args(con), {
    session$setInputs(open = 1, allow_new = "ola@klubb.no", allow_add = 1)
    expect_match(msg()$text, "SPONDNYMARK_LOGIN_KEY")
  }))
})
