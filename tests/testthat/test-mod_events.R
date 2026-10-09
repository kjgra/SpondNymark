events_args <- function(api, ctx) {
  list(context = ctx, user = reactive(fake_user(fake_spond_groups_two())), spond = api, now = fake_now)
}
view_html <- function(output) as.character(output$view$html)

test_that("the main group shows all upcoming events, marked with subgroups", {
  api <- fake_events_api()
  ctx <- reactiveVal(teams_context(fake_group()))
  testServer(mod_events_server, args = events_args(api, ctx), {
    html <- view_html(output)
    expect_match(html, "Trening Ulv")
    expect_match(html, "Kamp mot Fana")
    expect_match(html, "sn-tag-to\">Nymark Ulv G10")
    expect_match(html, "Avlyst")
    expect_match(html, "2 kommer · 1 ikke svart")
    expect_match(html, "Medlemmer i Nymark G/J 2016 · 4")
    expect_false(grepl("Gruppeutkast", html))   # needs the database (see test-mod_groups.R)
    q <- api$calls()[[1]]
    expect_equal(q$group_id, "G2016")
    expect_null(q$subgroup_id)
    expect_equal(q$min_end, fake_now())
    expect_null(q$max_end)
  })
})

test_that("a subgroup asks Spond for its own events and shows only those", {
  api <- fake_events_api()
  ctx <- reactiveVal(teams_context(fake_group(), "S-ulv"))
  testServer(mod_events_server, args = events_args(api, ctx), {
    html <- view_html(output)
    expect_match(html, "Trening Ulv")
    expect_false(grepl("Kamp mot Fana", html))
    expect_false(grepl("sn-tag-to", html))   # no subgroup labels inside a subgroup
    expect_null(api$calls()[[1]]$subgroup_id)   # upcoming: whole group, filtered here
  })
})

test_that("past events use a window that can be widened", {
  api <- fake_events_api()
  ctx <- reactiveVal(teams_context(fake_group()))
  testServer(mod_events_server, args = c(events_args(api, ctx), past_days = 30), {
    view_html(output)
    session$setInputs(tab = "past")
    html <- view_html(output)
    expect_match(html, "Viser siste 30 dager")
    q <- api$calls()[[3]]                   # 1-2: upcoming (sent, and not sent yet)
    expect_equal(q$max_end, fake_now())
    expect_equal(q$min_end, fake_now() - 30 * 86400)
    session$setInputs(older = "x")
    expect_match(view_html(output), "Viser siste 60 dager")
    expect_equal(api$calls()[[4]]$min_end, fake_now() - 60 * 86400)
    # Back to upcoming uses the cached result
    n <- length(api$calls())
    session$setInputs(tab = "upcoming")
    view_html(output)
    expect_equal(length(api$calls()), n)
  })
})

test_that("an event opens with its participants, and back returns to the list", {
  api <- fake_events_api()
  ctx <- reactiveVal(teams_context(fake_group()))
  testServer(mod_events_server, args = events_args(api, ctx), {
    view_html(output)
    session$setInputs(open = "E-ulv")
    html <- view_html(output)
    expect_match(html, "← Arrangementer")
    expect_match(html, "lør. 10. okt. kl. 10:00–11:30")
    expect_match(html, "Sendt til:")
    session$setInputs(open = "finnes-ikke")   # unknown ids are ignored
    expect_match(view_html(output), "Trening Ulv")
    session$setInputs(back = "x")
    expect_match(view_html(output), "Kommende")
  })
})

test_that("changing context closes the event and starts on upcoming", {
  api <- fake_events_api()
  ctx <- reactiveVal(teams_context(fake_group()))
  testServer(mod_events_server, args = events_args(api, ctx), {
    view_html(output)
    session$setInputs(tab = "past")
    session$setInputs(open = "E-ulv")
    ctx(teams_context(fake_group(), "S-gaupe"))
    session$flushReact()
    html <- view_html(output)
    expect_false(grepl("Sendt til:", html))
    expect_match(html, "aria-pressed=\"true\"[^>]*>Kommende|Kommende</button>")
    expect_match(html, "Trening Gaupe")
    expect_false(grepl("Trening Ulv", html))
  })
})

test_that("Spond errors are shown, other errors get a general message", {
  ctx <- reactiveVal(teams_context(fake_group()))
  api <- fake_events_api(fail_with = structure(class = c("spond_error", "error", "condition"),
                                               list(message = "Spond-sesjonen er utløpt.", call = NULL)))
  testServer(mod_events_server, args = events_args(api, ctx), {
    expect_match(view_html(output), "Spond-sesjonen er utløpt")
  })
  api2 <- fake_events_api(fail_with = "connection reset by peer")
  expect_message(
    testServer(mod_events_server, args = events_args(api2, ctx), {
      html <- view_html(output)
      expect_match(html, "Kunne ikke hente arrangementer")
      expect_false(grepl("connection reset", html))
    }),
    "connection reset"
  )
})

test_that("refresh fetches again", {
  api <- fake_events_api()
  ctx <- reactiveVal(teams_context(fake_group()))
  testServer(mod_events_server, args = events_args(api, ctx), {
    view_html(output)
    session$setInputs(refresh = "x")
    view_html(output)
    expect_equal(length(api$calls()), 4)      # two calls per fetch of upcoming events
  })
})

test_that("no context, no view", {
  testServer(mod_events_server, args = events_args(fake_events_api(), reactive(NULL)), {
    expect_error(output$view)
  })
})

test_that("ui is a tag list", {
  expect_s3_class(mod_events_ui("x"), "shiny.tag.list")
})

test_that("members and participants are shown as chips with tags", {
  api <- fake_events_api()
  ctx <- reactiveVal(teams_context(fake_group()))
  tagger <- list(chip = function(id, n, ...) member_chip(id, n, if (id == "M-1") "Keeper" else character(), "tags-open", ...),
                 error = function() NULL)
  testServer(mod_events_server, args = c(events_args(api, ctx), tagger = list(tagger)), {
    view_html(output)
    members <- as.character(output$members$html)
    expect_match(members, "Trykk på et navn")
    expect_match(members, "data-sn-value=\"M-1\"")
    expect_match(members, "sn-minitag\">Keeper")
    session$setInputs(open = "E-ulv")
    parts <- as.character(output$`participants-list`$html)
    expect_match(parts, "data-sn-value=\"M-1\"")
    expect_match(parts, "sn-minitag\">Keeper")
    # Former members are not clickable
    expect_false(grepl("data-sn-value=\"M-gone\"", parts))
    expect_match(parts, "Tidligere medlem")
  })
})

test_that("the members section stays open when the view is redrawn", {
  api <- fake_events_api()
  ctx <- reactiveVal(teams_context(fake_group()))
  testServer(mod_events_server, args = events_args(api, ctx), {
    expect_false(grepl("<details open data-sn-toggle=\"proxy1-members_open\"", view_html(output)))
    session$setInputs(members_open = TRUE)
    session$setInputs(tab = "past")
    expect_match(view_html(output), "<details class=\"sn-fold\" open data-sn-toggle=\"proxy1-members_open\"")
  })
})

test_that("an expired Spond session sends the trainer to the login page", {
  expired <- structure(class = c("spond_expired", "spond_error", "error", "condition"),
                       list(message = "Spond-sesjonen er utløpt. Logg inn på nytt.", call = NULL))
  called <- 0
  ctx <- reactiveVal(teams_context(fake_group()))
  testServer(mod_events_server, args = events_args(fake_events_api(fail_with = expired), ctx), {
    session$userData$sn_session_expired <- function() called <<- called + 1
    session$setInputs(refresh = "x")
    expect_gte(called, 1)
  })
})

test_that("the date block shows weekday, day and month, and marks matches and cancellations", {
  s <- as.POSIXct("2026-10-10 10:00", tz = "Europe/Oslo")
  html <- as.character(event_date_block(s))
  expect_match(html, "sn-event-wday\">lør<")
  expect_match(html, "sn-event-day\">10<")
  expect_match(html, "sn-event-month\">okt<")
  expect_match(html, "aria-hidden=\"true\"")
  expect_match(as.character(event_date_block(as.POSIXct(NA))), "sn-event-day\">\\?<")
  expect_equal(event_state_class(list(match = TRUE, cancelled = FALSE)), "sn-event-match")
  expect_equal(event_state_class(list(match = TRUE, cancelled = TRUE)), "sn-event-match sn-event-cancelled")
  expect_equal(event_state_class(list(match = FALSE, cancelled = FALSE)), "")
})

test_that("the tabs are only upcoming and past, and the approvals button shows the count", {
  ns <- NS("ev")
  tabs <- as.character(tab_switch(ns, "past"))
  expect_match(tabs, "Kommende")
  expect_match(tabs, "sn-seg-btn is-on\" aria-pressed=\"true\"[^>]*>Gjennomførte")
  expect_false(grepl("Godkjenning", tabs))
  expect_false(grepl("is-on", as.character(tab_switch(ns, "approvals"))))

  b <- as.character(approvals_button(ns, 3))
  expect_match(b, "aria-label=\"Godkjenning, 3 venter\"")
  expect_match(b, "sn-approve-count\" aria-hidden=\"true\">3<")
  # htmltools writes the quotes in onclick as &#39;
  expect_match(b, "ev-tab(&#39;|'), (&#39;|')approvals")
  b0 <- as.character(approvals_button(ns, 0, active = TRUE))
  expect_false(grepl("sn-approve-count", b0))
  expect_match(b0, "is-active")
})

test_that("without the database there is no approvals button and the tab is ignored", {
  api <- fake_events_api()
  ctx <- reactiveVal(teams_context(fake_group()))
  testServer(mod_events_server, args = events_args(api, ctx), {
    expect_false(grepl("sn-approve", paste(unlist(output$bar), collapse = "")))
    session$setInputs(tab = "approvals")
    expect_match(view_html(output), "Ingen kommende|Trening Ulv")
    expect_false(grepl("Til godkjenning", view_html(output)))
  })
})

test_that("with the database the top bar button shows the count and opens the approvals list", {
  con <- local_test_db()
  acc <- fake_user(fake_spond_groups_two())$access
  id <- ds_save_proposal(con, acc, "P-me", "G2016", "Rød/blå", c("Rød", "Blå"), event_id = "E-ulv")
  ds_transition(con, acc, id, "submit", "P-me")
  api <- fake_events_api()
  ctx <- reactiveVal(teams_context(fake_group()))
  testServer(mod_events_server, args = c(events_args(api, ctx), db = list(db_handle(function() con))), {
    session$flushReact()
    expect_match(paste(unlist(output$bar), collapse = ""), "Godkjenning, 1 venter")
    session$setInputs(open = "E-ulv")
    expect_false(is.null(selected()))
    session$setInputs(tab = "approvals")            # from an open event: back to the list
    expect_null(selected())
    expect_match(view_html(output), "Til godkjenning")
    expect_match(paste(unlist(output$bar), collapse = ""), "is-active")
  })
})

test_that("members are listed as trainers, players and (when any) other adults", {
  m <- data.frame(id = c("M-c", "M-p1", "M-p2"), display_name = c("Kari T.", "Ida S.", "Jon B."),
                  kind = c("coach", "player", "player"), roles = c("Teamleder", "", ""))
  html <- as.character(member_sections(m))
  expect_match(html, "Trenere · 1")
  expect_match(html, "Spillere · 2")
  expect_match(html, "sn-coach-role\">Teamleder")
  expect_false(grepl("Andre voksne", html))
  m2 <- rbind(m, data.frame(id = "M-a", display_name = "Anne V.", kind = "adult", roles = ""))
  expect_match(as.character(member_sections(m2)), "Andre voksne · 1")
})

test_that("only players get clickable chips", {
  tagger <- list(chip = function(id, name, note = NULL) tags$button(`data-sn-value` = id, name))
  html <- as.character(member_chips(c("M-c", "M-p", "M-a"), c("Kari", "Ida", "Anne"), tagger,
                                    kinds = c("coach", "player", "adult")))
  expect_match(html, "sn-chip sn-chip-coach\">Kari")
  expect_match(html, "data-sn-value=\"M-p\"")
  expect_false(grepl("data-sn-value=\"M-a\"", html))
  expect_false(grepl("data-sn-value=\"M-c\"", html))
})

test_that("trainers are not placed in event groups, but shown on the event page", {
  g <- fake_group()                                   # M-me is Teamleder
  ev <- event_minimal(fake_spond_events()[[1]])       # M-1 and M-3 are coming
  ev$responses <- rbind(ev$responses, data.frame(member_id = "M-me", status = "accepted"))
  d <- list(event = ev, labels = "A", assignments = character())
  m <- draft_members(d, g, members_in_context(g))
  expect_false("M-me" %in% m$member_id)
  expect_true(all(c("M-1", "M-3") %in% m$member_id))
  # A trainer placed before this change can still be moved out
  d$assignments <- c(`M-me` = "A")
  expect_true("M-me" %in% draft_members(d, g, members_in_context(g))$member_id)

  line <- as.character(event_coaches_line(ev, g))
  expect_match(line, "Trenere:")
  expect_match(line, "Kjetil G.")
  expect_match(line, "fordeles ikke")
  expect_null(event_coaches_line(event_minimal(fake_spond_events()[[1]]), g))
  ev$responses$status[ev$responses$member_id == "M-me"] <- "declined"
  expect_null(event_coaches_line(ev, g))
})

test_that("events not sent yet are marked, and only the next five are shown until 'Vis flere'", {
  plan <- function(k) {
    list(id = paste0("E-plan", k), heading = paste0("Plantrening ", k),
         startTimestamp = sprintf("2026-10-%02dT16:00:00Z", 10 + k), endTimestamp = sprintf("2026-10-%02dT17:30:00Z", 10 + k),
         inviteTime = sprintf("2026-10-%02dT16:00:00Z", 7 + k),
         recipients = list(group = list(id = "G2016", subGroups = list(list(id = "S-ulv")))),
         responses = list(unansweredIds = list("M-1", "M-3", "M-me")))
  }
  calls <- list()
  api <- fake_spond_api()
  api$events <- function(sess, ...) {
    q <- list(...)
    calls[[length(calls) + 1]] <<- q
    if (isTRUE(q$include_scheduled)) c(fake_spond_events(), lapply(1:7, plan)) else fake_spond_events()
  }
  ctx <- reactiveVal(teams_context(fake_group()))
  testServer(mod_events_server, args = list(context = ctx, user = reactive(fake_user(fake_spond_groups_two())),
                                            spond = api, now = fake_now), {
    html <- view_html(output)
    expect_match(html, "sn-event-unsent")
    expect_match(html, "Ikke sendt ut · sendes tor. 8. okt.")      # Plantrening 1
    expect_match(html, "3 får invitasjonen")
    expect_match(html, "Plantrening 5")
    expect_false(grepl("Plantrening 6", html))
    expect_match(html, "2 flere som ikke er sendt ut")
    expect_equal(sum(gregexpr("Trening Ulv", html)[[1]] > 0), 1)   # no duplicates from the two calls
    expect_true(isTRUE(calls[[2]]$include_scheduled))
    expect_equal(calls[[2]]$max_start, fake_now() + 183 * 86400)
    n <- length(calls)
    session$setInputs(later = "x")
    html <- view_html(output)
    expect_match(html, "Plantrening 7")
    expect_false(grepl("flere som ikke er sendt ut", html))
    expect_equal(length(calls), n)                                   # no new fetch
  })
})

test_that("before the invitation is sent, every invited player can be placed", {
  g <- fake_group()
  ev <- event_minimal(list(id = "E-plan", heading = "Trening", inviteTime = "2026-10-09T16:00:00Z",
                           responses = list(unansweredIds = list("M-1", "M-3", "M-me"))))
  expect_true(ev$not_sent)
  m <- draft_members(list(event = ev, labels = "A", assignments = character()), g, members_in_context(g))
  expect_setequal(m$member_id, c("M-1", "M-3"))            # not the trainer
  sent <- ev; sent$not_sent <- FALSE
  expect_equal(nrow(draft_members(list(event = sent, labels = "A", assignments = character()), g,
                                  members_in_context(g))), 0)
})

test_that("response icons: tick, question mark, cross, and nothing for unknown", {
  expect_match(as.character(status_icon("accepted")), "sn-st sn-st-accepted.*aria-label=\"Kommer\"")
  expect_match(as.character(status_icon("unanswered")), ">\\?<")
  expect_match(as.character(status_icon("declined")), "Kommer ikke")
  expect_null(status_icon(NA))
  expect_null(status_icon(NULL))
  expect_null(status_icon("noe annet"))
  chip <- as.character(member_chip("M-1", "Emma H.", status = "declined"))
  expect_match(chip, "sn-st-declined")
})
