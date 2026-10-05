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
    expect_match(html, "Gruppeutkast")
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
    expect_equal(api$calls()[[1]]$subgroup_id, "S-ulv")
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
    q <- api$calls()[[2]]
    expect_equal(q$max_end, fake_now())
    expect_equal(q$min_end, fake_now() - 30 * 86400)
    session$setInputs(older = "x")
    expect_match(view_html(output), "Viser siste 60 dager")
    expect_equal(api$calls()[[3]]$min_end, fake_now() - 60 * 86400)
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
    expect_equal(api$calls()[[length(api$calls())]]$subgroup_id, "S-gaupe")
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
    expect_equal(length(api$calls()), 2)
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
