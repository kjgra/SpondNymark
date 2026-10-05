test_that("participants are listed by status with counts", {
  g <- fake_group()
  e <- event_minimal(fake_spond_events()[[1]])
  testServer(mod_participants_server,
             args = list(event = reactive(e), group = reactive(g)), {
    html <- as.character(output$list$html)
    expect_match(html, "Kommer \\(2\\)")
    expect_match(html, "Ikke svart \\(1\\)")
    expect_match(html, "Kommer ikke \\(1\\)")
    expect_false(grepl("Venteliste", html))            # statuses without anyone are left out
    expect_match(html, "Noah S.")
    expect_match(html, "Tidligere medlem")
    # "Kommer" is open, "Kommer ikke" folded
    expect_match(html, '<details class="sn-status sn-status-accepted" open>')
    expect_match(html, '<details class="sn-status sn-status-declined">')
  })
})

test_that("an event without recipients says so", {
  e <- event_minimal(list(id = "E", heading = "Tomt"))
  testServer(mod_participants_server,
             args = list(event = reactive(e), group = reactive(fake_group())), {
    expect_match(as.character(output$list$html), "Ingen mottakere")
  })
})

test_that("ui is a tag list", {
  expect_s3_class(mod_participants_ui("x"), "shiny.tag.list")
})
