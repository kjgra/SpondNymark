test_that("only minimal fields are kept from an event", {
  e <- event_minimal(fake_spond_events()[[1]])
  expect_setequal(names(e), c("id", "heading", "start", "end", "cancelled", "match", "subgroup_ids",
                              "invite_time", "not_sent", "responses"))
  expect_false(e$not_sent)                    # sent: Spond gives no inviteTime
  expect_equal(e$subgroup_ids, "S-ulv")
  expect_equal(names(e$responses), c("member_id", "status"))
  flat <- paste(unlist(e), collapse = " ")
  for (secret in c("Skadet", "12345678", "2016-03-04", "Banen", "drikkeflaske", "P-1")) {
    expect_false(grepl(secret, flat), info = secret)
  }
})

test_that("responses are mapped to statuses", {
  e <- event_minimal(fake_spond_events()[[1]])
  expect_equal(e$responses$status[match(c("M-1", "M-3", "M-gone", "M-2"), e$responses$member_id)],
               c("accepted", "accepted", "declined", "unanswered"))
  empty <- event_minimal(list(id = "E", heading = "X"))
  expect_equal(nrow(empty$responses), 0)
  expect_equal(empty$subgroup_ids, character())
})

test_that("times are parsed as UTC, with or without milliseconds", {
  e <- event_minimal(fake_spond_events()[[1]])
  expect_equal(format(e$start, "%H:%M", tz = "UTC"), "08:00")
  expect_equal(format(e$end, "%H:%M", tz = "UTC"), "09:30")
  expect_true(is.na(spond_parse_time(NULL)))
})

test_that("subgroup ids are read from objects or plain ids", {
  ev <- events_for_context(fake_spond_events())
  ids <- vapply(ev, `[[`, "", "id")
  expect_equal(ids, c("E-kamp", "E-ulv", "E-gaupe"))     # sorted by start
  expect_equal(ev[[3]]$subgroup_ids, "S-gaupe")
  expect_true(ev[[3]]$cancelled)
  expect_true(ev[[1]]$match)
})

test_that("a subgroup sees only its own events; past events newest first", {
  ev <- events_for_context(fake_spond_events(), subgroup_id = "S-ulv")
  expect_equal(vapply(ev, `[[`, "", "id"), "E-ulv")
  past <- events_for_context(fake_spond_events(), decreasing = TRUE)
  expect_equal(vapply(past, `[[`, "", "id"), c("E-gaupe", "E-ulv", "E-kamp"))
  expect_equal(events_for_context(NULL), list())
})

test_that("events are labelled with the subgroups they were sent to", {
  g <- fake_group()
  ev <- events_for_context(fake_spond_events())
  expect_equal(event_sent_to(ev[[2]], g), "Nymark Ulv G10")
  expect_equal(event_sent_to(ev[[1]], g), "Nymark G/J 2016")  # sent to the main group
})

test_that("participants get display names and statuses, in display order", {
  g <- fake_group()
  e <- event_minimal(fake_spond_events()[[1]])
  p <- event_participants(e, g)
  expect_equal(p$display_name, c("Emma Haugen", "Noah S.", "Emma Hansen", "Tidligere medlem"))
  expect_equal(p$label, c("Kommer", "Kommer", "Ikke svart", "Kommer ikke"))
  expect_equal(event_counts(e), c(Kommer = 2L, `Ikke svart` = 1L, `Kommer ikke` = 1L))
})

test_that("times are shown in Norwegian, in Norwegian time", {
  now <- fake_now()
  t <- function(x) as.POSIXct(x, tz = "UTC")
  # Summer time in October: 08:00 UTC is 10:00 in Oslo
  expect_equal(event_when(t("2026-10-10 08:00"), t("2026-10-10 09:30"), now = now), "lør. 10. okt. kl. 10:00–11:30")
  # Winter time
  expect_equal(event_when(t("2026-12-01 17:00"), NA, now = now), "tir. 1. des. kl. 18:00")
  # Over several days, and another year
  expect_equal(event_when(t("2027-01-29 16:00"), t("2027-01-31 12:00"), now = now),
               "fre. 29. jan. 2027 kl. 17:00–søn. 31. jan. 2027 kl. 13:00")
  expect_equal(event_when(NA), "Tid ikke satt")
})
