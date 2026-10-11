# Fake Spond data shaped like the real API (field names from the spike run).
# Includes the kind of personal data Spond sends, so tests can check it is dropped.
fake_spond_groups <- function() {
  list(
    list(
      id = "G2016", name = "Nymark G/J 2016", clubName = "Nymark IL",
      roles = list(list(id = "R1", name = "Teamleder", permissions = list("events")),
                   list(id = "R2", name = "Foresatt", permissions = list())),
      subGroups = list(list(id = "S-ulv", name = "Nymark Ulv G10", color = "#f00"),
                       list(id = "S-gaupe", name = "Nymark Gaupe G10", color = "#0f0")),
      members = list(
        list(id = "M-me", firstName = "Kjetil", lastName = "Gramstad", roles = list("R1"),
             subGroups = list(), profile = list(id = "P-me"),
             email = "k@example.no", phoneNumber = "99999999", dateOfBirth = "1980-01-01"),
        list(id = "M-1", firstName = "Emma", lastName = "Haugen", roles = list(),
             subGroups = list("S-ulv"), dateOfBirth = "2016-03-04", address = "Gata 1",
             nationality = "NO", guardians = list(list(id = "GD1", firstName = "Mor", phoneNumber = "12345678")))
      )
    ),
    list(
      id = "G-parents", name = "Foreldre i 5B",
      roles = list(list(id = "R9", name = "Gruppestyrer")),
      members = list(list(id = "M-x", firstName = "Kjetil", profile = list(id = "P-me"), roles = list()))
    )
  )
}

fake_spond_profile <- function(id = "P-me") {
  list(id = id, firstName = "Kjetil", lastName = "Gramstad", primaryEmail = "k@example.no",
       dateOfBirth = "1980-01-01", phoneNumber = "99999999")
}

# A fake replacement for spond_api(): accepts one e-mail/password pair.
fake_spond_api <- function(profile_id = "P-me", fail_with = NULL) {
  list(
    login = function(email, password) {
      if (!is.null(fail_with)) stop(fail_with)
      if (!identical(email, "trener@klubb.no") || !identical(password, "riktig")) {
        spond_stop("Innlogging mot Spond feilet: feil e-post, mobilnummer eller passord.")
      }
      structure(list(token = "tok", base_url = "x"), class = "spond_session")
    },
    profile = function(sess) fake_spond_profile(profile_id),
    groups = function(sess) fake_spond_groups()
  )
}

# Two groups where P-me is Teamleder: one with subgroups (and a name collision),
# one without subgroups.
fake_spond_groups_two <- function() {
  g <- fake_spond_groups()
  g[[1]]$members <- c(g[[1]]$members, list(
    list(id = "M-2", firstName = "Emma", lastName = "Hansen", subGroups = list("S-gaupe")),
    list(id = "M-3", firstName = "Noah", lastName = "Sand", subGroups = list("S-ulv", "S-gaupe"))
  ))
  c(g, list(list(
    id = "G-senior", name = "Nymark Senior",
    roles = list(list(id = "R1", name = "Teamleder")),
    members = list(
      list(id = "M-me2", firstName = "Kjetil", lastName = "Gramstad", roles = list("R1"), profile = list(id = "P-me")),
      list(id = "M-9", firstName = "Ola", lastName = "Nordmann")
    )
  )))
}

# The user list mod_login_server() returns, built from fake data.
fake_user <- function(groups = fake_spond_groups(), roles = c("Teamleder")) {
  d <- spond_session_data(groups, fake_spond_profile(), roles)
  list(spond = structure(list(token = "tok"), class = "spond_session"),
       profile = d$profile, groups = d$groups, access = d$access)
}

# Events shaped like Spond's `sponds/` response, with the personal data Spond
# sends (profiles, guardians, decline messages) so tests can check it is dropped.
fake_spond_events <- function() {
  list(
    list(
      id = "E-ulv", heading = "Trening Ulv", type = "EVENT",
      startTimestamp = "2026-10-10T08:00:00Z", endTimestamp = "2026-10-10T09:30:00.000Z",
      description = "Ta med drikkeflaske", location = list(address = "Banen 1"),
      recipients = list(
        group = list(id = "G2016", name = "Nymark G/J 2016",
                     subGroups = list(list(id = "S-ulv", name = "Nymark Ulv G10")),
                     members = list(list(id = "M-1", firstName = "Emma", dateOfBirth = "2016-03-04"))),
        profiles = list(list(id = "P-1", firstName = "Emma")),
        guardians = list(list(id = "GD1", phoneNumber = "12345678"))
      ),
      responses = list(acceptedIds = list("M-1", "M-3"), declinedIds = list("M-gone"),
                       unansweredIds = list("M-2"), waitinglistIds = list(), unconfirmedIds = list(),
                       declineMessages = list(`M-gone` = "Skadet kneet"))
    ),
    list(
      id = "E-kamp", heading = "Kamp mot Fana", matchEvent = TRUE,
      startTimestamp = "2026-10-08T16:00:00Z", endTimestamp = "2026-10-08T17:00:00Z",
      recipients = list(group = list(id = "G2016")),
      responses = list(acceptedIds = list("M-2"), unansweredIds = list("M-1", "M-3"))
    ),
    list(
      id = "E-gaupe", heading = "Trening Gaupe", cancelled = TRUE,
      startTimestamp = "2026-10-12T15:00:00Z", endTimestamp = "2026-10-12T16:00:00Z",
      recipients = list(group = list(id = "G2016", subGroups = list("S-gaupe"))),
      responses = list(acceptedIds = list("M-2"))
    )
  )
}

# A Spond API stand-in that records the event queries it receives.
fake_events_api <- function(events = fake_spond_events(), fail_with = NULL) {
  calls <- list()
  api <- fake_spond_api()
  api$events <- function(sess, ...) {
    calls[[length(calls) + 1]] <<- list(...)
    if (!is.null(fail_with)) stop(fail_with)
    events
  }
  api$calls <- function() calls
  api
}

fake_now <- function() as.POSIXct("2026-10-05 12:00:00", tz = "UTC")

# The main group with subgroups and the name collision (Emma H.).
fake_group <- function() fake_user(fake_spond_groups_two())$groups[["G2016"]]
