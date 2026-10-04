# Fase 0: test av Spond-API-et mot ekte data ---------------------------------
#
# Formål: bekrefte det appen bygger på før vi lager brukergrensesnitt:
#   1. at innlogging virker (og om kontoen krever tofaktor),
#   2. at roller finnes på gruppe og medlem, og at innlogget profil kan
#      kobles til medlemsoppføringen,
#   3. hvordan undergrupper og arrangementenes mottakere ser ut,
#   4. at både kommende og gjennomførte arrangementer kan hentes.
#
# Kjør fra prosjektmappen:  source("dev/spike_spond.R")
# Krever pakkene pkgload, httr2 og askpass (askpass følger med httr2).
#
# Personvern: passordet lagres ikke. Rapporten (dev/spike_report.txt) viser
# bare feltnavn, antall og navn på grupper, undergrupper, roller og
# arrangementer. Den inneholder ikke navn, e-post eller telefon på personer.
# Lim gjerne inn rapporten i chatten etterpå.

pkgload::load_all(".", quiet = TRUE)

report <- character()
say <- function(...) {
  line <- paste0(...)
  cat(line, "\n", sep = "")
  report <<- c(report, line)
}
section <- function(title) say("\n## ", title)
field_names <- function(records) {
  sort(unique(unlist(lapply(records, names))))
}
safely <- function(label, expr) {
  tryCatch(expr, error = function(e) {
    say("FEIL i ", label, ": ", conditionMessage(e))
    NULL
  })
}

# Innlogging ----------------------------------------------------------------
email <- Sys.getenv("SPOND_EMAIL")
if (!nzchar(email)) email <- readline("Spond e-post: ")
password <- askpass::askpass("Spond-passord: ")

section("1. Innlogging")
session <- safely("innlogging", spond_login(email, password))
rm(password)
if (is.null(session)) stop("Stopper: innlogging feilet. Se meldingen over.")
say("OK. Token mottatt", if (!is.null(session$expiration)) paste0(", utløper ", session$expiration), ".")

# Profil --------------------------------------------------------------------
section("2. Profil")
profile <- safely("profil", spond_get_profile(session))
if (!is.null(profile)) {
  say("Felter: ", paste(sort(names(profile)), collapse = ", "))
  say("Har id: ", !is.null(profile$id))
}

# Grupper, roller og undergrupper -------------------------------------------
section("3. Grupper, roller og undergrupper")
groups <- safely("grupper", spond_get_groups(session))
if (!is.null(groups)) {
  say("Antall grupper: ", length(groups))
  say("Felter på gruppe: ", paste(field_names(groups), collapse = ", "))
  all_members <- unlist(lapply(groups, `[[`, "members"), recursive = FALSE)
  say("Felter på medlem: ", paste(field_names(all_members), collapse = ", "))
  guardians <- unlist(lapply(all_members, `[[`, "guardians"), recursive = FALSE)
  if (length(guardians)) say("Felter på foresatt: ", paste(field_names(guardians), collapse = ", "))
  all_roles <- unlist(lapply(groups, `[[`, "roles"), recursive = FALSE)
  if (length(all_roles)) say("Felter på rolle: ", paste(field_names(all_roles), collapse = ", "))
  all_sgs <- unlist(lapply(groups, `[[`, "subGroups"), recursive = FALSE)
  if (length(all_sgs)) say("Felter på undergruppe: ", paste(field_names(all_sgs), collapse = ", "))

  for (g in groups) {
    say("\n### Gruppe: ", g$name %||% "(uten navn)")
    members <- g$members %||% list()
    say("Medlemmer: ", length(members),
        " | med roller: ", sum(vapply(members, function(m) length(m$roles) > 0, logical(1))),
        " | med koblet profil: ", sum(vapply(members, function(m) !is.null(m$profile$id), logical(1))),
        " | med foresatte: ", sum(vapply(members, function(m) length(m$guardians) > 0, logical(1))))
    role_names <- vapply(g$roles %||% list(), function(r) as.character(r$name %||% "?"), character(1))
    say("Roller i gruppen: ", if (length(role_names)) paste(role_names, collapse = ", ") else "(ingen)")
    sgs <- spond_subgroups(g)
    say("Undergrupper: ", if (nrow(sgs)) paste0(sgs$name, " (", sgs$n_members, ")", collapse = ", ") else "(ingen)")

    if (!is.null(profile$id)) {
      me <- spond_member_for_profile(g, profile$id)
      as_guardian <- any(vapply(members, function(m) {
        any(vapply(m$guardians %||% list(), function(gd) identical(gd$profile$id, profile$id), logical(1)))
      }, logical(1)))
      say("Du er medlem her: ", !is.null(me),
          " | dine roller: ", if (!is.null(me) && length(spond_member_role_names(g, me))) paste(spond_member_role_names(g, me), collapse = ", ") else "(ingen)",
          " | du er foresatt her: ", as_guardian)
    }
  }
}

# Tilgang etter regelen i appen ---------------------------------------------
section("4. Tilgang etter regelen i appen")
say("Rollenavn som gir tilgang (golem-config.yml): ", paste(access_role_names(), collapse = ", "))
accessible <- if (!is.null(groups) && !is.null(profile)) spond_accessible_groups(groups, profile) else NULL
if (is.null(accessible) || nrow(accessible) == 0) {
  say("Ingen grupper gir tilgang. Sjekk at rollenavnene over stemmer med rollene i gruppelisten i del 3.")
} else {
  for (i in seq_len(nrow(accessible))) {
    say("Tilgang: ", accessible$name[i], " (rolle: ", accessible$roles[i], ", undergrupper: ", accessible$n_subgroups[i], ")")
  }
}

# Arrangementer ---------------------------------------------------------------
section("5. Arrangementer")
target <- NULL
if (!is.null(accessible) && nrow(accessible) > 0) {
  target <- Filter(function(g) identical(as.character(g$id), accessible$id[1]), groups)[[1]]
} else if (length(groups)) {
  target <- groups[[1]]
  say("(Bruker første gruppe siden ingen ga tilgang.)")
}

describe_event <- function(e) {
  rec <- e$recipients
  rec_sgs <- rec$group$subGroups %||% rec$subGroups
  resp <- e$responses
  say("- ", e$heading %||% "(uten tittel)", " | start ", e$startTimestamp %||% "?",
      " | mottakere-felter: ", paste(names(rec), collapse = ", "),
      if (!is.null(rec$group)) paste0(" | mottakere$group-felter: ", paste(names(rec$group), collapse = ", ")) else "",
      " | undergrupper i mottakere: ", length(rec_sgs),
      " | svar: ", if (length(resp)) paste0(names(resp), "=", vapply(resp, length, integer(1)), collapse = ", ") else "(ingen)")
}

if (!is.null(target)) {
  say("Gruppe: ", target$name)
  if (!is.null(target$myMembership)) say("Felter i myMembership: ", paste(sort(names(target$myMembership)), collapse = ", "))
  upcoming <- safely("kommende arrangementer",
                     spond_get_events(session, group_id = target$id, min_end = Sys.time(), max_events = 20))
  if (!is.null(upcoming)) {
    say("Kommende (maks 20): ", length(upcoming))
    if (length(upcoming)) {
      say("Felter på arrangement: ", paste(field_names(upcoming), collapse = ", "))
      for (e in head(upcoming, 5)) describe_event(e)
    }
  }

  past <- safely("gjennomførte arrangementer",
                 spond_get_events(session, group_id = target$id,
                                  min_end = Sys.time() - 60 * 60 * 24 * 30, max_end = Sys.time(),
                                  max_events = 20))
  if (!is.null(past)) say("Gjennomførte siste 30 dager (maks 20): ", length(past))

  sgs <- spond_subgroups(target)
  for (i in seq_len(nrow(sgs))) {
    sg_events <- safely(paste("arrangementer for", sgs$name[i]),
                        spond_get_events(session, group_id = target$id, subgroup_id = sgs$id[i],
                                         min_end = Sys.time(), max_events = 20))
    if (!is.null(sg_events)) {
      say("Undergruppe ", sgs$name[i], ": ", length(sg_events), " kommende",
          if (length(sg_events)) paste0(" (", paste(head(vapply(sg_events, function(e) e$heading %||% "?", character(1)), 3), collapse = "; "), ")") else "")
    }
  }
}

writeLines(report, "dev/spike_report.txt")
cat("\nRapporten er lagret i dev/spike_report.txt\n")
