# Fase 0: test av Spond-API-et mot ekte data ---------------------------------
#
# Formål: bekrefte det appen bygger på før vi lager brukergrensesnitt:
#   1. at innlogging virker (og om kontoen krever tofaktor),
#   2. at roller finnes på gruppe og medlem, og at innlogget profil kan
#      kobles til medlemsoppføringen,
#   3. hvordan undergrupper og arrangementenes mottakere ser ut,
#   4. at både kommende og gjennomførte arrangementer kan hentes,
#   6. (runde 1, punkt B) hvilke roller som finnes, og om trenere kan kobles
#      til barna sine via foresatte (profil-ID). Bare antall, ingen navn.
#   7. (runde 1, punkt C) hvordan arrangementer som ikke er sendt ut ennå
#      (planlagt utsending) ser ut. Bare feltnavn, tidspunkter og antall.
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

# Trenere og foresatte (runde 1, punkt B) -----------------------------------------
# Bare gruppene du har tilgang til. Rapporten viser rollenavn og antall, ingen
# navn eller ID-er på personer. En trener er forelder til et barn når trenerens
# profil-ID står blant barnets foresatte.
section("6. Trenere og foresatte")
coach_candidates <- c("Teamleder", "Trener", "Lagleder", "Hovedlagleder", "Hjelpetrener", "Keepertrener", "Hjelper")
say("Kandidater for trenerroller: ", paste(coach_candidates, collapse = ", "))
led <- if (!is.null(accessible) && nrow(accessible) > 0) {
  Filter(function(g) as.character(g$id) %in% accessible$id, groups)
} else list()
if (length(led) == 0) say("Ingen grupper å se på.")

for (g in led) {
  say("\n### ", g$name %||% "(uten navn)")
  members <- g$members %||% list()
  roles <- g$roles %||% list()
  role_ids <- vapply(roles, function(r) as.character(r$id %||% NA), character(1))
  role_nms <- vapply(roles, function(r) as.character(r$name %||% "?"), character(1))
  member_roles <- lapply(members, function(m) role_nms[role_ids %in% as.character(unlist(m$roles))])
  member_profile <- vapply(members, function(m) as.character(m$profile$id %||% NA), character(1))
  guardian_profiles <- lapply(members, function(m) {
    p <- vapply(m$guardians %||% list(), function(gd) as.character(gd$profile$id %||% NA), character(1))
    unique(p[!is.na(p)])
  })
  # For each member: how many members in the group list this member's profile as guardian.
  n_children <- vapply(member_profile, function(p) {
    if (is.na(p)) return(0L)
    sum(vapply(guardian_profiles, function(x) p %in% x, logical(1)))
  }, integer(1))

  all_guardians <- unlist(lapply(members, function(m) m$guardians %||% list()), recursive = FALSE)
  gp <- vapply(all_guardians, function(gd) as.character(gd$profile$id %||% NA), character(1))
  say("Foresatt-oppføringer: ", length(all_guardians), " | med profil: ", sum(!is.na(gp)),
      " | unike foresatte (profil): ", length(unique(gp[!is.na(gp)])),
      " | foresatte som også er medlem her: ", sum(unique(gp[!is.na(gp)]) %in% member_profile))

  say("Roller: antall medlemmer | med profil | forelder til minst ett medlem her | rettigheter")
  for (i in seq_along(roles)) {
    has <- vapply(member_roles, function(x) role_nms[i] %in% x, logical(1))
    perms <- sort(unique(as.character(unlist(roles[[i]]$permissions))))
    say("- ", role_nms[i], ": ", sum(has), " | ", sum(has & !is.na(member_profile)), " | ",
        sum(has & n_children > 0), " | ", if (length(perms)) paste(perms, collapse = ", ") else "(ingen)")
  }
  n_multi <- sum(vapply(member_roles, length, integer(1)) > 1)
  say("Medlemmer med flere roller: ", n_multi)

  is_coach <- vapply(member_roles, function(x) any(normalise_role_name(x) %in% normalise_role_name(coach_candidates)),
                     logical(1))
  say("Trenere etter kandidatlisten: ", sum(is_coach),
      " | med profil: ", sum(is_coach & !is.na(member_profile)),
      " | forelder til minst ett medlem her: ", sum(is_coach & n_children > 0))
  kids <- table(n_children[is_coach & n_children > 0])
  if (length(kids)) say("Barn per trener-forelder: ", paste0(names(kids), " barn: ", as.integer(kids), " trenere", collapse = ", "))
  parents_no_role <- sum(!is_coach & n_children > 0)
  say("Medlemmer uten trenerrolle som er forelder til et medlem her: ", parents_no_role)

  # Do coach-parents share a subgroup with their child?
  sgs <- lapply(members, function(m) as.character(unlist(m$subGroups)))
  links <- 0L; same_sg <- 0L
  for (ci in which(is_coach & n_children > 0)) {
    for (ki in which(vapply(guardian_profiles, function(x) member_profile[ci] %in% x, logical(1)))) {
      links <- links + 1L
      if (length(intersect(sgs[[ci]], sgs[[ki]]))) same_sg <- same_sg + 1L
    }
  }
  if (links > 0) say("Trener-barn-koblinger: ", links, " | i minst én felles undergruppe: ", same_sg)

  sg_tab <- spond_subgroups(g)
  if (nrow(sg_tab)) {
    say("Undergrupper: medlemmer | trenere (kandidatlisten) | med en eller annen rolle")
    for (j in seq_len(nrow(sg_tab))) {
      inn <- vapply(sgs, function(x) sg_tab$id[j] %in% x, logical(1))
      say("- ", sg_tab$name[j], ": ", sum(inn), " | ", sum(inn & is_coach), " | ",
          sum(inn & vapply(member_roles, length, integer(1)) > 0))
    }
  }
}

# Arrangementer som ikke er sendt ut (runde 1, punkt C) ------------------------------
# Henter kommende arrangementer med og uten scheduled=true og ser på dem som
# bare kommer med når scheduled=true. Viser feltnavn, felt som har med
# utsending å gjøre, og antall svar. Ingen navn på personer.
section("7. Arrangementer som ikke er sendt ut")
if (!is.null(target)) {
  normal <- safely("kommende uten scheduled",
                   spond_get_events(session, group_id = target$id, min_end = Sys.time(), max_events = 100))
  with_sched <- safely("kommende med scheduled",
                       spond_get_events(session, group_id = target$id, min_end = Sys.time(),
                                        include_scheduled = TRUE, max_events = 100))
  if (!is.null(normal) && !is.null(with_sched)) {
    ids_normal <- vapply(normal, function(e) as.character(e$id), character(1))
    only_sched <- Filter(function(e) !as.character(e$id) %in% ids_normal, with_sched)
    say("Kommende uten scheduled: ", length(normal), " | med scheduled: ", length(with_sched),
        " | bare med scheduled (ikke sendt ut): ", length(only_sched))
    fields_normal <- field_names(normal)
    if (length(only_sched)) {
      fs <- field_names(only_sched)
      say("Felter bare på ikke-utsendte: ", paste(setdiff(fs, fields_normal), collapse = ", "))
      say("Felter bare på utsendte: ", paste(setdiff(fields_normal, fs), collapse = ", "))
    }
    show_flags <- function(e) {
      keys <- grep("invite|sched|draft|publish|sent|send|remind|visib|hidden|open", names(e), value = TRUE, ignore.case = TRUE)
      vals <- vapply(keys, function(k) {
        v <- e[[k]]
        if (is.null(v)) "NULL" else if (is.atomic(v) && length(v) == 1) as.character(v)
        else paste0("<", class(v)[1], " ", length(v), ">")
      }, character(1))
      if (length(keys)) paste0(keys, "=", vals, collapse = ", ") else "(ingen)"
    }
    resp_counts <- function(e) {
      r <- e$responses
      if (length(r)) paste0(names(r), "=", vapply(r, length, integer(1)), collapse = ", ") else "(ingen)"
    }
    say("\nIkke sendt ut (maks 5):")
    for (e in head(only_sched, 5)) {
      say("- ", e$heading %||% "?", " | start ", e$startTimestamp %||% "?",
          " | ", show_flags(e), " | svar: ", resp_counts(e),
          " | mottakere-felter: ", paste(names(e$recipients), collapse = ", "))
    }
    say("\nSendt ut (maks 3, til sammenligning):")
    for (e in head(normal, 3)) {
      say("- ", e$heading %||% "?", " | start ", e$startTimestamp %||% "?",
          " | ", show_flags(e), " | svar: ", resp_counts(e))
    }
  }
}

writeLines(report, "dev/spike_report.txt")
cat("\nRapporten er lagret i dev/spike_report.txt\n")
