#' Group proposals: pure helpers for the list and the editor
#'
#' A proposal is either tied to one event (groups for that event) or a
#' gruppeutkast (a sketch of new subgroups, tied to the context it was made
#' in). The editor works on a "draft": a plain list that is only written to
#' the database when the trainer saves.
#'
#' @name fct_groups
#' @noRd
NULL

#' Display names of trainers by Spond profile id (for "Laget av", locks)
#' @noRd
profile_names <- function(group) {
  m <- members_in_context(group, NULL)
  m <- m[!is.na(m$profile_id) & nzchar(m$profile_id), , drop = FALSE]
  stats::setNames(m$display_name, m$profile_id)
}

#' Name for a profile id, with a fallback
#' @noRd
profile_name <- function(names, profile_id) {
  n <- unname(names[profile_id])
  if (length(n) == 0 || is.na(n)) "en annen trener" else n
}

#' Proposals with their groups, ready to show
#'
#' @param headers Result of `ds_list_proposals()`.
#' @param groups Result of `ds_proposal_groups()`.
#' @return A list of proposals: id, event_id, subgroup_id, name, status,
#'   created_by, updated_at, labels (character, in order) and assignments
#'   (named character: member id -> label).
#' @noRd
proposals_view <- function(headers, groups, threads = NULL) {
  if (is.null(headers) || nrow(headers) == 0) return(list())
  if (is.null(threads)) threads <- threads_empty()
  lapply(seq_len(nrow(headers)), function(i) {
    id <- as.integer(headers$id[i])
    l <- groups$labels[groups$labels$proposal_id == id, , drop = FALSE]
    m <- groups$members[groups$members$proposal_id == id, , drop = FALSE]
    th <- threads[threads$proposal_id == id, , drop = FALSE]
    na_chr <- function(x) if (is.na(x)) NULL else as.character(x)
    list(
      id = id,
      event_id = na_chr(headers$spond_event_id[i]),
      subgroup_id = na_chr(headers$spond_subgroup_id[i]),
      name = headers$name[i],
      status = headers$status[i],
      created_by = headers$created_by[i],
      updated_at = headers$updated_at[i],
      labels = as.character(l$label[order(l$sort_order)]),
      assignments = stats::setNames(as.character(m$label), as.character(m$spond_member_id)),
      comments = thread_part(th, "comment"),
      history = thread_part(th, "history")
    )
  })
}

#' Empty result of `ds_proposal_threads()`
#' @noRd
threads_empty <- function() {
  data.frame(proposal_id = integer(), kind = character(), actor = character(), text = character(),
             created_at = as.POSIXct(character(), tz = "UTC"), stringsAsFactors = FALSE)
}

# Comments or history of one proposal: data.frame(actor, text, created_at).
thread_part <- function(th, kind) {
  x <- th[th$kind == kind, c("actor", "text", "created_at"), drop = FALSE]
  rownames(x) <- NULL
  x
}

#' Proposal statuses that can be edited (editing sets the status to draft)
#' @noRd
proposal_editable <- function(status) status %in% c("draft", "rolled_back", "rejected")

#' Status label in Norwegian
#' @noRd
proposal_status_label <- function(status) unname(ds_status_label[status])

#' Proposals sorted for display: approved first, then waiting for approval,
#' drafts, and rejected/rolled back last; oldest first within each status
#' @noRd
proposals_sorted <- function(proposals) {
  if (length(proposals) == 0) return(proposals)
  rank <- c(approved = 1, pending = 2, draft = 3, rejected = 4, rolled_back = 5)
  r <- vapply(proposals, function(p) unname(rank[p$status]) %||% 9, numeric(1))
  id <- vapply(proposals, function(p) as.numeric(p$id), numeric(1))
  proposals[order(r, id)]
}

#' What can be done with a proposal in a status: action -> button text
#'
#' Every trainer with access may do every action (decided by Kjetil).
#' "edit" opens the editor; the others are status changes (`ds_transition()`).
#' @noRd
proposal_actions <- function(status) {
  switch(status,
    draft = c(edit = "Rediger grupper", submit = "Send til godkjenning", delete = "Slett grupper"),
    pending = c(approve = "Godkjenn", reject = "Avslå"),
    approved = c(rollback = "Rull tilbake"),
    rejected = ,
    rolled_back = c(edit = "Rediger grupper", delete = "Slett grupper"),
    character()
  )
}

#' The proposal list after a status change, without waiting for a reload
#'
#' Approving rolls back other approved proposals for the same event, as the
#' database does. Deleted proposals leave the list.
#' @noRd
proposals_after <- function(proposals, id, action) {
  to <- ds_transitions[[action]]$to
  target <- Filter(function(p) identical(as.integer(p$id), as.integer(id)), proposals)
  if (length(target) == 0 || is.null(to)) return(proposals)
  ev <- target[[1]]$event_id
  out <- lapply(proposals, function(p) {
    if (identical(as.integer(p$id), as.integer(id))) {
      p$status <- to
    } else if (identical(action, "approve") && !is.null(ev) && identical(p$event_id, ev) &&
               identical(p$status, "approved")) {
      p$status <- "rolled_back"
    }
    p
  })
  Filter(function(p) !identical(p$status, "deleted"), out)
}

#' History entry in Norwegian ("laget forslaget")
#' @noRd
history_label <- function(decision) {
  labels <- c(created = "laget forslaget", edited = "endret gruppene", sent_for_approval = "sendte til godkjenning",
              approved = "godkjente", rejected = "avslo", rolled_back = "rullet tilbake", deleted = "slettet")
  out <- unname(labels[decision])
  ifelse(is.na(out), decision, out)
}

#' Why a comment is not allowed, or NULL
#'
#' Same rule as for tags: no health or other sensitive information.
#' @noRd
comment_problem <- function(text) {
  text <- trimws(text %||% "")
  if (!nzchar(text)) return("Skriv en kommentar først.")
  if (nchar(text) > 2000) return("En kommentar kan ha maks 2000 tegn.")
  if (any(vapply(tag_sensitive_patterns(), grepl, logical(1), x = text, ignore.case = TRUE, perl = TRUE))) {
    return("Kommentarer skal ikke inneholde helseopplysninger eller andre sensitive opplysninger.")
  }
  NULL
}

#' "5. okt. kl. 18:40" in Norwegian time (with the year if not this year)
#' @noRd
short_time <- function(t, tz = "Europe/Oslo", now = Sys.time()) {
  t <- as.POSIXct(t)
  y <- format(t, "%Y", tz = tz)
  paste0(as.integer(format(t, "%d", tz = tz)), ". ", no_month(t, tz),
         if (y != format(now, "%Y", tz = tz)) paste0(" ", y) else "",
         " kl. ", format(t, "%H:%M", tz = tz))
}

#' "Gruppe A", "Gruppe B", ... the first one not in use
#' @noRd
next_group_label <- function(labels) {
  candidates <- c(paste("Gruppe", LETTERS), paste("Gruppe", seq_len(200)))
  candidates[!tolower(candidates) %in% tolower(labels)][1]
}

#' A new draft for the editor
#'
#' @param event Minimal event (from `event_minimal()`) for event groups, or
#'   NULL for a gruppeutkast in the context's subgroup (or main group).
#' @param existing Names of the proposals already there (to number the new one).
#' @noRd
draft_new <- function(event = NULL, subgroup_id = NULL, existing = character()) {
  base <- if (is.null(event)) "Utkast" else "Forslag"
  n <- 1
  while (paste(base, n) %in% existing) n <- n + 1
  list(
    id = NULL,
    event = event,
    subgroup_id = if (is.null(event)) subgroup_id else NULL,
    name = paste(base, n),
    labels = c("Gruppe A", "Gruppe B"),
    assignments = stats::setNames(character(), character())
  )
}

#' A draft from a saved proposal
#' @noRd
draft_from <- function(proposal, event = NULL) {
  list(id = proposal$id, event = event, subgroup_id = proposal$subgroup_id, name = proposal$name,
       labels = proposal$labels, assignments = proposal$assignments)
}

#' Players are placed in groups for an event; trainers and other adults are
#' not (see `spond_member_kinds()`). Unknown kind (former members) counts as
#' a player.
#' @noRd
is_player_kind <- function(kind) is.na(kind) | kind == "player"

#' Members who can be placed in the editor
#'
#' For an event: every player who has said "Kommer", plus those already
#' placed (even if they later changed their answer, or are trainers placed
#' before trainers were left out). For a gruppeutkast: the members of the
#' context, plus those already placed.
#' @param members Context members (`members_in_context()`), for gruppeutkast.
#' @return data.frame: member_id, display_name, status (NA for gruppeutkast),
#'   known (still in the group), kind (see `spond_member_kinds()`, NA if
#'   unknown), sorted by name.
#' @noRd
draft_members <- function(draft, group, members) {
  all <- members_in_context(group, NULL)
  placed <- names(draft$assignments)
  if (!is.null(draft$event)) {
    p <- event_participants(draft$event, group)
    p <- p[(p$status == "accepted" & is_player_kind(p$kind)) | p$member_id %in% placed, , drop = FALSE]
    out <- data.frame(member_id = p$member_id, display_name = p$display_name, status = p$status,
                      known = p$known, kind = kind_or_na(p$kind, nrow(p)), stringsAsFactors = FALSE)
    missing <- setdiff(placed, out$member_id)   # placed, but no longer a recipient
  } else {
    out <- data.frame(member_id = members$id, display_name = members$display_name,
                      status = NA_character_, known = TRUE, kind = kind_or_na(members$kind, nrow(members)),
                      stringsAsFactors = FALSE)
    missing <- setdiff(placed, out$member_id)
  }
  if (length(missing)) {
    nm <- all$display_name[match(missing, all$id)]
    out <- rbind(out, data.frame(member_id = missing, display_name = ifelse(is.na(nm), "Tidligere medlem", nm),
                                 status = NA_character_, known = !is.na(nm),
                                 kind = kind_or_na(all$kind, nrow(all))[match(missing, all$id)],
                                 stringsAsFactors = FALSE))
  }
  out <- out[order(tolower(out$display_name)), , drop = FALSE]
  rownames(out) <- NULL
  out
}

kind_or_na <- function(kind, n) if (is.null(kind)) rep(NA_character_, n) else as.character(kind)

#' Move a member, and let a trainer follow their child (gruppeutkast)
#'
#' When a member is moved, every trainer who is parent of that member (and
#' may be placed) is moved to the same group, or back to the pool. With
#' several children, the child moved last decides. A trainer can afterwards
#' be moved on their own.
#' @param links Trainer -> child links (`parent_links` from
#'   `spond_session_data()`), or NULL for no following (event proposals).
#' @return list(draft, followed = ids of the trainers who were moved along).
#' @noRd
draft_move_follow <- function(draft, member_id, label, allowed, links = NULL) {
  new <- draft_move(draft, member_id, label, allowed)
  if (identical(new, draft) || is.null(links) || nrow(links) == 0) return(list(draft = new, followed = character()))
  parents <- intersect(unique(links$parent_id[links$child_id == member_id]), allowed)
  parents <- setdiff(parents, member_id)
  followed <- character()
  for (p in parents) {
    before <- new
    new <- draft_move(new, p, label, allowed)
    if (!identical(before, new)) followed <- c(followed, p)
  }
  list(draft = new, followed = followed)
}

#' Warnings for trainers placed away from their child
#'
#' A trainer who is in a group, while none of their placed children is in the
#' same group, gets "Forelder til Emma H. (Gruppe A)". Children who are not
#' placed give no warning.
#' @param name_of Function giving display names for member ids.
#' @return Named character vector: trainer id -> warning.
#' @noRd
parent_notes <- function(assignments, links, name_of) {
  a <- unlist(assignments)
  if (is.null(links) || nrow(links) == 0 || length(a) == 0) return(character())
  out <- character()
  for (p in unique(links$parent_id)) {
    if (!p %in% names(a)) next
    kids <- links$child_id[links$parent_id == p]
    kids <- kids[kids %in% names(a)]
    if (length(kids) == 0 || any(a[kids] == a[[p]])) next
    out[[p]] <- paste0("Forelder til ", paste0(name_of(kids), " (", unname(a[kids]), ")", collapse = ", "))
  }
  out
}

#' Move a member to a group ("" = back to the pool)
#' @param allowed Member ids that may be placed (from `draft_members()`).
#' @return The changed draft, or the unchanged draft if the move is not allowed.
#' @noRd
draft_move <- function(draft, member_id, label, allowed) {
  if (is.null(member_id) || !member_id %in% allowed) return(draft)
  a <- draft$assignments
  if (is.null(label) || !nzchar(label)) {
    a <- a[names(a) != member_id]
  } else if (label %in% draft$labels) {
    a[[member_id]] <- label
  } else {
    return(draft)
  }
  draft$assignments <- a
  draft
}

# Problem with a new group name, or NULL.
group_label_problem <- function(label, labels) {
  if (!nzchar(label)) return("Skriv inn et navn på gruppen.")
  if (nchar(label) > 60) return("Et gruppenavn kan ha maks 60 tegn.")
  if (tolower(label) %in% tolower(labels)) return("Det finnes allerede en gruppe med det navnet.")
  NULL
}

#' Add, rename or remove a group
#' @return list(draft, error): error is NULL or a message; the draft is
#'   unchanged when there is an error.
#' @noRd
draft_add_group <- function(draft, label) {
  label <- tag_clean(label)
  if (!nzchar(label)) label <- next_group_label(draft$labels)
  err <- group_label_problem(label, draft$labels)
  if (!is.null(err)) return(list(draft = draft, error = err))
  draft$labels <- c(draft$labels, label)
  list(draft = draft, error = NULL)
}

#' @rdname draft_add_group
#' @noRd
draft_rename_group <- function(draft, from, to) {
  to <- tag_clean(to)
  if (is.null(from) || !from %in% draft$labels) return(list(draft = draft, error = NULL))
  if (identical(from, to)) return(list(draft = draft, error = NULL))
  err <- group_label_problem(to, setdiff(draft$labels, from))
  if (!is.null(err)) return(list(draft = draft, error = err))
  draft$labels[draft$labels == from] <- to
  draft$assignments[draft$assignments == from] <- to
  list(draft = draft, error = NULL)
}

#' @rdname draft_add_group
#' @noRd
draft_remove_group <- function(draft, label) {
  if (is.null(label) || !label %in% draft$labels) return(list(draft = draft, error = NULL))
  if (length(draft$labels) == 1) return(list(draft = draft, error = "Forslaget må ha minst én gruppe."))
  draft$labels <- setdiff(draft$labels, label)
  draft$assignments <- draft$assignments[draft$assignments != label]
  list(draft = draft, error = NULL)
}

#' Members of each group in a proposal, and who is not placed
#'
#' @param eligible Member ids that could be placed (for an event: those who
#'   are coming). Used to list "Ikke fordelt".
#' @return list(groups = list of list(label, ids), unplaced = ids)
#' @noRd
proposal_layout <- function(labels, assignments, eligible = character()) {
  list(
    groups = lapply(labels, function(l) list(label = l, ids = names(assignments)[assignments == l])),
    unplaced = setdiff(eligible, names(assignments))
  )
}

#' Short note for a placed member who is no longer coming
#' @noRd
response_note <- function(status) {
  if (is.na(status) || identical(status, "accepted")) return(NULL)
  c(declined = "Kommer ikke", unanswered = "Ikke svart", waiting = "Venteliste",
    unconfirmed = "Ikke bekreftet")[[status]]
}

#' "18:42" in Norwegian time
#' @noRd
clock_time <- function(t, tz = "Europe/Oslo") format(as.POSIXct(t), "%H:%M", tz = tz)

#' The proposal list with a just-saved proposal put in (new or replaced)
#'
#' Lets the page show the result of "Lagre" without waiting for a reload.
#' @noRd
proposal_upsert <- function(proposals, id, draft, name, actor, now = Sys.time(), status = "draft") {
  old <- Filter(function(p) identical(as.integer(p$id), as.integer(id)), proposals)
  rec <- list(
    id = as.integer(id),
    event_id = draft$event$id,
    subgroup_id = if (is.null(draft$event)) draft$subgroup_id else NULL,
    name = name,
    status = status,
    created_by = if (length(old)) old[[1]]$created_by else actor,
    updated_at = now,
    labels = draft$labels,
    assignments = draft$assignments,
    comments = if (length(old)) old[[1]]$comments else thread_part(threads_empty(), "comment"),
    history = if (length(old)) old[[1]]$history else thread_part(threads_empty(), "history")
  )
  if (length(old)) {
    lapply(proposals, function(p) if (identical(as.integer(p$id), as.integer(id))) rec else p)
  } else {
    c(proposals, list(rec))
  }
}
