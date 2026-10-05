#' Events (Spond "sponds"): data minimisation and display helpers
#'
#' Spond sends a lot per event (recipients with profiles and guardians,
#' comments, decline messages, addresses). The app keeps only what it shows:
#' id, heading, times, type flags, which subgroups the event was sent to and
#' the member ids per response status. Decline messages are dropped on
#' purpose: they often say why someone can't come (sick, injured).
#'
#' @name fct_events
#' @noRd
NULL

# Response status: Spond field, key used in the app, Norwegian label.
# The order is the display order.
event_statuses <- function() {
  data.frame(
    field = c("acceptedIds", "waitinglistIds", "unconfirmedIds", "unansweredIds", "declinedIds"),
    status = c("accepted", "waiting", "unconfirmed", "unanswered", "declined"),
    label = c("Kommer", "Venteliste", "Ikke bekreftet", "Ikke svart", "Kommer ikke"),
    stringsAsFactors = FALSE
  )
}

# Spond timestamps are UTC, "2026-10-10T08:00:00Z" (sometimes with milliseconds).
spond_parse_time <- function(x) {
  if (is.null(x) || length(x) == 0 || identical(x, "")) return(as.POSIXct(NA, tz = "UTC"))
  as.POSIXct(sub("Z$", "", x), format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
}

# Ids from a list that holds either ids or objects with an `id`.
ids_of <- function(x) {
  if (length(x) == 0) return(character())
  as.character(unlist(lapply(x, function(v) if (is.list(v)) v$id else v)))
}

#' Keep only what the app needs from one raw event
#' @return list(id, heading, start, end, cancelled, match, subgroup_ids,
#'   responses = data.frame(member_id, status))
#' @noRd
event_minimal <- function(e) {
  st <- event_statuses()
  resp <- e$responses %||% list()
  responses <- do.call(rbind, lapply(seq_len(nrow(st)), function(i) {
    ids <- ids_of(resp[[st$field[i]]])
    data.frame(member_id = ids, status = rep(st$status[i], length(ids)), stringsAsFactors = FALSE)
  }))
  # One status per member (first in display order wins if Spond ever lists someone twice).
  responses <- responses[!duplicated(responses$member_id), , drop = FALSE]
  rownames(responses) <- NULL
  rec <- e$recipients %||% list()
  list(
    id = as.character(e$id),
    heading = e$heading %||% "(uten tittel)",
    start = spond_parse_time(e$startTimestamp),
    end = spond_parse_time(e$endTimestamp),
    cancelled = isTRUE(e$cancelled),
    match = isTRUE(e$matchEvent),
    subgroup_ids = unique(ids_of(rec$group$subGroups %||% rec$subGroups)),
    responses = responses
  )
}

#' Minimal events for a context, sorted by start time
#'
#' @param raw Events as returned by `spond_get_events()`.
#' @param subgroup_id NULL for the main group, otherwise only events sent to
#'   that subgroup are kept (Spond filters on `subGroupId` too; this is a
#'   second check).
#' @param decreasing TRUE for past events (newest first).
#' @noRd
events_for_context <- function(raw, subgroup_id = NULL, decreasing = FALSE) {
  ev <- lapply(raw %||% list(), event_minimal)
  ev <- Filter(function(e) !is.null(e$id) && length(e$id) == 1, ev)
  if (!is.null(subgroup_id)) ev <- Filter(function(e) subgroup_id %in% e$subgroup_ids, ev)
  if (length(ev) == 0) return(list())
  starts <- vapply(ev, function(e) as.numeric(e$start), numeric(1))
  ev[order(starts, decreasing = decreasing, na.last = TRUE)]
}

#' Who an event was sent to: subgroup names, or the main group name
#' @noRd
event_sent_to <- function(event, group) {
  names <- group$subgroups$name[match(event$subgroup_ids, group$subgroups$id)]
  names <- names[!is.na(names)]
  if (length(names) == 0) group$name else names
}

#' Participants of an event (its recipients) with display name and status
#'
#' Names come from the whole main group, so they look the same in every
#' context. Members who are no longer in the group are shown as "Tidligere
#' medlem".
#' @return data.frame: member_id, display_name, status, label; sorted by
#'   status order, then name.
#' @noRd
event_participants <- function(event, group) {
  st <- event_statuses()
  r <- event$responses
  members <- members_in_context(group, NULL)
  r$display_name <- members$display_name[match(r$member_id, members$id)]
  r$display_name[is.na(r$display_name)] <- "Tidligere medlem"
  r$label <- st$label[match(r$status, st$status)]
  r <- r[order(match(r$status, st$status), tolower(r$display_name)), , drop = FALSE]
  rownames(r) <- NULL
  r
}

#' Counts per status, in display order, only statuses that occur
#' @return named integer vector, names are Norwegian labels.
#' @noRd
event_counts <- function(event) {
  st <- event_statuses()
  n <- vapply(st$status, function(s) sum(event$responses$status == s), integer(1))
  stats::setNames(n, st$label)[n > 0]
}

# Norwegian date formatting without depending on the server's locale.
no_weekday <- function(t, tz = "Europe/Oslo") {
  c("søn.", "man.", "tir.", "ons.", "tor.", "fre.", "lør.")[as.POSIXlt(t, tz = tz)$wday + 1]
}
no_month <- function(t, tz = "Europe/Oslo") {
  c("jan.", "feb.", "mars", "apr.", "mai", "juni", "juli", "aug.", "sep.", "okt.", "nov.", "des.")[as.POSIXlt(t, tz = tz)$mon + 1]
}

#' "lør. 10. okt. kl. 10:00–11:30" in Norwegian time
#'
#' Adds the year when it is not the current year, and the end date when the
#' event ends on another day.
#' @noRd
event_when <- function(start, end = NA, tz = "Europe/Oslo", now = Sys.time()) {
  if (is.na(start)) return("Tid ikke satt")
  s <- start
  day <- function(t) {
    y <- format(t, "%Y", tz = tz)
    paste0(no_weekday(t, tz), " ", as.integer(format(t, "%d", tz = tz)), ". ", no_month(t, tz),
           if (y != format(now, "%Y", tz = tz)) paste0(" ", y) else "")
  }
  out <- paste0(day(s), " kl. ", format(s, "%H:%M", tz = tz))
  if (!is.na(end)) {
    e <- end
    same_day <- format(s, "%Y-%m-%d", tz = tz) == format(e, "%Y-%m-%d", tz = tz)
    out <- paste0(out, "–", if (same_day) "" else paste0(day(e), " kl. "), format(e, "%H:%M", tz = tz))
  }
  out
}
