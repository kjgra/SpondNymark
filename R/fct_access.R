#' Access rules: who may use the app for which Spond group
#'
#' Only trainers and team leaders get access. In Spond, admin roles are
#' defined per group with names chosen by the club (e.g. "Trener",
#' "Lagleder"). A group has `roles` (list of `{id, name}`), and each member
#' has `roles` (list of role ids), `subGroups` (list of subgroup ids) and
#' optionally `profile` (the account linked to the member).
#'
#' Roles are granted on the main group and apply to all its subgroups, so
#' access is decided per main group.
#'
#' These field names come from the documented data model and must be
#' confirmed against real data (see dev/spike_spond.R).
#'
#' @name fct_access
#' @noRd
NULL

# Role names that give access, from golem-config.yml (`access_role_names`).
access_role_names <- function() {
  x <- tryCatch(get_golem_config("access_role_names"), error = function(e) NULL)
  if (is.null(x)) c("Teamleder", "Trener", "Lagleder", "Hovedlagleder") else unlist(x)
}

# Role names that make a member a trainer, from golem-config.yml
# (`coach_role_names`). Separate from the access list: a role can make someone
# a trainer in the member list without giving access to the app.
coach_role_names <- function() {
  x <- tryCatch(get_golem_config("coach_role_names"), error = function(e) NULL)
  if (is.null(x)) {
    c("Teamleder", "Trener", "Lagleder", "Hovedlagleder", "Hjelpetrener", "Keepertrener", "Hjelper")
  } else {
    unlist(x)
  }
}

normalise_role_name <- function(x) {
  tolower(trimws(x))
}

# The member record in `group` that belongs to the logged-in profile, or NULL.
spond_member_for_profile <- function(group, profile_id) {
  for (m in group$members) {
    if (!is.null(m$profile$id) && identical(m$profile$id, profile_id)) return(m)
  }
  NULL
}

# Names of the roles a member has in a group.
spond_member_role_names <- function(group, member) {
  if (is.null(member) || length(member$roles) == 0) return(character())
  role_ids <- unlist(member$roles)
  roles <- group$roles %||% list()
  ids <- vapply(roles, function(r) as.character(r$id %||% NA), character(1))
  nms <- vapply(roles, function(r) as.character(r$name %||% NA), character(1))
  unname(nms[ids %in% role_ids])
}

# Subgroups of a group with member counts.
spond_subgroups <- function(group) {
  sgs <- group$subGroups %||% list()
  if (length(sgs) == 0) {
    return(data.frame(id = character(), name = character(), n_members = integer()))
  }
  member_sgs <- lapply(group$members %||% list(), function(m) unlist(m$subGroups))
  ids <- vapply(sgs, function(s) as.character(s$id), character(1))
  data.frame(
    id = ids,
    name = vapply(sgs, function(s) as.character(s$name %||% ""), character(1)),
    n_members = vapply(ids, function(id) sum(vapply(member_sgs, function(x) id %in% x, logical(1))), integer(1)),
    row.names = NULL
  )
}

#' Groups the logged-in user may manage in the app
#'
#' @param groups Result of `spond_get_groups()`.
#' @param profile Result of `spond_get_profile()`.
#' @param role_names Role names that give access (case-insensitive).
#' @return A data.frame with one row per accessible group: `id`, `name`,
#'   `roles` (the matching role names, comma separated) and `n_subgroups`.
#' @noRd
spond_accessible_groups <- function(groups, profile, role_names = access_role_names()) {
  wanted <- normalise_role_name(role_names)
  rows <- lapply(groups, function(g) {
    me <- spond_member_for_profile(g, profile$id)
    my_roles <- spond_member_role_names(g, me)
    matching <- my_roles[normalise_role_name(my_roles) %in% wanted]
    if (length(matching) == 0) return(NULL)
    data.frame(
      id = as.character(g$id),
      name = as.character(g$name %||% ""),
      roles = paste(matching, collapse = ", "),
      n_subgroups = length(g$subGroups %||% list())
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame(id = character(), name = character(), roles = character(), n_subgroups = integer()))
  }
  do.call(rbind, rows)
}

#' Stop unless the user has access to `group_id`
#'
#' Used by the data layer before every read or write.
#' @noRd
assert_group_access <- function(accessible, group_id) {
  if (!isTRUE(group_id %in% accessible$id)) {
    stop("Du har ikke tilgang til denne gruppen.", call. = FALSE)
  }
  invisible(TRUE)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
