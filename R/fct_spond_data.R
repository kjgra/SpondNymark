#' Keep only the Spond data the app needs
#'
#' Spond's group data contains much more personal information than we use
#' (date of birth, address, phone, e-mail, nationality, guardians, ...).
#' Right after login we keep only:
#' - the groups where the user is a trainer/leader (all other groups are dropped),
#' - per group: id, name, the user's roles, subgroups (id, name), and members
#'   (id, first name, last name, linked profile id, subgroup memberships,
#'   kind: trainer/player/other adult, and the trainer role names),
#' - which trainer is parent of which member (member ids only), so a trainer
#'   can follow their child in group drafts. See `spond_member_kinds()`.
#'
#' The raw response is then discarded. Nothing here is written to the database.
#'
#' @name fct_spond_data
#' @noRd
NULL

#' @param groups Result of `spond_get_groups()`.
#' @param profile Result of `spond_get_profile()`.
#' @param role_names Role names that give access.
#' @return A list with `profile` (id, first_name, last_name), `access` (from
#'   `spond_accessible_groups()`) and `groups` (named by group id).
#' @noRd
spond_session_data <- function(groups, profile, role_names = access_role_names(),
                               coach_roles = coach_role_names()) {
  access <- spond_accessible_groups(groups, profile, role_names)
  kept <- Filter(function(g) as.character(g$id) %in% access$id, groups)
  slim <- lapply(kept, function(g) {
    members <- g$members %||% list()
    k <- spond_member_kinds(g, coach_roles)
    list(
      id = as.character(g$id),
      name = as.character(g$name %||% ""),
      my_roles = access$roles[access$id == as.character(g$id)],
      subgroups = spond_subgroups(g)[, c("id", "name")],
      members = data.frame(
        id = vapply(members, function(m) as.character(m$id), character(1)),
        first_name = vapply(members, function(m) as.character(m$firstName %||% ""), character(1)),
        last_name = vapply(members, function(m) as.character(m$lastName %||% ""), character(1)),
        profile_id = vapply(members, function(m) as.character(m$profile$id %||% NA), character(1)),
        kind = k$kind,
        roles = k$roles,
        stringsAsFactors = FALSE
      ),
      memberships = spond_memberships(members),
      parent_links = k$parent_links
    )
  })
  names(slim) <- vapply(slim, `[[`, character(1), "id")
  list(
    profile = list(
      id = as.character(profile$id),
      first_name = as.character(profile$firstName %||% ""),
      last_name = as.character(profile$lastName %||% "")
    ),
    access = access,
    groups = slim
  )
}

# Long format: one row per (member, subgroup).
spond_memberships <- function(members) {
  rows <- lapply(members, function(m) {
    sgs <- as.character(unlist(m$subGroups))
    if (length(sgs) == 0) return(NULL)
    data.frame(member_id = as.character(m$id), subgroup_id = sgs, stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame(member_id = character(), subgroup_id = character()))
  do.call(rbind, rows)
}

#' Trainers, players and other adults in a group
#'
#' - "coach": has one of `coach_roles`.
#' - "adult": no trainer role, but is guardian of another member in the group
#'   (their profile is among that member's guardians).
#' - "player": everyone else.
#'
#' Guardians are only read here. The only thing kept is which trainer is
#' parent of which member, as member ids, held in memory for this session.
#' Nothing about guardians is written to the database.
#'
#' @return list: `kind` and `roles` (trainer role names, comma separated, ""
#'   for others), one per member in Spond's order, and `parent_links`
#'   (data.frame: parent_id, child_id).
#' @noRd
spond_member_kinds <- function(group, coach_roles = coach_role_names()) {
  members <- group$members %||% list()
  no_links <- data.frame(parent_id = character(), child_id = character(), stringsAsFactors = FALSE)
  if (length(members) == 0) return(list(kind = character(), roles = character(), parent_links = no_links))
  ids <- vapply(members, function(m) as.character(m$id), character(1))
  wanted <- normalise_role_name(coach_roles)
  coach_roles_of <- lapply(members, function(m) {
    r <- spond_member_role_names(group, m)
    r[normalise_role_name(r) %in% wanted]
  })
  is_coach <- lengths(coach_roles_of) > 0
  profile <- vapply(members, function(m) as.character(m$profile$id %||% NA), character(1))
  guardian_profiles <- lapply(members, function(m) {
    p <- vapply(m$guardians %||% list(), function(gd) as.character(gd$profile$id %||% NA), character(1))
    unique(p[!is.na(p)])
  })
  children_of <- lapply(seq_along(members), function(i) {
    if (is.na(profile[i])) return(integer())
    setdiff(which(vapply(guardian_profiles, function(x) profile[i] %in% x, logical(1))), i)
  })
  is_parent <- lengths(children_of) > 0
  kind <- ifelse(is_coach, "coach", ifelse(is_parent, "adult", "player"))
  links <- lapply(which(is_coach & is_parent), function(i) {
    data.frame(parent_id = ids[i], child_id = ids[children_of[[i]]], stringsAsFactors = FALSE)
  })
  list(
    kind = unname(kind),
    roles = vapply(coach_roles_of, function(r) paste(r, collapse = ", "), character(1)),
    parent_links = if (length(links)) do.call(rbind, links) else no_links
  )
}
