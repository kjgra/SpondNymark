#' Keep only the Spond data the app needs
#'
#' Spond's group data contains much more personal information than we use
#' (date of birth, address, phone, e-mail, nationality, guardians, ...).
#' Right after login we keep only:
#' - the groups where the user is a trainer/leader (all other groups are dropped),
#' - per group: id, name, the user's roles, subgroups (id, name), and members
#'   (id, first name, last name, linked profile id, subgroup memberships).
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
spond_session_data <- function(groups, profile, role_names = access_role_names()) {
  access <- spond_accessible_groups(groups, profile, role_names)
  kept <- Filter(function(g) as.character(g$id) %in% access$id, groups)
  slim <- lapply(kept, function(g) {
    members <- g$members %||% list()
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
        stringsAsFactors = FALSE
      ),
      memberships = spond_memberships(members)
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
