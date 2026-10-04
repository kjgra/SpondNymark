#' Members: names for display and members in a context
#'
#' @name fct_members
#' @noRd
NULL

#' Compact name for cards and chips
#'
#' First name in full, every following name part as an initial:
#' "Emma Haugen" -> "Emma H.", "Anna Maria Olsen" -> "Anna M. O.".
#' @noRd
short_name <- function(first_name, last_name = "") {
  parts <- strsplit(trimws(paste(first_name, last_name)), "\\s+")
  vapply(parts, function(p) {
    if (length(p) <= 1) return(paste(p, collapse = ""))
    paste(c(p[1], paste0(toupper(substr(p[-1], 1, 1)), ".")), collapse = " ")
  }, character(1))
}

#' Display names for a set of members
#'
#' Short names, except where two members would get the same short name (e.g.
#' "Emma Haugen" and "Emma Hansen" are both "Emma H."): those get their full
#' name. Collisions are checked across the whole main group, so a name looks
#' the same in every subgroup.
#' @param members data.frame with `first_name` and `last_name`.
#' @noRd
member_display_names <- function(members) {
  if (nrow(members) == 0) return(character())
  short <- short_name(members$first_name, members$last_name)
  full <- trimws(paste(members$first_name, members$last_name))
  clash <- short %in% short[duplicated(short)]
  ifelse(clash, full, short)
}

#' Members of a group, or of one of its subgroups, with display names
#'
#' @param group A group from `spond_session_data()`.
#' @param subgroup_id NULL for the whole group.
#' @return data.frame: id, first_name, last_name, profile_id, display_name,
#'   sorted by display name.
#' @noRd
members_in_context <- function(group, subgroup_id = NULL) {
  m <- group$members
  m$display_name <- member_display_names(m)
  if (!is.null(subgroup_id)) {
    ids <- group$memberships$member_id[group$memberships$subgroup_id == subgroup_id]
    m <- m[m$id %in% ids, , drop = FALSE]
  }
  m <- m[order(tolower(m$display_name)), , drop = FALSE]
  rownames(m) <- NULL
  m
}

#' Number of members per subgroup (0 for empty subgroups)
#' @noRd
subgroup_sizes <- function(group) {
  ids <- group$subgroups$id
  tab <- table(factor(group$memberships$subgroup_id, levels = ids))
  stats::setNames(as.integer(tab), ids)
}
