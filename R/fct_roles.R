#' Extra rights in the app: KI and admin (phase T2b)
#'
#' Who is a trainer comes from Spond (`access`). On top of that:
#' - the superadmin is the Spond profile in SPONDNYMARK_SUPERADMIN, never in
#'   the database, so nobody can be locked out;
#' - `app_roles` gives a trainer KI access (used from T3) and/or admin.
#'
#' | | Superadmin | Admin | Trainer |
#' |---|---|---|---|
#' | See the gear (admin panel) | yes | yes | – |
#' | Season plan, team settings, exercises | yes | yes | – |
#' | Give and take KI | yes | yes | – |
#' | Give and take admin | yes | – | – |
#'
#' The checks are made on the server before every change; hiding a button is
#' not enough.
#' @name fct_roles
#' @noRd
NULL

#' The rights of a user
#' @param role One row of `ds_get_role()` (or NULL).
#' @return list(superadmin, admin, ai): admin and ai are TRUE for the
#'   superadmin.
#' @noRd
user_rights <- function(profile_id, role = NULL, superadmin = Sys.getenv("SPONDNYMARK_SUPERADMIN")) {
  sa <- is_superadmin(profile_id, superadmin)
  list(superadmin = sa,
       admin = sa || isTRUE(role$is_admin[1]),
       ai = sa || isTRUE(role$can_use_ai[1]))
}

no_rights <- function() list(superadmin = FALSE, admin = FALSE, ai = FALSE)

#' A reactive with the logged-in user's rights
#'
#' Read from the database when the user logs in, and again when `refresh`
#' changes (after the rights have been changed in the admin panel). If the
#' database cannot be read, the user gets only what SPONDNYMARK_SUPERADMIN
#' gives.
#' @noRd
rights_reactive <- function(user, db, superadmin = Sys.getenv("SPONDNYMARK_SUPERADMIN"), refresh = reactive(0)) {
  reactive({
    refresh()
    u <- user()
    if (is.null(u)) return(no_rights())
    pid <- u$profile$id
    if (is_superadmin(pid, superadmin)) return(user_rights(pid, NULL, superadmin))
    role <- tryCatch(db$run(function(con) ds_get_role(con, pid)), error = function(e) {
      message("Lesing av rettigheter feilet: ", conditionMessage(e))
      NULL
    })
    user_rights(pid, role, superadmin)
  })
}

#' The trainers in a main group who can be given rights: trainers in Spond
#' with a linked profile (the profile is what identifies them in the app).
#' @return data.frame(profile_id, name, roles), sorted by name.
#' @noRd
role_candidates <- function(group) {
  m <- group$members
  if (is.null(m) || nrow(m) == 0 || is.null(m$kind)) {
    return(data.frame(profile_id = character(), name = character(), roles = character()))
  }
  keep <- m$kind == "coach" & !is.na(m$profile_id) & nzchar(m$profile_id)
  m <- m[keep, , drop = FALSE]
  m <- m[!duplicated(m$profile_id), , drop = FALSE]
  out <- data.frame(profile_id = m$profile_id, name = member_display_names(m),
                    roles = if (is.null(m$roles)) "" else m$roles)
  out <- out[order(tolower(out$name)), , drop = FALSE]
  rownames(out) <- NULL
  out
}
