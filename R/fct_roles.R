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
#' @return list(superadmin, admin, ai, app): all TRUE for the superadmin;
#'   app is TRUE for admins.
#' @noRd
user_rights <- function(profile_id, role = NULL, superadmin = Sys.getenv("SPONDNYMARK_SUPERADMIN")) {
  sa <- is_superadmin(profile_id, superadmin)
  admin <- sa || isTRUE(role$is_admin[1])
  list(superadmin = sa,
       admin = admin,
       ai = sa || isTRUE(role$can_use_ai[1]),
       app = admin || isTRUE(role$can_use_app[1]))
}

no_rights <- function() list(superadmin = FALSE, admin = FALSE, ai = FALSE, app = FALSE)

#' Read a user's rights from the database now (never cached)
#'
#' Used before every action that needs a right (KI, admin panel), so a right
#' that has been taken away stops working at once, also in a session that
#' was opened before.
#' @return The rights (`user_rights()`), or NULL if the database could not be
#'   read (the superadmin is decided without the database).
#' @noRd
rights_read <- function(profile_id, db, superadmin = Sys.getenv("SPONDNYMARK_SUPERADMIN")) {
  if (is.null(profile_id) || length(profile_id) != 1 || is.na(profile_id)) return(no_rights())
  if (is_superadmin(profile_id, superadmin)) return(user_rights(profile_id, NULL, superadmin))
  role <- tryCatch(db$run(function(con) ds_get_role(con, profile_id)), error = function(e) {
    message("Lesing av rettigheter feilet: ", conditionMessage(e))
    NULL
  })
  if (is.null(role)) return(NULL)
  user_rights(profile_id, role, superadmin)
}

#' The logged-in user's rights, read now; no rights if the database fails
#' @noRd
rights_now_fn <- function(user, db, superadmin = Sys.getenv("SPONDNYMARK_SUPERADMIN")) {
  function() {
    u <- isolate(user())
    if (is.null(u)) return(no_rights())
    rights_read(u$profile$id, db, superadmin) %||% user_rights(u$profile$id, NULL, superadmin)
  }
}

# How often an open session reads its rights again (ms).
rights_refresh_ms <- 5 * 60 * 1000

#' A reactive with the logged-in user's rights
#'
#' Read from the database when the user logs in, again when `refresh`
#' changes, and every `every_ms` (so buttons follow changes made by an
#' admin). If the database cannot be read, the user gets only what
#' SPONDNYMARK_SUPERADMIN gives.
#' @noRd
rights_reactive <- function(user, db, superadmin = Sys.getenv("SPONDNYMARK_SUPERADMIN"), refresh = reactive(0),
                            every_ms = rights_refresh_ms) {
  reactive({
    refresh()
    if (!is.null(every_ms)) invalidateLater(every_ms)
    u <- user()
    if (is.null(u)) return(no_rights())
    pid <- u$profile$id
    rights_read(pid, db, superadmin) %||% user_rights(pid, NULL, superadmin)
  })
}

#' Log the user out when app access has been taken away
#'
#' Checks every `every_ms` while someone is logged in. Only a successful read
#' of the database counts: if it fails, the user stays logged in (the next
#' check decides).
#' @param logout function(message) from mod_login (`session$userData$sn_logout`).
#' @noRd
rights_watch <- function(user, db, logout, superadmin = Sys.getenv("SPONDNYMARK_SUPERADMIN"),
                         every_ms = rights_refresh_ms) {
  observe({
    invalidateLater(every_ms)
    u <- user()
    if (is.null(u)) return()
    r <- rights_read(u$profile$id, db, superadmin)
    if (!is.null(r) && !isTRUE(r$app)) logout("Du har ikke lenger tilgang til appen. Spør en administrator.")
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
