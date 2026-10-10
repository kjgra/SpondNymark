#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # One database connection per session, opened when first needed.
  db <- db_handle()

  # Only trainers with app access (and admins) may log in; see R/fct_allowlist.R.
  user <- mod_login_server("login", allow = function(ident, profile_id) login_check(db, profile_id))
  context <- mod_teams_server("teams", user)
  tagger <- mod_tags_server("tags", context, user, db)
  # Extra rights (KI, admin) for the logged-in trainer; see R/fct_roles.R.
  # `rights` drives what is shown (read again every 5 min); `rights_now`
  # reads the database again before every action that needs a right.
  rights <- rights_reactive(user, db)
  rights_now <- rights_now_fn(user, db)
  # Taking away app access logs the trainer out within 5 minutes.
  rights_watch(user, db, logout = function(m) session$userData$sn_logout(m))
  mod_events_server("events", context, user, tagger, db, rights = rights, rights_now = rights_now)
  mod_admin_server("admin", context, user, db, rights = rights, rights_now = rights_now)
  mod_quarto_test_server("quarto_test")  # TEMPORARY (T0), does nothing in production

  # Registered last, so the modules can release their edit locks first.
  session$onSessionEnded(db$close)
}
