#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # One database connection per session, opened when first needed.
  db <- db_handle()

  # Only people on the allowlist (and admins) may log in; see R/fct_allowlist.R.
  user <- mod_login_server("login", allow = function(ident, profile_id) login_check(db, ident, profile_id))
  context <- mod_teams_server("teams", user)
  tagger <- mod_tags_server("tags", context, user, db)
  mod_events_server("events", context, user, tagger, db)
  # Extra rights (KI, admin) for the logged-in trainer; see R/fct_roles.R.
  rights <- rights_reactive(user, db)
  mod_admin_server("admin", context, user, db, rights = rights)
  mod_quarto_test_server("quarto_test")  # TEMPORARY (T0), does nothing in production

  # Registered last, so the modules can release their edit locks first.
  session$onSessionEnded(db$close)
}
