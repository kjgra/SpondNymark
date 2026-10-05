#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # One database connection per session, opened when first needed.
  db <- db_handle()

  user <- mod_login_server("login")
  context <- mod_teams_server("teams", user)
  tagger <- mod_tags_server("tags", context, user, db)
  mod_events_server("events", context, user, tagger, db)

  # Registered last, so the modules can release their edit locks first.
  session$onSessionEnded(db$close)
}
