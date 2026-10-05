#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  user <- mod_login_server("login")
  context <- mod_teams_server("teams", user)
  mod_events_server("events", context, user)
}
