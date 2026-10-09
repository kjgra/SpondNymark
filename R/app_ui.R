#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    bslib::page_fluid(
      theme = app_theme(),
      title = app_title(),
      div(
        class = "sn-wrap",
        tags$header(
          class = "sn-topbar",
          div(class = "sn-brand", span(class = "sn-dot"), "SpondNymark", app_env_badge()),
          mod_teams_bar_ui("teams"),
          mod_login_bar_ui("login")
        ),
        mod_login_ui("login"),
        mod_teams_ui("teams"),
        mod_events_ui("events")
      )
    )
  )
}

# Colours from the UX prototype.
app_theme <- function() {
  bslib::bs_theme(
    version = 5,
    primary = "#2F7D55",
    secondary = "#5B6B5F",
    bg = "#F5F7F4",
    fg = "#1B271E"
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = app_title()
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
