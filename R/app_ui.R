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
      tags$header(
        class = "sn-topbar",
        div(
          class = "sn-topbar-inner",
          div(
            class = "sn-brand",
            tags$img(src = "www/favicon.svg", class = "sn-logo", alt = "", width = 32, height = 32),
            span(class = "sn-appname", span(class = "sn-appname-main", "Nymark"),
                 span(class = "sn-appname-sub", "Gruppeorganisering")),
            app_env_badge(),
            mod_quarto_test_ui("quarto_test")  # TEMPORARY (T0), NULL in production
          ),
          mod_teams_bar_ui("teams"),
          mod_events_bar_ui("events"),
          mod_login_bar_ui("login")
        )
      ),
      div(
        class = "sn-wrap",
        mod_login_ui("login"),
        mod_teams_ui("teams"),
        mod_events_ui("events")
      )
    )
  )
}

# Colours: black, yellow, white and Spond red (see claude/videreutvikling.md).
# The same colours are CSS variables (--sn-*) in inst/app/www/custom.css; keep
# the two in step. Yellow is "primary", so primary buttons get black text;
# links are black, since yellow text on white cannot be read.
app_theme <- function() {
  font <- "Barlow, system-ui, -apple-system, 'Segoe UI', sans-serif"
  bslib::bs_theme(
    version = 5,
    primary = "#FFC629",
    secondary = "#5E5E5E",
    success = "#1E7A45",
    danger = "#C81E52",
    warning = "#FFC629",
    bg = "#FFFFFF",
    fg = "#121212",
    base_font = font,
    heading_font = paste("'Barlow Condensed',", font),
    "link-color" = "#121212",
    "link-hover-color" = "#5E5E5E",
    "border-color" = "#E3E1DB",
    "border-radius" = ".75rem",
    "border-radius-sm" = ".5rem",
    "border-radius-lg" = "1rem",
    "headings-font-weight" = 800
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
    # The icon is inspired by Nymark (striped shirt) and Spond (red), without
    # copying either logo. favicon.ico is the fallback for older browsers.
    tags$link(rel = "icon", href = "www/favicon.ico", sizes = "16x16 32x32 48x48"),
    tags$link(rel = "icon", href = "www/favicon.svg", type = "image/svg+xml"),
    tags$link(rel = "apple-touch-icon", href = "www/apple-touch-icon.png"),
    tags$meta(name = "theme-color", content = "#121212"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = app_title()
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
