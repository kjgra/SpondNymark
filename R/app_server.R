#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  user <- mod_login_server("login")

  # Fase 2 erstatter dette med valg av hovedgruppe og undergruppe.
  output$main <- renderUI({
    u <- user()
    req(u)
    groups <- u$groups
    bslib::card(
      bslib::card_header(paste0("Hei, ", u$profile$first_name, "!")),
      bslib::card_body(
        p("Du har tilgang til ", if (length(groups) == 1) "denne gruppen:" else "disse gruppene:"),
        tags$ul(
          class = "sn-grouplist",
          lapply(groups, function(g) {
            tags$li(
              strong(g$name),
              span(class = "sn-hint",
                   paste0(" ", g$my_roles, " · ", nrow(g$members), " medlemmer · ",
                          nrow(g$subgroups), " undergrupper"))
            )
          })
        ),
        p(class = "sn-hint", "Valg av lag og undergruppe kommer i neste steg (fase 2).")
      )
    )
  })
}
