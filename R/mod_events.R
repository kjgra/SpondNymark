#' events UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_events_ui <- function(id) {
  ns <- NS(id)
  tagList(

  )
}

#' events Server Functions
#'
#' @noRd
mod_events_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_events_ui("events_1")

## To be copied in the server
# mod_events_server("events_1")
