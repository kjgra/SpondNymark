#' participants UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_participants_ui <- function(id) {
  ns <- NS(id)
  tagList(

  )
}

#' participants Server Functions
#'
#' @noRd
mod_participants_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_participants_ui("participants_1")

## To be copied in the server
# mod_participants_server("participants_1")
