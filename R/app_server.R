#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  user <- mod_login_server("login")
  context <- mod_teams_server("teams", user)

  # Fase 3 erstatter dette med arrangementer og deltakere.
  output$main <- renderUI({
    ctx <- context()
    req(ctx)
    bslib::card(
      bslib::card_header(context_title(ctx)),
      bslib::card_body(
        p(n_members(nrow(ctx$members))),
        div(class = "sn-chips", lapply(ctx$members$display_name, function(n) span(class = "sn-chip", n))),
        p(class = "sn-hint", "Arrangementer og deltakere kommer i neste steg (fase 3).")
      )
    )
  })
}

# Card title that always shows which main group a subgroup belongs to:
# "Nymark G/J 2016" or "Nymark G/J 2016 › Nymark Ulv G10".
context_title <- function(ctx) {
  if (length(ctx$path) == 1) return(span(class = "sn-crumb-current", ctx$path))
  tagList(
    span(class = "sn-crumb-parent", ctx$path[1]),
    span(class = "sn-crumb-sep", `aria-hidden` = "true", "\u203a"),
    span(class = "sn-crumb-current", ctx$path[2])
  )
}
