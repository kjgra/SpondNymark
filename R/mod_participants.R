#' Participants of one event
#'
#' The participants are always the event's recipients, grouped by response:
#' Kommer, Venteliste, Ikke bekreftet, Ikke svart, Kommer ikke. Only members
#' who are coming (and later: members already placed in a group) can be put
#' in groups, so "Kommer" is shown first and open.
#'
#' @param event Reactive minimal event (from `event_minimal()`), or NULL.
#' @param group Reactive group from `spond_session_data()`.
#' @param tagger Value of `mod_tags_server()`, or NULL for plain chips.
#' @noRd
#' @importFrom shiny NS tagList
mod_participants_ui <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("list")))
}

#' @noRd
mod_participants_server <- function(id, event, group, tagger = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    participants <- reactive({
      ev <- event()
      req(ev)
      event_participants(ev, group())
    })

    output$list <- renderUI({
      parts <- participants()
      if (nrow(parts) == 0) return(p(class = "sn-hint", "Ingen mottakere på dette arrangementet."))
      st <- event_statuses()
      present <- st[st$status %in% parts$status, , drop = FALSE]
      tagList(
        div(class = "sn-counts", lapply(seq_len(nrow(present)), function(i) {
          n <- sum(parts$status == present$status[i])
          span(class = paste0("sn-count sn-status-", present$status[i]),
               strong(n), " ", tolower(present$label[i]))
        })),
        lapply(seq_len(nrow(present)), function(i) {
          s <- present$status[i]
          rows <- parts[parts$status == s, , drop = FALSE]
          open_id <- ns(paste0("open_", s))
          remembered_details(
            open_id, isolate(input[[paste0("open_", s)]]), s %in% c("accepted", "waiting", "unconfirmed"),
            class = paste0("sn-status sn-status-", s),
            paste0(present$label[i], " (", nrow(rows), ")"),
            # Former members cannot be tagged: they are no longer in the group.
            member_chips(rows$member_id, rows$display_name, tagger, clickable = rows$known)
          )
        })
      )
    })

    invisible(participants)
  })
}
