#' Events for the chosen context
#'
#' Lists upcoming and past events for the main group or subgroup chosen in
#' `mod_teams_server()`. In the main group every event is shown, marked with
#' the subgroup(s) it was sent to; a subgroup shows only its own events.
#' Choosing an event opens it with its participants (`mod_participants`).
#' Below the list: the members of the context, and group drafts (folded away,
#' used rarely).
#'
#' Events are fetched from Spond when the context changes and kept per tab
#' until "Oppdater" is pressed. Only the minimal fields from
#' `events_for_context()` are kept.
#'
#' @param context Reactive from `mod_teams_server()`.
#' @param user Reactive from `mod_login_server()` (for the Spond session).
#' @param spond List of Spond functions, see `spond_api()`.
#' @param now Function giving the current time (replaceable in tests).
#' @param past_days Size of the window for past events, and how much "Vis
#'   eldre" adds.
#' @noRd
#' @importFrom shiny NS tagList
mod_events_ui <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("view")))
}

# Card title that always shows which main group a subgroup belongs to:
# "Nymark G/J 2016" or "Nymark G/J 2016 › Nymark Ulv G10".
context_title <- function(ctx) {
  if (length(ctx$path) == 1) return(span(class = "sn-crumb-current", ctx$path))
  tagList(
    span(class = "sn-crumb-parent", ctx$path[1]),
    span(class = "sn-crumb-sep", `aria-hidden` = "true", "›"),
    span(class = "sn-crumb-current", ctx$path[2])
  )
}

# JS that sends `value` to a Shiny input (value restricted to id characters).
set_input_js <- function(input_id, value) {
  sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
          input_id, gsub("[^A-Za-z0-9._-]", "", value))
}

# Small labels on an event: which subgroups it was sent to (main group view
# only), match, cancelled.
event_badges <- function(event, ctx) {
  tagList(
    if (is.null(ctx$subgroup_id)) {
      lapply(event_sent_to(event, ctx$group), function(n) span(class = "sn-tag sn-tag-to", n))
    },
    if (event$match) span(class = "sn-tag sn-tag-match", "Kamp"),
    if (event$cancelled) span(class = "sn-tag sn-tag-cancelled", "Avlyst")
  )
}

# "12 kommer · 3 ikke svart"
event_count_text <- function(event) {
  r <- event$responses$status
  parts <- c(paste(sum(r == "accepted"), "kommer"),
             if (any(r == "unanswered")) paste(sum(r == "unanswered"), "ikke svart"))
  paste(parts, collapse = " · ")
}

event_row <- function(ns, event, ctx) {
  s <- event$start
  tz <- "Europe/Oslo"
  tags$button(
    type = "button",
    class = paste("btn sn-event", if (event$cancelled) "sn-event-cancelled"),
    onclick = set_input_js(ns("open"), event$id),
    div(class = "sn-event-date", `aria-hidden` = "true",
        span(class = "sn-event-day", if (is.na(s)) "?" else as.integer(format(s, "%d", tz = tz))),
        span(class = "sn-event-month", if (is.na(s)) "" else no_month(s, tz))),
    div(class = "sn-event-main",
        span(class = "sn-event-title", event$heading),
        span(class = "sn-event-when", event_when(event$start, event$end)),
        div(class = "sn-tags", event_badges(event, ctx))),
    span(class = "sn-event-count", event_count_text(event))
  )
}

# Two-button switch between upcoming and past events.
tab_switch <- function(ns, tab) {
  btn <- function(value, label) {
    tags$button(type = "button", class = paste("btn btn-sm", if (tab == value) "btn-primary" else "btn-outline-primary"),
                `aria-pressed` = if (tab == value) "true" else "false",
                onclick = set_input_js(ns("tab"), value), label)
  }
  div(class = "btn-group", role = "group", `aria-label` = "Vis arrangementer",
      btn("upcoming", "Kommende"), btn("past", "Gjennomførte"))
}

#' @noRd
mod_events_server <- function(id, context, user, spond = spond_api(), now = Sys.time, past_days = 30) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tab <- reactiveVal("upcoming")
    past_window <- reactiveVal(past_days)
    selected <- reactiveVal(NULL)   # minimal event, or NULL for the list
    refresh <- reactiveVal(0)

    # A new context starts on the list of upcoming events.
    observeEvent(context(), ignoreNULL = FALSE, {
      selected(NULL)
      tab("upcoming")
      past_window(past_days)
    })

    fetch <- function(ctx, ...) {
      tryCatch(
        list(events = spond$events(user()$spond, group_id = ctx$group_id, subgroup_id = ctx$subgroup_id, ...),
             error = NULL),
        spond_error = function(e) list(events = NULL, error = conditionMessage(e)),
        error = function(e) {
          message("Henting av arrangementer feilet: ", conditionMessage(e))
          list(events = NULL, error = "Kunne ikke hente arrangementer fra Spond. Prøv igjen om litt.")
        }
      )
    }

    upcoming <- reactive({
      ctx <- context()
      req(ctx)
      refresh()
      res <- fetch(ctx, min_end = now(), max_events = 100)
      res$events <- events_for_context(res$events, ctx$subgroup_id)
      res
    })

    past <- reactive({
      ctx <- context()
      req(ctx)
      refresh()
      t <- now()
      res <- fetch(ctx, min_end = t - past_window() * 86400, max_end = t, max_events = 200)
      res$events <- events_for_context(res$events, ctx$subgroup_id, decreasing = TRUE)
      res
    })

    shown <- reactive(if (identical(tab(), "past")) past() else upcoming())

    observeEvent(input$tab, if (input$tab %in% c("upcoming", "past")) tab(input$tab))
    observeEvent(input$older, past_window(past_window() + past_days))
    observeEvent(input$refresh, refresh(refresh() + 1))
    observeEvent(input$back, selected(NULL))
    observeEvent(input$open, {
      ev <- Filter(function(e) identical(e$id, input$open), shown()$events)
      if (length(ev)) selected(ev[[1]])
    })

    # Keep an open event up to date after "Oppdater".
    observeEvent(shown(), {
      cur <- selected()
      if (is.null(cur)) return()
      ev <- Filter(function(e) identical(e$id, cur$id), shown()$events)
      if (length(ev)) selected(ev[[1]])
    })

    mod_participants_server("participants", event = selected, group = reactive(context()$group))

    list_view <- function(ctx) {
      res <- shown()
      is_past <- identical(tab(), "past")
      bslib::card(
        bslib::card_header(
          class = "sn-card-head",
          h2(class = "sn-title", context_title(ctx)),
          div(class = "sn-head-actions",
              tab_switch(ns, tab()),
              tags$button(type = "button", class = "btn btn-sm btn-link sn-refresh",
                          onclick = set_input_js(ns("refresh"), "x"), title = "Hent på nytt fra Spond",
                          "Oppdater"))
        ),
        bslib::card_body(
          if (!is.null(res$error)) {
            div(class = "sn-alert", role = "alert", res$error)
          } else if (length(res$events) == 0) {
            p(class = "sn-hint", if (is_past) paste0("Ingen gjennomførte arrangementer de siste ", past_window(), " dagene.")
                                 else "Ingen kommende arrangementer.")
          } else {
            div(class = "sn-events", lapply(res$events, function(e) event_row(ns, e, ctx)))
          },
          if (is_past && is.null(res$error)) {
            div(class = "sn-more",
                span(class = "sn-hint", paste0("Viser siste ", past_window(), " dager.")),
                tags$button(type = "button", class = "btn btn-sm btn-outline-secondary",
                            onclick = set_input_js(ns("older"), "x"), "Vis eldre"))
          }
        )
      )
    }

    output$view <- renderUI({
      ctx <- context()
      req(ctx)
      ev <- selected()
      if (!is.null(ev)) {
        return(div(
          class = "sn-detail",
          tags$button(type = "button", class = "btn btn-link sn-back",
                      onclick = set_input_js(ns("back"), "x"), "← Arrangementer"),
          bslib::card(
            bslib::card_header(
              class = "sn-card-head",
              div(class = "sn-crumbs", context_title(ctx)),
              h2(class = "sn-title", ev$heading),
              div(class = "sn-event-when", event_when(ev$start, ev$end)),
              div(class = "sn-tags",
                  span(class = "sn-hint", "Sendt til:"),
                  lapply(event_sent_to(ev, ctx$group), function(n) span(class = "sn-tag sn-tag-to", n)),
                  if (ev$match) span(class = "sn-tag sn-tag-match", "Kamp"),
                  if (ev$cancelled) span(class = "sn-tag sn-tag-cancelled", "Avlyst"))
            ),
            bslib::card_body(mod_participants_ui(ns("participants")))
          )
        ))
      }
      tagList(
        list_view(ctx),
        tags$details(
          class = "sn-fold",
          tags$summary(paste0("Medlemmer i ", ctx$label, " · ", nrow(ctx$members))),
          if (nrow(ctx$members) == 0) p(class = "sn-hint", "Ingen medlemmer.")
          else div(class = "sn-chips", lapply(ctx$members$display_name, function(n) span(class = "sn-chip", n)))
        ),
        tags$details(
          class = "sn-fold",
          tags$summary("Gruppeutkast"),
          p(class = "sn-hint", "Utkast til nye undergrupper. Kommer i en senere versjon av appen.")
        )
      )
    })

    invisible(selected)
  })
}
