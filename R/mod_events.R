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
#' @param tagger Value of `mod_tags_server()`: member chips show tags and open
#'   the tag editor. NULL gives plain chips.
#' @param db Database handle from `db_handle()`. With it, group proposals
#'   are shown and can be made (`mod_groups`); NULL leaves them out.
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

#' The "Godkjenning" button in the top bar (only with the database)
#' @noRd
mod_events_bar_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("bar"), class = "sn-approve-slot")
}

# Yellow button with the number of proposals waiting. On narrow screens only
# the icon and the number show (see custom.css); the button keeps its name for
# screen readers via aria-label.
approvals_button <- function(ns, n_pending, active = FALSE) {
  label <- if (n_pending > 0) paste0("Godkjenning, ", n_pending, " venter") else "Godkjenning, ingen venter"
  tags$button(
    type = "button", class = paste("btn sn-approve", if (active) "is-active"),
    `aria-label` = label, `aria-pressed` = if (active) "true" else "false",
    onclick = set_input_js(ns("tab"), "approvals"),
    HTML(paste0('<svg class="sn-approve-icon" viewBox="0 0 24 24" width="20" height="20" aria-hidden="true" ',
                'fill="none" stroke="currentColor" stroke-width="2.4" stroke-linecap="round" ',
                'stroke-linejoin="round"><path d="M20 6L9 17l-5-5"></path></svg>')),
    span(class = "sn-approve-text", "Godkjenning"),
    if (n_pending > 0) span(class = "sn-approve-count", `aria-hidden` = "true", n_pending)
  )
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

# Date block: weekday, day and month ("LØR / 10 / OKT"). Black, red for a
# match and grey when cancelled (see custom.css). Hidden from screen readers,
# which get the full date from event_when().
event_date_block <- function(s, tz = "Europe/Oslo") {
  short <- function(x) sub("\\.$", "", x)
  div(class = "sn-event-date", `aria-hidden` = "true",
      span(class = "sn-event-wday", if (is.na(s)) "" else short(no_weekday(s, tz))),
      span(class = "sn-event-day", if (is.na(s)) "?" else as.integer(format(s, "%d", tz = tz))),
      span(class = "sn-event-month", if (is.na(s)) "" else short(no_month(s, tz))))
}

# A trainer in the member list: black row with the role name.
coach_row <- function(name, roles) {
  div(class = "sn-coach",
      HTML(paste0('<svg class="sn-coach-icon" viewBox="0 0 24 24" width="16" height="16" aria-hidden="true" ',
                  'fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round">',
                  '<circle cx="9" cy="14" r="5"></circle><path d="M13 10l7-4M14 13h7"></path></svg>')),
      span(class = "sn-coach-name", name),
      if (nzchar(roles)) span(class = "sn-coach-role", roles))
}

# The members of a context in three sections: trainers (no tags), players
# (tags) and other adults (guardians who are members without a trainer role;
# the section is left out when empty).
member_sections <- function(m, tagger = NULL) {
  kind <- if (is.null(m$kind)) rep("player", nrow(m)) else m$kind
  roles <- if (is.null(m$roles)) rep("", nrow(m)) else m$roles
  coaches <- which(kind == "coach")
  players <- which(kind == "player")
  adults <- which(kind == "adult")
  section_title <- function(title, n) h3(class = "sn-members-title", paste0(title, " \u00b7 ", n))
  tagList(
    div(class = "sn-members-section",
        section_title("Trenere", length(coaches)),
        if (length(coaches) == 0) p(class = "sn-hint", "Ingen med trenerrolle her.")
        else div(class = "sn-coach-list", lapply(coaches, function(i) coach_row(m$display_name[i], roles[i])))),
    div(class = "sn-members-section",
        section_title("Spillere", length(players)),
        if (length(players) == 0) p(class = "sn-hint", "Ingen spillere her.")
        else tagList(
          if (!is.null(tagger)) p(class = "sn-hint", "Trykk på et navn for å legge til eller fjerne tagger."),
          member_chips(m$id[players], m$display_name[players], tagger)
        )),
    if (length(adults)) {
      div(class = "sn-members-section",
          section_title("Andre voksne", length(adults)),
          p(class = "sn-hint", "Foresatte som er medlemmer uten trenerrolle. De får ikke tagger."),
          member_chips(m$id[adults], m$display_name[adults]))
    }
  )
}

# Classes shared by the event row and the event page header.
event_state_class <- function(event) {
  paste(c(if (isTRUE(event$match)) "sn-event-match", if (isTRUE(event$cancelled)) "sn-event-cancelled"),
        collapse = " ")
}

event_row <- function(ns, event, ctx, groups = NULL) {
  tags$button(
    type = "button",
    class = paste("btn sn-event", event_state_class(event)),
    onclick = set_input_js(ns("open"), event$id),
    event_date_block(event$start),
    div(class = "sn-event-main",
        span(class = "sn-event-title", event$heading),
        span(class = "sn-event-when", event_when(event$start, event$end)),
        div(class = "sn-tags", event_badges(event, ctx), if (!is.null(groups)) groups$event_badge(event$id))),
    span(class = "sn-event-count", event_count_text(event))
  )
}

# Segmented control: upcoming or past events. "Godkjenning" is a button in the
# top bar (approvals_button()); while it is shown, neither segment is pressed.
tab_switch <- function(ns, tab) {
  btn <- function(value, ...) {
    on <- identical(tab, value)
    tags$button(type = "button", class = paste("sn-seg-btn", if (on) "is-on"),
                `aria-pressed` = if (on) "true" else "false",
                onclick = set_input_js(ns("tab"), value), ...)
  }
  div(class = "sn-seg", role = "group", `aria-label` = "Vis arrangementer",
      btn("upcoming", "Kommende"), btn("past", "Gjennomførte"))
}

#' @noRd
mod_events_server <- function(id, context, user, tagger = NULL, db = NULL, spond = spond_api(),
                              now = Sys.time, past_days = 30) {
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
        spond_error = function(e) list(events = NULL, error = conditionMessage(e),
                                       expired = inherits(e, "spond_expired")),
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

    # An expired Spond session sends the trainer back to the login page.
    observe({
      res <- shown()
      if (isTRUE(res$expired) && is.function(session$userData$sn_session_expired)) {
        session$userData$sn_session_expired()
      }
    })

    observeEvent(input$tab, {
      if (!input$tab %in% c("upcoming", "past", if (!is.null(db)) "approvals")) return()
      tab(input$tab)
      # The top bar button also works from an open event: back to the list.
      if (identical(input$tab, "approvals")) selected(NULL)
    })
    observeEvent(input$older, past_window(past_window() + past_days))
    observeEvent(input$refresh, refresh(refresh() + 1))
    observeEvent(input$back, selected(NULL))
    observeEvent(input$open, {
      # Also upcoming events, so "Åpne arrangement" works from the approvals list.
      ev <- Filter(function(e) identical(e$id, input$open), c(shown()$events, upcoming()$events))
      if (length(ev)) selected(ev[[1]])
    })

    # Keep an open event up to date after "Oppdater".
    observeEvent(shown(), {
      cur <- selected()
      if (is.null(cur)) return()
      ev <- Filter(function(e) identical(e$id, cur$id), shown()$events)
      if (length(ev)) selected(ev[[1]])
    })

    mod_participants_server("participants", event = selected, group = reactive(context()$group), tagger = tagger)

    groups <- if (!is.null(db)) {
      # Proposals are loaded for the upcoming events (for "Godkjenning") and
      # for the events in the tab that is shown.
      known_events <- reactive({
        ev <- c(upcoming()$events, if (identical(tab(), "past")) past()$events)
        ev[!duplicated(vapply(ev, function(e) e$id, ""))]
      })
      mod_groups_server("groups", context, user, db, tagger, event = selected, now = now,
                        events = known_events, open_event_input = ns("open"))
    }
    editing <- if (is.null(groups)) function() FALSE else groups$editing

    # "Godkjenning" in the top bar: only with the database, once a team is
    # chosen, and not while the group editor is open.
    output$bar <- renderUI({
      if (is.null(groups) || is.null(context()) || editing()) return(NULL)
      approvals_button(ns, groups$n_pending(), active = identical(tab(), "approvals") && is.null(selected()))
    })
    # The slot starts empty, and Shiny does not render outputs it thinks are
    # hidden. Render it anyway, so the button can appear.
    outputOptions(output, "bar", suspendWhenHidden = FALSE)

    # Members of the context, with tags. Rendered on its own so new tags do
    # not redraw the event list.
    output$members <- renderUI({
      ctx <- context()
      req(ctx)
      m <- ctx$members
      tagList(
        if (!is.null(tagger) && !is.null(tagger$error())) div(class = "sn-alert", role = "alert", tagger$error()),
        if (nrow(m) == 0) p(class = "sn-hint", "Ingen medlemmer.") else member_sections(m, tagger)
      )
    })

    list_view <- function(ctx) {
      res <- shown()
      is_past <- identical(tab(), "past")
      is_approvals <- identical(tab(), "approvals") && !is.null(groups)
      bslib::card(
        class = "sn-list-card",
        bslib::card_header(
          class = "sn-card-head",
          div(class = "sn-list-top",
              h2(class = "sn-title", context_title(ctx)),
              tags$button(type = "button", class = "btn btn-sm btn-link sn-refresh",
                          onclick = set_input_js(ns("refresh"), "x"), title = "Hent på nytt fra Spond",
                          "↻ Oppdater")),
          tab_switch(ns, tab())
        ),
        bslib::card_body(
          if (is_approvals) {
            tagList(h3(class = "sn-section-title", "Til godkjenning"), mod_groups_pending_ui(ns("groups")))
          } else if (!is.null(res$error)) {
            div(class = "sn-alert", role = "alert", res$error)
          } else if (length(res$events) == 0) {
            p(class = "sn-hint", if (is_past) paste0("Ingen gjennomførte arrangementer de siste ", past_window(), " dagene.")
                                 else "Ingen kommende arrangementer.")
          } else {
            div(class = "sn-events", lapply(res$events, function(e) event_row(ns, e, ctx, groups)))
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
      if (editing()) return(mod_groups_editor_ui(ns("groups")))
      ev <- selected()
      if (!is.null(ev)) {
        return(div(
          class = "sn-detail",
          tags$button(type = "button", class = "btn btn-link sn-back",
                      onclick = set_input_js(ns("back"), "x"), "← Arrangementer"),
          bslib::card(
            bslib::card_header(
              class = paste("sn-card-head sn-detail-head", event_state_class(ev)),
              event_date_block(ev$start),
              div(class = "sn-detail-main",
                  div(class = "sn-crumbs", context_title(ctx)),
                  h2(class = "sn-title", ev$heading),
                  div(class = "sn-event-when", event_when(ev$start, ev$end)),
                  div(class = "sn-tags",
                      span(class = "sn-hint", "Sendt til:"),
                      lapply(event_sent_to(ev, ctx$group), function(n) span(class = "sn-tag sn-tag-to", n)),
                      if (ev$match) span(class = "sn-tag sn-tag-match", "Kamp"),
                      if (ev$cancelled) span(class = "sn-tag sn-tag-cancelled", "Avlyst")))
            ),
            bslib::card_body(
              if (!is.null(groups)) div(class = "sn-event-groups", mod_groups_event_ui(ns("groups"))),
              mod_participants_ui(ns("participants"))
            )
          )
        ))
      }
      tagList(
        list_view(ctx),
        remembered_details(
          ns("members_open"), isolate(input$members_open), FALSE, class = "sn-fold",
          paste0("Medlemmer i ", ctx$label, " \u00b7 ", nrow(ctx$members)),
          uiOutput(ns("members"))
        ),
        if (!is.null(groups)) {
          n <- groups$n_drafts()
          remembered_details(
            ns("drafts_open"), isolate(input$drafts_open), FALSE, class = "sn-fold",
            paste0("Gruppeutkast", if (n > 0) paste0(" \u00b7 ", n)),
            mod_groups_drafts_ui(ns("groups"))
          )
        }
      )
    })

    invisible(selected)
  })
}
