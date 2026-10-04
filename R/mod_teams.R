#' Choose main group and subgroup
#'
#' After login the trainer chooses which main group (lag) to work with, and
#' then a subgroup or the whole group. Each step is skipped when there is only
#' one choice: one group skips the group step, a group without subgroups
#' skips the subgroup step. Both can be changed later from the top bar.
#'
#' The module has two UI parts sharing one server: `mod_teams_ui()` (the
#' choice pages) and `mod_teams_bar_ui()` (the selectors in the top bar).
#'
#' @param user Reactive from `mod_login_server()`.
#' @return A reactive: NULL until a context is chosen, otherwise a list with
#'   `group_id`, `group_name`, `subgroup_id` (NULL = whole group),
#'   `subgroup_name`, `label`, `members` (from `members_in_context()`) and
#'   `group` (the group from `spond_session_data()`).
#' @noRd
#' @importFrom shiny NS tagList
mod_teams_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("picker"))
}

#' @noRd
mod_teams_bar_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("bar"), class = "sn-ctx")
}

# Value used for "Hele gruppen" in inputs.
teams_all <- ".all"

# A button that sends `value` to the Shiny input `input_id`.
pick_button <- function(input_id, value, title, detail) {
  tags$button(
    type = "button", class = "btn sn-pickrow",
    onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                      input_id, gsub("[^A-Za-z0-9._-]", "", value)),
    span(class = "sn-pickrow-title", title),
    span(class = "sn-pickrow-detail", detail)
  )
}

# A compact select for the top bar: Bootstrap 5 dropdown style, label for screen readers.
bar_select <- function(input_id, label, choices, selected) {
  htmltools::tagAppendAttributes(
    selectInput(input_id, NULL, choices = choices, selected = selected, selectize = FALSE, width = "auto"),
    class = "form-select", `aria-label` = label, .cssSelector = "select"
  )
}

#' @noRd
mod_teams_server <- function(id, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    group_id <- reactiveVal(NULL)
    subgroup <- reactiveVal(NULL)   # NULL = not chosen yet, teams_all = whole group, else a subgroup id

    current_group <- reactive({
      u <- user()
      gid <- group_id()
      if (is.null(u) || is.null(gid)) NULL else u$groups[[gid]]
    })

    # Choosing a group also decides the subgroup step: skipped when there are no subgroups.
    choose_group <- function(gid) {
      u <- user()
      if (is.null(u) || is.null(gid) || !gid %in% names(u$groups)) return(invisible(FALSE))
      group_id(gid)
      subgroup(if (nrow(u$groups[[gid]]$subgroups) == 0) teams_all else NULL)
      invisible(TRUE)
    }

    choose_subgroup <- function(sid) {
      g <- current_group()
      if (is.null(g) || is.null(sid)) return(invisible(FALSE))
      if (identical(sid, teams_all) || sid %in% g$subgroups$id) subgroup(sid)
      invisible(TRUE)
    }

    observeEvent(user(), ignoreNULL = FALSE, {
      u <- user()
      group_id(NULL)
      subgroup(NULL)
      if (!is.null(u) && length(u$groups) == 1) choose_group(names(u$groups)[1])
    })

    step <- reactive({
      if (is.null(user())) "none"
      else if (is.null(group_id())) "pick_group"
      else if (is.null(subgroup())) "pick_subgroup"
      else "ready"
    })

    observeEvent(input$pick_group, choose_group(input$pick_group))
    observeEvent(input$pick_subgroup, choose_subgroup(input$pick_subgroup))
    observeEvent(input$back_to_groups, {
      group_id(NULL)
      subgroup(NULL)
    })
    observeEvent(input$group_select, ignoreInit = TRUE, {
      if (!identical(input$group_select, group_id())) choose_group(input$group_select)
    })
    observeEvent(input$subgroup_select, ignoreInit = TRUE, {
      if (!identical(input$subgroup_select, subgroup())) choose_subgroup(input$subgroup_select)
    })

    output$picker <- renderUI({
      u <- user()
      switch(step(),
        pick_group = div(
          class = "sn-pick",
          bslib::card(bslib::card_body(
            h2("Velg lag"),
            p(class = "sn-hint", "Du er trener eller lagleder i flere lag. Velg hvilket du vil jobbe med."),
            div(class = "sn-picklist", lapply(u$groups, function(g) {
              pick_button(ns("pick_group"), g$id, g$name,
                          paste0(g$my_roles, " · ", nrow(g$members), " medlemmer"))
            }))
          ))
        ),
        pick_subgroup = {
          g <- current_group()
          sizes <- subgroup_sizes(g)
          div(
            class = "sn-pick",
            bslib::card(bslib::card_body(
              if (length(u$groups) > 1) {
                tags$button(type = "button", class = "btn btn-link sn-back",
                            onclick = sprintf("Shiny.setInputValue('%s', Date.now(), {priority: 'event'})",
                                              ns("back_to_groups")),
                            "← Bytt lag")
              },
              h2(paste0(g$name, ": velg undergruppe")),
              p(class = "sn-hint", "Undergruppene har egne arrangementer og medlemmer. Velg hva du vil jobbe med."),
              div(
                class = "sn-picklist",
                pick_button(ns("pick_subgroup"), teams_all, "Hele gruppen", paste(nrow(g$members), "medlemmer")),
                lapply(seq_len(nrow(g$subgroups)), function(i) {
                  pick_button(ns("pick_subgroup"), g$subgroups$id[i], g$subgroups$name[i],
                              paste(sizes[[g$subgroups$id[i]]], "medlemmer"))
                })
              )
            ))
          )
        },
        NULL
      )
    })

    output$bar <- renderUI({
      if (!step() %in% c("pick_subgroup", "ready")) return(NULL)
      u <- user()
      g <- current_group()
      tagList(
        if (length(u$groups) > 1) {
          bar_select(ns("group_select"), "Lag",
                     choices = stats::setNames(names(u$groups), vapply(u$groups, `[[`, "", "name")),
                     selected = group_id())
        },
        if (identical(step(), "ready") && nrow(g$subgroups) > 0) {
          bar_select(ns("subgroup_select"), "Undergruppe",
                     choices = c(stats::setNames(teams_all, "Hele gruppen"),
                                 stats::setNames(g$subgroups$id, g$subgroups$name)),
                     selected = subgroup())
        }
      )
    })

    reactive({
      if (!identical(step(), "ready")) return(NULL)
      g <- current_group()
      sid <- if (identical(subgroup(), teams_all)) NULL else subgroup()
      sname <- if (is.null(sid)) NULL else g$subgroups$name[g$subgroups$id == sid]
      list(
        group_id = g$id,
        group_name = g$name,
        subgroup_id = sid,
        subgroup_name = sname,
        label = if (is.null(sid)) paste("Hele", g$name) else sname,
        members = members_in_context(g, sid),
        group = g
      )
    })
  })
}
