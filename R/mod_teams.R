#' Choose main group and subgroup
#'
#' After login the trainer chooses which main group (lag) to work with, and
#' then the main group itself or one of its subgroups. Each step is skipped when there is only
#' one choice: one group skips the group step, a group without subgroups
#' skips the subgroup step. Both can be changed later from the top bar.
#'
#' The module has two UI parts sharing one server: `mod_teams_ui()` (the
#' choice pages) and `mod_teams_bar_ui()` (the selectors in the top bar).
#'
#' @param user Reactive from `mod_login_server()`.
#' @return A reactive: NULL until a context is chosen, otherwise a list with
#'   `group_id`, `group_name`, `subgroup_id` (NULL = whole group),
#'   `subgroup_name`, `label` (main group or subgroup name), `path` (main
#'   group name, then subgroup name if any), `members` (from
#'   `members_in_context()`) and `group` (the group from `spond_session_data()`).
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

# Value used for "the main group itself" (no subgroup) in inputs.
teams_all <- ".all"

# A button that sends `value` to the Shiny input `input_id`.
pick_button <- function(input_id, value, title, detail, class = NULL) {
  tags$button(
    type = "button", class = paste(c("btn sn-pickrow", class), collapse = " "),
    onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                      input_id, gsub("[^A-Za-z0-9._-]", "", value)),
    span(class = "sn-pickrow-title", title),
    span(class = "sn-pickrow-detail", detail)
  )
}

# Subgroups in a native select: indented with a tree mark, so it is clear they
# belong to the main group listed above them (non-breaking spaces survive in <option>).
subgroup_option_label <- function(names) {
  paste0("\u00a0\u00a0\u2514\u00a0", names)
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
                          paste0(g$my_roles, " \u00b7 ", n_members(nrow(g$members)),
                                 if (nrow(g$subgroups)) paste0(" \u00b7 ", nrow(g$subgroups), " undergrupper")))
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
              h2("Velg gruppe"),
              p(class = "sn-hint",
                "Hovedgruppen viser alt i gruppen. Undergruppene har egne arrangementer og medlemmer."),
              # Hierarchy: the main group first, its subgroups nested underneath.
              div(
                class = "sn-picklist",
                pick_button(ns("pick_subgroup"), teams_all, g$name,
                            paste("Hovedgruppe \u00b7", n_members(nrow(g$members))), class = "sn-pickrow-main"),
                div(
                  class = "sn-subtree", role = "group", `aria-label` = paste("Undergrupper i", g$name),
                  div(class = "sn-subtree-label", paste("Undergrupper i", g$name)),
                  lapply(seq_len(nrow(g$subgroups)), function(i) {
                    pick_button(ns("pick_subgroup"), g$subgroups$id[i], g$subgroups$name[i],
                                n_members(sizes[[g$subgroups$id[i]]]))
                  })
                )
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
          # The main group first, its subgroups indented underneath.
          bar_select(ns("subgroup_select"), paste("Hovedgruppe eller undergruppe i", g$name),
                     choices = c(stats::setNames(teams_all, g$name),
                                 stats::setNames(g$subgroups$id, subgroup_option_label(g$subgroups$name))),
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
        label = if (is.null(sid)) g$name else sname,
        path = c(g$name, sname),   # main group, then subgroup if any
        members = members_in_context(g, sid),
        group = g
      )
    })
  })
}
