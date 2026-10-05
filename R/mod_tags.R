#' Member tags: loading, showing and editing
#'
#' Keeps the tags of the current main group up to date (reloaded when the
#' group changes, after own changes, and every `poll_ms` so tags set by other
#' trainers show up), and owns the tag editor: a dialog opened by clicking a
#' member chip anywhere in the app (see `member_chip()`).
#'
#' The module has no UI of its own. Other modules draw chips with the
#' returned `chip()` function.
#'
#' @param context Reactive from `mod_teams_server()`.
#' @param user Reactive from `mod_login_server()` (access and profile id).
#' @param db Database handle from `db_handle()`.
#' @return list(
#'   `table` reactive data.frame(member_id, tag) for the current main group,
#'   `error` reactive message if tags could not be loaded, else NULL,
#'   `chip(member_id, name, note)` a member chip with tags that opens the editor
#'   (call inside a reactive context))
#' @noRd
mod_tags_server <- function(id, context, user, db, poll_ms = 15000, suggestions = tag_suggestions()) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tag_data <- reactiveVal(tags_empty())   # only invalidates when the content changes
    load_error <- reactiveVal(NULL)
    refresh <- reactiveVal(0)
    member <- reactiveVal(NULL)            # member id in the editor
    msg <- reactiveVal(NULL)

    group_id <- reactive({
      ctx <- context()
      if (is.null(ctx)) NULL else ctx$group_id
    })

    observe({
      gid <- group_id()
      req(gid)
      refresh()
      invalidateLater(poll_ms)
      acc <- isolate(user())$access
      res <- tryCatch(db$run(function(con) ds_list_tags(con, acc, gid)), error = function(e) {
        message("Henting av tagger feilet: ", conditionMessage(e))
        NULL
      })
      if (is.null(res)) {
        load_error("Taggene kunne ikke hentes fra databasen. Prøv igjen om litt.")
      } else {
        load_error(NULL)
        tag_data(data.frame(member_id = as.character(res$spond_member_id), tag = as.character(res$tag),
                            stringsAsFactors = FALSE))
      }
    })

    # Members are looked up in the main group, so ids from the browser can
    # never point outside the group the trainer has access to.
    member_row <- function(id) {
      ctx <- context()
      if (is.null(ctx) || is.null(id)) return(NULL)
      m <- ctx$group$members[ctx$group$members$id == id, , drop = FALSE]
      if (nrow(m) == 1) m else NULL
    }

    write <- function(f, fail_msg) {
      ok <- tryCatch({
        db$run(f)
        TRUE
      }, error = function(e) {
        message("Lagring av tagg feilet: ", conditionMessage(e))
        FALSE
      })
      if (ok) {
        msg(NULL)
        refresh(refresh() + 1)
      } else {
        msg(fail_msg)
      }
      ok
    }

    add_tag <- function(raw) {
      id <- member()
      req(member_row(id))
      tag <- tag_clean(raw)
      problem <- tag_problem(tag)
      if (!is.null(problem)) return(msg(problem))
      tag <- tag_canonical(tag, c(tag_data()$tag, suggestions))
      if (tolower(tag) %in% tolower(tags_for(tag_data(), id))) {
        msg(NULL)
      } else {
        u <- user()
        gid <- group_id()
        if (!write(function(con) ds_add_tag(con, u$access, gid, id, tag, u$profile$id),
                   "Kunne ikke lagre taggen. Prøv igjen.")) return()
      }
      session$sendCustomMessage("sn-clear-input", ns("new_tag"))
    }

    observeEvent(input$open, {
      m <- member_row(input$open)
      req(m)
      member(m$id)
      msg(NULL)
      showModal(modalDialog(
        title = paste("Tagger for", trimws(paste(m$first_name, m$last_name))),
        easyClose = TRUE,
        footer = modalButton("Ferdig"),
        uiOutput(ns("current")),
        div(
          class = "sn-tag-add",
          tags$input(id = ns("new_tag"), type = "text", class = "form-control sn-tag-input",
                     placeholder = "Ny tag", maxlength = "40", autocomplete = "off",
                     `aria-label` = "Ny tag", `data-sn-submit` = ns("submit")),
          tags$button(type = "button", class = "btn btn-primary sn-tag-submit",
                      `data-sn-submit-for` = ns("new_tag"), `data-sn-submit` = ns("submit"), "Legg til")
        ),
        uiOutput(ns("msg")),
        uiOutput(ns("suggest")),
        p(class = "sn-hint sn-tag-privacy",
          "Ikke bruk helseopplysninger eller andre sensitive opplysninger i tagger. ",
          paste0("Taggene ses av alle trenere og lagledere i ", context()$group_name, "."))
      ))
    })

    observeEvent(input$submit, add_tag(input$submit))
    observeEvent(input$quick, add_tag(input$quick))
    observeEvent(input$remove, {
      id <- member()
      req(member_row(id))
      u <- user()
      gid <- group_id()
      write(function(con) ds_remove_tag(con, u$access, gid, id, input$remove),
            "Kunne ikke fjerne taggen. Prøv igjen.")
    })

    # A new main group closes the editor.
    observeEvent(group_id(), ignoreInit = TRUE, {
      member(NULL)
      removeModal()
    })

    output$current <- renderUI({
      id <- member()
      req(id)
      have <- tags_for(tag_data(), id)
      if (length(have) == 0) return(p(class = "sn-hint", "Ingen tagger ennå."))
      div(class = "sn-tags sn-tag-current", lapply(have, function(t) {
        span(class = "sn-chip sn-tag-chip", t,
             tags$button(type = "button", class = "sn-tag-remove", `aria-label` = paste("Fjern", t),
                         `data-sn-input` = ns("remove"), `data-sn-value` = t, "×"))
      }))
    })

    output$suggest <- renderUI({
      id <- member()
      req(id)
      s <- tag_suggestions_for(tag_data(), id, suggestions)
      if (length(s) == 0) return(NULL)
      div(class = "sn-tags sn-tag-suggest",
          span(class = "sn-hint", "Forslag:"),
          lapply(s, function(t) {
            tags$button(type = "button", class = "btn btn-sm btn-outline-secondary sn-suggest",
                        `data-sn-input` = ns("quick"), `data-sn-value` = t, t)
          }))
    })

    output$msg <- renderUI({
      m <- msg()
      if (is.null(m)) NULL else div(class = "sn-alert", role = "alert", m)
    })

    list(
      table = reactive(tag_data()),
      error = reactive(load_error()),
      chip = function(member_id, name, note = NULL) {
        member_chip(member_id, name, tags_for(tag_data(), member_id), ns("open"), note)
      }
    )
  })
}
