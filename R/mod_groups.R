#' Group proposals: list, editor and edit locks
#'
#' Shows the group proposals of the selected event and the gruppeutkast of
#' the context, and owns the editor where members are placed in groups.
#'
#' The editor board is one piece of HTML that works two ways (see www/sn.js):
#' with a mouse, cards are dragged to a column; with a finger (or a click
#' without dragging), a card is picked and then placed with "Plasser her".
#' On touch screens the columns are stacked (CSS `pointer: coarse`). Moves
#' happen in the browser at once and are reported to the server, which keeps
#' the draft; nothing is saved until "Lagre".
#'
#' Edit locks are advisory: other trainers see "Redigeres av …" and get a
#' warning, but can still edit. The lock is taken `defer_ms` after the editor
#' opens (so opening is not slowed down), renewed every `heartbeat_ms`
#' and released when the editor closes or the session ends.
#'
#' UI parts (placed by `mod_events`): `mod_groups_event_ui()` (proposals of
#' the selected event), `mod_groups_drafts_ui()` (gruppeutkast) and
#' `mod_groups_editor_ui()` (the editor, shown instead of everything else).
#'
#' @param context Reactive from `mod_teams_server()`.
#' @param user Reactive from `mod_login_server()`.
#' @param db Database handle from `db_handle()`.
#' @param tagger Value of `mod_tags_server()`, or NULL.
#' @param event Reactive selected event (minimal), or NULL.
#' @param event_ids Reactive ids of the events in the list (their proposals
#'   are loaded, so the list can show which events have groups).
#' @param now Function giving the current time.
#' @return list(`editing` reactive TRUE while the editor is open,
#'   `n_drafts` reactive number of gruppeutkast, `event_badge(event_id)` a
#'   small label for an event row, or NULL).
#' @noRd
#' @importFrom shiny NS tagList
mod_groups_event_ui <- function(id) uiOutput(NS(id, "event_cards"))

#' @noRd
mod_groups_drafts_ui <- function(id) uiOutput(NS(id, "draft_cards"))

#' @noRd
mod_groups_editor_ui <- function(id) uiOutput(NS(id, "editor"))

#' @noRd
mod_groups_server <- function(id, context, user, db, tagger = NULL, event = reactive(NULL),
                              event_ids = reactive(character()), now = Sys.time,
                              poll_ms = 10000, heartbeat_ms = 30000, defer_ms = 400) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session_key <- paste0("s-", session$token %||% "local", "-", id)

    proposals <- reactiveVal(list())
    locks <- reactiveVal(NULL)
    load_error <- reactiveVal(NULL)
    refresh <- reactiveVal(0)

    draft <- reactiveVal(NULL)
    is_editing <- reactiveVal(FALSE)  # separate from draft(), so moves do not redraw the page
    open_count <- reactiveVal(0)      # redraws the editor
    board_version <- reactiveVal(0)   # redraws the board (not on every move)
    editor_msg <- reactiveVal(NULL)
    pending_edit <- reactiveVal(NULL)

    db_try <- function(f, what) {
      tryCatch(db$run(f), error = function(e) {
        message(what, " feilet: ", conditionMessage(e))
        NULL
      })
    }

    # Proposals and locks, reloaded every poll_ms. reactiveVal() only
    # invalidates when the value changes, so unchanged data redraws nothing.
    observe({
      ctx <- context()
      req(ctx)
      ids <- event_ids()
      refresh()
      invalidateLater(poll_ms)
      u <- isolate(user())
      res <- db_try(function(con) {
        h <- ds_list_proposals(con, u$access, ctx$group_id, ids, ctx$subgroup_id)
        g <- ds_proposal_groups(con, u$access, ctx$group_id, h$id)
        l <- ds_active_locks(con, u$access, ctx$group_id)
        list(proposals = proposals_view(h, g), locks = l[l$session_key != session_key, , drop = FALSE])
      }, "Henting av gruppeforslag")
      if (is.null(res)) {
        load_error("Gruppeforslagene kunne ikke hentes fra databasen. Prøv igjen om litt.")
        return()
      }
      load_error(NULL)
      proposals(res$proposals)
      l <- res$locks
      locks(data.frame(proposal_id = l$proposal_id, event_id = l$spond_event_id,
                       subgroup_id = l$spond_subgroup_id, editor = l$editor,
                       started_at = l$started_at, stringsAsFactors = FALSE))
    })

    names_by_profile <- reactive({
      ctx <- context()
      req(ctx)
      profile_names(ctx$group)
    })
    editor_name <- function(profile_id) profile_name(names_by_profile(), profile_id)

    lock_on <- function(proposal_id) {
      l <- locks()
      if (is.null(l)) return(NULL)
      l <- l[!is.na(l$proposal_id) & l$proposal_id == proposal_id, , drop = FALSE]
      if (nrow(l)) l[1, ] else NULL
    }
    new_locks <- function(event_id = NULL, subgroup_id = NULL) {
      l <- locks()
      if (is.null(l)) return(l)
      l <- l[is.na(l$proposal_id), , drop = FALSE]
      if (!is.null(event_id)) return(l[!is.na(l$event_id) & l$event_id == event_id, , drop = FALSE])
      l <- l[is.na(l$event_id), , drop = FALSE]
      if (is.null(subgroup_id)) l else l[!is.na(l$subgroup_id) & l$subgroup_id == subgroup_id, , drop = FALSE]
    }

    # Editor ----------------------------------------------------------------

    eligible <- reactive({
      d <- draft()
      req(d)
      ctx <- context()
      draft_members(d, ctx$group, ctx$members)
    })

    # Database work the trainer does not need to wait for (taking and
    # releasing the edit lock, reloading the list) runs `defer_ms` after the
    # last click. The page is drawn in several rounds between browser and
    # server (the editor and the board are nested outputs), and work done in
    # between would delay every round. Tasks run in the order they were queued.
    queue <- new.env()
    queue$tasks <- list()
    kick <- reactiveVal(0)
    after_update <- function(f) {
      queue$tasks <- c(queue$tasks, list(f))
      kick(isolate(kick()) + 1)
    }
    kick_later <- debounce(kick, defer_ms)
    observeEvent(kick_later(), ignoreInit = TRUE, {
      tasks <- queue$tasks
      queue$tasks <- list()
      for (f in tasks) f()
    })

    acquire_lock <- function(d, ctx, u) {
      db_try(function(con) {
        ds_acquire_lock(con, u$access, ctx$group_id, u$profile$id, session_key,
                        proposal_id = d$id, subgroup_id = d$subgroup_id, event_id = d$event$id)
      }, "Redigeringslås")   # advisory: editing works even if the lock fails
    }

    open_editor <- function(d) {
      ctx <- context()
      u <- user()
      after_update(function() acquire_lock(d, ctx, u))
      draft(d)
      is_editing(TRUE)
      editor_msg(NULL)
      open_count(open_count() + 1)
      board_version(board_version() + 1)
    }

    close_editor <- function() {
      if (is.null(isolate(draft()))) return(invisible())
      draft(NULL)
      is_editing(FALSE)
      after_update(function() {
        db_try(function(con) ds_release_lock(con, session_key), "Frigjøring av lås")
        refresh(isolate(refresh()) + 1)
      })
    }

    observeEvent(input$new, {
      ctx <- context()
      req(ctx, is.null(draft()))
      props <- proposals()
      if (identical(input$new, "event")) {
        ev <- event()
        req(ev, !event_is_past(ev, now()))
        existing <- vapply(Filter(function(p) identical(p$event_id, ev$id), props), `[[`, "", "name")
        open_editor(draft_new(event = ev, existing = existing))
      } else if (identical(input$new, "draft")) {
        existing <- vapply(Filter(function(p) is.null(p$event_id), props), `[[`, "", "name")
        open_editor(draft_new(NULL, subgroup_id = ctx$subgroup_id, existing = existing))
      }
    })

    start_edit <- function(pid) {
      p <- Filter(function(p) identical(as.character(p$id), as.character(pid)), proposals())
      req(length(p) == 1, proposal_editable(p[[1]]$status))
      p <- p[[1]]
      ev <- if (is.null(p$event_id)) NULL else event()
      # An event proposal is edited from its event; the event gives the participants.
      req(is.null(p$event_id) || identical(ev$id, p$event_id))
      open_editor(draft_from(p, ev))
    }

    observeEvent(input$edit, {
      req(is.null(draft()))
      l <- lock_on(as.integer(input$edit))
      if (is.null(l)) return(start_edit(input$edit))
      pending_edit(input$edit)
      showModal(modalDialog(
        title = "Noen redigerer allerede",
        p(paste0(editor_name(l$editor), " redigerer dette forslaget nå (startet kl. ",
                 clock_time(l$started_at), ").")),
        p("Redigerer dere samtidig, blir den siste som lagrer stående."),
        footer = tagList(
          modalButton("Avbryt"),
          tags$button(type = "button", class = "btn btn-primary", `data-sn-input` = ns("force_edit"),
                      `data-sn-value` = input$edit, `data-sn-busy` = "Åpner", "Rediger likevel")
        )
      ))
    })

    observeEvent(input$force_edit, {
      removeModal()
      if (identical(as.character(input$force_edit), as.character(pending_edit()))) start_edit(input$force_edit)
      pending_edit(NULL)
    })

    # Moves come from the browser. Only members that may be placed, and only
    # existing groups, are accepted.
    observeEvent(input$move, {
      d <- draft()
      req(d)
      m <- input$move
      draft(draft_move(d, m$member, m$group, eligible()$member_id))
    })

    change_groups <- function(res) {
      draft(res$draft)
      editor_msg(res$error)
      board_version(board_version() + 1)
    }
    observeEvent(input$add_group, { req(draft()); change_groups(draft_add_group(draft(), input$add_group)) })
    observeEvent(input$remove_group, { req(draft()); change_groups(draft_remove_group(draft(), input$remove_group)) })
    observeEvent(input$rename, {
      req(draft())
      change_groups(draft_rename_group(draft(), input$rename$from, input$rename$to))
    })

    observeEvent(input$save, {
      d <- draft()
      req(d)
      ctx <- context()
      u <- user()
      name <- tag_clean(input$name %||% d$name)
      v <- tryCatch(ds_validate_proposal(name, d$labels, d$assignments), error = function(e) conditionMessage(e))
      if (is.character(v)) return(editor_msg(v))
      saved <- tryCatch({
        db$run(function(con) {
          ds_save_proposal(con, u$access, u$profile$id, ctx$group_id, name, d$labels, d$assignments,
                           subgroup_id = d$subgroup_id, event_id = d$event$id, id = d$id)
        })
      }, error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("kan ikke redigeres|ikke tilgang|annen gruppe", msg)) return(msg)
        message("Lagring av gruppeforslag feilet: ", msg)
        "Kunne ikke lagre. Prøv igjen om litt."
      })
      if (is.character(saved)) return(editor_msg(saved))
      # Show the saved proposal at once; the next reload confirms it.
      proposals(proposal_upsert(proposals(), saved, d, name, u$profile$id))
      close_editor()
      showNotification(paste0("«", name, "» er lagret."), type = "message", duration = 4)
    })

    observeEvent(input$cancel, close_editor())

    # A new context (or logging out) closes the editor without saving.
    observeEvent(context(), ignoreInit = TRUE, ignoreNULL = FALSE, close_editor())

    # Keep the lock alive while the editor is open (not on every move). If
    # it has expired, e.g. after the laptop slept, take it again. The first
    # run, right when the editor opens, only starts the timer.
    beat <- new.env()
    beat$started <- FALSE
    observe({
      if (!is_editing()) {
        beat$started <- FALSE
        return()
      }
      invalidateLater(heartbeat_ms)
      if (!beat$started) {
        beat$started <- TRUE
        return()
      }
      alive <- db_try(function(con) ds_heartbeat_lock(con, session_key), "Fornying av lås")
      if (isFALSE(alive)) {
        d <- isolate(draft())
        if (!is.null(d)) acquire_lock(d, isolate(context()), isolate(user()))
      }
    })

    session$onSessionEnded(function() {
      db_try(function(con) ds_release_lock(con, session_key), "Frigjøring av lås")
    })

    # Output: proposal cards -----------------------------------------------

    chip <- function(id, name, note = NULL) {
      if (is.null(tagger)) member_chip(id, name, note = note) else tagger$chip(id, name, note)
    }

    proposal_card <- function(prop, ctx, ev = NULL) {
      all <- members_in_context(ctx$group, NULL)
      parts <- if (is.null(ev)) NULL else event_participants(ev, ctx$group)
      coming <- if (is.null(parts)) character() else parts$member_id[parts$status == "accepted"]
      lay <- proposal_layout(prop$labels, prop$assignments, coming)
      name_of <- function(ids) {
        n <- all$display_name[match(ids, all$id)]
        ifelse(is.na(n), "Tidligere medlem", n)
      }
      chips <- function(ids) {
        if (length(ids) == 0) return(p(class = "sn-hint sn-empty-group", "Ingen her"))
        o <- order(tolower(name_of(ids)))
        ids <- ids[o]
        div(class = "sn-chips", lapply(ids, function(id) {
          note <- if (is.null(parts)) NULL else response_note(parts$status[match(id, parts$member_id)])
          if (id %in% all$id) chip(id, name_of(id), note) else member_chip(id, "Tidligere medlem")
        }))
      }
      l <- lock_on(prop$id)
      div(
        class = "sn-proposal",
        div(class = "sn-proposal-head",
            div(strong(class = "sn-proposal-name", prop$name),
                div(class = "sn-hint", paste("Laget av", editor_name(prop$created_by)))),
            div(class = "sn-tags",
                if (!is.null(l)) span(class = "sn-tag sn-tag-lock", paste("✎ Redigeres av", editor_name(l$editor))),
                span(class = paste0("sn-tag sn-status-pill sn-status-", prop$status), proposal_status_label(prop$status)))),
        div(class = "sn-groups", lapply(lay$groups, function(g) {
          div(class = "sn-group",
              div(class = "sn-group-head", span(g$label), span(class = "sn-col-count", length(g$ids))),
              chips(g$ids))
        })),
        if (length(lay$unplaced)) {
          div(class = "sn-unplaced",
              span(class = "sn-hint", paste0("Kommer, men ikke fordelt (", length(lay$unplaced), "):")),
              chips(lay$unplaced))
        },
        if (proposal_editable(prop$status)) {
          div(class = "sn-proposal-actions",
              tags$button(type = "button", class = "btn btn-sm btn-outline-primary",
                          `data-sn-input` = ns("edit"), `data-sn-value` = prop$id, `data-sn-busy` = "Åpner",
                          "Rediger grupper"))
        }
      )
    }

    lock_banners <- function(l) {
      if (is.null(l) || nrow(l) == 0) return(NULL)
      lapply(seq_len(nrow(l)), function(i) {
        div(class = "sn-lock-banner", role = "status",
            paste0("✎ ", editor_name(l$editor[i]), " lager et nytt forslag her nå (startet kl. ",
                   clock_time(l$started_at[i]), ")"))
      })
    }

    output$event_cards <- renderUI({
      ev <- event()
      req(ev)
      ctx <- context()
      props <- Filter(function(p) identical(p$event_id, ev$id), proposals())
      past <- event_is_past(ev, now())
      tagList(
        h3(class = "sn-section-title", "Grupper"),
        if (!is.null(load_error())) div(class = "sn-alert", role = "alert", load_error()),
        lock_banners(new_locks(event_id = ev$id)),
        if (length(props) == 0) {
          p(class = "sn-hint", if (past) "Det ble ikke laget noen gruppeinndeling for dette arrangementet."
                               else "Ingen gruppeforslag ennå.")
        },
        lapply(props, proposal_card, ctx = ctx, ev = ev),
        if (!past) {
          tags$button(type = "button", class = "btn btn-primary btn-sm sn-new",
                      `data-sn-input` = ns("new"), `data-sn-value` = "event", `data-sn-busy` = "Åpner",
                      "+ Lag gruppeforslag")
        } else {
          p(class = "sn-hint", "Arrangementet er gjennomført. Gruppene vises som historikk.")
        }
      )
    })

    drafts <- reactive(Filter(function(p) is.null(p$event_id), proposals()))

    output$draft_cards <- renderUI({
      ctx <- context()
      req(ctx)
      tagList(
        p(class = "sn-hint", "Skisser til nye undergrupper. Dette er bare forslag. De gjelder først når undergruppene opprettes eller endres i Spond."),
        if (!is.null(load_error())) div(class = "sn-alert", role = "alert", load_error()),
        lock_banners(new_locks(subgroup_id = ctx$subgroup_id)),
        lapply(drafts(), proposal_card, ctx = ctx),
        tags$button(type = "button", class = "btn btn-outline-primary btn-sm sn-new",
                    `data-sn-input` = ns("new"), `data-sn-value` = "draft", `data-sn-busy` = "Åpner",
                    "+ Lag gruppeutkast")
      )
    })

    # Output: editor --------------------------------------------------------

    output$editor <- renderUI({
      open_count()
      d <- isolate(draft())
      req(d)
      ctx <- isolate(context())
      bslib::card(
        bslib::card_header(
          class = "sn-card-head",
          div(class = "sn-crumbs", context_title(ctx)),
          h2(class = "sn-title", if (is.null(d$id)) "Nytt gruppeforslag" else "Rediger grupper"),
          div(class = "sn-hint",
              if (!is.null(d$event)) paste0("For ", d$event$heading, ", ", event_when(d$event$start, d$event$end))
              else paste0("Gruppeutkast i ", ctx$label, ". Må opprettes i Spond for å gjelde."))
        ),
        bslib::card_body(
          textInput(ns("name"), "Navn på forslaget", value = d$name, width = "100%"),
          p(class = "sn-own-edit", "✎ Andre trenere ser at du redigerer nå."),
          p(class = "sn-hint sn-board-hint",
            span(class = "sn-hint-drag", "Dra et kort til en gruppe, eller trykk på kortet og så «Plasser her»."),
            span(class = "sn-hint-tap", "Trykk på en spiller, og velg gruppe i feltet nederst.")),
          uiOutput(ns("board")),
          uiOutput(ns("editor_msg")),
          div(class = "sn-editor-actions",
              tags$button(type = "button", class = "btn btn-primary", `data-sn-input` = ns("save"),
                          `data-sn-value` = "x", `data-sn-busy` = "Lagrer", "Lagre"),
              tags$button(type = "button", class = "btn btn-outline-secondary", `data-sn-input` = ns("cancel"),
                          `data-sn-value` = "x", "Avbryt"))
        )
      )
    })

    output$editor_msg <- renderUI({
      m <- editor_msg()
      if (is.null(m)) NULL else div(class = "sn-alert", role = "alert", m)
    })

    output$board <- renderUI({
      board_version()
      d <- isolate(draft())
      req(d)
      el <- isolate(eligible())
      tag_table <- if (is.null(tagger)) tags_empty() else isolate(tagger$table())
      card <- function(i) {
        div(class = "sn-mcard", tabindex = "0", role = "button", `data-member` = el$member_id[i],
            `data-sort` = tolower(el$display_name[i]), `aria-label` = el$display_name[i],
            span(class = "sn-mcard-name", el$display_name[i]),
            span(class = "sn-mcard-tags",
                 lapply(tags_for(tag_table, el$member_id[i]), function(t) span(class = "sn-minitag", t)),
                 if (!is.null(response_note(el$status[i]))) span(class = "sn-note", response_note(el$status[i]))))
      }
      column <- function(label, title, ids, pool = FALSE) {
        idx <- which(el$member_id %in% ids)
        div(class = paste("sn-col", if (pool) "sn-col-pool"), `data-group` = label,
            div(class = "sn-col-head",
                if (pool) span(class = "sn-col-title", title)
                else tags$input(type = "text", class = "sn-col-name", value = label, maxlength = "60",
                                `aria-label` = "Gruppenavn", `data-sn-rename` = ns("rename"), `data-label` = label),
                span(class = "sn-col-count", length(idx)),
                if (!pool && length(d$labels) > 1) {
                  tags$button(type = "button", class = "sn-col-remove", `aria-label` = paste("Fjern", label),
                              title = "Fjern gruppen (spillerne flyttes tilbake)",
                              `data-sn-input` = ns("remove_group"), `data-sn-value` = label, "×")
                },
                tags$button(type = "button", class = "btn btn-sm btn-primary sn-place", "Plasser her")),
            div(class = paste("sn-col-body", if (length(idx) == 0) "is-empty"),
                lapply(idx, card),
                div(class = "sn-col-empty", if (pool) "Alle er fordelt" else "Slipp spillere her")))
      }
      placed <- names(d$assignments)
      div(
        class = "sn-board", `data-sn-move` = ns("move"),
        div(class = "sn-armed-banner", role = "status",
            span(class = "sn-armed-text"),
            tags$button(type = "button", class = "btn btn-sm btn-link sn-disarm", "Avbryt"),
            div(class = "sn-armed-targets")),
        div(
          class = "sn-board-cols",
          column("", if (is.null(d$event)) "Ikke fordelt" else "Kommer, ikke fordelt",
                 setdiff(el$member_id, placed), pool = TRUE),
          lapply(d$labels, function(l) column(l, l, names(d$assignments)[d$assignments == l])),
          div(class = "sn-col sn-col-add",
              tags$input(id = ns("new_group_name"), type = "text", class = "form-control form-control-sm",
                         placeholder = "Navn (valgfritt)", maxlength = "60", `aria-label` = "Navn på ny gruppe",
                         `data-sn-submit` = ns("add_group")),
              tags$button(type = "button", class = "btn btn-sm btn-outline-primary",
                          `data-sn-submit-for` = ns("new_group_name"), `data-sn-submit` = ns("add_group"),
                          "+ Ny gruppe"))
        )
      )
    })

    list(
      editing = reactive(is_editing()),
      n_drafts = reactive(length(drafts())),
      event_badge = function(event_id) {
        n <- sum(vapply(proposals(), function(p) identical(p$event_id, event_id), logical(1)))
        l <- locks()
        locked <- !is.null(l) && any(!is.na(l$event_id) & l$event_id == event_id)
        tagList(
          if (n > 0) span(class = "sn-tag sn-tag-groups", if (n == 1) "1 gruppeforslag" else paste(n, "gruppeforslag")),
          if (locked) span(class = "sn-tag sn-tag-lock", "✎ Redigeres")
        )
      }
    )
  })
}
