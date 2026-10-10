#' Treningsopplegg on an event (phase T2-3)
#'
#' Shown on every event that is not a match. Upcoming events can get a plan
#' put together from the exercise bank ("Lag opplegg") and changed later
#' ("Endre"); every save is a new version. Any version can be downloaded as
#' PDF. Past events only show what was made.
#'
#' The PDF gets the groups with player names from the event's approved
#' group proposal, if there is one. The names are only put into the PDF and
#' never stored in the plan.
#'
#' KI ("Lag med KI"), comments and approval come in T3
#' (claude/plan-treningsopplegg.md).
#'
#' @param context Reactive from `mod_teams_server()`.
#' @param user Reactive from `mod_login_server()`.
#' @param db Database handle from `db_handle()`.
#' @param event Reactive with the open event (from `event_minimal()`), or NULL.
#' @param now Function giving the current time (tests pass a fixed time).
#' @param make_pdf Function making the PDF (tests pass a fake).
#' @noRd
mod_plans_event_ui <- function(id) {
  ns <- NS(id)
  div(class = "sn-plan", uiOutput(ns("section")))
}

# "Kjetil G." for a profile id, or "en trener" if it is not in the group.
plan_author_name <- function(profile_id, group) {
  m <- group$members
  hit <- which(!is.na(m$profile_id) & m$profile_id == profile_id)
  if (length(hit) == 0) return("en trener")
  short_name(m$first_name[hit[1]], m$last_name[hit[1]])
}

plan_version_line <- function(v, n, author, t) {
  paste0("Versjon ", v, if (n > 1) paste0(" av ", n), " · laget av ", author, " · ", short_time(t))
}

# The exercises of a plan as a short list, with the schedule's times.
plan_summary_ui <- function(p) {
  sched <- plan_schedule(p)
  rot <- p$tidsplan$stasjoner$grupper > 1
  tagList(
    p(class = "sn-hint",
      paste0(sched$total, " min · ",
             if (rot) paste(length(p$ovelser), "stasjoner med rotasjon") else
               paste(length(p$ovelser), if (length(p$ovelser) == 1) "øvelse" else "øvelser"),
             if (nzchar(p$tema)) paste0(" · Tema: ", p$tema))),
    tags$ol(class = "sn-plan-list", lapply(p$ovelser, function(e) {
      tags$li(span(class = "sn-plan-ex", e$navn),
              if (nzchar(e$kategori)) span(class = "sn-hint", paste0(" · ", e$kategori)),
              if (is.null(e$tegning)) span(class = "sn-hint", " · uten tegning"))
    }))
  )
}

# Choices for the exercise picker: the bank, plus exercises kept from the
# plan being changed that are not in the bank. Values are exercise codes.
plan_exercise_choices <- function(bank, kept = list()) {
  cat_label <- function(k) if (k %in% names(exercise_categories)) exercise_categories[[k]] else k
  bank_lab <- if (nrow(bank)) paste0(bank$name, " · ", vapply(bank$category, cat_label, "")) else character()
  extra <- Filter(function(e) !e$kode %in% bank$code, kept)
  stats::setNames(c(bank$code, vapply(extra, `[[`, "", "kode")),
                  c(bank_lab, vapply(extra, function(e) paste0(e$navn, " · fra opplegget"), "")))
}

plan_editor_modal <- function(ns, s, choices, selected, minutes_hint = NULL) {
  num <- function(id, label, value, min, max) numericInput(ns(id), label, value = value, min = min, max = max, width = "100%")
  m <- modalDialog(
    title = "Treningsopplegg",
    size = "l", easyClose = FALSE,
    footer = tagList(uiOutput(ns("ed_msg")),
                     actionButton(ns("ed_save"), "Lagre", class = "btn-primary"),
                     modalButton("Avbryt")),
    div(class = "sn-admin-pane",
        div(class = "sn-admin-grid",
            textInput(ns("ed_tittel"), "Tittel", value = s$tittel, width = "100%"),
            textInput(ns("ed_tema"), "Tema", value = s$tema, width = "100%")),
        selectizeInput(ns("ed_ovelser"), "Øvelser, i rekkefølge", choices = choices, selected = selected,
                       multiple = TRUE, width = "100%",
                       options = list(placeholder = "Velg øvelser fra banken", plugins = list("remove_button"))),
        if (length(choices) == 0) {
          p(class = "sn-hint", "Banken er tom. Superadmin legger inn øvelser under tannhjulet → Øvelser.")
        },
        radioButtons(ns("ed_rotasjon"), NULL, inline = TRUE, selected = if (isTRUE(s$rotasjon)) "ja" else "nei",
                     choices = c("Stasjoner med rotasjon (én gruppe per øvelse)" = "ja",
                                 "Alle sammen, én øvelse om gangen" = "nei")),
        div(class = "sn-admin-grid sn-admin-grid-4",
            num("ed_oppvarming", "Oppvarming (min)", s$oppvarming, 0, 60),
            num("ed_stasjon", "Per øvelse (min)", s$stasjon, 1, 90),
            num("ed_bytte", "Bytte (min)", s$bytte, 0, 10),
            num("ed_avslutning", "Avslutning (min)", s$avslutning, 0, 60)),
        uiOutput(ns("ed_summary")),
        if (!is.null(minutes_hint)) p(class = "sn-hint", minutes_hint),
        div(class = "sn-admin-grid",
            textInput(ns("ed_oppvarming_tekst"), "Oppvarming", value = s$oppvarming_tekst, width = "100%"),
            textInput(ns("ed_avslutning_tekst"), "Avslutning", value = s$avslutning_tekst, width = "100%")),
        tags$details(
          class = "sn-plan-more",
          tags$summary("Fokus, stikkord og spørsmål (valgfritt)"),
          div(class = "sn-admin-grid",
              textAreaInput(ns("ed_forsvar"), "Fokus i forsvar", value = s$forsvar, rows = 2, width = "100%"),
              textAreaInput(ns("ed_angrep"), "Fokus i angrep", value = s$angrep, rows = 2, width = "100%")),
          lapply(1:3, function(i) {
            k <- if (length(s$stikkord) >= i) s$stikkord[[i]] else list(tittel = "", tekst = "")
            div(class = "sn-admin-grid",
                textInput(ns(paste0("ed_stikkord_", i)), paste("Stikkord", i), value = k$tittel, width = "100%"),
                textInput(ns(paste0("ed_stikkord_tekst_", i)), "Forklaring", value = k$tekst, width = "100%"))
          }),
          textAreaInput(ns("ed_sporsmal"), "Spørsmål til felles avslutning (ett per linje)", value = s$sporsmal,
                        rows = 3, width = "100%"),
          textAreaInput(ns("ed_merknad"), "Merknad til tidsplanen", value = s$merknad, rows = 2, width = "100%")
        ),
        p(class = "sn-hint", "Ikke skriv helseopplysninger eller navn på spillere. Navn fra godkjent",
          " gruppeforslag settes inn i PDF-en og lagres ikke."))
  )
  htmltools::tagQuery(m)$find(".modal-dialog")$addClass("modal-fullscreen-sm-down sn-admin-modal")$allTags()
}

#' @noRd
mod_plans_server <- function(id, context, user, db, event, now = Sys.time, make_pdf = plan_pdf) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh <- reactiveVal(0)
    msg <- reactiveVal(NULL)       # list(type, text) under the plan
    ed_msg <- reactiveVal(NULL)    # message in the editor
    chosen <- reactiveVal(NULL)    # plan id picked in the version list, NULL = latest
    editor <- reactiveVal(NULL)    # list(bank, kept) while the editor is open

    applies <- reactive({
      ev <- event()
      !is.null(ev) && !isTRUE(ev$match) && !is.null(context())
    })
    upcoming <- reactive({
      ev <- event()
      end <- if (is.na(ev$end)) ev$start else ev$end
      isTRUE(end > now())
    })
    observeEvent(event(), {
      chosen(NULL)
      msg(NULL)
    }, ignoreNULL = FALSE)

    read <- function(f) {
      tryCatch(db$run(f), error = function(e) {
        message("Lesing av treningsopplegg feilet: ", conditionMessage(e))
        msg(list(type = "error", text = "Fikk ikke hentet treningsopplegget. Prøv igjen om litt."))
        NULL
      })
    }

    versions <- reactive({
      req(applies())
      refresh()
      ev <- event()
      gid <- context()$group_id
      acc <- isolate(user())$access
      read(function(con) ds_plan_versions(con, acc, gid, ev$id))
    })

    current <- reactive({
      v <- versions()
      if (is.null(v) || nrow(v) == 0) return(NULL)
      id <- chosen()
      if (is.null(id) || !id %in% v$id) id <- v$id[1]
      acc <- isolate(user())$access
      read(function(con) ds_get_plan(con, acc, id))
    })

    output$section <- renderUI({
      if (!applies()) return(NULL)
      v <- versions()
      cur <- current()
      m <- msg()
      alert <- if (!is.null(m)) {
        div(class = paste("alert sn-alert", if (identical(m$type, "ok")) "alert-success" else "alert-danger"),
            role = if (identical(m$type, "ok")) "status" else "alert", m$text)
      }
      head <- h3(class = "sn-section-title", "Treningsopplegg")
      if (is.null(v)) return(tagList(head, alert))
      if (nrow(v) == 0 || is.null(cur)) {
        return(tagList(
          head, alert,
          if (upcoming()) {
            div(class = "sn-plan-empty",
                p(class = "sn-hint", "Ingen opplegg ennå. Sett sammen øvelser fra banken."),
                actionButton(ns("new"), "Lag opplegg", class = "btn-primary btn-sm"))
          } else {
            p(class = "sn-hint", "Ingen opplegg for denne treningen.")
          }
        ))
      }
      group <- context()$group
      tagList(
        head, alert,
        div(class = "sn-plan-card",
            div(class = "sn-plan-title", cur$plan$tittel),
            div(class = "sn-hint", plan_version_line(cur$version, nrow(v), plan_author_name(cur$created_by, group),
                                                     cur$created_at)),
            plan_summary_ui(cur$plan),
            div(class = "sn-plan-actions",
                downloadButton(ns("pdf"), "Last ned PDF", class = "btn-sm btn-primary"),
                if (upcoming()) actionButton(ns("edit"), "Endre", class = "btn-sm btn-outline-primary"),
                if (nrow(v) > 1) {
                  htmltools::tagAppendAttributes(
                    selectInput(ns("version"), NULL, selectize = FALSE, width = "auto", selected = cur$id,
                                choices = stats::setNames(v$id, paste("Versjon", v$version))),
                    `aria-label` = "Velg versjon", .cssSelector = "select")
                }))
      )
    })
    # The section is drawn inside the event card, which Shiny may think is
    # hidden at first; draw it anyway.
    outputOptions(output, "section", suspendWhenHidden = FALSE)

    observeEvent(input$version, {
      v <- versions()
      id <- suppressWarnings(as.integer(input$version))
      if (!is.null(v) && length(id) == 1 && id %in% v$id) chosen(id)
    })

    # Editor ------------------------------------------------------------------------
    open_editor <- function(s, kept) {
      ctx <- context()
      acc <- user()$access
      bank <- read(function(con) ds_list_exercises(con, acc, ctx$group_id))
      if (is.null(bank)) return()
      team <- read(function(con) ds_get_team_settings(con, acc, ctx$group_id))
      hint <- if (!is.null(team) && nzchar(team$session_minutes)) paste0("Lagets standard er ", team$session_minutes, " min.")
      editor(list(bank = bank, kept = kept, n_groups = length(print_groups())))
      ed_msg(NULL)
      selected <- vapply(kept, `[[`, "", "kode")
      showModal(plan_editor_modal(ns, s, plan_exercise_choices(bank, kept), selected, hint))
    }

    observeEvent(input$new, {
      if (!applies() || !upcoming()) return()
      ev <- event()
      acc <- user()$access
      theme <- read(function(con) ds_season_theme(con, acc, context()$group_id, as.Date(ev$start, tz = "Europe/Oslo")))
      open_editor(plan_settings_default(theme$theme %||% ""), list())
    })

    observeEvent(input$edit, {
      cur <- current()
      if (is.null(cur) || !upcoming()) return()
      open_editor(plan_settings_from_plan(cur$plan), cur$plan$ovelser)
    })

    editor_settings <- function() {
      int <- function(x, default) {
        v <- suppressWarnings(as.numeric(x))
        if (length(v) != 1 || is.na(v)) default else v
      }
      list(tittel = input$ed_tittel %||% "", tema = input$ed_tema %||% "", undertittel = "",
           forsvar = input$ed_forsvar %||% "", angrep = input$ed_angrep %||% "",
           stikkord = lapply(1:3, function(i) list(tittel = input[[paste0("ed_stikkord_", i)]] %||% "",
                                                   tekst = input[[paste0("ed_stikkord_tekst_", i)]] %||% "")),
           oppvarming = int(input$ed_oppvarming, 0), oppvarming_tekst = input$ed_oppvarming_tekst %||% "",
           stasjon = int(input$ed_stasjon, NA), bytte = int(input$ed_bytte, 0),
           rotasjon = identical(input$ed_rotasjon, "ja"),
           avslutning = int(input$ed_avslutning, 0), avslutning_tekst = input$ed_avslutning_tekst %||% "",
           sporsmal = input$ed_sporsmal %||% "", merknad = input$ed_merknad %||% "")
    }

    # The plan as the editor would save it: kept copies for exercises that
    # were in the plan, fresh copies from the bank for the others.
    editor_plan <- function() {
      ed <- editor()
      codes <- as.character(input$ed_ovelser %||% character())
      kept_codes <- vapply(ed$kept, `[[`, "", "kode")
      ex <- lapply(codes, function(code) {
        k <- match(code, kept_codes)
        if (!is.na(k)) return(ed$kept[[k]])
        row <- ed$bank[ed$bank$code == code, , drop = FALSE]
        if (nrow(row) == 1) plan_exercise_from_bank(row)
      })
      s <- editor_settings()
      if (text_is_sensitive(c(unlist(s[c("tittel", "tema", "forsvar", "angrep", "sporsmal", "merknad",
                                         "oppvarming_tekst", "avslutning_tekst")]), unlist(s$stikkord)))) {
        stop("Opplegget skal ikke inneholde helseopplysninger eller andre sensitive opplysninger.", call. = FALSE)
      }
      plan_build(Filter(Negate(is.null), ex), s)
    }

    output$ed_summary <- renderUI({
      req(editor())
      pl <- tryCatch(editor_plan(), error = function(e) e)
      if (inherits(pl, "error")) return(p(class = "sn-hint", conditionMessage(pl)))
      sched <- plan_schedule(pl)
      g <- pl$tidsplan$stasjoner$grupper
      n_prop <- editor()$n_groups
      tagList(
        div(class = "sn-plan-sched",
            strong(paste0("Totalt ", sched$total, " min")), ": ",
            paste(vapply(sched$rows, function(r) paste0(r$tid, " ", if (r$felles) r$tekst else paste(unlist(r$celler), collapse = " / ")), ""),
                  collapse = " · ")),
        # The rotation needs one group per station; say so if the approved
        # group proposal has another number of groups.
        if (g > 1 && n_prop > 0 && n_prop != g) {
          p(class = "sn-hint sn-plan-warn",
            paste0("Godkjent gruppeforslag har ", n_prop, " grupper, men rotasjonen har ", g,
                   ". Velg ", n_prop, " øvelser, eller endre gruppeforslaget."))
        }
      )
    })
    output$ed_msg <- renderUI({
      m <- ed_msg()
      if (is.null(m)) NULL else div(class = "alert alert-danger sn-alert sn-plan-edmsg", role = "alert", m)
    })

    observeEvent(input$ed_save, {
      if (is.null(editor()) || !applies() || !upcoming()) return()
      p <- tryCatch(editor_plan(), error = function(e) e)
      if (inherits(p, "error")) {
        ed_msg(conditionMessage(p))
        return()
      }
      ev <- event()
      u <- user()
      gid <- context()$group_id
      res <- tryCatch(db$run(function(con) ds_save_plan(con, u$access, gid, ev$id, p, u$profile$id)),
                      error = function(e) {
                        message("Lagring av treningsopplegg feilet: ", conditionMessage(e))
                        NULL
                      })
      if (is.null(res)) {
        ed_msg("Fikk ikke lagret. Prøv igjen om litt.")
        return()
      }
      editor(NULL)
      removeModal()
      chosen(res$id)
      msg(list(type = "ok", text = paste0("Opplegget er lagret som versjon ", res$version, ".")))
      refresh(refresh() + 1)
    })

    # PDF -----------------------------------------------------------------------------
    # Groups with names from the event's approved group proposal, or none.
    print_groups <- function() {
      ev <- event()
      ctx <- context()
      acc <- user()$access
      tryCatch(db$run(function(con) {
        props <- ds_list_proposals(con, acc, ctx$group_id, event_ids = ev$id)
        ok <- props[props$status == "approved" & props$spond_event_id %in% ev$id, , drop = FALSE]
        if (nrow(ok) == 0) return(list())
        g <- ds_proposal_groups(con, acc, ctx$group_id, ok$id[1])
        people <- members_in_context(ctx$group)
        plan_print_groups(g$labels, g$members, people)
      }), error = function(e) {
        message("Henting av grupper til PDF feilet: ", conditionMessage(e))
        list()
      })
    }

    output$pdf <- downloadHandler(
      filename = function() {
        ev <- event()
        paste0("treningsopplegg-", format(ev$start, "%Y-%m-%d", tz = "Europe/Oslo"), ".pdf")
      },
      content = function(file) {
        cur <- current()
        ev <- event()
        ctx <- context()
        res <- tryCatch(
          make_pdf(cur$plan, file, footer = paste(c(ctx$label, ev$heading), collapse = " · "),
                   info = c(event_when(ev$start, ev$end), ctx$label), groups = print_groups()),
          error = function(e) e)
        if (inherits(res, "error")) {
          message("PDF av treningsopplegg feilet: ", conditionMessage(res))
          msg(list(type = "error", text = "Fikk ikke laget PDF-en. Prøv igjen om litt."))
          stop("Fikk ikke laget PDF-en.", call. = FALSE)
        }
      }
    )

    invisible(list(versions = versions, current = current))
  })
}
