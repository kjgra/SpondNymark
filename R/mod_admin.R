#' Admin panel (the gear in the top bar)
#'
#' Phases T1 and T2b of the training plans (claude/plan-treningsopplegg.md,
#' kap. 12): the superadmin and admins see the gear. The superadmin is the
#' Spond profile in SPONDNYMARK_SUPERADMIN (not in git or the database, so
#' nobody can be locked out); admins are set in the panel (`app_roles`, see
#' R/fct_roles.R).
#'
#' The panel is a dialog for the chosen main group with four tabs: the
#' trainers (KI and admin rights), the season plan (årshjul) per year, the
#' team settings and the exercises.
#'
#' Every save checks again on the server that the user is an admin (and the
#' superadmin for admin rights), and the data layer checks access to the
#' group.
#'
#' @param context Reactive from `mod_teams_server()`.
#' @param user Reactive from `mod_login_server()`.
#' @param db Database handle from `db_handle()`.
#' @param rights Reactive with the user's rights (`rights_reactive()`); made
#'   here from `superadmin` if NULL.
#' @param superadmin Spond profile id of the superadmin.
#' @param today Function giving today's date (tests pass a fixed date).
#' @noRd
mod_admin_bar_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("bar"), class = "sn-admin-slot")
}

#' TRUE if `profile_id` is the superadmin. An empty setting means nobody is.
#' @noRd
is_superadmin <- function(profile_id, superadmin = Sys.getenv("SPONDNYMARK_SUPERADMIN")) {
  superadmin <- trimws(superadmin %||% "")
  nzchar(superadmin) && length(profile_id) == 1 && !is.na(profile_id) && identical(as.character(profile_id), superadmin)
}

admin_button <- function(ns) {
  tags$button(
    type = "button", class = "btn sn-admin", `aria-label` = "Innstillinger", title = "Innstillinger",
    onclick = sprintf("Shiny.setInputValue('%s', Date.now(), {priority: 'event'})", ns("open")),
    HTML(paste0(
      '<svg viewBox="0 0 24 24" width="22" height="22" aria-hidden="true" fill="none" stroke="currentColor" ',
      'stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><circle cx="12" cy="12" r="3"></circle>',
      '<path d="M19.4 15a1.65 1.65 0 0 0 .33 1.82l.06.06a2 2 0 1 1-2.83 2.83l-.06-.06a1.65 1.65 0 0 0-1.82-.33 ',
      '1.65 1.65 0 0 0-1 1.51V21a2 2 0 1 1-4 0v-.09A1.65 1.65 0 0 0 9 19.4a1.65 1.65 0 0 0-1.82.33l-.06.06a2 2 0 ',
      '1 1-2.83-2.83l.06-.06A1.65 1.65 0 0 0 4.68 15a1.65 1.65 0 0 0-1.51-1H3a2 2 0 1 1 0-4h.09A1.65 1.65 0 0 0 ',
      '4.6 9a1.65 1.65 0 0 0-.33-1.82l-.06-.06a2 2 0 1 1 2.83-2.83l.06.06A1.65 1.65 0 0 0 9 4.68a1.65 1.65 0 0 0 ',
      '1-1.51V3a2 2 0 1 1 4 0v.09a1.65 1.65 0 0 0 1 1.51 1.65 1.65 0 0 0 1.82-.33l.06-.06a2 2 0 1 1 2.83 2.83l-.06.06',
      'A1.65 1.65 0 0 0 19.4 9a1.65 1.65 0 0 0 1.51 1H21a2 2 0 1 1 0 4h-.09a1.65 1.65 0 0 0-1.51 1z"></path></svg>'
    ))
  )
}

admin_alert <- function(m) {
  if (is.null(m)) return(NULL)
  div(class = paste("alert sn-alert", if (identical(m$type, "ok")) "alert-success" else "alert-danger"),
      role = if (identical(m$type, "ok")) "status" else "alert", m$text)
}

admin_saved_line <- function(t) {
  if (length(t) == 0 || all(is.na(t))) return(NULL)
  p(class = "sn-hint", paste("Sist lagret", short_time(max(t, na.rm = TRUE))))
}

# The dialog. On phones it fills the screen.
# `groups`: named vector of main group ids (names are the group names).
admin_modal <- function(ns, groups, selected, years, year) {
  m <- modalDialog(
    title = tagList("Innstillinger",
                    if (length(groups) > 1) {
                      div(class = "sn-admin-group-pick",
                          htmltools::tagAppendAttributes(
                            selectInput(ns("admin_group"), NULL, choices = groups, selected = selected, selectize = FALSE,
                                        width = "auto"),
                            `aria-label` = "Lag", .cssSelector = "select"))
                    } else {
                      span(class = "sn-admin-group", names(groups)[1])
                    }),
    size = "xl", easyClose = TRUE, footer = modalButton("Lukk"),
    bslib::navset_underline(
      id = ns("tab"),
      bslib::nav_panel(
        "Trenere", value = "trainers",
        div(class = "sn-admin-pane",
            p(class = "sn-hint", "Trenere med profil i Spond. App gir tilgang til å logge inn i appen; ingen andre",
              " kommer inn. KI gir tilgang til å lage opplegg med KI. Admin gir tannhjulet og alltid app-tilgang.",
              " Bare superadmin kan gi admin."),
            uiOutput(ns("roles_msg")),
            uiOutput(ns("roles_body")))
      ),
      bslib::nav_panel(
        "Årshjul", value = "season",
        div(class = "sn-admin-pane",
            p(class = "sn-hint", "Ett hovedtema per måned. Temaet foreslås når et treningsopplegg lages,",
              " og kan justeres for hver trening."),
            div(class = "sn-admin-year",
                selectInput(ns("year"), "År", choices = years, selected = year, selectize = FALSE, width = "120px")),
            uiOutput(ns("season_form")),
            uiOutput(ns("season_msg")),
            actionButton(ns("season_save"), "Lagre årshjulet", class = "btn-primary"))
      ),
      bslib::nav_panel(
        "Lagets standard", value = "team",
        div(class = "sn-admin-pane",
            p(class = "sn-hint", "Utgangspunktet for hvert treningsopplegg. Kan endres for hver trening."),
            uiOutput(ns("team_form")),
            uiOutput(ns("team_msg")),
            actionButton(ns("team_save"), "Lagre", class = "btn-primary"))
      ),
      bslib::nav_panel(
        "Øvelser", value = "exercises",
        div(class = "sn-admin-pane",
            uiOutput(ns("ex_msg")),
            uiOutput(ns("ex_body")))
      )
    )
  )
  htmltools::tagQuery(m)$find(".modal-dialog")$addClass("modal-fullscreen-sm-down sn-admin-modal")$allTags()
}

season_form <- function(ns, rows) {
  div(
    class = "sn-season",
    lapply(1:12, function(i) {
      hit <- rows[rows$month == i, , drop = FALSE]
      div(
        class = "sn-season-row",
        div(class = "sn-season-month", month_names_nb[i]),
        htmltools::tagAppendAttributes(
          textInput(ns(paste0("theme_", i)), NULL, value = if (nrow(hit)) hit$theme[1] else "",
                    placeholder = "Tema", width = "100%"),
          `aria-label` = paste("Tema for", tolower(month_names_nb[i])), maxlength = 80, .cssSelector = "input"
        ),
        htmltools::tagAppendAttributes(
          textInput(ns(paste0("desc_", i)), NULL, value = if (nrow(hit)) hit$description[1] else "",
                    placeholder = "Kort beskrivelse (valgfri)", width = "100%"),
          `aria-label` = paste("Beskrivelse for", tolower(month_names_nb[i])), maxlength = 500, .cssSelector = "input"
        )
      )
    })
  )
}

team_form <- function(ns, s) {
  minutes <- suppressWarnings(as.integer(s$session_minutes))
  tagList(
    div(class = "sn-admin-grid",
        textInput(ns("age_group"), "Lag eller årsklasse", value = s$age_group, placeholder = "f.eks. G10"),
        numericInput(ns("session_minutes"), "Øktlengde (minutter)", value = if (is.na(minutes)) "" else minutes,
                     min = 15, max = 240, step = 5)),
    textInput(ns("pitch"), "Bane", value = s$pitch, placeholder = "f.eks. halv 7er-bane, kunstgress", width = "100%"),
    textAreaInput(ns("equipment"), "Utstyr", value = s$equipment, rows = 2, width = "100%",
                  placeholder = "f.eks. 30 kjegler, 4 småmål, vester i to farger, 15 baller"),
    textAreaInput(ns("principles"), "Prinsipper og føringer", value = s$principles, rows = 3, width = "100%",
                  placeholder = "f.eks. 1/3-prinsippet, mye ball per spiller, spør før du forteller"),
    p(class = "sn-hint", "Ikke skriv helseopplysninger eller andre sensitive opplysninger."),
    admin_saved_line(s$updated_at)
  )
}

exercise_meta <- function(ex) {
  players <- if (!is.na(ex$min_players) && !is.na(ex$max_players)) paste0(ex$min_players, "–", ex$max_players, " spillere")
             else if (!is.na(ex$min_players)) paste0("minst ", ex$min_players, " spillere")
             else if (!is.na(ex$max_players)) paste0("maks ", ex$max_players, " spillere")
  parts <- c(players, if (!is.na(ex$duration_minutes)) paste(ex$duration_minutes, "min"),
             if (nzchar(ex$area)) ex$area, if (!is.na(ex$drawing)) "med tegning")
  paste(parts, collapse = " · ")
}

exercise_list <- function(ns, ex, confirm_id = NULL) {
  head <- div(class = "sn-ex-top",
              p(class = "sn-hint", if (nrow(ex)) paste(nrow(ex), if (nrow(ex) == 1) "øvelse" else "øvelser")
                else "Ingen øvelser ennå."),
              actionButton(ns("ex_new"), "+ Ny øvelse", class = "btn-primary"))
  if (nrow(ex) == 0) return(head)
  cats <- unique(ex$category)
  cats <- c(intersect(names(exercise_categories), cats), setdiff(cats, names(exercise_categories)))
  tagList(head, lapply(cats, function(cat) {
    rows <- which(ex$category == cat)
    div(class = "sn-ex-group",
        h3(class = "sn-ex-cat", if (cat %in% names(exercise_categories)) exercise_categories[[cat]] else cat),
        lapply(rows, function(i) {
          e <- ex[i, , drop = FALSE]
          id <- as.character(e$id)
          meta <- exercise_meta(e)
          div(class = "sn-ex-row",
              div(class = "sn-ex-main",
                  div(class = "sn-ex-name", e$name),
                  if (nzchar(meta)) div(class = "sn-ex-meta", meta),
                  if (length(e$themes[[1]])) div(class = "sn-ex-themes",
                                                   lapply(e$themes[[1]], function(t) span(class = "sn-tag", t)))),
              if (identical(confirm_id, id)) {
                div(class = "sn-ex-actions sn-ex-confirm",
                    span("Slette øvelsen?"),
                    tags$button(type = "button", class = "btn btn-sm btn-danger",
                                onclick = set_input_js(ns("ex_delete_ok"), id), "Slett"),
                    tags$button(type = "button", class = "btn btn-sm btn-outline-primary",
                                onclick = set_input_js(ns("ex_delete_cancel"), id), "Avbryt"))
              } else {
                div(class = "sn-ex-actions",
                    tags$button(type = "button", class = "btn btn-sm btn-outline-primary",
                                onclick = set_input_js(ns("ex_edit"), id), "Rediger"),
                    tags$button(type = "button", class = "btn btn-sm btn-outline-danger",
                                `aria-label` = paste("Slett", e$name),
                                onclick = set_input_js(ns("ex_delete"), id), "Slett"))
              })
        }))
  }))
}

exercise_form <- function(ns, e, theme_choices) {
  num <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) "" else x
  area <- function(id, label, value, rows = 3, placeholder = NULL) {
    textAreaInput(ns(id), label, value = value %||% "", rows = rows, width = "100%", placeholder = placeholder)
  }
  themes <- e$themes[[1]] %||% character()
  tagList(
    h3(class = "sn-ex-cat", if (is.null(e$id)) "Ny øvelse" else "Rediger øvelse"),
    div(class = "sn-admin-grid",
        textInput(ns("ex_name"), "Navn", value = e$name %||% "", placeholder = "f.eks. 3 mot 1 i to ruter"),
        selectInput(ns("ex_category"), "Kategori",
                    choices = c("Velg kategori" = "", stats::setNames(names(exercise_categories), exercise_categories)),
                    selected = e$category %||% "", selectize = FALSE)),
    selectizeInput(ns("ex_themes"), "Temaer", choices = sort(unique(c(theme_choices, themes))), selected = themes,
                   multiple = TRUE, width = "100%",
                   options = list(create = TRUE, placeholder = "Velg fra årshjulet eller skriv et nytt tema")),
    div(class = "sn-admin-grid sn-admin-grid-4",
        numericInput(ns("ex_min_players"), "Minst spillere", value = num(e$min_players), min = 1, max = 60),
        numericInput(ns("ex_max_players"), "Flest spillere", value = num(e$max_players), min = 1, max = 60),
        numericInput(ns("ex_duration_minutes"), "Tid (min)", value = num(e$duration_minutes), min = 1, max = 120),
        textInput(ns("ex_area"), "Areal", value = e$area %||% "", placeholder = "20 × 15 m")),
    area("ex_organisation", "Organisering", e$organisation, placeholder = "Bane, kjegler, lag og startposisjoner"),
    area("ex_execution", "Gjennomføring", e$execution, placeholder = "Hva skjer i øvelsen, og hvordan får man poeng"),
    area("ex_learning_points", "Læringsmomenter", e$learning_points, placeholder = "Ett per linje"),
    area("ex_questions", "Spørsmål til spillerne", e$questions, rows = 2),
    div(class = "sn-admin-grid",
        area("ex_easier", "Enklere", e$easier, rows = 2),
        area("ex_harder", "Vanskeligere", e$harder, rows = 2)),
    textInput(ns("ex_nff_url"), "Lenke til øvelsen hos NFF (valgfri)", value = e$nff_url %||% "",
              placeholder = "https://", width = "100%"),
    p(class = "sn-hint", "Beskriv øvelsen med egne ord. Ikke kopier tekst eller tegninger fra NFF."),
    div(class = "sn-ex-drawing",
        htmltools::tagAppendAttributes(
          textAreaInput(ns("ex_drawing"), "Tegning (JSON, valgfri)", value = drawing_pretty(e$drawing),
                        rows = 8, width = "100%", placeholder = "Lim inn tegning, eller trykk «Sett inn eksempel»"),
          spellcheck = "false", class = "sn-code", .cssSelector = "textarea"),
        div(class = "sn-ex-drawbtns",
            actionButton(ns("ex_preview"), "Vis tegning", class = "btn-sm btn-outline-primary"),
            actionButton(ns("ex_example"), "Sett inn eksempel", class = "btn-sm btn-link")),
        uiOutput(ns("ex_preview_box"))),
    uiOutput(ns("ex_form_msg")),
    div(class = "sn-ex-formbtns",
        actionButton(ns("ex_save"), "Lagre øvelsen", class = "btn-primary"),
        actionButton(ns("ex_cancel"), "Avbryt", class = "btn-outline-primary"))
  )
}

# A checkbox that sends list(p = profile id, f = "ai"/"admin", v = checked).
role_checkbox <- function(ns, pid, field, checked, enabled, label) {
  pid <- gsub("[^A-Za-z0-9_-]", "", pid)
  tags$input(type = "checkbox", class = "form-check-input", `aria-label` = label,
             checked = if (isTRUE(checked)) NA, disabled = if (!enabled) NA,
             onchange = sprintf("Shiny.setInputValue('%s', {p: '%s', f: '%s', v: this.checked}, {priority: 'event'})",
                                ns("role"), pid, field))
}

# The trainers with their rights. The superadmin's row cannot be changed;
# only the superadmin can tick "Admin".
role_rows <- function(ns, cand, roles, rights, me, superadmin, group) {
  tags$table(
    class = "table table-sm sn-roles",
    tags$thead(tags$tr(tags$th("Trener"), tags$th(class = "sn-roles-c", "App"), tags$th(class = "sn-roles-c", "KI"),
                       tags$th(class = "sn-roles-c", "Admin"))),
    tags$tbody(lapply(seq_len(nrow(cand)), function(i) {
      pid <- cand$profile_id[i]
      r <- roles[roles$spond_profile_id == pid, , drop = FALSE]
      sa <- is_superadmin(pid, superadmin)
      ai <- sa || isTRUE(r$can_use_ai[1])
      adm <- sa || isTRUE(r$is_admin[1])
      app <- adm || isTRUE(r$can_use_app[1])     # admins always have app access
      changed <- if (nrow(r) && !sa) {
        paste0("Endret av ", plan_author_name(r$updated_by[1], group), " ", short_time(r$updated_at[1]))
      }
      tags$tr(
        tags$td(div(class = "sn-roles-name", cand$name[i], if (sa) span(class = "sn-tag sn-tag-match", "Superadmin"),
                    if (identical(pid, me)) span(class = "sn-hint", " (deg)")),
                div(class = "sn-hint", paste(Filter(nzchar, c(cand$roles[i], changed)), collapse = " \u00b7 "))),
        tags$td(class = "sn-roles-c", role_checkbox(ns, pid, "app", app, !adm && isTRUE(rights$admin),
                                                    paste("App-tilgang for", cand$name[i]))),
        tags$td(class = "sn-roles-c", role_checkbox(ns, pid, "ai", ai, !sa && isTRUE(rights$admin), paste("KI for", cand$name[i]))),
        tags$td(class = "sn-roles-c", role_checkbox(ns, pid, "admin", adm, !sa && isTRUE(rights$superadmin),
                                                    paste("Admin for", cand$name[i])))
      )
    }))
  )
}

#' @noRd
mod_admin_server <- function(id, context, user, db, rights = NULL, superadmin = Sys.getenv("SPONDNYMARK_SUPERADMIN"),
                             today = Sys.Date) {
  if (is.null(rights)) rights <- rights_reactive(user, db, superadmin)
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh <- reactiveVal(0)
    msg <- reactiveVal(NULL)            # list(tab, type = "ok"/"error", text)
    editing <- reactiveVal(NULL)        # NULL = list, "new", or an exercise id
    confirm_delete <- reactiveVal(NULL) # exercise id waiting for confirmation

    allowed <- reactive(!is.null(user()) && isTRUE(rights()$admin))
    # The main group the panel works on. The gear is there right after login;
    # the panel starts on the team chosen in the app, or the first one, and
    # has its own picker.
    admin_gid <- reactiveVal(NULL)
    group_id <- reactive({
      gid <- admin_gid()
      u <- user()
      if (is.null(u) || is.null(gid) || !gid %in% u$access$id) NULL else gid
    })
    the_group <- reactive({
      gid <- group_id()
      if (is.null(gid)) return(NULL)
      ctx <- context()
      if (!is.null(ctx) && identical(ctx$group_id, gid)) ctx$group else user()$groups[[gid]]
    })
    this_year <- function() as.integer(format(today(), "%Y"))

    output$bar <- renderUI({
      if (!allowed() || nrow(user()$access) == 0) return(NULL)
      admin_button(ns)
    })
    # The slot starts empty; render it anyway so the gear can appear.
    outputOptions(output, "bar", suspendWhenHidden = FALSE)

    say <- function(tab, type, text) msg(list(tab = tab, type = type, text = text))
    tab_msg <- function(tab) renderUI({
      m <- msg()
      if (is.null(m) || !identical(m$tab, tab)) NULL else admin_alert(m)
    })
    output$roles_msg <- tab_msg("trainers")
    output$season_msg <- tab_msg("season")
    output$team_msg <- tab_msg("team")
    # Exercises: in the list the message is at the top; in the form it is
    # next to the buttons, so it is seen without scrolling.
    output$ex_msg <- renderUI({
      m <- msg()
      if (is.null(m) || !identical(m$tab, "exercises") || !is.null(editing())) NULL else admin_alert(m)
    })
    output$ex_form_msg <- renderUI({
      m <- msg()
      if (is.null(m) || !identical(m$tab, "exercises") || is.null(editing())) NULL else admin_alert(m)
    })

    # Read from the database; NULL (and an error message) if it fails.
    read <- function(f, tab) {
      tryCatch(db$run(f), error = function(e) {
        message("Lesing i adminpanelet feilet: ", conditionMessage(e))
        say(tab, "error", "Fikk ikke hentet data fra databasen. Prøv igjen om litt.")
        NULL
      })
    }

    # Check the input with `check` (its error message is shown as is), then
    # save with `f`. Database errors get a general message; the details go to
    # the log. Returns TRUE if saved.
    save <- function(tab, check, f, ok_text) {
      if (!isTRUE(allowed()) || is.null(group_id())) {
        say(tab, "error", "Du har ikke tilgang til innstillingene.")
        return(FALSE)
      }
      problem <- tryCatch({
        check()
        NULL
      }, error = function(e) conditionMessage(e))
      if (!is.null(problem)) {
        say(tab, "error", problem)
        return(FALSE)
      }
      ok <- tryCatch({
        db$run(f)
        TRUE
      }, error = function(e) {
        message("Lagring i adminpanelet feilet: ", conditionMessage(e))
        FALSE
      })
      if (ok) {
        say(tab, "ok", ok_text)
        refresh(refresh() + 1)
      } else {
        say(tab, "error", "Fikk ikke lagret. Prøv igjen om litt.")
      }
      ok
    }

    observeEvent(input$open, {
      req(allowed())
      acc <- user()$access
      if (nrow(acc) == 0) return()
      ctx <- context()
      admin_gid(if (!is.null(ctx) && ctx$group_id %in% acc$id) ctx$group_id else acc$id[1])
      msg(NULL)
      editing(NULL)
      confirm_delete(NULL)
      y <- this_year()
      showModal(admin_modal(ns, stats::setNames(acc$id, acc$name), admin_gid(), years = (y - 1):(y + 1), year = y))
    })
    observeEvent(input$admin_group, {
      acc <- user()$access
      if (!input$admin_group %in% acc$id || identical(input$admin_group, admin_gid())) return()
      admin_gid(input$admin_group)
      msg(NULL)
      editing(NULL)
      confirm_delete(NULL)
    }, ignoreInit = TRUE)
    observeEvent(input$tab, msg(NULL), ignoreInit = TRUE)

    # Trainers and their rights ---------------------------------------------------
    candidates <- reactive({
      g <- the_group()
      if (is.null(g)) role_candidates(list()) else role_candidates(g)
    })

    output$roles_body <- renderUI({
      req(allowed(), group_id())
      refresh()
      cand <- candidates()
      if (nrow(cand) == 0) return(p(class = "sn-hint", "Ingen trenere med profil i Spond i dette laget."))
      roles <- read(function(con) ds_list_roles(con, cand$profile_id), "trainers")
      if (is.null(roles)) return(NULL)
      r <- isolate(rights())
      me <- isolate(user())$profile$id
      role_rows(ns, cand, roles, r, me, superadmin, the_group())
    })

    observeEvent(input$role, {
      x <- input$role
      cand <- candidates()
      pid <- as.character(x$p %||% "")
      role <- as.character(x$f %||% "")
      granted <- isTRUE(x$v)
      if (!pid %in% cand$profile_id || !role %in% c("app", "ai", "admin")) return()
      if (is_superadmin(pid, superadmin)) return()
      u <- user()
      r <- rights()
      name <- cand$name[cand$profile_id == pid][1]
      what <- switch(role, app = "app-tilgang", ai = "KI-tilgang", admin = "admin")
      save("trainers", function() {
        if (!isTRUE(r$admin)) stop("Du har ikke tilgang til å endre rettigheter.", call. = FALSE)
        if (role == "admin" && !isTRUE(r$superadmin)) stop("Bare superadmin kan gi og ta admin.", call. = FALSE)
      }, function(con) ds_set_role(con, r, u$profile$id, pid, role, granted, cand$profile_id),
      paste0(name, if (granted) " har fått " else " har ikke lenger ", what, "."))
    })

    # Season plan -----------------------------------------------------------------
    year <- reactive({
      y <- suppressWarnings(as.integer(input$year))
      if (length(y) == 1 && !is.na(y)) y else this_year()
    })

    output$season_form <- renderUI({
      req(allowed(), group_id())
      refresh()
      gid <- group_id()
      y <- year()
      acc <- isolate(user())$access
      rows <- read(function(con) ds_list_season_themes(con, acc, gid, y), "season")
      if (is.null(rows)) return(NULL)
      tagList(season_form(ns, rows), admin_saved_line(rows$updated_at))
    })

    observeEvent(input$season_save, {
      themes <- vapply(1:12, function(i) input[[paste0("theme_", i)]] %||% "", character(1))
      descs <- vapply(1:12, function(i) input[[paste0("desc_", i)]] %||% "", character(1))
      u <- user()
      gid <- group_id()
      y <- year()
      save("season", function() season_validate(y, themes, descs),
           function(con) ds_save_season(con, u$access, gid, y, themes, descs, u$profile$id),
           paste0("Årshjulet for ", y, " er lagret."))
    })

    # Team settings ---------------------------------------------------------------
    output$team_form <- renderUI({
      req(allowed(), group_id())
      refresh()
      gid <- group_id()
      acc <- isolate(user())$access
      s <- read(function(con) ds_get_team_settings(con, acc, gid), "team")
      if (is.null(s)) return(NULL)
      team_form(ns, s)
    })

    team_input <- function() {
      list(age_group = input$age_group, session_minutes = input$session_minutes, pitch = input$pitch,
           equipment = input$equipment, principles = input$principles)
    }

    observeEvent(input$team_save, {
      s <- team_input()
      u <- user()
      gid <- group_id()
      save("team", function() team_settings_validate(s),
           function(con) ds_save_team_settings(con, u$access, gid, s, u$profile$id),
           "Lagets standard er lagret.")
    })

    # Exercises -------------------------------------------------------------------
    exercises <- reactive({
      req(allowed(), group_id())
      refresh()
      gid <- group_id()
      acc <- isolate(user())$access
      read(function(con) ds_list_exercises(con, acc, gid), "exercises")
    })

    # Themes to choose from: this and next year's season plan, and themes
    # already used on exercises.
    theme_choices <- reactive({
      gid <- group_id()
      acc <- isolate(user())$access
      y <- this_year()
      season <- read(function(con) {
        rbind(ds_list_season_themes(con, acc, gid, y), ds_list_season_themes(con, acc, gid, y + 1))
      }, "exercises")
      ex <- exercises()
      unique(c(season$theme, unlist(ex$themes)))
    })

    output$ex_body <- renderUI({
      ex <- exercises()
      if (is.null(ex)) return(NULL)
      ed <- editing()
      if (is.null(ed)) return(exercise_list(ns, ex, confirm_delete()))
      e <- if (identical(ed, "new")) list() else {
        row <- ex[as.character(ex$id) == ed, , drop = FALSE]
        if (nrow(row) == 0) return(exercise_list(ns, ex, NULL))
        as.list(row)
      }
      exercise_form(ns, e, isolate(theme_choices()))
    })

    known_exercise <- function(id) {
      ex <- isolate(exercises())
      !is.null(ex) && !is.null(id) && id %in% as.character(ex$id)
    }

    observeEvent(input$ex_new, {
      msg(NULL)
      confirm_delete(NULL)
      editing("new")
    })
    observeEvent(input$ex_edit, {
      if (!known_exercise(input$ex_edit)) return()
      msg(NULL)
      confirm_delete(NULL)
      editing(input$ex_edit)
    })
    observeEvent(input$ex_cancel, {
      msg(NULL)
      editing(NULL)
    })

    # Drawing preview: drawn on request, cleared when another exercise is opened.
    preview <- reactiveVal(NULL)   # list(src, problems) or list(error)
    observeEvent(editing(), preview(NULL), ignoreNULL = FALSE)
    observeEvent(input$ex_preview, {
      preview(tryCatch(drawing_preview(input$ex_drawing), error = function(e) list(error = conditionMessage(e))))
    })
    observeEvent(input$ex_example, {
      updateTextAreaInput(session, "ex_drawing", value = drawing_example_json())
      preview(NULL)
    })
    output$ex_preview_box <- renderUI({
      pv <- preview()
      if (is.null(pv)) return(NULL)
      if (!is.null(pv$error)) return(div(class = "alert alert-danger sn-alert", role = "alert", pv$error))
      tagList(
        tags$img(src = pv$src, class = "sn-ex-preview", alt = "Forhåndsvisning av tegningen"),
        if (nrow(pv$problems)) {
          div(class = "alert alert-warning sn-alert", role = "status",
              strong("Sjekk tegningen: "), tags$ul(lapply(pv$problems$melding, tags$li)))
        } else {
          p(class = "sn-hint", "Ingen kollisjoner funnet.")
        }
      )
    })

    exercise_input <- function() {
      list(name = input$ex_name, category = input$ex_category, themes = input$ex_themes,
           min_players = input$ex_min_players, max_players = input$ex_max_players,
           duration_minutes = input$ex_duration_minutes, area = input$ex_area,
           organisation = input$ex_organisation, execution = input$ex_execution,
           learning_points = input$ex_learning_points, questions = input$ex_questions,
           easier = input$ex_easier, harder = input$ex_harder, nff_url = input$ex_nff_url,
           drawing = input$ex_drawing)
    }

    observeEvent(input$ex_save, {
      ed <- editing()
      if (is.null(ed)) return()
      ex <- exercise_input()
      u <- user()
      gid <- group_id()
      id <- if (identical(ed, "new")) NULL else as.integer(ed)
      ok <- save("exercises", function() exercise_validate(ex),
                 function(con) ds_save_exercise(con, u$access, gid, ex, u$profile$id, id = id),
                 paste0("«", trimws(ex$name %||% ""), "» er lagret."))
      if (ok) editing(NULL)
    })

    observeEvent(input$ex_delete, {
      if (!known_exercise(input$ex_delete)) return()
      msg(NULL)
      confirm_delete(input$ex_delete)
    })
    observeEvent(input$ex_delete_cancel, confirm_delete(NULL))
    observeEvent(input$ex_delete_ok, {
      id <- input$ex_delete_ok
      if (!identical(id, confirm_delete()) || !known_exercise(id)) return()
      u <- user()
      ex <- isolate(exercises())
      name <- ex$name[as.character(ex$id) == id]
      confirm_delete(NULL)
      save("exercises", function() NULL,
           function(con) ds_delete_exercise(con, u$access, as.integer(id)),
           paste0("«", name, "» er slettet."))
    })

    invisible(list(allowed = allowed, rights = rights))
  })
}
