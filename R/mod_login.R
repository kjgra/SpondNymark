#' Login with Spond
#'
#' Trainers log in with their own Spond e-mail and password. The password is
#' sent to Spond once, cleared from the form and never stored. After login we
#' keep the Spond session (token) and a minimised copy of the data the app
#' needs (see `spond_session_data()`) for this Shiny session only.
#'
#' Only users with a trainer/leader role in at least one group get in; others
#' see a message explaining why.
#'
#' The module has two UI parts sharing one server: `mod_login_ui()` (the login
#' form, or the no-access message) and `mod_login_bar_ui()` (who is logged in,
#' with a logout link, for the top bar).
#'
#' @param spond Functions used to talk to Spond. Tests pass fakes.
#' @param role_names Role names that give access.
#' @return A reactive: NULL while logged out, otherwise a list with `spond`
#'   (session), `profile`, `groups` and `access`.
#' @noRd
#' @importFrom shiny NS tagList
mod_login_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("ui")),
    login_js(ns)
  )
}

#' @noRd
mod_login_bar_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("bar"), class = "sn-bar")
}

spond_api <- function() {
  list(login = spond_login, profile = spond_get_profile, groups = spond_get_groups)
}

login_form <- function(ns) {
  div(
    class = "sn-login",
    div(
      class = "sn-login-title",
      h1("SpondNymark"),
      p("Gruppeinndeling for trenere og lagledere.")
    ),
    bslib::card(
      bslib::card_body(
        htmltools::tagAppendAttributes(
          textInput(ns("email"), "E-post i Spond", width = "100%"),
          autocomplete = "username", inputmode = "email", .cssSelector = "input"
        ),
        htmltools::tagAppendAttributes(
          passwordInput(ns("password"), "Passord i Spond", width = "100%"),
          autocomplete = "current-password", .cssSelector = "input"
        ),
        actionButton(ns("login"), "Logg inn", class = "btn-primary w-100"),
        uiOutput(ns("message")),
        p(class = "sn-hint",
          "Bruk samme e-post og passord som i Spond. Passordet sendes bare til Spond og lagres ikke.")
      )
    )
  )
}

no_access_ui <- function(ns, s) {
  div(
    class = "sn-login",
    bslib::card(
      bslib::card_body(
        h2("Ingen tilgang"),
        p("Du er logget inn som ", strong(s$profile$first_name, .noWS = "after"),
          ", men har ingen rolle i Spond som gir tilgang til appen."),
        p(paste0("Appen er for trenere og lagledere. Disse rollene gir tilgang: ",
                 paste(s$role_names, collapse = ", "), ".")),
        p(class = "sn-hint", "Mener du at du burde hatt tilgang, be en administrator i klubben sjekke rollen din i Spond."),
        actionButton(ns("logout"), "Logg ut", class = "btn-outline-secondary")
      )
    )
  )
}

# Enter submits; the button shows a busy state until the server is done.
login_js <- function(ns) {
  tags$script(htmltools::HTML(sprintf("
    (function() {
      var email = '#%1$s', password = '#%2$s', button = '#%3$s';
      $(document).on('keydown', email + ',' + password, function(e) {
        if (e.key === 'Enter') {
          e.preventDefault();
          // Let Shiny send the typed values (text inputs are debounced) before clicking.
          setTimeout(function() { $(button).trigger('click'); }, 300);
        }
      });
      $(document).on('click', button, function() {
        var b = $(this);
        setTimeout(function() { b.prop('disabled', true).text('Logger inn …'); }, 0);
      });
      Shiny.addCustomMessageHandler('sn-login-done', function(id) {
        $('#' + id).prop('disabled', false).text('Logg inn');
      });
    })();", ns("email"), ns("password"), ns("login"))))
}

#' @noRd
mod_login_server <- function(id, spond = spond_api(), role_names = access_role_names()) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    state <- reactiveVal(list(status = "logged_out"))
    error_msg <- reactiveVal(NULL)

    output$ui <- renderUI({
      s <- state()
      switch(s$status,
        logged_out = login_form(ns),
        no_access = no_access_ui(ns, s),
        ok = NULL
      )
    })

    output$bar <- renderUI({
      s <- state()
      if (!identical(s$status, "ok")) return(NULL)
      tagList(
        span("Innlogget som ", strong(s$profile$first_name)),
        actionLink(ns("logout"), "Logg ut")
      )
    })

    output$message <- renderUI({
      m <- error_msg()
      if (is.null(m)) return(NULL)
      div(class = "alert alert-danger sn-alert", role = "alert", m)
    })

    observeEvent(input$login, {
      on.exit(session$sendCustomMessage("sn-login-done", ns("login")), add = TRUE)
      email <- trimws(input$email %||% "")
      password <- input$password %||% ""
      # Remove the password from the form right away.
      updateTextInput(session, "password", value = "")

      if (!nzchar(email) || !nzchar(password)) {
        error_msg("Skriv inn både e-post og passord.")
        return()
      }
      error_msg(NULL)

      result <- tryCatch({
        sess <- spond$login(email, password)
        list(spond = sess, data = spond_session_data(spond$groups(sess), spond$profile(sess), role_names))
      }, error = function(e) e)
      rm(password)

      if (inherits(result, "error")) {
        if (inherits(result, "spond_error")) {
          error_msg(conditionMessage(result))
        } else {
          # Technical details go to the server log, not to the user.
          message("Innlogging feilet: ", conditionMessage(result))
          error_msg("Fikk ikke kontakt med Spond akkurat nå. Prøv igjen om litt.")
        }
        return()
      }

      d <- result$data
      if (nrow(d$access) == 0) {
        state(list(status = "no_access", profile = d$profile, role_names = role_names))
      } else {
        state(list(status = "ok", spond = result$spond, profile = d$profile,
                   groups = d$groups, access = d$access))
      }
    })

    observeEvent(input$logout, {
      state(list(status = "logged_out"))
      error_msg(NULL)
    })

    reactive({
      s <- state()
      if (identical(s$status, "ok")) s[c("spond", "profile", "groups", "access")] else NULL
    })
  })
}
