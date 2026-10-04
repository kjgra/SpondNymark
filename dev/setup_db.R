# Fase 0b: sett opp databasen i Supabase -----------------------------------------
#
# Forutsetning: Supabase-prosjektet er opprettet (se arbeidsplanen eller chatten).
# Kjør fra prosjektmappen:  source("dev/setup_db.R")
#
# Skriptet gjør dette:
#   1. Ber om tilkoblingsstrengen (Session pooler) og admin-passordet.
#      Ingen av dem lagres.
#   2. Oppretter tabellene: migrasjoner som ikke er kjørt før.
#   3. Oppretter app-brukeren «spondnymark_app» med et tilfeldig, sterkt passord.
#      Brukeren kan bare lese og skrive rader i appens tabeller, ikke endre
#      tabellene eller se noe annet i Supabase-prosjektet.
#   4. Skriver appens tilkoblingsstreng til .Renviron (SPONDNYMARK_DB_URL).
#      .Renviron står i .gitignore.
#   5. Sjekker at app-brukeren kan koble til, og at den ikke kan endre tabellene.
#
# Trygt å kjøre flere ganger. Hvis app-brukeren finnes fra før, kan du velge å
# beholde passordet eller lage et nytt.

pkgload::load_all(".", quiet = TRUE)
for (p in c("DBI", "RPostgres", "openssl", "askpass")) {
  if (!requireNamespace(p, quietly = TRUE)) stop("Installer pakken først: install.packages(\"", p, "\")")
}

role <- "spondnymark_app"

# 1. Admin-tilkobling -------------------------------------------------------------
# Tilkoblingsstrengen (uten passord) kan stå i .Renviron som SUPABASE_POOLER_URL,
# så slipper du å lime den inn hver gang. Den er ikke hemmelig.
template <- Sys.getenv("SUPABASE_POOLER_URL")
if (!nzchar(template)) {
  cat("Lim inn tilkoblingsstrengen fra Supabase (Connect -> Session pooler),\n",
      "slik den står med [YOUR-PASSWORD]:\n", sep = "")
  template <- trimws(readline("> "))
}
if (!grepl("^postgres(ql)?://", template)) stop("Det ser ikke ut som en tilkoblingsstreng fra Supabase.")
if (!grepl(":5432/", template, fixed = TRUE)) {
  warning("Strengen bruker ikke port 5432. Bruk «Session pooler», ikke «Transaction pooler».")
}

# Passorddialogen lukkes hvis du bytter vindu mens den er åpen. Derfor ber
# skriptet deg kopiere passordet først, og spør på nytt hvis noe går galt.
admin <- NULL
for (forsok in 1:3) {
  readline("Kopier admin-passordet nå (f.eks. fra passordbehandleren), og trykk Enter her når det er kopiert: ")
  password <- askpass::askpass("Lim inn admin-passordet til Supabase: ")
  if (is.null(password) || !nzchar(password)) {
    cat("Fikk ikke noe passord. Dialogen lukkes hvis du bytter vindu mens den er åpen. Prøv igjen.\n")
    next
  }
  admin_url <- ds_fill_password(template, password)
  rm(password)
  admin <- tryCatch(ds_connect(admin_url), error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("password authentication failed", msg, fixed = TRUE)) {
      cat("Feil passord. Prøv igjen.\n")
    } else {
      cat("Fikk ikke koblet til databasen:", msg, "\n")
    }
    NULL
  })
  if (!is.null(admin)) break
}
if (is.null(admin)) stop("Ga opp etter 3 forsøk. Kjør skriptet på nytt når du er klar.")

tryCatch({
  DBI::dbExecute(admin, "SET client_min_messages TO warning")

  # 2. Tabeller -------------------------------------------------------------------
  applied <- ds_migrate(admin)
  cat(if (length(applied)) paste("Kjørte migrasjoner:", paste(applied, collapse = ", "))
      else "Tabellene var allerede oppdatert.", "\n")

  # 3. App-bruker -------------------------------------------------------------------
  exists <- nrow(DBI::dbGetQuery(admin, "SELECT 1 FROM pg_roles WHERE rolname = $1", params = list(role))) > 0
  new_password <- NULL
  if (!exists) {
    new_password <- ds_random_password()
  } else {
    svar <- readline(paste0("App-brukeren finnes allerede. Lage nytt passord? ",
                            "Svar ja hvis SPONDNYMARK_DB_URL mangler i .Renviron (ja/nei): "))
    if (tolower(trimws(svar)) %in% c("ja", "j", "yes", "y")) new_password <- ds_random_password()
  }
  ds_setup_app_role(admin, role, new_password)
  cat("App-brukeren", role, if (exists) "er oppdatert." else "er opprettet.", "\n")

  # 4. .Renviron ------------------------------------------------------------------------
  if (!is.null(new_password)) {
    app_url <- ds_app_url(admin_url, role, new_password)
    ds_write_renviron("SPONDNYMARK_DB_URL", app_url)
    Sys.setenv(SPONDNYMARK_DB_URL = app_url)
    cat("Appens tilkoblingsstreng er lagret i .Renviron som SPONDNYMARK_DB_URL.\n",
        "Når appen publiseres på Posit Connect: kopier verdien fra .Renviron til\n",
        "Settings -> Runtime -> Environment Variables (navn: SPONDNYMARK_DB_URL).\n",
        "Hvis du har byttet passord, må verdien oppdateres der også.\n", sep = "")
  }
}, finally = DBI::dbDisconnect(admin))
rm(admin_url, template)

# 5. Sjekk app-brukeren -------------------------------------------------------------------
if (!nzchar(Sys.getenv("SPONDNYMARK_DB_URL")) && file.exists(".Renviron")) readRenviron(".Renviron")
if (!nzchar(Sys.getenv("SPONDNYMARK_DB_URL"))) {
  stop("SPONDNYMARK_DB_URL mangler. Kjør skriptet på nytt og svar ja til nytt passord.")
}
# Supabase sin pooler kan bruke litt tid på å kjenne igjen en ny bruker.
app <- NULL
for (forsok in 1:6) {
  app <- tryCatch(ds_connect(), error = function(e) {
    if (forsok == 6) stop("App-brukeren fikk ikke koblet til: ", conditionMessage(e), call. = FALSE)
    cat("Venter på at Supabase skal kjenne igjen app-brukeren ...\n")
    Sys.sleep(10)
    NULL
  })
  if (!is.null(app)) break
}
tryCatch({
  n <- DBI::dbGetQuery(app, "SELECT count(*)::integer AS n FROM group_proposals")$n
  can_create <- tryCatch({
    DBI::dbExecute(app, "CREATE TABLE spondnymark_rights_check (a integer)")
    DBI::dbExecute(app, "DROP TABLE spondnymark_rights_check")
    TRUE
  }, error = function(e) FALSE)
  cat("OK: app-brukeren kan lese tabellene (", n, " forslag).\n", sep = "")
  if (can_create) {
    warning("App-brukeren kunne opprette tabeller. Det skal den ikke. Si fra før du går videre.")
  } else {
    cat("OK: app-brukeren kan ikke endre tabellene.\n")
  }
  cat("Databasen er klar. Start R på nytt før du kjører appen, så .Renviron leses inn.\n")
}, finally = DBI::dbDisconnect(app))
