# Startfil for Posit Connect (og shinyapps.io). Ikke flytt eller endre navn.
# Connect kjører denne filen. Den laster pakken fra kildekoden og starter appen
# i produksjonsmodus. Miljøvariablene (SPONDNYMARK_DB_URL) settes i Connect,
# under Settings -> Runtime -> Environment Variables.
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)
SpondNymark::run_app()
