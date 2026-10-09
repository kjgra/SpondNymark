# Startfil for Posit Connect (og shinyapps.io). Ikke flytt eller endre navn.
# Connect kjører denne filen. Den laster pakken fra kildekoden og starter appen
# i produksjonsmodus. Miljøvariablene (SPONDNYMARK_DB_URL) settes i Connect,
# under Settings -> Runtime -> Environment Variables.
#
# To ting her er med vilje:
# - R/_disable_autoload.R hindrer Shiny i å kjøre alle filene i R/ på egen hånd
#   (de skal lastes som pakke, med importene fra NAMESPACE).
# - run_app() hentes fra pakkens navnerom via load_all(), ikke med
#   "SpondNymark::". Da tror ikke rsconnect::writeManifest() at SpondNymark er
#   en pakke som må installeres fra CRAN.
ns <- pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)$env
options("golem.app.prod" = TRUE)
ns$run_app()
