# Publisering til Posit Connect
#
# Kjør stegene ett og ett fra RStudio/Positron, fra prosjektmappen.
# Første gang: steg 1-4. Senere oppdateringer: steg 1, 2 og 4.

# 1. Test ------------------------------------------------------------------------
# Med SPONDNYMARK_TEST_DB_URL i .Renviron kjøres også databasetestene.
devtools::test()
# Sjekk pakken. Forventet: én advarsel om norske bokstaver (æ, ø, å) i R-filene.
# Den er ufarlig: DESCRIPTION sier Encoding: UTF-8, og Connect kjører UTF-8.
devtools::check(document = FALSE, args = "--no-manual")

# 2. Databasen ---------------------------------------------------------------------
# Bare hvis det er nye filer i inst/db/migrations/ (se README.md der):
# source("dev/setup_db.R")

# 3. Første publisering --------------------------------------------------------------
# Krever en konto på Posit Connect, koblet til RStudio/Positron:
# rsconnect::addServer(...) og rsconnect::connectApiUser(...), se dokumentasjonen
# for serveren din.
#
# Etter første publisering: åpne appen i Connect og legg inn miljøvariabelen
#   SPONDNYMARK_DB_URL = (verdien fra .Renviron)
# under Settings -> Runtime -> Environment Variables. Appen stopper ved oppstart
# med en tydelig melding til dette er gjort. Begrens gjerne tilgangen
# (Settings -> Access) til innloggede brukere i klubben.

# 4. Publiser ------------------------------------------------------------------------
# app.R starter appen. .rscignore holder .Renviron, dev/ og tests/ unna.
rsconnect::deployApp(
  appName = "spondnymark",
  appTitle = "SpondNymark",
  appFiles = c("app.R", "DESCRIPTION", "NAMESPACE", "LICENSE", "R/", "inst/"),
  lint = FALSE,
  forceUpdate = TRUE
)
