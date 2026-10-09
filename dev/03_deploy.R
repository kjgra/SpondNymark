# Publisering til Posit Connect fra GitHub
#
# Connect henter appen fra GitHub-repoet (git-backed deployment). Herfra
# (RStudio/Positron) kjører du bare tester, databaseendringer og manifestet,
# og så committer og pusher du. Kjør stegene ett og ett fra prosjektmappen.

# 1. Test ------------------------------------------------------------------------
# Med SPONDNYMARK_TEST_DB_URL i .Renviron kjøres også databasetestene.
devtools::test()
# Forventet: én advarsel om norske bokstaver (æ, ø, å) i R-filene. Den er
# ufarlig: DESCRIPTION sier Encoding: UTF-8, og Connect kjører UTF-8.
devtools::check(document = FALSE, args = "--no-manual")

# 2. Databasen (bare ved nye filer i inst/db/migrations/) ---------------------------
# Kjøres FØR push, så tabellene finnes når Connect starter den nye versjonen.
# Se inst/db/migrations/README.md. Svar "nei" på nytt passord.
# source("dev/setup_db.R")

# 3. Manifest -----------------------------------------------------------------------
# manifest.json forteller Connect hvilke filer og pakkeversjoner appen trenger.
# Lag det på nytt når pakker er lagt til/oppdatert eller filer er lagt til.
# Den faste fillisten holder .Renviron, dev/ og tests/ unna.
rsconnect::writeManifest(
  appFiles = c("app.R", "DESCRIPTION", "NAMESPACE", "LICENSE", "R", "inst")
)

# 4. Commit og push ---------------------------------------------------------------
# Commit endringene (også manifest.json) og push til main. Connect henter nye
# commits selv (omtrent hvert 15. minutt), eller trykk "Update now" i Connect.
# Sjekk før første push at .Renviron aldri har vært committet (tom utskrift = OK):
#   git log --all --oneline -- .Renviron

# Første gang i Posit Connect ---------------------------------------------------------
# 1. Publish -> Import from Git: repo-URL, gren main, mappe "/".
#    Privat repo: Connect må ha tilgang (settes opp av administrator).
# 2. Settings -> Runtime -> Environment Variables:
#      SPONDNYMARK_DB_URL = (verdien fra .Renviron)
#    Appen stopper ved oppstart med en tydelig melding til dette er gjort.
# 3. Settings -> Access: begrens tilgangen, f.eks. til innloggede brukere.

# Test-appen (grenen test) ------------------------------------------------------------
# En egen app i Connect mot test-prosjektet i Supabase. Prod-appen røres ikke.
# 1. Én gang: source("dev/setup_db.R") og svar «test». Det lager tabellene og
#    app-brukeren i test-databasen og skriver SPONDNYMARK_TEST_APP_DB_URL i .Renviron.
#    Nye migrasjoner: kjør skriptet med «test» før push til test, og med «prod»
#    før merge til main.
# 2. Lag manifest.json i grenen test (steg 3 over), commit og push til test.
# 3. Publish -> Import from Git: samme repo, gren test, mappe "/".
#    Gi appen et tydelig navn, f.eks. «SpondNymark TEST».
# 4. Settings -> Runtime -> Environment Variables:
#      SPONDNYMARK_DB_URL = (verdien av SPONDNYMARK_TEST_APP_DB_URL i .Renviron)
#      SPONDNYMARK_ENV    = test
#    Aldri SPONDNYMARK_TEST_DB_URL (admin) i Connect.
# 5. Settings -> Access: samme begrensning som prod-appen. Appen viser ekte
#    Spond-data, så samme personvernregler gjelder.
