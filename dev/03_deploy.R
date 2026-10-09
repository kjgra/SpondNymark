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
