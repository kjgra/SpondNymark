# SpondNymark

Shiny-app for trenere og lagledere i Nymark. Appen leser lag, medlemmer og
arrangementer fra Spond, og lar trenerne

- se kommende og gjennomførte arrangementer med deltakere og svar,
- tagge medlemmer (f.eks. «Keeper»),
- lage gruppeforslag med dra-og-slipp (mus) eller «trykk og flytt» (mobil),
- sende forslag til godkjenning, godkjenne, avslå, rulle tilbake og slette,
- kommentere forslag og se historikken,
- skissere nye undergrupper som gruppeutkast.

Appen skriver aldri til Spond. Godkjente grupper settes opp i Spond for hånd.

## Tilgang og personvern

- Bare medlemmer med rollen Teamleder, Trener, Lagleder eller Hovedlagleder i
  en Spond-gruppe slipper inn, og bare til sine egne grupper
  (`access_role_names` i `inst/golem-config.yml`).
- Databasen lagrer bare Spond-ID-er, tagger, gruppeforslag, kommentarer og
  historikk. Navn hentes fra Spond ved visning. Fødselsdato, kontaktinfo,
  foresatte og avslagsmeldinger fra Spond kastes med en gang.
- Tagger og kommentarer skal ikke inneholde helseopplysninger eller andre
  sensitive opplysninger. Appen minner om det og avviser åpenbare ord.
- Passordet til Spond sendes bare til Spond og lagres aldri.

## Oppsett

1. Installer pakkene i `DESCRIPTION` (Imports og Suggests).
2. Kopier `.Renviron.example` til `.Renviron` og fyll inn verdiene.
   `.Renviron` skal aldri i git eller opp til Posit Connect.
3. Første gang: opprett tabellene og app-brukeren med
   `source("dev/setup_db.R")`. Skriptet spør etter admin-passordet til
   Supabase og lagrer det ikke.
4. Start R på nytt, så `.Renviron` leses inn.

## Kjøre lokalt

```r
golem::run_dev()          # eller: pkgload::load_all(); run_app()
```

## Tester

```r
devtools::test()
```

Databasetestene kjører bare når `SPONDNYMARK_TEST_DB_URL` peker på en
test-database (aldri produksjon). De lager og sletter et eget skjema.

## Publisering

Appen publiseres på Posit Connect fra GitHub (git-backed). Se `dev/03_deploy.R`.
Kort:

1. Kjør testene.
2. Ved databaseendringer: `source("dev/setup_db.R")` fra IDE, før push.
3. `rsconnect::writeManifest(appFiles = c("app.R", "DESCRIPTION", "NAMESPACE", "LICENSE", "R", "inst"))`
4. Commit (med `manifest.json`) og push til `main`. Connect henter endringen selv.

Miljøvariabelen `SPONDNYMARK_DB_URL` legges inn i Posit Connect, aldri i git.

### Test-app

Grenen `test` publiseres som en egen app mot test-prosjektet i Supabase.
`source("dev/setup_db.R")` med svaret «test» lager tabellene og app-brukeren
der og skriver `SPONDNYMARK_TEST_APP_DB_URL` i `.Renviron`. I test-appen på
Connect settes `SPONDNYMARK_DB_URL` til den verdien og `SPONDNYMARK_ENV=test`
(gir et TEST-merke). Se `dev/03_deploy.R`.

## Endringer i databasen

Se `inst/db/migrations/README.md`.

## Oppbygging

| Fil | Innhold |
|---|---|
| `R/spond_client.R` | Kall mot Sponds (uoffisielle) API |
| `R/fct_*.R` | Rene hjelpefunksjoner (tilgang, medlemmer, arrangementer, tagger, grupper) |
| `R/data_store.R` | All lesing og skriving i databasen, med tilgangssjekk |
| `R/mod_login.R` | Innlogging |
| `R/mod_teams.R` | Valg av hovedgruppe og undergruppe |
| `R/mod_events.R` | Arrangementer, medlemmer, fanen «Godkjenning» |
| `R/mod_participants.R` | Deltakere på et arrangement |
| `R/mod_tags.R` | Tagger og tagg-dialogen |
| `R/mod_groups.R` | Gruppeforslag, editor, låser, godkjenning, kommentarer |
| `inst/app/www/sn.js` | Dra-og-slipp, «trykk og flytt» og små hjelpere i nettleseren |
| `inst/db/migrations/` | Databaseskjemaet |

Arbeidsplanen ligger i Claude-prosjektet (`claude/arbeidsplan-mvp.md`).

Alle rettigheter forbeholdt, se `LICENSE`.
