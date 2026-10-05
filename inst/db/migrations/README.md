# Endringer i databasen

Databasen (Supabase/Postgres) endres bare med nye SQL-filer i denne mappen.
Appen kjører aldri migrasjoner selv. App-brukeren `spondnymark_app` har ikke lov.

## Sjekkliste for en endring

1. **Ny fil, aldri endre en gammel.** Filer som er kjørt, er notert i
   `schema_migrations` og kjøres ikke igjen. Lag neste nummer:
   `002_kort_beskrivelse.sql`, `003_...`. Tre sifre, små bokstaver og `_`,
   ingen hull i nummereringen.
2. **Format.** Én SQL-setning per `;` på slutten av en linje. Ingen
   funksjoner eller `DO`-blokker (kjøreren deler filen på `;`). Kommentarer
   med `--` er fine. Hver fil kjøres i én transaksjon.
3. **Bare Spond-ID-er.** Nye kolonner kan inneholde Spond-ID-er og våre egne
   data, aldri navn, kontaktinformasjon, helseopplysninger eller annet fra Spond.
4. **Nye tabeller:**
   - `ALTER TABLE ... ENABLE ROW LEVEL SECURITY;` i samme fil.
   - Legg tabellnavnet til i `ds_app_tables` i `R/data_store.R`, ellers får
     ikke app-brukeren tilgang. Testen i `tests/testthat/test-migrations.R`
     feiler hvis dette er glemt.
   - Gi ikke rettigheter i SQL-filen. Det gjør `ds_setup_app_role()`.
5. **Lagringslaget.** Les og skriv bare via funksjoner i `R/data_store.R`, med
   tilgangssjekk (`assert_group_access()` eller `ds_proposal_group()`).
6. **Test lokalt.** Kjør testene med `SPONDNYMARK_TEST_DB_URL` satt (se
   `.Renviron.example`). De lager et midlertidig skjema, kjører alle
   migrasjonene og sletter skjemaet etterpå.
7. **Kjør mot Supabase.** `source("dev/setup_db.R")` på din egen maskin. Skriptet
   kjører nye migrasjoner som admin og oppdaterer rettighetene til app-brukeren.
   Svar **nei** på spørsmålet om nytt passord, med mindre du vil bytte det.
8. **Publiser appen** etter at databasen er oppdatert, ikke før.

## Hvis noe går galt

En migrasjon som feiler, rulles tilbake i sin helhet og noteres ikke. Rett
filen og kjør `dev/setup_db.R` på nytt. Er en migrasjon allerede kjørt og feil,
lag en ny fil som retter den. Endre ikke den gamle.
