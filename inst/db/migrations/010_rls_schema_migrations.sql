-- Sikkerhetsgjennomgang 10. okt. 2026, punkt 4.
--
-- schema_migrations lages av ds_migrate() i samme skjema som app-tabellene
-- (public i Supabase). Der får nye tabeller rettigheter for rollene anon og
-- authenticated, så uten RLS kunne tabellen leses og endres gjennom
-- Supabase sitt Data API. Med RLS på og ingen policy kommer ingen andre enn
-- eieren (admin) til. ds_setup_app_role() tar i tillegg bort rettighetene
-- til anon og authenticated (den rollen finnes bare i Supabase).
-- Regel for migrasjonsfiler: én SQL-setning per ";" på slutten av en linje.

ALTER TABLE schema_migrations ENABLE ROW LEVEL SECURITY;
