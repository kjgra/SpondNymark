-- Treningsopplegg, fase T3c: godkjenning av én versjon per trening.
-- Se claude/plan-treningsopplegg.md, kap. 15.
--
-- Én versjon per arrangement kan ha status approved. Når en versjon
-- godkjennes, settes andre godkjente versjoner for samme arrangement tilbake
-- til draft (i samme setning i appen). approved_by er en Spond-profil-ID.
-- Regel for migrasjonsfiler: én SQL-setning per ";" på slutten av en linje.

ALTER TABLE training_plans ADD COLUMN approved_by text;
ALTER TABLE training_plans ADD COLUMN approved_at timestamptz;

CREATE UNIQUE INDEX training_plans_one_approved_idx ON training_plans (spond_group_id, spond_event_id) WHERE status = 'approved';
