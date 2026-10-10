-- Treningsopplegg, fase T2-3: opplegg per arrangement, med versjoner.
-- Se claude/plan-treningsopplegg.md, kap. 7.1 og 15.
--
-- Opplegget lagres som JSON (et øyeblikksbilde med kopi av hver øvelse),
-- aldri som PDF. Ingen navn på spillere: gruppenavn med spillere settes inn
-- når PDF-en lages, og lagres ikke. created_by er en Spond-profil-ID.
-- Regel for migrasjonsfiler: én SQL-setning per ";" på slutten av en linje.

CREATE TABLE training_plans (
  id             integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  spond_group_id text NOT NULL,
  spond_event_id text NOT NULL,
  version        integer NOT NULL CHECK (version >= 1),
  -- Godkjenning kommer i T3; da brukes også pending, approved og rejected.
  status         text NOT NULL DEFAULT 'draft'
                 CHECK (status IN ('draft', 'pending', 'approved', 'rejected', 'deleted')),
  plan           jsonb NOT NULL,
  source         text NOT NULL DEFAULT 'manual' CHECK (source IN ('manual', 'ai')),
  model          text,
  cost_usd       numeric(10, 5),
  created_by     text NOT NULL,
  created_at     timestamptz NOT NULL DEFAULT now(),
  UNIQUE (spond_group_id, spond_event_id, version)
);

CREATE INDEX training_plans_event_idx ON training_plans (spond_group_id, spond_event_id);

ALTER TABLE training_plans ENABLE ROW LEVEL SECURITY;
