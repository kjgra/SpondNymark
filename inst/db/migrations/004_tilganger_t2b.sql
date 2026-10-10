-- Treningsopplegg, fase T2b: ekstra rettigheter for trenere i adminpanelet.
-- Se claude/plan-treningsopplegg.md, kap. 12.
--
-- Hvem som er trener, kommer fortsatt fra Spond. Tabellen gir bare ekstra
-- rettigheter: KI-tilgang (fra T3) og admin. Superadmin står ikke her, men i
-- miljøvariabelen SPONDNYMARK_SUPERADMIN, så ingen kan låse seg ute.
-- Bare Spond-profil-ID-er lagres. Ingen navn.
-- Regel for migrasjonsfiler: én SQL-setning per ";" på slutten av en linje.

CREATE TABLE app_roles (
  spond_profile_id text PRIMARY KEY,
  can_use_ai       boolean NOT NULL DEFAULT false,
  is_admin         boolean NOT NULL DEFAULT false,
  updated_by       text NOT NULL,
  updated_at       timestamptz NOT NULL DEFAULT now()
);

-- Hver endring logges: hvem fikk hva, av hvem, og når.
CREATE TABLE app_role_log (
  id               integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  spond_profile_id text NOT NULL,
  role             text NOT NULL CHECK (role IN ('ai', 'admin')),
  granted          boolean NOT NULL,
  actor            text NOT NULL,
  created_at       timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX app_role_log_profile_idx ON app_role_log (spond_profile_id);

ALTER TABLE app_roles ENABLE ROW LEVEL SECURITY;
ALTER TABLE app_role_log ENABLE ROW LEVEL SECURITY;
