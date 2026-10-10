-- Treningsopplegg, fase T3a: KI-forbruk, innstillinger for KI og varianter i øvelsesbanken.
-- Se claude/plan-treningsopplegg.md, kap. 12.3, 15 og 16.
--
-- ai_usage logger hvert kall til Claude API: tokens og kostnad, aldri tekst
-- fra prompten eller svaret. spond_profile_id er treneren som ba om kallet.
-- ai_settings har én rad (id = true). Mangler raden, bruker appen standardverdiene i R.
-- Regel for migrasjonsfiler: én SQL-setning per ";" på slutten av en linje.

CREATE TABLE ai_usage (
  id                 integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  created_at         timestamptz NOT NULL DEFAULT now(),
  spond_profile_id   text NOT NULL,
  spond_group_id     text NOT NULL,
  spond_event_id     text,
  plan_id            integer REFERENCES training_plans (id) ON DELETE SET NULL,
  kind               text NOT NULL CHECK (kind IN ('draft', 'revision', 'visual', 'learning')),
  model              text NOT NULL CHECK (length(model) <= 60),
  input_tokens       integer NOT NULL DEFAULT 0 CHECK (input_tokens >= 0),
  output_tokens      integer NOT NULL DEFAULT 0 CHECK (output_tokens >= 0),
  cache_read_tokens  integer NOT NULL DEFAULT 0 CHECK (cache_read_tokens >= 0),
  cache_write_tokens integer NOT NULL DEFAULT 0 CHECK (cache_write_tokens >= 0),
  cost_usd           numeric(10, 5) NOT NULL DEFAULT 0 CHECK (cost_usd >= 0),
  duration_ms        integer CHECK (duration_ms >= 0),
  status             text NOT NULL CHECK (status IN ('ok', 'error')),
  -- Bare en kort feilkode, f.eks. rate_limit_error, max_tokens eller ugyldig_json.
  error_code         text CHECK (length(error_code) <= 60)
);

CREATE INDEX ai_usage_created_idx ON ai_usage (created_at);
CREATE INDEX ai_usage_group_idx ON ai_usage (spond_group_id, created_at);

CREATE TABLE ai_settings (
  id                boolean PRIMARY KEY DEFAULT true CHECK (id),
  model             text NOT NULL CHECK (length(model) <= 60),
  group_limit_nok   numeric(8, 2) NOT NULL CHECK (group_limit_nok >= 0),
  total_limit_usd   numeric(8, 2) NOT NULL CHECK (total_limit_usd >= 0),
  max_new_exercises integer NOT NULL CHECK (max_new_exercises BETWEEN 0 AND 6),
  usd_nok           numeric(6, 3) NOT NULL CHECK (usd_nok > 0),
  updated_by        text NOT NULL,
  updated_at        timestamptz NOT NULL DEFAULT now()
);

-- Varianter og status i øvelsesbanken (kap. 15.4). based_on er koden til
-- grunnøvelsen i samme hovedgruppe, aldri en variant (sjekkes i appen).
ALTER TABLE exercises ADD COLUMN based_on text CHECK (based_on ~ '^[a-z0-9]+(-[a-z0-9]+)*$' AND length(based_on) <= 60);
ALTER TABLE exercises ADD COLUMN status text NOT NULL DEFAULT 'active' CHECK (status IN ('active', 'candidate', 'archived'));

ALTER TABLE ai_usage ENABLE ROW LEVEL SECURITY;
ALTER TABLE ai_settings ENABLE ROW LEVEL SECURITY;
