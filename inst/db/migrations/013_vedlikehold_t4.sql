-- Treningsopplegg, fase T4: vedlikehold, arkiv og sanering (plan kap. 7).
--
-- maintenance: én rad med siste kjøring av vedlikeholdet (ved oppstart av
-- appen maks én gang per døgn, eller «Kjør vedlikehold nå» i adminpanelet).
-- archive_log: hver bekreftelse av at et arkiv er lagret før oppleggene ble
-- slettet (lag, periode, antall, hvem og når).
-- ai_usage_monthly: KI-forbruk eldre enn 12 måneder, summert per måned, lag,
-- type og modell (uten trener). Radene i ai_usage slettes da.
-- Regel for migrasjonsfiler: én SQL-setning per ";" på slutten av en linje.

CREATE TABLE maintenance (
  id           integer PRIMARY KEY CHECK (id = 1),
  last_run     timestamptz,
  run_by       text CHECK (length(run_by) <= 60),
  last_result  text CHECK (length(last_result) <= 2000)
);

INSERT INTO maintenance (id) VALUES (1);

ALTER TABLE maintenance ENABLE ROW LEVEL SECURITY;

CREATE TABLE archive_log (
  id               integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  spond_group_ids  text[] NOT NULL,
  period_from      date,
  period_to        date,
  n_events         integer NOT NULL CHECK (n_events >= 0),
  n_versions       integer NOT NULL CHECK (n_versions >= 0),
  confirmed_by     text NOT NULL,
  confirmed_at     timestamptz NOT NULL DEFAULT now()
);

ALTER TABLE archive_log ENABLE ROW LEVEL SECURITY;

CREATE TABLE ai_usage_monthly (
  month              date NOT NULL,
  spond_group_id     text NOT NULL,
  kind               text NOT NULL,
  model              text NOT NULL,
  n                  integer NOT NULL,
  n_error            integer NOT NULL,
  input_tokens       bigint NOT NULL,
  output_tokens      bigint NOT NULL,
  cache_read_tokens  bigint NOT NULL,
  cache_write_tokens bigint NOT NULL,
  cost_usd           numeric(12, 5) NOT NULL,
  PRIMARY KEY (month, spond_group_id, kind, model)
);

ALTER TABLE ai_usage_monthly ENABLE ROW LEVEL SECURITY;
