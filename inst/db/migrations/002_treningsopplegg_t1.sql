-- Treningsopplegg, fase T1: årshjul, lagets standard og øvelser.
-- Alt gjelder én hovedgruppe i Spond (spond_group_id). Se
-- claude/plan-treningsopplegg.md, kap. 3.1.
--
-- updated_by og created_by er Spond-profil-ID-er. Ingen navn lagres.
-- Regel for migrasjonsfiler: én SQL-setning per ";" på slutten av en linje.

-- Årshjul: ett hovedtema per måned, eget for hvert år.
CREATE TABLE season_themes (
  spond_group_id text NOT NULL,
  year           integer NOT NULL CHECK (year BETWEEN 2020 AND 2100),
  month          integer NOT NULL CHECK (month BETWEEN 1 AND 12),
  theme          text NOT NULL CHECK (length(btrim(theme)) BETWEEN 1 AND 80),
  description    text CHECK (length(description) <= 500),
  updated_by     text NOT NULL,
  updated_at     timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (spond_group_id, year, month)
);

-- Lagets standard: brukes som utgangspunkt for hvert treningsopplegg.
CREATE TABLE team_settings (
  spond_group_id  text PRIMARY KEY,
  age_group       text CHECK (length(age_group) <= 40),
  session_minutes integer CHECK (session_minutes BETWEEN 15 AND 240),
  pitch           text CHECK (length(pitch) <= 200),
  equipment       text CHECK (length(equipment) <= 1000),
  principles      text CHECK (length(principles) <= 2000),
  updated_by      text NOT NULL,
  updated_at      timestamptz NOT NULL DEFAULT now()
);

-- Øvelser: lagt inn for hånd (source = 'manual') eller fra KI-opplegg som fikk
-- god evaluering (source = 'ai', fra T3b). code er stabil og brukes til å samle
-- erfaringer per øvelse. drawing er tegningen som koordinater (fra T2).
CREATE TABLE exercises (
  id               integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  spond_group_id   text NOT NULL,
  code             text NOT NULL CHECK (code ~ '^[a-z0-9]+(-[a-z0-9]+)*$' AND length(code) <= 60),
  name             text NOT NULL CHECK (length(btrim(name)) BETWEEN 1 AND 80),
  category         text NOT NULL CHECK (length(category) BETWEEN 1 AND 40),
  themes           text[] NOT NULL DEFAULT '{}',
  min_players      integer CHECK (min_players BETWEEN 1 AND 60),
  max_players      integer CHECK (max_players BETWEEN 1 AND 60),
  duration_minutes integer CHECK (duration_minutes BETWEEN 1 AND 120),
  area             text CHECK (length(area) <= 60),
  organisation     text CHECK (length(organisation) <= 2000),
  execution        text CHECK (length(execution) <= 2000),
  learning_points  text CHECK (length(learning_points) <= 2000),
  questions        text CHECK (length(questions) <= 2000),
  easier           text CHECK (length(easier) <= 1000),
  harder           text CHECK (length(harder) <= 1000),
  nff_url          text CHECK (nff_url ~ '^https://' AND length(nff_url) <= 500),
  drawing          jsonb,
  source           text NOT NULL DEFAULT 'manual' CHECK (source IN ('manual', 'ai')),
  created_by       text NOT NULL,
  created_at       timestamptz NOT NULL DEFAULT now(),
  updated_by       text NOT NULL,
  updated_at       timestamptz NOT NULL DEFAULT now(),
  UNIQUE (spond_group_id, code),
  CHECK (min_players IS NULL OR max_players IS NULL OR max_players >= min_players)
);

ALTER TABLE season_themes ENABLE ROW LEVEL SECURITY;
ALTER TABLE team_settings ENABLE ROW LEVEL SECURITY;
ALTER TABLE exercises ENABLE ROW LEVEL SECURITY;
