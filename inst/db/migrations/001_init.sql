-- SpondNymark: første skjema.
--
-- Alle *_id-kolonner som starter med spond_ (og created_by, actor, author,
-- editor) inneholder Spond-ID-er. Vi lagrer aldri navn, kontaktinfo eller andre
-- personopplysninger fra Spond. Navn hentes fra Spond ved visning.
--
-- Regel for migrasjonsfiler: én SQL-setning per ";" på slutten av en linje.
-- Bruk ikke funksjoner eller DO-blokker (migrasjonskjøreren deler på ";").

CREATE TABLE member_tags (
  spond_group_id  text NOT NULL,
  spond_member_id text NOT NULL,
  tag             text NOT NULL CHECK (length(btrim(tag)) BETWEEN 1 AND 40),
  created_by      text NOT NULL,
  created_at      timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (spond_group_id, spond_member_id, tag)
);

CREATE TABLE group_proposals (
  id                integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  spond_group_id    text NOT NULL,
  -- Bare for gruppeutkast: konteksten utkastet ble laget i (NULL = hele gruppen).
  -- Forslag knyttet til et arrangement vises der arrangementet vises.
  spond_subgroup_id text,
  spond_event_id    text,
  name              text NOT NULL CHECK (length(btrim(name)) BETWEEN 1 AND 120),
  status            text NOT NULL DEFAULT 'draft'
                    CHECK (status IN ('draft', 'pending', 'approved', 'rejected', 'rolled_back', 'deleted')),
  created_by        text NOT NULL,
  created_at        timestamptz NOT NULL DEFAULT now(),
  updated_at        timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX group_proposals_context_idx
  ON group_proposals (spond_group_id, spond_event_id, spond_subgroup_id)
  WHERE status <> 'deleted';

-- Gruppene (kolonnene) i et forslag, i rekkefølge.
CREATE TABLE group_proposal_labels (
  proposal_id integer NOT NULL REFERENCES group_proposals (id) ON DELETE CASCADE,
  label       text NOT NULL CHECK (length(btrim(label)) BETWEEN 1 AND 60),
  sort_order  integer NOT NULL,
  PRIMARY KEY (proposal_id, label)
);

-- Hvilken gruppe hvert medlem er plassert i.
CREATE TABLE group_proposal_members (
  proposal_id     integer NOT NULL,
  spond_member_id text NOT NULL,
  label           text NOT NULL,
  PRIMARY KEY (proposal_id, spond_member_id),
  FOREIGN KEY (proposal_id, label) REFERENCES group_proposal_labels (proposal_id, label)
    ON UPDATE CASCADE ON DELETE CASCADE
);

-- Historikk: hvem gjorde hva med et forslag, og når.
CREATE TABLE proposal_history (
  id          integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  proposal_id integer NOT NULL REFERENCES group_proposals (id) ON DELETE CASCADE,
  actor       text NOT NULL,
  decision    text NOT NULL
              CHECK (decision IN ('created', 'edited', 'sent_for_approval', 'approved', 'rejected', 'rolled_back', 'deleted')),
  created_at  timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX proposal_history_proposal_idx ON proposal_history (proposal_id);

CREATE TABLE proposal_comments (
  id          integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  proposal_id integer NOT NULL REFERENCES group_proposals (id) ON DELETE CASCADE,
  author      text NOT NULL,
  body        text NOT NULL CHECK (length(btrim(body)) BETWEEN 1 AND 2000),
  created_at  timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX proposal_comments_proposal_idx ON proposal_comments (proposal_id);

-- Hvem redigerer hva akkurat nå. proposal_id NULL = nytt forslag under opprettelse.
-- session_key er en tilfeldig nøkkel per åpen editor, slik at en trener med to
-- faner ikke blander låsene sine.
CREATE TABLE edit_locks (
  id                integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  spond_group_id    text NOT NULL,
  spond_subgroup_id text,
  spond_event_id    text,
  proposal_id       integer REFERENCES group_proposals (id) ON DELETE CASCADE,
  editor            text NOT NULL,
  session_key       text NOT NULL UNIQUE,
  started_at        timestamptz NOT NULL DEFAULT now(),
  heartbeat_at      timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX edit_locks_group_idx ON edit_locks (spond_group_id);

-- Databasen skal bare brukes av Shiny-serveren. Row Level Security uten
-- policies stenger for tilgang med Supabase sine offentlige nøkler (anon/authenticated).
ALTER TABLE member_tags ENABLE ROW LEVEL SECURITY;
ALTER TABLE group_proposals ENABLE ROW LEVEL SECURITY;
ALTER TABLE group_proposal_labels ENABLE ROW LEVEL SECURITY;
ALTER TABLE group_proposal_members ENABLE ROW LEVEL SECURITY;
ALTER TABLE proposal_history ENABLE ROW LEVEL SECURITY;
ALTER TABLE proposal_comments ENABLE ROW LEVEL SECURITY;
ALTER TABLE edit_locks ENABLE ROW LEVEL SECURITY;
