-- Hvitliste for innlogging: hvem som kan logge inn i appen (besluttet 10. okt. 2026).
-- Se claude/videreutvikling.md, «Hvitliste for innlogging».
--
-- E-post og mobilnummer lagres aldri i klartekst. id_hash er HMAC-SHA256 av
-- normalisert e-post eller mobilnummer, med nøkkelen i miljøvariabelen
-- SPONDNYMARK_LOGIN_KEY. hint er en maskert visning for admin, f.eks.
-- «k•••@gmail.com» eller «+47 •••• ••12». spond_profile_id settes første gang
-- personen logger inn, så admin kan se navnet (hentes fra Spond, lagres ikke).
-- Superadmin og trenere med admin slipper alltid inn og trenger ingen rad.
-- Regel for migrasjonsfiler: én SQL-setning per ";" på slutten av en linje.

CREATE TABLE login_allowlist (
  id_hash          text PRIMARY KEY CHECK (id_hash ~ '^[0-9a-f]{64}$'),
  kind             text NOT NULL CHECK (kind IN ('email', 'phone')),
  hint             text NOT NULL CHECK (length(hint) BETWEEN 1 AND 80),
  spond_profile_id text,
  last_login_at    timestamptz,
  added_by         text NOT NULL,
  added_at         timestamptz NOT NULL DEFAULT now()
);

-- Hver endring logges: hvem la til eller fjernet hva, og når.
CREATE TABLE login_allowlist_log (
  id         integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  id_hash    text NOT NULL,
  hint       text NOT NULL,
  action     text NOT NULL CHECK (action IN ('add', 'remove')),
  actor      text NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now()
);

ALTER TABLE login_allowlist ENABLE ROW LEVEL SECURITY;
ALTER TABLE login_allowlist_log ENABLE ROW LEVEL SECURITY;
