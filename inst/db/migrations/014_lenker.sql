-- Øvelser fra lenker i «Lag med KI» og «Juster med KI» (11. okt. 2026).
-- Nettstedene appen kan hente øvelser fra. Superadmin endrer listen under
-- Innstillinger → KI. Bare domenenavn, f.eks. tiim.no; underdomener er med.
-- Regel for migrasjonsfiler: én SQL-setning per ";" på slutten av en linje.

ALTER TABLE ai_settings ADD COLUMN link_domains text[] NOT NULL DEFAULT '{tiim.no,fotball.no}';
