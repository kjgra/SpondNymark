-- App-tilgang per trener i stedet for hvitliste med e-post og mobilnummer
-- (besluttet 10. okt. 2026). Se claude/videreutvikling.md.
--
-- Bare trenere med can_use_app (eller admin, eller superadmin fra
-- miljøvariabelen) kommer inn i appen. Hvitlisten fra migrasjon 006 brukes
-- ikke lenger og slettes. Endringer logges i app_role_log med role = 'app'.
-- Regel for migrasjonsfiler: én SQL-setning per ";" på slutten av en linje.

ALTER TABLE app_roles ADD COLUMN can_use_app boolean NOT NULL DEFAULT false;

ALTER TABLE app_role_log DROP CONSTRAINT app_role_log_role_check;
ALTER TABLE app_role_log ADD CONSTRAINT app_role_log_role_check CHECK (role IN ('ai', 'admin', 'app'));

DROP TABLE login_allowlist_log;
DROP TABLE login_allowlist;
