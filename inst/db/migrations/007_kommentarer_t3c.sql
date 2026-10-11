-- Treningsopplegg, fase T3c: kommentarer på opplegg, og «Be om endring» med KI.
-- Se claude/plan-treningsopplegg.md, kap. 13.
--
-- En kommentar gjelder én versjon (plan_id) og hele økta (exercise_no NULL)
-- eller én øvelse. Koden lagres også, så kommentaren kan knyttes til samme
-- øvelse i senere versjoner. used_in_plan_id er versjonen som KI laget med
-- kommentaren («tatt med i versjon 3»). author er en Spond-profil-ID.
-- Teksten sendes til KI uten forfatter. Samme sperre mot sensitive ord som
-- for andre kommentarer gjelder i appen.
-- Regel for migrasjonsfiler: én SQL-setning per ";" på slutten av en linje.

CREATE TABLE training_plan_comments (
  id              integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  spond_group_id  text NOT NULL,
  spond_event_id  text NOT NULL,
  plan_id         integer NOT NULL REFERENCES training_plans (id) ON DELETE CASCADE,
  exercise_no     integer CHECK (exercise_no BETWEEN 1 AND 6),
  exercise_code   text CHECK (length(exercise_code) <= 60),
  author          text NOT NULL,
  body            text NOT NULL CHECK (length(btrim(body)) BETWEEN 1 AND 1000),
  created_at      timestamptz NOT NULL DEFAULT now(),
  used_in_plan_id integer REFERENCES training_plans (id) ON DELETE SET NULL
);

CREATE INDEX training_plan_comments_event_idx ON training_plan_comments (spond_group_id, spond_event_id);

ALTER TABLE training_plan_comments ENABLE ROW LEVEL SECURITY;
