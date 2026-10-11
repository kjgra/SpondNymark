-- Treningsopplegg, fase T3e: evaluering av økter og øvelser, og lærdommer per lag.
-- Se claude/plan-treningsopplegg.md, kap. 14.
--
-- exercise_feedback: én trener sin evaluering av én øvelse (exercise_no 1–6)
-- eller av hele økta (exercise_no NULL) i én versjon av et opplegg. Hver
-- trener har én rad per øvelse og versjon, og kan endre den. rating er
-- 3 = bra, 2 = ok, 1 = dårlig. tags er faste nøkler (feedback_tags i
-- R/fct_feedback.R). actual_players gjelder hele økta. author er en
-- Spond-profil-ID. Fritekst sjekkes for sensitive ord og navn i appen.
--
-- team_learnings: lærdommer per lag (hovedgruppe), én rad per versjon; den
-- nyeste gjelder. Brukes fra T3e-4.
-- Regel for migrasjonsfiler: én SQL-setning per ";" på slutten av en linje.

CREATE TABLE exercise_feedback (
  id              integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  spond_group_id  text NOT NULL,
  spond_event_id  text NOT NULL,
  plan_id         integer NOT NULL REFERENCES training_plans (id) ON DELETE CASCADE,
  exercise_no     integer CHECK (exercise_no BETWEEN 1 AND 6),
  exercise_code   text CHECK (length(exercise_code) <= 60),
  rating          integer CHECK (rating BETWEEN 1 AND 3),
  tags            text[] NOT NULL DEFAULT '{}',
  actual_minutes  integer CHECK (actual_minutes BETWEEN 0 AND 240),
  actual_players  integer CHECK (actual_players BETWEEN 0 AND 200),
  body            text CHECK (length(body) <= 300),
  use_again       text CHECK (use_again IN ('yes', 'changes', 'no')),
  author          text NOT NULL,
  created_at      timestamptz NOT NULL DEFAULT now(),
  updated_at      timestamptz NOT NULL DEFAULT now()
);

CREATE UNIQUE INDEX exercise_feedback_one_idx ON exercise_feedback (plan_id, (coalesce(exercise_no, 0)), author);
CREATE INDEX exercise_feedback_event_idx ON exercise_feedback (spond_group_id, spond_event_id);
CREATE INDEX exercise_feedback_code_idx ON exercise_feedback (spond_group_id, exercise_code);

ALTER TABLE exercise_feedback ENABLE ROW LEVEL SECURITY;

CREATE TABLE team_learnings (
  id              integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  spond_group_id  text NOT NULL,
  body            text NOT NULL CHECK (length(body) <= 3000),
  source          text NOT NULL CHECK (source IN ('manual', 'ai')),
  created_by      text NOT NULL,
  created_at      timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX team_learnings_group_idx ON team_learnings (spond_group_id, created_at);

ALTER TABLE team_learnings ENABLE ROW LEVEL SECURITY;
