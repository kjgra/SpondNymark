-- Treningsopplegg, fase T3e-3: antall påmeldte spillere på evalueringen av
-- hele økta (exercise_no NULL), så appen kan regne ut faktisk oppmøte mot
-- påmeldt («Erfaringer fra laget» i prompten). Settes av appen fra Spond når
-- evalueringen lagres (spillere som har sagt ja, ikke trenerne). Bare et tall.
-- Regel for migrasjonsfiler: én SQL-setning per ";" på slutten av en linje.

ALTER TABLE exercise_feedback ADD COLUMN registered_players integer CHECK (registered_players BETWEEN 0 AND 200);
