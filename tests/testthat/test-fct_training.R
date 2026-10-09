test_that("exercise codes are stable, readable and unique in the group", {
  expect_equal(exercise_code("3 mot 1 i to ruter"), "3-mot-1-i-to-ruter")
  expect_equal(exercise_code("  Pasning på tvers – Ærlig øving! "), "pasning-pa-tvers-aerlig-oving")
  expect_equal(exercise_code("!!!"), "ovelse")
  expect_equal(exercise_code("Rondo", taken = c("rondo", "rondo-2")), "rondo-3")
  long <- exercise_code(strrep("ab ", 40))
  expect_lte(nchar(long), 50)
  expect_match(long, "^[a-z0-9]+(-[a-z0-9]+)*$")
})

test_that("an exercise is cleaned and checked", {
  v <- exercise_validate(list(name = "  Rondo  4 mot 1 ", category = "pasning_mottak",
                              themes = c("Samhandling", "samhandling", " ", "Pasning"),
                              min_players = " 5", max_players = "", area = " 10 x 10 m "))
  expect_equal(v$name, "Rondo 4 mot 1")
  expect_equal(v$themes, c("Samhandling", "Pasning"))
  expect_equal(v$min_players, "5")
  expect_equal(v$max_players, "")
  expect_equal(v$area, "10 x 10 m")
  expect_equal(v$harder, "")                      # missing fields are empty
  expect_error(exercise_validate(list(name = "", category = "angrep")), "Navnet")
  expect_error(exercise_validate(list(name = "X", category = "tull")), "kategori")
  expect_error(exercise_validate(list(name = "X", category = "angrep", min_players = "2,5")), "helt tall")
  expect_error(exercise_validate(list(name = "X", category = "angrep", min_players = "8", max_players = "4")),
               "mindre enn")
  expect_error(exercise_validate(list(name = "X", category = "angrep", nff_url = "http://x.no")), "https")
  expect_error(exercise_validate(list(name = "X", category = "angrep", easier = strrep("a", 1001))), "maks 1000")
})

test_that("the season plan needs a theme for every description and no sensitive words", {
  v <- season_validate(2026, c("Ballmestring", rep("", 8), " Samhandling ", "", ""),
                       c("", "", "", "", "", "", "", "", "", "Spille på lag", "", ""))
  expect_equal(v$month, c(1L, 10L))
  expect_equal(v$theme, c("Ballmestring", "Samhandling"))
  expect_equal(v$description, c("", "Spille på lag"))
  expect_equal(nrow(season_validate(2026, rep("", 12))), 0)
  expect_error(season_validate(2026, rep("", 12), c("x", rep("", 11))), "januar")
  expect_error(season_validate(1999, rep("", 12)), "år")
  expect_error(season_validate(2026, c("Mange skader", rep("", 11))), "sensitive")
})

test_that("team settings are cleaned and checked", {
  s <- team_settings_validate(list(age_group = " G10 ", session_minutes = "75", pitch = "Halv 7er-bane"))
  expect_equal(s$age_group, "G10")
  expect_equal(s$session_minutes, "75")
  expect_equal(s$equipment, "")
  expect_named(s, c("age_group", "session_minutes", "pitch", "equipment", "principles"))
  expect_error(team_settings_validate(list(session_minutes = "5")), "15 til 240")
  expect_error(team_settings_validate(list(principles = "Ola er syk")), "sensitive")
})
