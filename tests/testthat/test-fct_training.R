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

# Similarity (kap. 15.4) ------------------------------------------------------------------

sim_bank <- function() {
  ref <- plan_validate(jsonlite::fromJSON(testthat::test_path("..", "..", "inst", "extdata", "referanse-okt.json"),
                                          simplifyVector = FALSE))
  e <- ref$ovelser[[1]]
  data.frame(code = c("rondo-to-ruter", "haien"), name = c(e$navn, "Haien og fiskene"),
             category = c("pasning_mottak", "oppvarming"), themes = I(list("Samhandling", character())),
             organisation = c(paste(unlist(e$organisering), collapse = "\n"), "Alle med ball i en rute."),
             execution = c(paste(unlist(e$gjennomforing), collapse = "\n"), "Haien tar baller."),
             drawing = c(drawing_json(e$tegning), NA), status = c("active", "active"), stringsAsFactors = FALSE)
}

test_that("words for comparing skip short and common words", {
  expect_equal(exercise_words("Spill 3 mot 1 og ballen i ruta!"), c("spill", "ruta"))
})

test_that("an exercise like one in the bank scores high, a different one low", {
  ref <- plan_validate(jsonlite::fromJSON(testthat::test_path("..", "..", "inst", "extdata", "referanse-okt.json"),
                                          simplifyVector = FALSE))
  e <- ref$ovelser[[1]]
  e$kategori <- "Pasning og mottak"
  sim <- exercise_similarity(e, sim_bank(), "Samhandling")
  expect_equal(sim$code[1], "rondo-to-ruter")
  expect_gt(sim$score[1], 0.9)
  expect_lt(sim$score[2], 0.3)
  other <- exercise_similarity(ref$ovelser[[3]], sim_bank(), "")
  expect_lt(other$score[1], exercise_similarity_threshold)
  b <- sim_bank(); b$status[1] <- "archived"
  expect_equal(exercise_similarity(e, b)$code, "haien")
})

test_that("bank candidates: new exercises are saved unless similar, adjusted ones are not by default", {
  ref <- plan_validate(jsonlite::fromJSON(testthat::test_path("..", "..", "inst", "extdata", "referanse-okt.json"),
                                          simplifyVector = FALSE))
  ref$tema <- "Samhandling"
  ref$ovelser[[1]]$kategori <- "Pasning og mottak"        # like the bank's rondo
  ref$ovelser[[2]]$kilde <- "justert"; ref$ovelser[[2]]$basert_pa <- "haien"
  ref$ovelser[[3]]$kilde <- "bank"
  cands <- bank_candidates(ref, sim_bank())
  expect_length(cands, 2)
  expect_equal(cands[[1]]$match$code, "rondo-to-ruter")
  expect_equal(cands[[1]]$default, "skip")
  expect_equal(cands[[2]]$kilde, "justert")
  expect_equal(cands[[2]]$default, "skip")
  ref$ovelser[[1]]$kode <- "rondo-to-ruter"                # already in the bank: not a candidate
  expect_length(bank_candidates(ref, sim_bank()), 1)
  ref$ovelser[[2]]$kilde <- "ny"
  expect_equal(bank_candidates(ref, sim_bank())[[1]]$default, "save")
})

test_that("names of members are found in free text, whole words only", {
  m <- data.frame(first_name = c("Emma", "Per", "Ola Martin", "Jo"), last_name = c("Haugen", "Berg", "Lie", "Ås"))
  n <- member_names(m)
  expect_true(all(c("Emma Haugen", "Per Berg", "Emma", "Ola Martin", "Ola", "Martin") %in% n))
  expect_false("Per" %in% n)              # also a common word
  expect_false("Jo" %in% n)               # too short
  expect_equal(text_names_found("emma må stå i mål", n), "Emma")
  expect_equal(text_names_found("2 per gruppe, Per Berg er keeper", n), "Per Berg")
  expect_length(text_names_found("Emmaus og martinsdag", n), 0)
  expect_length(text_names_found("Mer avslutning.", n), 0)
  expect_length(text_names_found("Jo flere, jo bedre", n), 0)
  expect_match(names_problem(c("Fint.", "Ola trenger mer ballkontakt"), n), "«Ola»")
  expect_null(names_problem("Mer pasningsspill", n))
  expect_length(member_names(NULL), 0)
})
