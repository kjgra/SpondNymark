# KI (T3a). No test calls the real API: the HTTP step is replaced by a fake.

test_bank <- function() {
  data.frame(
    id = 1:3, code = c("rondo-3-mot-1", "kontring-3-mot-2", "gammel-ovelse"),
    name = c("Rondo 3 mot 1", "Kontring 3 mot 2", "Gammel øvelse"),
    category = c("pasning_mottak", "angrep", "annet"),
    themes = I(list("Samhandling", character(), character())),
    min_players = c(4L, NA, NA), max_players = c(12L, NA, NA), duration_minutes = c(15L, NA, NA),
    area = "", organisation = c("To ruter 12 × 12 m.", "Halv bane.", ""),
    execution = c("Hold ballen mot forsvareren.\nFem pasninger gir poeng.", "", ""),
    learning_points = c("Ut av pasningsskyggen.", "", ""), questions = "", easier = "", harder = "",
    nff_url = "", drawing = NA_character_, source = "manual", based_on = c("", "", ""),
    status = c("active", "candidate", "archived"), stringsAsFactors = FALSE)
}

# Money and limits -----------------------------------------------------------------------

test_that("the cost follows the price table", {
  u <- list(input_tokens = 1e6, output_tokens = 1e6, cache_read_tokens = 1e6, cache_write_tokens = 1e6)
  expect_equal(ai_cost(u, "claude-haiku-5-5"), 0.10 + 0.50 + 0.01 + 0.125)
  expect_equal(ai_cost(u, "claude-sonnet-5-5"), 2 + 10 + 0.10 + 2.50)
  expect_equal(ai_cost(list(input_tokens = 2000, output_tokens = 6000), "claude-haiku-5-5"), 0.0032)
  expect_error(ai_cost(u, "gpt"), "Ukjent modell")
})

test_that("amounts are shown in øre or kroner", {
  expect_equal(ai_format_nok(0.0032, 10), "ca. 3 øre")
  expect_equal(ai_format_nok(0.0001, 10), "ca. 1 øre")
  expect_equal(ai_format_nok(0.08, 10), "ca. 80 øre")
  expect_equal(ai_format_nok(0.12, 10), "ca. 1,20 kr")
})

test_that("the month starts at midnight in Oslo", {
  now <- as.POSIXct("2026-11-01 00:30", tz = "Europe/Oslo")
  expect_equal(format(ai_month_start(now), "%Y-%m-%d %H:%M %Z"), "2026-11-01 00:00 CET")
  expect_equal(format(ai_month_start(as.POSIXct("2026-10-31 23:30:00", tz = "UTC")), "%Y-%m-%d"), "2026-11-01")
})

test_that("the budget stops at the group limit and the total limit", {
  s <- ai_settings_default()
  expect_true(ai_budget(list(group_usd = 1, total_usd = 1), s)$ok)
  b <- ai_budget(list(group_usd = 3, total_usd = 3), s)            # 30 kr
  expect_false(b$ok)
  expect_match(b$message, "30,00 av 30 kr")
  b <- ai_budget(list(group_usd = 0, total_usd = 8), s)
  expect_false(b$ok)
  expect_match(b$message, "hele appen")
})

test_that("settings are checked", {
  ok <- ai_settings_validate(list(model = "claude-sonnet-5-5", group_limit_nok = "50", total_limit_usd = 9,
                                  max_new_exercises = 1, usd_nok = 9.6))
  expect_equal(ok$group_limit_nok, 50)
  expect_identical(ok$max_new_exercises, 1L)
  base <- ai_settings_default()
  expect_error(ai_settings_validate(modifyList(base, list(model = "x"))), "Ukjent modell")
  expect_error(ai_settings_validate(modifyList(base, list(max_new_exercises = 1.5))), "helt tall")
  expect_error(ai_settings_validate(modifyList(base, list(group_limit_nok = -1))), "Grensen per lag")
})

# Prompt ------------------------------------------------------------------------------------

test_that("the schema only uses what structured output supports", {
  walk <- function(x) {
    if (!is.list(x)) return(invisible())
    expect_false(any(c("minimum", "maximum", "minLength", "maxLength", "maxItems", "pattern") %in% names(x)))
    if (identical(x$type, "object")) {
      expect_false(x$additionalProperties)
      expect_setequal(unlist(x$required), names(x$properties))   # no optional fields
    }
    for (v in x) walk(v)
  }
  walk(ai_plan_schema())
  js <- jsonlite::toJSON(ai_plan_schema(), auto_unbox = TRUE)
  expect_match(js, '"additionalProperties":false', fixed = TRUE)
  expect_match(js, '"enum":["bank","justert","ny"]', fixed = TRUE)
})

test_that("the system prompt covers the categories and has a worked example", {
  st <- ai_system_text()
  for (k in names(exercise_categories)) expect_match(st, paste0("`", k, "`"), fixed = TRUE)
  expect_match(st, "# Eksempel på et godt svar", fixed = TRUE)
  expect_identical(st, ai_system_text())    # the same every time, so it can be cached
  r <- ai_plan_from_answer(example_text(), test_bank()[0, ], list(theme = "Samhandling", minutes = 65))
  expect_length(r$plan$ovelser, 3)
  expect_equal(plan_schedule(r$plan)$total, 65)
  expect_length(r$plan$ovelser[[1]]$tegning$skisser, 2)
})

test_that("the catalogue puts the theme first and leaves out archived exercises", {
  cat <- ai_catalogue(test_bank(), "Samhandling")
  lines <- grep("^- ", strsplit(cat, "\n")[[1]], value = TRUE)
  expect_length(lines, 2)
  expect_match(lines[1], "^- rondo-3-mot-1 \\| Rondo 3 mot 1 \\| pasning_mottak \\| tema: Samhandling \\| 4–12 spillere \\| 15 min")
  expect_match(lines[1], "kort: Hold ballen mot forsvareren. Fem pasninger", fixed = TRUE)
  expect_match(lines[2], "| kandidat", fixed = TRUE)
  expect_false(grepl("gammel", cat))
  expect_match(ai_catalogue(test_bank()[0, ], ""), "Banken er tom")
  expect_length(grep("^- ", strsplit(ai_catalogue(test_bank(), "", max = 1), "\n")[[1]]), 1)
  b <- test_bank(); b$based_on[2] <- "rondo-3-mot-1"
  expect_match(ai_catalogue(b), "variant av rondo-3-mot-1", fixed = TRUE)
})

test_that("the order has counts but no names, and checks the trainer's wish", {
  ctx <- list(start = as.POSIXct("2026-10-15 17:30", tz = "Europe/Oslo"), minutes = 75, theme = "Samhandling",
              team = list(age_group = "G10", pitch = "", equipment = "Små mål"), n_players = 18,
              group_sizes = c(6L, 6L, 6L), wish = "Mer avslutning på mål.", max_new = 1)
  o <- ai_order_text(ctx)
  expect_match(o, "torsdag 15. oktober 2026 kl. 17:30", fixed = TRUE)
  expect_match(o, "Grupper i godkjent gruppeforslag: 3 (6, 6, 6 spillere)", fixed = TRUE)
  expect_match(o, "Maks nye øvelser: 1", fixed = TRUE)
  expect_false(grepl("Bane:", o))                   # empty settings are left out
  expect_error(ai_order_text(modifyList(ctx, list(wish = "Ola er skadet i kneet"))), "sensitive")
  expect_error(ai_order_text(modifyList(ctx, list(wish = strrep("a", 1001)))), "maks 1000")
})

test_that("the request caches the system prompt and the catalogue", {
  b <- ai_request_body("claude-haiku-5-5", "# Øvelsesbanken", "# Bestilling", system = "Instruks")
  expect_length(b$system, 2)
  expect_equal(b$system[[1]]$cache_control$type, "ephemeral")
  expect_equal(b$system[[2]]$cache_control$type, "ephemeral")
  expect_equal(b$output_config$format$type, "json_schema")
  expect_equal(b$messages[[1]]$content, "# Bestilling")
  js <- jsonlite::toJSON(b, auto_unbox = TRUE, null = "null")
  expect_match(js, '"max_tokens":16000', fixed = TRUE)
})

# Answer -------------------------------------------------------------------------------------

test_that("API responses and errors are read", {
  r <- ai_read_response(fake_answer("{}"))
  expect_true(r$ok)
  expect_equal(r$usage$cache_write_tokens, 7000)
  expect_equal(ai_read_response(list(status = NA_integer_, body = NULL))$error_code, "nettverk")
  err <- function(status, type, msg = "x") list(status = status, body = list(type = "error", error = list(type = type, message = msg)))
  expect_match(ai_read_response(err(401L, "authentication_error"))$message, "API-nøkkelen")
  expect_equal(ai_read_response(err(529L, "overloaded_error"))$error_code, "overloaded_error")
  expect_equal(ai_read_response(err(400L, "invalid_request_error", "Your credit balance is too low"))$error_code, "grense")
  expect_equal(ai_read_response(list(status = 500L, body = NULL))$error_code, "http_500")
  expect_equal(ai_read_response(fake_answer("{}", stop_reason = "refusal"))$error_code, "refusal")
  expect_equal(ai_read_response(fake_answer("{\"tit", stop_reason = "max_tokens"))$error_code, "max_tokens")
  expect_equal(ai_read_response(fake_answer(""))$error_code, "tomt_svar")
})

test_that("bank exercises are filled in from the bank", {
  txt <- example_text(function(a) {
    a$ovelser[[1]] <- list(kilde = "bank", kode = "rondo-3-mot-1", fokus = "Gi ballfører to valg.",
                           tilpasning = list("Færre: 4 mot 2 i én rute."), navn = "", organisering = list())
    a
  })
  r <- ai_plan_from_answer(txt, test_bank(), list(theme = "Samhandling"))
  e <- r$plan$ovelser[[1]]
  expect_equal(e$kilde, "bank")
  expect_equal(e$navn, "Rondo 3 mot 1")
  expect_equal(e$fokus, "Gi ballfører to valg.")
  expect_equal(e$tilpasning, list("Færre: 4 mot 2 i én rute."))
  expect_equal(e$organisering, list("To ruter 12 × 12 m."))
})

test_that("unknown codes, clashes and bad drawings are handled without losing the answer", {
  txt <- example_text(function(a) {
    a$ovelser[[1]]$kilde <- "bank"; a$ovelser[[1]]$kode <- "finnes-ikke"            # has a name: kept as new
    a$ovelser[[2]]$kilde <- "justert"; a$ovelser[[2]]$basert_pa <- "ukjent"          # unknown base: new
    a$ovelser[[2]]$kode <- "rondo-3-mot-1"                                             # clashes with the bank
    a$ovelser[[2]]$naermeste_kode <- "kontring-3-mot-2"
    a$ovelser[[3]]$tegning_json <- "{\"skisser\": []}"
    a$ovelser[[3]]$naermeste_kode <- "ukjent"
    a
  })
  r <- ai_plan_from_answer(txt, test_bank(), list(theme = "Samhandling", minutes = 90), max_new = 2)
  ex <- r$plan$ovelser
  expect_equal(vapply(ex, `[[`, "", "kilde"), rep("ny", 3))
  expect_false(ex[[2]]$kode %in% test_bank()$code)
  expect_equal(ex[[2]]$basert_pa, "")
  expect_equal(ex[[2]]$naermeste_kode, "kontring-3-mot-2")
  expect_equal(ex[[3]]$naermeste_kode, "")
  expect_null(ex[[3]]$tegning)
  expect_true(any(grepl("Øvelse 1: koden «finnes-ikke»", r$warnings)))
  expect_true(any(grepl("Øvelse 3: tegningen kunne ikke brukes", r$warnings)))
  expect_true(any(grepl("3 nye øvelser \\(grensen er 2\\)", r$warnings)))
  expect_true(any(grepl("Tidsplanen er 65 min, men økta er 90 min", r$warnings)))
})

test_that("a justert exercise keeps its base, and an empty answer is an error", {
  txt <- example_text(function(a) {
    a$ovelser[[1]]$kilde <- "justert"; a$ovelser[[1]]$basert_pa <- "rondo-3-mot-1"
    a$ovelser[[1]]$naermeste_kode <- "rondo-3-mot-1"
    a
  })
  e <- ai_plan_from_answer(txt, test_bank())$plan$ovelser[[1]]
  expect_equal(e$basert_pa, "rondo-3-mot-1")
  expect_equal(e$naermeste_kode, "")              # only for new exercises
  expect_error(ai_plan_from_answer("ikke json", test_bank()), "gyldig JSON")
  none <- example_text(function(a) { a$ovelser <- list(list(kilde = "bank", kode = "x", navn = "")); a })
  expect_error(ai_plan_from_answer(none, test_bank()), "ingen øvelser")
})

test_that("the estimate uses the free token count", {
  prep <- list(model = "claude-haiku-5-5", settings = ai_settings_default(),
               body = ai_request_body("claude-haiku-5-5", "k", "o", system = "s"))
  seen <- NULL
  http <- function(path, body) { seen <<- list(path = path, body = body); list(status = 200L, body = list(input_tokens = 8000)) }
  est <- ai_estimate(prep, typical_output = NA, http = http)
  expect_equal(seen$path, "/v1/messages/count_tokens")
  expect_null(seen$body$max_tokens)
  expect_equal(est$usd, (8000 * 0.10 + 6000 * 0.50) / 1e6)
  expect_equal(est$text, "ca. 4 øre")
  expect_null(ai_estimate(prep, http = function(path, body) list(status = 500L, body = NULL)))
})

# With the database ----------------------------------------------------------------------------

test_that("KI settings have defaults and only the superadmin may change them", {
  con <- local_test_db()
  expect_equal(ds_get_ai_settings(con)$model, "claude-haiku-5-5")
  s <- modifyList(ai_settings_default(), list(model = "claude-sonnet-5-5", group_limit_nok = 50))
  expect_error(ds_save_ai_settings(con, list(superadmin = FALSE, admin = TRUE), "P1", s), "superadmin")
  ds_save_ai_settings(con, list(superadmin = TRUE), "P1", s)
  ds_save_ai_settings(con, list(superadmin = TRUE), "P1", s)          # updates the one row
  got <- ds_get_ai_settings(con)
  expect_equal(got$model, "claude-sonnet-5-5")
  expect_equal(got$group_limit_nok, 50)
  expect_identical(got$max_new_exercises, 2L)
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*)::integer AS n FROM ai_settings")$n, 1L)
})

test_that("usage is logged and summed per month", {
  con <- local_test_db(); acc <- test_access()
  u <- list(kind = "draft", model = "claude-haiku-5-5", input_tokens = 1000, output_tokens = 6000,
            cache_read_tokens = 0, cache_write_tokens = 7000, cost_usd = 0.0049, duration_ms = 31000,
            status = "ok", error_code = "")
  id <- ds_log_ai_usage(con, acc, "G1", "P1", u, event_id = "E1")
  ds_log_ai_usage(con, acc, "G1", "P1", modifyList(u, list(output_tokens = 4000)))
  DBI::dbExecute(con, "INSERT INTO ai_usage (spond_profile_id, spond_group_id, kind, model, cost_usd, status)
                       VALUES ('P9', 'G2', 'draft', 'claude-haiku-5-5', 1, 'ok')")
  DBI::dbExecute(con, "INSERT INTO ai_usage (spond_profile_id, spond_group_id, kind, model, cost_usd, status, created_at)
                       VALUES ('P1', 'G1', 'draft', 'claude-haiku-5-5', 5, 'ok', now() - interval '40 days')")
  sp <- ds_ai_spend(con, acc, "G1", Sys.time() - 3600)
  expect_equal(sp$group_usd, 2 * 0.0049)
  expect_equal(sp$total_usd, 1 + 2 * 0.0049)
  expect_error(ds_ai_spend(con, acc, "G2", Sys.time()), "ikke tilgang")
  expect_equal(ds_ai_typical_output(con, "draft", "claude-haiku-5-5"), 5000)   # median of 6000 and 4000
  expect_true(is.na(ds_ai_typical_output(con, "revision", "claude-haiku-5-5")))
})

test_that("a variant points to the base exercise, and deleting the base frees it", {
  con <- local_test_db(); acc <- test_access()
  base <- ds_save_exercise(con, acc, "G1", list(name = "Rondo", category = "pasning_mottak"), "P1")
  e <- list(kode = "rondo-to-touch", navn = "Rondo med to touch", kategori = "Pasning og mottak",
            basert_pa = "rondo")
  v1 <- ds_save_plan_exercise(con, acc, "G1", e, "P1", source = "ai", status = "candidate")
  e2 <- modifyList(e, list(kode = "rondo-to-touch-6-mot-2", basert_pa = "rondo-to-touch"))
  v2 <- ds_save_plan_exercise(con, acc, "G1", e2, "P1")
  x1 <- ds_get_exercise(con, acc, v1$id)
  expect_equal(x1$based_on, "rondo")
  expect_equal(x1$status, "candidate")
  expect_equal(ds_get_exercise(con, acc, v2$id)$based_on, "rondo")       # flat families
  expect_equal(ds_get_exercise(con, acc, base)$status, "active")
  expect_error(ds_save_plan_exercise(con, acc, "G1", e, "P1", status = "archived"), "Ugyldig status")
  ds_delete_exercise(con, acc, base)
  expect_equal(ds_get_exercise(con, acc, v1$id)$based_on, "")
})

test_that("a KI plan is prepared, sent and saved with its cost", {
  con <- local_test_db(); acc <- test_access()
  ctx <- list(minutes = 65, theme = "Samhandling", n_players = 24, wish = "")
  expect_error(ai_prepare(con, acc, list(ai = FALSE), "G1", ctx, key = "k"), "ikke tilgang til KI")
  expect_error(ai_prepare(con, acc, list(ai = TRUE), "G1", ctx, key = ""), "ANTHROPIC_API_KEY")
  ds_save_exercise(con, acc, "G1", list(name = "Rondo", category = "pasning_mottak", themes = "Samhandling"), "P1")

  prep <- ai_prepare(con, acc, list(ai = TRUE), "G1", ctx, key = "k")
  expect_match(prep$body$system[[2]]$text, "- rondo | Rondo", fixed = TRUE)
  expect_match(prep$body$messages[[1]]$content, "Maks nye øvelser: 2", fixed = TRUE)

  sent <- ai_send(prep, http = function(path, body) fake_answer(example_text()))
  res <- ai_finish(con, acc, "P1", prep, sent, event_id = "E1")
  expect_equal(res$version, 1L)
  expect_true(any(grepl("3 nye øvelser", res$warnings)))
  cost <- (1200 * 0.10 + 5000 * 0.50 + 7000 * 0.125) / 1e6
  expect_equal(res$cost_usd, cost)
  row <- DBI::dbGetQuery(con, "SELECT source, model, cost_usd::float8 AS cost FROM training_plans WHERE id = $1",
                         params = list(res$id))
  expect_equal(row$source, "ai")
  expect_equal(row$model, "claude-haiku-5-5")
  expect_equal(row$cost, round(cost, 5))
  log <- DBI::dbGetQuery(con, "SELECT plan_id, status, spond_event_id, cache_write_tokens FROM ai_usage")
  expect_equal(log$plan_id, res$id)
  expect_equal(log$status, "ok")
  expect_equal(log$cache_write_tokens, 7000L)
  expect_equal(ds_get_plan(con, acc, res$id)$plan$ovelser[[1]]$hvorfor_ny, "Banken hadde ingen øvelse for dette.")

  # A failed call is logged (it may have cost money) and stops with a message.
  bad <- ai_send(prep, http = function(path, body) fake_answer("ikke json"))
  expect_error(ai_finish(con, acc, "P1", prep, bad, event_id = "E1"), "KI-svaret kunne ikke brukes")
  log <- DBI::dbGetQuery(con, "SELECT status, error_code FROM ai_usage ORDER BY id")
  expect_equal(log$status, c("ok", "error"))
  expect_equal(log$error_code[2], "ugyldig_svar")

  # Over the limit: no call is prepared.
  DBI::dbExecute(con, "INSERT INTO ai_usage (spond_profile_id, spond_group_id, kind, model, cost_usd, status)
                       VALUES ('P1', 'G1', 'draft', 'claude-haiku-5-5', 3, 'ok')")
  expect_error(ai_prepare(con, acc, list(ai = TRUE), "G1", ctx, key = "k"), "av 30 kr")
})
