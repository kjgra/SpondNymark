# Maintenance, archive and clean-up (T4, plan kap. 7).

ref_plan <- function() {
  plan_validate(jsonlite::read_json(testthat::test_path("..", "..", "inst", "extdata", "referanse-okt.json"),
                                    simplifyVector = FALSE))
}

# Versions 1..n of a plan for one training, made `days` ago.
add_versions <- function(con, acc, group, event, n, days) {
  p <- ref_plan()
  ids <- vapply(seq_len(n), function(i) ds_save_plan(con, acc, group, event, p, "P1")$id, 0L)
  DBI::dbExecute(con, paste0("UPDATE training_plans SET created_at = now() - interval '", days, " days'
                              WHERE spond_group_id = '", group, "' AND spond_event_id = '", event, "'"))
  ids
}

test_that("the month 12 months back starts on the first, in Oslo time", {
  expect_equal(format(maintenance_month_start(as.POSIXct("2026-10-11 14:00", tz = "Europe/Oslo")), "%Y-%m-%d %H:%M"),
               "2025-10-01 00:00")
  expect_equal(format(maintenance_month_start(as.POSIXct("2026-01-31 23:30", tz = "Europe/Oslo")), "%Y-%m-%d"),
               "2025-01-01")
})

test_that("a ZIP without compression can be read, with UTF-8 names", {
  dir <- withr::local_tempdir()
  writeLines("hei på deg", file.path(dir, "a.txt"))
  writeBin(as.raw(0:255), file.path(dir, "b.bin"))
  zf <- file.path(dir, "test.zip")
  zip_store(file.path(dir, c("a.txt", "b.bin")), c("lag/øvelse.txt", "b.bin"), zf)
  lst <- utils::unzip(zf, list = TRUE)
  expect_equal(lst$Name, c("lag/øvelse.txt", "b.bin"))
  out <- withr::local_tempdir()
  utils::unzip(zf, exdir = out)
  expect_equal(readLines(file.path(out, "lag", "øvelse.txt"), encoding = "UTF-8"), "hei på deg")
  expect_equal(readBin(file.path(out, "b.bin"), "raw", 300), as.raw(0:255))
  if (nzchar(Sys.which("unzip"))) {
    expect_equal(system2("unzip", c("-tq", shQuote(zf)), stdout = FALSE, stderr = FALSE), 0L)   # CRC checked
  }
})

test_that("maintenance deletes intermediate versions after 30 days and sums old KI use per month", {
  con <- local_test_db(); acc <- test_access()
  old <- add_versions(con, acc, "G1", "E-old", 5, 40)      # versions 1-5, 40 days old
  ds_approve_plan(con, acc, old[3], "P1")                  # version 3 approved
  recent <- add_versions(con, acc, "G1", "E-new", 3, 5)    # too new to touch
  DBI::dbExecute(con, paste0("UPDATE training_plans SET status = 'deleted' WHERE id = ", old[5]))   # the last, deleted
  for (i in 1:3) {
    ds_log_ai_usage(con, acc, "G1", "P1", list(kind = "draft", model = "claude-haiku-5-5", input_tokens = 100,
                                                output_tokens = 10, cost_usd = 0.01, status = if (i == 3) "error" else "ok"))
  }
  ds_log_ai_usage(con, acc, "G1", "P1", list(kind = "draft", model = "claude-haiku-5-5", cost_usd = 0.02, status = "ok"))
  DBI::dbExecute(con, "UPDATE ai_usage SET created_at = now() - interval '14 months' WHERE id <= 3")

  res <- ds_run_maintenance(con)
  left <- DBI::dbGetQuery(con, "SELECT spond_event_id, version FROM training_plans ORDER BY spond_event_id, version")
  expect_equal(left$version[left$spond_event_id == "E-old"], c(1L, 3L))   # first and approved; 2, 4 and deleted 5 gone
  expect_equal(left$version[left$spond_event_id == "E-new"], 1:3)
  expect_equal(res$versions, 3L)
  expect_equal(res$ai_rows, 3L)
  m <- DBI::dbGetQuery(con, "SELECT n, n_error, input_tokens::integer AS inp, cost_usd::float8 AS usd FROM ai_usage_monthly")
  expect_equal(c(m$n, m$n_error, m$inp), c(3L, 1L, 300L))
  expect_equal(m$usd, 0.03)
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*)::integer AS n FROM ai_usage")$n, 1L)
  expect_match(maintenance_result_text(res), "^3 mellomversjoner slettet, 3 KI-kall fra før .* summert per måned\\.$")
  expect_equal(maintenance_result_text(list(versions = 0L, ai_rows = 0L, ai_before = Sys.time())),
               "Ingen mellomversjoner å slette.")
})

test_that("the maintenance runs at most once a day unless forced, and says what it did", {
  con <- local_test_db(); acc <- test_access()
  expect_type(maintenance_run(con, "oppstart"), "list")
  expect_null(maintenance_run(con, "oppstart"))
  st <- ds_maintenance_state(con, acc)
  expect_equal(st$run_by, "oppstart")
  expect_equal(st$last_result, "Ingen mellomversjoner å slette.")
  expect_type(maintenance_run(con, "P1", force = TRUE), "list")
  expect_equal(ds_maintenance_state(con, acc)$run_by, "P1")
  expect_type(maintenance_run(con, "oppstart", now = Sys.time() + 25 * 3600), "list")
  # Without a database URL nothing starts
  expect_false(maintenance_on_start(url = ""))
})

test_that("old trainings are archived as ZIP and deleted only when the superadmin confirms", {
  con <- local_test_db(); acc <- test_access()
  ids <- add_versions(con, acc, "G1", "E-old", 2, 400)
  ds_approve_plan(con, acc, ids[1], "P1")
  add_versions(con, acc, "G1", "E-new", 1, 10)
  p <- ref_plan()
  DBI::dbExecute(con, paste0("INSERT INTO training_plan_comments (spond_group_id, spond_event_id, plan_id, author, body)
                              VALUES ('G1', 'E-old', ", ids[2], ", 'P2', 'Mer pasning')"))
  ds_save_feedback(con, acc, ids[1], "P2", feedback_validate(list(list(exercise_no = 1, rating = "3", body = "Bra"),
                                                          list(exercise_no = NA, actual_players = "10")), p),
                   registered = 12L)

  pend <- ds_archive_pending(con, acc)
  expect_equal(pend$spond_event_id, "E-old")
  expect_equal(pend$n_versions, 2L)
  expect_match(archive_period_text(pend$last_at), "^[a-z.]+ 20[0-9]{2}$")

  data <- ds_archive_data(con, acc, pend)
  expect_equal(nrow(data$plans), 2)
  expect_equal(data$comments$body, "Mer pasning")
  expect_false("author" %in% c(names(data$comments), names(data$feedback)))
  dir <- withr::local_tempdir()
  zf <- file.path(dir, "arkiv.zip")
  made <- character()
  fake_pdf <- function(plan, path, ...) {
    made <<- c(made, plan)
    writeBin(charToRaw("%PDF-1.7 test"), path)
  }
  w <- archive_write(zf, data, pend, c(G1 = "Gutter 2016"), make_pdf = fake_pdf)
  expect_equal(c(w$n_events, w$n_versions, w$n_pdf_failed), c(1L, 2L, 0L))
  expect_length(made, 1)                                   # the approved version
  lst <- utils::unzip(zf, list = TRUE)$Name
  expect_true("LES-MEG.txt" %in% lst)
  folder <- paste0("gutter-2016/", format(pend$last_at, "%Y-%m-%d", tz = "Europe/Oslo"), "-Eold/")
  expect_true(all(paste0(folder, c("data.json", "opplegg-versjon-1.pdf")) %in% lst))
  out <- withr::local_tempdir()
  utils::unzip(zf, exdir = out)
  j <- jsonlite::read_json(file.path(out, folder, "data.json"))
  expect_equal(j$lag, "Gutter 2016")
  expect_length(j$versjoner, 2)
  expect_equal(j$versjoner[[1]]$opplegg$tittel, p$tittel)
  expect_equal(j$kommentarer[[1]]$tekst, "Mer pasning")
  session <- Filter(function(e) is.null(e$ovelse), j$evalueringer)[[1]]
  expect_equal(c(session$antall_kom, session$antall_pameldt), c(10L, 12L))
  expect_equal(Filter(function(e) !is.null(e$ovelse), j$evalueringer)[[1]]$tekst, "Bra")
  expect_false(grepl("P2", paste(readLines(file.path(out, folder, "data.json")), collapse = "")))
  # A PDF that fails gives a note instead
  w2 <- archive_write(file.path(dir, "b.zip"), data, pend, make_pdf = function(...) stop("ingen quarto"))
  expect_equal(w2$n_pdf_failed, 1L)
  expect_true(any(grepl("pdf-mangler.txt$", utils::unzip(file.path(dir, "b.zip"), list = TRUE)$Name)))

  # Only the superadmin, only trainings in the archive that are still old
  expect_error(ds_archive_confirm(con, acc, list(superadmin = FALSE, admin = TRUE), archive_keys(pend), "P1"),
               "superadmin")
  expect_error(ds_archive_confirm(con, acc, list(superadmin = TRUE), "G1|E-new", "P1"), "Ingen av oppleggene")
  res <- ds_archive_confirm(con, acc, list(superadmin = TRUE), c(archive_keys(pend), "G1|E-new"), "P1")
  expect_equal(c(res$n_events, res$n_versions), c(1L, 2L))
  left <- DBI::dbGetQuery(con, "SELECT spond_event_id FROM training_plans")$spond_event_id
  expect_equal(left, "E-new")
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*)::integer AS n FROM exercise_feedback")$n, 0L)
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*)::integer AS n FROM training_plan_comments")$n, 0L)
  log <- ds_maintenance_state(con, acc)$archives
  expect_equal(c(log$n_events, log$n_versions), c(1L, 2L))
  expect_equal(log$confirmed_by, "P1")
  expect_equal(nrow(ds_archive_pending(con, acc)), 0)
  # Other groups: nothing
  other <- data.frame(id = "G9", name = "Annet", roles = "Trener", n_subgroups = 0L)
  expect_equal(nrow(ds_maintenance_state(con, other)$archives), 0)
})
