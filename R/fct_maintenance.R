#' Maintenance, archive and clean-up (phase T4, plan kap. 7)
#'
#' Rules (kap. 7.2):
#' - Intermediate versions of a plan are deleted after 30 days. The first,
#'   the last and the approved version of each training are kept. Their
#'   comments go with them.
#' - Plans for trainings older than 12 months are never deleted
#'   automatically: the superadmin downloads them as a ZIP archive and
#'   confirms that it is saved (kap. 7.4). The evaluations go with the plans.
#'   The date of a training is taken from its newest plan version (the plans
#'   do not store the date of the event; a plan is made shortly before).
#' - `ai_usage` older than 12 months (whole months) is summed per month,
#'   group, kind and model in `ai_usage_monthly`, without the trainer.
#' - Exercises, season plans, team settings and learnings are kept.
#'
#' The maintenance runs when the app starts, at most once a day (the row in
#' `maintenance` is claimed with one UPDATE, so two processes never run it
#' at the same time), and from «Kjør vedlikehold nå» in the admin panel.
#' @name fct_maintenance
#' @noRd
NULL

maintenance_rules <- list(version_days = 30, archive_days = 365, ai_months = 12)

# A time as text for a timestamptz parameter.
ts_param <- function(t) format(as.POSIXct(t), "%Y-%m-%d %H:%M:%OS6%z")

# The first day of the month `months` before the month of `now`, in Oslo time.
maintenance_month_start <- function(now = Sys.time(), months = maintenance_rules$ai_months) {
  lt <- as.POSIXlt(now, tz = "Europe/Oslo")
  m <- lt$year * 12 + lt$mon - months
  as.POSIXct(sprintf("%04d-%02d-01 00:00:00", m %/% 12 + 1900, m %% 12 + 1), tz = "Europe/Oslo")
}

#' Run the clean-up rules once
#' @return list(versions = intermediate versions deleted, ai_rows = KI calls
#'   summed per month, ai_before = the cut-off for those).
#' @noRd
ds_run_maintenance <- function(con, now = Sys.time()) {
  ai_before <- maintenance_month_start(now)
  ds_transaction(con, {
    versions <- ds_query(con, "
      WITH ends AS (
        SELECT spond_group_id, spond_event_id, min(version) AS first, max(version) AS last
          FROM training_plans GROUP BY spond_group_id, spond_event_id)
      DELETE FROM training_plans p USING ends e
       WHERE p.spond_group_id = e.spond_group_id AND p.spond_event_id = e.spond_event_id
         AND p.status <> 'approved'
         AND (p.status = 'deleted' OR p.version NOT IN (e.first, e.last))
         AND p.created_at < $1::timestamptz - make_interval(days => $2::integer)
      RETURNING p.id",
      list(ts_param(now), as.character(maintenance_rules$version_days)))
    ds_exec(con, "
      INSERT INTO ai_usage_monthly (month, spond_group_id, kind, model, n, n_error, input_tokens, output_tokens,
                                    cache_read_tokens, cache_write_tokens, cost_usd)
      SELECT date_trunc('month', created_at AT TIME ZONE 'Europe/Oslo')::date, spond_group_id, kind, model,
             count(*), count(*) FILTER (WHERE status = 'error'), sum(input_tokens), sum(output_tokens),
             sum(cache_read_tokens), sum(cache_write_tokens), sum(cost_usd)
        FROM ai_usage WHERE created_at < $1::timestamptz
       GROUP BY 1, 2, 3, 4
      ON CONFLICT (month, spond_group_id, kind, model) DO UPDATE
        SET n = ai_usage_monthly.n + EXCLUDED.n, n_error = ai_usage_monthly.n_error + EXCLUDED.n_error,
            input_tokens = ai_usage_monthly.input_tokens + EXCLUDED.input_tokens,
            output_tokens = ai_usage_monthly.output_tokens + EXCLUDED.output_tokens,
            cache_read_tokens = ai_usage_monthly.cache_read_tokens + EXCLUDED.cache_read_tokens,
            cache_write_tokens = ai_usage_monthly.cache_write_tokens + EXCLUDED.cache_write_tokens,
            cost_usd = ai_usage_monthly.cost_usd + EXCLUDED.cost_usd",
      list(ts_param(ai_before)))
    ai_rows <- ds_exec(con, "DELETE FROM ai_usage WHERE created_at < $1::timestamptz", list(ts_param(ai_before)))
    list(versions = nrow(versions), ai_rows = as.integer(ai_rows), ai_before = ai_before)
  })
}

# One line for the panel about what the maintenance did.
maintenance_result_text <- function(res) {
  parts <- c(
    if (res$versions == 0) "Ingen mellomversjoner å slette"
    else paste0(res$versions, if (res$versions == 1) " mellomversjon" else " mellomversjoner", " slettet"),
    if (res$ai_rows > 0) paste0(res$ai_rows, " KI-kall fra før ", tolower(month_names_nb[as.POSIXlt(res$ai_before)$mon + 1]),
                                " ", as.POSIXlt(res$ai_before)$year + 1900, " summert per måned"))
  paste0(paste(parts, collapse = ", "), ".")
}

#' Run the maintenance if it has not run the last day (or `force`)
#'
#' The row in `maintenance` is claimed first, so only one process runs it.
#' A failure is written to the row and the log, and is not retried until
#' the next day.
#' @param by Who started it: "oppstart" or a profile id.
#' @return The result (see `ds_run_maintenance()`), or NULL if it was not due.
#' @noRd
maintenance_run <- function(con, by = "oppstart", force = FALSE, now = Sys.time()) {
  claimed <- ds_query(con, "
    UPDATE maintenance SET last_run = $1::timestamptz, run_by = $2
     WHERE id = 1 AND ($3::boolean OR last_run IS NULL OR last_run < $1::timestamptz - interval '23 hours')
    RETURNING id", list(ts_param(now), substr(txt1(by), 1, 60), if (isTRUE(force)) "true" else "false"))
  if (nrow(claimed) == 0) return(NULL)
  res <- tryCatch(ds_run_maintenance(con, now), error = function(e) e)
  text <- if (inherits(res, "error")) {
    message("Vedlikehold feilet: ", conditionMessage(res))
    "Vedlikeholdet feilet. Se loggen på Connect."
  } else {
    maintenance_result_text(res)
  }
  ds_exec(con, "UPDATE maintenance SET last_result = $1 WHERE id = 1", list(text))
  if (inherits(res, "error")) stop(res)
  res
}

# When this R process last checked whether the maintenance is due.
.sn_maintenance <- new.env(parent = emptyenv())

#' Start the maintenance shortly after a session starts, at most once an
#' hour per R process (the database decides whether it is due)
#'
#' Runs a few seconds later with its own connection, so the start of the
#' app is not delayed. Does nothing without `SPONDNYMARK_DB_URL`.
#' @noRd
maintenance_on_start <- function(connect = ds_connect, delay = 5, now = Sys.time,
                                 url = Sys.getenv("SPONDNYMARK_DB_URL")) {
  last <- .sn_maintenance$checked
  if (!nzchar(url) || (!is.null(last) && difftime(now(), last, units = "hours") < 1)) return(invisible(FALSE))
  .sn_maintenance$checked <- now()
  later::later(function() {
    h <- db_handle(connect)
    on.exit(h$close(), add = TRUE)
    tryCatch(h$run(function(con) maintenance_run(con, "oppstart")),
             error = function(e) message("Vedlikehold ved oppstart feilet: ", conditionMessage(e)))
  }, delay)
  invisible(TRUE)
}

#' The last maintenance run and the last archive confirmations
#' @return list(last_run, run_by, last_result, archives = data.frame(...)).
#' @noRd
ds_maintenance_state <- function(con, access) {
  m <- ds_query(con, "SELECT last_run, coalesce(run_by, '') AS run_by, coalesce(last_result, '') AS last_result
                        FROM maintenance WHERE id = 1")
  a <- ds_query(con, "
    SELECT period_from, period_to, n_events, n_versions, confirmed_by, confirmed_at FROM archive_log
     WHERE spond_group_ids && $1::text[] ORDER BY confirmed_at DESC LIMIT 5",
    list(ds_text_array(access$id)))
  list(last_run = if (nrow(m)) m$last_run else as.POSIXct(NA), run_by = if (nrow(m)) m$run_by else "",
       last_result = if (nrow(m)) m$last_result else "", archives = a)
}

# Archive (kap. 7.4) ------------------------------------------------------------------

#' Trainings in the user's groups whose newest plan is older than 12 months
#' @return data.frame(spond_group_id, spond_event_id, last_at, n_versions),
#'   oldest first.
#' @noRd
ds_archive_pending <- function(con, access, now = Sys.time()) {
  if (!NROW(access)) return(data.frame(spond_group_id = character(), spond_event_id = character(),
                                       last_at = as.POSIXct(character()), n_versions = integer()))
  ds_query(con, "
    SELECT spond_group_id, spond_event_id, max(created_at) AS last_at, count(*)::integer AS n_versions
      FROM training_plans WHERE spond_group_id = ANY($1::text[])
     GROUP BY spond_group_id, spond_event_id
    HAVING max(created_at) < $2::timestamptz - make_interval(days => $3::integer)
     ORDER BY last_at, spond_group_id, spond_event_id",
    list(ds_text_array(access$id), ts_param(now), as.character(maintenance_rules$archive_days)))
}

# "okt. 2025" or "okt. 2025 – jan. 2026".
archive_period_text <- function(t) {
  if (!length(t)) return("")
  one <- function(x) paste(no_month(x), format(x, "%Y", tz = "Europe/Oslo"))
  a <- one(min(t))
  b <- one(max(t))
  if (a == b) a else paste(a, "–", b)
}

# The training keys as "group|event".
archive_keys <- function(d) paste(d$spond_group_id, d$spond_event_id, sep = "|")

#' Everything about the given trainings, without names or authors
#' @param pending Rows of `ds_archive_pending()`.
#' @return list(plans, comments, feedback) as data.frames.
#' @noRd
ds_archive_data <- function(con, access, pending) {
  for (g in unique(pending$spond_group_id)) assert_group_access(access, g)
  keys <- list(ds_text_array(pending$spond_group_id), ds_text_array(pending$spond_event_id))
  sel <- "(spond_group_id, spond_event_id) IN (SELECT * FROM unnest($1::text[], $2::text[]))"
  list(
    plans = ds_query(con, paste0("
      SELECT id, spond_group_id, spond_event_id, version, status, source, coalesce(model, '') AS model,
             cost_usd::float8 AS cost_usd, plan::text AS plan, created_at, approved_at
        FROM training_plans WHERE ", sel, " ORDER BY spond_group_id, spond_event_id, version"), keys),
    comments = ds_query(con, paste0("
      SELECT c.spond_group_id, c.spond_event_id, p.version, c.exercise_no, c.body, c.created_at
        FROM training_plan_comments c JOIN training_plans p ON p.id = c.plan_id
       WHERE (c.spond_group_id, c.spond_event_id) IN (SELECT * FROM unnest($1::text[], $2::text[]))
       ORDER BY c.created_at, c.id"), keys),
    feedback = ds_query(con, paste0("
      SELECT f.spond_group_id, f.spond_event_id, p.version, f.exercise_no, coalesce(f.exercise_code, '') AS exercise_code,
             f.rating, array_to_string(f.tags, ',') AS tags, f.actual_minutes, f.actual_players, f.registered_players,
             coalesce(f.body, '') AS body, coalesce(f.use_again, '') AS use_again, f.created_at
        FROM exercise_feedback f JOIN training_plans p ON p.id = f.plan_id
       WHERE (f.spond_group_id, f.spond_event_id) IN (SELECT * FROM unnest($1::text[], $2::text[]))
       ORDER BY f.created_at, f.id"), keys)
  )
}

# Folder-safe text: letters, digits and dashes.
archive_slug <- function(x) {
  x <- iconv(tolower(txt1(x)), "UTF-8", "ASCII//TRANSLIT", sub = "")
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("^-+|-+$", "", x)
  if (nzchar(x)) x else "lag"
}

#' Write the archive as a ZIP file
#'
#' One folder per training (`<lag>/<dato>-<arrangement>/`) with the PDF of
#' the approved version (otherwise the newest) and `data.json` with all
#' versions, comments and evaluations. No names: no groups on the PDF, and
#' no authors. A README at the top says what it is.
#' @param data `ds_archive_data()`.
#' @param pending `ds_archive_pending()` (the trainings to include).
#' @param group_names Named character vector, group id -> name.
#' @param make_pdf `plan_pdf()` (tests pass a fake).
#' @return list(path, n_events, n_versions, n_pdf_failed).
#' @noRd
archive_write <- function(path, data, pending, group_names = character(), make_pdf = plan_pdf, now = Sys.time()) {
  dir <- tempfile("arkiv-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  files <- character()
  failed <- 0L
  as_time <- function(x) if (length(x) && !is.na(x)) format(x, "%Y-%m-%dT%H:%M:%S%z", tz = "Europe/Oslo") else NA
  for (i in seq_len(nrow(pending))) {
    g <- pending$spond_group_id[i]
    ev <- pending$spond_event_id[i]
    gname <- if (g %in% names(group_names)) group_names[[g]] else g
    folder <- file.path(archive_slug(gname), paste0(format(pending$last_at[i], "%Y-%m-%d", tz = "Europe/Oslo"), "-",
                                                    gsub("[^A-Za-z0-9]", "", ev)))
    dir.create(file.path(dir, folder), recursive = TRUE, showWarnings = FALSE)
    pl <- data$plans[data$plans$spond_group_id == g & data$plans$spond_event_id == ev, , drop = FALSE]
    cm <- data$comments[data$comments$spond_group_id == g & data$comments$spond_event_id == ev, , drop = FALSE]
    fb <- data$feedback[data$feedback$spond_group_id == g & data$feedback$spond_event_id == ev, , drop = FALSE]
    versions <- lapply(seq_len(nrow(pl)), function(k) {
      list(versjon = pl$version[k], status = pl$status[k], kilde = pl$source[k], modell = pl$model[k],
           kostnad_usd = pl$cost_usd[k], laget = as_time(pl$created_at[k]), godkjent = as_time(pl$approved_at[k]),
           opplegg = jsonlite::fromJSON(pl$plan[k], simplifyVector = FALSE))
    })
    json <- list(
      lag = gname, spond_gruppe_id = g, spond_arrangement_id = ev, arkivert = as_time(now),
      versjoner = versions,
      kommentarer = lapply(seq_len(nrow(cm)), function(k) {
        list(versjon = cm$version[k], ovelse = if (is.na(cm$exercise_no[k])) NULL else cm$exercise_no[k],
             tekst = cm$body[k], tid = as_time(cm$created_at[k]))
      }),
      evalueringer = lapply(seq_len(nrow(fb)), function(k) {
        list(versjon = fb$version[k], ovelse = if (is.na(fb$exercise_no[k])) NULL else fb$exercise_no[k],
             kode = fb$exercise_code[k], vurdering = fb$rating[k],
             stikkord = if (nzchar(txt1(fb$tags[k]))) as.list(strsplit(fb$tags[k], ",", fixed = TRUE)[[1]]) else list(),
             faktisk_tid = fb$actual_minutes[k], antall_kom = fb$actual_players[k],
             antall_pameldt = fb$registered_players[k], tekst = fb$body[k], bruke_igjen = fb$use_again[k],
             tid = as_time(fb$created_at[k]))
      }))
    jf <- file.path(folder, "data.json")
    writeLines(enc2utf8(as.character(jsonlite::toJSON(json, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null",
                                                      digits = NA))),
               file.path(dir, jf), useBytes = TRUE)
    files <- c(files, jf)
    # The PDF of the approved version, otherwise the newest that is not deleted.
    live <- pl[pl$status != "deleted", , drop = FALSE]
    best <- if (any(live$status == "approved")) live[live$status == "approved", , drop = FALSE][1, ]
            else if (nrow(live)) live[nrow(live), , drop = FALSE]
    if (!is.null(best) && nrow(best)) {
      pf <- file.path(folder, paste0("opplegg-versjon-", best$version, ".pdf"))
      ok <- tryCatch({
        make_pdf(best$plan, file.path(dir, pf), footer = gname,
                 info = c(paste("Arkivert", format(now, "%d.%m.%Y", tz = "Europe/Oslo")), gname))
        TRUE
      }, error = function(e) {
        message("PDF i arkivet feilet: ", conditionMessage(e))
        FALSE
      })
      if (ok && file.exists(file.path(dir, pf))) {
        files <- c(files, pf)
      } else {
        failed <- failed + 1L
        nf <- file.path(folder, "pdf-mangler.txt")
        writeLines("PDF-en kunne ikke lages. Opplegget finnes i data.json.", file.path(dir, nf))
        files <- c(files, nf)
      }
    }
  }
  readme <- c(
    "Arkiv fra Nymark - Gruppeorganisering",
    "",
    paste0("Laget ", format(now, "%d.%m.%Y kl. %H:%M", tz = "Europe/Oslo"), ". ",
           nrow(pending), " treninger og ", nrow(data$plans), " versjoner av treningsopplegg."),
    "",
    "Én mappe per trening: PDF av godkjent versjon (ellers den nyeste) og data.json med alle versjoner,",
    "kommentarer og evalueringer. Ingen navn på spillere eller trenere.",
    "",
    "Datoen i mappenavnet er dagen den nyeste versjonen ble laget.")
  writeLines(readme, file.path(dir, "LES-MEG.txt"))
  files <- c("LES-MEG.txt", files)
  zip_store(file.path(dir, files), files, path)
  list(path = path, n_events = nrow(pending), n_versions = nrow(data$plans), n_pdf_failed = failed)
}

#' Delete the archived trainings, after the superadmin has confirmed that the
#' archive is saved, and log it
#'
#' Only trainings that are still older than 12 months are deleted. Comments
#' and evaluations go with the plans (ON DELETE CASCADE).
#' @param keys "group|event" for the trainings in the downloaded archive.
#' @return list(n_events, n_versions).
#' @noRd
ds_archive_confirm <- function(con, access, rights, keys, actor, now = Sys.time()) {
  if (!isTRUE(rights$superadmin)) ds_user_stop("Bare superadmin kan slette arkiverte opplegg.")
  pending <- ds_archive_pending(con, access, now)
  pending <- pending[archive_keys(pending) %in% keys, , drop = FALSE]
  if (!nrow(pending)) ds_user_stop("Ingen av oppleggene i arkivet venter på sletting.")
  ds_transaction(con, {
    gone <- ds_query(con, "
      DELETE FROM training_plans
       WHERE (spond_group_id, spond_event_id) IN (SELECT * FROM unnest($1::text[], $2::text[]))
      RETURNING id", list(ds_text_array(pending$spond_group_id), ds_text_array(pending$spond_event_id)))
    ds_exec(con, "
      INSERT INTO archive_log (spond_group_ids, period_from, period_to, n_events, n_versions, confirmed_by)
      VALUES ($1::text[], $2::date, $3::date, $4::integer, $5::integer, $6)",
      list(ds_text_array(unique(pending$spond_group_id)),
           format(min(pending$last_at), "%Y-%m-%d", tz = "Europe/Oslo"),
           format(max(pending$last_at), "%Y-%m-%d", tz = "Europe/Oslo"),
           as.character(nrow(pending)), as.character(nrow(gone)), actor))
    list(n_events = nrow(pending), n_versions = nrow(gone))
  })
}

# ZIP ---------------------------------------------------------------------------------

#' Write a ZIP file without compression (no zip program needed on the server)
#'
#' Plain "stored" entries with CRC-32 and UTF-8 names. PDFs are compressed
#' already, and the JSON files are small.
#' @param paths Files on disk.
#' @param names Their names in the archive (with "/" between folders).
#' @noRd
zip_store <- function(paths, names, zipfile) {
  u16 <- function(x) {
    x <- as.numeric(x)
    as.raw(c(x %% 256, (x %/% 256) %% 256))
  }
  u32 <- function(x) {
    x <- as.numeric(x)
    as.raw(c(x %% 256, (x %/% 256) %% 256, (x %/% 65536) %% 256, (x %/% 16777216) %% 256))
  }
  lt <- as.POSIXlt(Sys.time(), tz = "Europe/Oslo")
  dos_time <- (lt$hour * 2048) + (lt$min * 32) + (as.integer(lt$sec) %/% 2)
  dos_date <- ((lt$year - 80) * 512) + ((lt$mon + 1) * 32) + lt$mday
  con <- file(zipfile, "wb")
  on.exit(close(con), add = TRUE)
  offset <- 0
  central <- list()
  for (i in seq_along(paths)) {
    data <- readBin(paths[i], "raw", file.info(paths[i])$size)
    name <- charToRaw(enc2utf8(names[i]))
    hex <- digest::digest(data, algo = "crc32", serialize = FALSE)
    hex <- formatC(hex, width = 8, flag = "0")
    crc <- strtoi(substr(hex, 1, 4), 16) * 65536 + strtoi(substr(hex, 5, 8), 16)
    common <- c(u16(20), u16(0x0800), u16(0), u16(dos_time), u16(dos_date), u32(crc), u32(length(data)),
                u32(length(data)), u16(length(name)), u16(0))
    local <- c(as.raw(c(0x50, 0x4b, 0x03, 0x04)), common, name)
    writeBin(local, con)
    writeBin(data, con)
    central[[i]] <- c(as.raw(c(0x50, 0x4b, 0x01, 0x02)), u16(20), common, u16(0), u16(0), u16(0), u32(0),
                      u32(offset), name)
    offset <- offset + length(local) + length(data)
  }
  cd <- do.call(c, central)
  writeBin(cd, con)
  writeBin(c(as.raw(c(0x50, 0x4b, 0x05, 0x06)), u16(0), u16(0), u16(length(paths)), u16(length(paths)),
             u32(length(cd)), u32(offset), u16(0)), con)
  invisible(zipfile)
}
