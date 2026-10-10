#' KI: training plans from Claude (phase T3a)
#'
#' One call to the Claude API (Messages) gives a plan as JSON, following a
#' fixed JSON schema (structured output). The app fills in bank exercises,
#' checks the drawings and makes the PDF; the model never writes drawing code.
#' See claude/plan-treningsopplegg.md, kap. 6, 15 and 16.
#'
#' - The system prompt is `inst/ai/system.md` plus a worked example made from
#'   the reference session. It is the same for every call and cached.
#' - The bank catalogue is a second cached system block.
#' - The order (bestilling) is the user message. It holds no names: only
#'   counts, the theme, the team settings and the trainer's own wishes
#'   (checked for sensitive words).
#' - Tokens and cost of every call are logged in `ai_usage`; the text of the
#'   prompt and the answer is never stored outside the plan itself.
#'
#' The API key is read from the environment variable ANTHROPIC_API_KEY and
#' never logged.
#'
#' The work is split in three so the slow middle part can run without
#' blocking other sessions (T3b): `ai_prepare()` (database, fast),
#' `ai_send()` (the HTTP call, 20–60 s) and `ai_finish()` (database, fast).
#'
#' @name fct_ai
#' @noRd
NULL

ai_api_base <- "https://api.anthropic.com"
ai_api_version <- "2023-06-01"

# Prices in USD per million tokens (checked 10 Oct 2026,
# https://platform.claude.com/docs/en/about-claude/pricing). Cache writes are
# 5-minute writes. Our prompts are far below 100k tokens, where Haiku's
# long-context price would start.
ai_models <- list(
  `claude-haiku-5-5` = list(label = "Haiku 5.5", choice = "Standard", input = 0.10, output = 0.50,
                            cache_write = 0.125, cache_read = 0.01),
  `claude-sonnet-5-5` = list(label = "Sonnet 5.5", choice = "Bedre kvalitet", input = 2, output = 10,
                             cache_write = 2.50, cache_read = 0.10)
)

# Long enough for six exercises with drawings; a normal plan is about 6 000.
ai_max_tokens <- 16000L

# The longest wish from the trainer, in characters.
ai_wish_max <- 1000L

# Settings ---------------------------------------------------------------------------

#' Default KI settings (used until the superadmin saves others, T3d)
#'
#' USD/NOK was 9.56 on 9 Oct 2026; 10 leaves room for card fees.
#' @noRd
ai_settings_default <- function() {
  list(model = "claude-haiku-5-5", group_limit_nok = 30, total_limit_usd = 8, max_new_exercises = 2L,
       usd_nok = 10, updated_by = "", updated_at = as.POSIXct(NA))
}

ai_settings_validate <- function(s) {
  num <- function(x, what, lo, hi) {
    v <- suppressWarnings(as.numeric(txt1(x)))
    if (is.na(v) || v < lo || v > hi) stop(what, " må være et tall fra ", lo, " til ", hi, ".", call. = FALSE)
    v
  }
  model <- txt1(s$model)
  if (!model %in% names(ai_models)) stop("Ukjent modell.", call. = FALSE)
  n <- num(s$max_new_exercises, "Maks nye øvelser", 0, 6)
  if (n != round(n)) stop("Maks nye øvelser må være et helt tall.", call. = FALSE)
  list(model = model,
       group_limit_nok = num(s$group_limit_nok, "Grensen per lag", 0, 10000),
       total_limit_usd = num(s$total_limit_usd, "Grensen totalt", 0, 1000),
       max_new_exercises = as.integer(n),
       usd_nok = num(s$usd_nok, "Dollarkursen", 1, 100))
}

# Money ------------------------------------------------------------------------------

#' Cost in USD of one call from its usage
#' @param usage list(input_tokens, output_tokens, cache_read_tokens, cache_write_tokens)
#' @noRd
ai_cost <- function(usage, model) {
  p <- ai_models[[model]]
  if (is.null(p)) stop("Ukjent modell.", call. = FALSE)
  n <- function(x) as.numeric(x %||% 0)
  (n(usage$input_tokens) * p$input + n(usage$output_tokens) * p$output +
     n(usage$cache_read_tokens) * p$cache_read + n(usage$cache_write_tokens) * p$cache_write) / 1e6
}

#' "ca. 5 øre" or "ca. 1,20 kr"
#' @noRd
ai_format_nok <- function(usd, rate) {
  nok <- usd * rate
  if (nok < 0.995) {
    paste0("ca. ", max(1, round(nok * 100)), " øre")
  } else {
    paste0("ca. ", formatC(nok, format = "f", digits = 2, decimal.mark = ","), " kr")
  }
}

#' The first moment of the month in Oslo, for the monthly limits
#' @noRd
ai_month_start <- function(now = Sys.time()) {
  as.POSIXct(format(now, "%Y-%m-01 00:00:00", tz = "Europe/Oslo"), tz = "Europe/Oslo")
}

#' Is there room for one more call this month?
#' @param spend list(group_usd, total_usd) from `ds_ai_spend()`.
#' @return list(ok, group_nok, group_limit_nok, total_usd, total_limit_usd, message)
#' @noRd
ai_budget <- function(spend, s) {
  group_nok <- spend$group_usd * s$usd_nok
  out <- list(ok = TRUE, group_nok = group_nok, group_limit_nok = s$group_limit_nok,
              total_usd = spend$total_usd, total_limit_usd = s$total_limit_usd, message = "")
  if (spend$total_usd >= s$total_limit_usd) {
    out$ok <- FALSE
    out$message <- "KI-budsjettet for hele appen er brukt opp denne måneden. Lag opplegget manuelt, eller be superadmin øke grensen."
  } else if (group_nok >= s$group_limit_nok) {
    out$ok <- FALSE
    out$message <- paste0("Laget har brukt ", formatC(group_nok, format = "f", digits = 2, decimal.mark = ","),
                          " av ", s$group_limit_nok, " kr til KI denne måneden. Lag opplegget manuelt, eller be superadmin øke grensen.")
  }
  out
}

# Prompt -----------------------------------------------------------------------------

#' The JSON schema of the answer
#'
#' Every field is required (the API allows few optional ones); empty strings
#' and lists stand for "not used". Numbers and lengths are checked in R,
#' since the schema cannot limit them.
#' @noRd
ai_plan_schema <- function() {
  str <- list(type = "string")
  strs <- list(type = "array", items = str)
  int <- list(type = "integer")
  obj <- function(props) list(type = "object", properties = props, required = as.list(names(props)),
                              additionalProperties = FALSE)
  exercise <- obj(list(
    kilde = list(type = "string", enum = as.list(plan_exercise_sources)),
    kode = str, basert_pa = str, naermeste_kode = str, hvorfor_ny = str,
    navn = str, kategori = list(type = "string", enum = as.list(names(exercise_categories))),
    fokus = str, organisering = strs, gjennomforing = strs, tilpasning = strs, laeringsmomenter = strs,
    sporsmal = strs, enklere = str, vanskeligere = str, tegning_json = str
  ))
  obj(list(
    tittel = str, undertittel = str, fokus_forsvar = str, fokus_angrep = str,
    stikkord = list(type = "array", items = obj(list(tittel = str, tekst = str))),
    stikkord_merknad = str,
    oppvarming_minutter = int, oppvarming_tekst = str, stasjon_minutter = int, bytte_minutter = int,
    rotasjon = list(type = "boolean"), avslutning_minutter = int, avslutning_tekst = str, merknad = str,
    avslutning_sporsmal = strs, kilde = str,
    ovelser = list(type = "array", items = exercise)
  ))
}

#' The reference session (inst/extdata) written as an answer in the schema,
#' used as the worked example in the system prompt
#' @noRd
ai_example_answer <- function() {
  p <- plan_validate(paste(readLines(app_sys("extdata", "referanse-okt.json"), encoding = "UTF-8"), collapse = "\n"))
  a <- ai_answer_from_plan(p)
  cats <- c("pasning_mottak", "forsvar", "smaaspill")
  for (i in seq_along(a$ovelser)) {
    a$ovelser[[i]]$kilde <- "ny"
    a$ovelser[[i]]$kategori <- cats[i]
    a$ovelser[[i]]$hvorfor_ny <- "Banken hadde ingen øvelse for dette."
  }
  a
}

#' A saved plan written in the answer schema, so the model can adjust it
#' («Juster med KI»). Bank exercises are sent in full, so the model can see
#' what it would change.
#' @noRd
ai_answer_from_plan <- function(p) {
  p <- plan_validate(p)
  tp <- p$tidsplan
  cat_key <- function(label) {
    k <- names(exercise_categories)[tolower(exercise_categories) == tolower(txt1(label))]
    if (length(k)) k[1] else "annet"
  }
  list(
    tittel = p$tittel, undertittel = p$undertittel, fokus_forsvar = p$fokus$forsvar, fokus_angrep = p$fokus$angrep,
    stikkord = p$stikkord, stikkord_merknad = p$stikkord_merknad,
    oppvarming_minutter = tp$oppvarming$minutter, oppvarming_tekst = tp$oppvarming$tekst,
    stasjon_minutter = tp$stasjoner$minutter, bytte_minutter = tp$stasjoner$bytte,
    rotasjon = tp$stasjoner$grupper > 1,
    avslutning_minutter = tp$avslutning$minutter, avslutning_tekst = tp$avslutning$tekst, merknad = tp$merknad,
    avslutning_sporsmal = p$avslutning_sporsmal, kilde = p$kilde,
    ovelser = lapply(p$ovelser, function(e) {
      list(kilde = e$kilde, kode = e$kode, basert_pa = e$basert_pa, naermeste_kode = e$naermeste_kode,
           hvorfor_ny = e$hvorfor_ny, navn = e$navn, kategori = cat_key(e$kategori), fokus = e$fokus,
           organisering = e$organisering, gjennomforing = e$gjennomforing, tilpasning = e$tilpasning,
           laeringsmomenter = e$laeringsmomenter, sporsmal = e$sporsmal, enklere = e$enklere,
           vanskeligere = e$vanskeligere, tegning_json = if (is.null(e$tegning)) "" else drawing_json(e$tegning))
    })
  )
}

#' The order for «Juster med KI»: the current plan, what to change, and the
#' counts as they are now. No names.
#' @param ctx As for `ai_order_text()`; `wish` is what to change (required).
#' @noRd
ai_revision_text <- function(base, ctx) {
  wish <- txt1(ctx$wish)
  if (!nzchar(wish)) stop("Skriv hva som skal endres.", call. = FALSE)
  facts <- ai_order_text(modifyList(ctx, list(wish = "", theme = "", theme_description = "")))
  facts <- sub("^# Bestilling", "# Fakta nå", sub("\n\nLag treningsøkta.", "", facts, fixed = TRUE))
  if (nchar(wish) > ai_wish_max) stop("Ønskene kan ha maks ", ai_wish_max, " tegn.", call. = FALSE)
  if (text_is_sensitive(wish)) {
    stop("Ønskene skal ikke inneholde helseopplysninger eller andre sensitive opplysninger.", call. = FALSE)
  }
  paste0("# Gjeldende opplegg\n\n", jsonlite::toJSON(ai_answer_from_plan(base), auto_unbox = TRUE, null = "null"),
         "\n\n", facts, "\n\n# Endringsønske\n\n", wish, "\n\nJuster opplegget.")
}

#' The fixed system prompt: instructions plus the worked example
#' @noRd
ai_system_text <- function() {
  instr <- paste(readLines(app_sys("ai", "system.md"), encoding = "UTF-8"), collapse = "\n")
  example <- jsonlite::toJSON(ai_example_answer(), auto_unbox = TRUE, pretty = FALSE)
  paste0(instr, "\n\n# Eksempel på et godt svar\n\n",
         "Bestilling: G10, ca. 24 spillere, 65 min, tema «Samhandling – spille på lag». ",
         "Utfordringer: laget kommer ikke hjem bak ballen, og ballfører har få pasningsalternativer. ",
         "Banken var tom.\n\n", example, "\n")
}

#' The bank as a short catalogue for the prompt (kap. 15.1)
#'
#' Archived exercises are left out. Exercises with the month's theme come
#' first; then active before candidates, then by category and name. At most
#' `max` lines.
#' @noRd
ai_catalogue <- function(bank, theme = "", max = 30) {
  if (is.null(bank) || nrow(bank) == 0) return("# Øvelsesbanken\n\nBanken er tom.")
  if (!"status" %in% names(bank)) bank$status <- "active"
  if (!"based_on" %in% names(bank)) bank$based_on <- ""
  bank <- bank[bank$status != "archived", , drop = FALSE]
  if (nrow(bank) == 0) return("# Øvelsesbanken\n\nBanken er tom.")
  hit <- vapply(bank$themes, function(t) nzchar(theme) && tolower(theme) %in% tolower(t), logical(1))
  ord <- order(!hit, bank$status != "active", match(bank$category, names(exercise_categories)), tolower(bank$name))
  bank <- bank[utils::head(ord, max), , drop = FALSE]
  one_line <- function(x, n = 140) {
    x <- gsub("\\s+", " ", txt1(x))
    if (nchar(x) > n) paste0(substr(x, 1, n - 1), "…") else x
  }
  players <- function(lo, hi) {
    if (is.na(lo) && is.na(hi)) return("")
    paste0(if (is.na(lo)) "?" else lo, "–", if (is.na(hi)) "?" else hi, " spillere")
  }
  lines <- vapply(seq_len(nrow(bank)), function(i) {
    r <- bank[i, , drop = FALSE]
    desc <- one_line(if (nzchar(txt1(r$execution))) r$execution else r$organisation)
    parts <- c(r$code, r$name, r$category,
               if (length(r$themes[[1]])) paste0("tema: ", paste(r$themes[[1]], collapse = ", ")),
               players(r$min_players, r$max_players),
               if (!is.na(r$duration_minutes)) paste(r$duration_minutes, "min"),
               if (nzchar(txt1(r$based_on))) paste("variant av", r$based_on),
               if (r$status == "candidate") "kandidat",
               if (nzchar(desc)) paste("kort:", desc))
    paste("-", paste(parts[nzchar(parts)], collapse = " | "))
  }, character(1))
  paste0("# Øvelsesbanken\n\nkode | navn | kategori | tema | spillere | minutter | merknader | kort beskrivelse\n\n",
         paste(lines, collapse = "\n"))
}

#' The order (bestilling) for one event, without names
#'
#' @param ctx list(start (POSIXct), minutes, theme, theme_description, team
#'   (from `ds_get_team_settings()`), n_players, group_sizes (integers),
#'   wish (free text from the trainer), max_new).
#' @noRd
ai_order_text <- function(ctx) {
  wish <- txt1(ctx$wish)
  if (nchar(wish) > ai_wish_max) stop("Ønskene kan ha maks ", ai_wish_max, " tegn.", call. = FALSE)
  if (text_is_sensitive(wish)) {
    stop("Ønskene skal ikke inneholde helseopplysninger eller andre sensitive opplysninger.", call. = FALSE)
  }
  team <- ctx$team %||% list()
  line <- function(label, x) if (nzchar(txt1(x))) paste0("- ", label, ": ", txt1(x))
  days <- c("søndag", "mandag", "tirsdag", "onsdag", "torsdag", "fredag", "lørdag")
  when <- if (!is.null(ctx$start) && !is.na(ctx$start)) {
    lt <- as.POSIXlt(ctx$start, tz = "Europe/Oslo")
    paste0(days[lt$wday + 1], " ", lt$mday, ". ", tolower(month_names_nb[lt$mon + 1]), " ", lt$year + 1900,
           " kl. ", format(ctx$start, "%H:%M", tz = "Europe/Oslo"))
  }
  sizes <- as.integer(ctx$group_sizes %||% integer())
  groups <- if (length(sizes)) paste0(length(sizes), " (", paste(sizes, collapse = ", "), " spillere)")
  out <- c("# Bestilling", "",
           line("Tidspunkt", when),
           line("Øktlengde", if (!is.null(ctx$minutes) && !is.na(ctx$minutes)) paste(ctx$minutes, "min")),
           line("Månedens tema", ctx$theme),
           line("Om temaet", ctx$theme_description),
           line("Alder", team$age_group),
           line("Bane", team$pitch),
           line("Utstyr", team$equipment),
           line("Lagets prinsipper", team$principles),
           line("Påmeldte spillere", if (!is.null(ctx$n_players) && !is.na(ctx$n_players)) ctx$n_players),
           line("Grupper i godkjent gruppeforslag", groups),
           line("Maks nye øvelser", ctx$max_new %||% ai_settings_default()$max_new_exercises),
           line("Ønsker fra treneren", wish))
  paste(c(out, "", "Lag treningsøkta."), collapse = "\n")
}

#' The request body for the Messages API
#' @noRd
ai_request_body <- function(model, catalogue, order, system = ai_system_text(), max_tokens = ai_max_tokens) {
  cache <- list(type = "ephemeral")
  list(
    model = model,
    max_tokens = max_tokens,
    system = list(list(type = "text", text = system, cache_control = cache),
                  list(type = "text", text = catalogue, cache_control = cache)),
    messages = list(list(role = "user", content = order)),
    output_config = list(format = list(type = "json_schema", schema = ai_plan_schema()))
  )
}

# HTTP -------------------------------------------------------------------------------

#' POST a JSON body to the Claude API
#'
#' Retries once on overload and rate limits. Never stops: network problems
#' come back as status NA.
#' @return list(status, body) where body is the parsed JSON (or NULL).
#' @noRd
ai_http_post <- function(path, body, key = Sys.getenv("ANTHROPIC_API_KEY"), timeout = 180) {
  req <- ai_http_request(path, body, key, timeout) |>
    httr2::req_retry(max_tries = 2, is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 529))
  ai_http_result(tryCatch(httr2::req_perform(req), error = function(e) NULL))
}

#' The same as `ai_http_post()`, but as a promise, so the call does not block
#' other sessions while the model writes (20–60 s). No retry.
#' @noRd
ai_http_post_async <- function(path, body, key = Sys.getenv("ANTHROPIC_API_KEY"), timeout = 180) {
  promises::then(httr2::req_perform_promise(ai_http_request(path, body, key, timeout)),
                 onFulfilled = ai_http_result,
                 onRejected = function(e) ai_http_result(NULL))
}

ai_http_request <- function(path, body, key, timeout) {
  json <- jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
  httr2::request(paste0(ai_api_base, path)) |>
    httr2::req_headers(`x-api-key` = key, `anthropic-version` = ai_api_version, .redact = "x-api-key") |>
    httr2::req_user_agent("SpondNymark") |>
    httr2::req_body_raw(json, type = "application/json") |>
    httr2::req_timeout(timeout) |>
    httr2::req_error(is_error = function(resp) FALSE)
}

ai_http_result <- function(resp) {
  if (is.null(resp)) return(list(status = NA_integer_, body = NULL))
  list(status = httr2::resp_status(resp),
       body = tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE), error = function(e) NULL))
}

# Errors meant for the user (Norwegian, no technical details). Other errors
# are logged and replaced by a general message.
ai_stop <- function(...) {
  stop(structure(class = c("ai_error", "error", "condition"), list(message = paste0(...), call = NULL)))
}

# Answer -----------------------------------------------------------------------------

#' Read an API response: the answer text, usage and a short error code
#' @return list(ok, text, usage, error_code, message).
#' @noRd
ai_read_response <- function(res) {
  b <- res$body %||% list()
  u <- b$usage %||% list()
  usage <- list(input_tokens = u$input_tokens %||% 0, output_tokens = u$output_tokens %||% 0,
                cache_read_tokens = u$cache_read_input_tokens %||% 0,
                cache_write_tokens = u$cache_creation_input_tokens %||% 0)
  fail <- function(code, msg) list(ok = FALSE, text = "", usage = usage, error_code = code, message = msg)
  if (is.na(res$status)) return(fail("nettverk", "Fikk ikke kontakt med KI-tjenesten. Prøv igjen om litt."))
  if (res$status != 200) {
    type <- txt1(b$error$type)
    msg <- txt1(b$error$message)
    if (grepl("credit|usage limit|spend limit|billing", msg, ignore.case = TRUE)) {
      return(fail("grense", "Grensen hos Anthropic er nådd, eller kreditten er brukt opp. Lag opplegget manuelt."))
    }
    code <- if (nzchar(type)) type else paste0("http_", res$status)
    text <- switch(code,
      authentication_error = ,
      permission_error = "API-nøkkelen ble avvist. Sjekk ANTHROPIC_API_KEY.",
      rate_limit_error = "For mange forespørsler akkurat nå. Prøv igjen om litt.",
      overloaded_error = "KI-tjenesten er overbelastet. Prøv igjen om litt.",
      paste0("KI-tjenesten svarte med en feil (", code, "). Prøv igjen om litt."))
    return(fail(code, text))
  }
  stop_reason <- txt1(b$stop_reason)
  if (stop_reason == "refusal") return(fail("refusal", "KI-en avslo forespørselen. Prøv å skrive ønskene annerledes."))
  if (stop_reason == "max_tokens") return(fail("max_tokens", "Svaret ble for langt og ble kuttet. Prøv med færre øvelser."))
  texts <- vapply(Filter(function(x) identical(x$type, "text"), b$content %||% list()), function(x) txt1(x$text), "")
  if (!length(texts) || !nzchar(texts[1])) return(fail("tomt_svar", "KI-en svarte ikke med et opplegg. Prøv igjen."))
  list(ok = TRUE, text = paste(texts, collapse = ""), usage = usage, error_code = "", message = "")
}

ai_clip <- function(x, n) {
  x <- txt1(x)
  if (nchar(x) > n) paste0(substr(x, 1, n - 1), "…") else x
}

ai_clips <- function(x, items = 8, n = 400) {
  x <- vapply(x %||% list(), function(v) ai_clip(v, n), character(1))
  as.list(utils::head(x[nzchar(x)], items))
}

#' Turn the model's answer into a plan
#'
#' Bank exercises are filled in from the bank. Drawings are checked and
#' fixed; a drawing that cannot be used is left out with a warning, so the
#' rest of the paid answer is kept. Codes that clash with the bank are
#' renamed.
#' @param answer The JSON text from the model.
#' @param bank `ds_list_exercises()` for the group.
#' @param ctx The order context (theme).
#' @return list(plan, warnings).
#' @noRd
ai_plan_from_answer <- function(answer, bank, ctx = list(), max_new = 2L) {
  a <- tryCatch(jsonlite::fromJSON(answer, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(a)) stop("Svaret er ikke gyldig JSON.", call. = FALSE)
  if (is.null(bank)) bank <- data.frame(code = character())
  warn <- character()
  codes <- bank$code %||% character()
  used <- character()
  ex <- list()
  for (i in seq_along(a$ovelser %||% list())) {
    e <- a$ovelser[[i]]
    w <- paste0("Øvelse ", i)
    kilde <- txt1(e$kilde)
    kode <- txt1(e$kode)
    if (kilde == "bank") {
      row <- bank[bank$code == kode, , drop = FALSE]
      if (nrow(row) == 1) {
        r <- plan_exercise_from_bank(row)
        if (nzchar(txt1(e$fokus))) r$fokus <- ai_clip(e$fokus, 300)
        tp <- ai_clips(e$tilpasning, 4)
        if (length(tp)) r$tilpasning <- tp
        ex[[length(ex) + 1]] <- r
        used <- c(used, kode)
        next
      }
      if (!nzchar(txt1(e$navn))) {
        warn <- c(warn, paste0(w, ": koden «", kode, "» finnes ikke i banken, så øvelsen er utelatt."))
        next
      }
      warn <- c(warn, paste0(w, ": koden «", kode, "» finnes ikke i banken, så øvelsen er lagt inn som ny."))
      kilde <- "ny"
    }
    if (!kilde %in% c("justert", "ny")) kilde <- "ny"
    base <- txt1(e$basert_pa)
    if (kilde == "justert" && !base %in% codes) {
      kilde <- "ny"
      base <- ""
    }
    if (kilde == "ny") base <- ""
    navn <- ai_clip(e$navn, 80)
    if (!nzchar(navn)) {
      warn <- c(warn, paste0(w, " manglet navn og er utelatt."))
      next
    }
    if (!grepl("^[a-z0-9]+(-[a-z0-9]+)*$", kode) || nchar(kode) > 60 || kode %in% c(codes, used)) {
      kode <- exercise_code(navn, c(codes, used))
    }
    used <- c(used, kode)
    cat_key <- txt1(e$kategori)
    tegning <- NULL
    if (nzchar(txt1(e$tegning_json))) {
      tegning <- tryCatch(drawing_fix(drawing_validate(e$tegning_json)), error = function(err) {
        warn <<- c(warn, paste0(w, ": tegningen kunne ikke brukes (", conditionMessage(err), ") og er utelatt."))
        NULL
      })
    }
    nearest <- txt1(e$naermeste_kode)
    ex[[length(ex) + 1]] <- list(
      kode = kode, navn = navn,
      kategori = if (cat_key %in% names(exercise_categories)) exercise_categories[[cat_key]] else "Annet",
      fokus = ai_clip(e$fokus, 300),
      organisering = ai_clips(e$organisering), gjennomforing = ai_clips(e$gjennomforing),
      tilpasning = ai_clips(e$tilpasning, 4), laeringsmomenter = ai_clips(e$laeringsmomenter),
      sporsmal = ai_clips(e$sporsmal, 6),
      enklere = ai_clip(e$enklere, 400), vanskeligere = ai_clip(e$vanskeligere, 400), nff_url = "",
      kilde = kilde, basert_pa = base,
      naermeste_kode = if (kilde == "ny" && nearest %in% codes) nearest else "",
      hvorfor_ny = if (kilde == "ny") ai_clip(e$hvorfor_ny, 300) else "",
      tegning = tegning)
  }
  ex <- utils::head(ex, 6)
  if (!length(ex)) stop("Svaret hadde ingen øvelser som kunne brukes.", call. = FALSE)
  # New exercises; when adjusting, the ones already in the plan do not count.
  n_new <- sum(vapply(ex, function(x) x$kilde == "ny" && !x$kode %in% (ctx$base_codes %||% character()), logical(1)))
  if (n_new > max_new) warn <- c(warn, paste0("KI-en laget ", n_new, " nye øvelser (grensen er ", max_new, ")."))
  int <- function(x, default) {
    v <- suppressWarnings(as.integer(x %||% default))
    if (length(v) != 1 || is.na(v)) default else v
  }
  rot <- isTRUE(a$rotasjon) && length(ex) > 1
  stikkord <- utils::head(Filter(function(k) nzchar(txt1(k$tittel)), a$stikkord %||% list()), 4)
  p <- list(
    tittel = ai_clip(if (nzchar(txt1(a$tittel))) a$tittel else ctx$theme %||% "Treningsøkt", 80),
    undertittel = ai_clip(a$undertittel, 160), tema = ai_clip(ctx$theme %||% "", 80),
    fokus = list(forsvar = ai_clip(a$fokus_forsvar, 300), angrep = ai_clip(a$fokus_angrep, 300)),
    stikkord = lapply(stikkord, function(k) list(tittel = ai_clip(k$tittel, 60), tekst = ai_clip(k$tekst, 160))),
    stikkord_merknad = ai_clip(a$stikkord_merknad, 300),
    tidsplan = list(
      oppvarming = list(minutter = min(max(int(a$oppvarming_minutter, 0L), 0L), 60L), tekst = ai_clip(a$oppvarming_tekst, 200)),
      stasjoner = list(minutter = min(max(int(a$stasjon_minutter, 15L), 1L), 90L),
                       bytte = if (length(ex) > 1) min(max(int(a$bytte_minutter, 2L), 0L), 10L) else 0L,
                       grupper = if (rot) length(ex) else 1L),
      avslutning = list(minutter = min(max(int(a$avslutning_minutter, 0L), 0L), 60L), tekst = ai_clip(a$avslutning_tekst, 200)),
      merknad = ai_clip(a$merknad, 500)),
    avslutning_sporsmal = ai_clips(a$avslutning_sporsmal, 6, 200),
    kilde = ai_clip(a$kilde, 500),
    ovelser = ex
  )
  plan <- plan_validate(p)
  if (!is.null(ctx$minutes) && !is.na(ctx$minutes)) {
    total <- plan_schedule(plan)$total
    if (abs(total - ctx$minutes) > 5) warn <- c(warn, paste0("Tidsplanen er ", total, " min, men økta er ", ctx$minutes, " min."))
  }
  list(plan = plan, warnings = warn)
}

# The three steps --------------------------------------------------------------------

#' Step 1: check rights and budget, and build the request
#'
#' Stops with a Norwegian message if the user may not use KI, the key is
#' missing or the budget is used up.
#' @param ctx The order, see `ai_order_text()`.
#' @param base_plan NULL for a new plan, or the plan to adjust («Juster med KI»).
#' @return A list used by `ai_estimate()`, `ai_send()` and `ai_finish()`.
#' @noRd
ai_prepare <- function(con, access, rights, group_id, ctx, kind = NULL, now = Sys.time(),
                       key = Sys.getenv("ANTHROPIC_API_KEY"), model = NULL, base_plan = NULL) {
  kind <- kind %||% if (is.null(base_plan)) "draft" else "revision"
  if (!isTRUE(rights$ai)) ai_stop("Du har ikke tilgang til KI. Spør en administrator.")
  if (!nzchar(key)) ai_stop("KI er ikke satt opp (ANTHROPIC_API_KEY mangler).")
  s <- ds_get_ai_settings(con)
  model <- model %||% s$model
  if (!model %in% names(ai_models)) ai_stop("Ukjent modell.")
  budget <- ai_budget(ds_ai_spend(con, access, group_id, ai_month_start(now)), s)
  if (!budget$ok) ai_stop(budget$message)
  bank <- ds_list_exercises(con, access, group_id)
  ctx$max_new <- s$max_new_exercises
  if (!is.null(base_plan)) ctx$base_codes <- vapply(base_plan$ovelser, function(e) txt1(e$kode), "")
  order <- tryCatch(if (is.null(base_plan)) ai_order_text(ctx) else ai_revision_text(base_plan, ctx),
                    error = function(e) ai_stop(conditionMessage(e)))
  body <- ai_request_body(model, ai_catalogue(bank, ctx$theme %||% ""), order)
  list(group_id = group_id, kind = kind, model = model, settings = s, budget = budget, bank = bank,
       ctx = ctx, body = body)
}

#' Price estimate before the call: input tokens counted by the API (free),
#' answer length from earlier calls (or 6 000 tokens). Assumes nothing is
#' cached, so it errs on the high side.
#' The count is made once, for the chosen model; `per_model` uses the same
#' token numbers for every model (close enough for a price hint).
#' @return list(input_tokens, output_tokens, usd, text, per_model = named
#'   list of list(usd, text)) or NULL if the count failed.
#' @noRd
ai_estimate <- function(prep, typical_output = NA, http = ai_http_post) {
  res <- http("/v1/messages/count_tokens", prep$body[c("model", "system", "messages", "output_config")])
  n_in <- res$body$input_tokens
  if (!isTRUE(res$status == 200) || !is.numeric(n_in)) return(NULL)
  n_out <- if (is.null(typical_output) || is.na(typical_output)) 6000 else typical_output
  per_model <- lapply(names(ai_models), function(m) {
    usd <- ai_cost(list(input_tokens = n_in, output_tokens = n_out), m)
    list(usd = usd, text = ai_format_nok(usd, prep$settings$usd_nok))
  })
  names(per_model) <- names(ai_models)
  c(list(input_tokens = n_in, output_tokens = n_out), per_model[[prep$model]], list(per_model = per_model))
}

#' Step 2: the call itself (slow)
#' @return list(res, duration_ms)
#' @noRd
ai_send <- function(prep, http = ai_http_post) {
  t0 <- Sys.time()
  res <- http("/v1/messages", prep$body)
  list(res = res, duration_ms = round(as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000))
}

#' Step 2 as a promise (used by the app, see `ai_send()`)
#' @noRd
ai_send_async <- function(prep, http_async = ai_http_post_async) {
  t0 <- Sys.time()
  promises::then(promises::as.promise(http_async("/v1/messages", prep$body)), function(res) {
    list(res = res, duration_ms = round(as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000))
  })
}

#' Step 3: log the call, turn the answer into a plan and save it as a new
#' version
#'
#' The call is logged also when it failed or the answer could not be used,
#' since it may still have cost money. Stops with a Norwegian message after
#' logging if there is no plan.
#' @return list(id, version, warnings, cost_usd, usage_id).
#' @noRd
ai_finish <- function(con, access, actor, prep, sent, event_id) {
  r <- ai_read_response(sent$res)
  plan <- NULL
  err <- r$error_code
  msg <- r$message
  if (r$ok) {
    plan <- tryCatch(ai_plan_from_answer(r$text, prep$bank, prep$ctx, prep$settings$max_new_exercises),
                     error = function(e) {
                       err <<- "ugyldig_svar"
                       msg <<- paste("KI-svaret kunne ikke brukes:", conditionMessage(e))
                       NULL
                     })
  }
  cost <- ai_cost(r$usage, prep$model)
  usage_id <- ds_log_ai_usage(con, access, prep$group_id, actor, event_id = event_id, u = c(r$usage, list(
    kind = prep$kind, model = prep$model, cost_usd = cost, duration_ms = sent$duration_ms,
    status = if (is.null(plan)) "error" else "ok", error_code = err)))
  if (is.null(plan)) ai_stop(msg)
  saved <- ds_save_plan(con, access, prep$group_id, event_id, plan$plan, actor, source = "ai",
                        model = prep$model, cost_usd = cost)
  ds_ai_usage_set_plan(con, access, prep$group_id, usage_id, saved$id)
  list(id = saved$id, version = saved$version, warnings = plan$warnings, cost_usd = cost, usage_id = usage_id)
}
