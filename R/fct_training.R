#' Training plans: rules for the season plan, team settings and exercises
#'
#' Validation and cleaning shared by the data layer and the admin panel
#' (phase T1 in claude/plan-treningsopplegg.md). Everything here is pure R,
#' so it can be tested without a database.
#'
#' @name fct_training
#' @noRd
NULL

# NFF's categories (barnefotball), plus warm-up and "other". The keys are
# stored; the labels are shown. New categories can be added here without a
# migration.
exercise_categories <- c(
  oppvarming = "Oppvarming",
  ballmestring = "Ballmestring",
  pasning_mottak = "Pasning og mottak",
  vending = "Vending",
  avslutning = "Avslutning",
  angrep = "Angrep",
  forsvar = "Forsvar",
  smaaspill = "Småspill",
  annet = "Annet"
)

month_names_nb <- c("Januar", "Februar", "Mars", "April", "Mai", "Juni", "Juli",
                    "August", "September", "Oktober", "November", "Desember")

# Text fields of an exercise and their maximum length (same as the CHECKs in
# migration 002).
exercise_text_fields <- c(
  area = 60, organisation = 2000, execution = 2000, learning_points = 2000,
  questions = 2000, easier = 1000, harder = 1000, nff_url = 500
)
exercise_int_fields <- c("min_players", "max_players", "duration_minutes")

team_settings_fields <- c(age_group = 40, pitch = 200, equipment = 1000, principles = 2000)

# One trimmed string; "" for NULL, NA or empty input.
txt1 <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return("")
  trimws(as.character(x[1]))
}

# TRUE if the text contains words for health or other sensitive information
# (same list as for tags and comments).
text_is_sensitive <- function(x) {
  x <- paste(x, collapse = " ")
  nzchar(x) && any(vapply(tag_sensitive_patterns(), grepl, logical(1), x = x, ignore.case = TRUE, perl = TRUE))
}

# An optional whole number within [lo, hi]: "" if empty, otherwise the number
# as a string. Stops with `label` in the message if it is not valid.
int_or_empty <- function(x, label, lo, hi) {
  x <- txt1(x)
  if (!nzchar(x)) return("")
  if (!grepl("^[0-9]+$", x) || as.numeric(x) < lo || as.numeric(x) > hi) {
    stop(label, " må være et helt tall fra ", lo, " til ", hi, ".", call. = FALSE)
  }
  as.character(as.integer(x))
}

#' A stable code for an exercise, made from its name
#'
#' "3 mot 1 i to ruter" -> "3-mot-1-i-to-ruter". Letters æ, ø, å become ae,
#' o, a. Used to collect evaluations per exercise across sessions, so it does
#' not change when the name is edited later.
#' @param taken Codes already in use in the group; a number is added if needed.
#' @noRd
exercise_code <- function(name, taken = character()) {
  x <- tolower(txt1(name))
  x <- chartr("øåéèüöä", "oaeeuoa", gsub("æ", "ae", x))
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("^-+|-+$", "", x)
  x <- substr(x, 1, 50)
  x <- sub("-+$", "", x)
  if (!nzchar(x)) x <- "ovelse"
  code <- x
  i <- 2L
  while (code %in% taken) {
    code <- paste0(x, "-", i)
    i <- i + 1L
  }
  code
}

#' Clean and check an exercise before it is saved
#'
#' @param ex A list with name, category, themes (character vector) and the
#'   fields in `exercise_text_fields` and `exercise_int_fields`. Missing
#'   fields are treated as empty.
#' @return The cleaned list: all text trimmed, "" for empty, numbers as
#'   strings, themes de-duplicated. Stops with a Norwegian message if
#'   something is wrong.
#' @noRd
exercise_validate <- function(ex) {
  out <- list(name = gsub("\\s+", " ", txt1(ex$name)), category = txt1(ex$category))
  if (!nzchar(out$name) || nchar(out$name) > 80) stop("Navnet må ha mellom 1 og 80 tegn.", call. = FALSE)
  if (!out$category %in% names(exercise_categories)) stop("Velg en kategori.", call. = FALSE)

  themes <- gsub("\\s+", " ", trimws(as.character(unlist(ex$themes))))
  themes <- themes[!is.na(themes) & nzchar(themes)]
  themes <- themes[!duplicated(tolower(themes))]
  if (length(themes) > 10) stop("En øvelse kan ha maks 10 temaer.", call. = FALSE)
  if (any(nchar(themes) > 40)) stop("Et tema kan ha maks 40 tegn.", call. = FALSE)
  out$themes <- themes

  labels <- c(min_players = "Minste antall spillere", max_players = "Største antall spillere",
              duration_minutes = "Tid")
  limits <- list(min_players = c(1, 60), max_players = c(1, 60), duration_minutes = c(1, 120))
  for (f in exercise_int_fields) out[[f]] <- int_or_empty(ex[[f]], labels[[f]], limits[[f]][1], limits[[f]][2])
  if (nzchar(out$min_players) && nzchar(out$max_players) &&
      as.integer(out$max_players) < as.integer(out$min_players)) {
    stop("Største antall spillere kan ikke være mindre enn minste antall.", call. = FALSE)
  }

  for (f in names(exercise_text_fields)) {
    out[[f]] <- txt1(ex[[f]])
    if (nchar(out[[f]]) > exercise_text_fields[[f]]) {
      stop("Feltet «", f, "» kan ha maks ", exercise_text_fields[[f]], " tegn.", call. = FALSE)
    }
  }
  if (nzchar(out$nff_url) && !grepl("^https://[^[:space:]]+$", out$nff_url)) {
    stop("Lenken må starte med https://.", call. = FALSE)
  }
  out$drawing <- exercise_drawing_json(ex$drawing)
  out
}

#' A drawing as compact JSON for the database, or "" for none
#'
#' Accepts JSON text or a list; the drawing is checked with
#' `drawing_validate()` and stored compact (`drawing_json()`).
#' @noRd
exercise_drawing_json <- function(x) {
  if (is.null(x) || (is.character(x) && !nzchar(txt1(x)))) return("")
  d <- tryCatch(drawing_validate(x), error = function(e) stop("Tegning: ", conditionMessage(e), call. = FALSE))
  if (is.null(d)) return("")
  drawing_json(d)
}

#' Clean and check one year of the season plan
#'
#' @param themes,descriptions Character vectors of length 12 (January to
#'   December). An empty theme means no theme that month; a description
#'   without a theme is an error.
#' @return data.frame(month, theme, description) for the months with a theme.
#' @noRd
season_validate <- function(year, themes, descriptions = rep("", 12)) {
  year <- suppressWarnings(as.integer(year))
  if (length(year) != 1 || is.na(year) || year < 2020 || year > 2100) stop("Ugyldig år.", call. = FALSE)
  themes <- vapply(seq_len(12), function(i) gsub("\\s+", " ", txt1(themes[i])), character(1))
  descriptions <- vapply(seq_len(12), function(i) txt1(descriptions[i]), character(1))
  if (any(nchar(themes) > 80)) stop("Et tema kan ha maks 80 tegn.", call. = FALSE)
  if (any(nchar(descriptions) > 500)) stop("En beskrivelse kan ha maks 500 tegn.", call. = FALSE)
  orphan <- !nzchar(themes) & nzchar(descriptions)
  if (any(orphan)) {
    stop("Skriv et tema for ", tolower(month_names_nb[which(orphan)[1]]), ", eller fjern beskrivelsen.", call. = FALSE)
  }
  if (text_is_sensitive(c(themes, descriptions))) {
    stop("Årshjulet skal ikke inneholde helseopplysninger eller andre sensitive opplysninger.", call. = FALSE)
  }
  keep <- nzchar(themes)
  data.frame(month = which(keep), theme = themes[keep], description = descriptions[keep])
}

#' Clean and check the team settings
#' @return list with age_group, session_minutes, pitch, equipment, principles
#'   (all strings, "" if empty).
#' @noRd
team_settings_validate <- function(s) {
  out <- list(session_minutes = int_or_empty(s$session_minutes, "Øktlengde", 15, 240))
  for (f in names(team_settings_fields)) {
    out[[f]] <- txt1(s[[f]])
    if (nchar(out[[f]]) > team_settings_fields[[f]]) {
      stop("Feltet «", f, "» kan ha maks ", team_settings_fields[[f]], " tegn.", call. = FALSE)
    }
  }
  if (text_is_sensitive(unlist(out))) {
    stop("Lagets standard skal ikke inneholde helseopplysninger eller andre sensitive opplysninger.", call. = FALSE)
  }
  out[c("age_group", "session_minutes", "pitch", "equipment", "principles")]
}

# Similarity between exercises (kap. 15.4) -----------------------------------------------
# Used when a plan is approved, so the bank does not fill up with nearly
# identical exercises. Pure R; the bank is small.

exercise_similarity_threshold <- 0.6

exercise_stopwords <- c("og", "med", "som", "til", "den", "det", "der", "for", "fra", "har", "ikke", "kan", "men",
                        "når", "seg", "sin", "skal", "slik", "the", "eller", "etter", "før", "hver", "inn", "mot",
                        "opp", "over", "også", "alle", "blir", "dem", "deg", "din", "dere", "han", "hun", "meg",
                        "mer", "noe", "nye", "ved", "vil", "være", "spiller", "spillere", "ballen", "ball")

# The words of a text, for comparing (lower case, at least 3 letters, no
# common words).
exercise_words <- function(...) {
  x <- tolower(paste(unlist(list(...)), collapse = " "))
  w <- unique(strsplit(x, "[^[:alnum:]æøå]+", perl = TRUE)[[1]])
  w[nchar(w) >= 3 & !w %in% exercise_stopwords & !grepl("^[0-9]+$", w)]
}

# Number of players and size of the (first) pitch in a drawing, or NULL.
exercise_shape <- function(d) {
  d <- tryCatch(if (is.null(d) || (is.character(d) && (is.na(d) || !nzchar(d)))) NULL else drawing_validate(d),
                error = function(e) NULL)
  if (is.null(d)) return(NULL)
  s <- d$skisser[[1]]
  list(players = sum(vapply(s$objekter, function(o) o$type == "spiller", logical(1))),
       area = s$bane$bredde * s$bane$lengde)
}

#' How similar a plan exercise is to each bank exercise (0–1)
#'
#' Words in name and description count 40 %, the same category 20 %, a
#' shared theme 20 %, and the number of players and pitch size in the
#' drawing 20 % (half of that when a drawing is missing).
#' @param e Exercise from a plan (plan_validate format).
#' @param bank `ds_list_exercises()`; archived exercises are skipped.
#' @param theme The plan's theme.
#' @return data.frame(code, name, score), most similar first.
#' @noRd
exercise_similarity <- function(e, bank, theme = "") {
  if (!"status" %in% names(bank)) bank$status <- rep("active", nrow(bank))
  bank <- bank[bank$status != "archived", , drop = FALSE]
  if (nrow(bank) == 0) return(data.frame(code = character(), name = character(), score = numeric()))
  key <- names(exercise_categories)[tolower(exercise_categories) == tolower(txt1(e$kategori))]
  key <- if (length(key)) key[1] else txt1(e$kategori)
  w1 <- exercise_words(e$navn, e$organisering, e$gjennomforing)
  sh1 <- exercise_shape(e$tegning)
  ratio <- function(a, b) if (a <= 0 || b <= 0) as.numeric(a == b) else min(a, b) / max(a, b)
  score <- vapply(seq_len(nrow(bank)), function(i) {
    w2 <- exercise_words(bank$name[i], bank$organisation[i], bank$execution[i])
    words <- if (length(union(w1, w2))) length(intersect(w1, w2)) / length(union(w1, w2)) else 0
    cat <- as.numeric(identical(key, bank$category[i]))
    th <- as.numeric(nzchar(theme) && tolower(theme) %in% tolower(bank$themes[[i]]))
    sh2 <- exercise_shape(bank$drawing[i])
    shape <- if (is.null(sh1) || is.null(sh2)) 0.5 else (ratio(sh1$players, sh2$players) + ratio(sh1$area, sh2$area)) / 2
    0.4 * words + 0.2 * cat + 0.2 * th + 0.2 * shape
  }, numeric(1))
  out <- data.frame(code = bank$code, name = bank$name, score = round(score, 2), stringsAsFactors = FALSE)
  out[order(-out$score), , drop = FALSE]
}

#' The exercises in a plan that may go into the bank when it is approved
#'
#' New exercises (kilde ny) and adjusted ones (justert) whose code is not in
#' the bank. Each gets a suggested choice: a new exercise with no similar
#' bank exercise is saved; one that is similar, or an adjusted one, is not
#' saved unless the trainer says so.
#' @return list of list(index, kode, navn, kilde, basert_pa, match (NULL or
#'   list(code, name, score)), default = "save"/"skip").
#' @noRd
bank_candidates <- function(p, bank) {
  out <- list()
  for (i in seq_along(p$ovelser)) {
    e <- p$ovelser[[i]]
    if (!e$kilde %in% c("ny", "justert") || e$kode %in% bank$code) next
    m <- NULL
    if (e$kilde == "ny") {
      sim <- exercise_similarity(e, bank, p$tema)
      if (nrow(sim) && sim$score[1] >= exercise_similarity_threshold) m <- as.list(sim[1, ])
    }
    out[[length(out) + 1]] <- list(index = i, kode = e$kode, navn = e$navn, kilde = e$kilde, basert_pa = e$basert_pa,
                                   match = m, default = if (e$kilde == "ny" && is.null(m)) "save" else "skip")
  }
  out
}
