#' Evaluation of sessions and exercises (T3e, plan kap. 14)
#'
#' After a training, every trainer in the team can evaluate the plan that was
#' used (the approved version, otherwise the newest): each exercise with a
#' rating, fixed keywords, actual time, free text and «Bruke igjen?», and the
#' session as a whole with a rating, the number of players who came, and
#' free text. It takes less than a minute; everything but the rating is
#' optional. Several trainers' evaluations are summed up per exercise.
#'
#' Free text is checked for sensitive words and for names of members (it may
#' later be summed up by KI, T3e-4). Only profile ids are stored.
#' @name fct_feedback
#' @noRd
NULL

# Ratings: stored as 3/2/1.
feedback_ratings <- c(`3` = "\U0001F44D Bra", `2` = "\U0001F44C OK", `1` = "\U0001F44E Dårlig")

# Keywords (stored as keys, shown as labels).
feedback_tags <- c(
  for_lett = "for lett", for_vanskelig = "for vanskelig", goy = "gøy", kjedelig = "kjedelig",
  ko = "mye venting/kø", hoy_intensitet = "høy intensitet", lav_intensitet = "lav intensitet",
  uklar = "uklar forklaring", bane_liten = "banen for liten", bane_stor = "banen for stor",
  for_lang = "for lang", for_kort = "for kort", tema_ja = "fikk fram temaet", tema_nei = "fikk ikke fram temaet"
)

feedback_use_again <- c(yes = "Ja", changes = "Med endringer", no = "Nei")

feedback_body_max <- 300

#' The version of an event's plan that is evaluated: the approved one,
#' otherwise the newest
#' @param v data.frame from `ds_plan_versions()` (newest first).
#' @return The plan id, or NULL if there is none.
#' @noRd
feedback_plan_id <- function(v) {
  if (is.null(v) || nrow(v) == 0) return(NULL)
  if (any(v$status == "approved")) v$id[v$status == "approved"][1] else v$id[1]
}

#' Check one trainer's evaluation of a plan and return it in a clean form
#'
#' @param rows List with one element per exercise (`exercise_no` 1..n) and
#'   one for the session (`exercise_no` NA). Fields: rating (1–3 or NA),
#'   tags (keys), actual_minutes, actual_players (session only), body,
#'   use_again (exercises only). The session's rating may be left out.
#' @param plan The plan (from `ds_get_plan()`), for the exercise codes.
#' @param names Member names that the text must not contain
#'   (`member_names()`).
#' @return data.frame(exercise_no, exercise_code, rating, tags (list),
#'   actual_minutes, actual_players, body, use_again); rows with nothing
#'   filled in are left out. Stops with a Norwegian message.
#' @noRd
feedback_validate <- function(rows, plan, names = character()) {
  n <- length(plan$ovelser)
  num <- function(x, what, lo, hi) {
    x <- txt1(x)
    if (!nzchar(x)) return(NA_integer_)
    v <- suppressWarnings(as.numeric(x))
    if (is.na(v) || v != round(v) || v < lo || v > hi) stop(what, " må være et helt tall fra ", lo, " til ", hi, ".", call. = FALSE)
    as.integer(v)
  }
  out <- lapply(rows, function(r) {
    no <- suppressWarnings(as.integer(r$exercise_no %||% NA))
    session <- length(no) != 1 || is.na(no)
    if (!session && (no < 1 || no > n)) stop("Ukjent øvelse.", call. = FALSE)
    what <- if (session) "Hele økta" else paste("Øvelse", no)
    rating <- num(r$rating, paste0(what, ": vurderingen"), 1, 3)
    tags <- unique(as.character(unlist(r$tags %||% character())))
    tags <- tags[nzchar(tags)]
    if (session) tags <- character()
    if (any(!tags %in% names(feedback_tags))) stop(what, ": ukjent stikkord.", call. = FALSE)
    body <- txt1(r$body)
    if (nchar(body) > feedback_body_max) stop(what, ": teksten kan ha maks ", feedback_body_max, " tegn.", call. = FALSE)
    if (text_is_sensitive(body)) {
      stop(what, ": teksten skal ikke inneholde helseopplysninger eller andre sensitive opplysninger.", call. = FALSE)
    }
    problem <- names_problem(body, names)
    if (!is.null(problem)) {
      stop(what, ": ", sub("Navn sendes ikke til KI", "Skriv uten navn, evalueringene kan oppsummeres av KI", problem,
                          fixed = TRUE), call. = FALSE)
    }
    use_again <- if (session) "" else txt1(r$use_again)
    if (nzchar(use_again) && !use_again %in% names(feedback_use_again)) stop(what, ": ugyldig svar på «Bruke igjen?».", call. = FALSE)
    minutes <- if (session) NA_integer_ else num(r$actual_minutes, paste0(what, ": faktisk tid"), 0, 240)
    players <- if (session) num(r$actual_players, "Antall spillere", 0, 200) else NA_integer_
    filled <- !is.na(rating) || length(tags) || nzchar(body) || nzchar(use_again) || (session && !is.na(players))
    if (!filled) return(NULL)
    # The session's rating is optional (the number of players is filled in
    # beforehand); an exercise needs one.
    if (is.na(rating) && !session) stop(what, ": velg Bra, OK eller Dårlig.", call. = FALSE)
    list(exercise_no = if (session) NA_integer_ else no,
         exercise_code = if (session) "" else txt1(plan$ovelser[[no]]$kode),
         rating = rating, tags = list(tags), actual_minutes = minutes, actual_players = players,
         body = body, use_again = use_again)
  })
  out <- Filter(Negate(is.null), out)
  if (!length(out)) stop("Vurder minst én øvelse eller hele økta.", call. = FALSE)
  if (anyDuplicated(vapply(out, function(r) if (is.na(r$exercise_no)) 0L else r$exercise_no, 0L))) {
    stop("Hver øvelse kan bare vurderes én gang.", call. = FALSE)
  }
  data.frame(
    exercise_no = vapply(out, `[[`, 0L, "exercise_no"),
    exercise_code = vapply(out, `[[`, "", "exercise_code"),
    rating = vapply(out, `[[`, 0L, "rating"),
    tags = I(lapply(out, function(r) r$tags[[1]])),
    actual_minutes = vapply(out, `[[`, 0L, "actual_minutes"),
    actual_players = vapply(out, `[[`, 0L, "actual_players"),
    body = vapply(out, `[[`, "", "body"),
    use_again = vapply(out, `[[`, "", "use_again"),
    stringsAsFactors = FALSE
  )
}

#' The evaluations of one plan version, summed up per exercise
#'
#' @param fb data.frame from `ds_list_feedback()` for one plan id.
#' @param plan The plan.
#' @return list(session = list(n, ratings (named counts), players, texts =
#'   data.frame(author, body)), exercises = list per exercise of list(no,
#'   name, planned, n, ratings, mean, tags (named counts, most first),
#'   minutes (mean actual), use_again (named counts), texts)).
#' @noRd
feedback_summary <- function(fb, plan) {
  count <- function(x, levels) {
    x <- x[!is.na(x) & nzchar(as.character(x))]
    tab <- table(factor(as.character(x), levels = levels))
    tab <- tab[tab > 0]
    stats::setNames(as.integer(tab), names(tab))
  }
  texts <- function(rows) {
    rows <- rows[!is.na(rows$body) & nzchar(rows$body), , drop = FALSE]
    data.frame(author = rows$author, body = rows$body, stringsAsFactors = FALSE)
  }
  planned <- plan$tidsplan$stasjoner$minutter
  s <- fb[is.na(fb$exercise_no), , drop = FALSE]
  ex <- lapply(seq_along(plan$ovelser), function(k) {
    r <- fb[!is.na(fb$exercise_no) & fb$exercise_no == k, , drop = FALSE]
    tags <- count(unlist(r$tags), names(feedback_tags))
    tags <- tags[order(-tags)]
    list(no = k, name = plan$ovelser[[k]]$navn, code = plan$ovelser[[k]]$kode, planned = planned, n = nrow(r),
         ratings = count(r$rating, c("3", "2", "1")),
         mean = if (nrow(r)) mean(r$rating) else NA_real_,
         tags = tags,
         minutes = if (any(!is.na(r$actual_minutes))) mean(r$actual_minutes, na.rm = TRUE) else NA_real_,
         use_again = count(r$use_again, names(feedback_use_again)),
         texts = texts(r))
  })
  list(session = list(n = nrow(s), ratings = count(s$rating, c("3", "2", "1")),
                      players = if (any(!is.na(s$actual_players))) round(mean(s$actual_players, na.rm = TRUE)) else NA,
                      texts = texts(s)),
       exercises = ex,
       authors = unique(fb$author))
}

# "👍 2 · 👌 1" from counts named "3"/"2"/"1".
feedback_ratings_text <- function(counts) {
  if (!length(counts)) return("")
  icons <- c(`3` = "\U0001F44D", `2` = "\U0001F44C", `1` = "\U0001F44E")
  paste(paste(icons[names(counts)], counts), collapse = " · ")
}

# The message after saving, naming candidates that became active.
feedback_saved_text <- function(activated, plan) {
  if (!length(activated)) return("Evalueringen er lagret. Takk!")
  codes <- vapply(plan$ovelser, function(o) txt1(o$kode), "")
  names <- vapply(plan$ovelser, function(o) txt1(o$navn), "")
  shown <- ifelse(activated %in% codes, names[match(activated, codes)], activated)
  shown <- paste0("«", shown, "»")
  list_txt <- if (length(shown) == 1) shown else paste(paste(utils::head(shown, -1), collapse = ", "), "og",
                                                       utils::tail(shown, 1))
  paste0("Evalueringen er lagret. Takk! ", list_txt, " er nå ", if (length(shown) == 1) "aktiv" else "aktive",
         " i øvelsesbanken.")
}

# Statistics per exercise in the bank (T3e-2) ------------------------------------
# The columns fb_* come from `ds_list_exercises()`.

#' Exercises KI should not use: the trainers rate them below OK on average,
#' or most say «Bruke igjen: Nei»
#' @param bank `ds_list_exercises()`; without fb_* columns nothing is avoided.
#' @return Logical vector, one per row.
#' @noRd
exercise_avoid <- function(bank) {
  if (is.null(bank) || !nrow(bank) || !"fb_n" %in% names(bank)) return(rep(FALSE, NROW(bank)))
  n <- bank$fb_n %||% 0L
  again <- bank$fb_yes + bank$fb_changes + bank$fb_no
  n > 0 & ((!is.na(bank$fb_mean) & bank$fb_mean < 2) | (again > 0 & bank$fb_no * 2 > again))
}

# Mean rating as "2,7" (Norwegian decimal comma).
feedback_mean_text <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = ",")

#' One line with the evaluations of a bank exercise, for the admin panel
#' @param e One row of `ds_list_exercises()`.
#' @return "" when the columns are missing, «Ikke vurdert ennå» when there
#'   are no evaluations.
#' @noRd
exercise_stats_text <- function(e) {
  if (!"fb_n" %in% names(e)) return("")
  if (is.na(e$fb_n) || e$fb_n == 0) return("Ikke vurdert ennå")
  ratings <- feedback_ratings_text(c(`3` = e$fb_good, `2` = e$fb_ok, `1` = e$fb_bad)[c(e$fb_good, e$fb_ok, e$fb_bad) > 0])
  again <- c(yes = e$fb_yes, changes = e$fb_changes, no = e$fb_no)
  again <- again[again > 0]
  tags <- e$fb_tags[[1]]
  tags <- utils::head(tags[names(tags) %in% names(feedback_tags)], 3)
  parts <- c(
    paste0(ratings, " (snitt ", feedback_mean_text(e$fb_mean), ")"),
    paste(e$fb_sessions, if (e$fb_sessions == 1) "økt" else "økter"),
    if (length(again)) paste0("Bruke igjen: ", paste(feedback_use_again[names(again)], again, collapse = ", ")),
    if (!is.na(e$fb_minutes)) paste0("faktisk tid ca. ", round(e$fb_minutes), " min"),
    if (length(tags)) paste0(feedback_tags[names(tags)], " (", tags, ")", collapse = ", "),
    if (!is.na(e$fb_last)) paste("sist", feedback_date_text(e$fb_last))
  )
  paste(parts, collapse = " · ")
}

# "12. okt." (with the year if not this year), Norwegian time.
feedback_date_text <- function(t, tz = "Europe/Oslo", now = Sys.time()) {
  t <- as.POSIXct(t)
  y <- format(t, "%Y", tz = tz)
  paste0(as.integer(format(t, "%d", tz = tz)), ". ", no_month(t, tz),
         if (y != format(now, "%Y", tz = tz)) paste0(" ", y) else "")
}

# Lærdommer (team_learnings, T3e-4) --------------------------------------------------

learning_max_chars <- 3000

#' Check the team's learnings before they are saved or sent to KI
#' @param names Member names the text must not contain (`member_names()`).
#' @return The trimmed text. Stops with a Norwegian message.
#' @noRd
learning_validate <- function(body, names = character(), allow_empty = FALSE) {
  body <- gsub("\n{3,}", "\n\n", gsub("\r\n?", "\n", txt1(body)))
  if (!nzchar(body)) {
    if (allow_empty) return("")
    stop("Skriv lærdommene først.", call. = FALSE)
  }
  if (nchar(body) > learning_max_chars) stop("Lærdommene kan ha maks ", learning_max_chars, " tegn.", call. = FALSE)
  if (text_is_sensitive(body)) {
    stop("Lærdommene skal ikke inneholde helseopplysninger eller andre sensitive opplysninger.", call. = FALSE)
  }
  problem <- names_problem(body, names)
  if (!is.null(problem)) stop(problem, call. = FALSE)
  body
}
