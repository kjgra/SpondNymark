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
