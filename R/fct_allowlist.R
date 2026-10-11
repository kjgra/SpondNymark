#' Who may log in (app-tilgang)
#'
#' Only trainers who have been given app access in the admin panel
#' («Trenere» → App) may use the app, plus admins and the superadmin
#' (SPONDNYMARK_SUPERADMIN), so nobody can lock everyone out. The rights are
#' stored per Spond profile ID in `app_roles`; no e-mail addresses or phone
#' numbers are stored. (Until 10 Oct 2026 this was an allowlist of hashed
#' e-mail addresses and numbers, migration 006; removed in 009.)
#'
#' @name fct_allowlist
#' @noRd
NULL

#' Read what the user typed as an e-mail address or a mobile number
#'
#' E-mail is lower-cased. Mobile numbers get the form +4799999999: spaces,
#' dashes, dots and brackets are removed, 00 becomes +, and an 8-digit number
#' is taken as Norwegian. Spond takes both in its login field.
#' @return list(kind = "email"/"phone", value) or NULL if it is neither.
#' @noRd
login_identifier <- function(x) {
  x <- txt1(x)
  if (!nzchar(x)) return(NULL)
  if (grepl("@", x, fixed = TRUE)) {
    x <- tolower(x)
    if (!grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[a-z]{2,}$", x)) return(NULL)
    return(list(kind = "email", value = x))
  }
  d <- gsub("[[:space:].()-]", "", x)
  d <- sub("^00", "+", d)
  if (grepl("^[0-9]{8}$", d)) d <- paste0("+47", d)
  if (!grepl("^\\+[0-9]{8,15}$", d)) return(NULL)
  list(kind = "phone", value = d)
}

#' May this Spond profile use the app?
#'
#' Checked after Spond has accepted the password.
#' @param db The session's database handle (`db_handle()`).
#' @return "ok", "denied" or "error" (database problem; nobody but the
#'   superadmin gets in then).
#' @noRd
login_check <- function(db, profile_id, superadmin = Sys.getenv("SPONDNYMARK_SUPERADMIN")) {
  if (is_superadmin(profile_id, superadmin)) return("ok")
  tryCatch({
    role <- db$run(function(con) ds_get_role(con, profile_id))
    if (isTRUE(role$is_admin[1]) || isTRUE(role$can_use_app[1])) "ok" else "denied"
  }, error = function(e) {
    message("Sjekk av app-tilgang feilet: ", conditionMessage(e))
    "error"
  })
}

# Limits on failed logins ---------------------------------------------------------
# Every login attempt is passed on to Spond. Without limits, the app could be
# used to guess passwords, and Spond could block the server's address, so
# nobody could log in. Failed attempts are counted in memory (never stored):
# per browser session, per e-mail/mobile number (as a hash) and for the whole
# R process.

login_limits <- list(
  session = list(max = 5, window = 60),        # 5 failures a minute in one browser
  ident = list(max = 10, window = 15 * 60),    # 10 failures per account per 15 min
  all = list(max = 60, window = 15 * 60)       # 60 failures in all per 15 min
)

#' A store of failed attempts: an environment of key -> times (seconds)
#' @noRd
login_throttle_new <- function() new.env(parent = emptyenv())

# Shared by all sessions in this R process.
login_throttle <- login_throttle_new()

#' The key for an identifier: a hash, so no e-mail or number is kept
#' @noRd
login_key <- function(ident) {
  paste0("id:", as.character(openssl::sha256(paste0(ident$kind, ":", ident$value))))
}

login_recent <- function(store, key, window, now) {
  t <- store[[key]] %||% numeric()
  t[t > now - window]
}

#' Seconds to wait before the next attempt (0 = go ahead)
#' @param keys Named character vector: names are entries in `limits`
#'   ("session", "ident", "all"), values are the keys; `stores` likewise.
#' @noRd
login_wait <- function(stores, keys, now = as.numeric(Sys.time()), limits = login_limits) {
  wait <- 0
  for (k in names(keys)) {
    lim <- limits[[k]]
    t <- login_recent(stores[[k]], keys[[k]], lim$window, now)
    if (length(t) >= lim$max) wait <- max(wait, sort(t, decreasing = TRUE)[lim$max] + lim$window - now)
  }
  ceiling(wait)
}

#' Count a failed attempt
#' @noRd
login_failed <- function(stores, keys, now = as.numeric(Sys.time()), limits = login_limits) {
  for (k in names(keys)) {
    lim <- limits[[k]]
    st <- stores[[k]]
    assign(keys[[k]], c(login_recent(st, keys[[k]], lim$window, now), now), envir = st)
    # Forget keys with no recent failures, so the store does not grow.
    for (old in setdiff(ls(st, all.names = TRUE), keys[[k]])) {
      if (!length(login_recent(st, old, lim$window, now))) rm(list = old, envir = st)
    }
  }
  invisible(TRUE)
}

#' A successful login clears the failures for that browser and account
#' @noRd
login_succeeded <- function(stores, keys) {
  for (k in intersect(names(keys), c("session", "ident"))) {
    if (exists(keys[[k]], envir = stores[[k]], inherits = FALSE)) rm(list = keys[[k]], envir = stores[[k]])
  }
  invisible(TRUE)
}

#' "1 minutt", "3 minutter", "40 sekunder"
#' @noRd
login_wait_text <- function(sec) {
  if (sec < 60) return(paste(sec, "sekunder"))
  m <- ceiling(sec / 60)
  paste(m, if (m == 1) "minutt" else "minutter")
}
