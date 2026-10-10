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
