#' Allowlist for login (hvitliste)
#'
#' Only people on the list may log in, plus the superadmin
#' (SPONDNYMARK_SUPERADMIN) and trainers with admin in `app_roles`, so nobody
#' can lock everyone out. Admins edit the list in the admin panel.
#'
#' A person is listed by the e-mail address or mobile number they log in to
#' Spond with. These are never stored as text: the list holds an HMAC-SHA256
#' of the normalised value, keyed with SPONDNYMARK_LOGIN_KEY, and a masked
#' hint («k•••@gmail.com», «+47 •••• ••12») so admins can tell the rows apart.
#' If the key is changed, the list must be filled in again.
#'
#' @name fct_allowlist
#' @noRd
NULL

#' Read what the user typed as an e-mail address or a mobile number
#'
#' E-mail is lower-cased. Mobile numbers get the form +4799999999: spaces,
#' dashes, dots and brackets are removed, 00 becomes +, and an 8-digit number
#' is taken as Norwegian.
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

#' The key for the hashes, from the environment
#' @noRd
login_key <- function() Sys.getenv("SPONDNYMARK_LOGIN_KEY")

#' HMAC-SHA256 of a normalised identifier, as 64 hex characters
#' @noRd
login_hash <- function(ident, key = login_key()) {
  if (!nzchar(key)) stop("SPONDNYMARK_LOGIN_KEY er ikke satt.", call. = FALSE)
  as.character(openssl::sha256(paste0(ident$kind, ":", ident$value), key = key))
}

#' A masked form of the identifier, for the list in the admin panel
#' @noRd
login_hint <- function(ident) {
  dot <- "•"
  if (ident$kind == "email") {
    parts <- strsplit(ident$value, "@", fixed = TRUE)[[1]]
    return(paste0(substr(parts[1], 1, 1), strrep(dot, 3), "@", parts[2]))
  }
  v <- ident$value
  cc <- if (startsWith(v, "+47")) "+47" else substr(v, 1, 3)
  paste0(cc, " ", strrep(dot, 4), " ", strrep(dot, 2), substr(v, nchar(v) - 1, nchar(v)))
}

#' May this person log in?
#'
#' Checked after Spond has accepted the password, so only people with a
#' Spond account learn whether they are on the list.
#' @param db The session's database handle (`db_handle()`).
#' @return "ok", "denied" or "error" (database or key missing; nobody but the
#'   superadmin gets in then).
#' @noRd
login_check <- function(db, ident, profile_id, superadmin = Sys.getenv("SPONDNYMARK_SUPERADMIN"),
                        key = login_key()) {
  if (is_superadmin(profile_id, superadmin)) return("ok")
  tryCatch({
    if (!nzchar(key)) stop("SPONDNYMARK_LOGIN_KEY er ikke satt.")
    db$run(function(con) {
      role <- ds_get_role(con, profile_id)
      if (isTRUE(role$is_admin[1])) return("ok")
      if (ds_allowlist_login(con, login_hash(ident, key), profile_id)) "ok" else "denied"
    })
  }, error = function(e) {
    message("Sjekk av hvitlisten feilet: ", conditionMessage(e))
    "error"
  })
}
