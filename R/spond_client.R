#' Spond API client
#'
#' A thin wrapper around Spond's unofficial API, modelled on the Python
#' library Olen/Spond (documented at https://martcl.github.io/spond/).
#' The API is unofficial and may change without notice, so everything that
#' talks to Spond goes through these functions.
#'
#' The password is only sent to Spond in `spond_login()` and is never stored.
#' Callers keep the returned session (token) in memory for the Shiny session.
#'
#' @name spond_client
#' @noRd
NULL

spond_base_url <- function() {
  "https://api.spond.com/core/v1/"
}

# Spond expects timestamps on this form (see Olen/Spond `_DT_FORMAT`).
spond_format_time <- function(x) {
  if (is.null(x)) return(NULL)
  format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%dT%H:%M:%S.000Z", tz = "UTC")
}

#' Log in to Spond
#'
#' @param email,password Spond credentials. The password is not stored.
#' @param base_url API base URL (override in tests).
#' @return A `spond_session`: a list with `token`, `expiration` and `base_url`.
#' @noRd
spond_login <- function(email, password, base_url = spond_base_url()) {
  stopifnot(is.character(email), length(email) == 1, nzchar(email))
  stopifnot(is.character(password), length(password) == 1, nzchar(password))

  resp <- httr2::request(base_url) |>
    httr2::req_url_path_append("auth2", "login") |>
    httr2::req_user_agent("SpondNymark") |>
    httr2::req_timeout(30) |>
    httr2::req_body_json(list(email = email, password = password)) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  status <- httr2::resp_status(resp)
  if (status %in% c(400, 401, 403)) {
    stop("Innlogging mot Spond feilet: feil e-post eller passord.", call. = FALSE)
  }
  if (status >= 400) {
    stop(sprintf("Innlogging mot Spond feilet (HTTP %s).", status), call. = FALSE)
  }

  body <- httr2::resp_body_json(resp)
  token <- body$accessToken$token
  if (is.null(token) || !nzchar(token)) {
    # Not the expected shape. One possible cause is two-factor login.
    # Report only the field names, never the values.
    stop(
      "Spond svarte uten tilgangstoken. Feltene i svaret var: ",
      paste(names(body), collapse = ", "),
      ". Dette kan bety at kontoen krever ekstra verifisering (f.eks. tofaktor).",
      call. = FALSE
    )
  }

  structure(
    list(
      token = token,
      expiration = body$accessToken$expiration,
      base_url = base_url
    ),
    class = "spond_session"
  )
}

#' @export
print.spond_session <- function(x, ...) {
  # Never print the token itself.
  cat("<spond_session>", if (!is.null(x$expiration)) paste("utløper", x$expiration), "\n")
  invisible(x)
}

# Builds an authenticated GET request. Kept separate so tests can inspect it.
spond_request <- function(session, ...) {
  stopifnot(inherits(session, "spond_session"))
  httr2::request(session$base_url) |>
    httr2::req_url_path_append(...) |>
    httr2::req_auth_bearer_token(session$token) |>
    httr2::req_user_agent("SpondNymark") |>
    httr2::req_timeout(30)
}

spond_get_json <- function(req) {
  resp <- req |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()
  status <- httr2::resp_status(resp)
  if (status == 401) {
    stop("Spond-sesjonen er utløpt. Logg inn på nytt.", call. = FALSE)
  }
  if (status >= 400) {
    stop(sprintf("Kall mot Spond feilet (HTTP %s).", status), call. = FALSE)
  }
  # simplifyVector = FALSE keeps Spond's nested structure as plain lists.
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

#' The logged-in user's own profile
#' @noRd
spond_get_profile <- function(session) {
  spond_get_json(spond_request(session, "profile"))
}

#' All groups the logged-in user belongs to, with members, roles and subgroups
#' @noRd
spond_get_groups <- function(session) {
  spond_get_json(spond_request(session, "groups", ""))
}

# Query parameters for `sponds/`, named as in Olen/Spond.
spond_events_query <- function(group_id = NULL,
                               subgroup_id = NULL,
                               min_start = NULL,
                               max_start = NULL,
                               min_end = NULL,
                               max_end = NULL,
                               include_scheduled = FALSE,
                               max_events = 100) {
  q <- list(
    max = as.character(max_events),
    scheduled = if (isTRUE(include_scheduled)) "true" else "false",
    groupId = group_id,
    subGroupId = subgroup_id,
    minStartTimestamp = spond_format_time(min_start),
    maxStartTimestamp = spond_format_time(max_start),
    minEndTimestamp = spond_format_time(min_end),
    maxEndTimestamp = spond_format_time(max_end)
  )
  q[!vapply(q, is.null, logical(1))]
}

#' Events ("sponds"), optionally for one group or subgroup and a time window
#' @noRd
spond_get_events <- function(session, ...) {
  req <- spond_request(session, "sponds", "")
  req <- httr2::req_url_query(req, !!!spond_events_query(...))
  spond_get_json(req)
}
