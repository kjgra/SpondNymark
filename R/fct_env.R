#' Environment variables and secrets
#'
#' All secrets come from environment variables, never from code or
#' golem-config.yml:
#' - Locally they live in `.Renviron` in the project folder (see
#'   `.Renviron.example`). `.Renviron` is in .gitignore, .Rbuildignore and
#'   .rscignore, so it never reaches git, the package build or Posit Connect.
#' - On Posit Connect they are set under Settings -> Runtime ->
#'   Environment Variables, where they are stored encrypted.
#'
#' @name fct_env
#' @noRd
NULL

# Variables the running app needs. Optional variables (SPONDNYMARK_ENV, and
# the local-only SPONDNYMARK_TEST_DB_URL, SPONDNYMARK_TEST_APP_DB_URL,
# SUPABASE_POOLER_URL, SPOND_EMAIL) are documented in .Renviron.example.
required_env_vars <- c("SPONDNYMARK_DB_URL")

#' Which environment the app runs in (SPONDNYMARK_ENV)
#'
#' Empty, "prod" or "production" means production. Anything else (e.g.
#' "test") is shown as a label in the top bar and the browser tab, so the
#' test app is never mistaken for the real one.
#' @return The label in upper case, or "" for production.
#' @noRd
app_env_label <- function(env = Sys.getenv("SPONDNYMARK_ENV")) {
  env <- toupper(trimws(env))
  if (!nzchar(env) || env %in% c("PROD", "PRODUCTION")) return("")
  substr(gsub("[^A-Z0-9 _-]", "", env), 1, 12)
}

#' Browser tab title, e.g. "SpondNymark" or "SpondNymark (TEST)"
#' @noRd
app_title <- function(env = Sys.getenv("SPONDNYMARK_ENV")) {
  label <- app_env_label(env)
  if (nzchar(label)) paste0("SpondNymark (", label, ")") else "SpondNymark"
}

#' Label next to the app name in the top bar, or NULL in production
#' @noRd
app_env_badge <- function(env = Sys.getenv("SPONDNYMARK_ENV")) {
  label <- app_env_label(env)
  if (nzchar(label)) shiny::span(class = "sn-env", label) else NULL
}

#' Stop with a clear message if a required environment variable is missing
#'
#' Called when the app starts. Names the missing variables but never prints
#' any values.
#' @noRd
check_env <- function(vars = required_env_vars) {
  missing <- vars[!nzchar(Sys.getenv(vars))]
  if (length(missing)) {
    stop(
      "Mangler miljøvariabel: ", paste(missing, collapse = ", "), ".\n",
      "Lokalt: legg den i .Renviron i prosjektmappen (se .Renviron.example) og start R på nytt.\n",
      "Posit Connect: legg den inn under Settings -> Runtime -> Environment Variables.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}
