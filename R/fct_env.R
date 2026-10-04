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

# Variables the running app needs. Optional, local-only variables
# (SPONDNYMARK_TEST_DB_URL, SPOND_EMAIL) are documented in .Renviron.example.
required_env_vars <- c("SPONDNYMARK_DB_URL")

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
