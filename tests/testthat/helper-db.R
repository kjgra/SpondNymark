# Database tests run only when SPONDNYMARK_TEST_DB_URL points to a Postgres
# database you are allowed to write to, e.g. in .Renviron:
#   SPONDNYMARK_TEST_DB_URL=postgresql://user:password@localhost:5432/spondtest
# Each test gets its own temporary schema, which is dropped afterwards, so the
# tests never touch the app's real tables. Do not point this at production.

test_db_connect <- function(url) {
  if (requireNamespace("RPostgres", quietly = TRUE)) {
    return(DBI::dbConnect(RPostgres::Postgres(), dbname = url))
  }
  if (requireNamespace("RPostgreSQL", quietly = TRUE)) {
    m <- regmatches(url, regexec("^postgres(?:ql)?://([^:]+):([^@]*)@([^:/]+)(?::(\\d+))?/([^?]+)", url, perl = TRUE))[[1]]
    return(DBI::dbConnect(RPostgreSQL::PostgreSQL(), user = m[2], password = m[3], host = m[4],
                          port = if (nzchar(m[5])) as.integer(m[5]) else 5432L, dbname = m[6]))
  }
  testthat::skip("Ingen Postgres-driver (RPostgres) installert.")
}

local_test_db <- function(env = parent.frame()) {
  url <- Sys.getenv("SPONDNYMARK_TEST_DB_URL")
  testthat::skip_if(!nzchar(url), "SPONDNYMARK_TEST_DB_URL er ikke satt.")
  con <- test_db_connect(url)
  DBI::dbExecute(con, "SET client_min_messages TO warning")
  schema <- paste0("test_", paste(sample(letters, 10, replace = TRUE), collapse = ""))
  DBI::dbExecute(con, paste0("CREATE SCHEMA ", schema))
  DBI::dbExecute(con, paste0("SET search_path TO ", schema))
  withr::defer({
    DBI::dbExecute(con, paste0("DROP SCHEMA ", schema, " CASCADE"))
    DBI::dbDisconnect(con)
  }, envir = env)
  ds_migrate(con, dir = test_migrations_dir())
  con
}

test_migrations_dir <- function() {
  installed <- system.file("db", "migrations", package = "SpondNymark")
  if (nzchar(installed)) installed else testthat::test_path("..", "..", "inst", "db", "migrations")
}

# Access for a trainer in group G1 only.
test_access <- function() {
  data.frame(id = "G1", name = "Testlag", roles = "Teamleder", n_subgroups = 2L)
}
