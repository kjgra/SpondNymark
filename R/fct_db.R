#' One database connection per Shiny session
#'
#' Connects on first use (so the login page works even if the database is
#' slow), and reconnects once if the connection has been dropped, e.g. by the
#' Supabase pooler after a long idle period. Statements are only retried when
#' the connection is dead, so a failed statement is never run twice on a
#' working connection.
#'
#' @param connect Function returning a DBI connection.
#' @return list(run = function(f) f(con), close = function())
#' @noRd
db_handle <- function(connect = ds_connect) {
  con <- NULL
  get <- function() {
    if (is.null(con) || !DBI::dbIsValid(con)) con <<- connect()
    con
  }
  alive <- function() {
    !is.null(con) && isTRUE(tryCatch({
      DBI::dbGetQuery(con, "SELECT 1")
      TRUE
    }, error = function(e) FALSE))
  }
  close <- function() {
    if (!is.null(con)) try(DBI::dbDisconnect(con), silent = TRUE)
    con <<- NULL
    invisible(NULL)
  }
  # The connection is fetched before calling `f`, so it is opened even if `f`
  # never uses its argument (R evaluates arguments lazily).
  run_once <- function(f) {
    current <- get()
    f(current)
  }
  run <- function(f) {
    tryCatch(run_once(f), error = function(e) {
      if (alive()) stop(e)
      close()
      run_once(f)
    })
  }
  list(run = run, close = close)
}
