#' Timing in the log (performance, plan «Etter T3», step 1)
#'
#' Measure before changing anything: every call to Spond, every new database
#' connection and slow database queries are written to the log as
#' `[tid] <økt> <hva>: <ms> ms (<detaljer>)`. On Posit Connect the log is
#' under «Logs» for the app. A new session also says how long the R process
#' has been running, which shows whether the app had to start from cold.
#'
#' `SPONDNYMARK_TIMING` (environment variable): "on" (default) logs Spond,
#' connections and queries over `timing_slow_ms`; "all" logs every query;
#' "off" logs nothing. No names, ids or query parameters are logged, only
#' the kind of call, the table and the size of the answer.
#' @name fct_timing
#' @noRd
NULL

.sn_timing <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  .sn_timing$process_start <- Sys.time()
  invisible()
}

timing_slow_ms <- 250

timing_mode <- function() {
  m <- tolower(trimws(Sys.getenv("SPONDNYMARK_TIMING", "on")))
  if (m %in% c("on", "all", "off")) m else "on"
}

# A short random tag per Shiny session, so the lines of one session can be
# followed in the log (set in app_server()).
timing_session_tag <- function() {
  s <- tryCatch(shiny::getDefaultReactiveDomain(), error = function(e) NULL)
  tag <- if (!is.null(s)) tryCatch(s$userData$sn_tag, error = function(e) NULL)
  if (is.null(tag)) "" else paste0(tag, " ")
}

#' Write one timing line
#' @param always FALSE for lines that are only written when slow (or in
#'   "all" mode).
#' @noRd
timing_log <- function(label, ms, detail = "", always = TRUE) {
  mode <- timing_mode()
  if (mode == "off") return(invisible(FALSE))
  if (!always && mode != "all" && ms < timing_slow_ms) return(invisible(FALSE))
  message(sprintf("[tid] %s%s: %d ms%s", timing_session_tag(), label, as.integer(round(ms)),
                  if (nzchar(detail)) paste0(" (", detail, ")") else ""))
  invisible(TRUE)
}

# A note without a duration, e.g. a new session.
timing_note <- function(text) {
  if (timing_mode() == "off") return(invisible(FALSE))
  message(sprintf("[tid] %s%s", timing_session_tag(), text))
  invisible(TRUE)
}

#' Run `expr` and log how long it took (also when it fails)
#' @noRd
timed <- function(label, expr, always = TRUE) {
  t0 <- proc.time()[["elapsed"]]
  on.exit(timing_log(label, (proc.time()[["elapsed"]] - t0) * 1000, always = always), add = TRUE)
  expr
}

# Seconds since the R process loaded the app.
timing_process_age <- function(now = Sys.time()) {
  start <- .sn_timing$process_start
  if (is.null(start)) return(NA_real_)
  as.numeric(difftime(now, start, units = "secs"))
}

#' "SELECT exercises" from a query: the kind and the first table, never
#' the values
#' @noRd
ds_sql_label <- function(sql) {
  s <- gsub("\\s+", " ", trimws(sql))
  verb <- toupper(sub("^([A-Za-z]+).*$", "\\1", s))
  # The table after FROM/INTO/UPDATE that is not a subquery or a CTE name.
  m <- regmatches(s, gregexpr("(?i)\\b(from|into|update|join)\\s+([a-z_][a-z0-9_.]*)", s, perl = TRUE))[[1]]
  tabs <- tolower(sub("(?i)^\\S+\\s+", "", m, perl = TRUE))
  tabs <- tabs[!tabs %in% c("unnest", "jsonb_array_elements_text", "f", "x", "e", "ends", "keep", "gone")]
  paste(c("db", verb, utils::head(tabs, 1)), collapse = " ")
}

# "12 kB" / "850 B".
timing_size <- function(bytes) {
  if (is.na(bytes)) return("")
  if (bytes >= 1024) paste(round(bytes / 1024), "kB") else paste(bytes, "B")
}
