#' Data layer
#'
#' All reads and writes to the database go through these functions. Shiny
#' modules never write SQL themselves. Every function that touches a group's
#' data takes `access` (from `spond_accessible_groups()`) and stops unless the
#' logged-in user is a trainer/leader in that group.
#'
#' We only store Spond IDs. Names and other personal data stay in Spond.
#'
#' Driver note: SQL uses `$1`-style parameters and never binds `NA`/`NULL`
#' directly. Optional IDs are passed as "" and turned into NULL with
#' `NULLIF($n, '')`, which behaves the same in RPostgres and RPostgreSQL.
#'
#' @name data_store
#' @noRd
NULL

# Connection --------------------------------------------------------------------

#' Connect to the database
#'
#' Reads the connection string from the environment variable
#' `SPONDNYMARK_DB_URL`, never from code. dev/setup_db.R writes it to
#' `.Renviron`. It logs in as the app user `spondnymark_app`, which can only
#' read and write rows in the app's tables.
#' @noRd
ds_connect <- function(url = Sys.getenv("SPONDNYMARK_DB_URL")) {
  if (!nzchar(url)) {
    stop("SPONDNYMARK_DB_URL er ikke satt. Legg tilkoblingsstrengen i .Renviron.", call. = FALSE)
  }
  DBI::dbConnect(RPostgres::Postgres(), dbname = url)
}

# Helpers -----------------------------------------------------------------------

# "" for missing optional values (see driver note above).
ds_opt <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) "" else as.character(x[1])
}

# A Postgres text[] literal, e.g. {"a","b"}, for `= ANY($n::text[])`.
ds_text_array <- function(x) {
  x <- as.character(x)
  if (length(x) == 0) return("{}")
  esc <- gsub('(["\\\\])', "\\\\\\1", x)
  paste0("{", paste0('"', esc, '"', collapse = ","), "}")
}

ds_query <- function(con, sql, params = list()) {
  if (length(params)) DBI::dbGetQuery(con, sql, params = unname(params)) else DBI::dbGetQuery(con, sql)
}

ds_exec <- function(con, sql, params = list()) {
  if (length(params)) DBI::dbExecute(con, sql, params = unname(params)) else DBI::dbExecute(con, sql)
}

ds_transaction <- function(con, code) {
  DBI::dbWithTransaction(con, code)
}

# The group a proposal belongs to, after checking the user may see it.
ds_proposal_group <- function(con, access, proposal_id) {
  row <- ds_query(con, "SELECT spond_group_id, status FROM group_proposals WHERE id = $1", list(as.integer(proposal_id)))
  if (nrow(row) == 0) stop("Fant ikke forslaget.", call. = FALSE)
  assert_group_access(access, row$spond_group_id)
  row
}

ds_log <- function(con, proposal_id, actor, decision) {
  ds_exec(con, "INSERT INTO proposal_history (proposal_id, actor, decision) VALUES ($1, $2, $3)",
          list(as.integer(proposal_id), actor, decision))
}

# Migrations --------------------------------------------------------------------

ds_split_sql <- function(sql) {
  lines <- strsplit(sql, "\n", fixed = TRUE)[[1]]
  lines <- lines[!grepl("^\\s*--", lines)]
  stmts <- strsplit(paste(lines, collapse = "\n"), ";\\s*(\n|$)")[[1]]
  stmts <- trimws(stmts)
  stmts[nzchar(stmts)]
}

#' Apply SQL migrations that have not been applied yet
#'
#' Run by the admin from dev/setup_db.R, never by the app (the app user is
#' not allowed to change tables). Safe to run again: applied files are skipped.
#' Each file runs in its own transaction.
#' @return The versions that were applied (invisibly).
#' @noRd
ds_migrate <- function(con, dir = app_sys("db", "migrations")) {
  ds_exec(con, "CREATE TABLE IF NOT EXISTS schema_migrations (version text PRIMARY KEY, applied_at timestamptz NOT NULL DEFAULT now())")
  done <- ds_query(con, "SELECT version FROM schema_migrations")$version
  files <- sort(list.files(dir, pattern = "\\.sql$", full.names = TRUE))
  applied <- character()
  for (f in files) {
    version <- sub("\\.sql$", "", basename(f))
    if (version %in% done) next
    sql <- paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
    ds_transaction(con, {
      for (stmt in ds_split_sql(sql)) ds_exec(con, stmt)
      ds_exec(con, "INSERT INTO schema_migrations (version) VALUES ($1)", list(version))
    })
    applied <- c(applied, version)
  }
  invisible(applied)
}

# Tags --------------------------------------------------------------------------

ds_list_tags <- function(con, access, group_id) {
  assert_group_access(access, group_id)
  ds_query(con, "SELECT spond_member_id, tag FROM member_tags WHERE spond_group_id = $1 ORDER BY spond_member_id, tag",
           list(group_id))
}

ds_add_tag <- function(con, access, group_id, member_id, tag, actor) {
  assert_group_access(access, group_id)
  tag <- trimws(tag)
  if (!nzchar(tag) || nchar(tag) > 40) stop("En tag må ha mellom 1 og 40 tegn.", call. = FALSE)
  ds_exec(con, "INSERT INTO member_tags (spond_group_id, spond_member_id, tag, created_by)
                VALUES ($1, $2, $3, $4) ON CONFLICT DO NOTHING",
          list(group_id, member_id, tag, actor))
  invisible(TRUE)
}

ds_remove_tag <- function(con, access, group_id, member_id, tag) {
  assert_group_access(access, group_id)
  ds_exec(con, "DELETE FROM member_tags WHERE spond_group_id = $1 AND spond_member_id = $2 AND tag = $3",
          list(group_id, member_id, tag))
  invisible(TRUE)
}

# Proposals ---------------------------------------------------------------------

#' Proposals visible in a context
#'
#' @param event_ids The Spond events visible in the current context (from
#'   Spond). Proposals tied to these events are returned.
#' @param subgroup_id The selected subgroup, or NULL for the whole group.
#'   Gruppeutkast (no event) are returned for this context; the whole group
#'   sees all of them.
#' @noRd
ds_list_proposals <- function(con, access, group_id, event_ids = character(), subgroup_id = NULL) {
  assert_group_access(access, group_id)
  ds_query(con, "
    SELECT p.id, p.spond_subgroup_id, p.spond_event_id, p.name, p.status,
           p.created_by, p.created_at, p.updated_at,
           (SELECT count(*)::integer FROM proposal_comments c WHERE c.proposal_id = p.id) AS n_comments
      FROM group_proposals p
     WHERE p.spond_group_id = $1
       AND p.status <> 'deleted'
       AND (p.spond_event_id = ANY($2::text[])
            OR (p.spond_event_id IS NULL AND ($3 = '' OR p.spond_subgroup_id = $3)))
     ORDER BY p.created_at, p.id",
    list(group_id, ds_text_array(event_ids), ds_opt(subgroup_id)))
}

#' One proposal with its groups, members, history and comments
#' @noRd
ds_get_proposal <- function(con, access, proposal_id) {
  ds_proposal_group(con, access, proposal_id)
  id <- as.integer(proposal_id)
  list(
    proposal = ds_query(con, "SELECT id, spond_group_id, spond_subgroup_id, spond_event_id, name, status,
                                     created_by, created_at, updated_at
                                FROM group_proposals WHERE id = $1", list(id)),
    labels = ds_query(con, "SELECT label, sort_order FROM group_proposal_labels WHERE proposal_id = $1 ORDER BY sort_order", list(id)),
    members = ds_query(con, "SELECT spond_member_id, label FROM group_proposal_members WHERE proposal_id = $1 ORDER BY label, spond_member_id", list(id)),
    history = ds_query(con, "SELECT actor, decision, created_at FROM proposal_history WHERE proposal_id = $1 ORDER BY created_at, id", list(id)),
    comments = ds_query(con, "SELECT id, author, body, created_at FROM proposal_comments WHERE proposal_id = $1 ORDER BY created_at, id", list(id))
  )
}

ds_validate_proposal <- function(name, labels, assignments) {
  name <- trimws(name)
  if (!nzchar(name) || nchar(name) > 120) stop("Navnet på forslaget må ha mellom 1 og 120 tegn.", call. = FALSE)
  labels <- trimws(as.character(labels))
  if (length(labels) == 0) stop("Forslaget må ha minst én gruppe.", call. = FALSE)
  if (any(!nzchar(labels)) || any(nchar(labels) > 60)) stop("Gruppenavn må ha mellom 1 og 60 tegn.", call. = FALSE)
  if (anyDuplicated(labels)) stop("To grupper kan ikke ha samme navn.", call. = FALSE)
  if (length(assignments)) {
    if (is.null(names(assignments)) || any(!nzchar(names(assignments)))) {
      stop("Fordelingen må være en navngitt vektor: medlems-ID = gruppenavn.", call. = FALSE)
    }
    if (anyDuplicated(names(assignments))) stop("Et medlem kan bare være i én gruppe.", call. = FALSE)
    unknown <- setdiff(unique(assignments), labels)
    if (length(unknown)) stop("Ukjent gruppe i fordelingen: ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  list(name = name, labels = labels)
}

ds_write_groups <- function(con, id, labels, assignments) {
  ds_exec(con, "DELETE FROM group_proposal_labels WHERE proposal_id = $1", list(id))
  for (i in seq_along(labels)) {
    ds_exec(con, "INSERT INTO group_proposal_labels (proposal_id, label, sort_order) VALUES ($1, $2, $3)",
            list(id, labels[i], i))
  }
  for (m in names(assignments)) {
    ds_exec(con, "INSERT INTO group_proposal_members (proposal_id, spond_member_id, label) VALUES ($1, $2, $3)",
            list(id, m, unname(assignments[[m]])))
  }
}

#' Create or update a proposal
#'
#' @param assignments Named character vector: names are Spond member IDs,
#'   values are group labels. Members not listed are unassigned.
#' @param id NULL to create, or the id of an existing proposal to edit.
#'   Only proposals in status draft, rolled_back or rejected can be edited;
#'   editing sets the status back to draft.
#' @param submit Also send the proposal for approval.
#' @return The proposal id.
#' @noRd
ds_save_proposal <- function(con, access, actor, group_id, name, labels, assignments = character(),
                             subgroup_id = NULL, event_id = NULL, id = NULL, submit = FALSE) {
  v <- ds_validate_proposal(name, labels, assignments)
  if (is.null(id)) {
    assert_group_access(access, group_id)
    # Event-bound proposals follow the event's visibility, so they carry no subgroup.
    sub <- if (is.null(event_id)) ds_opt(subgroup_id) else ""
    ds_transaction(con, {
      id <- ds_query(con, "INSERT INTO group_proposals (spond_group_id, spond_subgroup_id, spond_event_id, name, created_by)
                           VALUES ($1, NULLIF($2, ''), NULLIF($3, ''), $4, $5) RETURNING id",
                     list(group_id, sub, ds_opt(event_id), v$name, actor))$id
      ds_write_groups(con, id, v$labels, assignments)
      ds_log(con, id, actor, "created")
      if (isTRUE(submit)) {
        ds_exec(con, "UPDATE group_proposals SET status = 'pending', updated_at = now() WHERE id = $1", list(id))
        ds_log(con, id, actor, "sent_for_approval")
      }
    })
    return(as.integer(id))
  }

  current <- ds_proposal_group(con, access, id)
  if (!identical(current$spond_group_id, group_id)) stop("Forslaget hører til en annen gruppe.", call. = FALSE)
  id <- as.integer(id)
  ds_transaction(con, {
    n <- ds_exec(con, "UPDATE group_proposals SET name = $2, status = 'draft', updated_at = now()
                        WHERE id = $1 AND status IN ('draft', 'rolled_back', 'rejected')",
                 list(id, v$name))
    if (n == 0) stop("Forslaget kan ikke redigeres når status er «", current$status, "».", call. = FALSE)
    ds_write_groups(con, id, v$labels, assignments)
    ds_log(con, id, actor, "edited")
    if (isTRUE(submit)) {
      ds_exec(con, "UPDATE group_proposals SET status = 'pending', updated_at = now() WHERE id = $1", list(id))
      ds_log(con, id, actor, "sent_for_approval")
    }
  })
  id
}

# Allowed status changes: action -> from-statuses, new status, history entry.
ds_transitions <- list(
  submit   = list(from = "draft", to = "pending", log = "sent_for_approval"),
  approve  = list(from = "pending", to = "approved", log = "approved"),
  reject   = list(from = "pending", to = "rejected", log = "rejected"),
  rollback = list(from = "approved", to = "rolled_back", log = "rolled_back"),
  delete   = list(from = c("draft", "rolled_back", "rejected"), to = "deleted", log = "deleted")
)

ds_status_label <- c(draft = "Utkast", pending = "Til godkjenning", approved = "Godkjent",
                     rejected = "Avslått", rolled_back = "Rullet tilbake", deleted = "Slettet")

#' Change a proposal's status (submit, approve, reject, rollback, delete)
#'
#' Delete is soft: the row and its history stay, but the proposal disappears
#' from all lists. The update only succeeds from the allowed statuses, so two
#' trainers clicking at the same time cannot both succeed.
#' @noRd
ds_transition <- function(con, access, proposal_id, action, actor) {
  t <- ds_transitions[[action]]
  if (is.null(t)) stop("Ukjent handling: ", action, call. = FALSE)
  current <- ds_proposal_group(con, access, proposal_id)
  id <- as.integer(proposal_id)
  ds_transaction(con, {
    n <- ds_exec(con, "UPDATE group_proposals SET status = $2, updated_at = now()
                        WHERE id = $1 AND status = ANY($3::text[])",
                 list(id, t$to, ds_text_array(t$from)))
    if (n == 0) {
      stop("Kan ikke utføre dette når status er «", ds_status_label[[current$status]], "».", call. = FALSE)
    }
    ds_log(con, id, actor, t$log)
  })
  invisible(t$to)
}

# Comments ----------------------------------------------------------------------

ds_add_comment <- function(con, access, proposal_id, actor, body) {
  ds_proposal_group(con, access, proposal_id)
  body <- trimws(body)
  if (!nzchar(body) || nchar(body) > 2000) stop("En kommentar må ha mellom 1 og 2000 tegn.", call. = FALSE)
  ds_query(con, "INSERT INTO proposal_comments (proposal_id, author, body) VALUES ($1, $2, $3) RETURNING id",
           list(as.integer(proposal_id), actor, body))$id
}

# Edit locks --------------------------------------------------------------------
# A lock says "this trainer has an editor open". It is advisory (others get a
# warning, not a block). The editor renews it every 30 s; a lock older than
# `max_age` seconds is treated as gone (e.g. the browser was closed).

ds_lock_max_age <- 120

#' Register that this editor session is editing (proposal_id NULL = new proposal)
#' @noRd
ds_acquire_lock <- function(con, access, group_id, editor, session_key,
                            proposal_id = NULL, subgroup_id = NULL, event_id = NULL) {
  assert_group_access(access, group_id)
  if (!is.null(proposal_id)) {
    g <- ds_proposal_group(con, access, proposal_id)
    if (!identical(g$spond_group_id, group_id)) stop("Forslaget hører til en annen gruppe.", call. = FALSE)
  }
  ds_transaction(con, {
    ds_exec(con, "DELETE FROM edit_locks WHERE session_key = $1", list(session_key))
    ds_exec(con, "INSERT INTO edit_locks (spond_group_id, spond_subgroup_id, spond_event_id, proposal_id, editor, session_key)
                  VALUES ($1, NULLIF($2, ''), NULLIF($3, ''), NULLIF($4, '')::integer, $5, $6)",
            list(group_id, ds_opt(subgroup_id), ds_opt(event_id), ds_opt(proposal_id), editor, session_key))
  })
  invisible(TRUE)
}

ds_heartbeat_lock <- function(con, session_key) {
  invisible(ds_exec(con, "UPDATE edit_locks SET heartbeat_at = now() WHERE session_key = $1", list(session_key)) > 0)
}

ds_release_lock <- function(con, session_key) {
  invisible(ds_exec(con, "DELETE FROM edit_locks WHERE session_key = $1", list(session_key)))
}

#' Active locks in a group (stale locks are cleaned up first)
#' @noRd
ds_active_locks <- function(con, access, group_id, max_age = ds_lock_max_age) {
  assert_group_access(access, group_id)
  ds_exec(con, "DELETE FROM edit_locks WHERE heartbeat_at < now() - make_interval(secs => $1)", list(as.numeric(max_age)))
  ds_query(con, "SELECT spond_subgroup_id, spond_event_id, proposal_id, editor, session_key, started_at
                   FROM edit_locks WHERE spond_group_id = $1 ORDER BY started_at", list(group_id))
}

# Database setup (admin only) -----------------------------------------------------
# These functions are used by dev/setup_db.R with the admin connection. The
# running app never calls them, and the app user is not allowed to.

# The tables the app reads and writes. schema_migrations is deliberately not
# included: only the admin runs migrations.
ds_app_tables <- c("member_tags", "group_proposals", "group_proposal_labels", "group_proposal_members",
                   "proposal_history", "proposal_comments", "edit_locks")

#' A random password of letters and digits (cryptographically secure)
#'
#' Letters and digits only, so it never needs escaping in a connection string.
#' @noRd
ds_random_password <- function(n = 32) {
  alphabet <- c(LETTERS, letters, 0:9)
  out <- character()
  while (length(out) < n) {
    b <- as.integer(openssl::rand_bytes(n * 2))
    b <- b[b < 248]                      # 248 = 4 * 62, avoids modulo bias
    out <- c(out, alphabet[(b %% 62) + 1])
  }
  paste(out[seq_len(n)], collapse = "")
}

#' Create (or update) the app's own database user with minimal rights
#'
#' The user can read and write rows in the app's tables and nothing else: no
#' schema changes, no access to schema_migrations, other schemas or Supabase's
#' own tables. Row Level Security stays on; a policy lets this user (and only
#' this user) through. The public Supabase roles (anon, authenticated) get no
#' rights. Safe to run again; run it after every new migration that adds tables.
#'
#' @param password Required when the user is created. When the user already
#'   exists, a new password replaces the old one; NULL keeps the old one.
#' @noRd
ds_setup_app_role <- function(con, role = "spondnymark_app", password = NULL) {
  if (!grepl("^[a-z_][a-z0-9_]{0,62}$", role)) stop("Ugyldig rollenavn: ", role, call. = FALSE)
  if (!is.null(password) && !grepl("^[A-Za-z0-9]{24,}$", password)) {
    stop("Passordet til app-brukeren må ha minst 24 bokstaver og tall.", call. = FALSE)
  }
  q_role <- DBI::dbQuoteIdentifier(con, role)
  exists <- nrow(ds_query(con, "SELECT 1 FROM pg_roles WHERE rolname = $1", list(role))) > 0
  schema <- ds_query(con, "SELECT current_schema() AS s")$s
  q_schema <- DBI::dbQuoteIdentifier(con, schema)
  public_roles <- ds_query(con, "SELECT rolname FROM pg_roles WHERE rolname IN ('anon', 'authenticated')")$rolname

  ds_transaction(con, {
    if (!exists) {
      if (is.null(password)) stop("Oppgi et passord når app-brukeren opprettes.", call. = FALSE)
      ds_exec(con, paste0("CREATE ROLE ", q_role, " LOGIN PASSWORD ", DBI::dbQuoteString(con, password)))
    } else if (!is.null(password)) {
      ds_exec(con, paste0("ALTER ROLE ", q_role, " WITH LOGIN PASSWORD ", DBI::dbQuoteString(con, password)))
    }
    ds_exec(con, paste0("GRANT USAGE ON SCHEMA ", q_schema, " TO ", q_role))
    policy <- DBI::dbQuoteIdentifier(con, paste0(role, "_access"))
    for (t in ds_app_tables) {
      q_t <- paste0(q_schema, ".", DBI::dbQuoteIdentifier(con, t))
      ds_exec(con, paste0("GRANT SELECT, INSERT, UPDATE, DELETE ON ", q_t, " TO ", q_role))
      ds_exec(con, paste0("ALTER TABLE ", q_t, " ENABLE ROW LEVEL SECURITY"))
      ds_exec(con, paste0("DROP POLICY IF EXISTS ", policy, " ON ", q_t))
      ds_exec(con, paste0("CREATE POLICY ", policy, " ON ", q_t, " FOR ALL TO ", q_role, " USING (true) WITH CHECK (true)"))
      for (pr in public_roles) ds_exec(con, paste0("REVOKE ALL ON ", q_t, " FROM ", DBI::dbQuoteIdentifier(con, pr)))
    }
  })
  invisible(!exists)
}

#' Put a password into a connection string copied from Supabase
#'
#' Replaces the `[YOUR-PASSWORD]` placeholder (or the existing password) and
#' percent-encodes it, so special characters are safe.
#' @noRd
ds_fill_password <- function(url, password) {
  if (is.null(password) || length(password) != 1 || is.na(password) || !nzchar(password)) {
    stop("Mangler passord.", call. = FALSE)
  }
  enc <- utils::URLencode(password, reserved = TRUE)
  if (grepl("[YOUR-PASSWORD]", url, fixed = TRUE)) return(sub("[YOUR-PASSWORD]", enc, url, fixed = TRUE))
  m <- regmatches(url, regexec("^(postgres(?:ql)?://[^:/@]+):[^@]*(@.*)$", url, perl = TRUE))[[1]]
  if (length(m) == 0) {
    m <- regmatches(url, regexec("^(postgres(?:ql)?://[^:/@]+)(@.*)$", url, perl = TRUE))[[1]]
  }
  if (length(m) == 0) stop("Skjønte ikke tilkoblingsstrengen. Kopier den fra Supabase: Connect -> Session pooler.", call. = FALSE)
  paste0(m[2], ":", enc, m[3])
}

#' The app user's connection string, derived from the admin connection string
#'
#' On Supabase's pooler the user name is `<role>.<project-ref>`; the admin is
#' `postgres.<project-ref>`. Adds sslmode=require so traffic is always encrypted.
#' @noRd
ds_app_url <- function(admin_url, role, password) {
  m <- regmatches(admin_url, regexec("^(postgres(?:ql)?://)([^:/@]+)(?::[^@]*)?@([^?]*)(\\?.*)?$", admin_url, perl = TRUE))[[1]]
  if (length(m) == 0) stop("Skjønte ikke tilkoblingsstrengen.", call. = FALSE)
  admin_user <- m[3]
  user <- if (grepl(".", admin_user, fixed = TRUE)) sub("^[^.]+", role, admin_user) else role
  query <- m[5]
  if (!grepl("sslmode=", query, fixed = TRUE)) {
    query <- if (nzchar(query)) paste0(query, "&sslmode=require") else "?sslmode=require"
  }
  paste0(m[2], user, ":", utils::URLencode(password, reserved = TRUE), "@", m[4], query)
}

#' Set KEY=value in a .Renviron file (replaces an existing line for KEY)
#' @noRd
ds_write_renviron <- function(key, value, path = ".Renviron") {
  lines <- if (file.exists(path)) readLines(path, warn = FALSE) else character()
  new <- paste0(key, "=", value)
  hit <- grep(paste0("^\\s*", key, "\\s*="), lines)
  if (length(hit)) {
    lines[hit[1]] <- new                 # keep position and the comments around it
    if (length(hit) > 1) lines <- lines[-hit[-1]]
  } else {
    lines <- c(lines, new)
  }
  writeLines(lines, path)
  invisible(path)
}
