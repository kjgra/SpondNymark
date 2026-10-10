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
#' `NULLIF($n, '')`, which behaves the same with every Postgres driver.
#'
#' Speed: with RPostgres, a parameterised statement costs three round trips
#' to the server (prepare, describe, execute). Supabase is in Ireland, so
#' every round trip counts. `ds_query()`/`ds_exec()` therefore put the
#' parameters into the SQL as safely quoted literals (`dbQuoteLiteral()`,
#' libpq's own escaping) and send it in one round trip. Writes are batched
#' (`unnest()`), so saving a proposal is a handful of statements.
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
  # RPostgres does not read connection strings (it passes `dbname` on as a
  # plain database name), so the URL is split into separate arguments.
  do.call(DBI::dbConnect, c(list(RPostgres::Postgres()), ds_connect_args(url)))
}

#' Split a postgresql:// connection string into dbConnect() arguments
#'
#' User name and password are percent-decoded. Query parameters such as
#' `sslmode=require` are passed on as libpq options.
#' @noRd
ds_connect_args <- function(url) {
  m <- regmatches(url, regexec(
    "^postgres(?:ql)?://([^:/@]+)(?::([^@]*))?@([^:/?]+)(?::([0-9]+))?(?:/([^?]*))?(?:\\?(.*))?$",
    url, perl = TRUE))[[1]]
  if (length(m) == 0) {
    stop("Skjønte ikke tilkoblingsstrengen. Den skal se slik ut: postgresql://bruker:passord@vert:5432/postgres", call. = FALSE)
  }
  args <- list(
    host = m[4],
    port = if (nzchar(m[5])) as.integer(m[5]) else 5432L,
    user = utils::URLdecode(m[2]),
    password = utils::URLdecode(m[3]),
    dbname = if (nzchar(m[6])) utils::URLdecode(m[6]) else "postgres"
  )
  if (nzchar(m[7])) {
    for (kv in strsplit(m[7], "&", fixed = TRUE)[[1]]) {
      key <- sub("=.*$", "", kv)
      if (nzchar(key)) args[[key]] <- utils::URLdecode(sub("^[^=]*=?", "", kv))
    }
  }
  args
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

# Replace $1, $2, ... with the parameters as quoted SQL literals.
ds_interpolate <- function(con, sql, params) {
  vals <- vapply(unname(params), function(v) {
    if (length(v) != 1) stop("Hver parameter må ha lengde 1.", call. = FALSE)
    as.character(DBI::dbQuoteLiteral(con, v))
  }, character(1))
  m <- gregexpr("\\$[0-9]+", sql)
  idx <- as.integer(substring(regmatches(sql, m)[[1]], 2))
  if (any(idx > length(vals))) stop("For få parametere til spørringen.", call. = FALSE)
  regmatches(sql, m) <- list(vals[idx])
  sql
}

# One round trip with RPostgres (the driver the app uses). Any other DBI
# driver gets ordinary parameter binding.
ds_fast <- function(con) inherits(con, "PqConnection")

ds_query <- function(con, sql, params = list()) {
  if (!length(params)) return(DBI::dbGetQuery(con, sql))
  if (ds_fast(con)) {
    res <- withCallingHandlers(
      DBI::dbGetQuery(con, ds_interpolate(con, sql, params), immediate = TRUE),
      warning = function(w) {
        if (grepl("Don't need to call dbFetch", conditionMessage(w), fixed = TRUE)) invokeRestart("muffleWarning")
      }
    )
    # RPostgres returns an empty result without columns in this mode; ask
    # again the ordinary way so callers always get the column names and types.
    if (ncol(res) > 0) return(res)
  }
  DBI::dbGetQuery(con, sql, params = unname(params))
}

ds_exec <- function(con, sql, params = list()) {
  if (!length(params)) return(DBI::dbExecute(con, sql))
  if (ds_fast(con)) return(DBI::dbExecute(con, ds_interpolate(con, sql, params), immediate = TRUE))
  DBI::dbExecute(con, sql, params = unname(params))
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
#' @param event_ids The Spond events the app knows about (the whole group's
#'   upcoming events, so approval works from any context). Proposals tied to
#'   these events are returned.
#' @param subgroup_id The selected subgroup, or NULL for the whole group.
#'   Gruppeutkast (no event) are returned only for their own context (made in
#'   the whole group: only there; made in a subgroup: only there), except
#'   those waiting for approval, which are returned everywhere for
#'   "Godkjenning".
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
            OR (p.spond_event_id IS NULL
                AND (p.spond_subgroup_id IS NOT DISTINCT FROM NULLIF($3, '') OR p.status = 'pending')))
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

#' Groups and members of several proposals in one group, in two queries
#'
#' Used to show the proposals in a list. Proposals from other groups are
#' never returned, even if their ids are asked for.
#' @return list(labels = data.frame(proposal_id, label, sort_order),
#'   members = data.frame(proposal_id, spond_member_id, label))
#' @noRd
ds_proposal_groups <- function(con, access, group_id, proposal_ids) {
  assert_group_access(access, group_id)
  ids <- ds_text_array(as.integer(proposal_ids))
  list(
    labels = ds_query(con, "
      SELECT l.proposal_id, l.label, l.sort_order
        FROM group_proposal_labels l JOIN group_proposals p ON p.id = l.proposal_id
       WHERE p.spond_group_id = $1 AND l.proposal_id = ANY($2::integer[])
       ORDER BY l.proposal_id, l.sort_order", list(group_id, ids)),
    members = ds_query(con, "
      SELECT m.proposal_id, m.spond_member_id, m.label
        FROM group_proposal_members m JOIN group_proposals p ON p.id = m.proposal_id
       WHERE p.spond_group_id = $1 AND m.proposal_id = ANY($2::integer[])
       ORDER BY m.proposal_id, m.label, m.spond_member_id", list(group_id, ids))
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

# Replace the groups of a proposal: three statements, however many groups
# and members (deleting the labels also deletes the members, by cascade).
ds_write_groups <- function(con, id, labels, assignments) {
  id <- as.integer(id)
  ds_exec(con, "DELETE FROM group_proposal_labels WHERE proposal_id = $1", list(id))
  ds_exec(con, "INSERT INTO group_proposal_labels (proposal_id, label, sort_order)
                SELECT $1, x.label, x.ord FROM unnest($2::text[]) WITH ORDINALITY AS x(label, ord)",
          list(id, ds_text_array(labels)))
  if (length(assignments)) {
    ds_exec(con, "INSERT INTO group_proposal_members (proposal_id, spond_member_id, label)
                  SELECT $1, x.member, x.label FROM unnest($2::text[], $3::text[]) AS x(member, label)",
            list(id, ds_text_array(names(assignments)), ds_text_array(unname(assignments))))
  }
}

# History entries for one proposal in one statement.
ds_log_many <- function(con, proposal_id, actor, decisions) {
  ds_exec(con, "INSERT INTO proposal_history (proposal_id, actor, decision)
                SELECT $1, $2, d FROM unnest($3::text[]) WITH ORDINALITY AS x(d, ord) ORDER BY ord",
          list(as.integer(proposal_id), actor, ds_text_array(decisions)))
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
  status <- if (isTRUE(submit)) "pending" else "draft"
  if (is.null(id)) {
    assert_group_access(access, group_id)
    # Event-bound proposals follow the event's visibility, so they carry no subgroup.
    sub <- if (is.null(event_id)) ds_opt(subgroup_id) else ""
    decisions <- c("created", if (isTRUE(submit)) "sent_for_approval")
    if (ds_fast(con)) return(ds_create_proposal_fast(con, group_id, sub, ds_opt(event_id), v, status, actor,
                                                     assignments, decisions))
    ds_transaction(con, {
      id <- ds_query(con, "INSERT INTO group_proposals (spond_group_id, spond_subgroup_id, spond_event_id, name, created_by, status)
                           VALUES ($1, NULLIF($2, ''), NULLIF($3, ''), $4, $5, $6) RETURNING id",
                     list(group_id, sub, ds_opt(event_id), v$name, actor, status))$id
      ds_write_groups(con, id, v$labels, assignments)
      ds_log_many(con, id, actor, decisions)
    })
    return(as.integer(id))
  }

  current <- ds_proposal_group(con, access, id)
  if (!identical(current$spond_group_id, group_id)) stop("Forslaget hører til en annen gruppe.", call. = FALSE)
  id <- as.integer(id)
  not_editable <- function() stop("Forslaget kan ikke redigeres når status er «", current$status, "».", call. = FALSE)
  decisions <- c("edited", if (isTRUE(submit)) "sent_for_approval")
  if (ds_fast(con)) {
    if (!ds_update_proposal_fast(con, id, v, status, actor, assignments, decisions)) not_editable()
    return(id)
  }
  ds_transaction(con, {
    n <- ds_exec(con, "UPDATE group_proposals SET name = $2, status = $3, updated_at = now()
                        WHERE id = $1 AND status IN ('draft', 'rolled_back', 'rejected')",
                 list(id, v$name, status))
    if (n == 0) not_editable()
    ds_write_groups(con, id, v$labels, assignments)
    ds_log_many(con, id, actor, decisions)
  })
  id
}

# New proposal in ONE statement (one round trip): the proposal, its groups,
# members and history are inserted together with data-modifying CTEs. It is
# atomic on its own, and the foreign keys are checked at the end of it.
ds_create_proposal_fast <- function(con, group_id, sub, event_id, v, status, actor, assignments, decisions) {
  res <- ds_query(con, "
    WITH p AS (
      INSERT INTO group_proposals (spond_group_id, spond_subgroup_id, spond_event_id, name, created_by, status)
      VALUES ($1, NULLIF($2, ''), NULLIF($3, ''), $4, $5, $6) RETURNING id),
    l AS (
      INSERT INTO group_proposal_labels (proposal_id, label, sort_order)
      SELECT p.id, x.label, x.ord FROM p, unnest($7::text[]) WITH ORDINALITY AS x(label, ord)),
    m AS (
      INSERT INTO group_proposal_members (proposal_id, spond_member_id, label)
      SELECT p.id, x.member, x.label FROM p, unnest($8::text[], $9::text[]) AS x(member, label)),
    h AS (
      INSERT INTO proposal_history (proposal_id, actor, decision)
      SELECT p.id, $5, x.d FROM p, unnest($10::text[]) WITH ORDINALITY AS x(d, ord) ORDER BY x.ord)
    SELECT id FROM p",
    list(group_id, sub, event_id, v$name, actor, status, ds_text_array(v$labels),
         ds_text_array(names(assignments)), ds_text_array(unname(assignments)), ds_text_array(decisions)))
  as.integer(res$id)
}

# Edit in ONE round trip: several statements sent together, which Postgres
# runs as one transaction. The UPDATE only succeeds from an editable status;
# it sets updated_at = now() (the transaction's start time), and the later
# statements only run when that mark is there. The final SELECT tells
# whether the edit happened.
ds_update_proposal_fast <- function(con, id, v, status, actor, assignments, decisions) {
  done <- "EXISTS (SELECT 1 FROM group_proposals WHERE id = $1 AND updated_at = now())"
  sql <- paste0("
    UPDATE group_proposals SET name = $2, status = $3, updated_at = now()
     WHERE id = $1 AND status IN ('draft', 'rolled_back', 'rejected');
    DELETE FROM group_proposal_labels WHERE proposal_id = $1 AND ", done, ";
    INSERT INTO group_proposal_labels (proposal_id, label, sort_order)
    SELECT $1, x.label, x.ord FROM unnest($4::text[]) WITH ORDINALITY AS x(label, ord) WHERE ", done, ";
    INSERT INTO group_proposal_members (proposal_id, spond_member_id, label)
    SELECT $1, x.member, x.label FROM unnest($5::text[], $6::text[]) AS x(member, label) WHERE ", done, ";
    INSERT INTO proposal_history (proposal_id, actor, decision)
    SELECT $1, $7, x.d FROM unnest($8::text[]) WITH ORDINALITY AS x(d, ord) WHERE ", done, " ORDER BY x.ord;
    SELECT count(*)::integer AS n FROM group_proposals WHERE id = $1 AND updated_at = now()")
  res <- ds_query(con, sql, list(id, v$name, status, ds_text_array(v$labels), ds_text_array(names(assignments)),
                                 ds_text_array(unname(assignments)), actor, ds_text_array(decisions)))
  isTRUE(res$n[1] > 0)
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
#'
#' Approving a proposal for an event rolls back any other approved proposal
#' for the same event, so an event has at most one approved set of groups.
#' Everything happens in one statement (one round trip).
#' @return The new status (invisibly).
#' @noRd
ds_transition <- function(con, access, proposal_id, action, actor) {
  t <- ds_transitions[[action]]
  if (is.null(t)) stop("Ukjent handling: ", action, call. = FALSE)
  current <- ds_proposal_group(con, access, proposal_id)
  res <- ds_query(con, "
    WITH t AS (
      UPDATE group_proposals SET status = $2, updated_at = now()
       WHERE id = $1 AND status = ANY($3::text[])
      RETURNING id, spond_event_id),
    h AS (
      INSERT INTO proposal_history (proposal_id, actor, decision) SELECT id, $4, $5 FROM t),
    o AS (
      UPDATE group_proposals g SET status = 'rolled_back', updated_at = now()
        FROM t
       WHERE $6 AND g.spond_event_id = t.spond_event_id AND g.id <> t.id AND g.status = 'approved'
      RETURNING g.id),
    oh AS (
      INSERT INTO proposal_history (proposal_id, actor, decision) SELECT id, $4, 'rolled_back' FROM o)
    SELECT (SELECT count(*) FROM t)::integer AS n, (SELECT count(*) FROM o)::integer AS replaced",
    list(as.integer(proposal_id), t$to, ds_text_array(t$from), actor, t$log, identical(action, "approve")))
  if (res$n[1] == 0) {
    stop("Kan ikke utføre dette når status er «", ds_status_label[[current$status]], "».", call. = FALSE)
  }
  invisible(t$to)
}

#' Comments and history of several proposals, in one query
#'
#' @return data.frame: proposal_id, kind ("comment" or "history"), actor,
#'   text (comment body or history decision), created_at; oldest first.
#' @noRd
ds_proposal_threads <- function(con, access, group_id, proposal_ids) {
  assert_group_access(access, group_id)
  ds_query(con, "
    SELECT x.proposal_id, x.kind, x.actor, x.text, x.created_at FROM (
      SELECT c.proposal_id, 'comment'::text AS kind, c.author AS actor, c.body AS text, c.created_at, c.id
        FROM proposal_comments c
      UNION ALL
      SELECT h.proposal_id, 'history'::text, h.actor, h.decision, h.created_at, h.id
        FROM proposal_history h
    ) x JOIN group_proposals p ON p.id = x.proposal_id
     WHERE p.spond_group_id = $1 AND x.proposal_id = ANY($2::integer[])
     ORDER BY x.created_at, x.kind, x.id", list(group_id, ds_text_array(as.integer(proposal_ids))))
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
  # One statement: replaces this session's lock (session_key is unique).
  ds_exec(con, "INSERT INTO edit_locks (spond_group_id, spond_subgroup_id, spond_event_id, proposal_id, editor, session_key)
                VALUES ($1, NULLIF($2, ''), NULLIF($3, ''), NULLIF($4, '')::integer, $5, $6)
                ON CONFLICT (session_key) DO UPDATE
                  SET spond_group_id = EXCLUDED.spond_group_id, spond_subgroup_id = EXCLUDED.spond_subgroup_id,
                      spond_event_id = EXCLUDED.spond_event_id, proposal_id = EXCLUDED.proposal_id,
                      editor = EXCLUDED.editor, started_at = now(), heartbeat_at = now()",
          list(group_id, ds_opt(subgroup_id), ds_opt(event_id), ds_opt(proposal_id), editor, session_key))
  # Clean up locks from closed browsers now and then (here, not on every read).
  ds_exec(con, "DELETE FROM edit_locks WHERE heartbeat_at < now() - make_interval(secs => $1)", list(ds_lock_max_age))
  invisible(TRUE)
}

#' Renew this session's lock. FALSE if it is gone or has expired (then the
#' caller acquires it again).
#' @noRd
ds_heartbeat_lock <- function(con, session_key, max_age = ds_lock_max_age) {
  invisible(ds_exec(con, "UPDATE edit_locks SET heartbeat_at = now()
                           WHERE session_key = $1 AND heartbeat_at >= now() - make_interval(secs => $2)",
                    list(session_key, as.numeric(max_age))) > 0)
}

ds_release_lock <- function(con, session_key) {
  invisible(ds_exec(con, "DELETE FROM edit_locks WHERE session_key = $1", list(session_key)))
}

#' Active locks in a group (locks without a heartbeat for `max_age` seconds are ignored)
#' @noRd
ds_active_locks <- function(con, access, group_id, max_age = ds_lock_max_age) {
  assert_group_access(access, group_id)
  ds_query(con, "SELECT spond_subgroup_id, spond_event_id, proposal_id, editor, session_key, started_at
                   FROM edit_locks
                  WHERE spond_group_id = $1 AND heartbeat_at >= now() - make_interval(secs => $2)
                  ORDER BY started_at", list(group_id, as.numeric(max_age)))
}

# Season plan (årshjul) ---------------------------------------------------------
# One theme per month and year for a main group (migration 002).

#' The season plan for one year
#' @return data.frame(month, theme, description, updated_by, updated_at),
#'   only months with a theme, in month order. description is "" if empty.
#' @noRd
ds_list_season_themes <- function(con, access, group_id, year) {
  assert_group_access(access, group_id)
  ds_query(con, "SELECT month, theme, coalesce(description, '') AS description, updated_by, updated_at
                   FROM season_themes WHERE spond_group_id = $1 AND year = $2 ORDER BY month",
           list(group_id, as.integer(year)))
}

#' Replace the season plan for one year
#'
#' Months with an empty theme are removed. One statement (one round trip).
#' @param themes,descriptions Character vectors for January to December.
#' @noRd
ds_save_season <- function(con, access, group_id, year, themes, descriptions = rep("", 12), actor) {
  assert_group_access(access, group_id)
  v <- season_validate(year, themes, descriptions)
  ds_exec(con, "
    WITH d AS (
      DELETE FROM season_themes
       WHERE spond_group_id = $1 AND year = $2 AND NOT (month = ANY($3::integer[])))
    INSERT INTO season_themes (spond_group_id, year, month, theme, description, updated_by)
    SELECT $1, $2, x.m, x.t, NULLIF(x.d, ''), $6
      FROM unnest($3::integer[], $4::text[], $5::text[]) AS x(m, t, d)
    ON CONFLICT (spond_group_id, year, month) DO UPDATE
      SET theme = EXCLUDED.theme, description = EXCLUDED.description,
          updated_by = EXCLUDED.updated_by, updated_at = now()
      WHERE season_themes.theme IS DISTINCT FROM EXCLUDED.theme
         OR season_themes.description IS DISTINCT FROM EXCLUDED.description",
    list(group_id, as.integer(year), ds_text_array(v$month), ds_text_array(v$theme),
         ds_text_array(v$description), actor))
  invisible(nrow(v))
}

#' The theme for the month of `date`, or NULL if there is none
#' @return list(theme, description) or NULL.
#' @noRd
ds_season_theme <- function(con, access, group_id, date) {
  date <- as.Date(date)
  rows <- ds_list_season_themes(con, access, group_id, as.integer(format(date, "%Y")))
  hit <- rows[rows$month == as.integer(format(date, "%m")), , drop = FALSE]
  if (nrow(hit) == 0) return(NULL)
  list(theme = hit$theme[1], description = hit$description[1])
}

# Team settings -----------------------------------------------------------------

ds_team_settings_empty <- list(age_group = "", session_minutes = "", pitch = "", equipment = "", principles = "",
                               updated_by = "", updated_at = as.POSIXct(NA))

#' The team settings for a main group
#' @return list with age_group, session_minutes, pitch, equipment and
#'   principles as strings ("" if not set), plus updated_by and updated_at.
#' @noRd
ds_get_team_settings <- function(con, access, group_id) {
  assert_group_access(access, group_id)
  row <- ds_query(con, "
    SELECT coalesce(age_group, '') AS age_group, coalesce(session_minutes::text, '') AS session_minutes,
           coalesce(pitch, '') AS pitch, coalesce(equipment, '') AS equipment,
           coalesce(principles, '') AS principles, updated_by, updated_at
      FROM team_settings WHERE spond_group_id = $1", list(group_id))
  if (nrow(row) == 0) return(ds_team_settings_empty)
  as.list(row[1, , drop = FALSE])
}

ds_save_team_settings <- function(con, access, group_id, settings, actor) {
  assert_group_access(access, group_id)
  s <- team_settings_validate(settings)
  ds_exec(con, "
    INSERT INTO team_settings (spond_group_id, age_group, session_minutes, pitch, equipment, principles, updated_by)
    VALUES ($1, NULLIF($2, ''), NULLIF($3, '')::integer, NULLIF($4, ''), NULLIF($5, ''), NULLIF($6, ''), $7)
    ON CONFLICT (spond_group_id) DO UPDATE
      SET age_group = EXCLUDED.age_group, session_minutes = EXCLUDED.session_minutes, pitch = EXCLUDED.pitch,
          equipment = EXCLUDED.equipment, principles = EXCLUDED.principles,
          updated_by = EXCLUDED.updated_by, updated_at = now()",
    list(group_id, s$age_group, s$session_minutes, s$pitch, s$equipment, s$principles, actor))
  invisible(TRUE)
}

# Exercises ---------------------------------------------------------------------

# Columns returned for exercises. Empty text is returned as "" and missing
# numbers as NA. Themes are joined with the unit separator (never typed) and
# split again in ds_exercise_rows().
ds_exercise_select <- "
  SELECT id, code, name, category, array_to_string(themes, chr(31)) AS themes,
         min_players, max_players, duration_minutes,
         coalesce(area, '') AS area, coalesce(organisation, '') AS organisation,
         coalesce(execution, '') AS execution, coalesce(learning_points, '') AS learning_points,
         coalesce(questions, '') AS questions, coalesce(easier, '') AS easier,
         coalesce(harder, '') AS harder, coalesce(nff_url, '') AS nff_url,
         drawing::text AS drawing, source, coalesce(based_on, '') AS based_on, status,
         created_by, created_at, updated_by, updated_at
    FROM exercises"

ds_exercise_rows <- function(rows) {
  rows$themes <- lapply(rows$themes, function(x) {
    if (is.na(x) || !nzchar(x)) character() else strsplit(x, "\x1f", fixed = TRUE)[[1]]
  })
  rows
}

# The group an exercise belongs to, after checking the user may see it.
ds_exercise_group <- function(con, access, exercise_id) {
  row <- ds_query(con, "SELECT spond_group_id FROM exercises WHERE id = $1", list(as.integer(exercise_id)))
  if (nrow(row) == 0) stop("Fant ikke øvelsen.", call. = FALSE)
  assert_group_access(access, row$spond_group_id)
  row$spond_group_id
}

#' All exercises in a main group, sorted by category and name
#' @return data.frame; `themes` is a list column of character vectors.
#' @noRd
ds_list_exercises <- function(con, access, group_id) {
  assert_group_access(access, group_id)
  rows <- ds_query(con, paste(ds_exercise_select, "WHERE spond_group_id = $1 ORDER BY category, lower(name), id"),
                   list(group_id))
  ds_exercise_rows(rows)
}

ds_get_exercise <- function(con, access, exercise_id) {
  ds_exercise_group(con, access, exercise_id)
  ds_exercise_rows(ds_query(con, paste(ds_exercise_select, "WHERE id = $1"), list(as.integer(exercise_id))))
}

#' Create or update an exercise
#'
#' The code is made from the name when the exercise is created and is not
#' changed afterwards (evaluations are collected per code).
#' @param ex List of fields, see `exercise_validate()`.
#' @param id NULL to create, or the id of the exercise to update.
#' @return The exercise id.
#' @noRd
ds_save_exercise <- function(con, access, group_id, ex, actor, id = NULL) {
  assert_group_access(access, group_id)
  v <- exercise_validate(ex)
  fields <- list(v$name, v$category, ds_text_array(v$themes), v$min_players, v$max_players, v$duration_minutes,
                 v$area, v$organisation, v$execution, v$learning_points, v$questions, v$easier, v$harder,
                 v$nff_url, actor, v$drawing)
  if (is.null(id)) {
    taken <- ds_query(con, "SELECT code FROM exercises WHERE spond_group_id = $1", list(group_id))$code
    code <- exercise_code(v$name, taken)
    res <- ds_query(con, "
      INSERT INTO exercises (spond_group_id, code, name, category, themes, min_players, max_players,
                             duration_minutes, area, organisation, execution, learning_points, questions,
                             easier, harder, nff_url, created_by, updated_by, drawing)
      VALUES ($1, $2, $3, $4, $5::text[], NULLIF($6, '')::integer, NULLIF($7, '')::integer,
              NULLIF($8, '')::integer, NULLIF($9, ''), NULLIF($10, ''), NULLIF($11, ''), NULLIF($12, ''),
              NULLIF($13, ''), NULLIF($14, ''), NULLIF($15, ''), NULLIF($16, ''), $17, $17,
              NULLIF($18, '')::jsonb)
      RETURNING id", c(list(group_id, code), fields))
    return(as.integer(res$id))
  }
  if (!identical(ds_exercise_group(con, access, id), group_id)) {
    stop("Øvelsen hører til en annen gruppe.", call. = FALSE)
  }
  ds_exec(con, "
    UPDATE exercises
       SET name = $3, category = $4, themes = $5::text[], min_players = NULLIF($6, '')::integer,
           max_players = NULLIF($7, '')::integer, duration_minutes = NULLIF($8, '')::integer,
           area = NULLIF($9, ''), organisation = NULLIF($10, ''), execution = NULLIF($11, ''),
           learning_points = NULLIF($12, ''), questions = NULLIF($13, ''), easier = NULLIF($14, ''),
           harder = NULLIF($15, ''), nff_url = NULLIF($16, ''), drawing = NULLIF($18, '')::jsonb,
           updated_by = $17, updated_at = now()
     WHERE id = $1 AND spond_group_id = $2", c(list(as.integer(id), group_id), fields))
  as.integer(id)
}

ds_delete_exercise <- function(con, access, exercise_id) {
  group_id <- ds_exercise_group(con, access, exercise_id)
  # Variants of the deleted exercise become exercises of their own.
  ds_exec(con, "
    WITH gone AS (DELETE FROM exercises WHERE id = $1 AND spond_group_id = $2 RETURNING code)
    UPDATE exercises SET based_on = NULL
     WHERE spond_group_id = $2 AND based_on IN (SELECT code FROM gone)",
    list(as.integer(exercise_id), group_id))
  invisible(TRUE)
}

# Training plans (opplegg) -------------------------------------------------------
# One row per version of the plan for an event (migration 003). A new version
# is a new row; old versions are kept (cleaning comes in T4).

#' The latest version of the plan for each of these events
#' @return data.frame(spond_event_id, id, version, n_versions, title,
#'   created_by, created_at).
#' @noRd
ds_list_plans <- function(con, access, group_id, event_ids) {
  assert_group_access(access, group_id)
  ds_query(con, "
    SELECT DISTINCT ON (spond_event_id) spond_event_id, id, version,
           count(*) OVER (PARTITION BY spond_event_id)::integer AS n_versions,
           plan->>'tittel' AS title, created_by, created_at
      FROM training_plans
     WHERE spond_group_id = $1 AND spond_event_id = ANY($2::text[]) AND status <> 'deleted'
     ORDER BY spond_event_id, version DESC",
    list(group_id, ds_text_array(event_ids)))
}

#' All versions of the plan for one event, newest first
#' @noRd
ds_plan_versions <- function(con, access, group_id, event_id) {
  assert_group_access(access, group_id)
  ds_query(con, "
    SELECT id, version, status, source, plan->>'tittel' AS title, created_by, created_at
      FROM training_plans
     WHERE spond_group_id = $1 AND spond_event_id = $2 AND status <> 'deleted'
     ORDER BY version DESC",
    list(group_id, event_id))
}

#' One version of a plan, with the plan parsed and checked
#' @return list(id, group_id, event_id, version, status, source, model, cost_usd, created_by,
#'   created_at, plan).
#' @noRd
ds_get_plan <- function(con, access, plan_id) {
  row <- ds_query(con, "
    SELECT id, spond_group_id, spond_event_id, version, status, source, coalesce(model, '') AS model,
           cost_usd::float8 AS cost_usd, created_by, created_at, plan::text AS plan
      FROM training_plans WHERE id = $1 AND status <> 'deleted'", list(as.integer(plan_id)))
  if (nrow(row) == 0) stop("Fant ikke opplegget.", call. = FALSE)
  assert_group_access(access, row$spond_group_id)
  list(id = row$id, group_id = row$spond_group_id, event_id = row$spond_event_id, version = row$version,
       status = row$status, source = row$source, model = row$model, cost_usd = row$cost_usd,
       created_by = row$created_by, created_at = row$created_at, plan = plan_validate(row$plan))
}

#' Save a plan as a new version for an event
#'
#' The version number is the next free one, chosen in the same statement.
#' If two trainers save at the same moment, the unique key stops one of
#' them; that save is tried once more.
#' @return list(id, version).
#' @noRd
ds_save_plan <- function(con, access, group_id, event_id, plan, actor, source = "manual",
                         model = "", cost_usd = NA) {
  assert_group_access(access, group_id)
  if (!nzchar(txt1(event_id))) stop("Mangler arrangement.", call. = FALSE)
  js <- plan_json(plan)
  cost <- if (is.na(cost_usd)) "" else format(round(cost_usd, 5), scientific = FALSE)
  insert <- function() {
    ds_query(con, "
      INSERT INTO training_plans (spond_group_id, spond_event_id, version, plan, source, created_by, model, cost_usd)
      SELECT $1, $2, coalesce(max(version), 0) + 1, $3::jsonb, $4, $5, NULLIF($6, ''), NULLIF($7, '')::numeric
        FROM training_plans WHERE spond_group_id = $1 AND spond_event_id = $2
      RETURNING id, version",
      list(group_id, event_id, js, source, actor, txt1(model), cost))
  }
  res <- tryCatch(insert(), error = function(e) {
    if (!grepl("training_plans_spond_group_id_spond_event_id_version_key|duplicate key", conditionMessage(e))) stop(e)
    insert()
  })
  list(id = as.integer(res$id), version = as.integer(res$version))
}

#' Put one exercise from a plan into the bank («Lagre i banken»)
#'
#' A new exercise gets the plan's code, so evaluations are collected on the
#' same exercise. If the bank already has that code, nothing is changed.
#' `basert_pa` in the exercise makes it a variant; a variant of a variant
#' points to the base exercise instead, so families stay flat (kap. 15.4).
#' @param status "active", or "candidate" for exercises from KI.
#' @return list(id, created) where created is FALSE when it was there already.
#' @noRd
ds_save_plan_exercise <- function(con, access, group_id, exercise, actor, themes = character(), source = "manual",
                                  status = "active") {
  assert_group_access(access, group_id)
  if (!status %in% c("active", "candidate")) stop("Ugyldig status.", call. = FALSE)
  code <- txt1(exercise$kode)
  base <- txt1(exercise$basert_pa)
  known <- ds_query(con, "SELECT id, code, coalesce(based_on, '') AS based_on FROM exercises
                           WHERE spond_group_id = $1 AND code IN ($2, $3)", list(group_id, code, base))
  old <- known[known$code == code, , drop = FALSE]
  if (nrow(old)) return(list(id = as.integer(old$id[1]), created = FALSE))
  b <- known[known$code == base, , drop = FALSE]
  based_on <- if (!nzchar(base) || nrow(b) == 0) "" else if (nzchar(b$based_on[1])) b$based_on[1] else base
  v <- exercise_validate(bank_exercise_from_plan(exercise, themes))
  if (!grepl("^[a-z0-9]+(-[a-z0-9]+)*$", code) || nchar(code) > 60) code <- exercise_code(v$name)
  res <- ds_query(con, "
    INSERT INTO exercises (spond_group_id, code, name, category, themes, organisation, execution, learning_points,
                           questions, easier, harder, nff_url, drawing, source, created_by, updated_by,
                           based_on, status)
    VALUES ($1, $2, $3, $4, $5::text[], NULLIF($6, ''), NULLIF($7, ''), NULLIF($8, ''), NULLIF($9, ''),
            NULLIF($10, ''), NULLIF($11, ''), NULLIF($12, ''), NULLIF($13, '')::jsonb, $15, $14, $14,
            NULLIF($16, ''), $17)
    RETURNING id",
    list(group_id, code, v$name, v$category, ds_text_array(v$themes), v$organisation, v$execution,
         v$learning_points, v$questions, v$easier, v$harder, v$nff_url, v$drawing, actor, source,
         based_on, status))
  list(id = as.integer(res$id), created = TRUE)
}

# KI (migration 005) -----------------------------------------------------------------
# ai_usage logs tokens and cost of every call, never prompt or answer text.

#' The KI settings, with the defaults from `ai_settings_default()` where no
#' row has been saved
#' @noRd
ds_get_ai_settings <- function(con) {
  row <- ds_query(con, "
    SELECT model, group_limit_nok::float8 AS group_limit_nok, total_limit_usd::float8 AS total_limit_usd,
           max_new_exercises, usd_nok::float8 AS usd_nok, updated_by, updated_at
      FROM ai_settings WHERE id")
  out <- ai_settings_default()
  if (nrow(row)) {
    for (f in names(out)) out[[f]] <- row[[f]][1]
    out$max_new_exercises <- as.integer(out$max_new_exercises)
  }
  out
}

#' Save the KI settings (superadmin only, kap. 12.1)
#' @noRd
ds_save_ai_settings <- function(con, rights, actor, s) {
  if (!isTRUE(rights$superadmin)) stop("Bare superadmin kan endre KI-innstillingene.", call. = FALSE)
  v <- ai_settings_validate(s)
  ds_exec(con, "
    INSERT INTO ai_settings (id, model, group_limit_nok, total_limit_usd, max_new_exercises, usd_nok, updated_by)
    VALUES (true, $1, $2, $3, $4, $5, $6)
    ON CONFLICT (id) DO UPDATE SET model = EXCLUDED.model, group_limit_nok = EXCLUDED.group_limit_nok,
           total_limit_usd = EXCLUDED.total_limit_usd, max_new_exercises = EXCLUDED.max_new_exercises,
           usd_nok = EXCLUDED.usd_nok, updated_by = EXCLUDED.updated_by, updated_at = now()",
    list(v$model, v$group_limit_nok, v$total_limit_usd, v$max_new_exercises, v$usd_nok, actor))
  invisible(TRUE)
}

#' Log one KI call
#' @param u list(kind, model, input_tokens, output_tokens, cache_read_tokens,
#'   cache_write_tokens, cost_usd, duration_ms, status, error_code), see
#'   `ai_usage_row()`.
#' @return The id of the row.
#' @noRd
ds_log_ai_usage <- function(con, access, group_id, actor, u, event_id = "") {
  assert_group_access(access, group_id)
  int <- function(x) as.character(as.integer(round(x %||% 0)))
  res <- ds_query(con, "
    INSERT INTO ai_usage (spond_profile_id, spond_group_id, spond_event_id, kind, model, input_tokens,
                          output_tokens, cache_read_tokens, cache_write_tokens, cost_usd, duration_ms,
                          status, error_code)
    VALUES ($1, $2, NULLIF($3, ''), $4, $5, $6::integer, $7::integer, $8::integer, $9::integer,
            $10::numeric, NULLIF($11, '')::integer, $12, NULLIF($13, ''))
    RETURNING id",
    list(actor, group_id, txt1(event_id), u$kind, u$model, int(u$input_tokens), int(u$output_tokens),
         int(u$cache_read_tokens), int(u$cache_write_tokens),
         format(round(u$cost_usd %||% 0, 5), scientific = FALSE),
         if (is.null(u$duration_ms) || is.na(u$duration_ms)) "" else int(u$duration_ms),
         u$status, txt1(u$error_code)))
  as.integer(res$id)
}

#' Link a logged call to the plan version it made
#' @noRd
ds_ai_usage_set_plan <- function(con, access, group_id, usage_id, plan_id) {
  assert_group_access(access, group_id)
  ds_exec(con, "UPDATE ai_usage SET plan_id = $3 WHERE id = $1 AND spond_group_id = $2",
          list(as.integer(usage_id), group_id, as.integer(plan_id)))
  invisible(TRUE)
}

#' What has been spent since `since` (start of the month), for the group and
#' for the whole app. The total is a sum only; no other group's rows are read.
#' @return list(group_usd, total_usd).
#' @noRd
ds_ai_spend <- function(con, access, group_id, since) {
  assert_group_access(access, group_id)
  row <- ds_query(con, "
    SELECT coalesce(sum(cost_usd) FILTER (WHERE spond_group_id = $1), 0)::float8 AS group_usd,
           coalesce(sum(cost_usd), 0)::float8 AS total_usd
      FROM ai_usage WHERE created_at >= $2::timestamptz",
    list(group_id, format(since, "%Y-%m-%d %H:%M:%S%z")))
  list(group_usd = row$group_usd[1], total_usd = row$total_usd[1])
}

#' Typical answer length for a kind of call and model: the median of the last
#' 20 successful calls, or NA if there are none yet (for the price estimate)
#' @noRd
ds_ai_typical_output <- function(con, kind, model) {
  row <- ds_query(con, "
    SELECT percentile_cont(0.5) WITHIN GROUP (ORDER BY output_tokens)::float8 AS n
      FROM (SELECT output_tokens FROM ai_usage WHERE kind = $1 AND model = $2 AND status = 'ok' AND output_tokens > 0
             ORDER BY created_at DESC LIMIT 20) t",
    list(kind, model))
  row$n[1]
}

# Comments on plans (migration 007) -----------------------------------------------

#' All comments on the plans for one event, oldest first
#' @return data.frame(id, plan_id, version, exercise_no, exercise_code, author,
#'   body, created_at, used_in_plan_id, used_in_version).
#' @noRd
ds_list_plan_comments <- function(con, access, group_id, event_id) {
  assert_group_access(access, group_id)
  ds_query(con, "
    SELECT c.id, c.plan_id, p.version, c.exercise_no, coalesce(c.exercise_code, '') AS exercise_code, c.author,
           c.body, c.created_at, c.used_in_plan_id, u.version AS used_in_version
      FROM training_plan_comments c
      JOIN training_plans p ON p.id = c.plan_id
      LEFT JOIN training_plans u ON u.id = c.used_in_plan_id
     WHERE c.spond_group_id = $1 AND c.spond_event_id = $2 AND p.status <> 'deleted'
     ORDER BY c.created_at, c.id",
    list(group_id, event_id))
}

#' Comment on a plan version: the whole session (exercise_no NA) or one exercise
#' @return The id of the comment.
#' @noRd
ds_add_plan_comment <- function(con, access, plan_id, actor, body, exercise_no = NA) {
  pl <- ds_get_plan(con, access, plan_id)
  problem <- comment_problem(body)
  if (!is.null(problem)) stop(problem, call. = FALSE)
  body <- trimws(body)
  if (nchar(body) > 1000) stop("En kommentar kan ha maks 1000 tegn.", call. = FALSE)
  code <- ""
  no <- suppressWarnings(as.integer(exercise_no))
  if (length(no) == 1 && !is.na(no)) {
    if (no < 1 || no > length(pl$plan$ovelser)) stop("Ukjent øvelse.", call. = FALSE)
    code <- pl$plan$ovelser[[no]]$kode
  }
  ds_query(con, "
    INSERT INTO training_plan_comments (spond_group_id, spond_event_id, plan_id, exercise_no, exercise_code, author, body)
    VALUES ($1, $2, $3, NULLIF($4, '')::integer, NULLIF($5, ''), $6, $7) RETURNING id",
    list(pl$group_id, pl$event_id, as.integer(plan_id), if (length(no) == 1 && !is.na(no)) as.character(no) else "",
         code, actor, body))$id
}

#' Delete one's own comment, as long as KI has not used it
#' @noRd
ds_delete_plan_comment <- function(con, access, group_id, comment_id, actor) {
  assert_group_access(access, group_id)
  n <- ds_exec(con, "
    DELETE FROM training_plan_comments
     WHERE id = $1 AND spond_group_id = $2 AND author = $3 AND used_in_plan_id IS NULL",
    list(as.integer(comment_id), group_id, actor))
  if (n == 0) stop("Kan bare slette egne kommentarer som ikke er brukt av KI.", call. = FALSE)
  invisible(TRUE)
}

#' Note which version KI made with these comments
#' @noRd
ds_mark_comments_used <- function(con, access, group_id, comment_ids, plan_id) {
  assert_group_access(access, group_id)
  if (!length(comment_ids)) return(invisible(0))
  ds_exec(con, "
    UPDATE training_plan_comments SET used_in_plan_id = $3
     WHERE spond_group_id = $1 AND id = ANY($2::integer[]) AND used_in_plan_id IS NULL",
    list(group_id, paste0("{", paste(as.integer(comment_ids), collapse = ","), "}"), as.integer(plan_id)))
}

# Rights (app_roles, migration 004) ------------------------------------------------

#' The extra rights of one profile (0 or 1 row)
#' @noRd
ds_get_role <- function(con, profile_id) {
  ds_query(con, "SELECT spond_profile_id, can_use_ai, is_admin FROM app_roles WHERE spond_profile_id = $1",
           list(as.character(profile_id)))
}

#' The extra rights of several profiles, with who changed them last
#' @noRd
ds_list_roles <- function(con, profile_ids) {
  ds_query(con, "SELECT spond_profile_id, can_use_ai, is_admin, updated_by, updated_at
                   FROM app_roles WHERE spond_profile_id = ANY($1::text[])",
           list(ds_text_array(profile_ids)))
}

#' Give or take one right ("ai" or "admin") and log it
#'
#' @param rights The acting user's rights (`user_rights()`): admins may
#'   change KI, only the superadmin may change admin.
#' @param allowed_ids Profiles the actor may change (the trainers in the
#'   chosen main group).
#' @noRd
ds_set_role <- function(con, rights, actor, profile_id, role, granted, allowed_ids) {
  profile_id <- as.character(profile_id)
  if (!role %in% c("ai", "admin")) stop("Ukjent rettighet.", call. = FALSE)
  if (!isTRUE(rights$admin)) stop("Du har ikke tilgang til \u00e5 endre rettigheter.", call. = FALSE)
  if (role == "admin" && !isTRUE(rights$superadmin)) stop("Bare superadmin kan gi og ta admin.", call. = FALSE)
  if (!profile_id %in% allowed_ids) stop("Treneren er ikke med i dette laget.", call. = FALSE)
  if (!is.logical(granted) || length(granted) != 1 || is.na(granted)) stop("Ugyldig verdi.", call. = FALSE)
  col <- if (role == "ai") "can_use_ai" else "is_admin"
  ds_transaction(con, {
    ds_exec(con, paste0("
      INSERT INTO app_roles (spond_profile_id, ", col, ", updated_by) VALUES ($1, $2, $3)
      ON CONFLICT (spond_profile_id) DO UPDATE SET ", col, " = EXCLUDED.", col, ",
             updated_by = EXCLUDED.updated_by, updated_at = now()"),
      list(profile_id, granted, actor))
    ds_exec(con, "INSERT INTO app_role_log (spond_profile_id, role, granted, actor) VALUES ($1, $2, $3, $4)",
            list(profile_id, role, granted, actor))
  })
  invisible(TRUE)
}

# Allowlist for login (migration 006) ----------------------------------------------
# The list is for the whole app. Only hashes and masked hints are stored; see
# R/fct_allowlist.R.

ds_assert_admin <- function(rights) {
  if (!isTRUE(rights$admin)) stop("Du har ikke tilgang til å endre innloggingslisten.", call. = FALSE)
}

#' The allowlist, newest first (admins only)
#' @noRd
ds_list_allowlist <- function(con, rights) {
  ds_assert_admin(rights)
  ds_query(con, "
    SELECT id_hash, kind, hint, coalesce(spond_profile_id, '') AS spond_profile_id, last_login_at,
           added_by, added_at
      FROM login_allowlist ORDER BY added_at DESC, hint")
}

#' Add an e-mail address or mobile number to the allowlist
#' @param ident From `login_identifier()`.
#' @return TRUE if added, FALSE if it was on the list already.
#' @noRd
ds_add_allowlist <- function(con, rights, actor, ident, key = login_key()) {
  ds_assert_admin(rights)
  if (is.null(ident)) stop("Skriv en e-postadresse eller et mobilnummer.", call. = FALSE)
  h <- login_hash(ident, key)
  hint <- login_hint(ident)
  added <- FALSE
  ds_transaction(con, {
    res <- ds_query(con, "
      INSERT INTO login_allowlist (id_hash, kind, hint, added_by) VALUES ($1, $2, $3, $4)
      ON CONFLICT (id_hash) DO NOTHING RETURNING id_hash", list(h, ident$kind, hint, actor))
    added <- nrow(res) == 1
    if (added) {
      ds_exec(con, "INSERT INTO login_allowlist_log (id_hash, hint, action, actor) VALUES ($1, $2, 'add', $3)",
              list(h, hint, actor))
    }
  })
  added
}

#' Remove a row from the allowlist
#' @noRd
ds_remove_allowlist <- function(con, rights, actor, id_hash) {
  ds_assert_admin(rights)
  ds_transaction(con, {
    res <- ds_query(con, "DELETE FROM login_allowlist WHERE id_hash = $1 RETURNING hint", list(as.character(id_hash)))
    if (nrow(res)) {
      ds_exec(con, "INSERT INTO login_allowlist_log (id_hash, hint, action, actor) VALUES ($1, $2, 'remove', $3)",
              list(as.character(id_hash), res$hint[1], actor))
    }
  })
  invisible(TRUE)
}

#' At login: is this hash on the list? If so, note who logged in and when.
#' @noRd
ds_allowlist_login <- function(con, id_hash, profile_id) {
  res <- ds_query(con, "
    UPDATE login_allowlist SET spond_profile_id = $2, last_login_at = now()
     WHERE id_hash = $1 RETURNING id_hash", list(id_hash, as.character(profile_id)))
  nrow(res) == 1
}

# Database setup (admin only) -----------------------------------------------------
# These functions are used by dev/setup_db.R with the admin connection. The
# running app never calls them, and the app user is not allowed to.

# The tables the app reads and writes. schema_migrations is deliberately not
# included: only the admin runs migrations.
ds_app_tables <- c("member_tags", "group_proposals", "group_proposal_labels", "group_proposal_members",
                   "proposal_history", "proposal_comments", "edit_locks",
                   "season_themes", "team_settings", "exercises", "training_plans",
                   "app_roles", "app_role_log", "ai_usage", "ai_settings",
                   "login_allowlist", "login_allowlist_log",
                   "training_plan_comments")

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
