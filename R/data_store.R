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
