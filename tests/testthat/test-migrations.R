# Guards for database changes (see inst/db/migrations/README.md).

test_that("every table the migrations create is one the app user gets rights to", {
  con <- local_test_db()
  tables <- DBI::dbGetQuery(con, "SELECT table_name FROM information_schema.tables
                                   WHERE table_schema = current_schema() AND table_type = 'BASE TABLE'")$table_name
  # schema_migrations is for the admin only
  expect_setequal(setdiff(tables, "schema_migrations"), ds_app_tables)
})

test_that("every table has Row Level Security turned on", {
  con <- local_test_db()
  rls <- DBI::dbGetQuery(con, "SELECT c.relname, c.relrowsecurity FROM pg_class c
                                 JOIN pg_namespace n ON n.oid = c.relnamespace
                                WHERE n.nspname = current_schema() AND c.relkind = 'r'")
  expect_true(all(rls$relrowsecurity[rls$relname %in% c(ds_app_tables, "schema_migrations")]),
              info = paste(rls$relname, collapse = ", "))
})

test_that("migration files are numbered, in order, and follow the format rules", {
  dir <- test_migrations_dir()
  files <- list.files(dir, pattern = "\\.sql$")
  expect_true(length(files) > 0)
  expect_true(all(grepl("^[0-9]{3}_[a-z0-9_]+\\.sql$", files)), info = paste(files, collapse = ", "))
  nums <- as.integer(substr(files, 1, 3))
  expect_equal(nums, seq_along(files))          # 001, 002, ... without gaps
  for (f in files) {
    sql <- paste(readLines(file.path(dir, f), warn = FALSE), collapse = "\n")
    expect_false(grepl("\\bDO\\s+\\$", sql), info = paste(f, "inneholder en DO-blokk"))
    expect_false(grepl("spondnymark_app", sql, fixed = TRUE), info = paste(f, "gir rettigheter selv"))
  }
})
