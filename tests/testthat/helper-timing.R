# No timing lines in the test output (R/fct_timing.R); test-fct_timing.R turns
# them on where it needs them. Restored when the tests are done.
if (testthat::is_testing()) {
  withr::local_envvar(SPONDNYMARK_TIMING = "off", .local_envir = testthat::teardown_env())
}
