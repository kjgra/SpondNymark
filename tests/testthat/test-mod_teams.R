testthat::test_that("mod_teams ui", {
  testthat::expect_s3_class(mod_teams_ui("test"), "shiny.tag")
  testthat::expect_s3_class(mod_teams_bar_ui("test"), "shiny.tag")
  testthat::expect_true("id" %in% names(formals(mod_teams_ui)))
})

testthat::test_that("one group with subgroups: group step skipped, subgroup step shown", {
  user <- reactiveVal(NULL)
  testServer(mod_teams_server, args = list(user = user), {
    expect_equal(step(), "none")
    user(fake_user())
    session$flushReact()
    expect_equal(step(), "pick_subgroup")
    expect_null(session$returned())
    html <- paste(output$picker$html)
    expect_match(html, "Velg gruppe")
    expect_false(grepl("Hele gruppen", html))
    # The main group is a choice of its own, named after the group, with its subgroups nested under it
    expect_match(html, "Nymark G/J 2016</span>\\s*<span class=\"sn-pickrow-detail\">Hovedgruppe", perl = TRUE)
    expect_match(html, "Undergrupper i Nymark G/J 2016")
    expect_match(html, "sn-subtree[\\s\\S]*Nymark Ulv G10", perl = TRUE)
    expect_false(grepl("Bytt lag", html))   # only one group

    session$setInputs(pick_subgroup = "S-ulv")
    ctx <- session$returned()
    expect_equal(ctx$subgroup_name, "Nymark Ulv G10")
    expect_equal(ctx$label, "Nymark Ulv G10")
    expect_equal(ctx$path, c("Nymark G/J 2016", "Nymark Ulv G10"))
    expect_equal(ctx$members$id, "M-1")
    # Top bar: main group first, subgroups indented under it
    bar <- paste(output$bar$html)
    expect_match(bar, '<option value=".all">Nymark G/J 2016</option>')
    expect_match(bar, "\u00a0\u00a0\u2514\u00a0Nymark Ulv G10")

    session$setInputs(subgroup_select = teams_all)
    ctx <- session$returned()
    expect_null(ctx$subgroup_id)
    expect_equal(ctx$label, "Nymark G/J 2016")
    expect_equal(ctx$path, "Nymark G/J 2016")
    expect_equal(nrow(ctx$members), 2)
  })
})

testthat::test_that("two groups: pick a group; a group without subgroups goes straight to the whole group", {
  user <- reactiveVal(NULL)
  testServer(mod_teams_server, args = list(user = user), {
    user(fake_user(fake_spond_groups_two()))
    session$flushReact()
    expect_equal(step(), "pick_group")
    expect_match(paste(output$picker$html), "Nymark Senior")
    expect_match(paste(output$picker$html), "2 undergrupper")   # G2016 shows it has subgroups

    session$setInputs(pick_group = "G-senior")
    expect_equal(step(), "ready")
    expect_equal(session$returned()$label, "Nymark Senior")
    expect_match(paste(output$bar$html), "Nymark Senior")       # group select only
    expect_false(grepl("subgroup_select", paste(output$bar$html)))

    # Switch group from the top bar -> subgroup step for the group with subgroups
    session$setInputs(group_select = "G2016")
    expect_equal(step(), "pick_subgroup")
    expect_match(paste(output$picker$html), "Bytt lag")
    session$setInputs(back_to_groups = 1)
    expect_equal(step(), "pick_group")
  })
})

testthat::test_that("unknown ids from the browser are ignored", {
  user <- reactiveVal(NULL)
  testServer(mod_teams_server, args = list(user = user), {
    user(fake_user(fake_spond_groups_two()))
    session$flushReact()
    session$setInputs(pick_group = "G-parents")       # a group the user has no access to
    expect_equal(step(), "pick_group")
    session$setInputs(pick_group = "G2016")
    session$setInputs(pick_subgroup = "S-unknown")
    expect_equal(step(), "pick_subgroup")
  })
})

testthat::test_that("logging out clears the choice", {
  user <- reactiveVal(NULL)
  testServer(mod_teams_server, args = list(user = user), {
    user(fake_user())
    session$flushReact()
    session$setInputs(pick_subgroup = teams_all)
    expect_false(is.null(session$returned()))
    user(NULL)
    session$flushReact()
    expect_null(session$returned())
    expect_equal(step(), "none")
    # Logging in again starts over
    user(fake_user())
    session$flushReact()
    expect_equal(step(), "pick_subgroup")
  })
})

testthat::test_that("the card title shows which main group a subgroup belongs to", {
  title_text <- function(ctx) gsub("\\s+", " ", trimws(gsub("<[^>]+>", " ", as.character(context_title(ctx)))))
  testthat::expect_equal(title_text(list(path = "Nymark G/J 2016")), "Nymark G/J 2016")
  testthat::expect_equal(title_text(list(path = c("Nymark G/J 2016", "Nymark Ulv G10"))),
                         "Nymark G/J 2016 › Nymark Ulv G10")
})
