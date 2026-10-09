test_that("tags are cleaned and checked", {
  expect_equal(tag_clean("  Midt   bane "), "Midt bane")
  expect_equal(tag_clean(NULL), "")
  expect_null(tag_problem("Keeper"))
  expect_null(tag_problem("Venstrebein"))
  expect_null(tag_problem("Sykler til trening"))     # "syk" only as a whole word
  expect_match(tag_problem("  "), "Skriv inn")
  expect_match(tag_problem(strrep("a", 41)), "maks 40")
})

test_that("health and other sensitive words are refused", {
  for (t in c("Skadet", "skade i kneet", "Syk", "sykemeldt", "Astma", "ADHD", "Allergisk mot nøtter",
              "Diabetes", "Hjernerystelse", "Brudd", "Barnevern", "Psykisk", "Medisiner")) {
    expect_match(tag_problem(t), "sensitive", info = t)
  }
})

test_that("contact information is refused", {
  expect_match(tag_problem("mor@example.no"), "kontaktinformasjon")
  expect_match(tag_problem("Ring 99 88 77 66"), "kontaktinformasjon")
  expect_null(tag_problem("G2016"))
})

test_that("existing spelling is reused", {
  expect_equal(tag_canonical("keeper", c("Kaptein", "Keeper")), "Keeper")
  expect_equal(tag_canonical("Ny", c("Kaptein")), "Ny")
})

test_that("tags per member and suggestions", {
  t <- data.frame(member_id = c("M1", "M1", "M2", "M3"), tag = c("Keeper", "Angrep", "Venstrebein", "Venstrebein"))
  expect_equal(tags_for(t, "M1"), c("Angrep", "Keeper"))
  expect_equal(tags_for(tags_empty(), "M1"), character())
  s <- tag_suggestions_for(t, "M1", fixed = c("Keeper", "Kaptein"))
  expect_equal(s, c("Kaptein", "Venstrebein"))   # fixed first, then used in the group; not ones M1 has
})

test_that("default suggestions are neutral and allowed", {
  s <- tag_suggestions()
  expect_true(length(s) > 0)
  for (t in s) expect_null(tag_problem(t), info = t)
})

test_that("member chips show tags; clickable chips carry the member id", {
  plain <- as.character(member_chip("M1", "Emma H.", c("Keeper")))
  expect_match(plain, "<span class=\"sn-chip\">\\s*Emma H.")
  expect_match(plain, "sn-minitag\">Keeper")
  btn <- as.character(member_chip("M1", "Emma H.", c("Keeper"), open_input = "tags-open"))
  expect_match(btn, "data-sn-input=\"tags-open\"")
  expect_match(btn, "data-sn-value=\"M1\"")
  expect_match(btn, "aria-label=\"Emma H., tagger: Keeper. Endre tagger.\"")
  html <- as.character(member_chips(c("M1", "M2"), c("A", "B"),
                                    tagger = list(chip = function(id, n) member_chip(id, n, open_input = "x")),
                                    clickable = c(TRUE, FALSE)))
  expect_equal(lengths(regmatches(html, gregexpr("data-sn-value", html))), 1)
})

test_that("tag values are escaped in HTML", {
  html <- as.character(member_chip("M1", "<b>", c("\"><script>"), open_input = "x"))
  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_false(grepl("<b>", html, fixed = TRUE))
})

test_that("remembered details keep their open state", {
  d <- function(open) as.character(remembered_details("x", open, FALSE, "Tittel", "innhold"))
  expect_false(grepl(" open", d(NULL)))
  expect_match(d(TRUE), "<details open data-sn-toggle=\"x\">")
  expect_false(grepl(" open", d(FALSE)))
  expect_match(as.character(remembered_details("x", NULL, TRUE, "T")), "open")
})

test_that("tags get the shortest unique short label", {
  all <- c("Angrep", "Forsvar", "Keeper", "Kaptein", "Midtbane", "Ny")
  expect_equal(tag_abbrev(c("Angrep", "Forsvar", "Midtbane"), all), c("A", "F", "M"))
  expect_equal(tag_abbrev(c("Keeper", "Kaptein"), all), c("Ke", "Ka"))
  expect_equal(tag_abbrev("ny", c("ny", "nybegynner")), "ny")   # start of another tag: in full
  expect_equal(tag_abbrev("venstrebein", c("venstrebein", "Venstre kant")), "Venstreb")
  expect_equal(tag_abbrev(character(), all), character())
})

test_that("short chips show short labels with the full tag as tooltip", {
  html <- as.character(member_chip("M-1", "Emma H.", c("Angrep", "Keeper"), short = TRUE,
                                   universe = c("Angrep", "Keeper", "Kaptein")))
  expect_match(html, "title=\"Angrep\">A<")
  expect_match(html, "title=\"Keeper\">Ke<")
  full <- as.character(member_chip("M-1", "Emma H.", c("Angrep")))
  expect_match(full, "sn-minitag\">Angrep<")
})
