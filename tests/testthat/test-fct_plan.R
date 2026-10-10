ref_plan <- function() {
  path <- system.file("extdata", "referanse-okt.json", package = "SpondNymark")
  if (!nzchar(path)) path <- testthat::test_path("..", "..", "inst", "extdata", "referanse-okt.json")
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

test_that("the reference session is a valid plan", {
  p <- plan_validate(ref_plan())
  expect_equal(p$tittel, "Samhandling – spille på lag")
  expect_length(p$ovelser, 3)
  expect_length(p$stikkord, 3)
  expect_equal(p$tidsplan$stasjoner$grupper, 3L)
  expect_length(p$ovelser[[1]]$laeringsmomenter, 4)
  expect_length(p$ovelser[[1]]$tegning$skisser, 2)
  expect_equal(p$ovelser[[2]]$kode, "hjem-bak-ballen")
})

test_that("invalid plans get a clear Norwegian message", {
  p <- ref_plan()
  expect_error(plan_validate("{x"), "ikke gyldig JSON")
  expect_error(plan_validate(utils::modifyList(p, list(tittel = ""))), "Tittelen mangler")
  q <- p; q$ovelser <- list(); expect_error(plan_validate(q), "1 til 6")
  q <- p; q$ovelser <- q$ovelser[1:2]; expect_error(plan_validate(q), "3 grupper")
  q <- p; q$tidsplan$stasjoner$minutter <- 7.5; expect_error(plan_validate(q), "helt tall")
  q <- p; q$ovelser[[1]]$navn <- ""; expect_error(plan_validate(q), "Øvelse 1: Navnet mangler")
  q <- p; q$ovelser[[2]]$tegning$skisser[[1]]$objekter[[1]]$type <- "hest"
  expect_error(plan_validate(q), "Øvelse 2: Skisse 1: Ukjent objekttype")
  q <- p; q$ovelser[[1]]$nff_url <- "http://x"; expect_error(plan_validate(q), "https")
  # Missing parts get defaults; a missing code is made from the name
  q <- list(tittel = "Kort", ovelser = list(list(navn = "Rondo 4 mot 1")))
  v <- plan_validate(q)
  expect_equal(v$ovelser[[1]]$kode, "rondo-4-mot-1")
  expect_equal(v$tidsplan$stasjoner$grupper, 1L)
  expect_null(v$ovelser[[1]]$tegning)
})

test_that("the schedule rotates the groups like the reference session", {
  s <- plan_schedule(plan_validate(ref_plan()))
  expect_equal(s$columns, c("Tid", "Gruppe 1", "Gruppe 2", "Gruppe 3"))
  expect_equal(vapply(s$rows, `[[`, "", "tid"), c("0–8", "8–23", "25–40", "42–57", "57–65"))
  expect_equal(unlist(s$rows[[2]]$celler), c("Øvelse 1", "Øvelse 2", "Øvelse 3"))
  expect_equal(unlist(s$rows[[3]]$celler), c("Øvelse 2", "Øvelse 3", "Øvelse 1"))
  expect_equal(unlist(s$rows[[4]]$celler), c("Øvelse 3", "Øvelse 1", "Øvelse 2"))
  expect_true(s$rows[[1]]$felles)
  expect_equal(s$total, 65L)

  one <- plan_validate(list(tittel = "X", tidsplan = list(stasjoner = list(minutter = 20)),
                            ovelser = list(list(navn = "A"), list(navn = "B"))))
  s1 <- plan_schedule(one)
  expect_equal(s1$columns, c("Tid", "Alle"))
  expect_equal(unlist(s1$rows[[2]]$celler), "Øvelse 2: B")
  expect_equal(s1$total, 40L)
})

test_that("the legend is drawn", {
  path <- withr::local_tempfile(fileext = ".png")
  plan_legend_png(path, width_px = 900)
  expect_equal(readBin(path, "raw", 4), as.raw(c(0x89, 0x50, 0x4e, 0x47)))
})

test_that("the PDF is made with the template, fonts and drawings", {
  skip_if(!nzchar(quarto_bin()), "Quarto er ikke installert.")
  path <- withr::local_tempfile(fileext = ".pdf")
  res <- plan_pdf(ref_plan(), path, footer = "G10 · Test", info = c("Lør. 10. okt.", "24 spillere"),
                  groups = list(list(navn = "Gruppe A", spillere = c("Emma H.", "Noah S."))))
  expect_true(file.exists(path))
  expect_equal(readBin(path, "raw", 5), charToRaw("%PDF-"))
  expect_gt(file.size(path), 50000)
  expect_equal(nrow(attr(res, "problems")), 0)
  expect_error(plan_pdf(ref_plan(), path, quarto = ""), "Fant ikke Quarto")
  # Three named groups: the rotation table uses the names (checked in the PDF text)
  skip_if(!nzchar(Sys.which("pdftotext")), "pdftotext mangler.")
  plan_pdf(ref_plan(), path, groups = list(list(navn = "Ulv", spillere = "A"), list(navn = "Gaupe", spillere = "B"),
                                           list(navn = "Bjørn", spillere = "C")))
  txt <- paste(system2("pdftotext", c("-l", "1", shQuote(path), "-"), stdout = TRUE), collapse = " ")
  expect_match(txt, "Tid\\s+Ulv\\s+Gaupe\\s+Bjørn")
})

bank_rows <- function() {
  data.frame(id = 1:2, code = c("rondo", "smaaspill-4-4"), name = c("Rondo", "4 mot 4"),
             category = c("pasning_mottak", "smaaspill"),
             organisation = c("- 10 × 10 m\n- 4 mot 1", ""), execution = c("Hold ballen.", ""),
             learning_points = c("• Se opp\n• Åpne kroppen\n\n", ""), questions = c("", ""),
             easier = c("Større rute", ""), harder = c("", ""), nff_url = c("", ""),
             drawing = c(drawing_json(drawing_validate(drawing_example_json())), NA))
}

test_that("text fields become list items without bullets", {
  expect_equal(text_lines("- a\n• b\n\n 3) c \n*d"), list("a", "b", "c", "*d"))
  expect_equal(text_lines(""), list())
  expect_equal(text_lines(NA), list())
})

test_that("a manual plan is made from bank exercises, as a snapshot", {
  s <- plan_settings_default("Samhandling")
  p <- plan_from_exercises(bank_rows(), s)
  expect_equal(p$tittel, "Samhandling")
  expect_equal(p$tidsplan$stasjoner$grupper, 2L)       # rotation: one station per exercise
  expect_equal(p$undertittel, "2 stasjoner, 2 grupper · ca. 50 min")
  e <- p$ovelser[[1]]
  expect_equal(c(e$kode, e$kilde, e$kategori), c("rondo", "bank", "Pasning og mottak"))
  expect_equal(e$organisering, list("10 × 10 m", "4 mot 1"))
  expect_equal(e$laeringsmomenter, list("Se opp", "Åpne kroppen"))
  expect_length(e$tegning$skisser, 1)
  expect_null(p$ovelser[[2]]$tegning)

  s$rotasjon <- FALSE
  s$stikkord <- list(list(tittel = "Se opp", tekst = ""), list(tittel = "", tekst = "ignoreres"))
  s$sporsmal <- "«Hvem er ledig?»\n\n«Hvor er rommet?»"
  p1 <- plan_from_exercises(bank_rows(), s)
  expect_equal(p1$tidsplan$stasjoner$grupper, 1L)
  expect_length(p1$stikkord, 1)
  expect_length(p1$avslutning_sporsmal, 2)
  expect_match(p1$undertittel, "^2 øvelser")
  expect_error(plan_from_exercises(bank_rows()[0, ], s), "minst én")
})

test_that("a plan survives a round trip through JSON", {
  p <- plan_validate(jsonlite::fromJSON(testthat::test_path("..", "..", "inst", "extdata", "referanse-okt.json"),
                                        simplifyVector = FALSE))
  js <- plan_json(p)
  expect_false(grepl('"marg"|"oransje"', js))      # drawings are stored compact
  expect_equal(plan_validate(js), p)
  q <- p; q$ovelser[[1]]$kilde <- "tull"
  expect_error(plan_validate(q), "bank, justert eller ny")
})

test_that("a plan exercise is turned back into bank fields", {
  e <- plan_from_exercises(bank_rows(), plan_settings_default())$ovelser[[1]]
  b <- bank_exercise_from_plan(e, themes = "Samhandling")
  expect_equal(b$category, "pasning_mottak")
  expect_equal(b$organisation, "10 × 10 m\n4 mot 1")
  expect_match(b$drawing, '"skisser"')
  v <- exercise_validate(b)
  expect_equal(v$themes, "Samhandling")
  expect_equal(bank_exercise_from_plan(list(navn = "X", kategori = "Spille med og mot"))$category, "annet")
})
