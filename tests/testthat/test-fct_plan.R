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
})
