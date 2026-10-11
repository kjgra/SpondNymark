ref_session <- function() {
  path <- system.file("extdata", "referanse-okt.json", package = "SpondNymark")
  if (!nzchar(path)) path <- testthat::test_path("..", "..", "inst", "extdata", "referanse-okt.json")
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

simple_drawing <- function(...) {
  s <- utils::modifyList(list(bane = list(bredde = 20, lengde = 14)), list(...))
  list(skisser = list(s))
}

test_that("the reference session's drawings are valid and have no overlaps", {
  ref <- ref_session()
  expect_length(ref$ovelser, 3)
  for (ex in ref$ovelser) {
    d <- drawing_validate(ex$tegning)
    expect_equal(nrow(drawing_check(d)), 0, info = ex$navn)
  }
  d <- drawing_validate(ref$ovelser[[1]]$tegning)
  expect_length(d$skisser, 2)
  expect_equal(d$skisser[[1]]$bane$marg, 2.5)
  expect_equal(d$skisser[[1]]$objekter[[5]]$lag, "1")
})

test_that("invalid drawings get a clear Norwegian message", {
  expect_null(drawing_validate(""))
  expect_error(drawing_validate("{ikke json"), "ikke gyldig JSON")
  expect_error(drawing_validate(list(skisser = list())), "1 til 3 skisser")
  expect_error(drawing_validate(simple_drawing(bane = list(bredde = 2, lengde = 10))), "bredde")
  expect_error(drawing_validate(simple_drawing(objekter = list(list(type = "hest", x = 1, y = 1)))), "hest")
  expect_error(drawing_validate(simple_drawing(objekter = list(list(type = "spiller", x = 40, y = 1)))), "utenfor")
  expect_error(drawing_validate(simple_drawing(objekter = list(list(type = "spiller", lag = 3, x = 1, y = 1)))), "Lag")
  expect_error(drawing_validate(simple_drawing(piler = list(list(type = "pasning", fra = 1, til = c(2, 2))))), "punkt")
  expect_error(drawing_validate(simple_drawing(piler = list(list(type = "lop", fra = c(1, 1), til = c(2, 2), bue = 3)))), "Bue")
  expect_error(drawing_validate(simple_drawing(objekter = list(list(type = "spiller", x = 1, y = 1, tekst = "ABCD")))), "maks 3")
  # Defaults are filled in
  d <- drawing_validate(simple_drawing(objekter = list(list(type = "trener", x = 1, y = 1),
                                                         list(type = "maal", x = 0, y = 7)),
                                       piler = list(list(type = "lop", fra = c(1, 1), til = c(5, 5)))))
  expect_equal(d$skisser[[1]]$objekter[[1]]$tekst, "T")
  expect_equal(d$skisser[[1]]$objekter[[2]]$side, "venstre")
  expect_equal(d$skisser[[1]]$piler[[1]]$bue, 0)
})

test_that("overlaps are found, and players and labels are moved apart", {
  bad <- drawing_validate(simple_drawing(
    objekter = list(list(type = "spiller", lag = 1, x = 5, y = 7, tekst = "1"),
                    list(type = "spiller", lag = 2, x = 5.6, y = 7.3, tekst = "F"),
                    list(type = "spiller", lag = 1, x = 16, y = 7, tekst = "2"),
                    list(type = "spiller", lag = 2, x = 11, y = 7.2, tekst = "R")),
    piler = list(list(type = "pasning", fra = c(5.9, 6.6), til = c(16, 7))),
    etiketter = list(list(x = 11, y = 7.4, tekst = "Spill forbi forsvareren"))))
  found <- drawing_check(bad)
  expect_setequal(found$type, c("spillere", "pil", "etikett"))
  expect_match(found$melding[found$type == "spillere"], "Blå spiller 1 og rød spiller F overlapper")
  expect_match(found$melding[found$type == "pil"], "pasning går gjennom rød spiller R")

  fixed <- drawing_fix(bad)
  left <- drawing_check(fixed)
  expect_equal(left$type, "pil")              # arrows are not moved, only reported
  o <- fixed$skisser[[1]]$objekter
  expect_gt(sqrt((o[[1]]$x - o[[2]]$x)^2 + (o[[1]]$y - o[[2]]$y)^2), sqrt(0.6^2 + 0.3^2))
  expect_false(identical(fixed$skisser[[1]]$etiketter[[1]]$y, 7.4))
})

test_that("a drawing is written as a PNG, with the problems attached", {
  path <- withr::local_tempfile(fileext = ".png")
  res <- drawing_png(ref_session()$ovelser[[2]]$tegning, path, width_px = 900)
  expect_true(file.exists(path))
  expect_equal(readBin(path, "raw", 4), as.raw(c(0x89, 0x50, 0x4e, 0x47)))
  expect_s3_class(attr(res, "problems"), "data.frame")
  expect_error(drawing_png("", path), "tom")
  pv <- drawing_preview(drawing_example_json(), width_px = 600)
  expect_match(pv$src, "^data:image/png;base64,")
  expect_equal(nrow(pv$problems), 0)
})

test_that("the layout keeps one scale for sketches side by side", {
  d <- drawing_validate(ref_session()$ovelser[[1]]$tegning)
  lay <- drawing_layout(d, 1800)
  expect_equal(lay$scale, 1800 / (2 * (12 + 5)))
  expect_equal(lay$panels[[2]]$x0, 17 * lay$scale)
  expect_equal(lay$height, round(17 * lay$scale))
})

test_that("drawings are stored with an exercise as cleaned JSON", {
  expect_equal(exercise_drawing_json(""), "")
  expect_equal(exercise_drawing_json(NULL), "")
  js <- exercise_drawing_json(drawing_example_json())
  expect_match(js, '"skisser"')
  expect_false(grepl("\n", js))
  expect_error(exercise_drawing_json("{x"), "Tegning: ")
  v <- exercise_validate(list(name = "X", category = "annet", drawing = drawing_example_json()))
  expect_equal(v$drawing, js)
  expect_match(drawing_pretty(js), "\n  ")
  expect_equal(drawing_pretty(NA), "")
  # Compact: no empty parts or default values, and the drawing survives a round trip
  expect_false(grepl('"etiketter"|"oransje"|"marg"|"bue":0[,}]', js))
  expect_match(drawing_example_json(), '\n        \\{"type":"kjegle","x":0,"y":0\\},\n')   # one object per line
  expect_equal(drawing_validate(js), drawing_validate(drawing_example_json()))
  ref <- ref_session()$ovelser[[2]]$tegning
  expect_equal(drawing_validate(drawing_json(drawing_validate(ref))), drawing_validate(ref))
})

test_that("drawings have limits, so they cannot keep the server busy", {
  many <- function(n) lapply(seq_len(n), function(i) list(x = 1, y = 1, tekst = "x"))
  d <- list(skisser = list(list(bane = list(bredde = 20, lengde = 20), etiketter = many(16))))
  expect_error(drawing_validate(d), "Maks 15 etiketter")
  d$skisser[[1]]$etiketter <- NULL
  d$skisser[[1]]$soner <- lapply(1:11, function(i) list(x = 1, y = 1, b = 1, h = 1))
  expect_error(drawing_validate(d), "Maks 10 soner")
  expect_error(drawing_validate(strrep(" ", 20001)), NA)            # only blanks: no drawing
  expect_error(drawing_validate(paste0("{", strrep(" ", 20001), "}")), "for stor")
  ref <- jsonlite::read_json(testthat::test_path("..", "..", "inst", "extdata", "referanse-okt.json"),
                             simplifyVector = FALSE)
  for (ex in ref$ovelser) if (!is.null(ex$tegning)) expect_error(drawing_validate(ex$tegning), NA)
})
