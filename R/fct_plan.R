#' Training plans (treningsopplegg): format, schedule and PDF
#'
#' A plan is stored as JSON (see inst/extdata/referanse-okt.json for a full
#' example, and claude/plan-treningsopplegg.md). The PDF is made on demand
#' with Quarto's bundled Typst from a fixed template (inst/typst/opplegg.typ)
#' that reads the plan from data.json; the drawings are made as PNG files by
#' `drawing_png()`. Neither knitr, rmarkdown nor Pandoc is needed.
#'
#' Player names are never part of the plan. They can be passed to
#' `plan_pdf()` (`groups`) when the PDF is made, and are not stored.
#'
#' Text fields may use **double asterisks** for bold; the template turns
#' them into bold text. Nothing else in the text is treated as markup.
#'
#' @name fct_plan
#' @noRd
NULL

plan_text_lists <- c("organisering", "gjennomforing", "tilpasning", "laeringsmomenter", "sporsmal")

plan_str <- function(x, what, max, required = FALSE) {
  x <- txt1(x)
  if (required && !nzchar(x)) stop(what, " mangler.", call. = FALSE)
  if (nchar(x) > max) stop(what, " kan ha maks ", max, " tegn.", call. = FALSE)
  x
}

plan_strs <- function(x, what, max_items = 12, max_chars = 400) {
  x <- vapply(x %||% list(), function(v) txt1(v), character(1))
  x <- x[nzchar(x)]
  if (length(x) > max_items) stop(what, " kan ha maks ", max_items, " punkter.", call. = FALSE)
  if (any(nchar(x) > max_chars)) stop("Et punkt i ", tolower(what), " kan ha maks ", max_chars, " tegn.", call. = FALSE)
  as.list(x)
}

plan_int <- function(x, what, lo, hi, default) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) return(default)
  v <- suppressWarnings(as.numeric(unlist(x)))
  if (length(v) != 1 || is.na(v) || v != round(v) || v < lo || v > hi) {
    stop(what, " m\u00e5 v\u00e6re et helt tall fra ", lo, " til ", hi, ".", call. = FALSE)
  }
  as.integer(v)
}

#' Check a plan and return it in a clean, complete form
#'
#' Stops with a Norwegian message saying what is wrong.
#' @noRd
plan_validate <- function(p) {
  if (is.character(p)) {
    p <- tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE),
                  error = function(e) stop("Opplegget er ikke gyldig JSON.", call. = FALSE))
  }
  if (!is.list(p)) stop("Opplegget m\u00e5 v\u00e6re et JSON-objekt.", call. = FALSE)
  ex <- p$ovelser %||% list()
  if (length(ex) < 1 || length(ex) > 6) stop("Et opplegg m\u00e5 ha 1 til 6 \u00f8velser.", call. = FALSE)
  tp <- p$tidsplan %||% list()
  out <- list(
    versjon = 1L,
    tittel = plan_str(p$tittel, "Tittelen", 80, required = TRUE),
    undertittel = plan_str(p$undertittel, "Undertittelen", 160),
    tema = plan_str(p$tema, "Temaet", 80),
    fokus = list(forsvar = plan_str(p$fokus$forsvar, "Fokus i forsvar", 300),
                 angrep = plan_str(p$fokus$angrep, "Fokus i angrep", 300)),
    stikkord = lapply(p$stikkord %||% list(), function(s) {
      list(tittel = plan_str(s$tittel, "Et stikkord", 60, required = TRUE), tekst = plan_str(s$tekst, "Stikkordteksten", 160))
    }),
    stikkord_merknad = plan_str(p$stikkord_merknad, "Merknaden til stikkordene", 300),
    tidsplan = list(
      oppvarming = list(minutter = plan_int(tp$oppvarming$minutter, "Oppvarmingen", 0, 60, 0L),
                        tekst = plan_str(tp$oppvarming$tekst, "Oppvarmingsteksten", 200)),
      stasjoner = list(minutter = plan_int(tp$stasjoner$minutter, "Tid per \u00f8velse", 1, 90, 15L),
                       bytte = plan_int(tp$stasjoner$bytte, "Byttetiden", 0, 10, 0L),
                       grupper = plan_int(tp$stasjoner$grupper, "Antall grupper", 1, 6, 1L)),
      avslutning = list(minutter = plan_int(tp$avslutning$minutter, "Avslutningen", 0, 60, 0L),
                        tekst = plan_str(tp$avslutning$tekst, "Avslutningsteksten", 200)),
      merknad = plan_str(tp$merknad, "Merknaden til tidsplanen", 500)
    ),
    avslutning_sporsmal = plan_strs(p$avslutning_sporsmal, "Sp\u00f8rsm\u00e5l til avslutningen", 6, 200),
    kilde = plan_str(p$kilde, "Kildelinjen", 500)
  )
  if (length(out$stikkord) > 4) stop("Maks 4 stikkord.", call. = FALSE)
  g <- out$tidsplan$stasjoner$grupper
  if (g > 1 && g != length(ex)) {
    stop("Med ", g, " grupper som roterer m\u00e5 opplegget ha ", g, " \u00f8velser (\u00e9n per stasjon).", call. = FALSE)
  }
  out$ovelser <- lapply(seq_along(ex), function(i) {
    e <- ex[[i]]
    w <- paste0("\u00d8velse ", i, ": ")
    r <- list(
      kode = plan_str(e$kode, paste0(w, "Koden"), 60),
      navn = plan_str(e$navn, paste0(w, "Navnet"), 80, required = TRUE),
      kategori = plan_str(e$kategori, paste0(w, "Kategorien"), 60),
      fokus = plan_str(e$fokus, paste0(w, "Fokus"), 300)
    )
    for (f in plan_text_lists) r[[f]] <- plan_strs(e[[f]], paste0(w, f))
    r$enklere <- plan_str(e$enklere, paste0(w, "Enklere"), 400)
    r$vanskeligere <- plan_str(e$vanskeligere, paste0(w, "Vanskeligere"), 400)
    r$nff_url <- plan_str(e$nff_url, paste0(w, "NFF-lenken"), 500)
    if (nzchar(r$nff_url) && !grepl("^https://[^[:space:]]+$", r$nff_url)) {
      stop(w, "NFF-lenken m\u00e5 starte med https://.", call. = FALSE)
    }
    if (!nzchar(r$kode)) r$kode <- exercise_code(r$navn)
    r$tegning <- if (is.null(e$tegning)) NULL else {
      tryCatch(drawing_validate(e$tegning), error = function(err) stop(w, conditionMessage(err), call. = FALSE))
    }
    r
  })
  out
}

#' The schedule of a plan: rows of a table, with the rotation of groups
#'
#' With several groups, group j does exercise ((j + k - 2) mod n) + 1 in
#' round k, so every group does every exercise once. With one group the
#' exercises come one after the other.
#' @return list(columns, rows, total): `rows` is a list of
#'   list(tid = "8–23", felles = FALSE, celler = c("Øvelse 1", ...)) or
#'   list(tid = "0–8", felles = TRUE, tekst = "...").
#' @noRd
plan_schedule <- function(p) {
  tp <- p$tidsplan
  n <- length(p$ovelser)
  g <- tp$stasjoner$grupper
  span <- function(a, b) paste0(a, "\u2013", b)
  rows <- list()
  t <- 0L
  if (tp$oppvarming$minutter > 0) {
    rows[[length(rows) + 1]] <- list(tid = span(t, t + tp$oppvarming$minutter), felles = TRUE,
                                     tekst = if (nzchar(tp$oppvarming$tekst)) tp$oppvarming$tekst else "Felles oppvarming")
    t <- t + tp$oppvarming$minutter
  }
  for (k in seq_len(n)) {
    if (k > 1) t <- t + tp$stasjoner$bytte
    cells <- if (g > 1) {
      vapply(seq_len(g), function(j) paste("\u00d8velse", ((j + k - 2) %% n) + 1), character(1))
    } else {
      paste0("\u00d8velse ", k, ": ", p$ovelser[[k]]$navn)
    }
    rows[[length(rows) + 1]] <- list(tid = span(t, t + tp$stasjoner$minutter), felles = FALSE, celler = as.list(cells))
    t <- t + tp$stasjoner$minutter
  }
  if (tp$avslutning$minutter > 0) {
    rows[[length(rows) + 1]] <- list(tid = span(t, t + tp$avslutning$minutter), felles = TRUE,
                                     tekst = if (nzchar(tp$avslutning$tekst)) tp$avslutning$tekst else "Felles avslutning")
    t <- t + tp$avslutning$minutter
  }
  list(columns = c("Tid", if (g > 1) paste("Gruppe", seq_len(g)) else "Alle"), rows = rows, total = t)
}

#' Find Quarto (which brings Typst): QUARTO_PATH, PATH, then the usual places
#' @noRd
quarto_bin <- function() {
  cand <- c(Sys.getenv("QUARTO_PATH"), unname(Sys.which("quarto")), Sys.getenv("RSTUDIO_QUARTO"),
            Sys.glob("/opt/quarto/*/bin/quarto"), "/opt/quarto/bin/quarto", "/usr/local/bin/quarto",
            "C:/Program Files/Quarto/bin/quarto.exe",
            "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe",
            file.path(Sys.getenv("LOCALAPPDATA"), "Programs/Positron/resources/app/quarto/bin/quarto.exe"))
  cand <- cand[!is.na(cand) & nzchar(cand)]
  cand <- cand[file.exists(cand)]
  if (length(cand)) normalizePath(cand[[1]], winslash = "/") else ""
}

#' The legend (tegnforklaring) as a PNG, in the style of the drawings
#' @noRd
plan_legend_png <- function(path, width_px = 1800) {
  sz <- drawing_sizes(width_px)
  h <- round(width_px * 0.075)
  args <- list(filename = path, width = width_px, height = h, res = 72, bg = drawing_colours$grass[2])
  if (isTRUE(capabilities("cairo"))) args$type <- "cairo"
  do.call(grDevices::png, args)
  ok <- FALSE
  on.exit(if (!ok) grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(xscale = c(0, width_px), yscale = c(0, h)))
  y <- h / 2
  r <- sz$player_r * 0.85
  label <- function(x, t) grid::grid.text(t, x, y, just = "left", default.units = "native",
                                         gp = grid::gpar(col = "white", fontsize = sz$font * 0.95))
  person <- function(x, fill, txt = "") {
    grid::grid.circle(x, y, r, default.units = "native", gp = grid::gpar(fill = fill, col = "white", lwd = sz$line_lwd))
    if (nzchar(txt)) grid::grid.text(txt, x, y, default.units = "native",
                                     gp = grid::gpar(col = "white", fontface = "bold", fontsize = sz$font * 0.6))
  }
  step <- width_px / 8.5
  x <- step * 0.25
  person(x, drawing_colours$team[["1"]]); label(x + r * 1.5, "Lag 1"); x <- x + step
  person(x, drawing_colours$team[["2"]]); label(x + r * 1.5, "Lag 2"); x <- x + step
  person(x, drawing_colours$trainer, "T"); label(x + r * 1.5, "Trener"); x <- x + step
  grid::grid.circle(x, y, sz$ball_r, default.units = "native", gp = grid::gpar(fill = "white", col = "#121212", lwd = sz$line_lwd))
  grid::grid.circle(x, y, sz$ball_r * 0.38, default.units = "native", gp = grid::gpar(fill = "#121212", col = NA))
  label(x + r * 1.2, "Ball"); x <- x + step * 0.8
  cs <- sz$cone
  grid::grid.polygon(x + c(-cs / 2, cs / 2, 0), y + c(-cs * 0.38, -cs * 0.38, cs * 0.5), default.units = "native",
                     gp = grid::gpar(fill = drawing_colours$cone[["oransje"]], col = "#c45c00", lwd = sz$line_lwd * 0.5))
  label(x + r * 1.2, "Kjegle"); x <- x + step
  arrow <- function(x, type, txt) {
    px <- cbind(c(x, x + step * 0.42), c(y, y))
    col <- if (type == "pasning") "white" else "#121212"
    line <- if (type == "foring") drawing_wave(px, amp = r * 0.3, wavelength = r * 1.2) else px
    n <- nrow(line)
    grid::grid.lines(line[-n, 1], line[-n, 2], default.units = "native",
                     gp = grid::gpar(col = col, lwd = sz$arrow_lwd * 0.85, lty = if (type == "lop") "33" else "solid", lineend = "butt"))
    grid::grid.draw(drawing_head(px, sz$head * 0.85, col))
    label(x + step * 0.5, txt)
  }
  arrow(x, "pasning", "Pasning"); x <- x + step
  arrow(x, "lop", "L\u00f8p"); x <- x + step
  arrow(x, "foring", "F\u00f8ring")
  grid::popViewport()
  grDevices::dev.off()
  ok <- TRUE
  invisible(path)
}

#' Make the PDF of a plan
#'
#' @param p A plan (list or JSON text), checked with `plan_validate()`.
#' @param path Where to write the PDF.
#' @param footer Text in the footer, e.g. "G10 · Tema oktober: Samhandling".
#' @param info Lines under the title, e.g. date, team and numbers.
#' @param groups Optional list of list(navn, spillere = character()) shown on
#'   the first page. Names are only used here and are not stored.
#' @return `path`, invisibly, with the attribute "problems": overlaps found
#'   in the drawings (data.frame with ovelse, skisse, type, melding).
#' @noRd
plan_pdf <- function(p, path, footer = NULL, info = character(), groups = list(),
                     quarto = quarto_bin(), template = app_sys("typst", "opplegg.typ"),
                     fonts = app_sys("typst", "fonts")) {
  p <- plan_validate(p)
  if (!nzchar(quarto)) stop("Fant ikke Quarto p\u00e5 serveren, s\u00e5 PDF kan ikke lages.", call. = FALSE)
  if (!nzchar(template) || !file.exists(template)) stop("Fant ikke PDF-malen.", call. = FALSE)
  dir <- tempfile("opplegg-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  problems <- list()
  for (i in seq_along(p$ovelser)) {
    d <- p$ovelser[[i]]$tegning
    if (is.null(d)) next
    file <- sprintf("ovelse-%d.png", i)
    res <- drawing_png(d, file.path(dir, file), width_px = 1800)
    lay <- drawing_layout(drawing_validate(d), 1800)
    p$ovelser[[i]]$bilde <- file
    # Tall drawings are limited by height so the text fits on the same page.
    p$ovelser[[i]]$bilde_hoy <- lay$height / lay$width > 0.78
    pr <- attr(res, "problems")
    if (nrow(pr)) problems[[length(problems) + 1]] <- cbind(ovelse = i, pr)
    p$ovelser[[i]]$tegning <- NULL
  }
  plan_legend_png(file.path(dir, "tegnforklaring.png"))

  sched <- plan_schedule(p)
  data <- c(p, list(
    tidsplan_tabell = sched,
    varighet = sched$total,
    info = as.list(as.character(info)),
    bunntekst = footer %||% paste(c(if (nzchar(p$tema)) p$tema, "Laget med Nymark \u2013 Gruppeorganisering"),
                                  collapse = " \u00b7 "),
    grupper = lapply(groups, function(g) list(navn = txt1(g$navn), spillere = as.list(as.character(g$spillere))))
  ))
  jsonlite::write_json(data, file.path(dir, "data.json"), auto_unbox = TRUE, null = "null", digits = NA)
  file.copy(template, file.path(dir, "opplegg.typ"))

  out <- file.path(dir, "opplegg.pdf")
  res <- suppressWarnings(system2(quarto, c("typst", "compile", "--font-path", shQuote(fonts),
                                            shQuote(file.path(dir, "opplegg.typ")), shQuote(out)),
                                  stdout = TRUE, stderr = TRUE, timeout = 120))
  if (!file.exists(out)) {
    message("Typst feilet: ", paste(utils::tail(res, 15), collapse = "\n"))
    stop("Fikk ikke laget PDF-en.", call. = FALSE)
  }
  file.copy(out, path, overwrite = TRUE)
  problems <- if (length(problems)) do.call(rbind, problems) else
    data.frame(ovelse = integer(), skisse = integer(), type = character(), melding = character())
  invisible(structure(path, problems = problems))
}
