#' T0: can the server make a Typst PDF?
#'
#' TEMPORARY (plan-treningsopplegg, phase T0). Shown only outside production
#' (SPONDNYMARK_ENV set, e.g. "test"). Tries two ways of making a PDF and
#' shows a diagnosis:
#' 1. `quarto render test.qmd --to typst` (Markdown without R chunks, so no
#'    knitr is needed).
#' 2. `quarto typst compile mal.typ`, where the Typst template reads its
#'    data from data.json. Not test.typ: quarto render writes and then
#'    deletes its own intermediate test.typ.
#' Uses only base R, so the manifest does not change.
#'
#' Remove after T0: this file, tests/testthat/test-mod_quarto_test.R and the
#' two calls in app_ui.R and app_server.R.
#' @name mod_quarto_test
#' @noRd
NULL

quarto_test_enabled <- function(env = Sys.getenv("SPONDNYMARK_ENV")) {
  nzchar(app_env_label(env))
}

mod_quarto_test_ui <- function(id, env = Sys.getenv("SPONDNYMARK_ENV")) {
  if (!quarto_test_enabled(env)) return(NULL)
  ns <- NS(id)
  actionButton(ns("run"), "Quarto-test", class = "btn-sm btn-outline-light ms-2")
}

mod_quarto_test_server <- function(id, env = Sys.getenv("SPONDNYMARK_ENV")) {
  if (!quarto_test_enabled(env)) return(invisible(NULL))
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    result <- reactiveVal(NULL)
    dirs <- character()
    session$onSessionEnded(function() unlink(dirs, recursive = TRUE))

    observeEvent(input$run, {
      dir <- tempfile("quarto-test-")
      dir.create(dir)
      dirs <<- c(dirs, dir)
      res <- withProgress(message = "Lager test-PDF \u2026", value = 0.3, {
        quarto_test_run(dir)
      })
      result(res)
      showModal(modalDialog(
        title = "Quarto-test",
        size = "l",
        easyClose = TRUE,
        quarto_test_report(res),
        footer = tagList(
          if (res$render$ok) downloadButton(ns("pdf_render"), "PDF (quarto render)", class = "btn-primary"),
          if (res$typst$ok) downloadButton(ns("pdf_typst"), "PDF (typst compile)", class = "btn-primary"),
          if (isTRUE(res$plan$ok)) downloadButton(ns("pdf_plan"), "Referanseøkta", class = "btn-primary"),
          modalButton("Lukk")
        )
      ))
    })

    output$pdf_render <- downloadHandler(
      filename = "quarto-test-render.pdf",
      content = function(file) file.copy(result()$render$pdf, file, overwrite = TRUE)
    )
    output$pdf_typst <- downloadHandler(
      filename = "quarto-test-typst.pdf",
      content = function(file) file.copy(result()$typst$pdf, file, overwrite = TRUE)
    )
    output$pdf_plan <- downloadHandler(
      filename = "referanseokt.pdf",
      content = function(file) file.copy(result()$plan$pdf, file, overwrite = TRUE)
    )
  })
}

# Finds the Quarto program: QUARTO_PATH, PATH, the quarto package if
# installed, then the usual places on Linux servers and in RStudio/Positron.
quarto_test_bin <- function() {
  from_pkg <- if (requireNamespace("quarto", quietly = TRUE)) {
    tryCatch(as.character(quarto::quarto_path()), error = function(e) "")
  }
  cand <- c(
    Sys.getenv("QUARTO_PATH"), unname(Sys.which("quarto")), from_pkg,
    Sys.getenv("RSTUDIO_QUARTO"),
    Sys.glob("/opt/quarto/*/bin/quarto"), "/opt/quarto/bin/quarto",
    "/usr/local/bin/quarto", "/usr/lib/rstudio-server/bin/quarto/bin/quarto",
    "C:/Program Files/Quarto/bin/quarto.exe",
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe",
    file.path(Sys.getenv("LOCALAPPDATA"), "Programs/Positron/resources/app/quarto/bin/quarto.exe")
  )
  cand <- cand[!is.na(cand) & nzchar(cand)]
  cand <- cand[file.exists(cand)]
  if (length(cand)) normalizePath(cand[[1]], winslash = "/") else ""
}

# Runs a command and returns status, time and output (stdout and stderr).
quarto_test_cmd <- function(bin, args, timeout = 120) {
  t0 <- proc.time()[["elapsed"]]
  out <- tryCatch(
    suppressWarnings(system2(bin, args, stdout = TRUE, stderr = TRUE, timeout = timeout)),
    error = function(e) structure(conditionMessage(e), status = -1L)
  )
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  list(
    ok = identical(as.integer(status), 0L),
    status = status,
    secs = round(proc.time()[["elapsed"]] - t0, 1),
    out = as.character(out)
  )
}

# A pitch sketch in base graphics, in the style of the reference session.
quarto_test_sketch <- function(path) {
  args <- list(filename = path, width = 1600, height = 900, res = 200)
  if (isTRUE(capabilities("cairo"))) args$type <- "cairo"
  do.call(grDevices::png, args)
  on.exit(grDevices::dev.off())
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 40), ylim = c(0, 22.5), xaxs = "i", yaxs = "i")
  for (i in 0:7) {
    graphics::rect(i * 5, 0, i * 5 + 5, 22.5, border = NA,
                   col = if (i %% 2) "#3f8f3f" else "#4a9a4a")
  }
  graphics::rect(1, 1, 39, 21.5, border = "white", lwd = 3)
  graphics::segments(20, 1, 20, 21.5, col = "white", lwd = 3)
  graphics::points(c(3, 3, 37, 37), c(3, 19.5, 3, 19.5), pch = 24,
                   bg = "#ff8c1a", col = "#ff8c1a", cex = 2)
  graphics::arrows(8, 8, 13.3, 14.2, col = "white", lwd = 4, length = 0.12)
  graphics::arrows(14, 15, 25, 11, col = "black", lwd = 3, lty = 2, length = 0.12)
  graphics::points(c(8, 14, 26), c(8, 15, 10), pch = 21, bg = "#1f5fbf",
                   col = "white", cex = 4.5, lwd = 2)
  graphics::points(c(18, 30), c(12, 6), pch = 21, bg = "#d63a2f",
                   col = "white", cex = 4.5, lwd = 2)
  graphics::text(c(8, 14, 26), c(8, 15, 10), c("1", "2", "3"), col = "white", font = 2)
  invisible(path)
}

quarto_test_write <- function(lines, path) {
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
}

# Writes test.qmd, mal.typ, data.json and (if the PNG device works)
# skisse.png. Returns the error from the sketch, or "" if it worked.
quarto_test_inputs <- function(dir) {
  png_error <- tryCatch({
    quarto_test_sketch(file.path(dir, "skisse.png"))
    ""
  }, error = function(e) conditionMessage(e))
  img <- !nzchar(png_error)

  quarto_test_write(c(
    "---",
    "title: \"Quarto-test \u2013 \u00e6\u00f8\u00e5 \u00c6\u00d8\u00c5\"",
    "format:",
    "  typst:",
    "    papersize: a4",
    "    margin:",
    "      x: 1.5cm",
    "      y: 1.5cm",
    "---",
    "",
    "## \u00d8velse 1 \u00b7 Pasning og mottak",
    "",
    "Fokus: spille p\u00e5 lag. Gr\u00f8nn bane, bl\u00e5 og r\u00f8de spillere.",
    "",
    if (img) "![Baneskisse](skisse.png){width=100%}",
    "",
    "| Del | Tid |",
    "|---|---|",
    "| Oppvarming | 8 min |",
    "| Stasjoner (3 \u00d7 15 + bytte) | 49 min |",
    "| Avslutning | 8 min |"
  ), file.path(dir, "test.qmd"))

  quarto_test_write(
    "{\"tittel\": \"Typst-test \u2013 \u00e6\u00f8\u00e5\", \"tider\": [{\"del\": \"Oppvarming\", \"min\": 8}, {\"del\": \"Stasjoner\", \"min\": 49}, {\"del\": \"Avslutning\", \"min\": 8}]}",
    file.path(dir, "data.json")
  )

  quarto_test_write(c(
    "#set page(paper: \"a4\", margin: 1.5cm)",
    "#set text(lang: \"nb\", size: 11pt)",
    "#let data = json(\"data.json\")",
    "",
    "#text(size: 20pt, weight: \"bold\")[#data.tittel]",
    "",
    "#block(fill: rgb(\"#121212\"), inset: 6pt, radius: 3pt)[#text(fill: rgb(\"#FFC629\"), weight: \"bold\")[\u00d8VELSE 1 \u00b7 PASNING]]",
    "",
    if (img) "#image(\"skisse.png\", width: 100%)",
    "",
    "#table(columns: (1fr, auto), [*Del*], [*Tid*], ..data.tider.map(t => ([#t.del], [#str(t.min) min])).flatten())"
  ), file.path(dir, "mal.typ"))

  png_error
}

quarto_test_run <- function(dir) {
  png_error <- quarto_test_inputs(dir)
  bin <- quarto_test_bin()
  none <- list(ok = FALSE, status = NA, secs = NA, out = "Fant ikke Quarto.")
  pkgs <- c("quarto", "knitr", "rmarkdown", "ggplot2", "ragg", "jsonlite", "qrcode")
  res <- list(
    bin = bin,
    path = Sys.getenv("PATH"),
    r = R.version.string,
    os = paste(Sys.info()[["sysname"]], Sys.info()[["release"]]),
    png = if (nzchar(png_error)) paste("feil:", png_error) else
      paste0("ok (cairo: ", isTRUE(capabilities("cairo")), ")"),
    pkgs = vapply(pkgs, function(p) requireNamespace(p, quietly = TRUE), logical(1)),
    version = none, typst_version = none, render = none, typst = none
  )
  if (!nzchar(bin)) return(res)

  q <- function(p) shQuote(file.path(dir, p))
  res$version <- quarto_test_cmd(bin, "--version", timeout = 30)
  res$typst_version <- quarto_test_cmd(bin, c("typst", "--version"), timeout = 30)
  res$typst <- quarto_test_cmd(bin, c("typst", "compile", q("mal.typ"), q("typst.pdf")))
  res$typst$pdf <- file.path(dir, "typst.pdf")
  res$typst$ok <- res$typst$ok && file.exists(res$typst$pdf)
  res$render <- quarto_test_cmd(bin, c("render", q("test.qmd"), "--to", "typst"))
  res$render$pdf <- file.path(dir, "test.pdf")
  res$render$ok <- res$render$ok && file.exists(res$render$pdf)
  # T2-2: the real template, fonts and drawings, with the reference session.
  t0 <- proc.time()[["elapsed"]]
  res$plan <- tryCatch({
    ref <- jsonlite::fromJSON(app_sys("extdata", "referanse-okt.json"), simplifyVector = FALSE)
    pdf <- file.path(dir, "opplegg.pdf")
    plan_pdf(ref, pdf, footer = "G10 · Tema oktober: Samhandling – spille på lag", quarto = bin)
    list(ok = file.exists(pdf), status = 0L, secs = round(proc.time()[["elapsed"]] - t0, 1), out = character(), pdf = pdf)
  }, error = function(e) list(ok = FALSE, status = NA, secs = NA, out = conditionMessage(e)))
  res
}

quarto_test_report <- function(res) {
  status <- function(x) {
    if (isTRUE(x$ok)) paste0("OK (", x$secs, " s)") else paste0("FEIL (status ", x$status, ")")
  }
  tail_out <- function(x) {
    if (isTRUE(x$ok) || !length(x$out)) return(NULL)
    tags$pre(class = "small", style = "white-space: pre-wrap; max-height: 12rem; overflow: auto;",
             paste(utils::tail(x$out, 20), collapse = "\n"))
  }
  first <- function(x) if (length(x$out)) x$out[[1]] else ""
  pkgs <- paste0(names(res$pkgs), ifelse(res$pkgs, " \u2713", " \u2715"), collapse = ", ")
  rows <- list(
    c("Quarto", if (nzchar(res$bin)) res$bin else "ikke funnet"),
    c("Quarto-versjon", if (isTRUE(res$version$ok)) first(res$version) else status(res$version)),
    c("Typst-versjon", if (isTRUE(res$typst_version$ok)) first(res$typst_version) else status(res$typst_version)),
    c("quarto render \u2192 typst", status(res$render)),
    c("quarto typst compile", status(res$typst)),
    c("Treningsopplegg (plan_pdf)", if (is.null(res$plan)) "ikke kjørt" else status(res$plan)),
    c("PNG-skisse", res$png),
    c("R", res$r),
    c("System", res$os),
    c("Pakker", pkgs)
  )
  tagList(
    tags$table(
      class = "table table-sm",
      tags$tbody(lapply(rows, function(r) tags$tr(tags$th(r[[1]]), tags$td(r[[2]]))))
    ),
    if (!isTRUE(res$render$ok)) tagList(tags$strong("Utdata fra quarto render:"), tail_out(res$render)),
    if (!isTRUE(res$typst$ok)) tagList(tags$strong("Utdata fra typst compile:"), tail_out(res$typst)),
    if (!is.null(res$plan) && !isTRUE(res$plan$ok)) tagList(tags$strong("Feil fra plan_pdf:"), tail_out(res$plan)),
    tags$details(tags$summary("PATH"), tags$pre(class = "small", style = "white-space: pre-wrap;", res$path))
  )
}
