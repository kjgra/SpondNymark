# Prøv KI-opplegget med et ekte kall, uten appen og uten database (T3a).
#
# Bruk: les inst/ai/system.md først. Kjør så skriptet i RStudio/Positron i
# prosjektmappen. Det bruker ANTHROPIC_API_KEY fra .Renviron, viser anslått
# pris, gjør ett kall, viser faktisk pris og åpner PDF-en.
# Kostnad: ca. 4–5 øre med Haiku, ca. 1 kr med Sonnet.
# Ingenting lagres i databasen, og ingen navn sendes.

pkgload::load_all(quiet = TRUE)

modell <- "claude-haiku-5-5"   # eller "claude-sonnet-5-5"

bestilling <- list(
  start = as.POSIXct("2026-10-15 17:30", tz = "Europe/Oslo"),
  minutes = 65,
  theme = "Samhandling – spille på lag",
  theme_description = "",
  team = list(age_group = "Gutter 10 år, 7er-fotball", pitch = "Halv 11er-bane", equipment = "Kjegler, vester, småmål",
              principles = ""),
  n_players = 24,
  group_sizes = c(8L, 8L, 8L),
  wish = "Laget kommer ikke hjem bak ballen i forsvar, og ballfører har få pasningsalternativer.",
  max_new = 3L
)

if (!nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) stop("ANTHROPIC_API_KEY mangler i .Renviron. Start R på nytt etter endring.")

prep <- list(model = modell, settings = ai_settings_default(), bank = NULL, ctx = bestilling,
             body = ai_request_body(modell, ai_catalogue(NULL), ai_order_text(bestilling)))

est <- ai_estimate(prep)
if (is.null(est)) stop("Tellingen feilet. Sjekk nøkkelen og nettet.")
message("Anslått: ", est$input_tokens, " tokens inn, ", est$output_tokens, " ut, ", est$text)

sent <- ai_send(prep)
r <- ai_read_response(sent$res)
if (!r$ok) stop(r$message, " (", r$error_code, ")")
cost <- ai_cost(r$usage, modell)
message(sprintf("Faktisk: %s tokens inn, %s ut, %s skrevet til og %s lest fra hurtigbuffer, %s, %.0f s",
                r$usage$input_tokens, r$usage$output_tokens, r$usage$cache_write_tokens, r$usage$cache_read_tokens,
                ai_format_nok(cost, prep$settings$usd_nok), sent$duration_ms / 1000))

res <- ai_plan_from_answer(r$text, NULL, bestilling, bestilling$max_new)
if (length(res$warnings)) message("Merknader:\n- ", paste(res$warnings, collapse = "\n- "))
for (i in seq_along(res$plan$ovelser)) {
  probs <- if (is.null(res$plan$ovelser[[i]]$tegning)) "ingen tegning" else {
    nrow(drawing_check(res$plan$ovelser[[i]]$tegning))
  }
  message("Øvelse ", i, ": ", res$plan$ovelser[[i]]$navn, " (kollisjoner etter retting: ", probs, ")")
}

ut <- file.path(tempdir(), paste0("ki-proeve-", modell, ".pdf"))
plan_pdf(res$plan, ut, info = "KI-prøve")
writeLines(r$text, sub("\\.pdf$", ".json", ut))
message("PDF: ", ut)
utils::browseURL(ut)
