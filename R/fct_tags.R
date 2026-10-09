#' Member tags: checks, suggestions and display
#'
#' Tags are free text set by trainers (e.g. "Keeper", "Kaptein"). They belong
#' to the main group, so a tag shows in every subgroup. They are stored with
#' Spond ids only (see `ds_add_tag()`).
#'
#' Tags must not hold health or other sensitive information. The app reminds
#' the trainer, and in addition refuses tags that contain obvious words for
#' it (`tag_problem()`). The word list catches the common cases; it is a help,
#' not a guarantee, so the routine still matters.
#'
#' @name fct_tags
#' @noRd
NULL

#' Neutral tags offered with one click (positions and roles)
#' @noRd
tag_suggestions <- function() {
  x <- tryCatch(get_golem_config("tag_suggestions"), error = function(e) NULL)
  if (is.null(x)) c("Keeper", "Kaptein", "Forsvar", "Midtbane", "Angrep", "Ny") else unlist(x)
}

#' Trim and collapse inner whitespace
#' @noRd
tag_clean <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return("")
  gsub("\\s+", " ", trimws(as.character(x[1])))
}

# Words that point to health or other special categories of personal data
# (GDPR art. 9), and to things like child welfare. Matched at the start of a
# word, case-insensitive: "\\bskad" catches "skadet", "skade", "skadeavbrekk".
tag_sensitive_patterns <- function() {
  c("\\bskad", "\\bsyk(e|t|dom|dommen|meldt|emeldt)?\\b", "\\bdiagnos", "\\ballergi", "\\bastma", "\\badhd\\b",
    "\\bautis", "\\bepilep", "\\bdiabet", "\\bmedisin", "\\bhjernerystelse", "\\bbrudd\\b",
    "\\bbrukket", "\\boperer", "\\bsmert", "\\bdysleksi", "\\bspiseforstyrr", "\\bangst",
    "\\bdepre", "\\bpsyk", "\\bgravid", "\\bfysioterap", "\\brehab", "\\bsykehus",
    "\\bbarnevern", "\\brelig", "\\betnisk", "\\bfunksjonshem")
}

#' Why a tag is not allowed, or NULL if it is fine
#' @noRd
tag_problem <- function(tag) {
  tag <- tag_clean(tag)
  if (!nzchar(tag)) return("Skriv inn en tag.")
  if (nchar(tag) > 40) return("En tag kan ha maks 40 tegn.")
  if (any(vapply(tag_sensitive_patterns(), grepl, logical(1), x = tag, ignore.case = TRUE, perl = TRUE))) {
    return("Tagger skal ikke inneholde helseopplysninger eller andre sensitive opplysninger.")
  }
  if (grepl("@", tag, fixed = TRUE) || grepl("[0-9][0-9 ]{6,}[0-9]", tag)) {
    return("Tagger skal ikke inneholde kontaktinformasjon.")
  }
  NULL
}

#' Use the spelling already in use in the group ("keeper" -> "Keeper")
#' @noRd
tag_canonical <- function(tag, existing) {
  hit <- existing[tolower(existing) == tolower(tag)]
  if (length(hit)) hit[1] else tag
}

#' Empty tag table
#' @noRd
tags_empty <- function() data.frame(member_id = character(), tag = character(), stringsAsFactors = FALSE)

#' Tags of one member, in alphabetical order
#' @noRd
tags_for <- function(tags, member_id) {
  if (is.null(tags) || nrow(tags) == 0) return(character())
  sort(tags$tag[tags$member_id == member_id], method = "radix")
}

#' Suggestions for one member: the fixed list first, then tags used in the
#' group (most used first), without the ones the member already has
#' @noRd
tag_suggestions_for <- function(tags, member_id, fixed = tag_suggestions()) {
  used <- if (nrow(tags)) names(sort(table(tags$tag), decreasing = TRUE)) else character()
  all <- unique(c(fixed, used))
  have <- tolower(tags_for(tags, member_id))
  all[!tolower(all) %in% have]
}

#' A member chip with the member's tags. Clicking it opens the tag editor.
#'
#' @param open_input Shiny input id that receives the member id (from
#'   `mod_tags_server()`), or NULL for a plain, non-clickable chip.
#' @param note Optional short warning shown on the chip (e.g. "Kommer ikke").
#' @noRd
member_chip <- function(member_id, name, member_tags = character(), open_input = NULL, note = NULL) {
  mini <- tagList(lapply(member_tags, function(t) span(class = "sn-minitag", t)),
                  if (!is.null(note)) span(class = "sn-note", note))
  if (is.null(open_input)) return(span(class = "sn-chip", name, mini))
  tags$button(
    type = "button", class = "sn-chip sn-chip-btn",
    `data-sn-input` = open_input, `data-sn-value` = member_id,
    `aria-label` = paste0(name, if (!is.null(note)) paste0(" (", note, ")"),
                          if (length(member_tags)) paste0(", tagger: ", paste(member_tags, collapse = ", ")),
                          ". Endre tagger."),
    name, mini
  )
}

#' A <details> section that remembers whether it was open across re-renders
#'
#' The browser reports open/closed to `input[[input_id]]` (see www/sn.js).
#' @param open Current value of that input (read with `isolate()`), or NULL.
#' @param default Open or closed the first time.
#' @noRd
remembered_details <- function(input_id, open, default, summary, ..., class = NULL) {
  is_open <- if (is.null(open)) default else isTRUE(open)
  tags$details(class = class, open = if (is_open) NA, `data-sn-toggle` = input_id,
               tags$summary(summary), ...)
}

#' Chips for several members; clickable with tags when `tagger` (the value of
#' `mod_tags_server()`) is given, plain otherwise
#' @noRd
#' @param clickable Logical, recycled: FALSE for members who cannot be tagged
#'   (e.g. former members).
#' @param kinds Optional member kinds ("coach", "player", "adult"). Only
#'   players get tags; trainers get a black chip.
member_chips <- function(ids, names, tagger = NULL, clickable = TRUE, kinds = NULL) {
  clickable <- rep_len(clickable, length(ids))
  kinds <- if (is.null(kinds)) rep("player", length(ids)) else ifelse(is.na(kinds), "player", kinds)
  div(class = "sn-chips", lapply(seq_along(ids), function(i) {
    if (identical(kinds[i], "coach")) return(span(class = "sn-chip sn-chip-coach", names[i]))
    if (is.null(tagger) || !clickable[i] || !identical(kinds[i], "player")) member_chip(ids[i], names[i])
    else tagger$chip(ids[i], names[i])
  }))
}
