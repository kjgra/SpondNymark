# Fakes for the Claude API (used by test-fct_ai.R and test-mod_plans.R).

fake_answer <- function(text, status = 200L, stop_reason = "end_turn",
                        usage = list(input_tokens = 1200, output_tokens = 5000,
                                     cache_read_input_tokens = 0, cache_creation_input_tokens = 7000)) {
  list(status = status, body = list(content = list(list(type = "text", text = text)),
                                    stop_reason = stop_reason, usage = usage))
}

example_text <- function(modify = identity) {
  jsonlite::toJSON(modify(ai_example_answer()), auto_unbox = TRUE, null = "null")
}
