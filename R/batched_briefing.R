# ---------------------------------------------------------------------------
# Batched briefing generation for podcast.R
#
# Replaces:
#     input_text <- paste(blocks, collapse = "\n---\n")
#     briefing   <- chat(BRIEFING_PROMPT, input_text, max_tokens = 3500)
#
# with a two-pass map-reduce:
#   1. Compress articles into dense digest lines, a batch at a time.
#   2. Build the final briefing from the combined digests.
#
# Every article now reaches the model, rather than only whatever fitted
# inside the first MAX_INPUT_CHARS characters.
#
# Depends on chat(), BRIEFING_PROMPT and MAX_INPUT_CHARS from podcast.R.
# ---------------------------------------------------------------------------

MAX_ARTICLES_PER_BATCH <- 25

# Stay clear of the substr() truncation inside chat(). If a batch exceeded
# MAX_INPUT_CHARS it would be silently cut, which is the bug this replaces.
BATCH_CHAR_BUDGET <- function() {
  cap <- MAX_INPUT_CHARS
  if (is.na(cap) || cap < 5000) {
    stop("MAX_INPUT_CHARS is unset, NA or implausibly small: ", cap)
  }
  floor(cap * 0.65)
}

DIGEST_PROMPT <- "ROLE
You are compressing healthcare and life sciences news articles ahead of a briefing for health system leaders, investors and policymakers.

TASK
For each article below, write one or two lines capturing:
- The concrete event: a regulatory decision, product launch, pilot, funding round, acquisition, contract award, policy milestone, or a published finding
- Every named entity involved: companies, health systems, regulators, government bodies
- The publication the story comes from
- Any statistics or data points
- Where the event is happening (the country of the event, not the country of the publication)

RULES
- Omit any article with no concrete event. Do not pad.
- Omit duplicates. Where several articles cover the same event, keep one line.
- Preserve names, numbers and dates exactly. Do not generalise them away.
- No URLs, no asterisks, no preamble, no closing remarks.
- Plain lines only. This is intermediate working, not finished copy."

# Group blocks into batches bounded by both character count and item count.
chunk_texts <- function(texts, max_chars, max_items = MAX_ARTICLES_PER_BATCH) {

  texts <- texts[!is.na(texts) & nzchar(trimws(texts))]
  if (length(texts) == 0) return(list())

  # Guard against one oversized article filling a batch on its own.
  texts <- vapply(texts, function(t) substr(t, 1, max_chars), character(1),
                  USE.NAMES = FALSE)

  batches       <- list()
  current       <- character(0)
  current_chars <- 0

  for (txt in texts) {
    n <- nchar(txt)
    if (length(current) > 0 &&
        (current_chars + n > max_chars || length(current) >= max_items)) {
      batches[[length(batches) + 1]] <- current
      current       <- character(0)
      current_chars <- 0
    }
    current       <- c(current, txt)
    current_chars <- current_chars + n
  }

  if (length(current) > 0) batches[[length(batches) + 1]] <- current
  batches
}

# Run one batch, returning NA rather than halting the whole episode.
digest_batch <- function(batch, index, total) {
  message(sprintf("  Batch %d of %d: %d article(s), %d chars",
                  index, total, length(batch), sum(nchar(batch))))

  tryCatch(
    chat(DIGEST_PROMPT, paste(batch, collapse = "\n---\n"), max_tokens = 2000),
    error = function(e) {
      warning(sprintf("Batch %d failed: %s", index, conditionMessage(e)),
              call. = FALSE, immediate. = TRUE)
      NA_character_
    }
  )
}

digest_all <- function(texts, max_chars) {
  batches <- chunk_texts(texts, max_chars)
  if (length(batches) == 0) return(character(0))

  out <- vapply(seq_along(batches), function(i) {
    result <- digest_batch(batches[[i]], i, length(batches))
    if (i < length(batches)) Sys.sleep(1)
    result
  }, character(1))

  out[!is.na(out)]
}

build_briefing <- function(blocks) {

  max_chars <- BATCH_CHAR_BUDGET()
  total_chars <- sum(nchar(blocks))

  message(sprintf("Building briefing from %d article(s), %d chars total.",
                  length(blocks), total_chars))
  message(sprintf("Batch budget %d chars (chat() truncates at %d).",
                  max_chars, MAX_INPUT_CHARS))

  digests <- digest_all(blocks, max_chars)
  if (length(digests) == 0) {
    stop("Every digest batch failed. No briefing produced.")
  }

  combined <- paste(digests, collapse = "\n")
  message(sprintf("Pass 1: %d digest(s), %d chars.",
                  length(digests), nchar(combined)))

  # Fold again if the combined digest is still too large for one call.
  # The length comparison prevents looping when no reduction is possible.
  while (nchar(combined) > max_chars && length(digests) > 1) {
    message("Digest still over budget, folding again.")
    folded <- digest_all(digests, max_chars)
    if (length(folded) == 0 || length(folded) >= length(digests)) break
    digests  <- folded
    combined <- paste(digests, collapse = "\n")
    message(sprintf("Folded to %d digest(s), %d chars.",
                    length(digests), nchar(combined)))
  }

  if (nchar(combined) > MAX_INPUT_CHARS) {
    warning("Combined digest still exceeds MAX_INPUT_CHARS and will be truncated.",
            call. = FALSE, immediate. = TRUE)
  }

  chat(BRIEFING_PROMPT, combined, max_tokens = 3500)
}
