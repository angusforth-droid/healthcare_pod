# ---------------------------------------------------------------------------
# Batched briefing generation
#
# Replaces the single call:
#     chat(BRIEFING_PROMPT, input_text, max_tokens = 3500)
#
# with a two-pass map-reduce:
#   1. Condense articles into digests, a batch at a time.
#   2. Synthesise the final briefing from the combined digests.
#
# Assumes an existing chat(prompt, input_text, max_tokens) helper and a
# BRIEFING_PROMPT constant, both as used in podcast.R.
# ---------------------------------------------------------------------------

MAX_CHARS_PER_BATCH    <- 40000  # roughly 10k tokens of input per call
MAX_ARTICLES_PER_BATCH <- 25

DIGEST_PROMPT <- paste(
  "You are condensing healthcare news articles for a weekly audio briefing.",
  "For each article below, return at most three short bullet points covering:",
  "the substantive development, any named organisations or people, and why it",
  "matters to an NHS or health tech audience.",
  "Drop anything promotional, speculative, or duplicated across articles.",
  "If an article contains no concrete development, omit it entirely.",
  "Return plain text bullets only, with no preamble or closing remarks.",
  sep = " "
)

# Group texts into batches bounded by both character count and item count.
chunk_texts <- function(texts,
                        max_chars = MAX_CHARS_PER_BATCH,
                        max_items = MAX_ARTICLES_PER_BATCH) {

  texts <- texts[!is.na(texts) & nzchar(trimws(texts))]
  if (length(texts) == 0) return(list())

  # Guard against a single oversized article blowing the batch on its own.
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

# Run one batch through the model, returning NA rather than halting on failure.
digest_batch <- function(batch, index, total) {
  message(sprintf("Digesting batch %d of %d (%d item(s), %d chars)...",
                  index, total, length(batch), sum(nchar(batch))))

  input_text <- paste(batch, collapse = "\n\n---\n\n")

  tryCatch(
    chat(DIGEST_PROMPT, input_text, max_tokens = 1500),
    error = function(e) {
      warning(sprintf("Batch %d failed: %s", index, conditionMessage(e)),
              call. = FALSE)
      NA_character_
    }
  )
}

digest_all <- function(texts) {
  batches <- chunk_texts(texts)
  if (length(batches) == 0) return(character(0))

  out <- vapply(seq_along(batches), function(i) {
    result <- digest_batch(batches[[i]], i, length(batches))
    if (i < length(batches)) Sys.sleep(1)  # be polite between calls
    result
  }, character(1))

  out[!is.na(out)]
}

build_briefing <- function(article_texts) {

  message(sprintf("Building briefing from %d article(s).", length(article_texts)))

  digests <- digest_all(article_texts)
  if (length(digests) == 0) {
    stop("All digest batches failed; nothing to build a briefing from.")
  }

  combined <- paste(digests, collapse = "\n\n")
  message(sprintf("First pass produced %d digest(s), %d chars.",
                  length(digests), nchar(combined)))

  # If the combined digest is still too large, fold it again.
  # The length check prevents looping when no further reduction is possible.
  while (nchar(combined) > MAX_CHARS_PER_BATCH && length(digests) > 1) {
    message("Digest still large, folding again.")
    folded <- digest_all(digests)
    if (length(folded) == 0 || length(folded) >= length(digests)) break
    digests  <- folded
    combined <- paste(digests, collapse = "\n\n")
    message(sprintf("Folded to %d digest(s), %d chars.",
                    length(digests), nchar(combined)))
  }

  chat(BRIEFING_PROMPT, combined, max_tokens = 3500)
}
