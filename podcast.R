# podcast.R
# Replaces Zap 3: select enriched, unreleased articles -> build a grouped
# briefing -> rewrite for audio -> ElevenLabs TTS -> save MP3 + transcript into
# the repo's episodes/ folder -> mark podcast_released = TRUE.

suppressMessages({
  library(googlesheets4)
  library(httr2)
})

# ---- Config --------------------------------------------------------------
SHEET_ID          <- Sys.getenv("SHEET_ID")
WORKSHEET         <- Sys.getenv("WORKSHEET", "Sheet1")
ANTHROPIC_API_KEY <- Sys.getenv("ANTHROPIC_API_KEY")
LLM_MODEL         <- Sys.getenv("LLM_MODEL", "claude-haiku-4-5")
ELEVEN_API_KEY    <- Sys.getenv("ELEVENLABS_API_KEY")
ELEVEN_VOICE_ID   <- Sys.getenv("ELEVENLABS_VOICE_ID", "1hlpeD1ydbI2ow0Tt3EW")
ELEVEN_MODEL      <- Sys.getenv("ELEVENLABS_MODEL", "eleven_multilingual_v2")
MAX_INPUT_CHARS   <- as.integer(Sys.getenv("MAX_INPUT_CHARS", "60000"))

stopifnot("SHEET_ID is not set"           = nzchar(SHEET_ID),
          "ANTHROPIC_API_KEY is not set"  = nzchar(ANTHROPIC_API_KEY),
          "ELEVENLABS_API_KEY is not set" = nzchar(ELEVEN_API_KEY))

# ---- Auth (service account, for Sheets only) -----------------------------
sa_json <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON")
stopifnot("GOOGLE_SERVICE_ACCOUNT_JSON is not set" = nzchar(sa_json))
sa_file <- tempfile(fileext = ".json")
writeLines(sa_json, sa_file)
gs4_auth(path = sa_file)

a1col <- function(n) {
  s <- ""
  while (n > 0) { r <- (n - 1) %% 26; s <- paste0(LETTERS[r + 1], s); n <- (n - 1) %/% 26 }
  s
}

chat <- function(system_prompt, user_text, max_tokens = 3000) {
  user_text <- substr(user_text, 1, MAX_INPUT_CHARS)
  req <- request("https://api.anthropic.com/v1/messages") |>
    req_headers(`x-api-key` = ANTHROPIC_API_KEY,
                `anthropic-version` = "2023-06-01") |>
    req_body_json(list(
      model = LLM_MODEL,
      max_tokens = max_tokens,
      temperature = 0.3,
      system = system_prompt,
      messages = list(
        list(role = "user", content = user_text)
      )
    )) |>
    req_timeout(120)
  out <- resp_body_json(req_perform(req))
  out$content[[1]]$text
}

# ---- Prompts (ported from Zap 3 steps 4 and 5) ---------------------------
BRIEFING_PROMPT <- "ROLE
You are preparing a healthcare and life sciences news briefing for a highly sophisticated professional audience (health system leaders, investors, policymakers, digital health executives). This audience expects specifics, not generalities.

INPUT
Below is a collection of healthcare and life sciences news articles from the past week.

TASK
Produce a spoken-news-ready briefing with the following strict rules.

STRUCTURE
- Group stories by: UK, Global, Lifesciences (stories grouped by where the event is happening, NOT where it is reported from)
- Use concise bullet points only
- No URLs or citations in the output
- No asterisks in the text

CONTENT REQUIREMENTS (MANDATORY)
Every bullet point must include at least one named entity, such as:
- A company (e.g. Philips, Epic, Babylon)
- A health system (e.g. NHS England, Kaiser Permanente)
- A regulator or government body (e.g. NHS England, FDA, CMS)

On every bullet point note one of the publications the news is coming from.

Every bullet must describe a concrete event from the past week, such as:
- A regulatory decision or reform
- A product launch or pilot
- A finding like a product creating a certain amount of value
- A funding round, acquisition, or contract award
- A policy announcement or implementation milestone
Always add statistics or relevant data points where available.

PRIORITY SIGNALS
Prioritise news involving:
- Healthtech: Epic, Oracle Health, Cerner, Meditech, Dedalus, EMIS, Optum, TPP/SystmOne, Intersystems, Infor, Rhapsody, Mirth, Doccla
- Health systems: Ramsay, HCA, Kaiser Permanente, Bupa, IHH, Mayo Clinic, Cleveland Clinic, Nuffield, Spire, Blue Cross
- Life sciences: AstraZeneca, Pfizer, Roche, Novartis, Johnson & Johnson, Merck & Co., Sanofi, GSK, Eli Lilly, Bayer,, Amgen, Moderna, Novo Nordisk, Boehringer Ingelheim

STYLE
Avoid abstract language. Do not use phrases like 'the sector', 'stakeholders', 'experts', 'growing trend', 'health systems'."

AUDIO_PROMPT <- "You are a professional news narrator creating a spoken daily briefing.

TASK
Rewrite the following healthcare and life sciences news summary for AUDIO delivery.

RULES
- Soft, calm, professional tone
- Female voice style
- Slightly slower pacing than normal speech
- Use smooth, natural pauses only where explicitly indicated
- Do not insert breaths, filler sounds, or vocalisations
- Maintain a clean, continuous spoken flow

STRUCTURE
- Keep dot-point structure, but rewrite bullets as short spoken sentences
- Use spoken transitions exactly as written: 'In the UK.' 'Turning to Germany.' 'In the United States.' 'Globally.'
- After each transition, continue speaking without a long pause
- Do not read URLs aloud

TIMING
- Total length approximately 15 minutes when spoken

FORMATTING (IMPORTANT)
- Do not leave blank lines between paragraphs
- Use a single line break only where a pause is desired
- Never use bullet symbols; rewrite bullets as sentences separated by commas or short full stops"

# ---- Select unreleased, enriched rows ------------------------------------
dat <- read_sheet(SHEET_ID, sheet = WORKSHEET, col_types = "c")
dat$.row <- seq_len(nrow(dat)) + 1L

hdr <- names(dat)
col_released <- a1col(match("podcast_released", hdr))
stopifnot("Sheet needs a 'podcast_released' header" = !is.na(col_released))

ready <- dat[(!is.na(dat$enriched) & dat$enriched == "TRUE") &
             (is.na(dat$podcast_released) | dat$podcast_released != "TRUE"), , drop = FALSE]

out_dir <- "episodes"
dir.create(out_dir, showWarnings = FALSE)

`%||%` <- function(a, b) if (is.null(a) || is.na(a) || !nzchar(a)) b else a

if (nrow(ready) == 0) {
  message("No new articles ready. Refreshing the feed only, no audio generated.")
} else {
  # Prefer the rich AI summary, fall back to the RSS content extract.
  pick_text <- function(r) {
    s <- r$ai_summary
    if (is.na(s) || !nzchar(s)) s <- r$content
    s %||% ""
  }

  blocks <- vapply(seq_len(nrow(ready)), function(i) {
    r <- ready[i, ]
    sprintf("TITLE: %s\nSOURCE: %s\nSUMMARY: %s\n",
            r$title %||% "", r$link %||% "", pick_text(r))
  }, character(1))

  input_text <- paste(blocks, collapse = "\n---\n")

  # ---- Generate briefing, then audio script ------------------------------
  message(sprintf("Building briefing from %d article(s).", nrow(ready)))
  briefing <- chat(BRIEFING_PROMPT, input_text, max_tokens = 3500)
  audio_script <- chat(AUDIO_PROMPT, briefing, max_tokens = 4000)

  # ---- ElevenLabs text to speech -----------------------------------------
  stamp <- format(Sys.time(), "%Y-%m-%d %H%M", tz = "Europe/London")

  mp3_path <- file.path(out_dir, sprintf("Health Daily Download - %s.mp3", stamp))
  tts <- request(sprintf("https://api.elevenlabs.io/v1/text-to-speech/%s", ELEVEN_VOICE_ID)) |>
    req_headers(`xi-api-key` = ELEVEN_API_KEY, `Content-Type` = "application/json") |>
    req_body_json(list(
      text = audio_script,
      model_id = ELEVEN_MODEL,
      voice_settings = list(stability = 0.5, similarity_boost = 0.6)
    )) |>
    req_timeout(300) |>
    req_perform()
  writeBin(resp_body_raw(tts), mp3_path)

  # ---- Save transcript next to the audio ---------------------------------
  txt_path <- file.path(out_dir, sprintf("Health Daily Download (read) - %s.txt", stamp))
  writeLines(briefing, txt_path)
  message(sprintf("Saved episode: %s", mp3_path))
  message(sprintf("Saved transcript: %s", txt_path))

  # ---- Mark rows as released ---------------------------------------------
  for (rn in ready$.row) {
    range_write(SHEET_ID, sheet = WORKSHEET,
                data = data.frame(x = "TRUE"),
                range = sprintf("%s%d", col_released, rn), col_names = FALSE)
    Sys.sleep(0.5)
  }

  message(sprintf("Published podcast and marked %d row(s) released.", nrow(ready)))
}

# ---- Build / refresh the podcast RSS feed --------------------------------
PODCAST_TITLE <- Sys.getenv("PODCAST_TITLE", "Health Daily Download")
PODCAST_DESC  <- Sys.getenv("PODCAST_DESCRIPTION",
                            "A weekly spoken briefing on healthcare and life sciences news.")
COVER_IMAGE   <- Sys.getenv("COVER_IMAGE_URL", "")

site_base <- Sys.getenv("SITE_BASE_URL", "")
if (!nzchar(site_base)) {
  repo <- Sys.getenv("GITHUB_REPOSITORY")          # "owner/name", set by Actions
  if (nzchar(repo)) {
    parts <- strsplit(repo, "/", fixed = TRUE)[[1]]
    site_base <- sprintf("https://%s.github.io/%s", parts[1], parts[2])
  }
}
site_base <- sub("/$", "", site_base)

xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  gsub(">", "&gt;",  x, fixed = TRUE)
}

make_item <- function(path) {
  fname <- basename(path)
  size  <- file.info(path)$size
  m <- regmatches(fname, regexpr("\\d{4}-\\d{2}-\\d{2} \\d{4}", fname))
  when <- if (length(m) == 1) {
    as.POSIXct(m, format = "%Y-%m-%d %H%M", tz = "Europe/London")
  } else file.info(path)$mtime
  pub <- format(when, "%a, %d %b %Y %H:%M:%S %z", tz = "GMT")
  url <- sprintf("%s/%s/%s", site_base, out_dir, URLencode(fname, reserved = TRUE))
  paste0(
    "    <item>\n",
    "      <title>", xml_escape(tools::file_path_sans_ext(fname)), "</title>\n",
    "      <pubDate>", pub, "</pubDate>\n",
    "      <enclosure url=\"", url, "\" length=\"", size, "\" type=\"audio/mpeg\"/>\n",
    "      <guid isPermaLink=\"false\">", xml_escape(fname), "</guid>\n",
    "      <itunes:explicit>false</itunes:explicit>\n",
    "    </item>"
  )
}

mp3s  <- sort(list.files(out_dir, pattern = "\\.mp3$", full.names = TRUE), decreasing = TRUE)
items <- if (length(mp3s)) paste(vapply(mp3s, make_item, character(1)), collapse = "\n") else ""
image_tag <- if (nzchar(COVER_IMAGE)) sprintf("    <itunes:image href=\"%s\"/>\n", COVER_IMAGE) else ""

feed <- paste0(
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
  "<rss version=\"2.0\" xmlns:itunes=\"http://www.itunes.com/dtds/podcast-1.0.dtd\">\n",
  "  <channel>\n",
  "    <title>", xml_escape(PODCAST_TITLE), "</title>\n",
  "    <link>", site_base, "</link>\n",
  "    <description>", xml_escape(PODCAST_DESC), "</description>\n",
  "    <language>en-gb</language>\n",
  "    <itunes:author>", xml_escape(PODCAST_TITLE), "</itunes:author>\n",
  "    <itunes:explicit>false</itunes:explicit>\n",
  image_tag,
  items, if (nzchar(items)) "\n" else "",
  "  </channel>\n",
  "</rss>\n"
)

writeLines(feed, "feed.xml")
message(sprintf("Feed written with %d episode(s): %s/feed.xml", length(mp3s), site_base))
