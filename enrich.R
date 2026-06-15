# enrich.R
# Replaces Zap 1B: find un-enriched rows -> fetch full article -> AI summarise
# -> write ai_summary back and set enriched = TRUE.

suppressMessages({
  library(googlesheets4)
  library(httr2)
  library(xml2)
})

# ---- Config --------------------------------------------------------------
SHEET_ID          <- Sys.getenv("SHEET_ID")
WORKSHEET         <- Sys.getenv("WORKSHEET", "Sheet1")
RAPIDAPI_KEY      <- Sys.getenv("RAPIDAPI_KEY")
ANTHROPIC_API_KEY <- Sys.getenv("ANTHROPIC_API_KEY")
LLM_MODEL         <- Sys.getenv("LLM_MODEL", "claude-haiku-4-5")
MAX_PER_RUN       <- as.integer(Sys.getenv("MAX_PER_RUN", "25"))
MAX_INPUT_CHARS   <- as.integer(Sys.getenv("MAX_INPUT_CHARS", "12000"))

stopifnot("SHEET_ID is not set"          = nzchar(SHEET_ID),
          "ANTHROPIC_API_KEY is not set" = nzchar(ANTHROPIC_API_KEY))

# ---- Auth ----------------------------------------------------------------
sa_json <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON")
stopifnot("GOOGLE_SERVICE_ACCOUNT_JSON is not set" = nzchar(sa_json))
sa_file <- tempfile(fileext = ".json")
writeLines(sa_json, sa_file)
gs4_auth(path = sa_file)

# ---- Helpers -------------------------------------------------------------
strip_html <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return("")
  txt <- tryCatch(xml_text(read_html(paste0("<div>", x, "</div>"))),
                  error = function(e) x)
  trimws(gsub("\\s+", " ", txt))
}

a1col <- function(n) {
  s <- ""
  while (n > 0) { r <- (n - 1) %% 26; s <- paste0(LETTERS[r + 1], s); n <- (n - 1) %/% 26 }
  s
}

SUMMARY_PROMPT <- "ROLE
You are extracting key information from a healthcare/life sciences news article to prepare it for inclusion in a professional news briefing.

INPUT
Below is a single healthcare or life sciences news article.

TASK
Extract and summarise the article following these strict rules.

CONTENT REQUIREMENTS (MANDATORY)
Your summary MUST include:
- Named entities: companies, health systems, regulators, government bodies, or named individuals
- The concrete event: regulatory decision, product launch, pilot, funding round, acquisition, contract award, policy announcement, or implementation milestone
- Geographic location: where the event is happening (UK, Europe, US, Global)
- The publication source
- Any statistics or data points mentioned
- One sentence explaining why this matters (max 20 words)

PRIORITY SIGNALS
Flag if the article mentions:
- Healthtech: Epic, Oracle Health, Cerner, Meditech, Dedalus, EMIS, Optum, TPP/SystmOne, Intersystems, Infor, Rhapsody, Mirth, Doccla
- Health systems: Ramsay, HCA, Kaiser Permanente, Bupa, IHH, Mayo Clinic, Cleveland Clinic, Nuffield, Spire, Helios, Asklepios, Blue Cross
- Life sciences: AstraZeneca, Pfizer, Roche, Novartis, J&J, Merck, Sanofi, GSK, Eli Lilly, BMS, Bayer, Takeda, Amgen, Moderna, Novo Nordisk, Boehringer Ingelheim
- Topics: digital health, health tech, AI in healthcare, health data infrastructure, virtual care/wards

STYLE
- Be specific, not abstract
- Avoid phrases like 'the sector', 'stakeholders', 'experts', 'growing trend'
- Answer: who did what, under which framework, why does this matter now
- Professional, analytical tone
- 300 words maximum"

# ---- Full-text fetch (RapidAPI Full-Text RSS) ----------------------------
fetch_fulltext <- function(link) {
  if (is.na(link) || !nzchar(link) || !nzchar(RAPIDAPI_KEY)) return(NA_character_)
  req <- request("https://full-text-rss.p.rapidapi.com/extract.php") |>
    req_url_query(url = link, use_latest_readability = 1, links = "preserve",
                  images = 0, xss = 1, lang = 2, content = 1) |>
    req_headers(`x-rapidapi-host` = "full-text-rss.p.rapidapi.com",
                `x-rapidapi-key`  = RAPIDAPI_KEY) |>
    req_method("POST") |>
    req_timeout(45) |>
    req_error(is_error = function(resp) FALSE)

  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  if (is.null(resp) || resp_status(resp) >= 400) return(NA_character_)

  parsed <- tryCatch(resp_body_json(resp), error = function(e) NULL)
  raw_content <- NA_character_
  if (!is.null(parsed)) {
    if (!is.null(parsed$items) && length(parsed$items) > 0 &&
        !is.null(parsed$items[[1]]$content)) {
      raw_content <- parsed$items[[1]]$content
    } else if (!is.null(parsed$content)) {
      raw_content <- parsed$content
    }
  }
  if (is.na(raw_content)) raw_content <- tryCatch(resp_body_string(resp),
                                                  error = function(e) NA_character_)
  if (is.na(raw_content)) return(NA_character_)
  strip_html(raw_content)
}

# ---- LLM summarise -------------------------------------------------------
summarise_article <- function(article_text) {
  article_text <- substr(article_text, 1, MAX_INPUT_CHARS)
  req <- request("https://api.anthropic.com/v1/messages") |>
    req_headers(`x-api-key` = ANTHROPIC_API_KEY,
                `anthropic-version` = "2023-06-01") |>
    req_body_json(list(
      model = LLM_MODEL,
      max_tokens = 900,
      temperature = 0.2,
      system = SUMMARY_PROMPT,
      messages = list(
        list(role = "user", content = article_text)
      )
    )) |>
    req_timeout(90)
  out <- resp_body_json(req_perform(req))
  out$content[[1]]$text
}

# ---- Main ----------------------------------------------------------------
dat <- read_sheet(SHEET_ID, sheet = WORKSHEET, col_types = "c")
if (nrow(dat) == 0) { message("Sheet is empty."); quit(save = "no", status = 0) }

dat$.row <- seq_len(nrow(dat)) + 1L   # +1 for the header row

hdr <- names(dat)
col_enriched <- a1col(match("enriched",   hdr))
col_summary  <- a1col(match("ai_summary", hdr))
stopifnot("Sheet needs 'enriched' and 'ai_summary' headers" =
            !is.na(col_enriched) && !is.na(col_summary))

todo <- dat[is.na(dat$enriched) | dat$enriched != "TRUE", , drop = FALSE]
todo <- head(todo, MAX_PER_RUN)

if (nrow(todo) == 0) { message("Nothing to enrich."); quit(save = "no", status = 0) }

done <- 0L
for (i in seq_len(nrow(todo))) {
  row  <- todo[i, ]
  full <- fetch_fulltext(row$link)
  text_in <- if (is.na(full) || !nzchar(full)) row$content else full
  if (is.na(text_in) || !nzchar(text_in)) next

  summary <- tryCatch(summarise_article(text_in), error = function(e) NA_character_)
  if (is.na(summary) || !nzchar(summary)) {
    message(sprintf("Skipped row %d (no summary).", row$.row)); next
  }

  range_write(SHEET_ID, sheet = WORKSHEET,
              data = data.frame(x = summary),
              range = sprintf("%s%d", col_summary, row$.row), col_names = FALSE)
  range_write(SHEET_ID, sheet = WORKSHEET,
              data = data.frame(x = "TRUE"),
              range = sprintf("%s%d", col_enriched, row$.row), col_names = FALSE)
  done <- done + 1L
  Sys.sleep(1)
}

message(sprintf("Enriched %d row(s).", done))
