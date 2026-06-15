# ingest.R
# Replaces Zap 1: RSS -> clean -> dedupe -> append to Google Sheets.
# Runs on a schedule via GitHub Actions.

suppressMessages({
  library(googlesheets4)
  library(httr2)
  library(xml2)
})

# ---- Config (all from env / GitHub Secrets) -------------------------------
FEED_URL          <- Sys.getenv("FEED_URL", "https://rss.app/feeds/_PvHMa6fBLpsfJGK4.xml")
SHEET_ID          <- Sys.getenv("SHEET_ID")               # spreadsheet ID from its URL
WORKSHEET         <- Sys.getenv("WORKSHEET", "Sheet1")
SOURCE_NAME       <- Sys.getenv("SOURCE_NAME", "rss")
MAX_CONTENT_CHARS <- as.integer(Sys.getenv("MAX_CONTENT_CHARS", "3000"))

stopifnot("SHEET_ID is not set" = nzchar(SHEET_ID))

# Canonical column order. Your sheet's header row MUST match this exactly,
# because sheet_append() writes by position, not by name.
COLS <- c("item_id", "title", "content", "source", "link",
          "published_date", "ingested_at", "doc_released",
          "podcast_released", "enriched", "ai_summary")

# ---- Auth: service account JSON held in an env var ------------------------
sa_json <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON")
stopifnot("GOOGLE_SERVICE_ACCOUNT_JSON is not set" = nzchar(sa_json))
sa_file <- tempfile(fileext = ".json")
writeLines(sa_json, sa_file)
gs4_auth(path = sa_file)

# ---- Helpers --------------------------------------------------------------
strip_html <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return("")
  txt <- tryCatch(
    xml_text(read_html(paste0("<div>", x, "</div>"))),
    error = function(e) x
  )
  trimws(gsub("\\s+", " ", txt))
}

truncate_chars <- function(x, n) if (nchar(x) <= n) x else substr(x, 1, n)

node_text <- function(item, xpath) {
  node <- xml_find_first(item, xpath)
  if (inherits(node, "xml_missing")) NA_character_ else xml_text(node)
}

# ---- Fetch + parse the feed ----------------------------------------------
resp <- request(FEED_URL) |>
  req_user_agent("healthcare-podcast-ingest/1.0") |>
  req_timeout(45) |>
  req_perform()

doc   <- read_xml(resp_body_string(resp))
items <- xml_find_all(doc, "//item")

if (length(items) == 0) {
  message("Feed returned no items. Nothing to do.")
  quit(save = "no", status = 0)
}

parse_item <- function(item) {
  guid <- node_text(item, "./guid")
  link <- node_text(item, "./link")
  id   <- if (!is.na(guid) && nzchar(guid)) guid else link

  # Prefer content:encoded (richer) and fall back to description.
  body <- node_text(item, "./*[local-name()='encoded']")
  if (is.na(body) || !nzchar(body)) body <- node_text(item, "./description")

  data.frame(
    item_id          = id,
    title            = node_text(item, "./title"),
    content          = truncate_chars(strip_html(body), MAX_CONTENT_CHARS),
    source           = SOURCE_NAME,
    link             = link,
    published_date   = node_text(item, "./pubDate"),
    ingested_at      = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    doc_released     = "FALSE",
    podcast_released = "FALSE",
    enriched         = "FALSE",
    ai_summary       = "",
    stringsAsFactors = FALSE
  )
}

new_rows <- do.call(rbind, lapply(items, parse_item))
new_rows <- new_rows[!is.na(new_rows$item_id) & nzchar(new_rows$item_id), , drop = FALSE]
new_rows <- new_rows[COLS]   # enforce column order

# ---- Dedupe against what is already in the sheet --------------------------
existing <- tryCatch(
  read_sheet(SHEET_ID, sheet = WORKSHEET, col_types = "c"),
  error = function(e) NULL
)
existing_ids <- if (!is.null(existing) && "item_id" %in% names(existing)) {
  existing$item_id
} else {
  character(0)
}

fresh <- new_rows[!(new_rows$item_id %in% existing_ids), , drop = FALSE]
fresh <- fresh[!duplicated(fresh$item_id), , drop = FALSE]

# ---- Append --------------------------------------------------------------
if (nrow(fresh) == 0) {
  message("No new items.")
} else {
  sheet_append(SHEET_ID, fresh, sheet = WORKSHEET)
  message(sprintf("Appended %d new item(s).", nrow(fresh)))
}
