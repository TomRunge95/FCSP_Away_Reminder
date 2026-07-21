library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(httr)

# --------------------------------------------------
# Zeitzone setzen
# --------------------------------------------------
jetzt <- with_tz(Sys.time(), "Europe/Berlin")
heute <- as.Date(jetzt)

# --------------------------------------------------
# Logging-Funktion (sicher)
# --------------------------------------------------
safe_log <- function(msg) {
  cat(format(jetzt, "%Y-%m-%d %H:%M:%S"), "- ", msg, "\n")
}

# --------------------------------------------------
# Telegram Config
# --------------------------------------------------
bot_token <- Sys.getenv("BOT_TOKEN")
chat_id <- Sys.getenv("CHAT_ID")

if (nchar(bot_token) == 0 || nchar(chat_id) == 0) {
  stop("[FEHLER] BOT_TOKEN oder CHAT_ID nicht gesetzt!")
}

telegram_send_message <- function(text, log_prefix = "TELEGRAM") {
  resp <- POST(
    paste0("https://api.telegram.org/bot", bot_token, "/sendMessage"),
    body = list(chat_id = chat_id, text = text),
    encode = "json"
  )
  
  safe_log(paste0("[", log_prefix, "] HTTP Status: ", status_code(resp)))
  safe_log(paste0("[", log_prefix, "] Raw Response: ", content(resp, "text", encoding = "UTF-8")))
  
  res <- content(resp)
  safe_log(paste0("[", log_prefix, "] Telegram Response OK: ", res$ok))
  if (!res$ok) safe_log(paste0("[", log_prefix, "] Telegram Fehler: ", res$description))
  
  invisible(res)
}

test_bot <- tolower(Sys.getenv("TEST_BOT")) %in% c("1", "true", "ja", "yes")

if (test_bot) {
  safe_log("TEST_BOT aktiv: Testnachricht wird gesendet")
  telegram_send_message(
    paste0("✅ Testnachricht vom FCSP Ticket Reminder Bot\n\n",
           "Wenn diese Nachricht in der Gruppe ankommt, funktioniert der Telegram-Versand.\n",
           "Zeitpunkt: ", format(jetzt, "%Y-%m-%d %H:%M:%S %Z")),
    "TEST"
  )
  quit(save = "no", status = 0)
}

# --------------------------------------------------
# Status laden oder neu erstellen
# --------------------------------------------------
status_file <- "reminder_status.rds"
if (file.exists(status_file)) {
  reminder_status <- readRDS(status_file)
} else {
  reminder_status <- data.frame(
    spiel_id = character(),
    reminder_typ = character(),
    gesendet_am = as.POSIXct(character()),
    stringsAsFactors = FALSE
  )
}

# --------------------------------------------------
# URLs
# --------------------------------------------------
urls <- c(
  heim = "https://www.ticket-onlineshop.com/ols/fcstpauli-heim/de/hs/channel/shop/index",
  auswaerts = "https://www.ticket-onlineshop.com/ols/fcstpauli-auswaerts/de/aw/channel/shop/index"
)

# --------------------------------------------------
# Scraping-Funktion
# --------------------------------------------------
clean_shop_text <- function(x) {
  x %>%
    str_replace_all("\u00a0", " ") %>%
    str_replace_all("[\r\n\t]+", " ") %>%
    str_squish()
}

parse_shop_date <- function(x, reference_date = heute) {
  if (is.na(x) || x == "") return(NA)
  
  date_text <- str_replace_all(x, "[()]", "")
  date_text <- str_replace(date_text, "\\.$", "")
  
  parts <- str_split(date_text, "\\.", simplify = TRUE)
  if (ncol(parts) < 2) return(NA)
  
  day <- as.integer(parts[1])
  month <- as.integer(parts[2])
  year_value <- if (ncol(parts) >= 3 && parts[3] != "") as.integer(parts[3]) else year(reference_date)
  if (!is.na(year_value) && year_value < 100) year_value <- 2000 + year_value
  
  parsed <- suppressWarnings(make_date(year_value, month, day))
  if (!is.na(parsed) && parsed < (reference_date - days(180))) {
    parsed <- suppressWarnings(make_date(year_value + 1, month, day))
  }
  
  parsed
}

format_shop_date <- function(x) {
  if (is.na(x)) NA_character_ else format(x, "%d.%m.%Y")
}

parse_shop_datetime <- function(date_text, time_text) {
  if (is.na(date_text) || date_text == "") return(as.POSIXct(NA))
  
  time_match <- str_match(time_text, "^(\\d{1,2})(?::(\\d{2}))?")
  hour_value <- if (!is.na(time_match[, 2])) as.integer(time_match[, 2]) else 15
  minute_value <- if (!is.na(time_match[, 3])) as.integer(time_match[, 3]) else 0
  
  as.POSIXct(
    sprintf("%s %02d:%02d:00", date_text, hour_value, minute_value),
    format = "%d.%m.%Y %H:%M:%S",
    tz = "Europe/Berlin"
  )
}

extract_shop_games <- function(text, typ, ticket_link) {
  text <- clean_shop_text(text)
  text <- str_replace(text, "\\s*Hinweis:.*$", "")
  
  header_pattern <- paste0(
    "(?:FCSP|FC St\\. Pauli|[[:upper:]ÄÖÜ][^().!?]{1,80}?)\\s*-\\s*",
    "(?:FCSP|FC St\\. Pauli|[^().!?]{1,80}?)\\s*\\([^()]+\\)"
  )
  headers <- str_locate_all(text, header_pattern)[[1]]
  if (nrow(headers) == 0) return(tibble())
  
  bind_rows(lapply(seq_len(nrow(headers)), function(i) {
    header <- str_sub(text, headers[i, "start"], headers[i, "end"]) %>% clean_shop_text()
    details_start <- headers[i, "end"] + 1
    details_end <- if (i < nrow(headers)) headers[i + 1, "start"] - 1 else nchar(text)
    details <- str_sub(text, details_start, details_end) %>% clean_shop_text()
    
    match <- str_match(header, "^\\s*(.*?)\\s*-\\s*(.*?)\\s*\\(([^)]*)\\)")
    heim <- str_squish(match[, 2])
    gast <- str_squish(match[, 3])
    spiel_meta <- match[, 4]
    
    spiel_datum_raw <- str_extract(spiel_meta, "\\d{1,2}\\.\\d{1,2}\\.?(?:\\d{2,4})?")
    spiel_datum_parsed <- parse_shop_date(spiel_datum_raw)
    spiel_uhrzeit <- str_extract(spiel_meta, "\\d{1,2}:\\d{2}")
    
    mitglieder_info <- if (str_detect(details, regex("Mitglieder", ignore_case = TRUE))) details else NA_character_
    mitglieder_info <- clean_shop_text(mitglieder_info)
    mitglieder_info <- str_replace(mitglieder_info, regex("^Vorverkaufsinfo:\\s*", ignore_case = TRUE), "")
    
    vvk_datum_raw <- str_extract(
      mitglieder_info,
      "\\(?\\d{1,2}\\.\\d{1,2}\\.?(?:\\d{2,4})?\\)?"
    )
    vvk_datum_parsed <- parse_shop_date(vvk_datum_raw)
    vvk_uhrzeit <- str_extract(mitglieder_info, "\\d{1,2}(?::\\d{2})?\\s*Uhr")
    if (!is.na(vvk_uhrzeit)) vvk_uhrzeit <- str_squish(vvk_uhrzeit)
    if (!is.na(vvk_datum_parsed) && is.na(vvk_uhrzeit)) vvk_uhrzeit <- "15 Uhr"
    
    vvk_zielgruppe <- str_extract(mitglieder_info, regex("^.*?(?=:\\s|:\\s*ab|\\s+ab\\s)", ignore_case = TRUE))
    if (is.na(vvk_zielgruppe) || vvk_zielgruppe == "") vvk_zielgruppe <- "Mitglieder"
    vvk_zielgruppe <- clean_shop_text(vvk_zielgruppe)
    
    tibble(
      spieltyp = typ,
      datum = format_shop_date(spiel_datum_parsed),
      uhrzeit = ifelse(is.na(spiel_uhrzeit), NA_character_, spiel_uhrzeit),
      stadion = NA_character_,
      heim = heim,
      gast = gast,
      ticket_link = ticket_link,
      spielbericht_link = NA_character_,
      ticket_status = NA_character_,
      vvk_info = mitglieder_info,
      vvk_datum = format_shop_date(vvk_datum_parsed),
      vvk_uhrzeit = vvk_uhrzeit,
      vvk_zielgruppe = vvk_zielgruppe,
      art = typ
    )
  })) %>%
    filter(!is.na(vvk_datum), str_detect(vvk_info, regex("Mitglieder", ignore_case = TRUE)))
}

scrape_spiele <- function(url, typ) {
  resp <- GET(
    url,
    user_agent("Mozilla/5.0 FCSP-Ticket-Reminder/1.0"),
    timeout(30)
  )
  stop_for_status(resp)
  
  page <- read_html(content(resp, "text", encoding = "UTF-8"))
  info_nodes <- page %>%
    html_nodes(".copy-block.copy-block--full-width .hint, .copy-block .hint")
  if (length(info_nodes) == 0) info_nodes <- page %>% html_nodes("main#main")
  
  info_blocks <- info_nodes %>%
    html_text2() %>%
    paste(collapse = " ")
  
  if (info_blocks == "") {
    safe_log(paste("[WARN] Keine Ticketshop-Hinweisblöcke gefunden:", url))
    return(tibble())
  }
  
  extract_shop_games(info_blocks, typ, url)
}

# --------------------------------------------------
# Spiele abrufen
# --------------------------------------------------
df_spiele <- bind_rows(
  scrape_spiele(urls["heim"], "Heimspiel"),
  scrape_spiele(urls["auswaerts"], "Auswärtsspiel")
)

# --------------------------------------------------
# Datum vorbereiten
# --------------------------------------------------
safe_dmy <- function(x) suppressWarnings(dmy(x))
df_spiele <- df_spiele %>%
  mutate(
    vvk_datum_parsed = safe_dmy(vvk_datum),
    vvk_start = as.POSIXct(
      mapply(parse_shop_datetime, vvk_datum, vvk_uhrzeit),
      origin = "1970-01-01",
      tz = "Europe/Berlin"
    )
  )

# --------------------------------------------------
# Relevante Spiele filtern
# --------------------------------------------------
relevant <- df_spiele %>%
  filter(
    !is.na(vvk_datum_parsed) &
      art %in% c("Heimspiel", "Auswärtsspiel")
  )

baue_reminder_text <- function(spiel, reminder_typ) {
  zielgruppe <- ifelse(is.na(spiel$vvk_zielgruppe) || spiel$vvk_zielgruppe == "", "Mitglieder", spiel$vvk_zielgruppe)
  spielzeit <- ifelse(is.na(spiel$uhrzeit) || spiel$uhrzeit == "", "", paste0(" ", spiel$uhrzeit, " Uhr"))
  
  if (reminder_typ == "vor") {
    return(paste0("🎟️ MITGLIEDERVERKAUF MORGEN\n\n",
                  "Morgen startet der Vorverkauf für: ", zielgruppe, ".\n",
                  "Spiel: ", spiel$heim, " – ", spiel$gast, "\n",
                  "Datum: ", spiel$datum, spielzeit, "\n",
                  "VVK startet: ", spiel$vvk_datum, " ", spiel$vvk_uhrzeit))
  }
  
  paste0("🚨 MITGLIEDERVERKAUF STARTET HEUTE\n\n",
         "Heute startet der Vorverkauf für: ", zielgruppe, ".\n",
         "Spiel: ", spiel$heim, " – ", spiel$gast, "\n",
         "Datum: ", spiel$datum, spielzeit, "\n",
         "VVK um: ", spiel$vvk_uhrzeit)
}

# --------------------------------------------------
# Reminder-Logik
# --------------------------------------------------
for (i in seq_len(nrow(relevant))) {
  spiel <- relevant[i, ]
  jetzt <- with_tz(Sys.time(), "Europe/Berlin")
  heute <- as.Date(jetzt)
  
  spiel_id <- paste(spiel$heim, spiel$gast, spiel$datum, sep = "_")
  
  safe_log(paste("Spiel:", spiel_id))
  safe_log(paste("VVK Datum:", spiel$vvk_datum_parsed, "heute:", heute))
  
  # --------- Reminder "vor" ---------
  bedingung_vor <- !is.na(spiel$vvk_datum_parsed) && (spiel$vvk_datum_parsed - days(1)) == heute
  gesendet_vor <- any(reminder_status$spiel_id == spiel_id & reminder_status$reminder_typ == "vor")
  safe_log(paste("[VOR] Bedingung erfüllt:", bedingung_vor, "Bereits gesendet:", gesendet_vor))
  
  if (bedingung_vor && !gesendet_vor) {
    safe_log("[VOR] ➜ Nachricht wird gesendet")
    text <- baue_reminder_text(spiel, "vor")
    
    res <- telegram_send_message(text, "VOR")
    
    reminder_status <- rbind(reminder_status,
                             data.frame(spiel_id=spiel_id, reminder_typ="vor", gesendet_am=jetzt, stringsAsFactors=FALSE))
  }
  
  # --------- Reminder "tag" ---------
  startzeit <- if (!is.na(spiel$vvk_start)) spiel$vvk_start else as.POSIXct(paste(heute, "15:00:00"), tz="Europe/Berlin")
  bedingung_tag <- !is.na(spiel$vvk_datum_parsed) &&
    spiel$vvk_datum_parsed == heute &&
    jetzt >= (startzeit - hours(5)) &&
    jetzt <= (startzeit - minutes(30))
  
  gesendet_tag <- any(reminder_status$spiel_id == spiel_id & reminder_status$reminder_typ == "tag")
  safe_log(paste("[TAG] Bedingung erfüllt:", bedingung_tag, "Bereits gesendet:", gesendet_tag))
  
  if (bedingung_tag && !gesendet_tag) {
    safe_log("[TAG] ➜ Nachricht wird gesendet")
    text <- baue_reminder_text(spiel, "tag")
    
    res <- telegram_send_message(text, "TAG")
    
    reminder_status <- rbind(reminder_status,
                             data.frame(spiel_id=spiel_id, reminder_typ="tag", gesendet_am=jetzt, stringsAsFactors=FALSE))
  }
}

# --------------------------------------------------
# Status speichern
# --------------------------------------------------
saveRDS(reminder_status, status_file)
safe_log("Reminder-Status gespeichert ✅")
