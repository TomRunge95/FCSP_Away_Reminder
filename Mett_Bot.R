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

fallback_urls <- c(
  heim = "https://www.fcstpauli.com/fu%C3%9Fball/tickets/heimspiele",
  auswaerts = "https://www.fcstpauli.com/fu%C3%9Fball/tickets/auswaertsspiele"
)

shop_user_agent <- Sys.getenv(
  "SHOP_USER_AGENT",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36"
)
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

empty_spiele_tibble <- function() {
  tibble(
    spieltyp = character(),
    datum = character(),
    uhrzeit = character(),
    stadion = character(),
    heim = character(),
    gast = character(),
    ticket_link = character(),
    spielbericht_link = character(),
    ticket_status = character(),
    vvk_info = character(),
    vvk_datum = character(),
    vvk_uhrzeit = character(),
    vvk_zielgruppe = character(),
    art = character()
  )
}

absolute_fcsp_url <- function(path) {
  if (is.na(path) || path == "") return(NA_character_)
  if (str_detect(path, "^https?://")) return(path)
  paste0("https://www.fcstpauli.com/", str_replace(path, "^/", ""))
}

opponent_from_news <- function(title, uri, typ) {
  if (typ == "Heimspiel") {
    opponent <- str_replace(title, regex("^Ticket-Infos zum Heimspiel gegen\\s+", ignore_case = TRUE), "")
    opponent <- str_replace(opponent, regex("^(den|die|das)\\s+", ignore_case = TRUE), "")
    return(str_squish(opponent))
  }
  
  slug <- uri %>%
    str_replace("^.*/", "") %>%
    str_replace("^ticket-infos-auswaertsspiel-", "") %>%
    str_replace("-\\d{4}$", "") %>%
    str_replace("-\\d{2}\\d{2}$", "") %>%
    str_replace_all("-", " ") %>%
    str_to_title()
  
  if (str_detect(slug, regex("^Kiel$", ignore_case = TRUE))) return("Holstein Kiel")
  str_squish(slug)
}

extract_fcsp_news_game <- function(news_url, typ, source_url) {
  resp <- GET(
    news_url,
    user_agent("Mozilla/5.0 FCSP-Ticket-Reminder/1.0"),
    timeout(30)
  )
  stop_for_status(resp)
  
  page <- read_html(content(resp, "text", encoding = "UTF-8"))
  title <- page %>% html_node("h1") %>% html_text2()
  main_text <- page %>% html_node("main") %>% html_text2() %>% clean_shop_text()
  
  if (is.na(title) || title == "" || !str_detect(title, regex("Ticket-Infos", ignore_case = TRUE))) {
    return(empty_spiele_tibble())
  }
  
  uri <- str_replace(news_url, "^https://www\\.fcstpauli\\.com/", "")
  opponent <- opponent_from_news(title, uri, typ)
  
  spiel_meta <- str_extract(main_text, "\\(\\d{1,2}\\.\\d{1,2}\\.\\s*,\\s*\\d{1,2}:\\d{2}\\s*Uhr\\)")
  if (is.na(spiel_meta)) {
    spiel_meta <- str_extract(main_text, "\\(\\d{1,2}\\.\\d{1,2}\\.\\s*,?\\s*(?:ab\\s*)?\\d{1,2}(?::\\d{2})?\\s*Uhr\\)")
  }
  
  spiel_datum_raw <- str_extract(spiel_meta, "\\d{1,2}\\.\\d{1,2}\\.?(?:\\d{2,4})?")
  spiel_datum_parsed <- parse_shop_date(spiel_datum_raw)
  spiel_uhrzeit <- str_extract(spiel_meta, "\\d{1,2}:\\d{2}")
  
  vvk_info <- str_extract(
    main_text,
    regex("Mitgliedervorverkauf.{0,180}?\\d{1,2}\\.\\d{1,2}\\.?(?:\\d{2,4})?.{0,80}?\\d{1,2}(?::\\d{2})?\\s*Uhr", ignore_case = TRUE)
  )
  if (is.na(vvk_info)) {
    vvk_info <- str_extract(
      main_text,
      regex("Verkaufsphase\\s*1\\s*\\([^)]*Mitglieder[^)]*\\).{0,180}?\\d{1,2}\\.\\d{1,2}\\.?(?:\\d{2,4})?.{0,80}?\\d{1,2}(?::\\d{2})?\\s*Uhr", ignore_case = TRUE)
    )
  }
  if (is.na(vvk_info)) {
    vvk_info <- str_extract(
      main_text,
      regex("Mitglieder.{0,220}?\\d{1,2}\\.\\d{1,2}\\.?(?:\\d{2,4})?.{0,80}?\\d{1,2}(?::\\d{2})?\\s*Uhr", ignore_case = TRUE)
    )
  }
  
  if (is.na(vvk_info) || !str_detect(vvk_info, regex("Mitglieder", ignore_case = TRUE))) {
    safe_log(paste("[WARN] Keine Mitgliederverkaufsdaten in FCSP-News gefunden:", news_url))
    return(empty_spiele_tibble())
  }
  
  vvk_info <- clean_shop_text(vvk_info)
  vvk_datum_raw <- str_extract(vvk_info, "\\(?\\d{1,2}\\.\\d{1,2}\\.?(?:\\d{2,4})?\\)?")
  vvk_datum_parsed <- parse_shop_date(vvk_datum_raw)
  vvk_uhrzeit <- str_extract(vvk_info, "\\d{1,2}(?::\\d{2})?\\s*Uhr")
  if (!is.na(vvk_uhrzeit)) vvk_uhrzeit <- str_squish(vvk_uhrzeit)
  if (!is.na(vvk_datum_parsed) && is.na(vvk_uhrzeit)) vvk_uhrzeit <- "15 Uhr"
  
  zielgruppe <- str_match(vvk_info, regex("Verkaufsphase\\s*1\\s*\\(([^)]*Mitglieder[^)]*)\\)", ignore_case = TRUE))[, 2]
  if (is.na(zielgruppe) || zielgruppe == "") zielgruppe <- "Mitglieder"
  zielgruppe <- clean_shop_text(zielgruppe)
  
  ticket_link <- page %>%
    html_node("a[href*='ticket-onlineshop.com']") %>%
    html_attr("href")
  if (is.na(ticket_link) || ticket_link == "") ticket_link <- source_url
  
  tibble(
    spieltyp = typ,
    datum = format_shop_date(spiel_datum_parsed),
    uhrzeit = ifelse(is.na(spiel_uhrzeit), NA_character_, spiel_uhrzeit),
    stadion = NA_character_,
    heim = ifelse(typ == "Heimspiel", "FCSP", opponent),
    gast = ifelse(typ == "Heimspiel", opponent, "FCSP"),
    ticket_link = ticket_link,
    spielbericht_link = news_url,
    ticket_status = NA_character_,
    vvk_info = vvk_info,
    vvk_datum = format_shop_date(vvk_datum_parsed),
    vvk_uhrzeit = vvk_uhrzeit,
    vvk_zielgruppe = zielgruppe,
    art = typ
  ) %>%
    filter(!is.na(vvk_datum), !is.na(datum))
}
extract_shop_games <- function(text, typ, ticket_link) {
  text <- clean_shop_text(text)
  text <- str_replace(text, "\\s*Hinweis:.*$", "")
  
  header_pattern <- paste0(
    "(?:FCSP|FC St\\. Pauli|[[:upper:]ÄÖÜ][^().!?]{1,80}?)\\s*-\\s*",
    "(?:FCSP|FC St\\. Pauli|[^().!?]{1,80}?)\\s*\\([^()]+\\)"
  )
  headers <- str_locate_all(text, header_pattern)[[1]]
  if (nrow(headers) == 0) return(empty_spiele_tibble())
  
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
    user_agent(shop_user_agent),
    add_headers(
      Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      `Accept-Language` = "de-DE,de;q=0.9,en;q=0.8",
      `Cache-Control` = "no-cache",
      Pragma = "no-cache",
      `Upgrade-Insecure-Requests` = "1",
      `Sec-Fetch-Dest` = "document",
      `Sec-Fetch-Mode` = "navigate",
      `Sec-Fetch-Site` = "none",
      `Sec-Fetch-User` = "?1"
    ),
    timeout(30)
  )
  stop_for_status(resp)
  
  page <- read_html(content(resp, "text", encoding = "UTF-8"))
  page_title <- page %>% html_node("title") %>% html_text2()
  
  if (str_detect(page_title, regex("Queue-it", ignore_case = TRUE))) {
    safe_log(paste("[WARN] Queue-it statt Ticketshop erhalten:", url))
  }
  
  info_nodes <- page %>%
    html_nodes(".copy-block.copy-block--full-width .hint, .copy-block .hint")
  if (length(info_nodes) == 0) info_nodes <- page %>% html_nodes("main#main")
  if (length(info_nodes) == 0) info_nodes <- page %>% html_nodes("body")
  
  info_blocks <- info_nodes %>%
    html_text2() %>%
    paste(collapse = " ")
  
  if (info_blocks == "") {
    safe_log(paste("[WARN] Keine Ticketshop-Hinweisblöcke gefunden:", url))
    return(empty_spiele_tibble())
  }
  
  spiele <- extract_shop_games(info_blocks, typ, url)
  if (nrow(spiele) == 0) {
    safe_log(paste("[WARN] Keine Mitgliederverkaufsdaten gefunden:", url, "| Titel:", page_title))
    safe_log(paste("[WARN] HTML-Textauszug:", str_sub(clean_shop_text(info_blocks), 1, 300)))
  }
  
  spiele
}

scrape_fcsp_fallback <- function(url, typ) {
  resp <- GET(
    url,
    user_agent("Mozilla/5.0 FCSP-Ticket-Reminder/1.0"),
    timeout(30)
  )
  stop_for_status(resp)
  
  html <- content(resp, "text", encoding = "UTF-8")
  html <- str_replace_all(html, "\\\\/", "/")
  news_paths <- str_extract_all(
    html,
    "news/ticket-infos-(?:heimspiel|auswaertsspiel)-[a-z0-9-]+"
  )[[1]] %>%
    unique()
  
  news_paths <- news_paths[str_detect(news_paths, ifelse(typ == "Heimspiel", "heimspiel", "auswaertsspiel"))]
  
  if (length(news_paths) == 0) {
    safe_log(paste("[WARN] Keine FCSP-Fallback-News gefunden:", url))
    return(empty_spiele_tibble())
  }
  
  safe_log(paste("[INFO] FCSP-Fallback-News gefunden:", length(news_paths), "Quelle:", url))
  
  bind_rows(lapply(news_paths, function(path) {
    news_url <- absolute_fcsp_url(path)
    tryCatch(
      extract_fcsp_news_game(news_url, typ, url),
      error = function(e) {
        safe_log(paste("[WARN] FCSP-News konnte nicht gelesen werden:", news_url, "|", e$message))
        empty_spiele_tibble()
      }
    )
  })) %>%
    mutate(spiel_datum_check = suppressWarnings(dmy(datum))) %>%
    filter(is.na(spiel_datum_check) | spiel_datum_check >= heute) %>%
    select(-spiel_datum_check)
}

scrape_all_fcsp_fallbacks <- function(typ) {
  bind_rows(lapply(fallback_urls, function(url) {
    scrape_fcsp_fallback(url, typ)
  })) %>%
    distinct(spielbericht_link, .keep_all = TRUE)
}

# --------------------------------------------------
# Spiele abrufen
# --------------------------------------------------
df_spiele <- bind_rows(
  scrape_spiele(urls["heim"], "Heimspiel"),
  scrape_spiele(urls["auswaerts"], "Auswärtsspiel")
)

if (nrow(df_spiele) == 0) {
  safe_log("[INFO] Ticketshop liefert keine verwertbaren Daten. FCSP-Fallback wird genutzt.")
  df_spiele <- bind_rows(
    scrape_all_fcsp_fallbacks("Heimspiel"),
    scrape_all_fcsp_fallbacks("Auswärtsspiel")
  )
} else {
  safe_log(paste("[INFO] Ticketshop-Daten gefunden:", nrow(df_spiele)))
}

# --------------------------------------------------
# Datum vorbereiten
# --------------------------------------------------
safe_dmy <- function(x) suppressWarnings(dmy(x))
df_spiele <- df_spiele %>%
  mutate(
    vvk_datum_parsed = safe_dmy(vvk_datum),
    vvk_start = as.POSIXct(
      vapply(
        seq_along(vvk_datum),
        function(i) as.numeric(parse_shop_datetime(vvk_datum[i], vvk_uhrzeit[i])),
        numeric(1)
      ),
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

versand_status <- tibble(
  spiel_id = character(),
  reminder_typ = character(),
  status = character(),
  grund = character()
)

log_versand_status <- function(spiel_id, reminder_typ, status, grund) {
  versand_status <<- bind_rows(
    versand_status,
    tibble(
      spiel_id = spiel_id,
      reminder_typ = reminder_typ,
      status = status,
      grund = grund
    )
  )
}

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
    log_versand_status(spiel_id, "vor", "gesendet", "Vorverkauf ist morgen und Nachricht war noch nicht gesendet")
  } else if (gesendet_vor) {
    log_versand_status(spiel_id, "vor", "nicht gesendet", "Nachricht wurde bereits frueher gesendet")
  } else {
    log_versand_status(spiel_id, "vor", "nicht gesendet", "Vorverkauf ist nicht morgen")
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
    log_versand_status(spiel_id, "tag", "gesendet", "Vorverkauf ist heute und aktueller Zeitpunkt liegt im Versandfenster")
  } else if (gesendet_tag) {
    log_versand_status(spiel_id, "tag", "nicht gesendet", "Nachricht wurde bereits frueher gesendet")
  } else if (!is.na(spiel$vvk_datum_parsed) && spiel$vvk_datum_parsed == heute) {
    log_versand_status(spiel_id, "tag", "nicht gesendet", "Heute ist Vorverkauf, aber aktueller Zeitpunkt liegt ausserhalb des Versandfensters")
  } else {
    log_versand_status(spiel_id, "tag", "nicht gesendet", "Vorverkauf ist nicht heute")
  }
}

safe_log("----- Versandbericht -----")
if (nrow(relevant) == 0) {
  if (nrow(df_spiele) == 0) {
    safe_log("[VERSAND] Keine Nachricht gesendet: Es wurden keine Spiele mit Mitgliederverkaufsdaten gefunden.")
  } else {
    safe_log("[VERSAND] Keine Nachricht gesendet: Spiele gefunden, aber ohne gueltiges VVK-Datum.")
  }
} else {
  for (i in seq_len(nrow(versand_status))) {
    eintrag <- versand_status[i, ]
    safe_log(paste0(
      "[VERSAND] ", eintrag$status,
      " | Typ: ", eintrag$reminder_typ,
      " | Spiel: ", eintrag$spiel_id,
      " | Grund: ", eintrag$grund
    ))
  }
}

# --------------------------------------------------
# Status speichern
# --------------------------------------------------
saveRDS(reminder_status, status_file)
safe_log("Reminder-Status gespeichert ✅")
