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
  heim = "https://www.fcstpauli.com/fu%C3%9Fball/tickets/heimspiele",
  auswaerts = "https://www.fcstpauli.com/fu%C3%9Fball/tickets/auswaertsspiele"
)

# --------------------------------------------------
# Scraping-Funktion
# --------------------------------------------------
scrape_spiele <- function(url, typ) {
  page <- read_html(url)
  spiele <- page %>% html_nodes("li.md\\:after\\:bg-primary-brown")
  
  bind_rows(lapply(spiele, function(spiel) {
    spieltyp <- spiel %>% html_node("p.text-label-s.font-800") %>% html_text(trim = TRUE)
    datum_uhrzeit <- spiel %>% html_nodes("p.text-label-s") %>% html_text(trim = TRUE)
    datum <- str_extract(datum_uhrzeit[2], "\\d{2}\\.\\d{2}\\.\\d{4}")
    uhrzeit <- str_extract(datum_uhrzeit[2], "\\d{2}:\\d{2}")
    stadion <- datum_uhrzeit[3]
    vereine <- spiel %>% html_nodes("p.text-headline-5") %>% html_text(trim = TRUE)
    heim <- vereine[1]; gast <- vereine[2]
    ticket_link <- spiel %>% html_node("a[href*='ticket-onlineshop.com']") %>% html_attr("href")
    spielbericht_link <- spiel %>% html_node("a[href*='/news/']") %>% html_attr("href")
    ticket_status <- spiel %>% html_node("div:nth-child(3) > div:nth-child(1) > div:nth-child(2) > p:nth-child(2)") %>% html_text(trim = TRUE)
    vvk_info <- spiel %>% html_nodes(".flex.flex-col.items-start.gap-10.text-right") %>% html_text(trim = TRUE) %>% paste(collapse = " | ")
    
    vvk_block <- str_extract(vvk_info, "Mitglieder\\s*(?:&|und)?\\s*Abo-Inhaber\\*innen[^|]*")
    vvk_datum <- str_extract(vvk_block, "\\b\\d{1,2}\\.\\d{1,2}(?:\\.\\d{2,4})?\\b")
    vvk_uhrzeit <- str_extract(vvk_block, "\\d{1,2}\\s*Uhr")
    if (is.na(vvk_uhrzeit)) vvk_uhrzeit <- "15 Uhr"
    
    tibble(
      spieltyp = spieltyp,
      datum = datum,
      uhrzeit = uhrzeit,
      stadion = stadion,
      heim = heim,
      gast = gast,
      ticket_link = ticket_link,
      spielbericht_link = spielbericht_link,
      ticket_status = ticket_status,
      vvk_info = vvk_info,
      vvk_datum = vvk_datum,
      vvk_uhrzeit = vvk_uhrzeit,
      art = typ
    )
  }))
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
    vvk_datum_full = ifelse(!is.na(vvk_datum) & vvk_datum != "", paste0(vvk_datum, ".", year(heute)), NA_character_),
    vvk_datum_parsed = safe_dmy(vvk_datum_full)
  )

# --------------------------------------------------
# Relevante Spiele filtern
# --------------------------------------------------
relevant <- df_spiele %>%
  filter(
    (art == "Auswärtsspiel" & !is.na(vvk_datum_parsed)) |
      (art == "Heimspiel" & !grepl("^\\d+\\. Spieltag$", spieltyp))
  )

# --------------------------------------------------
# Telegram Config
# --------------------------------------------------
bot_token <- Sys.getenv("BOT_TOKEN")
chat_id <- Sys.getenv("CHAT_ID")

if (nchar(bot_token) == 0 || nchar(chat_id) == 0) {
  stop("[FEHLER] BOT_TOKEN oder CHAT_ID nicht gesetzt!")
}

# --------------------------------------------------
# Logging-Funktion (sicher)
# --------------------------------------------------
safe_log <- function(msg) {
  cat(format(jetzt, "%Y-%m-%d %H:%M:%S"), "- ", msg, "\n")
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
    text <- paste0("🎟️ REMINDER TICKETKAUF (MORGEN)\n\n",
                   "Spiel: ", spiel$heim, " – ", spiel$gast, "\n",
                   "Datum: ", spiel$datum, " ", spiel$uhrzeit, " Uhr\n",
                   "VVK startet: ", spiel$vvk_datum, " ", spiel$vvk_uhrzeit)
    
    resp <- POST(paste0("https://api.telegram.org/bot", bot_token, "/sendMessage"),
                 body = list(chat_id = chat_id, text = text), encode = "json")
    
    res <- content(resp)
    safe_log(paste("[VOR] Telegram Response OK:", res$ok))
    if (!res$ok) safe_log(paste("[VOR] Telegram Fehler:", res$description))
    
    reminder_status <- rbind(reminder_status,
                             data.frame(spiel_id=spiel_id, reminder_typ="vor", gesendet_am=jetzt, stringsAsFactors=FALSE))
  }
  
  # --------- Reminder "tag" ---------
  bedingung_tag <- !is.na(spiel$vvk_datum_parsed) &&
    spiel$vvk_datum_parsed == heute &&
    jetzt >= as.POSIXct(paste(heute, "10:00:00"), tz="Europe/Berlin") &&
    jetzt <= as.POSIXct(paste(heute, "14:30:00"), tz="Europe/Berlin")
  
  gesendet_tag <- any(reminder_status$spiel_id == spiel_id & reminder_status$reminder_typ == "tag")
  safe_log(paste("[TAG] Bedingung erfüllt:", bedingung_tag, "Bereits gesendet:", gesendet_tag))
  
  if (bedingung_tag && !gesendet_tag) {
    safe_log("[TAG] ➜ Nachricht wird gesendet")
    text <- paste0("🚨 GLEICH TICKETS KAUFEN!\n\n",
                   "Spiel: ", spiel$heim, " – ", spiel$gast, "\n",
                   "Datum: ", spiel$datum, " ", spiel$uhrzeit, " Uhr\n",
                   "VVK um: ", spiel$vvk_uhrzeit)
    
    resp <- POST(paste0("https://api.telegram.org/bot", bot_token, "/sendMessage"),
                 body = list(chat_id = chat_id, text = text), encode = "json")
    
    res <- content(resp)
    safe_log(paste("[TAG] Telegram Response OK:", res$ok))
    if (!res$ok) safe_log(paste("[TAG] Telegram Fehler:", res$description))
    
    reminder_status <- rbind(reminder_status,
                             data.frame(spiel_id=spiel_id, reminder_typ="tag", gesendet_am=jetzt, stringsAsFactors=FALSE))
  }
}

# --------------------------------------------------
# Status speichern
# --------------------------------------------------
saveRDS(reminder_status, status_file)
safe_log("Reminder-Status gespeichert ✅")