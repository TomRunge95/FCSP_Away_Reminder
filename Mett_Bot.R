library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(httr)



status_file <- "reminder_status.rds"

if (file.exists(status_file)) {
  reminder_status <- readRDS(status_file)
} else {
  reminder_status <- data.frame(
    spiel_id = character(),
    reminder_typ = character(), # "vor" oder "tag"
    gesendet_am = as.POSIXct(character()),
    stringsAsFactors = FALSE
  )
}


Sys.setenv(TZ = "Europe/Berlin")

# URLs für Heim- und Auswärtsspiele
urls <- c(
  heim = "https://www.fcstpauli.com/fu%C3%9Fball/tickets/heimspiele",
  auswaerts = "https://www.fcstpauli.com/fu%C3%9Fball/tickets/auswaertsspiele"
)


scrape_spiele <- function(url, typ) {
  page <- read_html(url)
  
  spiele <- page %>% html_nodes("li.md\\:after\\:bg-primary-brown")
  
  spiel_df <- lapply(spiele, function(spiel){
    
    spieltyp <- spiel %>%
      html_node("p.text-label-s.font-800") %>%
      html_text(trim = TRUE)
    
    datum_uhrzeit <- spiel %>%
      html_nodes("p.text-label-s") %>%
      html_text(trim = TRUE)
    
    datum <- str_extract(datum_uhrzeit[2], "\\d{2}\\.\\d{2}\\.\\d{4}")
    uhrzeit <- str_extract(datum_uhrzeit[2], "\\d{2}:\\d{2}")
    stadion <- datum_uhrzeit[3]
    
    vereine <- spiel %>%
      html_nodes("p.text-headline-5") %>%
      html_text(trim = TRUE)
    
    heim <- vereine[1]
    gast <- vereine[2]
    
    ticket_link <- spiel %>%
      html_node("a[href*='ticket-onlineshop.com']") %>%
      html_attr("href")
    
    spielbericht_link <- spiel %>%
      html_node("a[href*='/news/']") %>%
      html_attr("href")
    
    ticket_status <- spiel %>%
      html_node("div:nth-child(3) > div:nth-child(1) > div:nth-child(2) > p:nth-child(2)") %>%
      html_text(trim = TRUE)
    
    ticket_status_kategorie <- case_when(
      str_detect(ticket_status, regex("ausverkauft", ignore_case = TRUE)) ~ "Ausverkauft",
      str_detect(ticket_status, regex("verkauf startet", ignore_case = TRUE)) ~ "Verkauf startet später",
      str_detect(ticket_status, regex("im verkauf", ignore_case = TRUE)) ~ "Im Verkauf",
      TRUE ~ ticket_status
    )
    
    # VVK Info auslesen
    vvk_info <- spiel %>%
      html_nodes(".flex.flex-col.items-start.gap-10.text-right") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = " | ")
    
    # --------------------------------------------------
    # Mitglieder-&-Abo-Block extrahieren
    # --------------------------------------------------
    vvk_block <- str_extract(
      vvk_info,
      "Mitglieder\\s*(?:&|und)?\\s*Abo-Inhaber\\*innen[^|]*"
    )
    
    # Datum aus dem Block
    vvk_datum <- str_extract(vvk_block, "\\d{2}\\.\\d{2}")
    
    # Uhrzeit aus dem Block
    vvk_uhrzeit <- str_extract(vvk_block, "\\d{1,2}\\s*Uhr")
    
    # Fallback Uhrzeit
    if (is.na(vvk_uhrzeit)) {
      vvk_uhrzeit <- "15 Uhr"
    }
    
    
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
      ticket_status_kategorie = ticket_status_kategorie,
      vvk_info = vvk_info,
      vvk_datum = vvk_datum,
      vvk_uhrzeit = vvk_uhrzeit,
      art = typ
    )
    
  })
  
  bind_rows(spiel_df)
}

# Heim- und Auswärtsspiele abrufen
df_heim <- scrape_spiele(urls["heim"], "Heimspiel")
df_auswaerts <- scrape_spiele(urls["auswaerts"], "Auswärtsspiel")

# Zusammenführen
df_spiele <- bind_rows(df_heim, df_auswaerts)

# Ergebnis
print(df_spiele)


bot_token <- Sys.getenv("METT_TOKEN")
chat_id <- Sys.getenv("chat_id")

# Heute
heute <- Sys.Date()

# Sichere Datumskonvertierung
safe_dmy <- function(x) {
  res <- suppressWarnings(dmy(x))
  res[is.na(res)] <- NA
  return(res)
}

# VVK-Datum parsen
df_spiele <- df_spiele %>%
  mutate(
    vvk_datum_full = ifelse(!is.na(vvk_datum) & vvk_datum != "",
                            paste0(vvk_datum, ".", year(heute)),
                            NA_character_),
    vvk_datum_parsed = safe_dmy(vvk_datum_full)
  )

# Filter: Auswärtsspiel heute oder Sonder-Heimspiel
auswaerts_und_sonder_heim <- df_spiele %>%
  filter(
    # Auswärtsspiele mit VVK
    (art == "Auswärtsspiel" & !is.na(vvk_datum_parsed) & !is.na(vvk_uhrzeit)) |
      # Heimspiele, die nicht wie "xx. Spieltag" aussehen
      (art == "Heimspiel" & !grepl("^\\d+\\. Spieltag$", spieltyp))
  )




# Nachricht nur senden, wenn Bedingungen erfüllt sind
if (nrow(auswaerts_und_sonder_heim) > 0) {
  
  for (i in seq_len(nrow(auswaerts_und_sonder_heim))) {
    
    spiel <- auswaerts_und_sonder_heim[i, ]
    
    # Eindeutige Spiel-ID
    spiel_id <- paste(
      spiel$heim,
      spiel$gast,
      spiel$datum,
      sep = "_"
    )
    
    
    jetzt <- Sys.time()
    
    # Bedingungen prüfen
    reminder_vor <- !is.na(spiel$vvk_datum_parsed) &
      spiel$vvk_datum_parsed - 1 == heute
    
    reminder_tag <- !is.na(spiel$vvk_datum_parsed) &
      spiel$vvk_datum_parsed == heute &
      jetzt >= as.POSIXct(paste(heute, "12:55:00"), tz = "Europe/Berlin") &
      jetzt <= as.POSIXct(paste(heute, "13:15:00"), tz = "Europe/Berlin")
    
    # Welcher Reminder-Typ?
    reminder_typ <- NA
    if (reminder_vor) reminder_typ <- "vor"
    if (reminder_tag) reminder_typ <- "tag"
    
    if (!is.na(reminder_typ)) {
      
      # Prüfen ob schon gesendet
      schon_gesendet <- any(
        reminder_status$spiel_id == spiel_id &
          reminder_status$reminder_typ == reminder_typ
      )
      
      if (!schon_gesendet) {
        
        nachricht <- paste0(
          "🔔🎟️ REMINDER TICKETKAUF! 🔔🎟️\n\n",
          "Spieltyp: ", spiel$spieltyp, "\n",
          "Spiel: ", spiel$heim, " gegen ", spiel$gast, "\n",
          "Datum/Uhrzeit: ", spiel$datum, " ", spiel$uhrzeit, " Uhr\n",
          "VVK: ", spiel$vvk_datum, " ", spiel$vvk_uhrzeit
        )
        
        url <- paste0("https://api.telegram.org/bot", bot_token, "/sendMessage")
        
        response <- POST(
          url,
          body = list(
            chat_id = "75538067",
            text = nachricht
          ),
          encode = "json"
        )
        
        print(content(response, "parsed"))
        
        # Status speichern
        reminder_status <- rbind(
          reminder_status,
          data.frame(
            spiel_id = spiel_id,
            reminder_typ = reminder_typ,
            gesendet_am = Sys.time(),
            stringsAsFactors = FALSE
          )
        )
        
        saveRDS(reminder_status, status_file)
        
      } else {
        cat("⏭️ Reminder bereits gesendet für:", spiel_id, reminder_typ, "\n")
      }
    }
  }
  
} else {
  cat("Keine relevanten Spiele für Reminder heute.\n")
}
