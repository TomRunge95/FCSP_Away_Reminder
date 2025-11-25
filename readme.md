# FC St. Pauli Ticket Reminder Bot

Ein R-basiertes Telegram-Bot-Skript, das automatisch **Reminder für Auswärts- und Sonder-Heimspiele** von FC St. Pauli verschickt. Der Bot erinnert Mitglieder einer Telegram-Gruppe **1 Tag vor VVK-Beginn** und am **Spieltag um 14:00 Uhr**, wenn VVK-Datum und Uhrzeit verfügbar sind.

---

## 📝 Features

- Scraping der offiziellen FC St. Pauli Ticket-Seiten (Heim- und Auswärtsspiele).  
- Filtert automatisch Auswärtsspiele für oder Sonder-Heimspiele.  
- Automatische Versandzeit:
  - 1 Tag vorher
  - Am Spieltag um 14:00 Uhr
- Flexibel: kann in **Cronjobs oder Task Scheduler** täglich ausgeführt werden.

---

## 🚀 Voraussetzungen

- R >= 4.1  
- Pakete: `dplyr`, `lubridate`, `httr`, `rvest`, `stringr`  
  ```r
  install.packages(c("dplyr", "lubridate", "httr", "rvest", "stringr"))
