# FC St. Pauli Ticket Reminder Bot

Ein R-basiertes Telegram-Bot-Skript, das automatisch **Reminder für Auswärts- und Heimspiele** von FC St. Pauli verschickt. Der Bot erinnert Mitglieder einer Telegram-Gruppe **1 Tag vor VVK-Beginn** und am **Spieltag um 14:00 Uhr**, wenn VVK-Datum und Uhrzeit verfügbar sind. Für Heimspiele nutzt der Bot einen eigenen Text für alle ohne Dauerkarte.

---

## 📝 Features

- Scraping der offiziellen FC St. Pauli Ticket-Seiten (Heim- und Auswärtsspiele).  
- Filtert automatisch Auswärts- und Heimspiele mit verfügbarem VVK-Datum.  
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
  ```

## 🧪 Telegram-Test

Um nur zu testen, ob der Bot in die Gruppe schreiben kann, das Skript mit `TEST_BOT=1` starten:

```bash
TEST_BOT=1 Rscript Mett_Bot.R
```

Der Bot sendet dann sofort eine Testnachricht und beendet sich ohne Scraping und ohne Reminder-Status zu ändern.
