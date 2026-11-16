suppressPackageStartupMessages(library(stringr))

expressions <- parse("Mett_Bot.R")
for (expr in expressions) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      as.character(expr[[2]]) %in% c("opponent_from_news", "infer_game_type")) {
    eval(expr)
  }
}

title <- "Ticket-Infos zum Spiel beim VfL Osnabrück"
uri <- "news/ticket-infos-zum-spiel-beim-vfl-osnabrueck"
for (fallback in c("Heimspiel", "Auswärtsspiel")) {
  typ <- infer_game_type(title, uri, fallback)
  stopifnot(typ == "Auswärtsspiel", opponent_from_news(title, uri, typ) == "VfL Osnabrück")
}
stopifnot(
  infer_game_type("Ticket-Infos zum Spiel bei Holstein Kiel", "", "Heimspiel") == "Auswärtsspiel",
  opponent_from_news("Ticket-Infos zum Spiel bei Holstein Kiel", "", "Auswärtsspiel") == "Holstein Kiel",
  infer_game_type("Ticket-Infos zum Heimspiel gegen den VfL Bochum", "", "Auswärtsspiel") == "Heimspiel"
)
