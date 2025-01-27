# Funktion um schenll alle falsch eingelsenen Umlaute in einem Vektor umzucodieren
# cheatsheet für UTF-8 codierungen: https://bueltge.de/wp-content/download/wk/utf-8_kodierungen.pdf


recode_umlaut <- function(x){
  y <- gsub("Ã¤", "ä", x)
  y <- gsub("Ã¶", "ö", y)
  y <- gsub("Ã¼", "ü", y)
  y <- gsub("ÃŸ", "ß", y)
  y <- gsub("Ã„", "Ä", y)
  y <- gsub("Ã–", "Ö", y)
  y <- gsub("Ãœ", "Ü", y)
  return(y)
}
