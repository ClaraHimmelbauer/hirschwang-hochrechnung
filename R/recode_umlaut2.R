# Funktion um schenll alle falsch eingelsenen Umlaute in einem Vektor umzucodieren
# Version 2, also anderes encoding fehler

recode_umlaut2 <- function(x){
  y <- gsub("\xe4", "ä", x, useBytes = TRUE)
  y <- gsub("\xf6", "ö", y, useBytes = TRUE)
  y <- gsub("\xfc", "ü", y, useBytes = TRUE)
  y <- gsub("\xdf", "ß", y, useBytes = TRUE)
  y <- gsub("\xc4", "Ä", y, useBytes = TRUE)
  y <- gsub("\xd6", "Ö", y, useBytes = TRUE)
  y <- gsub("\xdc", "Ü", y, useBytes = TRUE)
  y <- gsub("\xc9", "E", y, useBytes = TRUE)
  return(y)
}

