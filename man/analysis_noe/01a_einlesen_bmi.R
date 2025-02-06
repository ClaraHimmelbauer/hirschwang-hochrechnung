# # einlesen BMI-Format
# rm(list = ls())
# 
# packages <- c("tidyverse", "readxl", "rvest", "httr")
# lapply(packages, library, character.only = T)

#---------------------------------------------------------------------------------
# time <- Sys.time()

# Mit server verbinden, authentifizieren
url <- "https://www.wahl.noe.gv.at/lw23.bmi"
html <- GET(url, authenticate(user = "ltw2023", password = "Minztee!23"))

# daten runterladen, splitten
cont <- rawToChar(content(html, as = "raw"))
cont <- unlist(strsplit(cont, "\r\n"))
cont <- cont[-1]

# einzlne cols erstellen
df <- data.frame(matrix(nrow = length(cont), ncol = 0))
df["gkz"]     <- substr(cont,  1,  5)
df["dat"]     <- substr(cont,  6, 13)
df["time"]    <- substr(cont, 14, 19)
df["wber"]    <- substr(cont, 20, 26)
df["abg"]     <- substr(cont, 41, 47)
df["ung"]     <- substr(cont, 48, 54)
df["gueltig"] <- substr(cont, 55, 61)

# stimmen
df["vp"]   <- substr(cont,  62,  68)
df["sp"]   <- substr(cont,  69,  75)
df["fp"]   <- substr(cont,  76,  82)
df["gr"]   <- substr(cont,  83,  89)
df["ne"]   <- substr(cont,  90,  96)
df["mfg"]  <- substr(cont,  97, 103)
df["kp"]   <- substr(cont, 104, 110)
df["ziel"] <- substr(cont, 111, 117)


df <- as.data.frame(lapply(df, as.numeric))
df <- df[!is.na(df$gkz), ]
df <- df[df$wber > 0, ]
df <- df[df$gkz > 30100, ]
df <- df[substr(df$gkz, 4, 5) != "00", ]
df$so <- df$mfg + df$kp + df$ziel

# Sys.time() - time
# 
# rm(html, cont, time, url); gc()


# importdaten abspeichern
# 
# # check
# df$ausgezaehlt <- ifelse(df$abg > 0, 1, 0)
# check1 <- data.frame("Region" = c("", "Ausgezaehlte Gemeinden", "abgegebene Stimmen", "Auszaehlungsgrad", "Wahlbeteiligung", "",
#                                      "vp", "sp", "fp", "gr", "ne", "so"),
#                         "Österreich" = c("",
#                                          sum(df$ausgezaehlt),
#                                          sum(df$gueltig),
#                                          round(sum(df$wber[df$ausgezaehlt == 1])/ sum(df$wber) * 100, 1),
#                                          round(sum(df$abg[df$ausgezaehlt == 1] / sum(df$wber[df$ausgezaehlt == 1]) * 100, 1)),
#                                          "",
#                                          round(sum(df$vp)/sum(df$gueltig) * 100, 1), 
#                                          round(sum(df$sp)/sum(df$gueltig) * 100, 1), 
#                                          round(sum(df$fp)/sum(df$gueltig) * 100, 1), 
#                                          round(sum(df$gr)/sum(df$gueltig) * 100, 1), 
#                                          round(sum(df$ne)/sum(df$gueltig) * 100, 1), 
#                                          round(sum(df$so)/sum(df$gueltig) * 100, 1)))
# 
# print("Check Rohdaten bundesweit")
# print(check1)

ready <- sum(df$abg)
