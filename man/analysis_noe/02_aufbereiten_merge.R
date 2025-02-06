# Datenaufbereitung

# rm(list = ls()); gc()

# zuerst noch die Testdaten einlesen
# Testdaten 
# auskommentieren für realen durchlauf
# if(gueltig == FALSE){
#   df <- read.csv2("data/testdaten_ltw23_2.csv")
# }


# ab hier immer
import <- df
import[is.na(import)] <- 0
import <- import[import$wber > 0, ]


# abgegebene Stimmen
import$abg <- import$gueltig + import$ung

# ausgezaehlte
import$ausg <- ifelse(import$gueltig > 0, 1, 0)
# import$ausg <- ifelse(is.na(import$gueltig), 0, import$ausg)

# auszaehlungsgrad
auszaehlungsgrad <- round(sum(import$wber[import$ausg == 1])/ sum(import$wber) * 100, 1)

# gibt es in einer gemeinde mehr abgegebene Stimmen als Wahlberechtigte?
help <- which(import$abg > import$wber)
if(length(help) > 0) {
  print(paste0("In der Gemeinde ", import$name[help], " gibt es mehr abgegebene Stimmen als Wahlberechtigte."))
  print("Diese Gemeinde(n) werden bei der Hochrechnung nicht beachtet")
  import$ausg[help] <- 0
}

# nichtwähler
import$nw <- import$wber - import$gueltig
import$nw <- ifelse(import$ausg == 0, 0, import$nw)


# check1: rohdaten
check1 <- data.frame("Check1" = c("", "ausgezaehlte Gemeinden", "abgegebene Stimmen", "Auszaehlungsgrad", "Wahlbeteiligung", "",
                                     "ÖVP", "SPÖ", "FPÖ", "GRÜNE", "NEOS", "SONSTIGE"),
                     "Rohdaten" = c("",
                                         sum(import$ausg),
                                         sum(import$gueltig),
                                         auszaehlungsgrad,
                                         round(sum(import$abg[import$ausg == 1] / sum(import$wber[import$ausg == 1]) * 100, 1)),
                                         "",
                                         round(sum(import$vp)/sum(import$gueltig) * 100, 1), 
                                         round(sum(import$sp)/sum(import$gueltig) * 100, 1), 
                                         round(sum(import$fp)/sum(import$gueltig) * 100, 1), 
                                         round(sum(import$gr)/sum(import$gueltig) * 100, 1), 
                                         round(sum(import$ne)/sum(import$gueltig) * 100, 1), 
                                         round(sum(import$so)/sum(import$gueltig) * 100, 1)))
print(check1)


# vergleichsdaten einlsesen
vergl <- read_xlsx("data/import_vergleichsdaten.xlsx")

# join
data <- left_join(import, vergl)

# colnames festlegen zum einfachen subsetten später
names_ltw <- c("vp18", "sp18", "fp18", "gr18", "ne18", "so18", "nw18")
names_nrw <- c("vp19", "sp19", "fp19", "gr19", "ne19", "so19", "nw19")
names <- c("vp", "sp", "fp", "gr", "ne", "so", "nw")

# anzahl der Wahlberechtigten an die aktuelle Wahl anpassen
data[, names(data) %in% names_ltw]  <- data[, names(data) %in% names_ltw]  * data$wber / data$wber18
data[, names(data) %in% names_nrw] <- data[, names(data) %in% names_nrw] * data$wber / data$wber19

# check this
# cbind(data$vp18 + data$sp18 + data$fp18 + data$gr18 + data$ne18 + data$so18 + data$nw18,
#       data$vp19 + data$sp19 + data$fp19 + data$gr19 + data$ne19 + data$so19 + data$nw19,
#       data$wber)

# check2: cluster
check2 <- table(data$cluster2[data$ausg == 1])
print("Verteilung in Clustern")
print(check2)
